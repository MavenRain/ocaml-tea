(** The Dream tier: thesis T1 served over HTTP (roadmap step 1, DESIGN §6).

    [Make (A)] turns any [APP] into a Dream handler. Per request: the Dream
    session resolves to a per-session Irmin branch, the model is loaded, the
    posted Msg is driven through the TEA {!Tea_core.Loop} (settling its [Cmd]
    tail), the transition is committed with the Msg as the commit label, and
    the new view is server-side rendered.

    The no-JS update path is form posts: {!page} rewrites every [On_click msg]
    site of the {i shared} view into a same-origin [<form>] POSTing the
    Repr-JSON Msg plus Dream's CSRF token — progressive enhancement, since the
    client tier will later re-attach live handlers to the same view. No
    WebSocket yet (that is roadmap step 3). *)

module Make (A : Tea_core.App.APP) = struct
  module Prim = Tea_core.Prim
  module Html = Tea_core.Html
  module Store = Tea_persist.Store.Make (A)
  module Codec = Tea_core.Codec.Make (A)

  (** [Loop]'s IO instantiated with Lwt: the one place the pure engine meets
      the server runtime. *)
  module Io = struct
    type 'a t = 'a Lwt.t

    let return = Lwt.return
    let bind = Lwt.bind
    let all = Lwt.all
  end

  module Loop = Tea_core.Loop.Loop (Io) (A)
  open Lwt.Syntax

  (** The two POST endpoints every ocaml-tea app serves. *)
  let msg_path = "/msg"

  let undo_path = "/undo"

  (* Dream session ids are opaque strings; hex-encoding maps them injectively
     into the [Session_id] alphabet (never empty, no '/'), so every browser
     session gets exactly its own Irmin branch. *)
  let hex (s : string) : string =
    String.init
      (2 * String.length s)
      (fun i ->
        let byte = Char.code s.[i / 2] in
        let nibble = if i mod 2 = 0 then byte lsr 4 else byte land 0xf in
        "0123456789abcdef".[nibble])

  let session_of_request (repo : Store.t) (request : Dream.request) :
      (Store.session, string) result Lwt.t =
    Prim.Session_id.of_string (hex (Dream.session_id request))
    |> Option.fold
         ~none:(Lwt.return (Error "session id unavailable"))
         ~some:(fun sid ->
           let* s = Store.session repo sid in
           Lwt.return (Ok s))

  let with_session (repo : Store.t) (request : Dream.request)
      (k : Store.session -> Dream.response Lwt.t) : Dream.response Lwt.t =
    let* resolved = session_of_request repo request in
    Result.fold resolved ~ok:k ~error:(fun reason ->
        Dream.respond ~status:`Internal_Server_Error reason)

  (* Admit only a CSRF-valid form post; every [form_result] case is
     enumerated so a new Dream failure mode is a compile error here. *)
  let with_form (request : Dream.request)
      (k : (string * string) list -> Dream.response Lwt.t) : Dream.response Lwt.t =
    let* outcome = Dream.form request in
    match (outcome : (string * string) list Dream.form_result) with
    | `Ok fields -> k fields
    | `Expired _ | `Wrong_session _ | `Invalid_token _ | `Missing_token _ | `Many_tokens _ ->
      Dream.respond ~status:`Forbidden "form token missing, stale, or invalid"
    | `Wrong_content_type -> Dream.respond ~status:`Bad_Request "expected a form post"

  type step_outcome =
    { model : A.model
    ; redirect : Prim.Url.t option
    }

  (** One TEA step over HTTP: load, [Loop.step] (settling the [Cmd] tail),
      commit labelled with the Msg so the branch log stays the event log. A
      [Navigate] effect is captured and surfaced as the redirect target. *)
  let step (s : Store.session) (msg : A.msg) : (step_outcome, Loop.err) result Lwt.t =
    let redirect = ref None in
    let fx =
      { Loop.sleep = (fun d -> Lwt_unix.sleep (float_of_int (Prim.Delay.to_ms d) /. 1000.))
      ; navigate =
          (fun url ->
            redirect := Some url;
            Lwt.return_unit)
      }
    in
    let* model = Store.load s in
    let* stepped = Loop.step ~fx ~fuel:Prim.Fuel.default msg model in
    Result.fold stepped
      ~ok:(fun model' ->
        let* () = Store.commit s ~label:(Codec.msg_to_label msg) model' in
        Lwt.return (Ok { model = model'; redirect = !redirect }))
      ~error:(fun (e : Loop.err) -> Lwt.return (Error e))

  (* --- SSR: the shared view rendered as a no-JS form-post page ---------- *)

  let hidden ~(name : string) ~(value : string) : A.msg Html.t =
    Html.input ~attrs:[ Html.type_ "hidden"; Html.name_ name; Html.value_ value ] ()

  (* Rewrite every [On_click msg] site into a form POSTing the Repr-JSON Msg
     (with the CSRF token). [On_input] has no static equivalent; it is kept
     for the client tier and dropped by [Render_static]. Known limit: an
     [On_click] ancestor of another [On_click] renders as nested forms, which
     is invalid HTML; keep click handlers on leaf controls until id-based
     form association ships with the client tier. *)
  let rec formify ~(csrf : string) (h : A.msg Html.t) : A.msg Html.t =
    match h with
    | Html.Text _ -> h
    | Html.Element (tag, attrs, children) ->
      let children' = List.map (formify ~csrf) children in
      let clicks, kept =
        List.partition_map
          (fun (a : A.msg Html.attr) ->
            match a with
            | Html.On_click m -> Either.Left m
            | Html.Attr _ -> Either.Right a
            | Html.On_input _ -> Either.Right a)
          attrs
      in
      let element = Html.elt (Prim.Tag.to_string tag) ~attrs:kept children' in
      (* [Html.elt] guarantees at most one On_click per element, so the tail
         here is provably empty; the head is the element's one click Msg. *)
      (match clicks with
       | [] -> element
       | msg :: _ ->
         Html.elt "form"
           ~attrs:[ Html.method_ "post"; Html.action_ msg_path ]
           [ hidden ~name:"dream.csrf" ~value:csrf
           ; hidden ~name:"msg" ~value:(Codec.msg_to_json msg)
           ; element
           ])

  (** The full document: title from [A.title], body from the formified shared
      view, plus the undo control every versioned-model app inherits (T1). *)
  let page ~(csrf : string) (model : A.model) : string =
    let undo =
      Html.elt "form"
        ~attrs:[ Html.method_ "post"; Html.action_ undo_path ]
        [ hidden ~name:"dream.csrf" ~value:csrf; Html.button [ Html.text "undo" ] ]
    in
    let doc =
      Html.elt "html"
        [ Html.elt "head" [ Html.elt "title" [ Html.text (Prim.Title.to_string A.title) ] ]
        ; Html.elt "body" [ formify ~csrf (A.view model); undo ]
        ]
    in
    "<!doctype html>" ^ Tea_core.Render_static.to_string doc

  (* --- Handlers ---------------------------------------------------------- *)

  let handle_root (repo : Store.t) (request : Dream.request) : Dream.response Lwt.t =
    with_session repo request (fun s ->
        let* model = Store.load s in
        Dream.html (page ~csrf:(Dream.csrf_token request) model))

  let redirect_target (outcome : step_outcome) : string =
    Option.fold ~none:"/" ~some:Prim.Url.to_string outcome.redirect

  let apply_msg (s : Store.session) (request : Dream.request) (json : string) :
      Dream.response Lwt.t =
    Result.fold (Codec.msg_of_json json)
      ~ok:(fun msg ->
        let* outcome = step s msg in
        Result.fold outcome
          ~ok:(fun o -> Dream.redirect request (redirect_target o))
          ~error:(fun (Loop.Fuel_exhausted : Loop.err) ->
            Dream.respond ~status:`Internal_Server_Error "command loop exhausted its fuel"))
      ~error:(fun (Codec.Decode_failed reason) ->
        Dream.respond ~status:`Bad_Request ("undecodable msg: " ^ reason))

  let handle_msg (repo : Store.t) (request : Dream.request) : Dream.response Lwt.t =
    with_session repo request (fun s ->
        with_form request (fun fields ->
            List.assoc_opt "msg" fields
            |> Option.fold
                 ~none:(Dream.respond ~status:`Bad_Request "missing msg field")
                 ~some:(apply_msg s request)))

  let handle_undo (repo : Store.t) (request : Dream.request) : Dream.response Lwt.t =
    with_session repo request (fun s ->
        with_form request (fun _fields ->
            (* At the history root undo is a no-op; either way, re-render. *)
            let* _restored = Store.undo s in
            Dream.redirect request "/"))

  (* --- Assembly ----------------------------------------------------------- *)

  let router (repo : Store.t) : Dream.handler =
    Dream.router
      [ Dream.get "/" (handle_root repo)
      ; Dream.post msg_path (handle_msg repo)
      ; Dream.post undo_path (handle_undo repo)
      ]

  (** The full request pipeline: session middleware over the router. Exposed
      so tests can drive it with [Dream.test] against an in-memory repo. *)
  let handler (repo : Store.t) : Dream.handler = Dream.memory_sessions (router repo)

  (** Blocking entry point for a native server binary. *)
  let serve ?(interface = "localhost") ?(port = 8080) () : unit =
    let repo = Lwt_main.run (Store.create ()) in
    Dream.run ~interface ~port (Dream.logger (handler repo))
end
