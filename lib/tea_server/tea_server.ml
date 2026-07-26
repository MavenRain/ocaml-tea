(** The Dream tier: thesis T1 served over HTTP (roadmap step 1, DESIGN §6).

    [Make (A)] turns any [APP] into a Dream handler. Per request: the Dream
    session resolves to a per-session Irmin branch, the model is loaded, the
    posted Msg is driven through the TEA {!Tea_core.Loop} (settling its [Cmd]
    tail), the transition is committed with the Msg as the commit label, and
    the new view is server-side rendered.

    The no-JS update path is form posts: {!page} rewrites every [On_click msg]
    site of the {i shared} view into a same-origin [<form>] POSTing the
    Repr-JSON Msg plus Dream's CSRF token — progressive enhancement, since the
    client tier re-attaches live handlers to the same view.

    The live-view path is the {!Tea_core.Wire.ws_path} WebSocket (roadmap
    step 3, DESIGN §7): Msg frames travel up and are driven through the same
    {!step}; every commit on the session branch — whether born from this
    socket, a form post, or another tab — comes back down as a full Repr-JSON
    model frame via the Irmin watch. *)

(** The backend-generic handler bodies (roadmap step 8, D2): every current
    handler, router, step, and WS pump, functorized over any
    {!Tea_persist.Store_core.CORE}. [Make] instantiates it over the in-memory
    store; [Tea_server_pack.Make_pack] instantiates the {i same bodies} over
    the durable pack store, so irmin-pack stays out of this unit's (and the
    js_of_ocaml client's) dependency closure. *)
module Handlers
    (A : Tea_core.App.APP)
    (St : Tea_persist.Store_core.CORE with type model = A.model and type msg = A.msg) =
struct
  module Prim = Tea_core.Prim
  module Html = Tea_core.Html
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

  let session_of_request (repo : St.t) (request : Dream.request) :
      (St.session, string) result Lwt.t =
    Prim.Session_id.of_string (hex (Dream.session_id request))
    |> Option.fold
         ~none:(Lwt.return (Error "session id unavailable"))
         ~some:(fun sid ->
           let* s = St.session repo sid in
           Lwt.return (Ok s))

  let with_session (repo : St.t) (request : Dream.request)
      (k : St.session -> Dream.response Lwt.t) : Dream.response Lwt.t =
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

  (** One TEA step: load, [Loop.step] (settling the [Cmd] tail), then persist
      through [commit] — the one seam the form-post path (plain event-log
      commit) and the WS pump (coalesced commit) share, so every path mints
      its commit dates from the store's single clock. A [Navigate] effect is
      captured and surfaced as the redirect target. *)
  let step_with ~(commit : St.session -> msg:A.msg -> A.model -> unit Lwt.t)
      (s : St.session) (msg : A.msg) : (step_outcome, Loop.err) result Lwt.t =
    let redirect = ref None in
    let fx =
      { Loop.sleep = (fun d -> Lwt_unix.sleep (float_of_int (Prim.Delay.to_ms d) /. 1000.))
      ; navigate =
          (fun url ->
            redirect := Some url;
            Lwt.return_unit)
      }
    in
    let* model = St.load s in
    let ctx = St.ctx_of_session s in
    let* stepped = Loop.step ~ctx ~fx ~fuel:Prim.Fuel.default msg model in
    Result.fold stepped
      ~ok:(fun model' ->
        let* () = commit s ~msg model' in
        Lwt.return (Ok { model = model'; redirect = !redirect }))
      ~error:(fun (e : Loop.err) -> Lwt.return (Error e))

  (** One TEA step over HTTP: one commit per Msg, labelled with the Msg so
      the branch log stays the event log. *)
  let step : St.session -> A.msg -> (step_outcome, Loop.err) result Lwt.t =
    step_with ~commit:(fun s ~msg model ->
        St.commit s ~label:(Codec.msg_to_label msg) model)

  (* --- Live view over WebSocket (roadmap step 3, DESIGN §7) -------------- *)

  let ws_path = Tea_core.Wire.ws_path

  (** The two effects a live session needs from its socket, abstracted so the
      pump logic is testable without a WebSocket handshake: [server_test]
      drives it with in-memory queues, the same seam discipline as
      {!Tea_core.Loop}'s IO. *)
  type live_transport =
    { send_frame : string -> unit Lwt.t
    ; receive_frame : unit -> string option Lwt.t
    }

  (** One live session: register the store watch, announce the current model,
      then pump incoming Msg frames through {!step} until the peer closes or
      breaks protocol (an undecodable frame or an exhausted loop ends the
      session — the socket close is the error signal). Every down-frame,
      including the reply to an accepted Msg, travels commit → watch → the
      single [Lwt_stream] writer, so ordering has one source and sends never
      interleave. A [Navigate] effect has no WS surface and is dropped here:
      the client's own optimistic run of the same Msg performs it locally.
      Transient reordering right after connect is possible (the initial
      announcement races frames for commits landing during registration); the
      stream converges on the newest head because later commits always fire
      later watch callbacks. *)
  let live_session ?(coalesce = Tea_core.Coalesce_spec.Keep_all) (s : St.session)
      (t : live_transport) : unit Lwt.t =
    (* One coalescer per socket (R1): a chatty client folds its own run of
       Msgs into one amended commit, and can never amend a commit some other
       writer minted — a form post, a merge, or an undo ends the run. *)
    let cz = St.Coalescer.v coalesce in
    let step_ws = step_with ~commit:(St.commit_coalesced cz) in
    let frames, push = Lwt_stream.create () in
    let* w =
      St.watch s (fun m ->
          push (Some (Codec.model_to_json m));
          Lwt.return_unit)
    in
    let* model0 = St.load s in
    push (Some (Codec.model_to_json model0));
    let rec pump () =
      let* frame = t.receive_frame () in
      match frame with
      | None -> Lwt.return_unit
      | Some json ->
        Result.fold (Codec.msg_of_json json)
          ~ok:(fun msg ->
            let* stepped = step_ws s msg in
            Result.fold stepped
              ~ok:(fun (_ : step_outcome) -> pump ())
              ~error:(fun (Loop.Fuel_exhausted : Loop.err) -> Lwt.return_unit))
          ~error:(fun (Codec.Decode_failed (_ : string)) -> Lwt.return_unit)
    in
    Lwt.finalize
      (fun () -> Lwt.pick [ Lwt_stream.iter_s t.send_frame frames; pump () ])
      (fun () -> St.unwatch w)

  (* Cross-site WebSocket hijacking gate. The full CSWSH rationale now lives on
     [Tea_safe.Origin_gate]'s doc in tea_safe.mli; [accept_ws] is the only
     function that names [Dream.websocket], and it demands the proof that
     [Origin_gate.check] mints, so a socket accepted without the same-origin
     check cannot be expressed here. *)
  let accept_ws (_ : Tea_safe.Origin_gate.same_origin Tea_safe.Proof.t)
      ~(coalesce : A.msg Tea_core.Coalesce_spec.t) (repo : St.t)
      (request : Dream.request) : Dream.response Lwt.t =
    with_session repo request (fun s ->
        Dream.websocket (fun ws ->
            live_session ~coalesce s
              { send_frame = Dream.send ws
              ; receive_frame = (fun () -> Dream.receive ws)
              }))

  let handle_ws ~(coalesce : A.msg Tea_core.Coalesce_spec.t) (repo : St.t)
      (request : Dream.request) : Dream.response Lwt.t =
    Tea_safe.Origin_gate.check
      ~origin:(Dream.header request "Origin")
      ~host:(Dream.header request "Host")
    |> Result.fold
         ~ok:(fun proof -> accept_ws proof ~coalesce repo request)
         ~error:(fun (d : Tea_safe.Origin_gate.denial) ->
           match d with
           | Origin_missing | Host_missing | Both_missing | Origin_mismatch ->
             Dream.respond ~status:`Forbidden "cross-origin websocket rejected")

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

  let handle_root (repo : St.t) (request : Dream.request) : Dream.response Lwt.t =
    with_session repo request (fun s ->
        let* model = St.load s in
        Dream.html (page ~csrf:(Dream.csrf_token request) model))

  let redirect_target (outcome : step_outcome) : string =
    Option.fold ~none:"/" ~some:Prim.Url.to_string outcome.redirect

  let apply_msg (s : St.session) (request : Dream.request) (json : string) :
      Dream.response Lwt.t =
    Result.fold (Codec.msg_of_json json)
      ~ok:(fun msg ->
        let* outcome = step s msg in
        Result.fold outcome
          ~ok:(fun o -> Dream.redirect request (redirect_target o))
          ~error:(fun (Loop.Fuel_exhausted : Loop.err) ->
            Dream.respond ~status:`Internal_Server_Error "command loop exhausted its fuel"))
      ~error:(fun (Codec.Decode_failed reason) ->
        (* Pin text/plain so the attacker-derived reason can never be sniffed as
           HTML (defense in depth beside the nosniff header). *)
        Dream.respond ~status:`Bad_Request
          ~headers:[ ("Content-Type", "text/plain; charset=utf-8") ]
          ("undecodable msg: " ^ reason))

  let handle_msg (repo : St.t) (request : Dream.request) : Dream.response Lwt.t =
    with_session repo request (fun s ->
        with_form request (fun fields ->
            List.assoc_opt "msg" fields
            |> Option.fold
                 ~none:(Dream.respond ~status:`Bad_Request "missing msg field")
                 ~some:(apply_msg s request)))

  let handle_undo (repo : St.t) (request : Dream.request) : Dream.response Lwt.t =
    with_session repo request (fun s ->
        with_form request (fun _fields ->
            (* At the history root undo is a no-op; either way, re-render. *)
            let* _restored = St.undo s in
            Dream.redirect request "/"))

  (* --- Assembly ----------------------------------------------------------- *)

  (* [?client_dir]: also serve a compiled js_of_ocaml client bundle (the
     [/app] pages), so the SSR tier, the live client, and the WebSocket share
     one origin — which is precisely what {!same_origin} and the shared Dream
     session cookie require. *)
  let router ?(client_dir : string option) ?(rpc : Dream.route list = [])
      ?(coalesce = Tea_core.Coalesce_spec.Keep_all) (repo : St.t) : Dream.handler =
    let client_routes =
      Option.fold client_dir ~none:[]
        ~some:(fun dir ->
          [ Dream.get "/app" (fun request -> Dream.redirect request "/app/index.html")
          ; Dream.get "/app/**" (Dream.static dir)
          ])
    in
    Dream.router
      ([ Dream.get "/" (handle_root repo)
       ; Dream.post msg_path (handle_msg repo)
       ; Dream.post undo_path (handle_undo repo)
       ; Dream.get ws_path (handle_ws ~coalesce repo)
       ]
      @ rpc @ client_routes)

  (* Append the strict security headers to every response (CSP, X-Frame-Options,
     X-Content-Type-Options). Outermost so even error and 403 responses carry
     them; [Tea_safe.Security_headers] proves the values are control-byte free. *)
  let secure_headers (inner : Dream.handler) : Dream.handler =
   fun request ->
    let* response = inner request in
    Tea_safe.Security_headers.(to_headers strict)
    |> List.iter (fun h -> Dream.set_header response (Tea_safe.Header.name h) (Tea_safe.Header.value h));
    Lwt.return response

  (** The full request pipeline: session middleware over the security-headers
      middleware over the router. Exposed so tests can drive it with
      [Dream.test] against an in-memory repo. *)
  let handler ?client_dir ?rpc ?coalesce (repo : St.t) : Dream.handler =
    Dream.memory_sessions (secure_headers (router ?client_dir ?rpc ?coalesce repo))

  (** Blocking entry point for a native server binary. [?coalesce] is the
      app's commit-coalescing policy for live (WS) sessions; the default
      keeps one commit per Msg. *)
end

(** The mem-backed Dream tier: [Make (A)] instantiates {!Handlers} over the
    in-memory store, so the T1/T2 surface (and every existing test) is
    preserved byte for byte. The durable pack-backed tier is
    [Tea_server_pack.Make_pack]. *)
module Make (A : Tea_core.App.APP) = struct
  module Store = Tea_persist.Store.Make (A)
  include Handlers (A) (Store)

  let serve ?(interface = "localhost") ?(port = 8080) ?client_dir ?rpc ?coalesce () : unit =
    let repo = Lwt_main.run (Store.create ()) in
    Dream.run ~interface ~port (Dream.logger (handler ?client_dir ?rpc ?coalesce repo))
end

(** Typed RPC dispatch (DESIGN §8): one fixed [Dream.post] route per element
    of [Api.all], derived from the same [Tea_rpc.Make] closures the client
    posts with — no second copy of a name or codec exists to drift. Mount the
    result via [Make(A).serve ~rpc:(Rpc(Api).routes { handle })]; the routes
    then inherit the session and security-header middleware like every other
    route. HTTP statuses are exclusively the transport-error channel (404
    route-miss, 415 content-type gate, 413 size cap, 400 decode refusal);
    app-level fallibility is declared inside ['resp] in the GADT and rides
    the 200 channel. Both milestone endpoints are read-only BY POLICY: no
    mutating RPC endpoint ships until an anti-CSRF check (Origin_gate or
    token) lands on this path — state-changing traffic stays on [/msg]
    behind Dream's form tokens. *)
module Rpc (Api : Tea_rpc.API) = struct
  module R = Tea_rpc.Make (Api)

  let ( let* ) = Lwt.bind

  (* Post-read cap: [Dream.body] has ALREADY buffered the full request by the
     time this runs (alpha8 exposes no pre-read limit), so the cap bounds
     decode work and downstream handling, NOT peak memory per request. The
     streaming [Dream.body_stream] chunk-accounted cap is a recorded deferral
     (DESIGN §8). 64 KiB fits every plausible RPC payload here. *)
  let max_body_bytes = 65_536

  (* Pin text/plain on refusal bodies so an attacker-derived reason can never
     be sniffed as markup (the handle_msg precedent). *)
  let text_plain = [ ("Content-Type", "text/plain; charset=utf-8") ]

  let json_content_type : (string * string) list =
    Tea_safe.Header.Value.of_string "application/json; charset=utf-8"
    |> Result.fold
         ~ok:(fun value ->
           let h = Tea_safe.Header.v (Tea_safe.Header.Name.v "Content-Type") value in
           [ (Tea_safe.Header.name h, Tea_safe.Header.value h) ])
         ~error:(fun (Tea_safe.Header.Value.Control_char (_ : char)) ->
           (* Dead: the literal above is control-free; typed fallback beats a
              partial match. *)
           [ ("Content-Type", "application/json") ])

  (* Media type only, parameters stripped, case-folded: "application/json",
     "application/json; charset=utf-8", and "APPLICATION/JSON" all pass. The
     gate makes a cross-site RPC POST non-simple (a browser preflights it),
     which is the cheap half of CSRF hardening for read-only endpoints. *)
  let content_type_is_json (request : Dream.request) : bool =
    Dream.header request "Content-Type"
    |> Option.fold ~none:false ~some:(fun ct ->
           match String.split_on_char ';' ct with
           | [] -> false (* unreachable: split_on_char never returns [] *)
           | media :: (_ : string list) ->
             String.equal (String.lowercase_ascii (String.trim media)) "application/json")

  (* Rank-2 record so one value handles every endpoint at its own type — the
     [Vdom_blit.Cmd.handler] precedent. *)
  type handler = { handle : 'req 'resp. ('req, 'resp) Api.t -> 'req -> 'resp Lwt.t }

  let route (h : handler) (p : Api.any) : Dream.route =
    match p with
    | Api.Any ep ->
      Dream.post
        (Tea_core.Prim.Rpc_path.to_string (R.path_of ep))
        (fun request ->
          if not (content_type_is_json request) then
            Dream.respond ~status:`Unsupported_Media_Type ~headers:text_plain
              "rpc requires Content-Type: application/json"
          else
            let* body = Dream.body request in
            if String.length body > max_body_bytes then
              Dream.respond ~status:`Payload_Too_Large ~headers:text_plain
                "rpc body too large"
            else
              R.decode_req ep body
              |> Result.fold
                   ~error:(fun (Tea_core.Codec.Decode_failed reason) ->
                     Dream.respond ~status:`Bad_Request ~headers:text_plain
                       ("undecodable rpc request: " ^ reason))
                   ~ok:(fun req ->
                     let* resp = h.handle ep req in
                     Dream.respond ~status:`OK ~headers:json_content_type
                       (R.encode_resp ep resp)))

  let routes (h : handler) : Dream.route list = List.map (route h) Api.all
end
