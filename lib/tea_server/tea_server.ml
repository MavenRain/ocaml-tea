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

(** The replay guard (roadmap step 10, D15), re-exported because the library's
    own name module is what callers see: [server_test] and [exactly_once_test]
    build an isolated guard to hand [live_session]. *)
module Replay_guard = Replay_guard
module Guard_sink = Guard_sink
module Durable_guard = Durable_guard

(** Where a browser's identity lives and for how long (roadmap step 12, D17),
    re-exported for the same reason: the session id names the Irmin branch and
    {i is} the CRDT replica id, so choosing a back end is choosing how long a
    model, its undo history, and its replay floor survive. *)
module Session_secret = Session_secret

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
    ; water : Prim.Store_water.t
          (** what [commit] returned for THIS step's own commit — the only
              value a floor persisted for this message may claim as its
              witness (never a head read, which could belong to a later
              writer). *)
    }

  (** One TEA step: load, [Loop.step] (settling the [Cmd] tail), then persist
      through [commit] — the one seam the form-post path (plain event-log
      commit) and the WS pump (coalesced commit) share, so every path mints
      its commit dates from the store's single clock, and the outcome carries
      the water [commit] returned so a caller persisting a floor stamps it
      with the state it actually de-duplicated against. A [Navigate] effect
      is captured and surfaced as the redirect target. *)
  let step_with
      ~(commit :
          St.session -> msg:A.msg -> A.model -> Tea_core.Prim.Store_water.t Lwt.t)
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
        let* water = commit s ~msg model' in
        Lwt.return (Ok { model = model'; redirect = !redirect; water }))
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

  (** One live session: register the store watch, announce this session's
      replica id and current model ({!Tea_core.Wire.Hello}, D14), then pump
      incoming Msg frames through {!step} until the peer closes or
      breaks protocol (an undecodable frame or an exhausted loop ends the
      session — the socket close is the error signal). Every down-frame,
      including the reply to an accepted Msg, travels commit → watch → the
      single [Lwt_stream] writer, so ordering has one source and sends never
      interleave. A [Navigate] effect has no WS surface and is dropped here:
      the client's own optimistic run of the same Msg performs it locally.
      Transient reordering right after connect is possible (the initial
      announcement races frames for commits landing during registration); the
      stream converges on the newest head because later commits always fire
      later watch callbacks. That race is why the watch is registered {i
      before} the [Hello] is pushed and not after: a [Head] arriving ahead of
      the [Hello] costs a client one stale-identity fold that the [Hello] then
      resyncs, whereas registering later would drop the commit entirely. *)
  (* One guard per functor application, process-lifetime: it must outlive the
     socket, because a socket dying is precisely the case de-duplication exists
     to handle. A guard minted per [live_session] would forget everything at the
     moment its knowledge becomes load-bearing. [?guard] is for tests, which
     drive [live_session] directly and want an isolated table, and for the pack
     tier, which supplies one backed by a file journal (roadmap step 11, D16).
     This default — a null sink over empty floors — is step 10's in-memory
     behaviour byte for byte. *)
  let guard : Durable_guard.t =
    Durable_guard.v ~sessions:Replay_guard.default_sessions
      ~tabs:Replay_guard.default_tabs ~sink:Guard_sink.null
      ~floors:Durable_guard.Floors.empty

  let live_session ?(coalesce = Tea_core.Coalesce_spec.Keep_all) ?(guard = guard)
      (s : St.session) (t : live_transport) : unit Lwt.t =
    (* One coalescer per socket (R1): a chatty client folds its own run of
       Msgs into one amended commit, and can never amend a commit some other
       writer minted — a form post, a merge, or an undo ends the run. *)
    let cz = St.Coalescer.v coalesce in
    let step_ws = step_with ~commit:(St.commit_coalesced cz) in
    let frames, push = Lwt_stream.create () in
    let* w =
      St.watch s (fun m ->
          push (Some (Codec.down_to_json (Tea_core.Wire.Head m)));
          Lwt.return_unit)
    in
    let* model0 = St.load s in
    (* The opening frame announces the replica id this session applies under
       (D14), so the client's optimistic edits predict the same slot instead of
       minting a second one. It is the session's own context that is asked -
       not a second derivation of the branch name - so an announcement can
       never disagree with what [step_with] actually applies under. *)
    let replica = Tea_core.Crdt.Ctx.replica (St.ctx_of_session s) in
    push (Some (Codec.down_to_json (Tea_core.Wire.Hello (replica, model0))));
    (* An acknowledgement is minted by the PUMP, never by the store watch
       (roadmap step 10, D15). The watch fires for every writer on the branch —
       a form post, an undo, the other tab on the same cookie — so a [Head]
       cannot stand in for one; and a message whose update is a no-op produces
       no commit at all, so a watch-derived ack would never arrive and the
       client would retry it forever. *)
    let ack (n : Tea_core.Prim.Msg_seq.t) : unit =
      push (Some (Codec.down_to_json (Tea_core.Wire.Ack n)))
    in
    (* Validate the client-chosen header, then consume. [None] is a header this
       session could not read at all — a length or alphabet violation, or a
       non-positive sequence number — which our own client cannot produce. The
       validated tab id rides out with the verdict because the [Fresh] arm
       persists under it (D16). *)
    let admit ~(tab : string) ~(seq : int) :
        (Tea_core.Prim.Tab_id.t * Replay_guard.verdict) option =
      Result.fold (Tea_core.Prim.Tab_id.of_string tab)
        ~error:(fun (_ : Tea_core.Prim.Tab_id.err) -> None)
        ~ok:(fun tab ->
          Option.map
            (fun seq -> (tab, Durable_guard.take guard ~replica ~tab ~seq))
            (Tea_core.Prim.Msg_seq.of_int seq))
    in
    let rec pump () =
      let* frame = t.receive_frame () in
      match frame with
      | None -> Lwt.return_unit
      | Some json ->
        Result.fold (Codec.up_of_json json)
          ~ok:(fun (Tea_core.Wire.Apply { tab; seq; msg }) ->
            (* [~none:] is eager, and deliberately a value with no effect here:
               a resolved promise constant, so evaluating it always is free.
               An unreadable delivery header ends the session, exactly as an
               undecodable frame does — it is the same protocol break. *)
            Option.fold ~none:Lwt.return_unit
              ~some:(fun
                  ((tab, v) : Tea_core.Prim.Tab_id.t * Replay_guard.verdict) ->
                (* Record "taken" durably (D16). The write happens after the
                   apply attempt and before the acknowledgement, so a crash
                   between the two replays as a visible duplicate, never a
                   loss. A failed append degrades the same direction: one
                   audible line, then carry on under step-10 in-memory
                   semantics — never end the session over durability. *)
                let persist_taken ~(water : Prim.Store_water.t)
                    (n : Tea_core.Prim.Msg_seq.t) : unit Lwt.t =
                  let* persisted =
                    Durable_guard.persist guard ~replica ~tab ~seq:n ~water
                  in
                  Result.fold persisted ~ok:Lwt.return
                    ~error:(fun (e : Guard_sink.err) ->
                      let reason =
                        match e with
                        | Guard_sink.Sink_closed -> "sink closed"
                        | Guard_sink.Io io -> io
                      in
                      Printf.eprintf
                        "tea_server: guard persist failed (%s); continuing at-least-once\n%!"
                        reason;
                      Lwt.return_unit)
                in
                match v with
                (* Consume-before-apply is structural, not a convention:
                   [Cell.take] is synchronous and there is no Lwt yield point
                   between deciding a seq is fresh and recording it, so two live
                   sockets for one tab cannot both see it as fresh. It also
                   means the high water records "this seq was taken", not "this
                   seq was applied", so a message whose update exhausts fuel is
                   attempted exactly once instead of killing the session on
                   every reconnect forever. *)
                | Replay_guard.Fresh n ->
                  let* stepped = step_ws s msg in
                  Result.fold stepped
                    (* The floor's witness is the water THIS step's commit
                       returned, never a head read: a head read after the
                       commit could belong to a later writer, and a floor
                       claiming a state it did not de-duplicate against is a
                       forged witness. *)
                    ~ok:(fun (o : step_outcome) ->
                      let* () = persist_taken ~water:o.water n in
                      ack n;
                      pump ())
                    (* Fuel exhaustion still ends the session, but the taken
                       record is persisted first: the high water means
                       "attempted", so a fuel-poison msg is attempted once per
                       guard lifetime — now once ever, not once per restart.
                       Nothing was committed, so there is no store state this
                       floor de-duplicates against: bottom, "no claim",
                       explicitly. *)
                    ~error:(fun (Loop.Fuel_exhausted : Loop.err) ->
                      persist_taken ~water:Prim.Store_water.bottom n)
                | Replay_guard.Duplicate n ->
                  (* Acknowledge without applying. An unacknowledged duplicate
                     is a replay loop that never terminates. *)
                  ack n;
                  pump ()
                | Replay_guard.Gapped ->
                  (* Ignore it, and do NOT end the session: ending it would hand
                     a same-session tab a socket-kill primitive against its
                     sibling. An honest client cannot produce a gap. *)
                  pump ())
              (admit ~tab ~seq))
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
      ~(coalesce : A.msg Tea_core.Coalesce_spec.t) ~(guard : Durable_guard.t)
      (repo : St.t) (request : Dream.request) : Dream.response Lwt.t =
    with_session repo request (fun s ->
        Dream.websocket (fun ws ->
            live_session ~coalesce ~guard s
              { send_frame = Dream.send ws
              ; receive_frame = (fun () -> Dream.receive ws)
              }))

  let handle_ws ~(coalesce : A.msg Tea_core.Coalesce_spec.t)
      ~(guard : Durable_guard.t) (repo : St.t) (request : Dream.request) :
      Dream.response Lwt.t =
    Tea_safe.Origin_gate.check
      ~origin:(Dream.header request "Origin")
      ~host:(Dream.header request "Host")
    |> Result.fold
         ~ok:(fun proof -> accept_ws proof ~coalesce ~guard repo request)
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
      ?(coalesce = Tea_core.Coalesce_spec.Keep_all) ?(guard = guard)
      (repo : St.t) : Dream.handler =
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
       ; Dream.get ws_path (handle_ws ~coalesce ~guard repo)
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
      [Dream.test] against an in-memory repo.

      [?sessions] chooses where a browser's identity is kept (roadmap step 12,
      D17). The default is {!Session_secret.memory}, which is
      [Dream.memory_sessions] - the byte-for-byte step-11 behaviour, so every
      existing [Dream.test] suite keeps minting a fresh id per process. A
      durable back end is what makes a session id (hence the Irmin branch name
      and the CRDT replica id) outlive the process that first issued it. Note
      that this is a {i per-application} default, not a per-request one: the
      middleware is built once, when the handler is, so a caller cannot change
      back ends mid-flight and strand the branches already minted. *)
  let handler ?client_dir ?rpc ?coalesce ?guard ?(sessions = Session_secret.memory)
      (repo : St.t) : Dream.handler =
    (* Sessions OUTSIDE the security headers, exactly as before: the header
       middleware decorates whatever response comes back, including the
       redirects and 403s the session layer itself can produce. *)
    Session_secret.middleware sessions
      (secure_headers (router ?client_dir ?rpc ?coalesce ?guard repo))

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

  (** Deliberately NO [?sessions] (roadmap step 12, D17). "Identity durability
      must never exceed model durability" is the rule, and this is the one
      entry point where the two could be paired the wrong way round: a durable
      cookie over a store that dies with the process would hand a returning tab
      back its old session id, hence its old branch name and its old CRDT
      replica id, over a store rebuilt empty, reusing a replica id against a
      reset causal clock. Leaving the argument off makes the rule structural
      here rather than merely a default. {!Handlers.handler} keeps [?sessions],
      because {!Tea_server_pack.Make_pack.serve_pack} genuinely needs it and
      {!Tea_server_pack.Make_pack} pairs it with a durable store. *)
  let serve ?(interface = "localhost") ?(port = 8080) ?client_dir ?rpc ?coalesce ()
      : unit =
    let repo = Lwt_main.run (Store.create ()) in
    Dream.run ~interface ~port
      (Dream.logger (handler ?client_dir ?rpc ?coalesce repo))
end

(** Request-body admission (roadmap step 8, D11): a size cap enforced {i while}
    the body streams in, so an oversized body is refused without ever being
    fully buffered.

    The bytes this module accumulates never exceed the cap, and the bytes it
    pulls never exceed the cap plus the one chunk that crossed it, whatever the
    attacker's [Content-Length] says. That is a bound on {i this} loop, not a
    memory budget for the process: [Buffer] growth reallocates as it doubles,
    [Buffer.contents] copies the admitted body once more, and how many requests
    may be in flight at once is Dream's connection limit rather than anything
    decided here (DESIGN {b section 10}, R9).

    Within the cap the result is byte-identical to [Dream.body], which is what
    keeps every existing RPC assertion true.

    [read] is a seam, the {!Handlers.live_transport} discipline: a test drives
    the cap with a counting chunk source to witness that the refusal happens
    before the whole body is buffered, with no live connection involved.

    App-generic on purpose: it names no [Api], so there is one copy of the cap
    loop however many RPC contracts a program mounts. *)
module Body = struct
  type capped =
    | Within_cap of string
    | Body_too_large

  (* Check BEFORE adding, so the buffer itself never holds more than [max]
     bytes: the chunk that would cross the cap is refused rather than
     accumulated. The boundary is the one the old post-read
     [String.length body > max] check had - exactly [max] bytes is admitted,
     [max + 1] is refused - and it is independent of how the transport happens
     to split the body.

     Matching [chunk] directly rather than through [Option.fold] is deliberate
     (the {!Handlers.live_session} pump precedent): [~none:] is eager, so
     folding here would copy the whole accumulated buffer on every chunk,
     quadratic in the body size. *)
  let read_capped ~(max : int) ~(read : unit -> string option Lwt.t) :
      capped Lwt.t =
    let buf = Buffer.create 1024 in
    let rec pull () =
      Lwt.bind (read ()) (fun chunk ->
          match chunk with
          | None -> Lwt.return (Within_cap (Buffer.contents buf))
          | Some c ->
            if Buffer.length buf + String.length c > max then Lwt.return Body_too_large
            else (
              Buffer.add_string buf c;
              pull ()))
    in
    pull ()

  let of_request ~(max : int) (request : Dream.request) : capped Lwt.t =
    (* [Dream.body_stream] is resolved ONCE and then pulled: it is the
       request's single consumable body stream, not a fresh reader per call.
       A refused body leaves the remainder unpulled, which is the point of the
       streaming cap. What the transport then does with a request whose body
       nobody drained is Dream's business, and NOT something this module should
       claim to know: an undrained keep-alive connection may well be held until
       a timeout rather than closed, so the refusal is cheap in memory without
       being free (DESIGN {b section 10}, R9). *)
    let stream = Dream.body_stream request in
    read_capped ~max ~read:(fun () -> Dream.read stream)
end

(** Typed RPC dispatch (DESIGN §8): one fixed [Dream.post] route per element
    of [Api.all], derived from the same [Tea_rpc.Make] closures the client
    posts with — no second copy of a name or codec exists to drift. Mount the
    result via [Make(A).serve ~rpc:(Rpc(Api).routes { handle })]; the routes
    then inherit the session and security-header middleware like every other
    route. HTTP statuses are exclusively the transport-error channel (403
    cross-origin gate, 404 route-miss, 415 content-type gate, 413 size cap,
    400 decode refusal); app-level fallibility is declared inside ['resp] in
    the GADT and rides the 200 channel.

    State-changing endpoints are admitted here (roadmap step 8, D12): an
    endpoint the [Api] classifies [Tea_rpc.Mutating] dispatches only behind the
    [same_origin] proof {!Tea_safe.Origin_gate.check} mints, and answers 403 on
    every [denial] arm. The gate runs {i first}, before the content-type and
    body checks, so a forged cross-site POST is refused without its body ever
    being read. A [Tea_rpc.Read_only] endpoint stays ungated behind the
    content-type gate alone, which is a convention its handler must honour: see
    {!Tea_rpc.endpoint_kind}. Form-posted Msg traffic on [/msg] keeps using
    Dream's own token instead.

    How much that gate is worth, stated honestly. The [kind] witness is total,
    so no endpoint can reach the router unclassified, and [dispatch_mutating]
    demands the proof, so the gated path cannot be taken without one. But
    [dispatch] is necessarily in scope for the [Read_only] arm as well, so
    unlike [accept_ws] - which is the module's only mention of
    [Dream.websocket], hence a syntactically isolated sink - rewriting the
    [Mutating] arm to call [dispatch] directly would compile. Keeping the two
    arms distinct is therefore a {i review} obligation backed by
    [test/csrf_test] per endpoint, NOT an invariant the type checker carries
    (DESIGN {b section 10}, R7/R8). *)
module Rpc (Api : Tea_rpc.API) = struct
  module R = Tea_rpc.Make (Api)

  let ( let* ) = Lwt.bind

  (* Streaming cap (D11): enforced chunk by chunk by [Body.of_request], so it
     bounds peak memory per request (cap + one chunk) as well as decode work.
     64 KiB fits every plausible RPC payload here. *)
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
      (* Content-type gate, then the streaming size cap, then decode, then the
         app's handler. Shared by both kinds: gating is the only difference a
         [Mutating] classification makes. *)
      let dispatch (request : Dream.request) : Dream.response Lwt.t =
        if not (content_type_is_json request) then
          Dream.respond ~status:`Unsupported_Media_Type ~headers:text_plain
            "rpc requires Content-Type: application/json"
        else
          let* capped = Body.of_request ~max:max_body_bytes request in
          match (capped : Body.capped) with
          | Body_too_large ->
            Dream.respond ~status:`Payload_Too_Large ~headers:text_plain
              "rpc body too large"
          | Within_cap body ->
            R.decode_req ep body
            |> Result.fold
                 ~error:(fun (Tea_core.Codec.Decode_failed reason) ->
                   Dream.respond ~status:`Bad_Request ~headers:text_plain
                     ("undecodable rpc request: " ^ reason))
                 ~ok:(fun req ->
                   let* resp = h.handle ep req in
                   Dream.respond ~status:`OK ~headers:json_content_type
                     (R.encode_resp ep resp))
      in
      (* The gated path demands the [same_origin] proof, so it cannot be taken
         without one. It is not a full [accept_ws]-style isolated sink, though:
         see the module doc above for exactly what this does and does not
         guarantee. *)
      let dispatch_mutating
          (_ : Tea_safe.Origin_gate.same_origin Tea_safe.Proof.t)
          (request : Dream.request) : Dream.response Lwt.t =
        dispatch request
      in
      Dream.post
        (Tea_core.Prim.Rpc_path.to_string (R.path_of ep))
        (fun request ->
          match (Api.kind ep : Tea_rpc.endpoint_kind) with
          | Read_only -> dispatch request
          | Mutating ->
            Tea_safe.Origin_gate.check
              ~origin:(Dream.header request "Origin")
              ~host:(Dream.header request "Host")
            |> Result.fold
                 ~ok:(fun proof -> dispatch_mutating proof request)
                 ~error:(fun (d : Tea_safe.Origin_gate.denial) ->
                   match d with
                   | Origin_missing | Host_missing | Both_missing | Origin_mismatch ->
                     Dream.respond ~status:`Forbidden ~headers:text_plain
                       "cross-origin mutating rpc rejected"))

  let routes (h : handler) : Dream.route list = List.map (route h) Api.all
end
