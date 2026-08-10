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

(** The RPC channel's reply store (roadmap step 15, D20.2), re-exported for the
    same reason: it is the one half of the exactly-once pump whose laws can be
    driven directly, so [reply_cache_test] owns both sides of the window
    instead of racing for it. *)
module Reply_cache = Reply_cache

(** The duplicate-ack parking registry (roadmap step 17, D22), re-exported for
    the same reason as {!Replay_guard}: a test hands [live_session] an isolated
    table beside its isolated guard, and reads [parked_count] as the positive
    witness that a replay parked rather than raced. *)
module Ack_park = Ack_park

(** The reaper loop's timing shell (roadmap step 22, D24), re-exported so an
    entry point's caller spells the knob [Tea_server.Reaper.spec] and a test
    drives {!Reaper.loop} with an injected timer, without reaching into the
    library's internals. *)
module Reaper = Reaper

(** Where a browser's identity lives and for how long (roadmap step 12, D17),
    re-exported for the same reason: the session id names the Irmin branch and
    {i is} the CRDT replica id, so choosing a back end is choosing how long a
    model, its undo history, and its replay floor survive. *)
module Session_secret = Session_secret

(** The exactly-once RPC channel's server-side state, {i outside} the {!Rpc}
    functor although that is where it is used (roadmap step 15, D20).

    None of the three fields mentions the [Api], so keeping the record inside
    the functor would make it a fresh nominal type per instantiation - and the
    composition sites that must hand one over ({!Make.serve},
    {!Tea_server_pack.Make_pack.serve_pack}) know nothing of any [Api]. The
    journal is opened by the entry point, which also owns its teardown close,
    so the value can only travel in that direction: framework builds it, the
    app's route builder receives it. {!Rpc.once} re-exports this type with its
    fields, so an app still spells it [Rpc.once]. *)
module Rpc_once = struct
  type t =
    { guard : Durable_guard.t
    ; replies : Reply_cache.Cell.cell
    ; floor_replica : Tea_core.Crdt.Replica.t
    }

  (** The general constructor: an already-composed guard (durable or not) plus
      the branch its keyed handlers commit on. The reply cache is minted here
      rather than accepted, because one [t] {i is} one channel: two values
      sharing a cache would answer each other's retries from bytes their own
      guard never took, and two caches behind one guard would answer [Gone]
      for deliveries the other is still holding. *)
  let v ~(guard : Durable_guard.t) ~(floor_replica : Tea_core.Crdt.Replica.t) : t =
    { guard; replies = Reply_cache.Cell.v (); floor_replica }

  (** The mem tier's value, which is D16's default exactly: the RPC guard over
      {!Guard_sink.null} and empty floors. Nothing here outlives the process,
      which is the whole of what the mem tier ever promised. No [?mirror] is
      passed, so the bound is [Durable_guard.default_mirror ~sessions ~tabs ()]
      with no journal cap to clear - the sinkless composition the derivation
      was written for (D20.4). *)
  let mem ~(floor_replica : Tea_core.Crdt.Replica.t) : t =
    v ~floor_replica
      ~guard:
        (Durable_guard.v ~sessions:Replay_guard.default_rpc_replicas
           ~tabs:Replay_guard.default_rpc_tabs ~sink:Guard_sink.null
           ~floors:Durable_guard.Floors.empty ())
end

(* Dream session ids are opaque strings; hex-encoding maps them injectively
   into the [Session_id] alphabet (never empty, no '/'), so every browser
   session gets exactly its own Irmin branch.

   Top-level rather than functor-local since roadmap step 15: the RPC channel's
   floor-tab derivation hashes the SAME encoding of the SAME cookie
   ({!Rpc.floor_tab}, D20.1), and two spellings of "the session's hex id" would
   put the guard's dedup namespace and the store's branch name on different
   keys with no symptom either way. Injectivity is also what lets that
   derivation use NUL as a field separator: a hex-encoded half can contain no
   NUL, so no two different (session, tab) pairs can slide into one digest
   input.

   Written over [String.to_seq] rather than the index arithmetic it replaces:
   same bytes for every input ([Char.code] is always [0..255] and ["%02x"] is
   the two lowercase nibbles), no partial accessor. *)
let hex (s : string) : string =
  String.to_seq s
  |> Seq.map (fun (c : char) -> Printf.sprintf "%02x" (Char.code c))
  |> List.of_seq |> String.concat ""

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
      is captured and surfaced as the redirect target.

      Witnessed since D19 (roadmap step 14): the read mints a
      {!St.based} token and the commit closure can only be handed that token,
      so nothing routed through this seam can reach the last-write-wins door.
      Under contention the outcome carries the {i committed} model and the
      water of the commit that actually landed, so a floor stamped from it
      still names a commit the store has.

      [?interpose] is fired between this step's witnessed read and its commit,
      so a test can land a competing writer in the one window that matters and
      get a deterministic reconcile without a sleep or a scheduler assumption.
      Defaults to a resolved unit, the same seam discipline as [?guard]. It is
      deliberately NOT an injectable commit function: a commit seam would let
      an injected closure ignore the witness and call the plain
      last-write-wins commit, which is a compile-clean way to disable D19 in
      production, and this module has no [.mli] in which to forbid it. *)
  let step_with ?(interpose : unit -> unit Lwt.t = fun () -> Lwt.return_unit)
      ~(commit : St.based -> msg:A.msg -> A.model -> St.committed Lwt.t)
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
    let* based = St.load_based s in
    let ctx = St.ctx_of_session s in
    let* stepped = Loop.step ~ctx ~fx ~fuel:Prim.Fuel.default msg (St.based_model based) in
    Result.fold stepped
      ~ok:(fun model' ->
        let* () = interpose () in
        let* (landed : St.committed) = commit based ~msg model' in
        Lwt.return
          (Ok { model = landed.St.model; redirect = !redirect; water = landed.St.water }))
      ~error:(fun (e : Loop.err) -> Lwt.return (Error e))

  (** One TEA step over HTTP: one commit per Msg, labelled with the Msg so
      the branch log stays the event log. *)
  let step ?(interpose : unit -> unit Lwt.t = fun () -> Lwt.return_unit) :
      St.session -> A.msg -> (step_outcome, Loop.err) result Lwt.t =
    step_with ~interpose ~commit:(fun based ~msg model ->
        St.commit_based based ~label:(Codec.msg_to_label msg) model)

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
      ~floors:Durable_guard.Floors.empty ()

  (* One parking registry per functor application, process-lifetime, for the
     same reason as [guard]: the duplicate that parks arrives on a DIFFERENT
     socket from the attempt it parks on, so a per-session registry could
     never pair them. In-process only, deliberately (D22): a process death
     loses the registry and every in-flight attempt together, and the durable
     floor then adjudicates the replay on its own. [?park] is for tests,
     which want an isolated table beside their isolated [?guard]. *)
  let park : Ack_park.t = Ack_park.create ()

  let live_session ?(coalesce = Tea_core.Coalesce_spec.Keep_all) ?(guard = guard)
      ?(park = park)
      ?(interpose : unit -> unit Lwt.t = fun () -> Lwt.return_unit) (s : St.session)
      (t : live_transport) : unit Lwt.t =
    (* One coalescer per socket (R1): a chatty client folds its own run of
       Msgs into one amended commit, and can never amend a commit some other
       writer minted — a form post, a merge, or an undo ends the run. *)
    let cz = St.Coalescer.v coalesce in
    let step_ws = step_with ~interpose ~commit:(St.commit_coalesced cz) in
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
    (* Cancellation atomicity (R10d, D21). Two distinct cancellation sources
       could once land inside the take-to-ack span (admit, [step_ws],
       [persist_taken], [ack]), and each is closed by its own mechanism. The
       internal one was the teardown's own [Lwt.pick], which raced the WHOLE
       pump against the send loop: a send failure cancelled the pump mid
       [step_ws], leaving the seq consumed, the effect unapplied and no floor
       persisted, so the reconnect replay read [Duplicate] and the message was
       acked without ever applying (window W1: silent loss). That source is
       closed by construction: the per-frame body now lives in [handle_frame],
       which is never an element of any [Lwt.pick]/[Lwt.choose] list and never
       a [Lwt.cancel] target, and the sender's death is observed only between
       frames. The external source, a caller cancelling the promise
       [live_session] returned, is stopped by the [Lwt.protected] barrier
       inside the [Fresh] arm. The span therefore keeps the three-case family
       form: exactly-once (applied, floored, acked), a licensed visible
       convergent duplicate, or the fuel arm's once-ever
       attempted-with-no-claim floor, never a silently consumed-but-unapplied
       sequence number. *)
    (* The sender runs beside the pump - bound as [drained], joined by
       teardown, never raced. [died] is the one-shot signal
       that the send side failed. A [wait] promise is not cancelable, so no
       cancellation search can ever reject it; and [mark_died] fires at most
       once, because [iter_s]'s promise rejects at most once and the
       [Lwt.catch] handler below therefore runs at most once, which is what
       keeps [wakeup_later] away from its double-resolve failure mode. *)
    let died, mark_died = Lwt.wait () in
    let sender () =
      Lwt.catch
        (fun () -> Lwt_stream.iter_s t.send_frame frames)
        (fun (exn : exn) ->
          Printf.eprintf
            "tea_server: live_session send failed (%s); ending the pump\n%!"
            (Printexc.to_string exn);
          Lwt.wakeup_later mark_died ();
          Lwt.return_unit)
    in
    (* [died_arm] is minted ONCE, above the recursion: a per-iteration
       [Lwt.map] would register one more permanent waiter on the pending
       [died] every frame - a leak that grows for the whole life of a healthy
       session. [choose]'s own waiters are removable and are cleaned up when
       each choose settles; a [map]'s waiter is not. *)
    let died_arm = Lwt.map (fun () -> `Died) died in
    (* The park's own death arm (step 17, D22), minted ONCE for the same
       leak reason as [died_arm] and separately from it: the two [choose]
       sites close the arm over different variant rows, so one shared arm
       cannot type at both. *)
    let died_park_arm = Lwt.map (fun () -> `Died) died in
    let rec pump () =
      (* The only race left in the session: wait-for-next-frame against
         observe-the-sender's-death. [Lwt.choose], unlike [Lwt.pick], never
         cancels the losing branch, so a hung [receive_frame] cannot block
         death detection, and death detection cannot cancel a receive. The
         [Lwt.state] check first makes the between-frames death observation
         deterministic: [choose] picks uniformly at random among candidates
         that are ALREADY resolved, so a death that fired during the previous
         span, raced against a pipelining client's already-available frame,
         would otherwise be a coin flip. With the pre-check, [choose] only
         ever starts on two pending promises, and a frame that loses to
         [died] there is abandoned unread; the client retransmits it on
         reconnect and the guard adjudicates the replay as usual. *)
      match Lwt.state died with
      | Lwt.Return () -> Lwt.return_unit
      | Lwt.Fail (_ : exn) -> Lwt.return_unit
      | Lwt.Sleep ->
        let* arrival =
          Lwt.choose
            [ died_arm
            ; Lwt.map (fun frame -> `Frame frame) (t.receive_frame ())
            ]
        in
        (match arrival with
         | `Died -> Lwt.return_unit
         | `Frame frame ->
           Option.fold ~none:Lwt.return_unit ~some:handle_frame frame)
    and handle_frame (json : string) : unit Lwt.t =
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
                (* The protected span (R10d, D21): ONE [Lwt.protected] body
                   covers [step_ws] and BOTH [persist_taken] arms, the
                   success floor and the fuel-exhaustion bottom floor. If a
                   cancellation from outside this module reaches the pump's
                   chain, the backwards search stops at this wrapper: the
                   wrapper rejects, but the body is not cancelled and runs to
                   natural completion as an orphan, so a torn window lands at
                   worst in W2's already-licensed visible-duplicate shape,
                   and the fuel arm still persists its once-ever bottom
                   floor. The continuation, [ack] and the recursion, is
                   deliberately NOT under the barrier: [protected], unlike
                   [no_cancel], lets the outer chain reject immediately, so
                   the pump can never recurse past the death of its
                   socket. Two Lwt truths bound that claim rather than
                   break it: a cancel landing in the callback-deferral
                   window after the body already resolved is a no-op - Lwt
                   cancellation is advisory, and the session then lives on
                   exactly as if the cancel had arrived a frame later - and
                   [Lwt.catch]'s default filter passes runtime exceptions
                   (Out_of_memory, Stack_overflow) through without the
                   release, out of scope with the rest of the
                   no-exceptions discipline.

                   The [Lwt.catch] is the REJECTION barrier (R10d round 3),
                   and it sits INSIDE the [protected] body: the position IS
                   the discrimination. [protected] never rejects its body -
                   a wrapper cancellation rejects only the outer promise
                   while the body runs on as the orphan, untouched - so
                   every exception this catch sees originated inside the
                   body itself. A body that rejected is dead with no floor
                   coming, and [release] is unconditionally its
                   compensation: no match on the exception value at all.
                   That closes both round-2 holes. A body that rejects AFTER
                   the wrapper was cancelled releases from inside the orphan
                   (the re-fail is then dropped by [protected]'s
                   already-settled outer promise, which is fine - the
                   release has already happened). A body-internal [Canceled]
                   (a third party cancelling something inside [step_ws]; no
                   orphan exists) is a dead body like any other, and
                   releases. The round-2 shape - catch OUTSIDE [protected],
                   discriminating on the exception VALUE - got both of those
                   wrong: the orphan's late rejection was dropped before any
                   release, and a body-internal [Canceled] was misread as
                   orphan-alive. Mirror of the keyed HTTP tier's R27:
                   [release] re-opens exactly [seq], so the replay reads
                   Fresh and re-applies; if the body half-landed before
                   rejecting, that is a visible convergent duplicate, the
                   licensed direction. *)
                (* Open the parking row in the same continuation as the
                   [Fresh] verdict (step 17, D22): no Lwt yield separates
                   [Cell.take]'s consume from this registration, so a
                   duplicate that arrives after the take can never miss the
                   row. Every exit from the protected span below settles it -
                   persist success, the fuel-exhaustion bottom floor, or the
                   rejection catch (D21's three-case closure) - so a row
                   lives exactly as long as its attempt. The returned handle
                   keys those settles: a row minted by a LATER attempt, over
                   a seq the guard's tab-LRU eviction re-opened, is never
                   settled by this one (D22). *)
                let attempt = Ack_park.register park ~replica ~tab ~seq:n in
                let* (landed : (unit, unit) result) =
                  Lwt.protected
                    (Lwt.catch
                       (fun () ->
                         let* stepped = step_ws s msg in
                         Result.fold stepped
                           (* The floor's witness is the water THIS step's
                              commit returned, never a head read: a head read
                              after the commit could belong to a later
                              writer, and a floor claiming a state it did not
                              de-duplicate against is a forged witness. *)
                           ~ok:(fun (o : step_outcome) ->
                             let* () = persist_taken ~water:o.water n in
                             (* Settle INSIDE the protected body, after the
                                floor landed: the outer ack site below is
                                dead code for the orphan (its wrapper already
                                rejected), so a settle placed there would
                                strand every parked waiter on exactly the
                                F5' path. *)
                             Ack_park.settle park ~replica ~tab ~seq:n
                               ~handle:attempt ~outcome:Ack_park.Landed;
                             Lwt.return (Ok ()))
                           (* Fuel exhaustion still ends the session, but the
                              taken record is persisted first: the high water
                              means "attempted", so a fuel-poison msg is
                              attempted once per guard lifetime — now once
                              ever, not once per restart. Nothing was
                              committed, so there is no store state this
                              floor de-duplicates against: bottom, "no
                              claim", explicitly. *)
                           ~error:(fun (Loop.Fuel_exhausted : Loop.err) ->
                             let* () =
                               persist_taken ~water:Prim.Store_water.bottom n
                             in
                             (* [Released], not [Landed]: nothing was
                                committed, so a parked duplicate must not be
                                acked on this attempt's account. This path
                                never minted an ack before either - the
                                once-ever poison contract is unchanged. *)
                             Ack_park.settle park ~replica ~tab ~seq:n
                               ~handle:attempt ~outcome:Ack_park.Released;
                             Lwt.return (Error ())))
                       (fun (exn : exn) ->
                         Durable_guard.release guard ~replica ~tab ~seq:n;
                         (* Settle AFTER the release, so a parked waiter that
                            wakes with [Released] and provokes a client retry
                            finds the seq already reopened: the retry reads
                            [Fresh] and re-applies. Both calls are
                            synchronous; [wakeup_later] defers the waiters
                            past this whole handler regardless. *)
                         Ack_park.settle park ~replica ~tab ~seq:n
                           ~handle:attempt ~outcome:Ack_park.Released;
                         Printf.eprintf
                           "tea_server: ws apply failed before persist (%s); seq released for retry\n%!"
                           (Printexc.to_string exn);
                         Lwt.fail exn))
                in
                Result.fold landed
                  ~ok:(fun () ->
                    ack n;
                    pump ())
                  ~error:(fun () -> Lwt.return_unit)
              | Replay_guard.Duplicate n ->
                (* Acknowledge without applying - unless the seq's own
                   take-to-ack attempt is still in flight (step 17, D22;
                   F5'). The unconditional ack asserted "consumed" on the
                   orphan's behalf: if the orphan then rejected, [release]
                   reopened the seq, but the client had already dropped the
                   message on this ack and no retransmission was coming -
                   silent loss. Parking makes the ack wait for the attempt's
                   own verdict: [Landed] lets it flow; [Released] drops it,
                   and the client's retry reads [Fresh] after the release
                   and re-applies. [None] - no in-flight row - is the
                   ordinary stale replay and keeps the immediate-ack path,
                   because an unacknowledged duplicate is a replay loop that
                   never terminates. Both branches are [unit -> _] closures
                   applied exactly once ([~none:] is eager). The parked wait
                   rides its own [Lwt.protected], so THIS socket's teardown
                   stays prompt while the shared settlement (a [wait]
                   promise, non-cancelable) is untouched for its sibling
                   waiters. *)
                Ack_park.find park ~replica ~tab ~seq:n
                |> Option.fold
                     ~none:(fun () ->
                       ack n;
                       pump ())
                     ~some:(fun (settlement : Ack_park.outcome Lwt.t) () ->
                       (* The park is a blocking point like any other in the
                          pump, so it keeps the pump's death-race
                          discipline: raced against [died] through the
                          once-minted [died_park_arm], behind the same
                          deterministic state pre-check. A settlement that
                          never arrives - a stalled persist, or a row
                          superseded out from under its attempt - can then
                          no longer wedge this socket past its sender's
                          death and hold [finalize]'s teardown (the watch,
                          the stream close, [drained]) hostage. [choose]
                          never cancels the loser: the shared settlement
                          stays untouched for its sibling waiters. *)
                       match Lwt.state died with
                       | Lwt.Return () -> Lwt.return_unit
                       | Lwt.Fail (_ : exn) -> Lwt.return_unit
                       | Lwt.Sleep ->
                         let* arrival =
                           Lwt.choose
                             [ died_park_arm
                             ; Lwt.map
                                 (fun (o : Ack_park.outcome) -> `Settled o)
                                 (Lwt.protected settlement)
                             ]
                         in
                         (match arrival with
                          | `Died -> Lwt.return_unit
                          | `Settled Ack_park.Landed ->
                            ack n;
                            pump ()
                          | `Settled Ack_park.Released -> pump ()))
                |> fun step -> step ()
              | Replay_guard.Gapped ->
                (* Ignore it, and do NOT end the session: ending it would hand
                   a same-session tab a socket-kill primitive against its
                   sibling. An honest client cannot produce a gap. *)
                pump ())
            (admit ~tab ~seq))
        ~error:(fun (Codec.Decode_failed (_ : string)) -> Lwt.return_unit)
    in
    let drained = sender () in
    Lwt.finalize
      (fun () -> pump ())
      (fun () ->
        (* Teardown is unconditional on WHY the pump's promise settled:
           [finalize]'s cleanup runs after fulfilment and after rejection
           alike, so a [handle_frame] failure cannot skip it. Order matters
           and is load-bearing: [unwatch] first, so no NEW watch callback can
           be dispatched after the stream closes. An already-in-flight
           delivery is NOT joined: one that read the registration before
           [unwatch] removed it can still push against the closed stream,
           where the push rejects and irmin's own protect logs it - the
           frame had nowhere to go either way, so the guarantee is "no frame
           is silently minted after teardown", not "no callback runs". Then
           [push None], and the cleanup BINDS [drained]: closing the stream
           is what lets the backgrounded sender's [iter_s] finish the queue
           and terminate, and awaiting it here is what makes "the sender
           drains" true rather than hoped - the last ack of a
           promptly-closed session is on the wire before the session promise
           settles and the socket can be closed over it. A sender that
           already died resolved [drained] in its own catch arm, so this
           bind never parks on a dead peer. The inner [finalize] keeps the
           close unconditional even when [unwatch] itself rejects, while
           still letting that rejection propagate, exactly as it did
           before. *)
        Lwt.finalize
          (fun () -> St.unwatch w)
          (fun () ->
            push None;
            drained))

  (* Cross-site WebSocket hijacking gate. The full CSWSH rationale now lives on
     [Tea_safe.Origin_gate]'s doc in tea_safe.mli; [accept_ws] is the only
     function that names [Dream.websocket], and it demands the proof that
     [Origin_gate.check] mints, so a socket accepted without the same-origin
     check cannot be expressed here. *)
  let accept_ws (_ : Tea_safe.Origin_gate.same_origin Tea_safe.Proof.t)
      ~(coalesce : A.msg Tea_core.Coalesce_spec.t) ~(guard : Durable_guard.t)
      ~(park : Ack_park.t) (repo : St.t) (request : Dream.request) :
      Dream.response Lwt.t =
    with_session repo request (fun s ->
        Dream.websocket (fun ws ->
            live_session ~coalesce ~guard ~park s
              { send_frame = Dream.send ws
              ; receive_frame = (fun () -> Dream.receive ws)
              }))

  let handle_ws ~(coalesce : A.msg Tea_core.Coalesce_spec.t)
      ~(guard : Durable_guard.t) ~(park : Ack_park.t) (repo : St.t)
      (request : Dream.request) : Dream.response Lwt.t =
    Tea_safe.Origin_gate.check
      ~origin:(Dream.header request "Origin")
      ~host:(Dream.header request "Host")
    |> Result.fold
         ~ok:(fun proof -> accept_ws proof ~coalesce ~guard ~park repo request)
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

  (* [?interpose] fires between the witness and the guarded move - the one
     window that matters - so the denial arm is testable in program order,
     mirroring the [step] seam. Default no-op; production never passes it. *)
  let handle_undo ?(interpose : St.session -> unit Lwt.t = fun (_ : St.session) -> Lwt.return_unit)
      (repo : St.t) (request : Dream.request) : Dream.response Lwt.t =
    with_session repo request (fun s ->
        with_form request (fun _fields ->
            let* w = St.load_based s in
            let* () = interpose s in
            let* undone = St.undo w in
            Result.fold undone
              ~ok:(fun (_ : St.model) -> Dream.redirect request "/")
              ~error:(fun (e : St.undo_error) ->
                match e with
                (* At the history root undo is a no-op; re-render. *)
                | St.At_root -> Dream.redirect request "/"
                (* A commit landed past the witness (step 19, R10b): surface
                   the refusal instead of re-rendering it as a success. *)
                | St.Branch_moved -> Dream.redirect request "/?undo=denied")))

  (* --- Assembly ----------------------------------------------------------- *)

  (* [?client_dir]: also serve a compiled js_of_ocaml client bundle (the
     [/app] pages), so the SSR tier, the live client, and the WebSocket share
     one origin — which is precisely what {!same_origin} and the shared Dream
     session cookie require. *)
  let router ?(client_dir : string option) ?(rpc : Dream.route list = [])
      ?(coalesce = Tea_core.Coalesce_spec.Keep_all) ?(guard = guard)
      ?(park = park) ?(undo_interpose : (St.session -> unit Lwt.t) option)
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
       ; Dream.post undo_path (handle_undo ?interpose:undo_interpose repo)
       ; Dream.get ws_path (handle_ws ~coalesce ~guard ~park repo)
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
  let handler ?client_dir ?rpc ?coalesce ?guard ?park ?undo_interpose
      ?(sessions = Session_secret.memory) (repo : St.t) : Dream.handler =
    (* Sessions OUTSIDE the security headers, exactly as before: the header
       middleware decorates whatever response comes back, including the
       redirects and 403s the session layer itself can produce. *)
    Session_secret.middleware sessions
      (secure_headers
         (router ?client_dir ?rpc ?coalesce ?guard ?park ?undo_interpose repo))

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
  let serve ?(interface = "localhost") ?(port = 8080) ?client_dir ?rpc ?rpc_once
      ?coalesce ?reaper () : unit =
    let repo = Lwt_main.run (Store.create ()) in
    (* [?rpc_once] takes a route BUILDER, not a built list, because the value it
       needs does not exist until the entry point makes it. On this tier that is
       merely a fresh mem channel; on {!Tea_server_pack.Make_pack.serve_pack} it
       is a guard over a journal that same function opens and closes, so the app
       could not have composed it beforehand even in principle. Keeping both
       tiers on the one shape means an app moves between them by swapping the
       entry point, not by restructuring its wiring.

       The builder also receives the STORE, and that is not a convenience: an
       app's keyed handler is a function of the repo (it commits through
       {!step}), and this entry point is where the repo is created, so a builder
       that saw only the channel could be written by no app that touches its own
       model. The counter, whose pack tier has no RPC at all, is why the
       narrower shape survived its first increment unchallenged.

       Both [Option.fold] arms are closures applied once: [~none:] is eager, and
       an eager arm here would mint a guard and a reply cache for every caller
       that never asked for the keyed channel. *)
    let rpc =
      Option.fold rpc_once ~none:(fun () -> rpc)
        ~some:(fun (build : Store.t -> Rpc_once.t -> Dream.route list) () ->
          Some
            (Option.value rpc ~default:[]
            @ build repo (Rpc_once.mem ~floor_replica:Store.main_replica)))
        ()
    in
    (* [?reaper] (roadmap step 22, D24): the pipeline and the sweep must
       judge replays against ONE guard, so when the reaper is on the guard is
       minted HERE - the router's own default construction, null sink over
       empty floors - and handed to [handler ?guard], the argument that
       existed "for tests" and finds its load-bearing use. The in-memory
       floors this tier accumulates are exactly as loss-prone after a reap as
       durable ones, which is why this tier reaps at all. The [forget]
       closure is [Tea_server_pack.forget_into]'s local twin (that library
       depends on this one, not the reverse). No teardown exists on this tier
       (Dream.run never returns), so the loop's stop promise never resolves
       and the loop dies with the process: [Lwt.async] needs no fence from a
       close that never happens. The sweep's [now] is [Store.default_now],
       the wall family [Store.create ()] seeded the clock from. *)
    let guard =
      Option.map
        (fun (r : Reaper.spec) ->
          let guard =
            Durable_guard.v ~sessions:Replay_guard.default_sessions
              ~tabs:Replay_guard.default_tabs ~sink:Guard_sink.null
              ~floors:Durable_guard.Floors.empty ()
          in
          let forget (sid : Tea_core.Prim.Session_id.t) : unit Lwt.t =
            let open Lwt.Syntax in
            let* forgotten =
              Durable_guard.forget guard ~replica:(Tea_core.Crdt.Replica.v sid)
            in
            Result.fold forgotten ~ok:Lwt.return
              ~error:(fun (e : Guard_sink.err) ->
                let reason =
                  match e with
                  | Guard_sink.Sink_closed -> "sink closed"
                  | Guard_sink.Io io -> io
                in
                Printf.eprintf
                  "tea_server: reaper: forget failed for session %s (%s); continuing the sweep\n%!"
                  (Tea_core.Prim.Session_id.to_string sid)
                  reason;
                Lwt.return_unit)
          in
          let sweep ~(now : int64) : int Lwt.t =
            let open Lwt.Syntax in
            let* swept = Store.reap ~forget repo ~ttl:r.ttl ~now in
            (if swept > 0 then
               Printf.eprintf
                 "tea_server: reaper: swept %d idle session branch(es)\n%!"
                 swept);
            Lwt.return swept
          in
          Printf.eprintf
            "tea_server: reaper: sweeping session branches idle longer than %.0fs, every %.0fs\n%!"
            (Tea_core.Prim.Ttl.to_seconds r.ttl)
            (Reaper.Cadence.to_seconds r.every);
          Lwt.async (fun () ->
              Reaper.loop ~sweep ~now:Store.default_now
                ~timer:(fun (c : Reaper.Cadence.t) ->
                  Lwt_unix.sleep (Reaper.Cadence.to_seconds c))
                ~every:r.every
                ~stop:(fst (Lwt.wait ())));
          guard)
        reaper
    in
    Dream.run ~interface ~port
      (Dream.logger (handler ?client_dir ?rpc ?coalesce ?guard repo))
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
      (* A [Mutating] endpoint's 200 body ALWAYS rides the [keyed_resp]
         envelope, keyed request or not (roadmap step 15): the client's
         [expect] closure is fixed when [call] builds the command and cannot
         see whether the runtime attached a delivery key, so a body shape that
         varied with the header would be undecodable by construction. Until
         the guard lands, every enveloped answer is [Reply]; [Replayed] is what
         the de-duplicating route will add. [Read_only] is untouched.

         The match is wildcard-free, so a third kind cannot silently pick a
         wire shape here. *)
      let encode_reply resp =
        (* Unannotated on purpose: [resp] has the existential type [Any] bound,
           which a written-out ['resp] would rebind to a fresh variable and
           then let escape its scope. *)
        match (Api.kind ep : Tea_rpc.endpoint_kind) with
        | Read_only -> R.encode_resp ep resp
        | Mutating ->
          Tea_core.Codec.to_json
            (Tea_rpc.keyed_resp_t (Api.resp_t ep))
            (Tea_rpc.Reply resp)
      in
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
                     (encode_reply resp))
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

  (** The floor-tab derivation (roadmap step 15, D20.1): the namespace half of
      the delivery key, and the reason the wire half can be forged freely
      without reaching anyone else's floor.

      The tab in [x-tea-key] is client-asserted, so it never keys a floor on
      its own. What keys a floor is
      [Digest.to_hex (Digest.string ("tea-rpc-floor\000" ^ session_id_hex ^ "\000" ^ client_tab))],
      which binds the session cookie to the client stream. That is exactly 32
      lowercase hex characters, exactly {!Tea_core.Prim.Tab_id}'s grammar, so
      the parse back succeeds by construction; its unreachable error arm folds
      to [None], which the route treats as keyless — the accept direction,
      never an exception.

      The journal therefore records the DERIVATION, not the input. The tab-id
      disclosure DESIGN R14 documents confers no floor authority: an attacker
      who wants a victim's floor needs the victim's cookie, which is the WS
      tier's own precondition (T13). Both halves are hex, so neither can
      contain the NUL that separates them and no two distinct (session, tab)
      pairs can slide into one digest input.

      Honest about the primitive (R23): this is the unkeyed stdlib [Digest]
      (MD5, the {!Session_secret} precedent). Only preimage and second-preimage
      resistance are relied on, and an attacker cannot choose the session half,
      so collision weakness does not apply; a secret-keyed derivation is the
      upgrade if the session secret is ever plumbed to this route.

      On the pack tier the derivation is stable across restarts precisely
      because D17 made identity durable. Under {!Session_secret.memory} it is
      not, which is that tier's in-process-only sentence restated: a restart
      lands every tab in a fresh namespace, so nothing is deduplicated across
      it. *)
  let floor_tab ~(session_id_hex : string) ~(client_tab : Tea_core.Prim.Tab_id.t) :
      Tea_core.Prim.Tab_id.t option =
    "tea-rpc-floor\000" ^ session_id_hex ^ "\000"
    ^ Tea_core.Prim.Tab_id.to_string client_tab
    |> Digest.string |> Digest.to_hex |> Tea_core.Prim.Tab_id.of_string
    |> Result.fold
         ~ok:(fun (t : Tea_core.Prim.Tab_id.t) -> Some t)
         ~error:(fun (_ : Tea_core.Prim.Tab_id.err) ->
           (* Unreachable: [Digest.to_hex] is 32 lowercase hex by construction.
              [None] is the accept direction regardless. *)
           None)

  (** The keyed handler: the rank-2 {!handler} plus the one fact the guard
      cannot learn on its own, which store water the effect's commit returned.

      {!Tea_core.Prim.Store_water.bottom} when nothing committed — a no-op
      update, a fuel-exhausted step, an app-level rejection — because the D16
      "taken, never applied" sentence needs exactly that arm: a floor claiming
      a water no commit ever minted would survive a boot filter it should not.
      The app already holds the value as [step_outcome.water], which is
      [commit]'s own winning-round mint and never a head read (a head read
      could belong to a later writer). *)
  type keyed_handler =
    { handle_keyed :
        'req 'resp.
        ('req, 'resp) Api.t -> 'req -> ('resp * Tea_core.Prim.Store_water.t) Lwt.t
    }

  (** The exactly-once channel's server half, built once per app at wiring
      time.

      [floor_replica] names the ONE branch keyed handlers may commit on: the
      single-branch contract, a wiring-time declaration in the idiom of
      "request-free by contract", pinned by a test rather than hoped. It is
      what keeps the boot filter's question meaningful and
      {!Durable_guard}'s only-rollback-is-evidence law true — floors keyed on a
      branch the filter never checks would be adopted blind. Multi-branch apps
      need a [floor_replica] per route group (DESIGN R26); the journal, filter
      and [Floors] already handle arbitrary replicas, so that extension is
      wiring rather than redesign.

      [replies] is deliberately in-process. A durable reply store would be a
      fourth restore-coherence artifact beside the pack root, the journal and
      the secret, and the consumed marker and the reply bytes would then have
      to land as ONE record with their own witness and boot filter (R20
      recursed). Losing the bytes is the direction this family always
      degrades toward: a visible [Replayed], never a silent second apply.

      The record itself lives at {!Rpc_once}, one nominal type shared by every
      instantiation, because the entry points that build it (and own the
      journal behind [guard]) are not parameterised by any [Api]. The
      re-export below carries the fields, so [Rpc.once] reads and constructs
      exactly as a functor-local record would. *)
  type once = Rpc_once.t =
    { guard : Durable_guard.t
    ; replies : Reply_cache.Cell.cell
    ; floor_replica : Tea_core.Crdt.Replica.t
    }

  (** The mem tier's [once], which is D16's default exactly: the RPC guard over
      {!Guard_sink.null} and empty floors, plus a fresh reply cache. Nothing
      here outlives the process, which is the whole of what the mem tier ever
      promised. The durable tier composes the same record over a journal-backed
      sink instead. *)
  let once_mem ~(floor_replica : Tea_core.Crdt.Replica.t) () : once =
    Rpc_once.mem ~floor_replica

  let route_once ?(on_taken = fun () -> Lwt.return_unit) (o : once) (h : keyed_handler)
      (p : Api.any) : Dream.route =
    match p with
    | Api.Any ep ->
      (* The reply cache keys on the endpoint as well as the seq (D20.2, J3):
         the mounted path is this endpoint's unique wire identity already, so
         reusing it here cannot drift from what the router answers. The
         endpoint is defence in depth, not a key axis — a client that reuses
         one seq across two endpoints is malformed, and this makes it read
         [Replayed] rather than another endpoint's bytes. *)
      let endpoint = Tea_core.Prim.Rpc_path.to_string (R.path_of ep) in
      let ok_json (payload : string) : Dream.response Lwt.t =
        Dream.respond ~status:`OK ~headers:json_content_type payload
      in
      let enveloped resp =
        (* Unannotated for the same reason [route]'s [encode_reply] is: [resp]
           carries the existential bound by [Any]. *)
        Tea_core.Codec.to_json (Tea_rpc.keyed_resp_t (Api.resp_t ep)) (Tea_rpc.Reply resp)
      in
      let replayed_body : string =
        Tea_core.Codec.to_json (Tea_rpc.keyed_resp_t (Api.resp_t ep)) Tea_rpc.Replayed
      in
      (* Step 1 of the pump, shared by both kinds: content-type gate, streaming
         size cap, decode. Everything that can refuse without consuming refuses
         here, BEFORE any [take]. *)
      let with_decoded (request : Dream.request) k : Dream.response Lwt.t =
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
                 ~ok:k
      in
      (* [Read_only] dispatches exactly as today: no key is read, no floor is
         touched, and the answer is a bare ['resp]. The water a keyed handler
         returns is meaningless here and is discarded rather than recorded. *)
      let dispatch_read_only (request : Dream.request) : Dream.response Lwt.t =
        with_decoded request (fun req ->
            let* resp, (_ : Tea_core.Prim.Store_water.t) = h.handle_keyed ep req in
            Dream.respond ~status:`OK ~headers:json_content_type (R.encode_resp ep resp))
      in
      (* Steps 2 and 3 of the pump's legacy arm: no key at all, or a
         derivation that declined. Today's semantics — no dedup, no consume —
         and the answer is still enveloped, because the 200 shape is fixed by
         KIND, not by the request. *)
      let unkeyed req : Dream.response Lwt.t =
        let* resp, (_ : Tea_core.Prim.Store_water.t) = h.handle_keyed ep req in
        ok_json (enveloped resp)
      in
      (* Steps 3 to 6: the verdict and its arms. *)
      let pump req (floor : Tea_core.Prim.Tab_id.t) (seq : Tea_core.Prim.Msg_seq.t) :
          Dream.response Lwt.t =
        match Durable_guard.take o.guard ~replica:o.floor_replica ~tab:floor ~seq with
        | Replay_guard.Gapped ->
          (* Nothing consumed, nothing applied, no state touched. The seq space
             is dense per tab (the client queue is one-in-flight over dense
             numbering), so an honest client never gaps. The WS tier's
             silent-ignore arm is unavailable because HTTP must answer
             something, and answering on the 200 channel would type a protocol
             violation into every endpoint's contract. *)
          Dream.respond ~status:`Bad_Request ~headers:text_plain "rpc delivery gap"
        | Replay_guard.Duplicate (_ : Tea_core.Prim.Msg_seq.t) ->
          (* Never invoke the handler, never persist: the effect already
             happened, and this arm exists only to answer for it. *)
          (match Reply_cache.Cell.find o.replies ~tab:floor ~seq ~endpoint with
          | Reply_cache.Busy ->
            (* The taken delivery is still inside its window. 503 means "the
               effect's fate is unknown at this instant, ask again", a
               transport-channel meaning the client runtime's 5xx retry arm
               consumes; it never reaches the app's [expect]. *)
            Dream.respond ~status:`Service_Unavailable ~headers:text_plain
              "rpc delivery in flight"
          | Reply_cache.Original bytes ->
            (* Byte-identical to what the one taken delivery computed: replayed
               verbatim, never recomputed at replay time. *)
            ok_json bytes
          | Reply_cache.Gone ->
            (* Effect certain, value lost. The typed arm the whole envelope
               exists for; the client surfaces it as [Applied_reply_lost]. *)
            ok_json replayed_body)
        | Replay_guard.Fresh (_ : Tea_core.Prim.Msg_seq.t) ->
          (* [mark_pending] in the SAME continuation as the verdict, with no
             Lwt bind between them: a yield here would let a concurrent
             duplicate read "no entry" and be told [Replayed] for an effect
             still running, which is the one answer that would be a lie. *)
          Reply_cache.Cell.mark_pending o.replies ~tab:floor ~seq;
          Lwt.try_bind
            (fun () ->
              let* () = on_taken () in
              h.handle_keyed ep req)
            (fun (resp, water) ->
              let body = enveloped resp in
              let* persisted =
                Durable_guard.persist o.guard ~replica:o.floor_replica ~tab:floor ~seq
                  ~water
              in
              let () =
                Result.fold persisted
                  ~ok:(fun () -> ())
                  ~error:(fun (e : Guard_sink.err) ->
                    (* One stderr line and in-process semantics, never a failed
                       response: the effect landed, and refusing to answer for it
                       would lose the reply on top of the floor. The pump
                       precedent. *)
                    let reason =
                      match e with
                      | Guard_sink.Sink_closed -> "sink closed"
                      | Guard_sink.Io reason -> reason
                    in
                    Printf.eprintf
                      "tea_server: rpc delivery floor NOT recorded (%s); this delivery keeps in-process semantics and may re-apply as a visible duplicate after a restart\n%!"
                      reason)
              in
              Reply_cache.Cell.settle o.replies ~tab:floor ~seq ~endpoint ~body;
              (* The 200 IS this channel's acknowledgement, and it is written
                 strictly after the persist attempt completes, on BOTH persist
                 outcomes. Answering first would acknowledge a delivery whose floor
                 might never reach the journal. *)
              ok_json body)
            (fun (exn : exn) ->
              (* The barrier (R27): the delivery was consumed but its apply
                 REJECTED before any floor could be appended. Un-take in this
                 same continuation: the retry must read [Fresh] and re-run the
                 handler, because a consumed seq with no effect drains through
                 the reply cache into a 200 [Replayed] for an effect that never
                 happened, the silent-loss direction the family forbids. The
                 [Pending] marker is deliberately LEFT standing: a duplicate
                 racing the retry must read [Busy] (503, ask again), never
                 [Gone] and a [Replayed] lie, and the retry's own [Fresh]
                 re-take re-marks the window. If the handler half-applied
                 before rejecting, the retry re-applies: a visible convergent
                 duplicate, the licensed direction. [release] is conditional on
                 the high water still being exactly [seq], so a non-conforming
                 pipeliner's later take is never un-taken by this one's
                 failure. *)
              Durable_guard.release o.guard ~replica:o.floor_replica ~tab:floor ~seq;
              Printf.eprintf
                "tea_server: rpc keyed handler failed before persist (%s); delivery released for retry\n%!"
                (Printexc.to_string exn);
              Dream.respond ~status:`Internal_Server_Error ~headers:text_plain
                "rpc handler failed")
      in
      (* The gated path demands the [same_origin] proof, so it cannot be taken
         without one — the origin gate runs first, before the body is read and
         long before anything is consumed. *)
      let dispatch_mutating (_ : Tea_safe.Origin_gate.same_origin Tea_safe.Proof.t)
          (request : Dream.request) : Dream.response Lwt.t =
        with_decoded request (fun req ->
            (* Step 1's last refusal: the key parse. A malformed key is a 400
               that burns no sequence number. *)
            (Dream.header request Tea_rpc.key_header
            |> Option.fold
                 ~none:(fun () -> unkeyed req)
                 ~some:(fun (raw : string) () ->
                   Tea_rpc.Key.of_string raw
                   |> Result.fold
                        ~error:(fun (_ : Tea_rpc.Key.err) ->
                          Dream.respond ~status:`Bad_Request ~headers:text_plain
                            "malformed rpc delivery key")
                        ~ok:(fun (key : Tea_rpc.Key.t) ->
                          (floor_tab
                             ~session_id_hex:(hex (Dream.session_id request))
                             ~client_tab:(Tea_rpc.Key.tab key)
                          |> Option.fold
                               ~none:(fun () -> unkeyed req)
                               ~some:(fun (floor : Tea_core.Prim.Tab_id.t) () ->
                                 pump req floor (Tea_rpc.Key.seq key)))
                            ())))
              ())
      in
      Dream.post
        (Tea_core.Prim.Rpc_path.to_string (R.path_of ep))
        (fun request ->
          match (Api.kind ep : Tea_rpc.endpoint_kind) with
          | Read_only -> dispatch_read_only request
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

  (** One POST route per {!Api.all} element, as {!routes}, but with the
      [Mutating] tier under the exactly-once guard (roadmap step 15, D20).
      [Read_only] endpoints dispatch exactly as today.

      [?on_taken] is a tests-only seam defaulting to [Lwt.return_unit], invoked
      between the [Fresh] verdict (with its synchronous [mark_pending] already
      recorded) and the handler call. It is the only way to land a duplicate or
      a sibling writer inside the one window that matters while still driving
      the REAL app handler, which is what keeps the two-writer and 503 checks
      honest instead of testing a forked copy of the pipeline. *)
  let routes_once ?on_taken (o : once) (h : keyed_handler) : Dream.route list =
    List.map (route_once ?on_taken o h) Api.all
end
