module Prim = Tea_core.Prim
module Subs = Tea_client.Subs
module W = Js_browser.Window
module WS = Js_browser.WebSocket
module X = Js_browser.XHR
module Rpc_delivery = Tea_client.Rpc_delivery
module Backoff = Tea_client.Reconnect.Backoff

let window = Js_browser.window

let log (line : string) =
  Js_browser.Console.log Js_browser.console (Ojs.string_to_js line)

(* The page's delivery identities, minted here rather than inside {!Start_local}
   because the keyed RPC channel below is a peer of the mount, not a part of it
   (roadmap step 15, I5). A session cookie names a session, not a sender: two
   tabs share one cookie, one branch and one replica id, so without this both
   would number their messages from 1 and the server would read the second
   tab's first edit as the first tab's replay (D15).

   [Random.self_init] is the entropy source rather than a [crypto] binding:
   these values are de-duplication keys inside an already-authenticated
   session, not credentials, so what they need is collision-freedom between a
   handful of tabs on one machine, not unpredictability against an attacker.
   Under js_of_ocaml [self_init] seeds from the JS RNG. A collision would
   degrade exactly to the two-tabs bug this closes, which is why the mint is
   one expression with no fallback path to drift from.

   Seeded ONCE at module load and drawn from twice, rather than re-seeded per
   mint: this page now mints two ids back to back (the websocket tab and the
   RPC channel's), and two seedings that close together would rest their
   independence on the resolution of the seed source, where two draws from one
   seeded generator are independent by construction. *)
let () = Random.self_init ()
let mint_tab () : Prim.Tab_id.t = Prim.Tab_id.of_draws (fun () -> Random.int 256)

(* [Status.of_int 0 = None] is the network-failure classifier: offline, DNS
   failure, a refused connection and an aborted request all surface as XHR
   status 0. *)
let outcome_of (xhr : X.t) : (string, Tea_core.Cmd.http_failure) result =
  Prim.Status.of_int (X.status xhr)
  |> Option.fold
       ~none:(Error Tea_core.Cmd.Network_error)
       ~some:(fun (st : Prim.Status.t) ->
         if Prim.Status.is_success st then Ok (X.response_text xhr)
         else Error (Tea_core.Cmd.Http_status st))

(* One same-origin JSON POST. Both delivery channels go through this, so the
   content type and the readiness classification cannot drift between them -
   the difference between a bare call and a keyed one is [headers] and who owns
   the outcome, nothing about the request itself. *)
let post ~(path : string) ~(headers : (string * string) list) ~(body : string)
    ~(settle : (string, Tea_core.Cmd.http_failure) result -> unit) : unit =
  let xhr = X.create () in
  X.open_ xhr "POST" path;
  X.set_request_header xhr "Content-Type" "application/json";
  List.iter
    (fun ((name : string), (value : string)) -> X.set_request_header xhr name value)
    headers;
  X.set_onreadystatechange xhr (fun () ->
      match X.ready_state xhr with
      | X.Done -> settle (outcome_of xhr)
      | X.Unsent | X.Opened | X.Headers_received | X.Loading -> ()
      | X.Other (_ : int) -> ());
  X.send xhr (Ojs.string_to_js body)

(* --- The keyed RPC channel (roadmap step 15, D20, I5) ---------------------

   Page-global, exactly like the mount it serves: one page is one RPC channel,
   one tab id, one dense sequence. It lives outside {!Start_local} because the
   command interpreter does, and the interpreter is where a keyed command
   arrives.

   The queue is instantiated at [unit] rather than at an app's msg type because
   [env]'s handler is polymorphic in msg: the only way to keep ONE queue for
   the page is to store each continuation already applied to its context.
   [Vdom_blit.Cmd.send_msg ctx] is that application, and firing it from an
   async callback is the established [After]/[set_timeout] precedent. *)
module Keyed = struct
  let queue : unit Rpc_delivery.t ref = ref (Rpc_delivery.v ~tab:(mint_tab ()))

  (* [inflight] is a request that has been sent and not yet settled;
     [waiting] is a settled failure whose retry is on the backoff ladder. The
     two together are the G4 shell invariant, AT MOST ONE LIVE REQUEST PER HEAD
     SEQ, and they hold it in the strongest form available: rather than abort a
     request when a retry comes due, {!pump} never schedules the retry until
     the request it retries has settled, so the two never overlap at all. The
     spec's abort is then unreachable, which is why none is issued (js_browser
     binds no [abort], and reaching past it through [Ojs] would buy nothing).

     What that costs is named honestly: a request that never settles stalls the
     queue behind it until the browser's own connection timeout settles it as
     status 0. It cannot lose or duplicate a call, only delay one. *)
  let inflight : bool ref = ref false
  let waiting : W.timeout_id option ref = ref None
  let backoff : Backoff.t ref = ref Backoff.initial

  (* The only place a keyed request starts, and the only place a retry is
     scheduled. A retry is [head] re-sent under the key it was recorded with:
     key stability needs no discipline here because no operation in
     {!Rpc_delivery} renumbers a recorded entry. *)
  let rec pump () : unit =
    if (not !inflight) && Option.is_none !waiting then
      Option.iter send (Rpc_delivery.head !queue)

  and send ((seq, entry) : Prim.Msg_seq.t * unit Rpc_delivery.entry) : unit =
    inflight := true;
    post ~path:entry.path ~body:entry.body
      ~headers:
        [ ( Tea_rpc.key_header
          , Tea_rpc.Key.to_string
              (Tea_rpc.Key.v ~tab:(Rpc_delivery.tab !queue) ~seq) )
        ]
      ~settle:(settle seq entry)

  and settle (seq : Prim.Msg_seq.t) (entry : unit Rpc_delivery.entry)
      (outcome : (string, Tea_core.Cmd.http_failure) result) : unit =
    inflight := false;
    Result.fold outcome
      ~ok:(fun (body : string) ->
        (* Acknowledge BEFORE dispatching. [expect] runs the app's [update]
           synchronously and that update may record the next keyed call, which
           would otherwise queue behind an entry the server has already
           answered - and a duplicate response could not fire [expect] twice
           for one entry even in principle. *)
        queue := Rpc_delivery.ack seq !queue;
        backoff := Backoff.initial;
        entry.expect (Ok body);
        pump ())
      ~error:(fun (failure : Tea_core.Cmd.http_failure) ->
        match failure with
        (* The retryable half: the effect's fate is unknown, which is precisely
           the case the server-side guard was built to absorb. The framework's
           503 for a delivery still in flight (D20.2) arrives here as a 5xx and
           is retried like any other. *)
        | Tea_core.Cmd.Network_error -> retry ()
        (* Unreachable from {!outcome_of}, which mints only the two wire
           outcomes, and terminal rather than retryable on purpose: an
           interpreter that answers "no transport" is not one a backoff ladder
           will talk round. Kept as its own arm so that a future runtime which
           CAN produce it inherits the honest behaviour instead of a spin. *)
        | Tea_core.Cmd.No_transport ->
          queue := Rpc_delivery.ack seq !queue;
          backoff := Backoff.initial;
          entry.expect (Error failure);
          pump ()
        | Tea_core.Cmd.Http_status st ->
          if Prim.Status.to_int st >= 500 then retry ()
          else (
            (* 4xx is poison: a rejected key, a rejected body or a lost session
               will be rejected identically forever, so retrying is a spin. The
               entry leaves, the app hears the failure, and the tab rotates -
               under a fresh tab there is no floor, and an absent floor accepts
               any first sequence number, so the numbers this tab already
               burned stop mattering. *)
            queue := Rpc_delivery.ack seq !queue;
            queue := Rpc_delivery.rotate ~tab:(mint_tab ()) !queue;
            backoff := Backoff.initial;
            entry.expect (Error failure);
            pump ()))

  and retry () : unit =
    let delay = Backoff.to_ms !backoff in
    backoff := Backoff.bump !backoff;
    waiting :=
      Some
        (W.set_timeout window
           (fun () ->
             waiting := None;
             pump ())
           delay)

  let record ~(path : string) ~(body : string)
      ~(expect : (string, Tea_core.Cmd.http_failure) result -> unit) : unit =
    (Rpc_delivery.record { Rpc_delivery.path; body; expect } !queue
    |> Option.fold
         ~none:(fun () ->
           (* The [Delivery.record] defined arm, unreachable in a page life.
              Reported rather than dropped in silence: this call's continuation
              will now never fire, which an app reads as a hang, not a no-op. *)
           log "tea: keyed RPC sequence space exhausted; call not sent")
         ~some:(fun
             ((q, (_ : Prim.Msg_seq.t * unit Rpc_delivery.entry)) :
               unit Rpc_delivery.t * (Prim.Msg_seq.t * unit Rpc_delivery.entry))
             ()
           ->
           queue := q;
           pump ()))
      ()
end

(* The command handler contract: react to the commands you recognize and
   return [true]; [false] passes the command along. [Vdom.Cmd.t] is an open
   (extensible) type — `type 'msg t = ..` — so the trailing catch-all is the
   protocol itself, not a shortcut over a finite sum: exhaustive enumeration
   of an open type is impossible by construction. *)
let env =
  Vdom_blit.cmd
    { f =
        (fun ctx cmd ->
          match cmd with
          | Tea_client.After (ms, msg) ->
            (* The timeout id is deliberately dropped: {!Start} mounts one app
               for the lifetime of the page and never disposes it, so there is
               no teardown for a pending timer to outlive. If a dispose path
               ever appears (multi-mount, live-view reload), the ids must be
               tracked and [clear_timeout]-ed there — see DESIGN §7. *)
            let (_ : W.timeout_id) =
              W.set_timeout window (fun () -> Vdom_blit.Cmd.send_msg ctx msg) ms
            in
            true
          | Tea_client.Navigate url ->
            Js_browser.History.push_state (W.history window) Ojs.null "" url;
            true
          | Tea_client.Http { path; body; delivery; expect } -> (
            (* The wire half of [Tea_rpc.Make.call]: POST the encoded request,
               classify the transport outcome, feed it to the [expect]
               continuation the typed layer built. [send_msg] from an async
               callback is the established [After]/[set_timeout] precedent.

               The two arms differ in who owns the outcome, not in what is
               sent (roadmap step 15, D20). Spelled as an exhaustive match, so
               a third delivery contract is a compile error here - which is the
               whole reason [Http_delivery] is a sum rather than a flag. *)
            match delivery with
            | Tea_core.Cmd.Http_delivery.Bare ->
              (* Today's semantics, unchanged: one request, no key, no retry,
                 and the app hears whatever came back. *)
              post ~path ~headers:[] ~body ~settle:(fun outcome ->
                  Vdom_blit.Cmd.send_msg ctx (expect outcome));
              true
            | Tea_core.Cmd.Http_delivery.Keyed ->
              (* Retry ownership moves into the runtime: the call is recorded
                 first and sent from the queue, so a failure re-sends it under
                 the key it already has instead of surfacing as a transport
                 error the app would have to retry itself - which it could only
                 do by calling again, i.e. under a NEW key, i.e. as a second
                 delivery of the same effect. *)
              Keyed.record ~path ~body ~expect:(fun outcome ->
                  Vdom_blit.Cmd.send_msg ctx (expect outcome));
              true)
          | unrecognized ->
            ignore unrecognized;
            false)
    }

module Start_local
    (A : Tea_core.App.APP)
    (L : Tea_core.Local.LOCAL with type shared = A.model and type msg = A.msg) =
struct
  module Client = Tea_client.Make_local (A) (L)
  module Codec = Tea_core.Codec.Make (A)
  module Rc = Tea_client.Reconnect
  module Delivery = Tea_client.Delivery
  module Channel = Tea_client.Local_channel
  module Store = Tea_client.Local_store.Make (A)
  module Flow = Store.Flow (Idb)

  (* --- Live subscription state (one mount per page life) ------------------

     The runtime half of {!Tea_core.Sub}: [Every] becomes [setInterval],
     [Store_watch] becomes the {!Tea_core.Wire.ws_path} WebSocket. Handlers
     are looked up from the *current* model's subscriptions at fire time
     (via [Vdom_blit.get]), so the keyed resources below never hold stale
     callbacks; see {!Tea_client.Subs.key}.

     [socket : WS.t option] became [conn] in D8: a dropped link is now a state
     with a timer in it, not an absence. [outbox] is D9's other half - the
     edits made while [conn] could not send. Both are driven by pure state
     machines in [Tea_client]; everything below is the effect half. *)

  (* The websocket tab's delivery identity now lives inside the boot gate
     (D25): minted by the module-level {!mint_tab} when hydration rules out a
     persisted record, or ADOPTED from one. The keyed RPC channel draws its
     own from the same mint and must never share it: the two channels number
     their streams independently, so one id across both would make each
     channel's first frame look like a replay of the other's. *)

  type live =
    { mutable app : (Client.state, A.msg) Vdom_blit.app option
    ; mutable conn : (WS.t, W.timeout_id) Rc.t
    ; mutable gate : Store.gate
    ; mutable store_conn : Flow.conn option
    ; mutable root : Js_browser.Element.t option
    ; mutable applying_remote : bool
    ; mutable intervals : (int * W.interval_id) list
    }

  let live : live =
    { app = None
    ; conn = Rc.down
    ; gate = Store.buffering
    ; store_conn = None
    ; root = None
    ; applying_remote = false
    ; intervals = []
    }

  let dispatch (msg : A.msg) : unit =
    Option.iter (fun app -> Vdom_blit.process app msg) live.app

  let shared_model () : A.model option =
    Option.map (fun app -> Client.shared (Vdom_blit.get app)) live.app

  (* Remote-born msgs must not echo back up the socket (commit → push →
     dispatch → mirror → commit → …). [Vdom_blit.process] runs [update]
     synchronously — only the redraw is deferred — so fencing the call with a
     flag is sound in single-threaded JS. *)
  let dispatch_remote (msg : A.msg) : unit =
    live.applying_remote <- true;
    Fun.protect
      ~finally:(fun () -> live.applying_remote <- false)
      (fun () -> dispatch msg)

  let current_specs () : (A.model, A.msg) Subs.spec list =
    Option.fold ~none:[]
      ~some:(fun model -> Subs.specs_of (A.subscriptions model))
      (shared_model ())

  let fire_every (ms : int) () : unit =
    let now = int_of_float (Js_browser.Date.now ()) in
    current_specs ()
    |> List.iter (fun (s : (A.model, A.msg) Subs.spec) ->
           match s with
           | Subs.Spec_every (ms', f) -> if Int.equal ms ms' then dispatch (f now)
           | Subs.Spec_store (_ : A.model -> A.msg) -> ())

  (* --- The browser-local checkpoint (roadmap step 25, D25) ----------------

     Fire-and-forget: persistence never gates dispatch, forward or send.
     Every failure classifies into the closed [idb_error] surface, logs once
     per kind, and leaves the page exactly as live as before step 25. *)

  let err_name (e : Tea_client.Local_store.idb_error) : string =
    match e with
    | Tea_client.Local_store.Unsupported -> "Unsupported"
    | Tea_client.Local_store.Blocked -> "Blocked"
    | Tea_client.Local_store.Version_error -> "VersionError"
    | Tea_client.Local_store.Quota_exceeded -> "QuotaExceededError"
    | Tea_client.Local_store.Not_found -> "NotFoundError"
    | Tea_client.Local_store.Other s -> s

  let checkpoint_logged : string list ref = ref []

  (* Once per KIND, deduplicated on the constructor: the [Other] family
     shares one slot, so a browser that varies its error text cannot grow
     the list past the six constructors. *)
  let err_bucket (e : Tea_client.Local_store.idb_error) : string =
    match e with
    | Tea_client.Local_store.Other (_ : string) -> "Other"
    | Tea_client.Local_store.Unsupported | Tea_client.Local_store.Blocked
    | Tea_client.Local_store.Version_error
    | Tea_client.Local_store.Quota_exceeded
    | Tea_client.Local_store.Not_found ->
      err_name e

  let log_checkpoint_error (e : Tea_client.Local_store.idb_error) : unit =
    let bucket = err_bucket e in
    if List.exists (String.equal bucket) !checkpoint_logged then ()
    else (
      checkpoint_logged := bucket :: !checkpoint_logged;
      log ("tea_client_run: local checkpoint failed (" ^ err_name e ^ ")"))

  (* The record is written under the replica the queue belongs to: the
     confirmed identity once a Hello has arrived, else the ADOPTED identity
     still awaiting its verdict. A provisional page with nothing adopted has
     no honest key and skips - there is nothing worth surviving yet. *)
  let key_replica () : Tea_core.Crdt.Replica.t option =
    if Tea_client.Identity.is_provisional () then
      Store.unconfirmed_replica live.gate
    else Some (Tea_client.Identity.replica ())

  let checkpoint () : unit =
    Option.iter
      (fun (replica : Tea_core.Crdt.Replica.t) ->
        Store.delivery live.gate
        |> Option.iter (fun (delivery : A.msg Delivery.t) ->
               shared_model ()
               |> Option.iter (fun (model : A.model) ->
                      Flow.checkpoint live.store_conn
                        (Store.checkpoint_record ~replica ~delivery
                           ~clock_floor:(Tea_client.Identity.clock_floor ())
                           ~model
                           ~now_ms:(Js_browser.Date.now ()))
                        ~k:
                          (Result.fold ~ok:(fun () -> ())
                             ~error:(fun (e : Tea_client.Local_store.idb_error)
                               ->
                                 log_checkpoint_error e;
                                 (* A write failure means the store no longer
                                    tracks this page; a record that lies about
                                    [next] costs the NEXT life its first edits
                                    (R28). Clear it and degrade, once. *)
                                 Flow.invalidate live.store_conn
                                   ~k:
                                     (Result.fold ~ok:(fun () -> ())
                                        ~error:log_checkpoint_error))))))
      (key_replica ())

  (* Timeout-0 coalescing: a burst of updates in one task writes once. *)
  let checkpoint_scheduled : bool ref = ref false

  let schedule_checkpoint () : unit =
    if !checkpoint_scheduled then ()
    else (
      checkpoint_scheduled := true;
      let (_ : W.timeout_id) =
        W.set_timeout window
          (fun () ->
            checkpoint_scheduled := false;
            checkpoint ())
          0
      in
      ())

  let clear_provisional () : unit =
    Option.iter
      (fun (root : Js_browser.Element.t) ->
        Js_browser.Element.remove_attribute root "data-tea-provisional")
      live.root

  (* The optimistic mirror (DESIGN §7): every locally-born msg is also sent up
     the live socket; the server drives it through the same [update] and the
     committed head comes back as a [Store_watch] frame.

     D9: a msg born while the link is not [Up] is no longer merely "applied
     locally and logged"  -  it goes into the outbox and is replayed on
     reconnect. [Rc.sendable] is [Some] in the [Up] state only, deliberately
     including CONNECTING under "buffer it": a [send] on a connecting socket
     raises in the browser. *)
  (* One frame, sent if the link can carry it. A send that cannot happen is not
     an error and needs no branch: the entry is already recorded, so it stays in
     the queue and the next reconnect replays it (D15). *)
  let send_one ~(tab : Tea_core.Prim.Tab_id.t)
      ((seq : Tea_core.Prim.Msg_seq.t), (msg : A.msg)) : unit =
    Option.iter
      (fun ws ->
        WS.send ws
          (Codec.up_to_json
             (Tea_core.Wire.Apply
                { tab = Tea_core.Prim.Tab_id.to_string tab
                ; seq = Tea_core.Prim.Msg_seq.to_int seq
                ; msg
                })))
      (Rc.sendable live.conn)

  (* Record FIRST, send second. Before D15 this buffered only when the link was
     down, so a message handed to a socket that then died was in no queue at all
     and was lost silently. Now every shared message is recorded, and the only
     thing the link state changes is whether the send happens now or on
     reconnect - or, since D25, whether the gate is even live yet: a payload
     recorded while buffering or while an adopted queue awaits its verdict is
     withheld here and released by the flush that follows resolution. *)
  let send_or_buffer (msg : A.msg) : unit =
    let gate, entry = Store.record_msg msg live.gate in
    live.gate <- gate;
    Option.iter
      (fun (e : Tea_core.Prim.Msg_seq.t * A.msg) ->
        Store.delivery live.gate
        |> Option.iter (fun (d : A.msg Delivery.t) ->
               send_one ~tab:(Delivery.tab d) e))
      entry;
    schedule_checkpoint ()

  (* Replay in the order the edits were made, and do NOT empty the queue: an
     entry leaves only when the server acknowledges it. Re-sending an entry the
     server already has is safe because the server de-duplicates above
     [A.update] ([Tea_server.Replay_guard]) - not because of anything here.
     [Store.replay] is [None] while the gate buffers or while an adopted queue
     awaits its first Hello (D25), so a flush physically cannot leak an
     unconfirmed queue. *)
  let flush_outbox () : unit =
    Option.iter
      (fun
          ((tab, entries) :
            Tea_core.Prim.Tab_id.t * (Tea_core.Prim.Msg_seq.t * A.msg) list)
        -> List.iter (send_one ~tab) entries)
      (Store.replay live.gate)

  (* A down-frame from the server ({!Tea_core.Wire.down}). Every [Store_watch]
     leaf of the *current* subscriptions turns the resulting head into a msg;
     decode failure is loud (it would mean codec drift, thesis T3's one
     disallowed state  -  or R5, a Repr/jsoo divergence).

     The whole decision - rebase a [Head] onto the local model (D9), or adopt
     an identity and resync to a [Hello]'s head (D14) - lives in the pure
     {!Tea_client.Rebase.absorb}, so it is unit-tested off the browser. What is
     left here is the two effects that function cannot perform: rebinding the
     tab's identity and dispatching into the mounted app. *)
  let to_subs (head : A.model) : unit =
    current_specs ()
    |> List.iter (fun (s : (A.model, A.msg) Subs.spec) ->
           match s with
           | Subs.Spec_store f -> dispatch_remote (f head)
           | Subs.Spec_every ((_ : int), (_ : int -> A.msg)) -> ())

  let on_frame (json : string) : unit =
    Result.fold (Codec.down_of_json json)
      ~ok:(fun down ->
        match Tea_client.Rebase.absorb A.merge ~local:(shared_model ()) down with
        (* Adopt before dispatching: the store-watch msg this frame becomes is
           run through [A.update] like any other, and an app whose sync handler
           mints a dot must mint it under the identity the frame just
           announced, not the one it superseded. *)
        | Tea_client.Rebase.Resync (replica, head) ->
          Tea_client.Identity.adopt replica;
          to_subs head;
          (* The first Hello is the adoption verdict (D25): a matching replica
             releases the withheld queue; a mismatch drops exactly the adopted
             prefix and flushes what was born here. Later Hellos are [Idle]. *)
          let gate, eff = Store.confirm ~announced:replica live.gate in
          live.gate <- gate;
          (match eff with
          | Store.Idle -> ()
          | Store.Flush | Store.Prune_and_flush -> flush_outbox ());
          clear_provisional ();
          schedule_checkpoint ()
        | Tea_client.Rebase.Rebased head -> to_subs head
        (* An acknowledgement touches the delivery queue and nothing else: it
           carries no model, so there is no store-watch msg to dispatch and no
           dot to mint (D15). *)
        | Tea_client.Rebase.Acked seq ->
          live.gate <- Store.ack seq live.gate;
          schedule_checkpoint ())
      ~error:(fun (Codec.Decode_failed reason) ->
        log ("tea_client_run: undecodable model frame: " ^ reason))

  let ws_url () : string =
    let loc = W.location window in
    let scheme =
      if String.equal (Js_browser.Location.protocol loc) "https:" then "wss://"
      else "ws://"
    in
    scheme ^ Js_browser.Location.host loc ^ Tea_core.Wire.ws_path

  let forward (msg : A.msg) : unit =
    if not live.applying_remote then send_or_buffer msg

  (* Both the [open] event and the first frame confirm the link: [Rc.on_up] is
     idempotent, and flushing an already-empty outbox is a no-op, so calling
     this from both is cheaper than tracking which arrived first. A stale
     socket's events are inert  -  [on_up] only matches the socket the machine
     currently holds. *)
  let mark_up (ws : WS.t) : unit =
    live.conn <- Rc.on_up ~sock:ws live.conn;
    Option.iter (fun (_ : WS.t) -> flush_outbox ()) (Rc.sendable live.conn)

  (* Adoption endpoint (D25): called exactly once per page life, from the one
     hydration-kickoff arm that ran. The gate turns live here; pending
     payloads number after anything adopted; a stale paint rides {!to_subs},
     so the [applying_remote] fence suppresses the echo for free. *)
  let resolve_gate (outcome : Store.record option) : unit =
    let confirmed =
      if Tea_client.Identity.is_provisional () then None
      else Some (Tea_client.Identity.replica ())
    in
    let { Store.gate; paint; seed } =
      Store.resolve ~mint:mint_tab ~confirmed
        (outcome
        |> Option.fold ~none:Store.No_record
             ~some:(fun (r : Store.record) -> Store.Adopt r))
        live.gate
    in
    live.gate <- gate;
    Option.iter (fun (floor : int64) -> Tea_client.Identity.clock_seed floor) seed;
    Option.iter (fun (model : A.model) -> to_subs model) paint;
    if Store.flushable live.gate then flush_outbox ();
    schedule_checkpoint ()

  (* D8: a close is classified against the socket the machine holds, and an
     unrecognised one does nothing at all  -  a superseded socket's close event
     arrives *after* its replacement is already opening, and acting on it would
     tear the healthy socket down and arm a second timer. An intentional
     [stop_key] leaves [conn = Down], where every close is [Stale], so closing
     a subscription never schedules a reconnect. *)
  let rec open_socket (next : Rc.Backoff.t) : unit =
    let ws = WS.create (ws_url ()) () in
    WS.add_event_listener ws Js_browser.Event.Open
      (fun (_ : Js_browser.Event.t) -> mark_up ws)
      false;
    WS.add_event_listener ws Js_browser.Event.Message
      (fun e ->
        mark_up ws;
        on_frame (Ojs.string_of_js (Js_browser.Event.data e)))
      false;
    WS.add_event_listener ws Js_browser.Event.Close
      (fun (_ : Js_browser.Event.t) -> on_socket_close ws)
      false;
    live.conn <- Rc.opening ~sock:ws ~next

  and on_socket_close (ws : WS.t) : unit =
    match Rc.on_close ~sock:ws live.conn with
    | Rc.Stale -> ()
    | Rc.Reopen_after { delay_ms; next } ->
      log
        (Printf.sprintf "tea_client_run: live view socket closed; reopening in %dms"
           delay_ms);
      let timer = W.set_timeout window (fun () -> open_socket next) delay_ms in
      live.conn <- Rc.waiting ~timer ~next

  let start_key (k : Subs.key) : unit =
    match k with
    | Subs.Key_every ms ->
      let id = W.set_interval window (fire_every ms) ms in
      live.intervals <- (ms, id) :: live.intervals
    | Subs.Key_store -> open_socket Rc.Backoff.initial

  let stop_key (k : Subs.key) : unit =
    match k with
    | Subs.Key_every ms ->
      let stopping, kept =
        List.partition
          (fun ((ms', (_ : W.interval_id)) : int * W.interval_id) -> Int.equal ms ms')
          live.intervals
      in
      List.iter (fun (((_ : int), id) : int * W.interval_id) -> W.clear_interval window id) stopping;
      live.intervals <- kept
    | Subs.Key_store ->
      (match Rc.stop live.conn with
      | Rc.Nothing -> ()
      | Rc.Close_socket ws -> WS.close ws ()
      | Rc.Cancel_timer timer -> W.clear_timeout window timer);
      live.conn <- Rc.down

  (* Re-plan the standing resources against a model's subscriptions: stop
     what is no longer wanted, start what is newly wanted, leave the rest
     running. Called after every [update] and once at mount. *)
  let resync (model : A.model) : unit =
    let wanted = Subs.keys_of (Subs.specs_of (A.subscriptions model)) in
    (* [Rc.active] is true in every state but [Down], [Waiting] included: a
       reconnect that is merely pending still counts the store-watch resource
       as provisioned, or [plan] would tear the machine down and rebuild it on
       the next update  -  cancelling the ladder and reconnecting in a tight
       loop. *)
    let active =
      List.map (fun ((ms, (_ : W.interval_id)) : int * W.interval_id) -> Subs.Key_every ms)
        live.intervals
      @ (if Rc.active live.conn then [ Subs.Key_store ] else [])
    in
    let to_start, to_stop = Subs.plan ~active ~wanted in
    List.iter stop_key to_stop;
    List.iter start_key to_start

  (* [Client.app] with the runtime's two hooks on every update: mirror the
     msg up the live socket, then re-plan subscriptions against the new
     model. Neither hook may call [Vdom_blit.process] synchronously — vdom
     commits the new model only after this update returns.

     D10: the mirror is now gated on the channel. A msg the local companion
     claimed changed nothing replicated, so forwarding it would ask the server
     to apply an edit that does not exist  -  and would leak a per-client
     concern (an RPC round trip, a UI toggle) to every peer. *)
  let hooked_app : (Client.state, A.msg) Vdom.app =
    { Client.app with
      update =
        (fun state msg ->
          let state', cmd, channel = Client.step msg state in
          (match channel with
          | Channel.Local_only -> ()
          | Channel.Shared -> forward msg);
          resync (Client.shared state');
          (* Every update - local, remote or companion-only - refreshes the
             browser-local checkpoint; the timeout-0 flag coalesces a burst
             into one write (D25). *)
          schedule_checkpoint ();
          (state', cmd))
    }

  let current_url () =
    let loc = W.location window in
    Prim.Url.of_string
      (Js_browser.Location.pathname loc ^ Js_browser.Location.search loc)

  (* [urlToMsg] both at load time and on history traversal: the URL bar is an
     input to the app, never just an output of [Navigate]. A location that
     fails the relative-URL validator (e.g. a ["//"]-prefixed pathname) skips
     [msg_of_url], but audibly — silent state/URL divergence is a debugging
     trap. *)
  let dispatch_url app =
    let err_label = function
      | Prim.Url.Empty -> "empty"
      | Prim.Url.Not_relative -> "not relative"
      | Prim.Url.Backslash -> "contains a backslash"
      | Prim.Url.Control_char _ -> "contains a control byte"
    in
    Result.fold
      ~ok:(fun url -> A.msg_of_url url |> Option.iter (Vdom_blit.process app))
      ~error:(fun err ->
        log ("tea_client_run: location is " ^ err_label err ^ "; msg_of_url skipped"))
      (current_url ())

  let main () =
    Js_browser.Document.set_title (W.document window)
      (Prim.Title.to_string A.title);
    let app = Vdom_blit.run ~env hooked_app in
    live.app <- Some app;
    let root = Vdom_blit.dom app in
    live.root <- Some root;
    (* Provisional until the first Hello (D25): a hydrated paint is honest
       about not being server truth. Set before hydration can possibly
       paint; cleared in the Resync arm, the earliest moment a verdict
       exists. *)
    Js_browser.Element.set_attribute root "data-tea-provisional" "true";
    Js_browser.Element.append_child
      (Js_browser.Document.body (W.document window))
      root;
    resync (Client.shared (Vdom_blit.get app));
    dispatch_url app;
    W.add_event_listener window Js_browser.Event.Popstate
      (fun (_ : Js_browser.Event.t) -> dispatch_url app)
      false;
    (* Late checkpoints for the page's last edits: pagehide is the reliable
       end-of-life signal, and visibilitychange catches the mobile
       backgrounding that never fires pagehide. Both call the checkpoint
       directly - a timeout scheduled here may never run. *)
    W.add_event_listener window Js_browser.Event.Pagehide
      (fun (_ : Js_browser.Event.t) -> checkpoint ())
      false;
    W.add_event_listener window Js_browser.Event.Visibilitychange
      (fun (_ : Js_browser.Event.t) -> checkpoint ())
      false;
    (* Hydration kickoff (D25): sole-writer election, then boot and adopt.
       Every degrade arm resolves the gate memory-only - exactly the
       pre-step-25 page - and the mount above never waited on any of it. *)
    if Idb.supported () && Tab_lock.supported () then
      Tab_lock.acquire
        ~name:
          (Tea_client.Local_store.lock_name
             ~title:(Prim.Title.to_string A.title))
        ~granted:(fun (got : bool) ->
          if got then
            Flow.boot ~title:(Prim.Title.to_string A.title)
              ~k:(fun
                  ((conn : Flow.conn option), (records : Store.record list))
                ->
                live.store_conn <- conn;
                resolve_gate (Store.choose records))
          else resolve_gate None)
    else resolve_gate None

  let boot () = W.set_onload window main
end

(* The plain mount: the same runtime with the empty companion. Defined as an
   alias rather than duplicated, so reconnect, outbox and rebase can never be
   fixed in one mount path and left broken in the other. *)
module Local_none = Tea_core.Local.None_

module Start (A : Tea_core.App.APP) = Start_local (A) (Tea_core.Local.None_ (A))
