module Prim = Tea_core.Prim
module Subs = Tea_client.Subs
module W = Js_browser.Window
module WS = Js_browser.WebSocket

let window = Js_browser.window

let log (line : string) =
  Js_browser.Console.log Js_browser.console (Ojs.string_to_js line)

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
          | Tea_client.Http { path; body; expect } ->
            (* The wire half of [Tea_rpc.Make.call]: POST the encoded request,
               classify the transport outcome, feed it to the [expect]
               continuation the typed layer built. [send_msg] from an async
               callback is the established [After]/[set_timeout] precedent.
               [Status.of_int 0 = None] is the network-failure classifier:
               offline/DNS/abort surface as XHR status 0. *)
            let module X = Js_browser.XHR in
            let xhr = X.create () in
            X.open_ xhr "POST" path;
            X.set_request_header xhr "Content-Type" "application/json";
            X.set_onreadystatechange xhr (fun () ->
                match X.ready_state xhr with
                | X.Done ->
                  let outcome =
                    Prim.Status.of_int (X.status xhr)
                    |> Option.fold
                         ~none:(Error Tea_core.Cmd.Network_error)
                         ~some:(fun st ->
                           if Prim.Status.is_success st then Ok (X.response_text xhr)
                           else Error (Tea_core.Cmd.Http_status st))
                  in
                  Vdom_blit.Cmd.send_msg ctx (expect outcome)
                | X.Unsent | X.Opened | X.Headers_received | X.Loading -> ()
                | X.Other (_ : int) -> ());
            X.send xhr (Ojs.string_to_js body);
            true
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
  module Outbox = Tea_client.Rebase.Outbox
  module Channel = Tea_client.Local_channel

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

  type live =
    { mutable app : (Client.state, A.msg) Vdom_blit.app option
    ; mutable conn : (WS.t, W.timeout_id) Rc.t
    ; mutable outbox : A.msg Outbox.t
    ; mutable applying_remote : bool
    ; mutable intervals : (int * W.interval_id) list
    }

  let live : live =
    { app = None
    ; conn = Rc.down
    ; outbox = Outbox.empty
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

  (* A down-frame from the server ({!Tea_core.Wire.down}). Every [Store_watch]
     leaf of the *current* subscriptions turns the resulting head into a msg;
     decode failure is loud (it would mean codec drift, thesis T3's one
     disallowed state  -  or R5, a Repr/jsoo divergence).

     The whole decision - rebase a [Head] onto the local model (D9), or adopt
     an identity and resync to a [Hello]'s head (D14) - lives in the pure
     {!Tea_client.Rebase.absorb}, so it is unit-tested off the browser. What is
     left here is the two effects that function cannot perform: rebinding the
     tab's identity and dispatching into the mounted app. *)
  let on_frame (json : string) : unit =
    Result.fold (Codec.down_of_json json)
      ~ok:(fun down ->
        let head, announced =
          Tea_client.Rebase.absorb A.merge ~local:(shared_model ()) down
        in
        (* Adopt before dispatching: the store-watch msg this frame becomes is
           run through [A.update] like any other, and an app whose sync handler
           mints a dot must mint it under the identity the frame just
           announced, not the one it superseded. *)
        Option.iter Tea_client.Identity.adopt announced;
        current_specs ()
        |> List.iter (fun (s : (A.model, A.msg) Subs.spec) ->
               match s with
               | Subs.Spec_store f -> dispatch_remote (f head)
               | Subs.Spec_every ((_ : int), (_ : int -> A.msg)) -> ()))
      ~error:(fun (Codec.Decode_failed reason) ->
        log ("tea_client_run: undecodable model frame: " ^ reason))

  let ws_url () : string =
    let loc = W.location window in
    let scheme =
      if String.equal (Js_browser.Location.protocol loc) "https:" then "wss://"
      else "ws://"
    in
    scheme ^ Js_browser.Location.host loc ^ Tea_core.Wire.ws_path

  (* The optimistic mirror (DESIGN §7): every locally-born msg is also sent up
     the live socket; the server drives it through the same [update] and the
     committed head comes back as a [Store_watch] frame.

     D9: a msg born while the link is not [Up] is no longer merely "applied
     locally and logged"  -  it goes into the outbox and is replayed on
     reconnect. [Rc.sendable] is [Some] in the [Up] state only, deliberately
     including CONNECTING under "buffer it": a [send] on a connecting socket
     raises in the browser. *)
  let send_or_buffer (msg : A.msg) : unit =
    Rc.sendable live.conn
    |> Option.to_result ~none:()
    |> Result.fold
         ~ok:(fun ws -> WS.send ws (Codec.msg_to_json msg))
         ~error:(fun () -> live.outbox <- Outbox.buffer msg live.outbox)

  (* Replay in the order the edits were made. [drain] empties the outbox
     first, so a msg that cannot be sent after all (the link dropped again
     mid-flush) is re-buffered by [send_or_buffer] rather than replayed twice
      -  and the survivors keep their relative order, because re-buffering walks
     the remaining list in the same direction. *)
  let flush_outbox () : unit =
    let msgs, emptied = Outbox.drain live.outbox in
    live.outbox <- emptied;
    List.iter send_or_buffer msgs

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
    Js_browser.Element.append_child
      (Js_browser.Document.body (W.document window))
      (Vdom_blit.dom app);
    resync (Client.shared (Vdom_blit.get app));
    dispatch_url app;
    W.add_event_listener window Js_browser.Event.Popstate
      (fun (_ : Js_browser.Event.t) -> dispatch_url app)
      false

  let boot () = W.set_onload window main
end

(* The plain mount: the same runtime with the empty companion. Defined as an
   alias rather than duplicated, so reconnect, outbox and rebase can never be
   fixed in one mount path and left broken in the other. *)
module Local_none = Tea_core.Local.None_

module Start (A : Tea_core.App.APP) = Start_local (A) (Tea_core.Local.None_ (A))
