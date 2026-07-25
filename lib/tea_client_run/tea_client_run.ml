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
          | unrecognized ->
            ignore unrecognized;
            false)
    }

module Start (A : Tea_core.App.APP) = struct
  module Client = Tea_client.Make (A)
  module Codec = Tea_core.Codec.Make (A)

  (* --- Live subscription state (one mount per page life) ------------------

     The runtime half of {!Tea_core.Sub}: [Every] becomes [setInterval],
     [Store_watch] becomes the {!Tea_core.Wire.ws_path} WebSocket. Handlers
     are looked up from the *current* model's subscriptions at fire time
     (via [Vdom_blit.get]), so the keyed resources below never hold stale
     callbacks; see {!Tea_client.Subs.key}. *)

  type live =
    { mutable app : (A.model, A.msg) Vdom_blit.app option
    ; mutable socket : WS.t option
    ; mutable applying_remote : bool
    ; mutable intervals : (int * W.interval_id) list
    }

  let live : live =
    { app = None; socket = None; applying_remote = false; intervals = [] }

  let dispatch (msg : A.msg) : unit =
    Option.iter (fun app -> Vdom_blit.process app msg) live.app

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
      ~some:(fun app -> Subs.specs_of (A.subscriptions (Vdom_blit.get app)))
      live.app

  let fire_every (ms : int) () : unit =
    let now = int_of_float (Js_browser.Date.now ()) in
    current_specs ()
    |> List.iter (fun (s : (A.model, A.msg) Subs.spec) ->
           match s with
           | Subs.Spec_every (ms', f) -> if Int.equal ms ms' then dispatch (f now)
           | Subs.Spec_store (_ : A.model -> A.msg) -> ())

  (* A model frame from the server: the committed head of this session's
     branch. Every [Store_watch] leaf of the *current* subscriptions turns it
     into a msg; decode failure is loud (it would mean codec drift, thesis
     T3's one disallowed state — or R5, a Repr/jsoo divergence). *)
  let on_frame (json : string) : unit =
    Result.fold (Codec.model_of_json json)
      ~ok:(fun model ->
        current_specs ()
        |> List.iter (fun (s : (A.model, A.msg) Subs.spec) ->
               match s with
               | Subs.Spec_store f -> dispatch_remote (f model)
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

  let open_socket () : WS.t =
    let ws = WS.create (ws_url ()) () in
    WS.add_event_listener ws Js_browser.Event.Message
      (fun e -> on_frame (Ojs.string_of_js (Js_browser.Event.data e)))
      false;
    WS.add_event_listener ws Js_browser.Event.Close
      (fun (_ : Js_browser.Event.t) ->
        log "tea_client_run: live view socket closed; reload to reconnect")
      false;
    ws

  (* The optimistic mirror (DESIGN §7): every locally-born msg is also sent
     up the open socket; the server drives it through the same [update] and
     the committed head comes back as a [Store_watch] frame. Msgs born before
     the socket finishes connecting (or after it dies) are applied locally
     only — logged, because silent divergence from the store is a debugging
     trap (R6: the server head is the authority). *)
  let forward (msg : A.msg) : unit =
    if not live.applying_remote then
      Option.iter
        (fun ws ->
          match WS.ready_state ws with
          | WS.Open -> WS.send ws (Codec.msg_to_json msg)
          | WS.Connecting | WS.Closing | WS.Closed ->
            log "tea_client_run: live view socket not open; msg applied locally only")
        live.socket

  let start_key (k : Subs.key) : unit =
    match k with
    | Subs.Key_every ms ->
      let id = W.set_interval window (fire_every ms) ms in
      live.intervals <- (ms, id) :: live.intervals
    | Subs.Key_store -> live.socket <- Some (open_socket ())

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
      Option.iter (fun ws -> WS.close ws ()) live.socket;
      live.socket <- None

  (* Re-plan the standing resources against a model's subscriptions: stop
     what is no longer wanted, start what is newly wanted, leave the rest
     running. Called after every [update] and once at mount. *)
  let resync (model : A.model) : unit =
    let wanted = Subs.keys_of (Subs.specs_of (A.subscriptions model)) in
    let active =
      List.map (fun ((ms, (_ : W.interval_id)) : int * W.interval_id) -> Subs.Key_every ms)
        live.intervals
      @ Option.fold ~none:[] ~some:(fun (_ : WS.t) -> [ Subs.Key_store ]) live.socket
    in
    let to_start, to_stop = Subs.plan ~active ~wanted in
    List.iter stop_key to_stop;
    List.iter start_key to_start

  (* [Client.app] with the runtime's two hooks on every update: mirror the
     msg up the live socket, then re-plan subscriptions against the new
     model. Neither hook may call [Vdom_blit.process] synchronously — vdom
     commits the new model only after this update returns. *)
  let hooked_app : (A.model, A.msg) Vdom.app =
    { Client.app with
      update =
        (fun model msg ->
          let model', cmd = Client.app.update model msg in
          forward msg;
          resync model';
          (model', cmd))
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
    resync (Vdom_blit.get app);
    dispatch_url app;
    W.add_event_listener window Js_browser.Event.Popstate
      (fun (_ : Js_browser.Event.t) -> dispatch_url app)
      false

  let boot () = W.set_onload window main
end
