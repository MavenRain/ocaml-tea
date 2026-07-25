module Prim = Tea_core.Prim
module W = Js_browser.Window

let window = Js_browser.window

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
        Js_browser.Console.log Js_browser.console
          (Ojs.string_to_js
             ("tea_client_run: location is " ^ err_label err
            ^ "; msg_of_url skipped")))
      (current_url ())

  let main () =
    Js_browser.Document.set_title (W.document window)
      (Prim.Title.to_string A.title);
    let app = Vdom_blit.run ~env Client.app in
    Js_browser.Element.append_child
      (Js_browser.Document.body (W.document window))
      (Vdom_blit.dom app);
    dispatch_url app;
    W.add_event_listener window Js_browser.Event.Popstate
      (fun (_ : Js_browser.Event.t) -> dispatch_url app)
      false

  let boot () = W.set_onload window main
end
