(** The client tier's effectful half: interpret {!Tea_client}'s command
    extensions and mount an [APP] in the browser.

    js_of_ocaml-only ([Vdom_blit] / [Js_browser] have no native archive),
    which is exactly why it is a separate library from the natively-testable
    [Tea_client]. Lwt is deliberately absent (DESIGN §7): every handler is a
    plain callback. *)

(** Interprets {!Tea_client.After} via [setTimeout] and {!Tea_client.Navigate}
    via [history.pushState]; every other command is left to the next handler
    ([Vdom_blit] itself runs [Echo]/[Batch]/[Map]/[Bind]).

    [After] timers are fire-and-forget: {!Start} mounts one app for the
    lifetime of the page and never disposes it, so pending timers have no
    teardown to outlive. A future dispose path must track and clear them. *)
val env : Vdom_blit.env

module Start (A : Tea_core.App.APP) : sig
  (** Set the document title, mount the app onto [document.body], dispatch
      [A.msg_of_url] for the current location, and re-dispatch it on
      back/forward ([popstate]) — the client mirror of lean-tea's [urlToMsg].

      Also the client half of {!Tea_core.Sub} (roadmap step 3): after mount
      and after every [update], the app's subscriptions are re-planned
      ({!Tea_client.Subs}) — [Every] runs on [setInterval]; [Store_watch]
      opens the {!Tea_core.Wire.ws_path} WebSocket, mirrors every
      locally-born msg up it, and turns each model frame pushed down into
      msgs via the subscription's own mapping. The server head is the
      authority (R6): a frame overwrites optimistic local state; there is no
      auto-reconnect (a closed socket logs to the console; reload to
      reconnect). Call it from the page's [onload] (or use {!boot}). *)
  val main : unit -> unit

  (** [boot () = onload := main]: the one-liner for a client executable. *)
  val boot : unit -> unit
end
