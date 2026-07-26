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

(** Mount [A] together with a client-local companion (roadmap step 8, D10).

    The runtime is the same one {!Start} uses  -  {!Start} {i is} this functor
    applied to {!Local_none}  -  with one extra decision on every update: a
    message the companion claims changes only [L.local], is not mirrored up the
    live socket, and is therefore invisible to every peer. *)
module Start_local
    (A : Tea_core.App.APP)
    (L : Tea_core.Local.LOCAL with type shared = A.model and type msg = A.msg) : sig
  (** Set the document title, mount the app onto [document.body], dispatch
      [A.msg_of_url] for the current location, and re-dispatch it on
      back/forward ([popstate]) — the client mirror of lean-tea's [urlToMsg].

      Also the client half of {!Tea_core.Sub} (roadmap step 3): after mount
      and after every [update], the app's subscriptions are re-planned
      ({!Tea_client.Subs}) — [Every] runs on [setInterval]; [Store_watch]
      opens the {!Tea_core.Wire.ws_path} WebSocket, mirrors every
      shared-channel msg up it, and turns each model frame pushed down into
      msgs via the subscription's own mapping.

      Roadmap step 8 replaced the two standing hazards of that loop:
      - {b D8}: a closed socket reopens on an exponential ladder
        ({!Tea_client.Reconnect}: 500ms, doubling, capped at 30s) instead of
        logging "reload to reconnect" and going quiet for the rest of the page's
        life;
      - {b D9}: a msg born while the link is down waits in an outbox and is
        replayed in order once it is back, and an arriving head is rebased onto
        the tab's own model under [A.merge] ({!Tea_client.Rebase}) rather than
        overwriting it.

      The server head is still the authority (R6) wherever the app's merge
      policy has no way to keep both sides: under [Last_write_wins] a frame
      still wins outright. Under [Crdt_join] nothing is lost.

      Call it from the page's [onload] (or use {!boot}). *)
  val main : unit -> unit

  (** [boot () = onload := main]: the one-liner for a client executable. *)
  val boot : unit -> unit
end

(** The empty companion ({!Tea_core.Local.None_}), re-exported so a plain mount
    reads as one name. *)
module Local_none = Tea_core.Local.None_

(** Mount [A] with no local companion: exactly
    [Start_local (A) (Local_none (A))]. *)
module Start (A : Tea_core.App.APP) : sig
  val main : unit -> unit
  val boot : unit -> unit
end
