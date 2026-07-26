(** Live-socket reconnection as a pure state machine (roadmap step 8, D8).

    Before D8 the runtime held [socket : WS.t option] and a closed socket was
    terminal: the console said "reload to reconnect" and the tab silently
    stopped receiving heads (DESIGN R6). This module is that [option] replaced
    by the four states a reconnecting link actually has, plus the exponential
    ladder that spaces the retries.

    Nothing here touches the browser: the socket and the timer id are type
    parameters, so the whole machine links natively and [client_reconnect_test]
    drives it with ordinary values. {!Tea_client_run.Start_local} supplies
    [Js_browser.WebSocket.t] and [Window.timeout_id] and performs the effects
    the transitions ask for.

    {2 The stale-socket hazard}

    A socket that has been superseded still delivers its [close] event, and it
    arrives {i after} its replacement is already opening. Acting on it would
    tear down the healthy new socket and, worse, arm a second timer, so every
    close is matched by {b physical} equality against the socket the machine
    currently holds - [==], not a structural test, because two distinct
    WebSockets to the same URL are structurally indistinguishable. An unmatched
    close is {!Stale} and does nothing. *)

(** The retry ladder: 500ms, doubling, capped at 30s. [private int] so a delay
    can be read but never fabricated - a hand-built [0] would spin the
    reconnect loop against a server that is down. *)
module Backoff : sig
  type t = private int

  val base_ms : int
  val cap_ms : int

  (** [base_ms]. The delay after a link that had been up: the common case is a
      server restart or a laptop lid, and half a second is short enough to feel
      instant. *)
  val initial : t

  val to_ms : t -> int

  (** Double, saturating at [cap_ms]. Idempotent once capped. *)
  val bump : t -> t
end

(** The connection's four states. [Opening] and [Waiting] carry the ladder
    position to use for their {i next} attempt; [Up] does not, because a link
    that reached [Up] resets to {!Backoff.initial}, and [Down] has no attempt
    pending.

    (The step-8 sketch put the ladder only in [Waiting]. It has to ride
    [Opening] too: the escalation is otherwise lost at exactly the
    [Waiting -> Opening] step, and a server that refuses every connection would
    be retried at 500ms forever.) *)
type ('sock, 'timer) t = private
  | Down  (** not subscribed: no socket, no timer, nothing pending *)
  | Opening of
      { sock : 'sock
      ; next : Backoff.t
      }  (** created, not yet confirmed by an [open] event or a first frame *)
  | Up of 'sock  (** confirmed: the only state that can send *)
  | Waiting of
      { timer : 'timer
      ; next : Backoff.t
      }  (** the link is down and a reopen is armed *)

val down : ('sock, 'timer) t
val opening : sock:'sock -> next:Backoff.t -> ('sock, 'timer) t
val waiting : timer:'timer -> next:Backoff.t -> ('sock, 'timer) t

(** Whether the store-watch resource counts as provisioned. True in every state
    but {!Down} - including [Waiting], where there is no socket at all. This is
    what stops [Subs.plan] from tearing the machine down and rebuilding it on
    the next [resync] while a reconnect is merely pending. *)
val active : ('sock, 'timer) t -> bool

(** The socket a message may be written to: [Some] in {!Up} only. [Opening]
    deliberately yields [None] - a send on a CONNECTING socket raises in the
    browser, and the message belongs in the outbox ({!Rebase}) instead. *)
val sendable : ('sock, 'timer) t -> 'sock option

(** What a [close] event should cause. *)
type close =
  | Stale
      (** the closing socket is not the one currently held (or there is none):
          do nothing at all *)
  | Reopen_after of
      { delay_ms : int  (** arm a timer for this long *)
      ; next : Backoff.t
            (** then park in [waiting ~timer ~next]; the ladder is already
                bumped, so this is the delay the {i following} failure uses *)
      }

(** Classify a [close] event for [sock]. The caller performs the effect
    (schedule a timer) and parks the result with {!waiting}: minting a timer id
    is the one thing this module cannot do purely. *)
val on_close : sock:'sock -> ('sock, 'timer) t -> close

(** Confirm [sock] is live: [Opening sock] becomes [Up sock] and the ladder
    resets. Idempotent on an already-[Up] socket, because both the [open] event
    and the first frame call it. Any other state, or a different socket, is
    left untouched. *)
val on_up : sock:'sock -> ('sock, 'timer) t -> ('sock, 'timer) t

(** The one resource a teardown must release. *)
type ('sock, 'timer) teardown =
  | Nothing
  | Close_socket of 'sock
  | Cancel_timer of 'timer

(** What to release when the subscription goes away. The caller performs it and
    stores {!down}; the [close] event that a [Close_socket] provokes then lands
    on [Down] and classifies as {!Stale}, which is what keeps an intentional
    stop from arming a reconnect. *)
val stop : ('sock, 'timer) t -> ('sock, 'timer) teardown
