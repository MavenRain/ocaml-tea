(** The reaper loop (roadmap step 22, D24): the timing shell that runs a
    session sweep on a fixed cadence until a stop promise resolves.

    The module holds no store, no guard and no policy. [loop] receives the
    sweep as a closure and the clock and the timer as seams, the
    [live_transport] discipline: a native test drives the loop with a
    queue-backed timer and a frozen clock, and no real time is involved.
    The module writes nothing to stderr; observability lives at the wiring
    sites ({!Tea_server_pack.Make_pack.serve_pack} and {!Make.serve}). *)

module Cadence : sig
  type t
  (** How often the loop sweeps, in seconds. A separate newtype from
      {!Tea_core.Prim.Ttl} because the two answer different questions: a
      [ttl] is the idle age that makes a branch a victim, a [Cadence] is
      how often the loop looks for one. *)

  val of_seconds : float -> t option
  (** [None] unless the value is strictly positive: a zero or negative
      cadence names a spin loop, not a policy. Sub-second cadences are FINE
      (a cadence is a wait, measured by a real timer), which is why this
      gate is looser than {!Tea_core.Prim.Ttl.of_seconds}'s one-second
      floor (a ttl is compared against whole-second commit dates). *)

  val to_seconds : t -> float
end

type spec =
  { ttl : Tea_core.Prim.Ttl.t
        (** Branches whose head is idle longer than this are swept. Pick a
            value well above the longest a CONNECTED client may sit idle: a
            live socket does not exclude its branch from the sweep, and the
            ttl is the one idle bound (see the [?reaper] doc on the entry
            points for the recovery semantics when it is picked too low).
            The enforced span is {!Tea_core.Prim.Ttl.whole_seconds} (ceil),
            the same value the boot banner announces; sub-second ttls are
            refused at {!Tea_core.Prim.Ttl.of_seconds}. *)
  ; every : Cadence.t  (** How often the sweep runs. *)
  }
(** Everything an entry point needs to run a reaper. *)

val loop :
  sweep:(now:int64 -> int Lwt.t) ->
  now:(unit -> int64) ->
  timer:(Cadence.t -> unit Lwt.t) ->
  every:Cadence.t ->
  stop:unit Lwt.t ->
  unit Lwt.t
(** Run [sweep] once per [every] tick until [stop] resolves.

    Each round mints [timer every], waits on [Lwt.choose [tick; stop]], and
    sweeps only if [stop] still sleeps. The consequences are each
    load-bearing:

    - Zero sweeps run before the first tick, so a boot is never raced by an
      immediate sweep.
    - [stop] wins ties: when a tick and the stop are both resolved, the loop
      returns without sweeping.
    - A sweep in flight always completes before the loop's promise resolves,
      so a caller that sequences its teardown after this promise knows no
      removal is mid-air ({!Tea_server_pack.Make_pack.serve_pack} joins on
      exactly that).
    - A sweep that REJECTS is fenced here: the failure is one stderr line,
      the round counts as zero sweeps, and the loop arms the next tick.
      The loop's own promise therefore never rejects - which is what lets
      the pack tier [Lwt.join] it with the server and know the teardown
      after that join always runs ([Lwt.join] re-raises only after every
      member resolves, and a rejecting member would poison the whole
      [Lwt_main.run], skipping the ordered closes behind it). The timer
      seam stays trusted: it is the entry point's own [Lwt_unix.sleep] (or
      a test's queue), not a store path.
    - [Lwt.choose], never [Lwt.pick]: pick cancels the losing branch, and a
      cancelled sweep could stop between a guard tombstone and its branch
      removal (the [live_session] pump ruling, for the same reason).
    - The timer a final round leaves losing stays pending; [choose] does not
      cancel it, and it resolves into nothing.

    [timer] is injected so the loop is deterministic under test; production
    passes [fun c -> Lwt_unix.sleep (Cadence.to_seconds c)]. [now] is read
    after each tick's wait, so a sweep judges idle age against its own
    tick's clock, not the loop's start. *)
