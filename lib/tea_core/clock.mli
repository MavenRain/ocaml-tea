(** Monotonic per-store commit clock (DESIGN §5).

    Irmin commits are content-addressed and [Info] dates have one-second
    resolution, so two branches making the {i identical} edit (same tree, same
    parents, same second) collapse to one commit — which can move a later
    merge base off the true fork point. This clock makes every minted date
    strictly greater than the last, so two applies can never produce
    content-identical commits, even inside one wall-clock second.

    Pure: the wall source is injected ([create ~now]), stdlib-only. *)

type t

type stamp = private int64
(** A minted commit date: [max (now ()) (last + 1)] — strictly increasing,
    still reading as approximate wall-clock seconds. *)

val create : now:(unit -> int64) -> t

val seed : t -> int64 -> unit
(** [seed t d] raises the floor to [max last d]; it never lowers. Called with
    each branch-head [Info] date when a store handle (re)opens, so stamps
    resume strictly above everything already in history. *)

val next : t -> stamp
(** Strictly greater than every stamp minted on [t] and every value seeded
    into it; lock-free compare-and-set retry. *)
