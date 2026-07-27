(** State-based CRDTs (CvRDTs) as a join-semilattice (roadmap step 8, D1).

    Every state exposes a [join] (least upper bound) that is {b idempotent}
    ([join x x = x]), {b commutative} ([join a b = join b a]) and {b associative}
    ([join (join a b) c = join a (join b c)]). Those three laws are exactly
    Strong Eventual Consistency: two replicas that have observed the same set of
    updates hold [join]-equal states, so a merge never conflicts and never needs
    the common ancestor. The persistence layer registers a 2-way [join] as
    Irmin's merge via {!Merge_spec.Crdt_join}, discarding the base — sound
    precisely because [join] is a LUB.

    Each state also carries a {!Repr.t} witness so it doubles as an Irmin
    [Contents] payload and a wire value, exactly like every other model field. *)

(** A replica identity: which session minted an update. The deterministic
    tie-break for the LWW register (two replicas that mint the same clock stamp
    order by replica), and the per-replica key of the PN-counter. *)
module Replica : sig
  type t

  val v : Prim.Session_id.t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val t : t Repr.t
end

(** A causally-unique event stamp: a monotonic {!Clock} stamp paired with the
    minting {!Replica}. Two updates never share a [Dot] — a replica's clock is
    strictly monotone, and distinct replicas differ in the replica component —
    so the lexicographic [(stamp, replica)] order is total.

    {b One caveat, load-bearing since D14.} A live client mints under the
    replica its server session announced, so that id now has {i two} clocks
    behind it: the server's (real wall-seconds) and the tab's (wall source [0],
    deliberately, so a client write loses every LWW tie to the server — R6).
    Uniqueness across the pair is therefore a magnitude argument, not a
    construction: the tab's stamps are small counts and the server's are
    wall-seconds, so a collision needs on the order of 2^31 edits in one page
    life. If the client is ever given a real clock, the dot needs a tier tag
    rather than this reasoning. *)
module Dot : sig
  type t = private
    { stamp : int64
    ; replica : Replica.t
    }

  val v : Clock.stamp -> Replica.t -> t

  (** Lexicographic on [(stamp, replica)]: the total order LWW maxes over and
      OR-Set sorts by. *)
  val compare : t -> t -> int

  val equal : t -> t -> bool
  val t : t Repr.t
end

(** The per-update context threaded into {!App.APP.update}: the replica this
    tier writes as, plus the clock its dots are minted from. One context per
    session (server) or per client tab. *)
module Ctx : sig
  type t

  val v : clock:Clock.t -> replica:Replica.t -> t

  (** A fresh, strictly-monotone {!Dot} for a new LWW/OR-Set event. *)
  val dot : t -> Dot.t

  val replica : t -> Replica.t
end

(** A positive-negative counter: per-replica [(increments, decrements)], the
    value is [sum increments - sum decrements]. [join] takes the per-replica max
    of each half, so re-merging never double-counts (idempotent) and the order
    of merges is irrelevant (commutative, associative). *)
module Pn_counter : sig
  type state

  val t : state Repr.t
  val bottom : state
  val value : state -> int
  val join : state -> state -> state
  val equal : state -> state -> bool

  val inc : Replica.t -> state -> state
  val dec : Replica.t -> state -> state

  (** Bring the value back to [0] by charging the current value to this
      replica's decrement half. (A local reset; see the counter example.) *)
  val reset : Replica.t -> state -> state
end

(** The element of an {!Or_set}: a total order (set membership + canonical
    form) plus a {!Repr.t} witness. *)
module type ELT = sig
  type t

  val compare : t -> t -> int
  val t : t Repr.t
end

(** An observed-remove add-wins set (OR-Set). An add tags the element with a
    fresh {!Dot}; a remove tombstones exactly the dots it has observed. [join]
    unions the add-set and the tombstone-set, so an add concurrent with a remove
    of an earlier dot survives (add-wins), and a remove-then-re-add is present.
    Union is idempotent, commutative and associative. *)
module Or_set (E : ELT) : sig
  type state

  val t : state Repr.t
  val bottom : state
  val value : state -> E.t list
  val join : state -> state -> state
  val equal : state -> state -> bool

  val add : Dot.t -> E.t -> state -> state
  val remove : E.t -> state -> state
  val mem : E.t -> state -> bool
end

(** The value carried by an {!Lww} register: it needs only a {!Repr.t} witness;
    equality is derived from it. *)
module type VAL = sig
  type t

  val t : t Repr.t
end

(** A last-write-wins register: a value tagged with the {!Dot} that last wrote
    it (or [None] before any write). [join] keeps the value with the greater
    dot, [None] being the least. The [(stamp, replica)] order makes the winner
    deterministic even for two concurrent writes in the same wall-second, so
    [join] is idempotent, commutative and associative. *)
module Lww (V : VAL) : sig
  type state

  val t : state Repr.t

  (** The register before any write, holding the given initial value. *)
  val bottom : V.t -> state

  val value : state -> V.t
  val join : state -> state -> state
  val equal : state -> state -> bool

  (** Overwrite the value, stamping it with [dot] (which must dominate every
      dot this replica has already written for the write to take effect on a
      converged peer). *)
  val set : Dot.t -> V.t -> state -> state
end

(** One field of a {!record} join: read it ([get]), write the joined value back
    ([set]), and the field's own [join]. *)
type 'r field

val field : get:('r -> 'a) -> set:('a -> 'r -> 'r) -> join:('a -> 'a -> 'a) -> 'r field

(** Join a record field-by-field: each field joins independently. The result is
    idempotent/commutative/associative iff every listed field's join is — and a
    field left out silently takes the left argument (a last-writer-wins hazard),
    so {b every replicated field must be listed}. *)
val record : 'r field list -> 'r -> 'r -> 'r
