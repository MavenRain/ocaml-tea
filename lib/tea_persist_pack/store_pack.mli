(** The irmin-pack backend: a durable on-disk store with checkpoint + GC
    retention (roadmap step 6, R1). Same {!Tea_persist.Store_core.CORE}
    surface as the in-memory shim, plus [create] on a filesystem root and
    [gc]. *)

(** A pack store root directory. *)
module Root : sig
  type t

  val v : string -> t
  (** The store's root directory. The PARENT must already exist (only the
      final component is created on first open); existing data is always
      reopened, never truncated — there is deliberately no [fresh]. *)

  val to_string : t -> string
end

val framed : 'a Repr.t -> 'a -> 'a Repr.t
(** [framed inner fallback] re-encodes [inner] as a single length-prefixed
    [Repr.string], which is the codec shape irmin-pack requires of a contents
    type: a pack entry is [hash][kind][the codec's own bytes] and the reader
    recovers the entry's length by decoding a varint at the head of those bytes.
    A [Repr.record]'s leading varint frames only its {i first field}, so a
    multi-field model read back short and its decoder walked off the end of the
    buffer ([Invalid_argument "index out of bounds"]) — a single-field model
    satisfied the contract by accident, which is why the Counter-based pack
    tests never saw it.

    Exposed so the contract is testable on its own, and applied to every store
    this module builds. [fallback] is the corrupt-bytes arm (no exception in
    normal control flow); it cannot mask corruption, because irmin re-hashes
    what it decoded and a fallback value hashes to something other than the
    requested key. *)

module Make (A : Tea_core.App.APP) : sig
  include Tea_persist.Store_core.CORE with type model = A.model and type msg = A.msg

  val create :
    ?now:(unit -> int64) -> ?lower_root:string -> ?exploded:exploder -> Root.t -> t Lwt.t
  (** Open (or initialise) the pack store at [Root]. Always configured with
      [Indexing_strategy.minimal], so delete-mode GC stays allowed for the
      root's entire life — an [always]-indexed root is permanently poisoned
      against GC. Call {!close} before exit.

      [?lower_root] configures a lower-layer directory (its parent must exist);
      GC then archives discarded data there instead of deleting it, so
      pre-checkpoint commits stay readable ([Gc.behaviour] becomes [`Archive]).
      Omitted, GC deletes (D5). *)

  val gc_behaviour : t -> [ `Archive | `Delete ]
  (** [`Archive] when a [lower_root] was configured (GC moves discarded data to
      the lower layer), [`Delete] otherwise. Observable without running a GC. *)

  type gc_error =
    | Gc_disallowed
    | Gc_already_running
    | Gc_failed of string

  val gc : t -> retain:checkpoint -> (unit, gc_error) result Lwt.t
  (** Discard every commit, tree, and blob older than [retain] and
      unreachable from it, blocking until the forked GC worker finalises.
      [retain]'s commit and full tree survive; its parents remain only as
      dangling stubs, so history/undo walks crossing the boundary degrade to
      truncation/[None]. Precondition: run with other sessions quiescent,
      merged, or forked after the checkpoint — branch heads written {i
      before} it become unreadable. *)
end
