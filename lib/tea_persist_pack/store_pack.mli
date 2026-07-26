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

module Make (A : Tea_core.App.APP) : sig
  include Tea_persist.Store_core.CORE with type model = A.model and type msg = A.msg

  val create : ?now:(unit -> int64) -> Root.t -> t Lwt.t
  (** Open (or initialise) the pack store at [Root]. Always configured with
      [Indexing_strategy.minimal], so delete-mode GC stays allowed for the
      root's entire life — an [always]-indexed root is permanently poisoned
      against GC. Call {!close} before exit. *)

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
