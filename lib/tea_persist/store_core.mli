(** Backend-generic core of the versioned model store (roadmap step 6).

    Everything that made the in-memory store work — session branches,
    apply-as-commit, history/undo, watch, merge (T1/T2) — lives here once,
    functorized over the Irmin store [S], so the in-memory shim
    ({!Tea_persist.Store}) and the on-disk pack backend
    ([Tea_persist_pack.Store_pack]) share one body. Step 6 adds the three
    history-hygiene features (R1):

    - every commit date is minted by the handle's single monotonic {!Clock},
      so identical sibling edits can never collapse into one
      content-addressed commit (DESIGN §5);
    - {!CORE.commit_coalesced} folds a run of chatty Msgs into one amended
      commit under an app-supplied {!Tea_core.Coalesce_spec} policy;
    - {!CORE.checkpoint} squashes a session branch to a single root commit —
      the only admissible retention target for pack GC. *)

module Contents (A : Tea_core.App.APP) : Irmin.Contents.S with type t = A.model

(** The store surface every backend shim [include]s. Backends add only their
    own [create] (and, for pack, [gc]). *)
module type CORE = sig
  type model
  type msg

  module S : Irmin.Generic_key.KV with type Schema.Contents.t = model

  type t
  (** A store handle: the repo plus the one clock every commit date on this
      handle is minted from. Contract: one read-write handle per backing
      store — two handles carry independent clocks and could re-collide. *)

  type session
  (** A handle onto one session's branch; shares the owning handle's clock. *)

  val v : now:(unit -> int64) -> S.repo -> t Lwt.t
  (** The backend-instantiator seam: wrap a repo, seeding the clock from
      every branch head's [Info] date so new stamps land strictly above
      everything already in history (heads dominate live history because
      every minted commit's date exceeds its parent's). *)

  val repo : t -> S.repo
  val default_now : unit -> int64
  val model_path : Tea_safe.Safe_key.t
  val model_path_raw : string list
  val session : t -> Tea_core.Prim.Session_id.t -> session Lwt.t
  val main_session : t -> session Lwt.t
  val fork : t -> from:session -> Tea_core.Prim.Session_id.t -> session Lwt.t
  val load : session -> model Lwt.t
  val commit : session -> label:string -> model -> unit Lwt.t
  val apply : session -> msg -> model Lwt.t
  val head_ref : session -> Tea_core.Prim.Commit_ref.t option Lwt.t
  val history : session -> Tea_core.Prim.Commit_ref.t list Lwt.t
  val undo : session -> model option Lwt.t
  val model_at : S.commit -> model Lwt.t

  type watch = S.watch

  val watch : session -> (model -> unit Lwt.t) -> watch Lwt.t
  val unwatch : watch -> unit Lwt.t
  val merge_into : src:session -> dst:session -> (unit, string) result Lwt.t

  val close : t -> unit Lwt.t
  (** Release backend resources: required before exit on pack, a no-op cost
      on mem. *)

  type checkpoint
  (** A squashed root commit — the only admissible GC retention target. *)

  val checkpoint_commit : checkpoint -> S.commit
  val checkpoint_ref : checkpoint -> Tea_core.Prim.Commit_ref.t

  type checkpoint_error =
    | Empty_branch
    | Branch_moved

  val checkpoint : session -> label:string -> (checkpoint, checkpoint_error) result Lwt.t
  (** Squash: one root commit (parents = [][]) carrying the branch's current
      tree, moved onto the branch by test-and-set. [Branch_moved] means a
      concurrent writer won; the squash minted nothing visible and the caller
      may retry. Note a checkpoint severs merge ancestry with branches forked
      {i before} it — fork after checkpointing, or merge first. *)

  (** Commit coalescing (R1): fold a run of chatty Msgs into one commit by
      amending the head — same parents, new tree, relabelled — while the
      app's {!Tea_core.Coalesce_spec} policy keeps folding. *)
  module Coalescer : sig
    type t
    (** One per chatty pipeline (the WS pump mints one per socket). It amends
        only commits it minted itself, so a head produced by anyone else — a
        form post, a merge, an undo — ends the run and is never amended
        away. Not for sharing across concurrent writers. *)

    val v : msg Tea_core.Coalesce_spec.t -> t

    val seal : t -> unit
    (** Force a run boundary: the next Msg appends a fresh commit. Call
        before {!fork} when the forked base must remain the exact future
        merge base. *)
  end

  val commit_coalesced : Coalescer.t -> session -> msg:msg -> model -> unit Lwt.t
  val apply_coalesced : Coalescer.t -> session -> msg -> model Lwt.t
end

module Make
    (A : Tea_core.App.APP)
    (S : Irmin.Generic_key.KV with type Schema.Contents.t = A.model) :
  CORE with type model = A.model and type msg = A.msg and module S = S
