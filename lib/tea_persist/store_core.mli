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

  type exploder
  (** The exploded-tree witness (roadmap step 8, D6): the fixed set of Irmin
      paths a model is scattered over, one per CRDT field, so a single-field
      edit rewrites a single path instead of the whole model blob.

      Irmin fixes this store's contents type to [model] at {i every} path, so a
      leaf is not a type of its own but a {i field-isolated model} — [bottom]
      with exactly one field carried over. Reassembly is therefore the CvRDT
      [join] over the leaves, which is why D6 depends on D1: without a lattice
      there is nothing to reassemble with. Registering a witness is total: it
      carries its own [join] rather than reading one back off
      {!Tea_core.App.APP.merge}, so there is no half-configured state. *)

  val exploder :
    bottom:model ->
    join:(model -> model -> model) ->
    fields:(Tea_safe.Safe_key.t * (model -> model)) list ->
    exploder
  (** Build a witness. [fields] pairs each field's store path with the
      projection isolating it ([bottom] everywhere but that field); paths must
      be pairwise distinct, or the fields sharing one collapse into a single
      leaf and reassembly loses whichever the join does not dominate. *)

  val v : now:(unit -> int64) -> ?exploded:exploder -> S.repo -> t Lwt.t
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

  val ctx_of_session : session -> Tea_core.Crdt.Ctx.t
  (** The CRDT context (D1) a step on this session applies under: the session's
      branch name as its replica id, plus the handle's monotonic clock. The
      server runtime builds one to drive {!Tea_core.Loop.step}; {!apply} uses it
      internally. *)

  val apply : session -> msg -> model Lwt.t
  val head_ref : session -> Tea_core.Prim.Commit_ref.t option Lwt.t
  val history : session -> Tea_core.Prim.Commit_ref.t list Lwt.t

  val undo : session -> model option Lwt.t
  (** Move the branch head back one commit, returning the restored model
      ([None] at the root). Crash-safe (D3): the pre-undo head is recorded on
      the session's durable [redo-] ref {i before} the head moves, so a crash
      strands only a harmless redo pointer, never a moved head with no way
      back. *)

  val redo : session -> model option Lwt.t
  (** Undo's inverse: move the head forward onto the session's durable redo
      pointer and clear it ([None] when there is nothing to redo). *)

  val model_at : t -> S.commit -> model Lwt.t
  (** The model as of one specific commit. Takes the handle because reading is
      witness-directed under D6: an exploded store gathers the commit's field
      leaves, a whole-blob store reads its single path. *)

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

  module Retention = Tea_core.Prim.Retention

  val retain : t -> retention:Retention.t -> checkpoint -> checkpoint Lwt.t
  (** Link a [checkpoint] onto the durable [__checkpoints] spine (a chain, each
      entry parented on the prior) and return the single GC [~retain] anchor:
      the [K]-th checkpoint from the head, [K = Retention.to_int retention].
      Retaining it keeps the last [K] checkpoints and everything newer; older
      spine entries become collectible. The spine ref persists across restart
      (D4). *)

  val checkpoints_head : t -> checkpoint option Lwt.t
  (** The current head of the durable checkpoint spine, if any. *)

  val reap : t -> ttl:Tea_core.Prim.Ttl.t -> now:int64 -> int Lwt.t
  (** Sweep expired session branches: remove every non-reserved branch whose
      head [Info] date (a {!Clock} stamp) is older than [now - ttl]; [main],
      the [__checkpoints] spine and every [redo-] pointer are never swept.
      Returns the number removed (D3). *)

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
