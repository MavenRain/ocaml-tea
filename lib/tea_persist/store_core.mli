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

  val main_replica : Tea_core.Crdt.Replica.t
  (** The {!Tea_core.Crdt.Replica} the canonical branch is addressed by, from
      the same private mint {!branch_waters} and the persist path already use
      (roadmap step 15). Named here rather than rebuilt at each call site
      because a second spelling of "the main replica" would key delivery floors
      no boot filter ever looks up — mass duplicates or mass blindness, with no
      symptom either way. It is what an app declares as the [floor_replica] of
      a [Tea_server.Rpc.once] when its keyed endpoints commit on the canonical
      branch, and what makes that floor's water checkable against the head
      water [branch_waters] reports for [main]. *)

  val fork : t -> from:session -> Tea_core.Prim.Session_id.t -> session Lwt.t
  val load : session -> model Lwt.t
  (** The model standing at this session branch's head, read off the branch.
      A read-only surface since D19: the model it returns carries no witness,
      so an edit derived from it and handed to {!commit} is a last-write-wins
      write that can erase a concurrent writer (R10). Read, edit and commit
      through {!load_based} instead. *)

  val commit : session -> label:string -> model -> Tea_core.Prim.Store_water.t Lwt.t
  (** Persist one model as one commit and return the
      {!Tea_core.Prim.Store_water} of the very commit minted (roadmap step 13):
      the certificate the WS pump stamps into the durable delivery floor, so
      the floor can never claim a commit the store does not have. Captured
      from the commit's own [Info] date, never read back off the head — a
      concurrent writer between commit and read-back would hand the floor a
      water it has no right to.

      {b Precondition since D19}: the model must not derive from a prior read
      of this branch. This is the load-free writer's door — a boot seed, a
      test that commits values directly — and a read-modify-write routed
      through it is exactly R10, which is why the step seam can no longer
      reach it. *)

  type based
  (** One writer's witnessed read: the branch head observed at read time, the
      model read {i through} that very commit, and the session both came from.
      Abstract, and {!load_based} is the only mint, for two reasons a labelled
      [~base] argument cannot supply.

      First, a witness that could be built from a fresh head read is
      self-satisfying, and self-satisfying is the whole bug: the pre-D19
      [append_commit] re-read the head and tested against that read, so the
      test passed while the stale model still landed on the racer — R10's
      "preserves history, not content".

      Second, the token carries its session, so a witness minted on one branch
      cannot be presented to another. Under a total commit that mistake would
      not even be an error: the test would fail, the loop would reload the
      other branch and join a foreign model into it. Prose has been tried on
      the neighbouring hazard ({!Coalescer.t}'s "not for sharing across
      concurrent writers"); a token is cheaper than prose.

      Both reads are fenced, so the token is total in fact and not merely in
      type: a backend that raises on a collected head or tree yields the
      absent witness, from which a commit is a create-if-still-absent that a
      moved branch simply denies. That is the fence {!head_water} and the
      watch delivery already carry. *)

  val load_based : session -> based Lwt.t
  (** Read the head once, then gather the model through {i that} commit rather
      than off the branch. Two reads off the branch would be two Lwt steps, so
      a writer landing between them would hand back a model older than the
      witness and the commit would then pass its test while erasing that
      writer: R10 rebuilt inside its own fix. An empty branch yields the
      absent witness paired with the app's initial model, exactly the value
      {!load} reports for one. *)

  val based_model : based -> model
  (** The model this writer saw, and the only model a witnessed commit should
      be computed from. Reading it off the token rather than off a second
      {!load} is deliberate: it makes the safe wiring the {i shorter} one, the
      lesson this module already wrote down about [?forget]'s no-op default. *)

  (** What a witnessed commit actually landed. A record rather than a tuple
      because two of its three fields are easy to confuse with the caller's
      own inputs: the model here is the {i committed} one, which under
      contention differs from the one passed in, and the water is the winning
      round's mint rather than the first attempt's. A tuple would let a caller
      silently pick up its own stale value by position. *)
  type committed =
    { water : Tea_core.Prim.Store_water.t
          (** The date of the commit that actually landed, minted by the
              winning round. The only value a delivery floor for this message
              may claim, and read off the commit itself, never back off a
              head: a head read after the commit can belong to a later
              writer. *)
    ; model : model
          (** What the branch now holds for this write: the model as
              committed, which under contention is the reconciled model
              rather than the one the caller passed in. A caller reporting
              state to a user must report this and not its own input. *)
    ; rounds : int
          (** How many times contention forced a reconcile before this commit
              landed; [0] on an uncontended write. Exposed because a loop
              whose progress cannot be observed can only fail as a hang, and
              a hang is not a test result. *)
    }

  val commit_based : based -> label:string -> model -> committed Lwt.t
  (** Persist one model as one commit against the witness, resolving
      contention rather than refusing it.

      Each round mints a commit parented on the witnessed head, carrying the
      model scattered onto that commit's tree, and moves the branch by
      test-and-set. On denial the head that actually landed is re-observed and
      becomes the new witness, the app's {!Tea_core.Merge_spec.t} reconciles
      the caller's model against the model at that head with the {i new}
      witness as ancestor, and the round repeats. Nothing is re-run: the
      caller's step, its effects and its CRDT dots all happened exactly once,
      and only their result is carried forward. Re-running would double a real
      [Lwt_unix.sleep] and mint a second dot for one user operation, which a
      join then keeps — one click, two set elements.

      Total on purpose, not for convenience. A refusal would have to travel to
      the WS pump, whose [Cell] has already consumed the message's sequence
      number before the apply and whose guard outlives the socket, so a
      refused message replays as [Duplicate] and is acknowledged without ever
      being applied. That is the silent loss the whole D16 family exists to
      prevent, and the only way to un-take is to lower a floor whose law is
      that it never lowers.

      Unbounded, and bounded in fact: a round is entered only after the head
      moved, and a head only moves when another writer's write landed, so
      rounds are paid for by competing writes rather than by a constant. A
      budget was rejected because every loss-free exhaustion arm is either
      this loop continued or a plain set, and a plain set is R10. What a
      budget would have bought — visible progress — is bought instead by
      [rounds] and by one diagnostic line at eight.

      An absent head is not a merge input. When the re-observed head is [None]
      the branch was removed underneath us (the reaper), and the caller's
      model is committed as a fresh root rather than reconciled against the
      app's initial model, which a three-way policy would read as "theirs
      deleted everything". *)

  val ctx_of_session : session -> Tea_core.Crdt.Ctx.t
  (** The CRDT context (D1) a step on this session applies under: the session's
      branch name as its replica id, plus the handle's monotonic clock. The
      server runtime builds one to drive {!Tea_core.Loop.step}; {!apply} uses it
      internally. *)

  val head_water : session -> Tea_core.Prim.Store_water.t Lwt.t
  (** The [Info] date standing at this session branch's head, or
      {!Tea_core.Prim.Store_water.bottom} when there is no head at all —
      never written, reaped, or gone with a restored-older root. Total: a
      backend that raises on a collected head is fenced to [bottom], exactly
      as the branch walk in {!reap} fences it. Boot-time and test surface;
      the persist path stamps floors with {!commit}'s own return instead. *)

  val branch_waters :
    t -> (Tea_core.Crdt.Replica.t * Tea_core.Prim.Store_water.t) list Lwt.t
  (** Every branch in the repo, paired with the {!Tea_core.Crdt.Replica} it
      would be addressed by and its head's water (roadmap step 13). Reserved
      refs ([main], the [__checkpoints] spine) are NOT filtered out: a floor
      can legitimately be keyed on [main], and a replica name with no floor
      costs nothing.

      The replica is built by the same private mint {!ctx_of_session} uses,
      so the boot-time lookup and the persist-time key cannot drift apart.
      One [S.Branch.list] plus one fenced head read per branch, at boot only:
      the same walk {!v} already makes to seed the clock. *)

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

  val reap :
    ?forget:(Tea_core.Prim.Session_id.t -> unit Lwt.t) ->
    t ->
    ttl:Tea_core.Prim.Ttl.t ->
    now:int64 ->
    int Lwt.t
  (** Sweep expired session branches: remove every non-reserved branch whose
      head [Info] date (a {!Clock} stamp) is older than [now - ttl]; [main],
      the [__checkpoints] spine and every [redo-] pointer are never swept.
      Returns the number removed (D3).

      [?forget] is called with each victim's session id {i before} its branch
      is removed (default: nothing, today's behaviour exactly, and the
      {b unsafe} default). {b Hard precondition when a durable replay guard
      is live} (roadmap step 11, D16): a server wiring [reap] in {b must}
      pass [Tea_server.Durable_guard.forget] here, because a guard floor that
      outlives its branch converts every replay onto the recreated branch
      into a [Duplicate] against a stale high water — total silent loss onto
      an empty model, the one loss-side path in the guard's degradation
      (stated on [Tea_server.Replay_guard.forget], closed here by ordering:
      tombstone first, then removal). The step-13 boot filter bounds the
      unwired case in time but does not void this precondition: a reaped
      branch has no head, so at the NEXT boot every {i witnessed} floor
      under it is dropped ([dropped_no_branch]) and only [bottom]-water
      floors (no-op takes, pre-step-13 records) survive — the mid-life
      window between the reap and that restart, and those unwitnessed
      floors, stay the caller's obligation. The same filter is what makes
      the other restore-shaped failure — an older pack root under a newer
      journal — a visible duplicate rather than silent loss (D18).

      Since roadmap step 12 made session identity durable, this precondition
      is LIVE rather than theoretical: a client can now return to a branch
      that was reaped weeks earlier. [?forget] defaulting to a no-op therefore
      makes the UNSAFE wiring the SHORTER one. Reap without forget leaves a
      floor over a branch that no longer exists, and the returning client's
      replay is judged [Duplicate] against an empty model - total silent loss.
      Reap WITH forget is sound but not free of surprise: the replay reads
      [Fresh] (a first sighting is accepted whatever its seq) and applies once
      onto [bottom], so the client observes a rollback followed by one edit. *)

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

  val commit_coalesced : Coalescer.t -> based -> msg:msg -> model -> committed Lwt.t
  (** {!commit_based} through the coalescer: amend the head while the policy
      keeps folding {i and} the head is the commit this coalescer minted,
      otherwise append. Returns what landed, whichever path minted it.

      The ownership guard is unchanged; what changed in D19 is when it is
      asked. The amend decision and its test-and-set both consult the witness
      rather than a head re-read taken after the caller's load, because a
      writer landing in that gap used to demote the run to an append that then
      carried an already stale model. The append path is one witnessed
      test-and-set with {!commit_based}'s reconcile loop; the historical
      three-attempt retry and its plain-{!commit} fallback are gone, the first
      because retrying without recomputing is what R10 indicts and the second
      because an unconditional last write wins is R10 itself. *)

  val apply_coalesced : Coalescer.t -> session -> msg -> model Lwt.t
end

module Make
    (A : Tea_core.App.APP)
    (S : Irmin.Generic_key.KV with type Schema.Contents.t = A.model) :
  CORE with type model = A.model and type msg = A.msg and module S = S
