(** Backend-generic core of the versioned model store: see store_core.mli.
    The T1/T2 body moved verbatim from the original [store.ml]; step 6 adds
    the monotonic clock threading, checkpoint squash, and the coalescer. *)

open Lwt.Syntax

(* Lift the pure merge policy into an Irmin Contents merge. Irmin only invokes
   this when both branches changed the same path, so single-session use never
   hits it. *)
module Contents (A : Tea_core.App.APP) = struct
  type t = A.model

  let t = A.model_t

  let merge : t option Irmin.Merge.t =
    match A.merge with
    | Tea_core.Merge_spec.Last_write_wins -> Irmin.Merge.(option (default A.model_t))
    | Tea_core.Merge_spec.Crdt_join join ->
      (* The CvRDT least-upper-bound registered as Irmin's merge: the ancestor
         is discarded ([~old:_]), sound because [join] is idempotent /
         commutative / associative (SEC). A 2-way join never conflicts, so the
         result is always [Ok]. *)
      Irmin.Merge.(option (v A.model_t (fun ~old:_ ours theirs -> Lwt.return (Ok (join ours theirs)))))
    | Tea_core.Merge_spec.Three_way f ->
      let merge_f ~old ours theirs =
        let* old_r = old () in
        match old_r with
        | Error _ as e -> Lwt.return e
        | Ok ancestor -> (
          match f ~ancestor ~ours ~theirs with
          | Ok m -> Lwt.return (Ok m)
          | Error msg -> Lwt.return (Error (`Conflict msg)))
      in
      Irmin.Merge.(option (v A.model_t merge_f))
end

module type CORE = sig
  type model
  type msg

  module S : Irmin.Generic_key.KV with type Schema.Contents.t = model

  type t
  type session

  (** The D6 exploded-tree witness: one Irmin path per CRDT field, each holding
      a field-isolated model, reassembled by the lattice join. See the
      implementation for why a leaf is a model rather than a type of its own. *)
  type exploder

  val exploder :
    bottom:model ->
    join:(model -> model -> model) ->
    fields:(Tea_safe.Safe_key.t * (model -> model)) list ->
    exploder

  val v : now:(unit -> int64) -> ?exploded:exploder -> S.repo -> t Lwt.t
  val repo : t -> S.repo
  val default_now : unit -> int64
  val model_path : Tea_safe.Safe_key.t
  val model_path_raw : string list
  val session : t -> Tea_core.Prim.Session_id.t -> session Lwt.t
  val main_session : t -> session Lwt.t
  val main_replica : Tea_core.Crdt.Replica.t
  val fork : t -> from:session -> Tea_core.Prim.Session_id.t -> session Lwt.t
  val load : session -> model Lwt.t
  val commit : session -> label:string -> model -> Tea_core.Prim.Store_water.t Lwt.t

  type based

  val load_based : session -> based Lwt.t
  val based_model : based -> model

  type committed =
    { water : Tea_core.Prim.Store_water.t
    ; model : model
    ; rounds : int
    }

  val commit_based : based -> label:string -> model -> committed Lwt.t
  val ctx_of_session : session -> Tea_core.Crdt.Ctx.t
  val head_water : session -> Tea_core.Prim.Store_water.t Lwt.t

  val branch_waters :
    t -> (Tea_core.Crdt.Replica.t * Tea_core.Prim.Store_water.t) list Lwt.t
  val apply : session -> msg -> model Lwt.t
  val head_ref : session -> Tea_core.Prim.Commit_ref.t option Lwt.t
  val history : session -> Tea_core.Prim.Commit_ref.t list Lwt.t
  val undo : session -> model option Lwt.t
  val redo : session -> model option Lwt.t
  val model_at : t -> S.commit -> model Lwt.t

  type watch = S.watch

  val watch : session -> (model -> unit Lwt.t) -> watch Lwt.t
  val unwatch : watch -> unit Lwt.t
  val merge_into : src:session -> dst:session -> (unit, string) result Lwt.t
  val close : t -> unit Lwt.t

  type checkpoint

  val checkpoint_commit : checkpoint -> S.commit
  val checkpoint_ref : checkpoint -> Tea_core.Prim.Commit_ref.t

  type checkpoint_error =
    | Empty_branch
    | Branch_moved

  val checkpoint : session -> label:string -> (checkpoint, checkpoint_error) result Lwt.t

  module Retention = Tea_core.Prim.Retention

  val retain : t -> retention:Retention.t -> checkpoint -> checkpoint Lwt.t
  val checkpoints_head : t -> checkpoint option Lwt.t
  val reap :
    ?forget:(Tea_core.Prim.Session_id.t -> unit Lwt.t) ->
    t ->
    ttl:Tea_core.Prim.Ttl.t ->
    now:int64 ->
    int Lwt.t

  module Coalescer : sig
    type t

    val v : msg Tea_core.Coalesce_spec.t -> t
    val seal : t -> unit
  end

  val commit_coalesced : Coalescer.t -> based -> msg:msg -> model -> committed Lwt.t
  val apply_coalesced : Coalescer.t -> session -> msg -> model Lwt.t
end

module Make
    (A : Tea_core.App.APP)
    (S : Irmin.Generic_key.KV with type Schema.Contents.t = A.model) =
struct
  module Codec = Tea_core.Codec.Make (A)
  module S = S

  type model = A.model
  type msg = A.msg

  (** The exploded-tree witness (roadmap step 8, D6): the fixed set of Irmin
      paths one model is scattered over, one per CRDT field.

      Irmin fixes this store's contents type to [A.model] at {i every} path, so
      a leaf cannot be a type of its own — it is a {i field-isolated model}:
      [project] returns [bottom] with exactly one field carried over. That makes
      reassembly the CvRDT join over the leaves, which is precisely why D6
      follows D1 (P3): without a lattice there is nothing to reassemble with,
      and per-path merge would have no per-field join to dispatch.

      The witness carries its own [join] rather than reading it back off
      {!Tea_core.App.APP.merge}, so registering one is total: there is no
      "witness present but the app merge is not a [Crdt_join]" state to degrade
      out of silently. *)
  type exploder =
    { bottom : A.model
    ; join : A.model -> A.model -> A.model
    ; fields : (Tea_safe.Safe_key.t * (A.model -> A.model)) list
    }

  let exploder ~(bottom : A.model) ~(join : A.model -> A.model -> A.model)
      ~(fields : (Tea_safe.Safe_key.t * (A.model -> A.model)) list) : exploder =
    { bottom; join; fields }

  (** The store handle: the repo plus the one clock every commit date on this
      handle is minted from (a per-branch clock would re-collide across
      branches — the DESIGN §5 dedup bug in another coat). [exploded] is the
      optional D6 witness, shared by every session opened on this handle. *)
  type t =
    { repo : S.repo
    ; clock : Clock.t
    ; exploded : exploder option
    }

  (** A handle onto one session's branch. [clock] is shared by reference with
      the owning {!t}: one handle, one date order. [name] is the branch's ref
      name, kept so undo can mint the session's durable [redo-] pointer (D3). *)
  type session =
    { repo : S.repo
    ; branch : S.t
    ; clock : Clock.t
    ; name : string
    ; exploded : exploder option
    }

  (* Typed store key: a compile-time-literal step, dropped to Irmin's raw
     string-list path at the one bridge. Byte-identical to the former
     [\[ "model" \]]; the byte-compat pin lives in [test/safe_test]. *)
  let model_path = Tea_safe.Safe_key.(root (Step.v "model"))
  let model_path_raw = Tea_safe.Safe_key.to_steps model_path

  module Retention = Tea_core.Prim.Retention

  (* Reserved branch refs the reaper (D3) must never sweep and the retention
     spine (D4) reads. The durable checkpoint chain lives on [spine_branch];
     each undo mints a durable redo pointer on [redo_prefix ^ session-name].
     The [Irmin.Branch.String] grammar admits only [A-Za-z0-9-_.], so the
     spec-sketch's ["redo/<sid>"] with a '/' is not a legal ref here; the
     '/'-free [redo-] prefix is the adaptation (documented deviation). *)
  let spine_branch = "__checkpoints"
  let spine_label = "checkpoint"
  let redo_prefix = "redo-"
  let redo_ref_name (s : session) : string = redo_prefix ^ s.name

  (* A ref the reaper leaves alone: the canonical [main] branch, the retention
     spine, and every [redo-] pointer. Session branches (["session-<hex>"]) are
     the only sweepable refs. *)
  let is_reserved (name : string) : bool =
    String.equal name (Tea_core.Prim.Branch_name.(to_string main))
    || String.equal name spine_branch
    || String.starts_with ~prefix:redo_prefix name

  let default_now () : int64 = Int64.of_float (Unix.gettimeofday ())

  (* Every commit date is minted by the handle's clock — strictly increasing,
     so identical edits on sibling branches never share a content address
     (DESIGN §5). [info_v] ticks immediately (for [S.Commit.v]); [info_f]
     defers the tick to commit time (for [set_exn]/[merge_into]). *)
  let info_v (clock : Clock.t) (label : string) : S.Info.t =
    S.Info.v ~author:"ocaml-tea" ~message:label (Clock.next clock :> int64)

  let info_f (clock : Clock.t) (label : string) = fun () -> info_v clock label

  let v ~(now : unit -> int64) ?(exploded : exploder option) (repo : S.repo) : t Lwt.t =
    let clock = Clock.create ~now in
    let* heads = S.Repo.heads repo in
    List.iter (fun c -> Clock.seed clock (S.Info.date (S.Commit.info c))) heads;
    Lwt.return { repo; clock; exploded }

  let repo (t : t) : S.repo = t.repo

  let session (t : t) (sid : Tea_core.Prim.Session_id.t) : session Lwt.t =
    let name = Tea_core.Prim.Branch_name.(to_string (of_session sid)) in
    let* branch = S.of_branch t.repo name in
    Lwt.return { repo = t.repo; branch; clock = t.clock; name; exploded = t.exploded }

  let main_session (t : t) : session Lwt.t =
    let name = Tea_core.Prim.Branch_name.(to_string main) in
    let* branch = S.main t.repo in
    Lwt.return { repo = t.repo; branch; clock = t.clock; name; exploded = t.exploded }

  (** Fork a fresh session branch off another session's current head, so the
      two branches share that commit as their single common ancestor. This is
      the "open the shared document in a new session" primitive behind the
      collaboration demo (T2): each collaborator forks the shared doc, edits on
      their own branch, and reconciles via {!merge_into} against a well-defined
      base. An empty source yields an empty session (nothing to fork yet).

      Forking initialises a {i fresh} destination only: if the [sid]'s branch
      already holds committed work it is returned unchanged, so a reused or
      double-forked [sid] can never silently lose history to a forced head move
      (R3-adjacent). Re-seeding a diverged session is a {!merge_into}, not a
      fork. *)
  let fork (t : t) ~(from : session) (sid : Tea_core.Prim.Session_id.t) : session Lwt.t =
    let* dst = session t sid in
    let* dst_head = S.Head.find dst.branch in
    match dst_head with
    | Some _ -> Lwt.return dst
    | None -> (
      let* head = S.Head.find from.branch in
      match head with
      | None -> Lwt.return dst
      | Some c ->
        let* () = S.Head.set dst.branch c in
        Lwt.return dst)

  (* D6: the (path, value) writes one model becomes — the single whole-blob
     path with no witness registered, one field-isolated path per CRDT field
     with one. The no-witness list is exactly the historical single write, so
     the whole-blob path stays bit-for-bit what it was. *)
  let writes (x : exploder option) (model : A.model) : (string list * A.model) list =
    Option.fold x
      ~none:[ (model_path_raw, model) ]
      ~some:(fun e ->
        List.map
          (fun (k, project) -> (Tea_safe.Safe_key.to_steps k, project model))
          e.fields)

  (* Scatter [model] over its paths, layered onto [base] so anything else the
     tree carries survives. One [Tree.add] per field, one commit for the lot. *)
  let scatter (x : exploder option) (base : S.tree) (model : A.model) : S.tree Lwt.t =
    Lwt_list.fold_left_s
      (fun acc (path, leaf) -> S.Tree.add acc path leaf)
      base
      (writes x model)

  (* D6 reassembly: join every present field leaf back into a whole model.

     A branch written before the witness was registered carries no field paths
     at all, so the legacy whole-blob at [model_path] is read instead — the
     migration-on-first-read the spec calls for, and the reason the fallback is
     keyed on "no leaf found" rather than on "some leaf missing". A branch with
     neither is simply fresh, and yields the app's initial model. *)
  let gather (x : exploder option) (find : string list -> A.model option Lwt.t) :
      A.model Lwt.t =
    let legacy () =
      let* v = find model_path_raw in
      Lwt.return (Option.value v ~default:(fst A.init))
    in
    (Option.fold x ~none:legacy ~some:(fun e () ->
         let* leaves =
           Lwt_list.filter_map_s
             (fun ((k : Tea_safe.Safe_key.t), (_ : A.model -> A.model)) ->
               find (Tea_safe.Safe_key.to_steps k))
             e.fields
         in
         match leaves with
         | [] -> legacy ()
         | _ :: (_ : A.model list) -> Lwt.return (List.fold_left e.join e.bottom leaves)))
      ()

  let load (s : session) : A.model Lwt.t = gather s.exploded (S.find s.branch)

  (** The model as of one specific commit rather than the branch head: watch
      notifications read the tree they were notified about, so a burst of
      commits yields one frame per commit instead of n reads of whatever the
      final head happens to be. Since D19 it is also the witnessed read: the
      whole point of a witness is that the model came from the commit the
      test-and-set will name, and only a read {i through} that commit can say
      so. *)
  let model_at_with (x : exploder option) (c : S.commit) : A.model Lwt.t =
    let* at = S.of_commit c in
    gather x (S.find at)

  let model_at (t : t) (c : S.commit) : A.model Lwt.t = model_at_with t.exploded c

  (* Every read the witnessed path makes is fenced the way [head_water] and
     the watch delivery already fence theirs (D19): a surface with no error
     channel must not leak the backend's exceptions instead, or the raise
     lands in the WS pump after the sequence number was taken and before the
     floor was written, which is the silent loss the whole guard family
     exists to prevent. A fenced miss degrades to the ABSENT witness, from
     which a commit is a create-if-still-absent that any live branch simply
     denies — so the cost of a fenced miss is one extra reconcile round, not
     a wrong write. *)
  let fenced_head (s : session) : S.commit option Lwt.t =
    Lwt.catch (fun () -> S.Head.find s.branch) (fun (_ : exn) -> Lwt.return None)

  let fenced_model_at (x : exploder option) (c : S.commit) : A.model Lwt.t =
    Lwt.catch (fun () -> model_at_with x c) (fun (_ : exn) -> Lwt.return (fst A.init))

  (** One writer's witnessed read (roadmap step 14, D19). See store_core.mli:
      the head observed at read time, the model read through {i that} commit,
      and the session both came from, so neither can be forged nor crossed. *)
  type based =
    { session : session
    ; head : S.commit option
    ; model : A.model
    }

  let load_based (s : session) : based Lwt.t =
    let* head = fenced_head s in
    (* Absence yields [fst A.init] rather than a [gather] over an empty find,
       which is the same value by construction ([gather]'s legacy arm defaults
       a missing blob to it, and its exploded arm falls into that legacy arm
       when no leaf is present) and one fewer backend round trip. *)
    let* model =
      Option.fold head
        ~none:(fun () -> Lwt.return (fst A.init))
        ~some:(fun (c : S.commit) () -> fenced_model_at s.exploded c)
        ()
    in
    Lwt.return { session = s; head; model }

  let based_model (b : based) : A.model = b.model

  (* The outcome of one reconcile round, as a value rather than as a side
     effect (D19). The conflict arm carries its reason OUT rather than
     printing it: an app-declared loss recorded only by [Printf.eprintf] dies
     with the process, while the branch is this system's audit surface, so the
     single commit site appends the reason to the label instead. *)
  type resolution =
    | Joined of A.model
    | Declared of A.model
    | Conflicted of
        { model : A.model
        ; reason : string
        }

  (* Exhaustive over {!Tea_core.Merge_spec.t}'s three constructors with no
     wildcard, so a fourth policy has to come here and say what losing a race
     means for it.

     [ours] wins every non-joining arm, and that is forced rather than chosen:
     the message being committed is one the WS pump has already TAKEN and is
     about to ACK, and the D16 contract is that an acked effect exists in the
     store. [theirs] is not erased either — it is the parent of the commit
     this round mints, so its content stays reachable from history. That is
     the honest limit of the closure (R10c) and still a strict improvement on
     the pre-D19 [set_exn], where the loser was erased AND unreachable.

     Pure: no IO, no printing. The reason travels as data. *)
  let resolve ~(ancestor : A.model option) ~(ours : A.model) ~(theirs : A.model) : resolution =
    match A.merge with
    | Tea_core.Merge_spec.Crdt_join join -> Joined (join ours theirs)
    | Tea_core.Merge_spec.Last_write_wins -> Declared ours
    | Tea_core.Merge_spec.Three_way f ->
      Result.fold
        (f ~ancestor ~ours ~theirs)
        ~ok:(fun (m : A.model) -> Joined m)
        ~error:(fun (reason : string) -> Conflicted { model = ours; reason })

  let resolved_model (r : resolution) : A.model =
    match r with
    | Joined m -> m
    | Declared m -> m
    | Conflicted { model; reason = (_ : string) } -> model

  (* An app-declared loss is written into the commit message, so "the loser is
     recoverable from history" is something a reader of the log can act on
     rather than something this file claims. [Declared] is labelled too: a
     [Last_write_wins] app that quietly drops a concurrent write leaves the
     same trace as one whose merge function refused. *)
  let resolved_label (label : string) (r : resolution) : string =
    match r with
    | Joined (_ : A.model) -> label
    | Declared (_ : A.model) -> label ^ " [last-write-wins over a concurrent commit]"
    | Conflicted { model = (_ : A.model); reason } -> label ^ " [conflict: " ^ reason ^ "]"

  (** What a witnessed commit actually landed (D19). See store_core.mli: a
      record rather than a tuple because two of its three fields are easy to
      confuse with the caller's own inputs. *)
  type committed =
    { water : Tea_core.Prim.Store_water.t
    ; model : A.model
    ; rounds : int
    }

  (* Loud once, at a round count no honest workload reaches: the loop is
     unbounded on purpose (every loss-free exhaustion arm is either this loop
     continued or a plain set, and a plain set is R10), so its only pathology
     is a socket that keeps being outrun, and an unobservable pathology is a
     hang rather than a test result. *)
  let reconcile_noisy_round = 8

  (* The loop itself, handing back the commit it minted as well as what that
     commit landed. Only the coalescer needs the commit (its run bookkeeping
     is keyed on the hash of a commit it minted ITSELF), and it must not get
     it by reading the head back afterwards: a head read after a successful
     commit can belong to a later writer, and a run that then amends that
     writer's commit is the ownership guard defeated. *)
  let commit_witnessed (b : based) ~(label : string) (model : A.model) :
      (S.commit * committed) Lwt.t =
    let s = b.session in
    let parents_of (h : S.commit option) : S.commit_key list =
      Option.fold h ~none:[] ~some:(fun (c : S.commit) -> [ S.Commit.key c ])
    in
    let base_tree (h : S.commit option) : S.tree =
      Option.fold h ~none:(S.Tree.empty ()) ~some:S.Commit.tree
    in
    (* One tail-recursive function carrying its own witness AND the ancestor
       that witness stands for, so the ancestor is a function of the round and
       freezing it at round zero is unwritable rather than merely untested.
       With a frozen ancestor, round two of a three-way merge would compare an
       [ours] that already contains round one's [theirs] against a base that
       predates it, and read a re-add as a delete. *)
    let rec attempt
        (witness : S.commit option)
        (ancestor : A.model)
        (ours : A.model)
        (label : string)
        (rounds : int) : (S.commit * committed) Lwt.t =
      if Int.equal rounds reconcile_noisy_round then
        Printf.eprintf
          "tea-store: %d reconcile rounds on branch %s (%s): a writer may be outrun\n%!"
          rounds s.name label;
      let* tree = scatter s.exploded (base_tree witness) ours in
      (* The date is re-minted per round, never frozen at round zero: a
         retried commit carrying an older date than the racing parent it
         retried over would break the branch-date monotonicity
         {!Tea_core.Prim.Store_water} rests on (the step-13 lesson at the
         comment above [commit]). Gaps in the clock are harmless; order is
         not. *)
      let* c =
        S.Commit.v s.repo ~info:(info_v s.clock label) ~parents:(parents_of witness) tree
      in
      let* moved = S.Head.test_and_set s.branch ~test:witness ~set:(Some c) in
      if moved then
        Lwt.return
          ( c
          , ({ water = Tea_core.Prim.Store_water.of_date (S.Info.date (S.Commit.info c))
             ; model = ours
             ; rounds
             }
              : committed) )
      else
        let* landed = fenced_head s in
        Option.fold landed
          ~none:(fun () ->
            (* A denied test-and-set does not imply a competitor committed:
               the reaper removes whole branches. An absent head must never
               reach the resolver, because [gather] reports absence as the
               app's INITIAL model and a three-way policy would read that as
               "theirs deleted everything". Our content becomes a fresh root
               instead; the reaper's own precondition already scrubbed the
               delivery floor, so the client's next replay reads Fresh. *)
            attempt None ancestor ours label (rounds + 1))
          ~some:(fun (landed_c : S.commit) () ->
            let* theirs = fenced_model_at s.exploded landed_c in
            let r = resolve ~ancestor:(Some ancestor) ~ours ~theirs in
            attempt (Some landed_c) theirs (resolved_model r) (resolved_label label r)
              (rounds + 1))
          ()
    in
    attempt b.head b.model model label 0

  (** Persist one model as one commit against the witness, resolving
      contention rather than refusing it (roadmap step 14, D19). See
      store_core.mli for why it is total and why nothing is re-run. *)
  let commit_based (b : based) ~(label : string) (model : A.model) : committed Lwt.t =
    let* ((_ : S.commit), landed) = commit_witnessed b ~label model in
    Lwt.return landed

  (* Whole-blob and exploded stores take genuinely different write paths, and
     the no-witness one is the historical [set_exn] {i verbatim}. Unifying both
     onto a root [set_tree_exn] is what a first cut does, and it is wrong: a
     root tree rebuilt from [S.tree] and re-saved does not survive a pack
     close/reopen (the reopened contents fail to decode), while [set_exn]'s
     transaction does. The witness path pays that cost only where it must —
     several paths have to move in one commit — and its durability is pinned by
     [exploded_test]'s close/reopen round trip. *)
  let commit (s : session) ~(label : string) (model : A.model) :
      Tea_core.Prim.Store_water.t Lwt.t =
    (* The minted date is CAPTURED inside the info thunk rather than read back
       off the head (roadmap step 13): [set_exn] re-mints per attempt, and
       freezing one date up front would let a retried commit land with an
       older date than the racing parent it retried over — breaking the
       branch-date monotonicity the {!Tea_core.Prim.Store_water} witness rests
       on. The last mint is the one the successful attempt carried. *)
    let minted = ref Tea_core.Prim.Store_water.bottom in
    let info () : S.Info.t =
      let i = info_v s.clock label in
      minted := Tea_core.Prim.Store_water.of_date (S.Info.date i);
      i
    in
    let* () =
      (Option.fold s.exploded
         ~none:(fun () -> S.set_exn s.branch model_path_raw model ~info)
         ~some:(fun (_ : exploder) () ->
           let* head = S.Head.find s.branch in
           let base = Option.fold head ~none:(S.Tree.empty ()) ~some:S.Commit.tree in
           let* tree = scatter s.exploded base model in
           S.set_tree_exn s.branch [] tree ~info))
        ()
    in
    Lwt.return !minted

  (* The CRDT context a step on this session applies under (D1): the session's
     own branch name is its stable replica id (a '/'-free framework literal, so
     the trusted mint is right), joined with the handle's single monotonic
     clock so every minted dot is causally-unique and strictly increasing. *)
  (* The ONE branch-name-to-replica mint (roadmap step 13): [ctx_of_session]
     keys the persist-time floors with it and [branch_waters] keys the
     boot-time lookup with it, so the two cannot drift apart. If they ever
     did, every floor would be dropped (mass duplicates) or none would (mass
     blindness), with no symptom either way — which is why the mint has
     exactly one home. *)
  let replica_of_name (name : string) : Tea_core.Crdt.Replica.t =
    Tea_core.Crdt.Replica.v (Tea_core.Prim.Session_id.v name)

  (* The canonical branch's replica, through the one mint above, so the RPC
     channel's single-branch floor key (roadmap step 15) and the boot filter's
     head lookup name the same thing by construction rather than by two
     matching string literals. *)
  let main_replica : Tea_core.Crdt.Replica.t =
    replica_of_name Tea_core.Prim.Branch_name.(to_string main)

  let ctx_of_session (s : session) : Tea_core.Crdt.Ctx.t =
    Tea_core.Crdt.Ctx.v ~clock:s.clock ~replica:(replica_of_name s.name)

  (* The one route from a possibly-absent head commit to a water, shared by
     [head_water] and [branch_waters] so the two cannot diverge. Absence means
     "no claim" ([bottom]) in both: never written, reaped, or gone with a
     restored-older root. *)
  let water_of_commit_opt (head : S.commit option) : Tea_core.Prim.Store_water.t =
    Option.fold head
      ~none:Tea_core.Prim.Store_water.bottom
      ~some:(fun c -> Tea_core.Prim.Store_water.of_date (S.Info.date (S.Commit.info c)))

  let head_water (s : session) : Tea_core.Prim.Store_water.t Lwt.t =
    let* head =
      Lwt.catch (fun () -> S.Head.find s.branch) (fun (_ : exn) -> Lwt.return None)
    in
    Lwt.return (water_of_commit_opt head)

  let branch_waters (t : t) :
      (Tea_core.Crdt.Replica.t * Tea_core.Prim.Store_water.t) list Lwt.t =
    let* names = S.Branch.list t.repo in
    Lwt_list.map_s
      (fun (name : S.branch) ->
        let* head =
          Lwt.catch
            (fun () -> S.Branch.find t.repo name)
            (fun (_ : exn) -> Lwt.return None)
        in
        Lwt.return (replica_of_name name, water_of_commit_opt head))
      names

  (** One TEA step, persisted as one commit. (Cmd effects are the server
      runtime's job; here we persist the model transition and label the commit
      with the Msg so the history reads as an event log.)

      Witnessed since D19: the read mints a token, the step runs off that
      token's model, and the commit tests against the head the model was read
      through, so a writer landing mid-step is reconciled instead of erased.
      The returned model is therefore the {i committed} one, not the caller's
      own transition — under contention the two differ, and the callers that
      immediately assert store state want the former. *)
  let apply (s : session) (msg : A.msg) : A.model Lwt.t =
    let* b = load_based s in
    let model', (_ : A.msg Tea_core.Cmd.t) =
      A.update (ctx_of_session s) msg (based_model b)
    in
    let* (landed : committed) = commit_based b ~label:(Codec.msg_to_label msg) model' in
    Lwt.return landed.model

  let ref_of_commit (c : S.commit) : Tea_core.Prim.Commit_ref.t =
    Tea_core.Prim.Commit_ref.of_hash (Irmin.Type.to_string S.Hash.t (S.Commit.hash c))

  let head_ref (s : session) : Tea_core.Prim.Commit_ref.t option Lwt.t =
    let* head = S.Head.find s.branch in
    match head with
    | None -> Lwt.return None
    | Some c -> Lwt.return (Some (ref_of_commit c))

  (* Total variant of [S.Commit.of_key]: a collected or dangling parent key
     reads as [None] — history truncates, undo bottoms out — instead of
     escaping as a backend exception once pack GC can drop commits. The
     handler is on the open exception type by necessity: backends differ in
     how they surface a missing key (None today, a pack error after GC). *)
  let commit_of_key_opt (repo : S.repo) (key : S.commit_key) : S.commit option Lwt.t =
    Lwt.catch (fun () -> S.Commit.of_key repo key) (fun (_ : exn) -> Lwt.return None)

  (** First-parent commit chain, newest first. *)
  let history (s : session) : Tea_core.Prim.Commit_ref.t list Lwt.t =
    let rec walk c acc =
      let acc = ref_of_commit c :: acc in
      match S.Commit.parents c with
      | [] -> Lwt.return (List.rev acc)
      | pkey :: _ -> (
        let* p = commit_of_key_opt s.repo pkey in
        match p with
        | None -> Lwt.return (List.rev acc)
        | Some pc -> walk pc acc)
    in
    let* head = S.Head.find s.branch in
    match head with
    | None -> Lwt.return []
    | Some c -> walk c []

  (** Move the branch head back one commit. Returns the restored model, or
      [None] when already at the root. Crash-safe redo (D3): the pre-undo head
      is durably recorded on the session's [redo-] ref {i before} the head
      moves back (write-ref-first). A crash between the two strands only a
      harmless redo pointer at [c] — which is still the branch head until the
      move lands — instead of a moved head with no way back and an orphaned
      commit. *)
  let undo (s : session) : A.model option Lwt.t =
    let* head = S.Head.find s.branch in
    Option.fold head ~none:(Lwt.return None) ~some:(fun c ->
        match S.Commit.parents c with
        | [] -> Lwt.return None
        | pkey :: (_ : S.commit_key list) ->
          let* p = commit_of_key_opt s.repo pkey in
          Option.fold p ~none:(Lwt.return None) ~some:(fun pc ->
              let* redo = S.of_branch s.repo (redo_ref_name s) in
              let* () = S.Head.set redo c in
              let* () = S.Head.set s.branch pc in
              let* model = load s in
              Lwt.return (Some model)))

  (** Undo's inverse: read the session's durable [redo-] pointer, move the head
      forward onto it, and clear the pointer. [None] when there is nothing to
      redo. The pointer is single-slot, so a fresh commit or a further undo
      overwrites it; redo restores exactly the last undone step. *)
  let redo (s : session) : A.model option Lwt.t =
    let* redo_b = S.of_branch s.repo (redo_ref_name s) in
    let* target = S.Head.find redo_b in
    Option.fold target ~none:(Lwt.return None) ~some:(fun c ->
        let* () = S.Head.set s.branch c in
        let* () = S.Branch.remove s.repo (redo_ref_name s) in
        let* model = load s in
        Lwt.return (Some model))

  type watch = S.watch

  (** Invoke [k] with the committed model after every head change of [s]'s
      branch — the server half of {!Tea_core.Sub.Store_watch}. Registration
      anchors at the current head ([?init]), so only changes after this call
      fire; a deleted head delivers the app's initial model, mirroring
      {!load} on an empty branch. Callers must {!unwatch} (R3). *)
  let watch (s : session) (k : A.model -> unit Lwt.t) : watch Lwt.t =
    (* A notification can outlive its commit once pack GC lands: fall back to
       the branch head rather than crash the pump. *)
    let deliver (c : S.commit) : unit Lwt.t =
      Lwt.bind (Lwt.catch (fun () -> model_at_with s.exploded c) (fun (_ : exn) -> load s)) k
    in
    let* head = S.Head.find s.branch in
    S.watch s.branch ?init:head (fun (diff : S.commit Irmin.Diff.t) ->
        match diff with
        | `Added c -> deliver c
        | `Updated (_, c) -> deliver c
        | `Removed _ -> k (fst A.init))

  let unwatch (w : watch) : unit Lwt.t = S.unwatch w

  (** Three-way merge [src]'s branch into [dst]'s, invoking the app's merge on
      conflicting paths. This is the collaborative-editing (T2) primitive. *)
  let merge_into ~(src : session) ~(dst : session) : (unit, string) result Lwt.t =
    let* r = S.merge_into ~info:(info_f dst.clock "merge session") src.branch ~into:dst.branch in
    match r with
    | Ok () -> Lwt.return (Ok ())
    | Error (`Conflict msg) -> Lwt.return (Error msg)

  let close (t : t) : unit Lwt.t = S.Repo.close t.repo

  (* --- Step 6: checkpoint squash + commit coalescing --------------------- *)

  type checkpoint = Checkpoint of S.commit

  let checkpoint_commit (Checkpoint c : checkpoint) : S.commit = c
  let checkpoint_ref (Checkpoint c : checkpoint) : Tea_core.Prim.Commit_ref.t = ref_of_commit c

  type checkpoint_error =
    | Empty_branch
    | Branch_moved

  (** Squash the session branch to a single root commit carrying its current
      tree. Test-and-set keeps it lossless: if any writer lands a commit
      between the head read and the move, the squash minted nothing visible
      and the caller gets [Branch_moved] to retry against the new head. *)
  let checkpoint (s : session) ~(label : string) : (checkpoint, checkpoint_error) result Lwt.t =
    let* head = S.Head.find s.branch in
    match head with
    | None -> Lwt.return (Error Empty_branch)
    | Some c ->
      let* squashed =
        S.Commit.v s.repo ~info:(info_v s.clock label) ~parents:[] (S.Commit.tree c)
      in
      let* moved = S.Head.test_and_set s.branch ~test:(Some c) ~set:(Some squashed) in
      if moved then Lwt.return (Ok (Checkpoint squashed))
      else Lwt.return (Error Branch_moved)

  (* --- D3: bounded session reaper ---------------------------------------- *)

  (** Sweep expired session branches: fold [S.Branch.list], skip every reserved
      ref, and [S.Branch.remove] any remaining branch whose head [Info] date (a
      {!Clock} stamp) is strictly older than [now - ttl]. Returns the count
      removed. Bounded by the branch count and single-pass; the only [Lwt.catch]
      fences the per-branch head lookup, which can raise once pack GC has
      dropped a branch's head commit. *)
  let reap ?(forget = fun (_ : Tea_core.Prim.Session_id.t) -> Lwt.return_unit)
      (t : t) ~(ttl : Tea_core.Prim.Ttl.t) ~(now : int64) : int Lwt.t =
    let cutoff = Int64.sub now (Int64.of_float (Tea_core.Prim.Ttl.to_seconds ttl)) in
    let* names = S.Branch.list t.repo in
    Lwt_list.fold_left_s
      (fun (reaped : int) (name : S.branch) ->
        if is_reserved name then Lwt.return reaped
        else
          let* head =
            Lwt.catch (fun () -> S.Branch.find t.repo name) (fun (_ : exn) -> Lwt.return None)
          in
          Option.fold head ~none:(Lwt.return reaped) ~some:(fun c ->
              if Int64.compare (S.Info.date (S.Commit.info c)) cutoff < 0 then
                (* Tombstone before removal (D16): a durable guard floor must
                   die no later than its branch, else a replay onto the
                   recreated branch reads Duplicate against a stale high
                   water — the silent-loss path. A non-session branch name
                   simply has no floor to tombstone. *)
                let forget_victim () : unit Lwt.t =
                  Tea_core.Prim.Session_id.of_string name
                  |> Option.fold ~none:Lwt.return_unit ~some:forget
                in
                let remove_branch () : unit Lwt.t = S.Branch.remove t.repo name in
                let* () = forget_victim () in
                let* () = remove_branch () in
                Lwt.return (reaped + 1)
              else Lwt.return reaped))
      0 names

  (* --- D4: checkpoint retention ring on the __checkpoints spine ---------- *)

  (** The current head of the durable checkpoint spine, if any. *)
  let checkpoints_head (t : t) : checkpoint option Lwt.t =
    let* spine = S.of_branch t.repo spine_branch in
    let* head = S.Head.find spine in
    Lwt.return (Option.map (fun c -> Checkpoint c) head)

  (** Link a fresh [checkpoint] onto the durable [__checkpoints] spine and
      return the single GC [~retain] anchor.

      The spine is a {i chain}: the first checkpoint roots it (the squashed
      commit itself, which the caller's branch already holds), and every later
      checkpoint is a new commit carrying the same tree but parented on the
      prior spine head. The ref persists across restart, so the retention
      window survives a reopen.

      The anchor is the [K]-th checkpoint counting from the head (head = 1st):
      walk back [K-1] parents, bottoming out at the spine root. GC retaining it
      keeps that checkpoint and everything newer — exactly the last [K]
      checkpoints and every branch head minted after the anchor — while older
      spine entries become collectible. Because the anchor is an ancestor of
      the (just-written) spine head, it is never newer than the checkpoints it
      bounds, so retaining it can never drop the newest checkpoint's data. *)
  let retain (t : t) ~(retention : Retention.t) (Checkpoint cp : checkpoint) : checkpoint Lwt.t =
    let* spine = S.of_branch t.repo spine_branch in
    let* spine_head = S.Head.find spine in
    let* linked =
      Option.fold spine_head
        ~none:(Lwt.return cp)
        ~some:(fun prev ->
          S.Commit.v t.repo ~info:(info_v t.clock spine_label)
            ~parents:[ S.Commit.key prev ] (S.Commit.tree cp))
    in
    let* () = S.Head.set spine linked in
    let k = Retention.to_int retention in
    let rec nth (c : S.commit) (steps : int) : S.commit Lwt.t =
      if steps <= 0 then Lwt.return c
      else
        match S.Commit.parents c with
        | [] -> Lwt.return c
        | pkey :: (_ : S.commit_key list) ->
          let* p = commit_of_key_opt t.repo pkey in
          Option.fold p ~none:(Lwt.return c) ~some:(fun pc -> nth pc (steps - 1))
    in
    let* anchor = nth linked (k - 1) in
    Lwt.return (Checkpoint anchor)

  module Coalescer = struct
    (** One coalescer per chatty pipeline (the WS pump mints one per socket).
        [run] remembers the head commit this coalescer minted itself plus the
        Msg it folded so far — the ownership guard: a head produced by anyone
        else (a form post, a merge, an undo) fails the hash check, ends the
        run, and is never amended away. *)
    type t =
      { spec : A.msg Tea_core.Coalesce_spec.t
      ; mutable run : (S.hash * A.msg) option
      }

    let v (spec : A.msg Tea_core.Coalesce_spec.t) : t = { spec; run = None }
    let seal (t : t) : unit = t.run <- None
  end

  let hash_equal : S.hash -> S.hash -> bool = Irmin.Type.(unstage (equal S.Hash.t))

  (* A fresh commit on top of the WITNESSED head, moved by test-and-set and
     reconciled on denial: {!commit_witnessed} plus the one piece of
     bookkeeping the run needs, the hash of the commit this coalescer minted.

     Two things are gone since D19, and both were losses hiding inside the
     mitigation. The three-attempt retry re-committed the same stale model
     against a freshly read head, which is exactly what R10 indicts ("preserves
     history, not content"); and its exhaustion arm sealed the run and fell
     back to a plain [commit], an unconditional last-write-wins [set_exn]
     after three losses, which is R10's own loss mechanism. There is nothing
     left to fall back TO because the loop no longer has a failing case. *)
  let append_commit (cz : Coalescer.t) (b : based) ~(msg : A.msg) (model : A.model) :
      committed Lwt.t =
    let* (c, landed) = commit_witnessed b ~label:(Codec.msg_to_label msg) model in
    cz.Coalescer.run <- Some (S.Commit.hash c, msg);
    Lwt.return landed

  (** Commit one Msg through the coalescer: amend the head while the policy
      keeps folding {i and} the head is one this coalescer minted; otherwise
      append a fresh commit. [Keep_all] takes the historical path bit for
      bit. *)
  let commit_coalesced (cz : Coalescer.t) (b : based) ~(msg : A.msg) (model : A.model) :
      committed Lwt.t =
    let s = b.session in
    match cz.Coalescer.spec with
    | Tea_core.Coalesce_spec.Keep_all -> commit_based b ~label:(Codec.msg_to_label msg) model
    | Tea_core.Coalesce_spec.Fold_run f ->
      let amend =
        (* Pure decision: amendable iff the head is the commit we minted and
           the policy folds the run's Msg with the incoming one.

           Taken against the WITNESS since D19, not against a fresh head read.
           The old read happened after the caller's load, so a writer landing
           in that gap flipped the run to the append path while the model in
           hand was already stale: the amend test was right and its timing was
           wrong. Asking the witness makes the decision and the test-and-set
           agree about which commit this writer actually saw. *)
        Option.bind b.head (fun (h : S.commit) ->
            Option.bind cz.Coalescer.run (fun ((minted : S.hash), (last : A.msg)) ->
                if hash_equal (S.Commit.hash h) minted then
                  Option.map (fun (folded : A.msg) -> (h, folded)) (f ~last ~next:msg)
                else None))
      in
      Option.fold amend
        ~none:(fun () -> append_commit cz b ~msg model)
        ~some:(fun ((h : S.commit), (folded : A.msg)) () ->
          let* tree = scatter s.exploded (S.Commit.tree h) model in
          let* c =
            S.Commit.v s.repo
              ~info:(info_v s.clock (Codec.msg_to_label folded))
              ~parents:(S.Commit.parents h) tree
          in
          let* moved = S.Head.test_and_set s.branch ~test:(Some h) ~set:(Some c) in
          if moved then (
            cz.Coalescer.run <- Some (S.Commit.hash c, folded);
            Lwt.return
              ({ water = Tea_core.Prim.Store_water.of_date (S.Info.date (S.Commit.info c))
               ; model
               ; rounds = 0
               }
                : committed))
          else (
            (* A writer landed mid-amend: their commit wins, ours is
               unreferenced; start a fresh run on top of theirs. The append we
               fall into now RECONCILES against the witness we still hold, so
               that writer keeps their content and not merely their history. *)
            Coalescer.seal cz;
            append_commit cz b ~msg model))
        ()

  let apply_coalesced (cz : Coalescer.t) (s : session) (msg : A.msg) : A.model Lwt.t =
    let* b = load_based s in
    let model', (_ : A.msg Tea_core.Cmd.t) =
      A.update (ctx_of_session s) msg (based_model b)
    in
    let* (landed : committed) = commit_coalesced cz b ~msg model' in
    Lwt.return landed.model
end
