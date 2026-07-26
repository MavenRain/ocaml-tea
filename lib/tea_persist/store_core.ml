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

  val v : now:(unit -> int64) -> S.repo -> t Lwt.t
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

  type checkpoint

  val checkpoint_commit : checkpoint -> S.commit
  val checkpoint_ref : checkpoint -> Tea_core.Prim.Commit_ref.t

  type checkpoint_error =
    | Empty_branch
    | Branch_moved

  val checkpoint : session -> label:string -> (checkpoint, checkpoint_error) result Lwt.t

  module Coalescer : sig
    type t

    val v : msg Tea_core.Coalesce_spec.t -> t
    val seal : t -> unit
  end

  val commit_coalesced : Coalescer.t -> session -> msg:msg -> model -> unit Lwt.t
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

  (** The store handle: the repo plus the one clock every commit date on this
      handle is minted from (a per-branch clock would re-collide across
      branches — the DESIGN §5 dedup bug in another coat). *)
  type t =
    { repo : S.repo
    ; clock : Clock.t
    }

  (** A handle onto one session's branch. [clock] is shared by reference with
      the owning {!t}: one handle, one date order. *)
  type session =
    { repo : S.repo
    ; branch : S.t
    ; clock : Clock.t
    }

  (* Typed store key: a compile-time-literal step, dropped to Irmin's raw
     string-list path at the one bridge. Byte-identical to the former
     [\[ "model" \]]; the byte-compat pin lives in [test/safe_test]. *)
  let model_path = Tea_safe.Safe_key.(root (Step.v "model"))
  let model_path_raw = Tea_safe.Safe_key.to_steps model_path

  let default_now () : int64 = Int64.of_float (Unix.gettimeofday ())

  (* Every commit date is minted by the handle's clock — strictly increasing,
     so identical edits on sibling branches never share a content address
     (DESIGN §5). [info_v] ticks immediately (for [S.Commit.v]); [info_f]
     defers the tick to commit time (for [set_exn]/[merge_into]). *)
  let info_v (clock : Clock.t) (label : string) : S.Info.t =
    S.Info.v ~author:"ocaml-tea" ~message:label (Clock.next clock :> int64)

  let info_f (clock : Clock.t) (label : string) = fun () -> info_v clock label

  let v ~(now : unit -> int64) (repo : S.repo) : t Lwt.t =
    let clock = Clock.create ~now in
    let* heads = S.Repo.heads repo in
    List.iter (fun c -> Clock.seed clock (S.Info.date (S.Commit.info c))) heads;
    Lwt.return { repo; clock }

  let repo (t : t) : S.repo = t.repo

  let session (t : t) (sid : Tea_core.Prim.Session_id.t) : session Lwt.t =
    let name = Tea_core.Prim.Branch_name.(to_string (of_session sid)) in
    let* branch = S.of_branch t.repo name in
    Lwt.return { repo = t.repo; branch; clock = t.clock }

  let main_session (t : t) : session Lwt.t =
    let* branch = S.main t.repo in
    Lwt.return { repo = t.repo; branch; clock = t.clock }

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

  let load (s : session) : A.model Lwt.t =
    let* v = S.find s.branch model_path_raw in
    match v with
    | Some m -> Lwt.return m
    | None -> Lwt.return (fst A.init)

  let commit (s : session) ~(label : string) (model : A.model) : unit Lwt.t =
    S.set_exn s.branch model_path_raw model ~info:(info_f s.clock label)

  (** One TEA step, persisted as one commit. (Cmd effects are the server
      runtime's job; here we persist the model transition and label the commit
      with the Msg so the history reads as an event log.) *)
  let apply (s : session) (msg : A.msg) : A.model Lwt.t =
    let* model = load s in
    let model', _cmd = A.update msg model in
    let* () = commit s ~label:(Codec.msg_to_label msg) model' in
    Lwt.return model'

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
      [None] when already at the root. *)
  let undo (s : session) : A.model option Lwt.t =
    let* head = S.Head.find s.branch in
    match head with
    | None -> Lwt.return None
    | Some c -> (
      match S.Commit.parents c with
      | [] -> Lwt.return None
      | pkey :: _ -> (
        let* p = commit_of_key_opt s.repo pkey in
        match p with
        | None -> Lwt.return None
        | Some pc ->
          let* () = S.Head.set s.branch pc in
          let* model = load s in
          Lwt.return (Some model)))

  (** The model as of one specific commit rather than the branch head: watch
      notifications read the tree they were notified about, so a burst of
      commits yields one frame per commit instead of n reads of whatever the
      final head happens to be. *)
  let model_at (c : S.commit) : A.model Lwt.t =
    let* at = S.of_commit c in
    let* v = S.find at model_path_raw in
    match v with
    | Some m -> Lwt.return m
    | None -> Lwt.return (fst A.init)

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
      Lwt.bind (Lwt.catch (fun () -> model_at c) (fun (_ : exn) -> load s)) k
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

  (* A fresh commit on top of the observed head, moved by test-and-set so a
     racing writer is never overwritten; after repeated interference, fall
     back to the plain event-log commit and break the run. Three attempts:
     interference on a single session is transient (one racing form post or
     merge), so more retries would only defer the safe fallback. *)
  let append_retry_budget = 3

  let append_commit (cz : Coalescer.t) (s : session) ~(msg : A.msg) (model : A.model) :
      unit Lwt.t =
    let label = Codec.msg_to_label msg in
    let rec attempt (fuel : int) : unit Lwt.t =
      if fuel <= 0 then (
        Coalescer.seal cz;
        S.set_exn s.branch model_path_raw model ~info:(info_f s.clock label))
      else
        let* head = S.Head.find s.branch in
        let parents = Option.fold head ~none:[] ~some:(fun h -> [ S.Commit.key h ]) in
        let base_tree = Option.fold head ~none:(S.Tree.empty ()) ~some:S.Commit.tree in
        let* tree = S.Tree.add base_tree model_path_raw model in
        let* c = S.Commit.v s.repo ~info:(info_v s.clock label) ~parents tree in
        let* moved = S.Head.test_and_set s.branch ~test:head ~set:(Some c) in
        if moved then (
          cz.Coalescer.run <- Some (S.Commit.hash c, msg);
          Lwt.return_unit)
        else attempt (fuel - 1)
    in
    attempt append_retry_budget

  (** Commit one Msg through the coalescer: amend the head while the policy
      keeps folding {i and} the head is one this coalescer minted; otherwise
      append a fresh commit. [Keep_all] takes the historical path bit for
      bit. *)
  let commit_coalesced (cz : Coalescer.t) (s : session) ~(msg : A.msg) (model : A.model) :
      unit Lwt.t =
    match cz.Coalescer.spec with
    | Tea_core.Coalesce_spec.Keep_all -> commit s ~label:(Codec.msg_to_label msg) model
    | Tea_core.Coalesce_spec.Fold_run f ->
      let* head = S.Head.find s.branch in
      let amend =
        (* Pure decision: amendable iff the head is the commit we minted and
           the policy folds the run's Msg with the incoming one. *)
        Option.bind head (fun h ->
            Option.bind cz.Coalescer.run (fun (minted, last) ->
                if hash_equal (S.Commit.hash h) minted then
                  Option.map (fun folded -> (h, folded)) (f ~last ~next:msg)
                else None))
      in
      (match amend with
       | None -> append_commit cz s ~msg model
       | Some (h, folded) ->
         let* tree = S.Tree.add (S.Commit.tree h) model_path_raw model in
         let* c =
           S.Commit.v s.repo
             ~info:(info_v s.clock (Codec.msg_to_label folded))
             ~parents:(S.Commit.parents h) tree
         in
         let* moved = S.Head.test_and_set s.branch ~test:(Some h) ~set:(Some c) in
         if moved then (
           cz.Coalescer.run <- Some (S.Commit.hash c, folded);
           Lwt.return_unit)
         else (
           (* A writer landed mid-amend: their commit wins, ours is unreferenced;
              start a fresh run on top of theirs. *)
           Coalescer.seal cz;
           append_commit cz s ~msg model))

  let apply_coalesced (cz : Coalescer.t) (s : session) (msg : A.msg) : A.model Lwt.t =
    let* model = load s in
    let model', _cmd = A.update msg model in
    let* () = commit_coalesced cz s ~msg model' in
    Lwt.return model'
end
