(** The versioned model store: the "Irmin instead of SQLite" core.

    For an [APP], the model lives on a per-session Irmin branch at path
    [\["model"\]]. Every TEA step is exactly one commit whose message is the
    serialized Msg, so the commit log {i is} the event log: undo/redo and
    time-travel are [Head]/parents operations, not bespoke features. Concurrent
    sessions reconcile through Irmin's tree merge, which dispatches to the
    [Contents.merge] lifted from the app's {!Tea_core.Merge_spec}.

    This scaffold uses the in-memory backend ([irmin.mem]); swapping to
    [irmin-git] (durable, git-inspectable) or [irmin-pack] (scale + GC) is a
    change of functor argument only. *)

open Lwt.Syntax

module Make (A : Tea_core.App.APP) = struct
  module Codec = Tea_core.Codec.Make (A)

  (* Lift the pure merge policy into an Irmin Contents merge. Irmin only invokes
     this when both branches changed the same path, so single-session use never
     hits it. *)
  module Contents = struct
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

  module S = Irmin_mem.KV.Make (Contents)
  module Info = Irmin_unix.Info (S.Info)

  type t = S.repo

  (** A handle onto one session's branch. *)
  type session =
    { repo : S.repo
    ; branch : S.t
    }

  (* Typed store key: a compile-time-literal step, dropped to Irmin's raw
     string-list path at the one bridge. Byte-identical to the former
     [\[ "model" \]]; the byte-compat pin lives in [test/safe_test]. *)
  let model_path = Tea_safe.Safe_key.(root (Step.v "model"))
  let model_path_raw = Tea_safe.Safe_key.to_steps model_path
  let info label = Info.v ~author:"ocaml-tea" "%s" label

  let create () : t Lwt.t = S.Repo.v (Irmin_mem.config ())

  let session (repo : t) (sid : Tea_core.Prim.Session_id.t) : session Lwt.t =
    let name = Tea_core.Prim.Branch_name.(to_string (of_session sid)) in
    let* branch = S.of_branch repo name in
    Lwt.return { repo; branch }

  let main_session (repo : t) : session Lwt.t =
    let* branch = S.main repo in
    Lwt.return { repo; branch }

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
  let fork (repo : t) ~(from : session) (sid : Tea_core.Prim.Session_id.t) : session Lwt.t =
    let* dst = session repo sid in
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
    S.set_exn s.branch model_path_raw model ~info:(info label)

  (** One TEA step, persisted as one commit. (Cmd effects are the server
      runtime's job; here we persist the model transition and label the commit
      with the Msg so the history reads as an event log.) *)
  let apply (s : session) (msg : A.msg) : A.model Lwt.t =
    let* model = load s in
    let model', _cmd = A.update msg model in
    let* () = commit s ~label:(Codec.msg_to_label msg) model' in
    Lwt.return model'

  let head_ref (s : session) : Tea_core.Prim.Commit_ref.t option Lwt.t =
    let* head = S.Head.find s.branch in
    match head with
    | None -> Lwt.return None
    | Some c ->
      let h = Irmin.Type.to_string S.Hash.t (S.Commit.hash c) in
      Lwt.return (Some (Tea_core.Prim.Commit_ref.of_hash h))

  (** First-parent commit chain, newest first. *)
  let history (s : session) : Tea_core.Prim.Commit_ref.t list Lwt.t =
    let ref_of c =
      Tea_core.Prim.Commit_ref.of_hash (Irmin.Type.to_string S.Hash.t (S.Commit.hash c))
    in
    let rec walk c acc =
      let acc = ref_of c :: acc in
      match S.Commit.parents c with
      | [] -> Lwt.return (List.rev acc)
      | pkey :: _ -> (
        let* p = S.Commit.of_key s.repo pkey in
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
        let* p = S.Commit.of_key s.repo pkey in
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
    let* head = S.Head.find s.branch in
    S.watch s.branch ?init:head (fun (diff : S.commit Irmin.Diff.t) ->
        match diff with
        | `Added c -> Lwt.bind (model_at c) k
        | `Updated (_, c) -> Lwt.bind (model_at c) k
        | `Removed _ -> k (fst A.init))

  let unwatch (w : watch) : unit Lwt.t = S.unwatch w

  (** Three-way merge [src]'s branch into [dst]'s, invoking the app's merge on
      conflicting paths. This is the collaborative-editing (T2) primitive. *)
  let merge_into ~(src : session) ~(dst : session) : (unit, string) result Lwt.t =
    let* r = S.merge_into ~info:(info "merge session") src.branch ~into:dst.branch in
    match r with
    | Ok () -> Lwt.return (Ok ())
    | Error (`Conflict msg) -> Lwt.return (Error msg)
end
