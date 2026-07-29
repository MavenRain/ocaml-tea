(** The durable pack-backed Dream tier (roadmap step 8, D2).

    [Make_pack (A)] instantiates the {i same} {!Tea_server.Handlers} bodies as
    the mem tier, but over {!Tea_persist_pack.Store_pack} rather than the
    in-memory store, and adds one Origin-gated endpoint:
    [POST /admin/checkpoint], which squashes [main], links the checkpoint onto
    the durable retention spine, and GCs to the retention anchor.

    Living in its own library is deliberate: it keeps irmin-pack (and Unix) out
    of [tea_server]'s dependency closure, so the js_of_ocaml client tier never
    drags a native-only backend into its link. *)

module Root = Tea_persist_pack.Store_pack.Root
module Guard_file = Guard_file
module Guard_sink = Tea_server.Guard_sink
module Durable_guard = Tea_server.Durable_guard
module Replay_guard = Tea_server.Replay_guard
module Session_secret = Tea_server.Session_secret

module Make_pack (A : Tea_core.App.APP) = struct
  module Store = Tea_persist_pack.Store_pack.Make (A)
  include Tea_server.Handlers (A) (Store)
  open Lwt.Syntax

  let checkpoint_path = "/admin/checkpoint"

  (** [POST /admin/checkpoint]: the same same-origin discipline as [accept_ws]
      — the only producer of the proof is {!Tea_safe.Origin_gate.check}, so a
      checkpoint accepted without it cannot be expressed. On a valid proof,
      squash [main], link the checkpoint onto the durable retention spine, and
      GC to the retention anchor. Every {!Tea_safe.Origin_gate.denial}, every
      {!Store.checkpoint_error}, and every {!Store.gc_error} is matched
      exhaustively. *)
  let handle_checkpoint ?(retention = Store.Retention.default) (repo : Store.t)
      (request : Dream.request) : Dream.response Lwt.t =
    Tea_safe.Origin_gate.check
      ~origin:(Dream.header request "Origin")
      ~host:(Dream.header request "Host")
    |> Result.fold
         ~error:(fun (d : Tea_safe.Origin_gate.denial) ->
           match d with
           | Origin_missing | Host_missing | Both_missing | Origin_mismatch ->
             Dream.respond ~status:`Forbidden "cross-origin checkpoint rejected")
         ~ok:(fun (_ : Tea_safe.Origin_gate.same_origin Tea_safe.Proof.t) ->
           let* main = Store.main_session repo in
           let* checkpointed = Store.checkpoint main ~label:"admin checkpoint" in
           Result.fold checkpointed
             ~error:(fun (e : Store.checkpoint_error) ->
               match e with
               | Empty_branch ->
                 Dream.respond ~status:`OK "checkpoint: empty branch, nothing to do"
               | Branch_moved ->
                 Dream.respond ~status:`Conflict "checkpoint: branch moved, retry")
             ~ok:(fun cp ->
               let* anchor = Store.retain repo ~retention cp in
               let* gced = Store.gc repo ~retain:anchor in
               Result.fold gced
                 ~error:(fun (ge : Store.gc_error) ->
                   match ge with
                   | Gc_disallowed ->
                     Dream.respond ~status:`Internal_Server_Error "gc disallowed"
                   | Gc_already_running -> Dream.respond ~status:`OK "gc already running"
                   | Gc_failed (_ : string) ->
                     Dream.respond ~status:`Internal_Server_Error "gc failed")
                 ~ok:(fun () ->
                   Dream.respond ~status:`OK
                     (Tea_core.Prim.Commit_ref.to_string (Store.checkpoint_ref cp)))))

  let checkpoint_route ?retention (repo : Store.t) : Dream.route =
    Dream.post checkpoint_path (handle_checkpoint ?retention repo)

  (** The full pack request pipeline: the mem tier's {!handler} with
      [POST /admin/checkpoint] folded into the RPC route list, so it inherits
      the session and security-header middleware like every other route. *)
  let handler_pack ?client_dir ?(rpc = []) ?coalesce ?retention ?guard ?sessions
      (repo : Store.t) : Dream.handler =
    handler ?client_dir ~rpc:(checkpoint_route ?retention repo :: rpc) ?coalesce
      ?guard ?sessions repo

  (** The two durability siblings are named off the root, never nested inside
      it. [Root.v] keeps its string verbatim, so a root spelled with a trailing
      separator ([TEA_ROOT=/data/store/], one keystroke) would otherwise put
      [.secret] and [.guard] INSIDE the directory irmin-pack owns, which is
      exactly the bet the journal comment in {!serve_pack} refuses to make.
      Paths without a trailing separator are concatenated byte for byte, as
      before. *)
  let sibling (root : Root.t) (ext : string) : string =
    let s = Root.to_string root in
    if String.ends_with ~suffix:Filename.dir_sep s then
      Filename.concat (Filename.dirname s) (Filename.basename s ^ ext)
    else s ^ ext

  (** Blocking entry point for a durable pack server. A stop signal resolves
      Dream's [~stop] promise; once Dream returns, the repo is closed so the
      pack suffix is flushed to disk — mutation survival across a restart
      depends on this teardown close.

      Both SIGINT and SIGTERM are handled, and the second one matters as much as
      the first: SIGINT is what a terminal sends, but SIGTERM is what every
      supervisor sends — including the browser harness, which restarts this
      binary to observe that de-duplication survives a restart. A SIGTERM that
      bypassed this handler would kill the process before the teardown close,
      so the reopened store would be missing its most recent suffix and the
      scenario would be measuring a lost store rather than a lost delivery
      record. The wake is guarded by [is_sleeping], so a second signal (or both
      signals) resolves the promise once. *)
  let serve_pack ?(interface = "localhost") ?(port = 8080) ?client_dir ?rpc ?coalesce ?retention
      ?lower_root ?sessions ~(root : Root.t) () : unit =
    (* The three durability siblings are three separate paths with no
       cross-binding, so a restore or a manual wipe can keep some and drop
       others (DESIGN R20). One direction is silent LOSS rather than the
       duplicate this system always degrades towards: with the pack root gone
       but [.guard] and [.secret] surviving, a returning tab's cookie still
       decrypts to its old session id, hence its old branch name and its old
       replica id, hence its old guard key. The branch is gone so the model
       materialises at bottom, but the floor is still there, so the next
       replayed message is judged Duplicate and dropped onto an empty model.
       Refusing loudly is the whole fix for the wipe case. A rollback to an
       OLDER pack snapshot under a NEWER journal leaves the root present, so
       this preflight cannot see it; that case is caught below instead, at
       guard-open time: every floor carries the store water it was taken
       under, and the boot filter drops floors the restored branch heads no
       longer cover — an audible drop, after which the replays land Fresh
       (visible duplicates, never silent loss). What ordering cannot answer
       is IDENTITY: a DIFFERENT store whose same-named branch stands at a
       newer head passes the filter (DESIGN R20a residual). *)
    let guard_dir = sibling root ".guard" in
    if (not (Sys.file_exists (Root.to_string root))) && Sys.file_exists guard_dir then (
      Printf.eprintf
        "tea_server_pack: guard journal orphaned: %s holds delivery floors but the pack root %s does not exist; not serving, because serving would drop returning clients' replays onto empty branches. Remove %s to start clean, or restore the pack root beside it.\n%!"
        guard_dir (Root.to_string root) guard_dir;
      exit 1)
    else
    (* The preflight (roadmap step 12): an unusable root - a plain existing
       directory, a missing parent, a file - used to reach irmin-pack raw and
       die as an uncaught [Pack_error "Invalid_layout"] on this very first
       line. Refusal is one audible stderr line, no listen, and a NON-ZERO
       exit: this function returning [unit] would otherwise end the binary at
       status 0, and a supervisor (systemd [Restart=on-failure], k8s
       [restartPolicy: OnFailure], a CI gate) cannot tell a root it never
       opened from a clean shutdown. The raw [Pack_error] it replaced at least
       exited non-zero. Nothing is created beside a root that was refused, so
       the error path leaves no .secret and no .guard sibling behind. *)
    Lwt_main.run (Store.open_root ?lower_root root)
    |> Result.fold
         ~error:(fun (e : Store.open_error) ->
           Printf.eprintf "tea_server_pack: pack root unusable (%s): %s; not serving\n%!"
             (Root.to_string root) (Store.explain e);
           exit 1)
         ~ok:(fun (repo : Store.t) ->
    (* Resolved AFTER the store opens (an unusable root never leaves a stray
       .secret beside it) and BEFORE the guard journal, its durability sibling.
       Both arms are closures applied exactly once: Option.fold's [~none:] is
       eager, and an eager arm here would mint a secret file even when the
       caller supplied a back end. Degradation is one audible line and a
       per-process identity - the same direction as the journal below: a
       server without durability beats no server. *)
    let sessions =
      Option.fold sessions
        ~none:(fun () ->
          Session_secret.resolve ~file:(sibling root ".secret") ()
          |> Result.fold
               ~ok:(fun (s : Session_secret.t) -> s)
               ~error:(fun (e : Session_secret.err) ->
                 Printf.eprintf
                   "tea_server_pack: session secret unavailable (%s); identity is PER-PROCESS, so a restart lands every tab on a fresh branch\n%!"
                   (Session_secret.explain e);
                 Session_secret.memory))
        ~some:(fun (s : Session_secret.t) () -> s)
        ()
    in
    Printf.eprintf "tea_server_pack: %s\n%!" (Session_secret.describe sessions);
    (* The guard journal lives in a SIBLING directory of the pack root, never
       inside it: whether irmin-pack 3.11 tolerates a foreign file in its root
       across GC and migration is not a bet worth making. A failed open is one
       audible line and a null-sink guard — a server without durability beats
       no server, and the degradation direction is duplicate, never loss. *)
    let guard, journal =
      Lwt_main.run
        (let open Lwt.Syntax in
         (* One branch_waters read, taken between the store open and the
            guard open, feeds the boot filter: the journal's floors are
            checked against the heads THIS boot will actually serve from. *)
         let* waters = Store.branch_waters repo in
         Guard_file.open_ ~dir:guard_dir ~cap:32768
           ~head_water:(Guard_file.head_water_of_list waters))
      |> Result.fold
           ~ok:(fun
               ((sink, floors, verdict, jf) :
                 Guard_sink.t
                 * Durable_guard.Floors.t
                 * Guard_file.verdict
                 * Guard_file.t)
             ->
             let { Guard_file.kept = (_ : int)
                 ; dropped_behind
                 ; dropped_no_branch
                 ; unwitnessed
                 } =
               verdict
             in
             (if dropped_behind > 0 then
                Printf.eprintf
                  "tea_server_pack: dropped %d delivery floor(s) standing ABOVE their branch heads: the pack root is OLDER than the guard journal (a restored snapshot, a rolled-back store); the affected replays will re-apply as visible duplicates rather than be silently lost\n%!"
                  dropped_behind);
             (if dropped_no_branch > 0 then
                Printf.eprintf
                  "tea_server_pack: dropped %d delivery floor(s) with an unreadable or absent branch head — a collected, reaped, or never-written branch (routine after checkpoint GC), or a pack root restored from BEFORE the branch existed; either way the affected replays re-apply as visible duplicates rather than be silently lost\n%!"
                  dropped_no_branch);
             (if unwitnessed > 0 then
                Printf.eprintf
                  "tea_server_pack: adopted %d delivery floor(s) with no store witness (pre-step-13 journal); this boot is NOT protected against a restored-older pack root\n%!"
                  unwitnessed);
             ( Durable_guard.v ~sessions:Replay_guard.default_sessions
                 ~tabs:Replay_guard.default_tabs ~sink ~floors
             , Some jf ))
           ~error:(fun (e : Guard_file.open_err) ->
             let reason =
               match e with
               | Guard_file.Io reason -> reason
               | Guard_file.Bad_dir reason -> reason
             in
             Printf.eprintf
               "tea_server_pack: guard journal unavailable (%s); serving at-least-once\n%!"
               reason;
             ( Durable_guard.v ~sessions:Replay_guard.default_sessions
                 ~tabs:Replay_guard.default_tabs ~sink:Guard_sink.null
                 ~floors:Durable_guard.Floors.empty
             , None ))
    in
    let stop, wake = Lwt.wait () in
    List.iter
      (fun signal ->
        let (_ : Lwt_unix.signal_handler_id) =
          Lwt_unix.on_signal signal (fun (_ : int) ->
              if Lwt.is_sleeping stop then Lwt.wakeup_later wake ())
        in
        ())
      [ Sys.sigint; Sys.sigterm ];
    Lwt_main.run
      (Dream.serve ~interface ~port ~stop
         (Dream.logger
            (handler_pack ?client_dir ?rpc ?coalesce ?retention ~guard ~sessions repo)));
    (* The REPO first, then the journal — the order is load-bearing. Both
       fsync on this path, and a crash between the two closes must land on
       the duplicate side: repo-first leaves commits durable and floors
       possibly behind (a replay reads Fresh, re-applies, visible duplicate);
       journal-first would leave floors durable over commits still in the
       pack's user-space buffer, and a replay would then read Duplicate
       against a commit that never reached disk — silent loss. "Floor
       durability never exceeds commit durability" is the invariant, and at
       teardown this ordering is what enforces it. *)
    Lwt_main.run
      (let open Lwt.Syntax in
       let* () = Store.close repo in
       Option.fold journal ~none:Lwt.return_unit ~some:Guard_file.close))
end
