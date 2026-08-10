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
module Rpc_once = Tea_server.Rpc_once

type guards =
  { ws : Durable_guard.t
  ; ws_journal : Guard_file.t option
  ; rpc : Durable_guard.t
  ; rpc_journal : Guard_file.t option
  }
(** Both delivery channels' guards, opened as one unit (roadmap step 15,
    D20.3). A journal that failed to open is [None] beside a null-sink guard,
    never an absent channel: the caller serves at-least-once on that channel
    rather than not at all. *)

(** The boot line an {!Guard_file.identity_outcome} earns, or [None] when it
    earns none.

    Pure, and a function of the BINDING as well as the outcome, because one
    outcome means two different things depending on it.
    [Adopted_unbound] under a bound caller ends with the journal stamped, so
    the next boot is protected; under
    {!Tea_core.Prim.Store_identity.Unresolved} there is no token to stamp
    with, {!Guard_file.open_} writes nothing, and the journal is still unbound
    when the process exits. One sentence promising protection would then be
    false at exactly the boot an operator reads it. *)
let explain_outcome ~(binding : Tea_core.Prim.Store_identity.binding)
    ~(channel : string) (outcome : Guard_file.identity_outcome) : string option =
  let adopted (n : int) : string =
    match binding with
    | Tea_core.Prim.Store_identity.Bound (_ : Tea_core.Prim.Store_identity.t) ->
      Printf.sprintf
        "tea_server_pack: %s channel: adopted %d delivery floor(s) from a journal carrying NO store-identity binding (a pre-step-18 journal, or one whose header was lost); this boot is NOT protected against a journal restored beside a DIFFERENT store, and the journal is bound to this store now, so the next boot IS protected\n"
        channel n
    | Tea_core.Prim.Store_identity.Unresolved ->
      Printf.sprintf
        "tea_server_pack: %s channel: adopted %d delivery floor(s) from a journal carrying NO store-identity binding, and this pack root's own identity token could not be read or minted, so there is nothing to stamp the journal WITH; the journal REMAINS UNBOUND and the next boot is NOT protected against a journal restored beside a DIFFERENT store\n"
        channel n
  in
  match outcome with
  | Guard_file.Matched | Guard_file.Freshly_bound -> None
  | Guard_file.Adopted_unbound n -> if n > 0 then Some (adopted n) else None
  | Guard_file.Rebound n ->
    Some
      (Printf.sprintf
         "tea_server_pack: %s channel: dropped ALL %d delivery floor(s): this guard journal is bound to a DIFFERENT pack store than the one at this root (a journal restored beside the wrong store, or a store replaced under a surviving journal); the affected replays will re-apply as visible duplicates rather than be silently lost, and the journal is re-bound to this store now\n"
         channel n)
  | Guard_file.Unresolved_cleared n ->
    Some
      (Printf.sprintf
         "tea_server_pack: %s channel: dropped %d delivery floor(s) because this pack root's own identity token could not be read or minted; the journal's binding is LEFT INTACT and nothing is re-stamped or appended, so a boot that can read the token again keeps its floors - until then every boot re-applies the affected replays as visible duplicates\n"
         channel n)

(** Open both channels' guard journals under [guard_dir].

    Since roadmap step 15 there are TWO journals, one per delivery channel:
    the websocket channel keeps [<root>.guard] byte for byte, and the keyed RPC
    channel gets [<root>.guard/rpc]. They are separate files rather than one
    file split at boot because the channels have different bounds, different
    caps and different compaction pressure, and because a step-14 root must
    open here with the RPC journal simply ABSENT (created on first keyed take)
    while a step-14 binary reading a step-15 root never looks inside the
    subdirectory at all. Nesting the RPC journal under the websocket one keeps
    the sibling discipline intact: [.guard] is still the single durability
    artifact a restore has to keep beside the pack root.

    The two opens live in ONE function, and not at two call sites, because
    what relates them is load-bearing twice over. The ORDER is: [Guard_file.
    open_] creates its own [dir] but never the parent, so the websocket open
    is what brings [<root>.guard] into existence, and an rpc-first boot would
    die ENOENT on a fresh root. The HEAD SNAPSHOT is: both boot filters are
    checked against the same [head_water], taken once by the caller between
    the store open and this call, so the two channels cannot straddle a write
    and judge their floors against different heads.

    Separate from {!Make_pack} because neither channel's composition depends on
    the app: it is the same pair for every [A], and a test can drive the real
    composition against a temp root without binding a port. That matters here
    more than it reads — the directory each channel is opened at is exactly the
    thing that must not drift, and inside a blocking [serve_pack] no test could
    observe it. *)
let open_guards ~(guard_dir : string)
    ~(head_water :
       Tea_core.Crdt.Replica.t -> Tea_core.Prim.Store_water.t option)
    ~(identity : Tea_core.Prim.Store_identity.binding)
    ?(fence : (unit -> unit Lwt.t) option) () : guards =
  let open_journal ~(channel : string) ~(dir : string) ~(cap : int)
      ~(sessions : Replay_guard.Bound.t) ~(tabs : Replay_guard.Bound.t) :
      Durable_guard.t * Guard_file.t option =
    Lwt_main.run (Guard_file.open_ ~dir ~cap ~head_water ~identity)
    |> Result.fold
         ~ok:(fun
             ((sink, floors, verdict, identity_outcome, jf) :
               Guard_sink.t
               * Durable_guard.Floors.t
               * Guard_file.verdict
               * Guard_file.identity_outcome
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
                "tea_server_pack: %s channel: dropped %d delivery floor(s) standing ABOVE their branch heads: the pack root is OLDER than the guard journal (a restored snapshot, a rolled-back store); the affected replays will re-apply as visible duplicates rather than be silently lost\n%!"
                channel dropped_behind);
           (if dropped_no_branch > 0 then
              Printf.eprintf
                "tea_server_pack: %s channel: dropped %d delivery floor(s) with an unreadable or absent branch head — a collected, reaped, or never-written branch (routine after checkpoint GC), or a pack root restored from BEFORE the branch existed; either way the affected replays re-apply as visible duplicates rather than be silently lost\n%!"
                channel dropped_no_branch);
           (if unwitnessed > 0 then
              Printf.eprintf
                "tea_server_pack: %s channel: adopted %d delivery floor(s) with no store witness (pre-step-13 journal); this boot is NOT protected against a restored-older pack root\n%!"
                channel unwitnessed);
           (* The wording is decided by a pure function of the outcome AND the
              binding, so what an operator reads can be asserted by a test
              rather than grepped out of a captured stderr. *)
           Option.iter
             (fun (line : string) -> Printf.eprintf "%s%!" line)
             (explain_outcome ~binding:identity ~channel identity_outcome);
           (* The mirror bound is DERIVED at this composition site, never a
              constant (D20.4), and this is the only place that knows [cap]:
              the guard reaches its journal through an append-only sink and
              cannot ask. Passing it is what keeps the boot fold from
              overflowing the very table it is refilling. *)
           ( Durable_guard.v ~sessions ~tabs
               ~mirror:(Durable_guard.default_mirror ~sessions ~tabs ~journal_cap:cap ())
               ?fence ~sink ~floors ()
           , Some jf ))
         ~error:(fun (e : Guard_file.open_err) ->
           let reason =
             match e with
             | Guard_file.Io reason -> reason
             | Guard_file.Bad_dir reason -> reason
           in
           Printf.eprintf
             "tea_server_pack: %s channel: guard journal unavailable (%s); serving at-least-once\n%!"
             channel reason;
           (* No journal, hence no cap to clear: the sinkless derivation, the
              mem tier's composition exactly — including NO fence. A null
              sink appends nothing, so there is nothing to order, and a live
              fence here would bill [Store.flush] per message (or turn an
              unconditionally [Ok] persist into [Error]) for a zero
              invariant. *)
           ( Durable_guard.v ~sessions ~tabs ~sink:Guard_sink.null
               ~floors:Durable_guard.Floors.empty ()
           , None ))
  in
  let ws, ws_journal =
    open_journal ~channel:"websocket" ~dir:guard_dir ~cap:32768
      ~sessions:Replay_guard.default_sessions ~tabs:Replay_guard.default_tabs
  in
  let rpc, rpc_journal =
    open_journal ~channel:"rpc"
      ~dir:(Filename.concat guard_dir "rpc")
      ~cap:16384 ~sessions:Replay_guard.default_rpc_replicas
      ~tabs:Replay_guard.default_rpc_tabs
  in
  { ws; ws_journal; rpc; rpc_journal }

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
  let serve_pack ?(interface = "localhost") ?(port = 8080) ?client_dir ?rpc ?rpc_once
      ?coalesce ?retention ?lower_root ?sessions ~(root : Root.t) () : unit =
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
       newer head passes the filter. Step 18 (D23) answers that with a
       store-identity token minted once into [<root>/tea.identity] and stamped
       into each journal's first frame, so a journal beside an INDEPENDENT
       store drops its floors instead of trusting them. What the token still
       cannot answer is a divergent COPY of one lineage - a clone carries the
       token unchanged (DESIGN R20b residual). *)
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
       no server, and the degradation direction is duplicate, never loss.

       The step-18 store-identity token IS inside the root, and that is not the
       same bet: [Irmin_pack.Layout.Classification.Upper.v] is a closed match on
       the [store.*] naming scheme whose fallthrough is [`Unknown], the post-GC
       [cleanup] sweep never removes an [`Unknown] entry, and [open_rw]/[open_ro]
       do not scan the directory at all - so a [tea.]-prefixed file is invisible
       to open, to GC, and to migration. The token has to live there: a fourth
       [<root>.identity] sibling would be R20 recursed one level, and only a
       file inside the tree travels with the [cp -r] that creates R20 in the
       first place. *)
    (* ONE branch_waters read, taken between the store open and the guard opens,
       feeds BOTH boot filters: each journal's floors are checked against the
       heads THIS boot will actually serve from, and reading twice could
       straddle a write and judge the two channels against different heads. *)
    let head_water =
      Lwt_main.run
        (Lwt.map Guard_file.head_water_of_list (Store.branch_waters repo))
    in
    (* The store's own lineage token, resolved ONCE here. AFTER [open_root]
       because the backend does exactly one [mkdir], and a [tea.identity]
       minted into a directory nothing else had written would leave a root the
       next boot classifies [Root_not_a_pack_store]. BEFORE [open_guards]
       because one value has to reach both channels: two reads could disagree
       across a mint race and bind the two journals to different tokens. *)
    let identity, identity_origin = Store.resolve_identity root in
    Printf.eprintf "tea_server_pack: %s\n%!" (Store.explain_identity identity_origin);
    (* Both channels' journals, opened together and against this one head
       snapshot (D20.3). The directory each channel lands in, and the order the
       two are opened in, live in {!open_guards} rather than here so a native
       test can drive the real composition without binding a port. *)
    let { ws = guard; ws_journal = journal; rpc = rpc_guard; rpc_journal } =
      (* The commit fence is hard-wired, not a [serve_pack] parameter: a
         production boot with the fence off would quietly go back to leaning
         on irmin-pack's undocumented batch-end flush, and no caller has a
         legitimate reason to want that. The fence is what makes the
         floor/commit ordering THIS repo's invariant. *)
      open_guards ~guard_dir ~head_water ~identity
        ~fence:(fun () -> Store.flush repo) ()
    in
    (* [?rpc_once] takes a route BUILDER for the reason {!Tea_server.Make.serve}
       gives, and here the reason is at its strongest: the guard it wraps is
       backed by a journal THIS function opened three lines ago and will close
       at teardown, so no caller could have composed the value in advance. The
       repo is handed over for the same reason and is just as load-bearing: the
       app's keyed handler commits through {!step}, this function is what opened
       the store it commits to, and an app cannot reopen a pack root a live
       server already holds.

       [floor_replica] is [main] because that is the branch the boot filter
       above just checked these floors against; the single-branch contract
       (D20, R26) is what makes the two agree. *)
    let rpc =
      Option.fold rpc_once ~none:(fun () -> rpc)
        ~some:(fun (build : Store.t -> Tea_server.Rpc_once.t -> Dream.route list) () ->
          Some
            (Option.value rpc ~default:[]
            @ build repo
                (Tea_server.Rpc_once.v ~guard:rpc_guard
                   ~floor_replica:Store.main_replica)))
        ()
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
       teardown this ordering is what enforces it.

       One window survives the ordering: a cancelled span's orphan body
       (R10d) can resume inside THIS [Lwt_main.run] and reach [persist]
       after the repo closed. Its fence then rejects on the closed repo,
       and [persist] degrades that to [Error] with no floor appended — one
       audible line, the duplicate direction. That is why the fence must
       reject rather than swallow (see [Store_pack.flush]): a swallowing
       fence would append that orphan's floor against a closed repo. *)
    Lwt_main.run
      (let open Lwt.Syntax in
       let* () = Store.close repo in
       (* Both journals, and the order BETWEEN them does not matter for the same
          reason the order against the repo does: each channel's floors are only
          ever compared against commits, never against the other channel's. What
          matters is that neither is skipped, since an unclosed journal is the
          silent-loss direction this whole teardown exists to avoid. *)
       let* () = Option.fold journal ~none:Lwt.return_unit ~some:Guard_file.close in
       Option.fold rpc_journal ~none:Lwt.return_unit ~some:Guard_file.close))
end
