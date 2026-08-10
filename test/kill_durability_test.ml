(** The durability guarantee under a real SIGKILL (roadmap step 20, R11/D16).

    Every other step-20 check drives the fence through doubles; this file is
    the executed crash. A child process, forked BEFORE this image ever runs
    Lwt, opens the pack root and both guard journals itself, pumps K
    deterministic messages in the pump's documented order (commit, then
    [Durable_guard.persist] through the websocket guard), and reports each
    persisted floor as ONE marker byte on a pipe, written only after that
    persist's promise resolved [Ok]. The parent blocking-reads N markers
    (N < K would leave the child mid-pump; the child in fact finishes and
    parks on a pipe the parent never writes), delivers kill -9, and only
    after [waitpid] confirms the SIGKILL reopens the SAME root and guard dir
    in-process to ask the D16 question: every floor the boot filter KEPT is
    covered by the restored head.

    SENSITIVITY IS BOUNDED, by discovery rather than by choice: irmin-pack
    3.11 ends every write batch in [File_manager.flush] on the success path,
    and [Commit.v] runs inside such a batch, so a commit's suffix/dict bytes
    are already in the page cache BEFORE [Head.test_and_set] moves the
    branch head and long before any floor append. NO repetition of this
    harness can therefore manufacture the loss the fence exists to prevent:
    the R11 window (floor more durable than its commit) is unreachable
    through this repo's public API on the pump path. What this file proves
    under a real SIGKILL is the POSITIVE durability guarantee (every kept
    floor covered, nothing dropped behind, fenced and unfenced alike) and
    the kill discipline itself (WSIGNALED [Sys.sigkill] EXACTLY, marker-
    gated timing). M4 (serve_pack's fence rewired to a no-op) is therefore
    a DOCUMENTED WEAK MUTANT at this harness's level: rep (b) executes
    exactly that wiring and must pass the same assertions. The fence's value
    is converting that EMERGENT, undocumented upstream flush into a LOCAL
    invariant of [Durable_guard.persist]'s call order, which the
    fence-double tests pin; this file pins the discovered invariant itself.

    Three repetitions (fresh root and guard dir each):
    (a) the real fence, wired exactly as [serve_pack] wires it
        ([~fence:(fun () -> Store.flush repo)]), kill -9 mid-life: the
        killed child's floor comes back kept and covered.
    (b) the fence ABSENT, same kill, SAME assertions as (a). This rep was
        first cut as a loss-demonstrating positive control and CANNOT be
        one: even with the cumulative payload far under the 16_384-byte
        suffix auto-flush threshold, the batch-end flush has already
        ordered the commit bytes before any floor append. It now pins that
        upstream invariant under an executed SIGKILL; if irmin-pack 3.12
        drops the batch-end flush, this is the rep that goes red first.
    (c) a clean shutdown (journals closed, store closed, exit 0): every
        floor kept and covered, so the crash assertions are not vacuous.

    Process discipline (the fix for the hang this file first shipped with):
    ALL THREE children are forked, driven, and REAPED in a plain-Unix phase;
    the parent's first [Lwt_main.run] happens only after the last [waitpid].
    A pre-fork [Lwt_main.run] leaves engine state (watcher fds, wakeup
    pipes) a forked child inherits, and the child's own first [Lwt_main.run]
    then parks in [select] on fds nothing will ever fire. This is
    [session_secret_test] S14's "Unix.fork before any Lwt", held for the
    whole image. Each child's stderr is redirected to a per-rep file so a
    child-side refusal is diagnosable from the test output.

    The fork/pipe/waitpid idiom is [session_secret_test]'s S14; the scratch
    root, [open_guards] call shape, and journal teardown follow
    [pack_guards_test]; the commit-water reads follow [guard_water_test]. *)

module App = struct
  open Tea_core

  (** The smallest persistable app, as [guard_water_test] mints it: the
      child commits int models directly and never runs [update], but the
      store functor wants a whole APP. *)
  type model = int

  type msg = Bump

  let model_t = Repr.int

  let msg_t =
    Repr.(
      variant "msg" (fun bump -> function Bump -> bump)
      |~ case0 "Bump" Bump
      |> sealv)

  let init = (0, Cmd.none)
  let update (_ : Crdt.Ctx.t) (Bump : msg) (m : model) = (m + 1, Cmd.none)
  let view (m : model) = Html.text (string_of_int m)
  let subscriptions (_ : model) = Sub.none
  let merge = Merge.(to_spec (atomic ~eq:Int.equal))
  let title = Prim.Title.v "kill-probe"
  let url_of_model (_ : model) = None
  let msg_of_url (_ : Prim.Url.t) = None
end

module _ : Tea_core.App.APP = App
module Store = Tea_persist_pack.Store_pack.Make (App)
module Root = Tea_persist_pack.Store_pack.Root
module Guard_file = Tea_server_pack.Guard_file
module Guard_sink = Tea_server.Guard_sink
module Dguard = Tea_server.Durable_guard
module Floors = Tea_server.Durable_guard.Floors
module Msg_seq = Tea_core.Prim.Msg_seq
module Tab_id = Tea_core.Prim.Tab_id
module Water = Tea_core.Prim.Store_water
module Store_identity = Tea_core.Prim.Store_identity
module Session_id = Tea_core.Prim.Session_id
module Replica = Tea_core.Crdt.Replica

(* --- Harness ---------------------------------------------------------------
   Like [pack_guards_test], this file reports every failure and exits at the
   END: the three repetitions are INDEPENDENT (fenced crash, unfenced crash,
   clean shutdown) and a mutation sweep needs to see which of them a mutant
   breaks; stopping at the first red would hide whether another rep is live
   at all. *)
let failures = ref 0

let check (name : string) (cond : bool) : unit =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    incr failures;
    Printf.printf "FAIL - %s\n%!" name)

(* Immediate: nothing downstream of a broken setup could attribute its own
   failure. PARENT-side only; a child must never [exit]. *)
let die (what : string) : 'a =
  Printf.printf "FAIL - %s\n%!" what;
  exit 1

(* [Option.fold]'s [~none:] is EAGER; both arms are closures applied once. *)
let must (what : string) (o : 'a option) : 'a =
  Option.fold ~none:(fun () -> die what) ~some:(fun (x : 'a) () -> x) o ()

(* Parent-side Lwt boundary, PHASE 2 ONLY (see the header): a surprise on a
   crashed root becomes a named setup FAIL, never a backtrace. *)
let run_lwt (what : string) (f : unit -> 'a Lwt.t) : 'a =
  Lwt_main.run
    (Lwt.catch f (fun (exc : exn) ->
         Lwt.return
           (die (Printf.sprintf "setup: %s (%s)" what (Printexc.to_string exc)))))

(* --- Deterministic fixtures, all minted in the parent BEFORE any fork ------ *)

(* K commits of small int models, N markers read before the kill. The
   payload stays far below the 16_384-byte suffix auto-flush threshold on
   purpose: rep (b)'s coverage then isolates irmin-pack's BATCH-END flush,
   not the size threshold. *)
let k_messages = 6
let n_markers = 3
let journal_cap = 16_384
let session_name = "kill-durability"
let the_sid : Session_id.t =
  must "setup: session id" (Session_id.of_string session_name)

(* The acting REPLICA is never minted raw here: a bare [Replica.v] of the
   session id is NOT the mint the session's ctx persists under (measured:
   they disagree), so every rep re-derives it from [Store.session] on the
   reopened store, exactly as the child derived its own. *)
let the_tab : Tab_id.t =
  must "setup: tab id"
    (Tab_id.of_bytes (List.init 16 (fun (i : int) -> (0x20 + i) land 0xff)))

let pump_steps : (int * Msg_seq.t) list =
  List.init k_messages (fun (i : int) ->
      ( i + 1
      , must (Printf.sprintf "setup: Msg_seq.of_int %d" (i + 1))
          (Msg_seq.of_int (i + 1)) ))

let now : unit -> int64 = fun () -> 1_000L

(* ONE deterministic binding for the child's open and the parent's reopen,
   the [pack_guards_test] shape: the reopen must be [Matched], because a
   rebind would clear floors for a reason this file is not asking about. *)
let store_id : Store_identity.t = Store_identity.of_draws (fun () -> 0x4b)
let identity : Store_identity.binding = Store_identity.Bound store_id

let epoch0 :
    Tea_core.Prim.Store_epoch.binding * Tea_core.Prim.Store_epoch.binding =
  ( Tea_core.Prim.Store_epoch.Bound Tea_core.Prim.Store_epoch.bottom
  , Tea_core.Prim.Store_epoch.Bound Tea_core.Prim.Store_epoch.bottom )

let parent_dir = Filename.temp_dir "ocaml-tea-kill-durability" ""

(* root, guard dir, and the per-rep file the child's stderr is dup2'd to. *)
let rep_dirs (name : string) : string * string * string =
  let d = Filename.concat parent_dir name in
  Unix.mkdir d 0o755;
  ( Filename.concat d "store"
  , Filename.concat d "store.guard"
  , Filename.concat d "child.stderr" )

(* Exhaustive on purpose: "process gone" would also be satisfied by a child
   that raced through the pump and exited before the signal landed, and that
   child proves nothing about dying dirty. *)
let signalled_sigkill (st : Unix.process_status) : bool =
  match st with
  | Unix.WEXITED (_ : int) -> false
  | Unix.WSIGNALED (sg : int) -> Int.equal sg Sys.sigkill
  | Unix.WSTOPPED (_ : int) -> false

let exited_zero (st : Unix.process_status) : bool =
  match st with
  | Unix.WEXITED (n : int) -> Int.equal n 0
  | Unix.WSIGNALED (_ : int) -> false
  | Unix.WSTOPPED (_ : int) -> false

(* A child-side refusal must be diagnosable from the test output: read back
   the per-rep stderr file. *)
let child_stderr (err_path : string) : string =
  if Sys.file_exists err_path then
    In_channel.with_open_bin err_path In_channel.input_all
  else ""

let dump_child_stderr (what : string) (err_path : string) : unit =
  let s = child_stderr err_path in
  if String.length s > 0 then
    Printf.printf "---- child stderr for %s ----\n%s---- end child stderr ----\n%!"
      what s

let die_with_stderr (what : string) (err_path : string) : 'a =
  dump_child_stderr what err_path;
  die what

(* --- The child --------------------------------------------------------------
   Runs strictly AFTER the fork, so every [Lwt_main.run] below is the child's
   own first; ends in [Unix._exit] on every path (never [exit], never a
   check line on the shared stdout; refusals narrate on the REDIRECTED
   stderr first, so the parent can print why). In the killed reps it must
   NOT close the store or the journals: dying dirty is the point. After the
   pump it parks on a blocking read of a pipe the parent never writes, so it
   cannot exit ahead of the signal: no sleep, no poll, just a fd the kill
   tears out from under it. *)
let child_body ~(root : string) ~(guard_dir : string) ~(fenced : bool)
    ~(clean : bool) ~(marker : Unix.file_descr) ~(hold : Unix.file_descr) :
    unit =
  (* Explicitly polymorphic: an implicit ['a] here would be scoped to the
     whole enclosing binding and pinned by the first use. *)
  let run_c : 'a. string -> (unit -> 'a Lwt.t) -> 'a =
    fun what f ->
      Lwt_main.run
        (Lwt.catch f (fun (exc : exn) ->
             Printf.eprintf "child %s: %s\n%!" what (Printexc.to_string exc);
             Unix._exit 1))
  in
  let t = run_c "create" (fun () -> Store.create ~now (Root.v root)) in
  let s = run_c "session" (fun () -> Store.session t the_sid) in
  let replica = Tea_core.Crdt.Ctx.replica (Store.ctx_of_session s) in
  (* One [branch_waters] read between the store open and the guard open: the
     snapshot discipline [open_guards]'s contract states. *)
  let waters = run_c "branch_waters" (fun () -> Store.branch_waters t) in
  let head_water = Guard_file.head_water_of_list waters in
  let fence : (unit -> unit Lwt.t) option =
    if fenced then Some (fun () -> Store.flush t) else None
  in
  let { Tea_server_pack.ws; ws_journal; rpc = (_ : Dguard.t); rpc_journal } =
    Tea_server_pack.open_guards ~guard_dir ~head_water ~identity ~epoch:epoch0
      ?fence ()
  in
  (* A [None] journal is a null sink: the rep would measure nothing, so the
     child refuses loudly (a nonzero status fails the parent's status
     check). *)
  let () =
    if Option.is_some ws_journal then ()
    else (
      Printf.eprintf "child: open_guards returned no websocket journal\n%!";
      Unix._exit 1)
  in
  let persist_one ((i : int), (sq : Msg_seq.t)) : unit =
    let w =
      run_c "commit" (fun () ->
          Store.commit s ~label:(Printf.sprintf "kill-%d" i) i)
    in
    run_c "persist" (fun () ->
        Dguard.persist ws ~replica ~tab:the_tab ~seq:sq ~water:w)
    |> Result.fold
         ~error:(fun (_ : Guard_sink.err) ->
           Printf.eprintf "child: persist %d resolved Error\n%!" i;
           Unix._exit 1)
         ~ok:(fun () ->
           (* The marker: written only AFTER the persist resolved [Ok], by
              statement order inside this arm. *)
           let (_ : int) = Unix.write marker (Bytes.make 1 '.') 0 1 in
           ())
  in
  List.iter persist_one pump_steps;
  if clean then (
    let close_j (j : Guard_file.t option) : unit =
      Option.fold
        ~none:(fun () ->
          Printf.eprintf "child: journal absent at close\n%!";
          Unix._exit 1)
        ~some:(fun (jf : Guard_file.t) () ->
          run_c "journal close" (fun () -> Guard_file.close jf))
        j ()
    in
    close_j ws_journal;
    close_j rpc_journal;
    run_c "store close" (fun () -> Store.close t);
    Unix._exit 0)
  else (
    let (_ : int) = Unix.read hold (Bytes.create 1) 0 1 in
    Unix._exit 1)

(* --- fork/pipe/waitpid, the [session_secret_test] S14 idiom -----------------
   Both pipes exist BEFORE the fork; stdout is flushed so the child inherits
   no buffered check line. The parent closes its copies of the child-held
   ends IMMEDIATELY, so a child that dies early turns the marker read into
   EOF instead of a hang. [marker]: child to parent, one byte per persisted
   floor. [hold]: parent to child, never written; the killed reps' child
   blocks on it until the signal lands. The child dup2s its stderr onto the
   per-rep file first of all. *)
let fork_child ~(err_path : string)
    (run : marker:Unix.file_descr -> hold:Unix.file_descr -> unit) :
    int * Unix.file_descr * Unix.file_descr =
  let m_r, m_w = Unix.pipe () in
  let h_r, h_w = Unix.pipe () in
  flush stdout;
  flush stderr;
  match Unix.fork () with
  | 0 ->
    Unix.close m_r;
    Unix.close h_w;
    let efd =
      Unix.openfile err_path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
    in
    Unix.dup2 efd Unix.stderr;
    Unix.close efd;
    run ~marker:m_w ~hold:h_r;
    (* [run] never returns; a child that somehow did must still _exit. *)
    Unix._exit 1
  | (pid : int) ->
    Unix.close m_w;
    Unix.close h_r;
    (pid, m_r, h_w)

(* Blocking and deterministic: EOF (a child that died early) returns false
   and fails the rep's setup loudly instead of hanging or retrying. *)
let rec read_markers (fd : Unix.file_descr) (n : int) : bool =
  n <= 0
  || (Int.equal (Unix.read fd (Bytes.create 1) 0 1) 1
     && read_markers fd (n - 1))

(* --- Phase 1: fork, pump, kill, reap; plain Unix ONLY -----------------------
   The parent must not touch Lwt before the LAST fork (header: the hang
   fix). All three children are forked, driven, and REAPED here, one at a
   time; every parent-side [Lwt_main.run] lives in phase 2 below. *)

let killed_status ~(what : string) ~(root : string) ~(guard_dir : string)
    ~(err_path : string) ~(fenced : bool) : Unix.process_status =
  let pid, m_r, h_w =
    fork_child ~err_path (child_body ~root ~guard_dir ~fenced ~clean:false)
  in
  let () =
    if read_markers m_r n_markers then ()
    else (
      let ((_ : int), (_ : Unix.process_status)) = Unix.waitpid [] pid in
      die_with_stderr
        (Printf.sprintf
           "setup: %s child died before writing %d markers (marker read saw \
            EOF)"
           what n_markers)
        err_path)
  in
  Unix.kill pid Sys.sigkill;
  let ((_ : int), (st : Unix.process_status)) = Unix.waitpid [] pid in
  Unix.close m_r;
  Unix.close h_w;
  st

let clean_status ~(root : string) ~(guard_dir : string) ~(err_path : string) :
    Unix.process_status =
  let pid, m_r, h_w =
    fork_child ~err_path (child_body ~root ~guard_dir ~fenced:true ~clean:true)
  in
  let ((_ : int), (st : Unix.process_status)) = Unix.waitpid [] pid in
  Unix.close m_r;
  Unix.close h_w;
  st

let root_a, guard_a, err_a = rep_dirs "a-fenced"
let root_b, guard_b, err_b = rep_dirs "b-unfenced"
let root_c, guard_c, err_c = rep_dirs "c-clean"

let st_a =
  killed_status ~what:"(a)" ~root:root_a ~guard_dir:guard_a ~err_path:err_a
    ~fenced:true

let () = if signalled_sigkill st_a then () else dump_child_stderr "(a)" err_a

let st_b =
  killed_status ~what:"(b)" ~root:root_b ~guard_dir:guard_b ~err_path:err_b
    ~fenced:false

let () = if signalled_sigkill st_b then () else dump_child_stderr "(b)" err_b
let st_c = clean_status ~root:root_c ~guard_dir:guard_c ~err_path:err_c
let () = if exited_zero st_c then () else dump_child_stderr "(c)" err_c

(* --- Phase 2: the reopen's assertion passes ---------------------------------
   [open_guards] never returns the verdict, so the reopen drives
   [Guard_file.open_] directly at the SAME dir the child's websocket journal
   lives in (<guard_dir>/journal), against the head snapshot the caller just
   read. Runs only AFTER every [waitpid]: the children are dead and
   irmin-pack's flock is free, and no fork can inherit this phase's Lwt
   engine. The boot filter may narrate dropped floors on stderr; that noise
   is expected and nothing below reads it: the assertions are on the verdict
   counts. *)
let open_err_name (e : Guard_file.open_err) : string =
  match e with
  | Guard_file.Io s -> Printf.sprintf "Io %S" s
  | Guard_file.Bad_dir s -> Printf.sprintf "Bad_dir %S" s

let boot_filter ~(what : string) ~(guard_dir : string)
    ~(head_water : Replica.t -> Water.t option) :
    Guard_file.verdict * Floors.t =
  Lwt_main.run
    (Guard_file.open_ ~dir:guard_dir ~cap:journal_cap ~head_water ~identity
       ~epoch:epoch0)
  |> Result.fold
       ~error:(fun (e : Guard_file.open_err) ->
         die (Printf.sprintf "setup: %s journal reopen (%s)" what (open_err_name e)))
       ~ok:(fun
           ( (_ : Guard_sink.t)
           , (fl : Floors.t)
           , (v : Guard_file.verdict)
           , (_ : Guard_file.identity_outcome)
           , (_ : Guard_file.epoch_outcome)
           , (jf : Guard_file.t) )
         ->
         Lwt_main.run (Guard_file.close jf);
         (v, fl))

let stamped_of ~(replica : Replica.t) (fl : Floors.t) :
    (Msg_seq.t * Water.t) option =
  Floors.find_stamped ~replica ~tab:the_tab fl

(* The D16 direction: the kept floor's water covered by the restored head,
   never a floor standing above the store. *)
let covered ~(replica : Replica.t) (lookup : Replica.t -> Water.t option)
    (fl : Floors.t) : bool =
  stamped_of ~replica fl
  |> Option.fold ~none:false
       ~some:(fun ((_ : Msg_seq.t), (w : Water.t)) ->
         lookup replica
         |> Option.fold ~none:false
              ~some:(fun (h : Water.t) -> Water.compare w h <= 0))

(* Shared reopen plumbing so reps (a) and (b) assert over IDENTICAL reads:
   the point of rep (b) is that the only difference is the child's wiring. *)
let reopened ~(what : string) ~(root : string) ~(guard_dir : string) :
    Guard_file.verdict * Floors.t * (Replica.t -> Water.t option) * Replica.t =
  let t = run_lwt (what ^ " reopen root") (fun () -> Store.create ~now (Root.v root)) in
  (* The acting key, re-derived exactly as the child derived it; then the
     child's own open order: session, then ONE [branch_waters] snapshot. *)
  let s = run_lwt (what ^ " session") (fun () -> Store.session t the_sid) in
  let acting = Tea_core.Crdt.Ctx.replica (Store.ctx_of_session s) in
  let waters = run_lwt (what ^ " branch_waters") (fun () -> Store.branch_waters t) in
  let lookup = Guard_file.head_water_of_list waters in
  let v, fl = boot_filter ~what ~guard_dir ~head_water:lookup in
  run_lwt (what ^ " close") (fun () -> Store.close t);
  (v, fl, lookup, acting)

(* --- (a) the wired fence, then kill -9 -------------------------------------- *)

let () =
  let v, fl, lookup, acting =
    reopened ~what:"(a)" ~root:root_a ~guard_dir:guard_a
  in
  check
    "A1 the fenced child died by SIGKILL exactly (WSIGNALED, so it did not race \
     the pump to a clean exit)"
    (signalled_sigkill st_a);
  check
    "A2 the reopened boot filter KEEPS the killed child's floor (kept >= 1 for \
     the acting key)"
    (v.Guard_file.kept >= 1 && Option.is_some (stamped_of ~replica:acting fl));
  check "A3 nothing behind the restored head (dropped_behind = 0)"
    (Int.equal v.Guard_file.dropped_behind 0);
  check
    "A4 the kept floor's water is covered by the restored head: D16's direction, \
     never a floor standing above the store"
    (covered ~replica:acting lookup fl)

(* --- (b) the UPSTREAM-INVARIANT rep: fence absent, same kill -----------------
   Cut as a loss-demonstrating positive control, and it CANNOT be one
   (header, and discovery: irmin-pack 3.11's batch-end [File_manager.flush]
   has the commit bytes page-cache-durable before any floor append, payload
   size notwithstanding). SAME assertions as (a); the only wiring
   difference is the absent fence. What it pins is the upstream ordering
   under a real SIGKILL, the exact invariant the fence re-states locally. *)

let () =
  let v, fl, lookup, acting =
    reopened ~what:"(b)" ~root:root_b ~guard_dir:guard_b
  in
  check "B1 the unfenced child died by SIGKILL exactly (same harness as rep (a))"
    (signalled_sigkill st_b);
  check
    "B2 even WITHOUT the fence the boot filter keeps the killed child's floor \
     (kept >= 1): irmin-pack's batch-end flush already ordered the commit \
     bytes, the discovered invariant executed under kill -9"
    (v.Guard_file.kept >= 1 && Option.is_some (stamped_of ~replica:acting fl));
  check "B3 nothing behind the restored head unfenced either (dropped_behind = 0)"
    (Int.equal v.Guard_file.dropped_behind 0);
  check
    "B4 the unfenced kept floor's water is covered by the restored head (goes \
     red first if upstream drops the batch-end flush)"
    (covered ~replica:acting lookup fl)

(* --- (c) clean-shutdown baseline --------------------------------------------
   Same K messages, journals and store CLOSED, exit 0, no kill: proves the
   crash assertions above are not vacuously green on an empty ledger. *)

let () =
  check "C1 the clean-shutdown child exited 0 (WEXITED 0 exactly)"
    (exited_zero st_c);
  let t = run_lwt "(c) reopen root" (fun () -> Store.create ~now (Root.v root_c)) in
  let s = run_lwt "(c) session" (fun () -> Store.session t the_sid) in
  (* The same re-derived acting key every rep uses (fixture comment above:
     a raw [Replica.v] mint is NOT this, measured). *)
  let acting = Tea_core.Crdt.Ctx.replica (Store.ctx_of_session s) in
  let waters = run_lwt "(c) branch_waters" (fun () -> Store.branch_waters t) in
  let hw = run_lwt "(c) head_water" (fun () -> Store.head_water s) in
  let v, fl =
    boot_filter ~what:"(c)" ~guard_dir:guard_c
      ~head_water:(Guard_file.head_water_of_list waters)
  in
  check
    "C2 nothing dropped on the clean baseline (kept >= 1, both drop counters 0)"
    (v.Guard_file.kept >= 1
    && Int.equal v.Guard_file.dropped_behind 0
    && Int.equal v.Guard_file.dropped_no_branch 0);
  check
    "C3 the restored floor stands at seq K with water EQUAL to the restored \
     head (W2's commit/head agreement, across a whole life)"
    (stamped_of ~replica:acting fl
    |> Option.fold ~none:false
         ~some:(fun ((sq : Msg_seq.t), (w : Water.t)) ->
           Int.equal (Msg_seq.to_int sq) k_messages && Water.equal w hw));
  run_lwt "(c) close" (fun () -> Store.close t)

(* --- Teardown and the exit --------------------------------------------------- *)

let rec rm_rf (path : string) : unit =
  if Sys.is_directory path then (
    Array.iter
      (fun (entry : string) -> rm_rf (Filename.concat path entry))
      (Sys.readdir path);
    Sys.rmdir path)
  else Sys.remove path

let () =
  rm_rf parent_dir;
  if !failures > 0 then (
    Printf.printf "\n%d check(s) failed.\n%!" !failures;
    exit 1);
  Printf.printf
    "\n\
     A kill -9 mid-pump leaves every journaled floor KEPT and COVERED, fence \
     wired or absent: rep (a) pins serve_pack's wiring, rep (b) pins \
     irmin-pack's batch-end flush (the discovery that bounds this harness's \
     sensitivity; no rep can manufacture the loss), and a clean shutdown \
     keeps everything: D16 adjudicated over an executed SIGKILL, not a \
     double.\n\
     %!"
