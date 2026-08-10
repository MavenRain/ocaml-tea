(** The commit fence observed from the filesystem (roadmap step 20, R11).

    This file pins a DISCOVERY made while building the fence: irmin-pack 3.11
    ends every write batch in [File_manager.flush] on the success path
    (irmin-pack/unix/store.ml:423, [batch]'s [on_success]), and irmin's
    [Commit.v] runs inside such a batch — so a commit's suffix/dict bytes are
    ALREADY in the page cache when the commit resolves, before the branch
    head moves and long before any guard floor is appended. The R11 window
    this step set out to close (a floor more durable than its commit) was
    already closed upstream, by behaviour no irmin-pack interface documents
    or promises. [Store_pack.flush] therefore meets a clean buffer on the
    pump path; its value is making the ordering a LOCAL invariant of
    [Durable_guard.persist]'s own call order, so an upstream 3.12 that drops
    the batch-end flush cannot silently reopen R11 — F1 here is the alarm
    that would catch that drop.

    Every claim is [Unix.stat] arithmetic over the SUM of the root's
    [store.*] file sizes (robust to irmin-pack's split suffix naming): no
    timing, no polling, no wall-clock.

    - F1 pins the upstream invariant: one sub-16_384-byte commit and the
      on-disk total has ALREADY grown by commit-resolve time.
    - F2 certifies the payload stayed under the hardcoded suffix
      [auto_flush_threshold], so the batch-end flush, not the overflow
      trigger, is what moved the bytes.
    - F3/F4 call [flush] after the resolved commit, twice: byte-identical
      totals, the empty-buffer short-circuit — the fence is a no-op on the
      pump path, which is exactly why it is affordable on every persist.
    - N1 is the stat control: two reads with no intervening operation are
      byte-identical, so F1/F3's arithmetic measures the store, not the
      stat. *)

module App = struct
  open Tea_core

  (** The smallest persistable app: an int model, one message. These checks
      never run [update]; they commit models directly, but the store
      functor wants a whole APP. *)
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
  let title = Prim.Title.v "flush-probe"
  let url_of_model (_ : model) = None
  let msg_of_url (_ : Prim.Url.t) = None
end

module _ : Tea_core.App.APP = App
module Store = Tea_persist_pack.Store_pack.Make (App)
module Root = Tea_persist_pack.Store_pack.Root
module Water = Tea_core.Prim.Store_water
open Lwt.Syntax

(* --- Harness ---------------------------------------------------------------
   Filesystem work is spelled entirely in [Lwt_unix] with ONE [Lwt.catch]
   boundary ([fs]) where any failure becomes a loud setup FAIL: errors stay
   values, and no helper below can raise into a check. *)

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

let fs (what : string) (f : unit -> 'a Lwt.t) : 'a Lwt.t =
  Lwt.catch f (fun (exc : exn) ->
      Printf.printf "FAIL - test setup: %s (%s)\n%!" what
        (Printexc.to_string exc);
      exit 1)

(* [Option.fold]'s [~none:] is EAGER; both branches are closures so the
   refusal cannot run on the happy path. *)
let must (what : string) (o : 'a option) : 'a =
  Option.fold
    ~none:(fun () ->
      Printf.printf "FAIL - test setup: %s\n%!" what;
      exit 1)
    ~some:(fun x () -> x)
    o ()

let sid (name : string) : Tea_core.Prim.Session_id.t =
  must "session id" (Tea_core.Prim.Session_id.of_string name)

let entries_of (path : string) : string list Lwt.t =
  fs (Printf.sprintf "list %s" path) (fun () ->
      Lwt_stream.to_list (Lwt_unix.files_of_directory path)
      |> Lwt.map
           (List.filter (fun (e : string) ->
                not (String.equal e "." || String.equal e ".."))))

let mkdir (path : string) : unit Lwt.t =
  fs (Printf.sprintf "mkdir %s" path) (fun () -> Lwt_unix.mkdir path 0o755)

let rec rm_rf (path : string) : unit Lwt.t =
  fs (Printf.sprintf "remove %s" path) (fun () ->
      let* st = Lwt_unix.stat path in
      match st.Lwt_unix.st_kind with
      | Unix.S_DIR ->
        let* entries = entries_of path in
        let* () =
          Lwt_list.iter_s
            (fun (entry : string) -> rm_rf (Filename.concat path entry))
            entries
        in
        Lwt_unix.rmdir path
      | Unix.S_REG | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO
      | Unix.S_SOCK ->
        Lwt_unix.unlink path)

(* A fresh scratch parent per test section: wall-clock micros + a counter,
   unique across a crashed prior run's leftovers without any raising
   temp-dir helper. *)
let scratch_counter : int ref = ref 0

let fresh_scratch () : string =
  let n = !scratch_counter in
  scratch_counter := n + 1;
  Filename.concat
    (Filename.get_temp_dir_name ())
    (Printf.sprintf "ocaml-tea-flush-%.0f-%d" (Unix.gettimeofday () *. 1e6) n)

let in_scratch_pack (f : parent:string -> root:string -> unit Lwt.t) : unit =
  Lwt_main.run
    (let parent = fresh_scratch () in
     let* () = mkdir parent in
     let* () = f ~parent ~root:(Filename.concat parent "store") in
     rm_rf parent)

(** The size of a regular file, 0 for anything else: directory sizes are
    filesystem noise no fence claim should ride on. *)
let regular_size (path : string) : int Lwt.t =
  fs (Printf.sprintf "stat %s" path) (fun () ->
      let* st = Lwt_unix.stat path in
      match st.Lwt_unix.st_kind with
      | Unix.S_REG -> Lwt.return st.Lwt_unix.st_size
      | Unix.S_DIR | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO
      | Unix.S_SOCK ->
        Lwt.return 0)

(** The on-disk byte total of the root's [store.*] files, re-listed on every
    call: summing rather than naming one layout file is what survives
    irmin-pack's split suffix naming ([store.0.suffix], [store.dict], ...). *)
let store_bytes (root : string) : int Lwt.t =
  let* entries = entries_of root in
  Lwt_list.fold_left_s
    (fun (total : int) (entry : string) ->
      if String.starts_with ~prefix:"store" entry then
        Lwt.map
          (fun (n : int) -> total + n)
          (regular_size (Filename.concat root entry))
      else Lwt.return total)
    0 entries

let now : unit -> int64 = fun () -> 1_000L

(* --- F1-F4: the pump path meets an already-flushed store ------------------- *)

let () =
  in_scratch_pack (fun ~parent:(_ : string) ~root ->
      let* t = Store.create ~now (Root.v root) in
      let* base = store_bytes root in
      let* s = Store.session t (sid "flush-fence") in
      let* (_ : Water.t) = Store.commit s ~label:"f-one" 1 in
      let* committed = store_bytes root in
      check
        "F1 the commit's bytes are ALREADY on disk when commit resolves \
         (irmin-pack's batch-end flush): the upstream invariant the fence \
         pins locally"
        (base < committed);
      check
        "F2 the committed delta stays under the 16_384-byte auto-flush \
         threshold, so the batch-end flush, not the overflow trigger, moved \
         the bytes"
        (committed - base < 16_384);
      let* () = Store.flush t in
      let* after = store_bytes root in
      check
        "F3 flush after a resolved commit moves nothing: the empty-buffer \
         short-circuit that makes the fence affordable on every persist"
        (Int.equal committed after);
      let* () = Store.flush t in
      let* second = store_bytes root in
      check "F4 a second flush is the same no-op: idempotent, not cheap once"
        (Int.equal after second);
      Store.close t)

(* --- N1: stat control ------------------------------------------------------ *)

let () =
  in_scratch_pack (fun ~parent:(_ : string) ~root ->
      let* t = Store.create ~now (Root.v root) in
      let* s = Store.session t (sid "flush-none") in
      let* (_ : Water.t) = Store.commit s ~label:"n-one" 1 in
      let* before = store_bytes root in
      let* after = store_bytes root in
      check
        "N1 stat control: two reads with no intervening operation are \
         byte-identical, so the F-checks measure the store, not the stat"
        (Int.equal before after);
      Store.close t)

(* --- F5: the fence's failure mode is a REJECTION, not a swallow ------------ *)

let () =
  in_scratch_pack (fun ~parent:(_ : string) ~root ->
      let* t = Store.create ~now (Root.v root) in
      let* s = Store.session t (sid "flush-closed") in
      let* (_ : Water.t) = Store.commit s ~label:"c-one" 1 in
      let* () = Store.close t in
      let* rejected =
        Lwt.catch
          (fun () -> Lwt.map (fun () -> false) (Store.flush t))
          (fun (_ : exn) -> Lwt.return true)
      in
      check
        "F5 flush on a closed repo REJECTS: the fence fails by rejection, \
         which persist degrades to Error with no floor appended - a \
         re-introduced swallow inside flush turns this check red"
        rejected;
      Lwt.return_unit)

let () =
  Printf.printf
    "\nThe fence meets a clean buffer: commit bytes land at commit-resolve \
     (upstream batch-end flush, pinned by F1), and flush is the idempotent \
     no-op that makes fencing every persist affordable (F3-F4, N1) whose \
     failure mode is a rejection (F5), R11.\n%!"
