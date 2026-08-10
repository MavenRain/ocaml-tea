(** The hot-copy window, re-cut to the discovered invariant (step 20, R20).

    DISCOVERY (differential probe + source): irmin-pack 3.11 ends every
    write batch in [File_manager.flush] on the success path —
    irmin-pack/unix/store.ml, [batch]'s [on_success] calls
    [File_manager.flush t.fm] (lines 404-443) — and [Commit.v] runs inside
    such a batch. A commit's suffix/dict bytes are therefore already in the
    page cache when the commit resolves, BEFORE [Head.test_and_set] moves
    the branch head. The original "unflushed hot copy drops the floor" case
    is UNREACHABLE through this repo's public API: no sequence of public
    calls leaves a resolved commit's bytes behind its own branch head.

    What the file pins instead. H1': the byte-for-byte copy of a LIVE,
    UNCLOSED root taken after one commit resolves CARRIES that commit — the
    copy opens and its head water covers the commit's own water. H2':
    [Floors.filter] over the copy's heads KEEPS the floor a pump would have
    persisted beside it. H3': a companion rep with an explicit [Store.flush]
    before the copy lands the same outcome, and the copy's store.* byte
    total equals a PRE-flush measurement of the live root — the flush had
    nothing left to move. H4': the drop direction stays exercised against a
    real copied store: a floor minted strictly ABOVE the copy's head (a
    later commit on the live root, the same way every water here is minted)
    is dropped with dropped_behind = 1.

    H1' is one of the two alarms (with store_pack_flush_test's F1) that
    fire if a future irmin-pack drops the batch-end flush: the fence in
    [Durable_guard.persist] would then be the only thing standing between a
    branch head and its commit bytes.

    Every water here is a real commit return — nothing hand-picks a water —
    and the filter is driven directly through [Floors.filter] over the
    COPY's [branch_waters], the exact head function boot builds. Pure
    filesystem copy plus arithmetic on verdict counts: no timing, no fork. *)

module App = struct
  open Tea_core

  (** The smallest persistable app: an int model, one message. Nothing here
      runs [update] — the reps commit models directly — but the store
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
  let title = Prim.Title.v "hot-copy-probe"
  let url_of_model (_ : model) = None
  let msg_of_url (_ : Prim.Url.t) = None
end

module _ : Tea_core.App.APP = App
module Store = Tea_persist_pack.Store_pack.Make (App)
module Root = Tea_persist_pack.Store_pack.Root
module Guard_file = Tea_server_pack.Guard_file
module Guard_sink = Tea_server.Guard_sink
module Floors = Tea_server.Durable_guard.Floors
module Msg_seq = Tea_core.Prim.Msg_seq
module Tab_id = Tea_core.Prim.Tab_id
module Water = Tea_core.Prim.Store_water
module Replica = Tea_core.Crdt.Replica
open Lwt.Syntax

(* --- Harness ---------------------------------------------------------------
   Filesystem work is spelled in [Lwt_unix]/[Lwt_io] for scratch management
   with ONE [Lwt.catch] boundary ([fs]) where any failure becomes a loud
   setup FAIL, plus the store_identity_test sync copy helpers ([attempt])
   for the hot copy itself: errors stay values, and no helper below can
   raise into a check. *)

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

let attempt (f : unit -> 'a) : 'a option =
  try Some (f ()) with (_ : exn) -> None

let sid (name : string) : Tea_core.Prim.Session_id.t =
  must "session id" (Tea_core.Prim.Session_id.of_string name)

let tab (n : int) : Tab_id.t =
  must "tab id mint refused a valid seed"
    (Tab_id.of_bytes (List.init 16 (fun i -> (n + i) land 0xff)))

let seq (n : int) : Msg_seq.t =
  must (Printf.sprintf "Msg_seq.of_int refused %d" n) (Msg_seq.of_int n)

(** The floor a pump would persist for the commit under test: an [Advance]
    carrying the commit's OWN water, keyed by the session's replica. *)
let adv (r : Replica.t) (t : Tab_id.t) (n : int) (w : Water.t) :
    Guard_sink.event =
  Guard_sink.Advance { replica = r; tab = t; seq = seq n; water = w }

(* --- The hot copy (store_identity_test's byte-for-byte recursive copy) ---- *)

let read_bytes (path : string) : string option =
  attempt (fun () -> In_channel.with_open_bin path In_channel.input_all)

let write_bytes (path : string) (contents : string) : bool =
  attempt (fun () ->
      Out_channel.with_open_bin path (fun (oc : Out_channel.t) ->
          Out_channel.output_string oc contents))
  |> Option.is_some

let entries_of (path : string) : string list option =
  attempt (fun () -> Sys.readdir path)
  |> Option.map (fun (names : string array) ->
         List.sort String.compare (Array.to_list names))

(** Byte-for-byte recursive copy: the [cp -r] that creates the hot copy in
    the first place, run in-process so the check owns its own failure rather
    than a shell's exit code. Deliberately SYNCHRONOUS and run while the
    source store is still open — the situation under test. *)
let rec copy_dir (src : string) (dst : string) : bool =
  Option.is_some (attempt (fun () -> Unix.mkdir dst 0o755))
  && Option.fold (entries_of src) ~none:false ~some:(fun (entries : string list) ->
         List.for_all
           (fun (entry : string) ->
             let s = Filename.concat src entry and d = Filename.concat dst entry in
             attempt (fun () -> Unix.lstat s)
             |> Option.fold ~none:false ~some:(fun (st : Unix.stats) ->
                    match st.Unix.st_kind with
                    | Unix.S_DIR -> copy_dir s d
                    | Unix.S_REG ->
                      Option.fold (read_bytes s) ~none:false ~some:(write_bytes d)
                    | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO | Unix.S_SOCK ->
                      false))
           entries)

let copied (what : string) (ok : bool) : unit =
  if ok then ()
  else (
    Printf.printf "FAIL - test setup: copy %s\n%!" what;
    exit 1)

(** Total on-disk bytes of a root's [store.*] files: the pre/post-flush
    measurement for H3'. Sizes come from [Unix.stat] under [attempt], so a
    missing file is a loud [None], never a raise. *)
let store_bytes (dir : string) : int option =
  entries_of dir
  |> Option.map (List.filter (String.starts_with ~prefix:"store."))
  |> Option.fold ~none:None
       ~some:
         (List.fold_left
            (fun (acc : int option) (entry : string) ->
              Option.fold acc ~none:None ~some:(fun (total : int) ->
                  attempt (fun () ->
                      (Unix.stat (Filename.concat dir entry)).Unix.st_size)
                  |> Option.map (( + ) total)))
            (Some 0))

(* --- Scratch ---------------------------------------------------------------- *)

let dir_entries (path : string) : string list Lwt.t =
  fs (Printf.sprintf "list %s" path) (fun () ->
      Lwt_stream.to_list (Lwt_unix.files_of_directory path)
      |> Lwt.map
           (List.filter (fun (e : string) ->
                not (String.equal e "." || String.equal e ".."))))

let rec rm_rf (path : string) : unit Lwt.t =
  fs (Printf.sprintf "remove %s" path) (fun () ->
      let* st = Lwt_unix.stat path in
      match st.Lwt_unix.st_kind with
      | Unix.S_DIR ->
        let* entries = dir_entries path in
        let* () =
          Lwt_list.iter_s
            (fun (entry : string) -> rm_rf (Filename.concat path entry))
            entries
        in
        Lwt_unix.rmdir path
      | Unix.S_REG | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO
      | Unix.S_SOCK ->
        Lwt_unix.unlink path)

(** A fresh mkdtemp parent per rep: root and copy both live under it, and
    the whole tree is removed at the end. [Filename.temp_dir] raises, so it
    runs under [attempt] and stays a value. *)
let fresh_parent () : string =
  must "mkdtemp parent"
    (attempt (fun () -> Filename.temp_dir "ocaml-tea-hot-copy" ""))

let in_scratch_pack (f : parent:string -> root:string -> unit Lwt.t) : unit =
  Lwt_main.run
    (let parent = fresh_parent () in
     let* () = f ~parent ~root:(Filename.concat parent "store") in
     rm_rf parent)

let floor_at (fl : Floors.t) (r : Replica.t) (t : Tab_id.t) ~(is : int) : bool =
  Floors.find ~replica:r ~tab:t fl
  |> Option.fold ~none:false ~some:(fun n -> Int.equal (Msg_seq.to_int n) is)

let now : unit -> int64 = fun () -> 1_000L

(* --- H1'-H2': the live copy carries its commit -------------------------------
   Commit once, clone the LIVE root aside with no flush and no close, open
   the clone: the upstream batch-end flush means the commit's bytes were on
   disk before its branch head moved, so the copy's head COVERS the
   commit's water and the floor the pump would have persisted beside it is
   KEPT — kept = 1 with zero drops is the load-bearing count. If either
   check ever fails, upstream dropped the batch-end flush (the alarm shared
   with store_pack_flush_test F1). *)

let () =
  in_scratch_pack (fun ~parent ~root ->
      let copy = Filename.concat parent "copy" in
      let* t = Store.create ~now (Root.v root) in
      let* s = Store.session t (sid "hot-live") in
      let* w = Store.commit s ~label:"hot-1" 1 in
      let rep = Tea_core.Crdt.Ctx.replica (Store.ctx_of_session s) in
      let fl = Floors.of_events [ adv rep (tab 1) 1 w ] in
      (* The hot copy itself: no flush, no close, the byte-for-byte clone
         of the live root. *)
      copied "the live unclosed root" (copy_dir root copy);
      let* t2 = Store.create ~now (Root.v copy) in
      let* waters = Store.branch_waters t2 in
      let head = Guard_file.head_water_of_list waters in
      check
        "H1' the hot copy of a live unclosed root carries the commit: the \
         copy opens and its head water covers the commit's own water"
        (head rep
        |> Option.fold ~none:false ~some:(fun (hw : Water.t) ->
               Water.compare hw w >= 0));
      let (fl' : Floors.t), (v : Floors.verdict) = Floors.filter ~head fl in
      check
        "H2' the filter keeps the floor at exactly the commit's water: \
         kept = 1, dropped_behind = 0, dropped_no_branch = 0, seq intact"
        (Int.equal v.Floors.kept 1
        && Int.equal v.dropped_behind 0
        && Int.equal v.dropped_no_branch 0
        && Int.equal v.unwitnessed 0
        && floor_at fl' rep (tab 1) ~is:1);
      let* () = Store.close t2 in
      Store.close t)

(* --- H3'-H4': an explicit flush moves nothing; the drop direction stays ------
   The SAME recipe with one [Store.flush] between the commit and the copy:
   the outcome is unchanged, and the copy's store.* byte total equals a
   measurement of the live root taken BEFORE the flush — the batch-end
   flush left the explicit one nothing to move. The drop direction is then
   exercised against the same real copied store: a floor at a LATER
   commit's water (minted on the live root after the copy, a real commit
   return like every other water here) stands strictly above the copy's
   head and must fall as dropped_behind. *)

let () =
  in_scratch_pack (fun ~parent ~root ->
      let copy = Filename.concat parent "copy" in
      let* t = Store.create ~now (Root.v root) in
      let* s = Store.session t (sid "hot-flushed") in
      let* w = Store.commit s ~label:"hot-2" 2 in
      let rep = Tea_core.Crdt.Ctx.replica (Store.ctx_of_session s) in
      let fl = Floors.of_events [ adv rep (tab 1) 1 w ] in
      let pre = must "measure the live root before flush" (store_bytes root) in
      let* () = Store.flush t in
      copied "the flushed root" (copy_dir root copy);
      let post = must "measure the copied root" (store_bytes copy) in
      (* The later water for H4', minted before any second store opens so
         the two-instance window stays read-only. The copy's head cannot
         see it: the clone predates the commit. *)
      let* w2 = Store.commit s ~label:"hot-3" 3 in
      let* t2 = Store.create ~now (Root.v copy) in
      let* waters = Store.branch_waters t2 in
      let head = Guard_file.head_water_of_list waters in
      let (fl' : Floors.t), (v : Floors.verdict) = Floors.filter ~head fl in
      check
        "H3' flushed companion, same outcome: the copy's head covers the \
         commit's water and the filter keeps the floor (kept = 1, zero \
         drops)"
        ((head rep
         |> Option.fold ~none:false ~some:(fun (hw : Water.t) ->
                Water.compare hw w >= 0))
        && Int.equal v.Floors.kept 1
        && Int.equal v.dropped_behind 0
        && Int.equal v.dropped_no_branch 0
        && Int.equal v.unwitnessed 0
        && floor_at fl' rep (tab 1) ~is:1);
      check
        "H3' the flush had nothing to move: the copied root's store.* byte \
         total equals the live root's pre-flush total"
        (Int.equal pre post);
      let fl2 = Floors.of_events [ adv rep (tab 2) 2 w2 ] in
      let (fl2' : Floors.t), (v2 : Floors.verdict) = Floors.filter ~head fl2 in
      check
        "H4' a floor strictly above the copy's head is dropped: \
         dropped_behind = 1, kept = 0, and the key is gone"
        ((head rep
         |> Option.fold ~none:false ~some:(fun (hw : Water.t) ->
                Water.compare w2 hw > 0))
        && Int.equal v2.Floors.kept 0
        && Int.equal v2.dropped_behind 1
        && Int.equal v2.dropped_no_branch 0
        && Int.equal v2.unwitnessed 0
        && Option.is_none (Floors.find ~replica:rep ~tab:(tab 2) fl2'));
      let* () = Store.close t2 in
      Store.close t)

let () =
  Printf.printf
    "\nThe hot copy of a live root carries its commit and the boot filter \
     keeps its floor (H1'-H2'); an explicit flush changes nothing, byte \
     for byte (H3'); only a floor above the copy's head is dropped, and it \
     falls behind (H4').\n%!"
