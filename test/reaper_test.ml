(** The D3 session reaper and crash-safe redo on the pack backend (roadmap
    step 8).

    Reaper: a stale session branch (head date older than [now - ttl]) is swept,
    a fresh one is KEPT, and the reserved refs ([main], the [__checkpoints]
    spine, every [redo-] pointer) are never swept. The kept/swept pair is the
    anti-vacuity: a reaper hardwired to reap everything fails "live branch
    KEPT", a no-op reaper fails "stale branch swept".

    Redo: [undo] durably records the pre-undo head on the session's [redo-]
    pointer (write-ref-first), and the pointer plus [redo] survive a
    close/reopen — the crash-safety observable that a durable, ordered redo
    pointer provides. *)

module Store = Tea_persist_pack.Store_pack.Make (Counter_app.App)
module Prim = Tea_core.Prim

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let open Counter_app.App in
     let check name cond =
       if cond then Printf.printf "ok   - %s\n%!" name
       else (
         Printf.printf "FAIL - %s\n%!" name;
         exit 1)
     in
     let sid s = Option.get (Prim.Session_id.of_string s) in
     let ttl = Option.get (Prim.Ttl.of_seconds 1000.) in

     let parent = Filename.temp_dir "ocaml-tea-reaper" "" in
     let root = Tea_persist_pack.Store_pack.Root.v (Filename.concat parent "store") in

     (* A frozen, movable wall source: commit dates are Clock stamps seeded by
        it, so advancing it opens a measurable gap between an "old" branch and
        a "live" one. *)
     let now_r = ref 1000L in
     let* t = Store.create ~now:(fun () -> !now_r) root in

     (* main and a stale session both commit while the clock reads ~1000. *)
     let* main = Store.main_session t in
     let* (_ : model) = Store.apply main Increment in
     let* stale = Store.session t (sid "staleaa") in
     let* (_ : model) = Store.apply stale Increment in

     (* Advance the clock far ahead; a live session commits at ~9000. *)
     now_r := 9000L;
     let* live = Store.session t (sid "livebb") in
     let* (_ : model) = Store.apply live Increment in

     let branch_of (s : string) = Prim.Branch_name.(to_string (of_session (sid s))) in
     let* pre = Store.S.Branch.list (Store.repo t) in
     check "before reap: both session branches exist"
       (List.mem (branch_of "staleaa") pre && List.mem (branch_of "livebb") pre);

     (* cutoff = 9000 - 1000 = 8000: stale (1001) < cutoff, live (9000) >= it. *)
     let* reaped = Store.reap t ~ttl ~now:9000L in
     check "reap removes exactly the one stale branch" (reaped = 1);
     let* post = Store.S.Branch.list (Store.repo t) in
     check "the stale branch was swept" (not (List.mem (branch_of "staleaa") post));
     check "the LIVE branch is KEPT" (List.mem (branch_of "livebb") post);
     check "main is reserved and kept" (List.mem "main" post);
     (* The live branch's model is intact after the sweep. *)
     let* live_m = Store.load live in
     check "the kept live branch still loads its model (count = 1)" (live_m.count = 1);

     (* --- Crash-safe redo (write-ref-first) ------------------------------- *)
     (* Build a little history on the live branch, then undo. *)
     let* (_ : model) = Store.apply live Increment in
     let* (_ : model) = Store.apply live Increment in
     let* before_undo = Store.load live in
     check "live branch is at count = 3 before undo" (before_undo.count = 3);
     let* undone = Store.undo live in
     let undo_ok =
       match undone with
       | Some m -> m.count = 2
       | None -> false
     in
     check "undo walks back one commit (count = 2)" undo_ok;

     (* The pre-undo head is durably recorded on the redo pointer. *)
     let redo_ref = "redo-" ^ branch_of "livebb" in
     let* has_redo = Store.S.Branch.mem (Store.repo t) redo_ref in
     check "undo writes a durable redo- pointer" has_redo;
     let* redo_head = Store.S.Branch.find (Store.repo t) redo_ref in
     let* redo_model =
       Option.fold redo_head
         ~none:(Lwt.return (fst init))
         ~some:(fun c -> Store.model_at c)
     in
     check "the redo pointer targets the pre-undo head (count = 3)" (redo_model.count = 3);
     (* The reaper must never sweep a redo- pointer, even though it is a branch. *)
     let* reaped2 = Store.reap t ~ttl ~now:9000L in
     let* after = Store.S.Branch.list (Store.repo t) in
     check "the reaper leaves the reserved redo- pointer alone"
       (reaped2 = 0 && List.mem redo_ref after);

     (* Crash/restart: close and reopen. The durable redo pointer survives, and
        redo restores the pre-undo head, then clears the pointer. *)
     let* () = Store.close t in
     let* t2 = Store.create ~now:(fun () -> 9000L) root in
     let* live2 = Store.session t2 (sid "livebb") in
     let* survived = Store.S.Branch.mem (Store.repo t2) redo_ref in
     check "the redo pointer survives a close/reopen (durable)" survived;
     let* redone = Store.redo live2 in
     let redo_ok =
       match redone with
       | Some m -> m.count = 3
       | None -> false
     in
     check "redo after restart restores the pre-undo head (count = 3)" redo_ok;
     let* cleared = Store.S.Branch.mem (Store.repo t2) redo_ref in
     check "redo clears its single-slot pointer" (not cleared);
     let* () = Store.close t2 in

     let rec rm_rf (path : string) : unit =
       if Sys.is_directory path then (
         Array.iter (fun entry -> rm_rf (Filename.concat path entry)) (Sys.readdir path);
         Sys.rmdir path)
       else Sys.remove path
     in
     rm_rf parent;
     Printf.printf "\nThe reaper sweeps stale sessions and spares live+reserved refs; redo is crash-safe (D3).\n%!";
     Lwt.return_unit)
