(** The irmin-pack backend end-to-end (roadmap step 6, R1): baseline store
    behaviour on disk, checkpoint squash, GC behind the retained checkpoint
    (old commits gone, head model intact, stale walks degrade gracefully),
    GC error paths, and durability across close/reopen. *)

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let open Counter_app.App in
     let module Store = Tea_persist_pack.Store_pack.Make (Counter_app.App) in
     let check name cond =
       if cond then Printf.printf "ok   - %s\n%!" name
       else (
         Printf.printf "FAIL - %s\n%!" name;
         exit 1)
     in
     let module Prim = Tea_core.Prim in
     let sid s = Option.get (Prim.Session_id.of_string s) in
     (* Scratch root: [Filename.temp_dir] creates the parent; the store
        itself lives one component below (create makes the final component). *)
     let parent = Filename.temp_dir "ocaml-tea-pack" "" in
     let root = Tea_persist_pack.Store_pack.Root.v (Filename.concat parent "store") in

     (* (a) Baseline: the same T1 behaviour as mem, now on disk. *)
     let* t = Store.create ~now:(fun () -> 42L) root in
     let* main = Store.main_session t in
     let* (_ : model) = Store.apply main Increment in
     let* (_ : model) = Store.apply main Increment in
     let* m3 = Store.apply main Increment in
     check "pack baseline: three increments -> count = 3" (m3.count = 3);
     let* hist = Store.history main in
     check "pack baseline: history has one commit per update (3)" (List.length hist = 3);
     (* Capture the pre-checkpoint head commit key for the post-GC probe. *)
     let* mainb = Store.S.main (Store.repo t) in
     let* head0 = Store.S.Head.find mainb in
     let k0 = Option.map Store.S.Commit.key head0 in
     check "pre-checkpoint head captured" (Option.is_some k0);

     (* (b) Checkpoint semantics on pack. *)
     let* cp_r = Store.checkpoint main ~label:"checkpoint" in
     let cp =
       match cp_r with
       | Ok cp -> Some cp
       | Error (Empty_branch | Branch_moved) -> None
     in
     check "checkpoint succeeds on a non-empty branch" (Option.is_some cp);
     let cp = Option.get cp in
     let* m_after = Store.load main in
     check "checkpoint leaves the model unchanged (count = 3)" (m_after.count = 3);
     let* hist_cp = Store.history main in
     let cp_pinned =
       match hist_cp with
       | [ only ] ->
         String.equal
           (Prim.Commit_ref.to_string only)
           (Prim.Commit_ref.to_string (Store.checkpoint_ref cp))
       | [] | _ :: _ :: _ -> false
     in
     check "history after checkpoint is exactly [the checkpoint]" cp_pinned;
     let* undone = Store.undo main in
     check "undo at the squashed root is None" (Option.is_none undone);
     let* fresh = Store.session t (sid "fresh") in
     let* cp_empty = Store.checkpoint fresh ~label:"nothing" in
     let empty_arm =
       match cp_empty with
       | Error Empty_branch -> true
       | Error Branch_moved -> false
       | Ok (_ : Store.checkpoint) -> false
     in
     check "checkpoint on an empty branch is Error Empty_branch" empty_arm;

     (* (c) GC retaining the checkpoint; a session forked AFTER it stays
        healthy, walks across the boundary degrade instead of crashing. *)
     let* b = Store.fork t ~from:main (sid "bb") in
     let* (_ : model) = Store.apply b Increment in
     let* gc_r = Store.gc t ~retain:cp in
     let gc_ok =
       match gc_r with
       | Ok () -> true
       | Error (Gc_disallowed | Gc_already_running | Gc_failed _) -> false
     in
     check "gc ~retain:checkpoint succeeds (minimal indexing)" gc_ok;
     let* m_main = Store.load main in
     check "main's model is intact after gc (count = 3)" (m_main.count = 3);
     let* hist_main = Store.history main in
     check "main's history is still exactly the checkpoint"
       (List.length hist_main = 1);
     (* The pre-checkpoint head must be unreadable now; record HOW it fails. *)
     let* k0_read =
       Lwt.catch
         (fun () ->
           let* c = Store.S.Commit.of_key (Store.repo t) (Option.get k0) in
           Lwt.return
             (match c with
              | None -> `Gone_none
              | Some (_ : Store.S.commit) -> `Still_readable))
         (fun (_ : exn) -> Lwt.return `Gone_raised)
     in
     (match k0_read with
      | `Gone_none -> Printf.printf "note - of_key on the GCed key returns None\n%!"
      | `Gone_raised -> Printf.printf "note - of_key on the GCed key raises\n%!"
      | `Still_readable -> ());
     check "the pre-checkpoint commit is unreadable after gc"
       (match k0_read with
        | `Gone_none | `Gone_raised -> true
        | `Still_readable -> false);
     let* m_b = Store.load b in
     check "the post-checkpoint fork's model is intact (count = 4)" (m_b.count = 4);
     let* undo_b = Store.undo b in
     let undo_b_ok =
       match undo_b with
       | Some u -> u.count = 3
       | None -> false
     in
     check "undo on the fork lands on the checkpoint model (count = 3)" undo_b_ok;
     let* undo_b2 = Store.undo b in
     check "a second undo degrades to None at the squashed root (no exception)"
       (Option.is_none undo_b2);

     (* (d) GC error path: waiting for an already-finalised GC is fine, but a
        second run retaining the same checkpoint must fail cleanly — the
        finalised suffix no longer contains it. *)
     let* gc_again = Store.gc t ~retain:cp in
     let second_gc_surfaces =
       match gc_again with
       | Ok () -> false
       | Error (Gc_failed (_ : string)) -> true
       | Error Gc_disallowed -> false
       | Error Gc_already_running -> false
     in
     check "an immediate second gc on the same checkpoint fails cleanly"
       second_gc_surfaces;

     (* (e) Durability: close, reopen the same root, model survives — pins
        no-fresh reopen and the Varint contents codec round-trip. *)
     let* () = Store.close t in
     let* t2 = Store.create ~now:(fun () -> 43L) root in
     let* main2 = Store.main_session t2 in
     let* m_re = Store.load main2 in
     check "close/reopen keeps the checkpoint-era model (count = 3)" (m_re.count = 3);
     (* The reopened clock is seeded from the branch heads, so a commit under
        a frozen wall clock BELOW the stored dates still stamps strictly
        above them — restart cannot reopen the dedup window. *)
     let* mainb2 = Store.S.main (Store.repo t2) in
     let* head_cp = Store.S.Head.find mainb2 in
     let date_of c = Store.S.Info.date (Store.S.Commit.info c) in
     let cp_date = Option.fold head_cp ~none:Int64.min_int ~some:date_of in
     let* (_ : model) = Store.apply main2 Increment in
     let* head_new = Store.S.Head.find mainb2 in
     let new_date = Option.fold head_new ~none:Int64.min_int ~some:date_of in
     check "a post-reopen commit (frozen now below history) stamps strictly above it"
       (Int64.compare cp_date Int64.min_int > 0 && Int64.compare new_date cp_date > 0);
     let* () = Store.close t2 in

     (* Cleanup: the store is confined to the scratch dir. *)
     let rec rm_rf (path : string) : unit =
       if Sys.is_directory path then (
         Array.iter (fun entry -> rm_rf (Filename.concat path entry)) (Sys.readdir path);
         Sys.rmdir path)
       else Sys.remove path
     in
     rm_rf parent;
     Printf.printf "\nThe pack backend holds: checkpoint + GC retention on disk (R1).\n%!";
     Lwt.return_unit)
