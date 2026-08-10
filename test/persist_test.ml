(** End-to-end proof of the versioned model store (thesis T1): drive the Counter
    through the Irmin-backed store and check that every step is a commit, that
    history reads back, and that undo walks the commit chain. *)

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let open Counter_app.App in
     let module Store = Tea_persist.Store.Make (Counter_app.App) in
     let check name cond =
       if cond then Printf.printf "ok   - %s\n%!" name
       else (
         Printf.printf "FAIL - %s\n%!" name;
         exit 1)
     in
     let* repo = Store.create () in
     let* s = Store.main_session repo in
     let* _ = Store.apply s Increment in
     let* _ = Store.apply s Increment in
     let* m3 = Store.apply s Increment in
     check "three increments -> count = 3" (value m3 = 3);
     let* hist = Store.history s in
     check "history has one commit per update (3)" (List.length hist = 3);
     let* wu = Store.load_based s in
     let* undone = Store.undo wu in
     let undo_ok =
       Result.fold undone
         ~ok:(fun m -> value m = 2)
         ~error:(fun (_ : Store.undo_error) -> false)
     in
     check "undo walks to previous commit -> count = 2" undo_ok;
     let* loaded = Store.load s in
     check "reload after undo -> count = 2" (value loaded = 2);
     Printf.printf "\nAll persistence invariants hold (T1 proven end-to-end).\n%!";
     Lwt.return_unit)

(** The live-view half of the store (roadmap step 3): a watch delivers the
    committed model after every head change, and unwatch really deregisters —
    proven with an ordering fence (a later watch's delivery) rather than a
    bare sleep. *)
let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let open Counter_app.App in
     let module Store = Tea_persist.Store.Make (Counter_app.App) in
     let check name cond =
       if cond then Printf.printf "ok   - %s\n%!" name
       else (
         Printf.printf "FAIL - %s\n%!" name;
         exit 1)
     in
     let await = Test_util.await in
     let* repo = Store.create () in
     let* s = Store.main_session repo in
     let seen1 = ref [] in
     let* w1 =
       Store.watch s (fun m ->
           seen1 := m :: !seen1;
           Lwt.return_unit)
     in
     let* (_ : model) = Store.apply s Increment in
     let* delivered = await (fun () -> !seen1 <> []) in
     check "watch delivers the committed model (count = 1)"
       (delivered && List.map value !seen1 = [ 1 ]);
     (* Fence: unwatch w1, commit, register w2, commit again. w2's delivery
        proves the notification round for the later commit completed, so a
        still-live w1 would have recorded the second commit by now. *)
     let* () = Store.unwatch w1 in
     let* (_ : model) = Store.apply s Increment in
     let seen2 = ref [] in
     let* w2 =
       Store.watch s (fun m ->
           seen2 := m :: !seen2;
           Lwt.return_unit)
     in
     let* (_ : model) = Store.apply s Increment in
     let* fenced = await (fun () -> !seen2 <> []) in
     check "a fresh watch anchors at registration (sees only count = 3)"
       (fenced && List.map value !seen2 = [ 3 ]);
     check "unwatch stops delivery (w1 recorded exactly one frame)"
       (List.length !seen1 = 1);
     let* () = Store.unwatch w2 in
     Printf.printf "\nStore watches deliver commits, and unwatch fences (step 3).\n%!";
     Lwt.return_unit)

(** Checkpoint squash on the mem backend (roadmap step 6): the squashed root
    carries the model, history collapses to one commit, and undo bottoms
    out — the same semantics [pack_test] pins on disk, pinned cheaply here. *)
let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let open Counter_app.App in
     let module Store = Tea_persist.Store.Make (Counter_app.App) in
     let check name cond =
       if cond then Printf.printf "ok   - %s\n%!" name
       else (
         Printf.printf "FAIL - %s\n%!" name;
         exit 1)
     in
     let* repo = Store.create () in
     let* s = Store.main_session repo in
     let* (_ : model) = Store.apply s Increment in
     let* (_ : model) = Store.apply s Increment in
     let* cp_r = Store.checkpoint s ~label:"checkpoint" in
     let cp_ok =
       match cp_r with
       | Ok (_ : Store.checkpoint) -> true
       | Error (Empty_branch | Branch_moved) -> false
     in
     check "checkpoint on mem succeeds" cp_ok;
     let* hist = Store.history s in
     check "history after checkpoint is exactly one commit" (List.length hist = 1);
     let* m = Store.load s in
     check "the model survives the squash (count = 2)" (value m = 2);
     let* wu2 = Store.load_based s in
     let* u = Store.undo wu2 in
     check "undo at the squashed root refuses with At_root"
       (Result.fold u
          ~ok:(fun _m -> false)
          ~error:(fun (e : Store.undo_error) ->
            match e with
            | Store.At_root -> true
            | Store.Branch_moved -> false));
     let sid = Option.get (Tea_core.Prim.Session_id.of_string "cafe") in
     let* empty_s = Store.session repo sid in
     let* cp_e = Store.checkpoint empty_s ~label:"nothing" in
     let empty_arm =
       match cp_e with
       | Error Empty_branch -> true
       | Error Branch_moved -> false
       | Ok (_ : Store.checkpoint) -> false
     in
     check "checkpoint on an empty branch is Error Empty_branch" empty_arm;
     Printf.printf "\nCheckpoint squash holds on the mem backend (step 6).\n%!";
     Lwt.return_unit)
