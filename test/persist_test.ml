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
     check "three increments -> count = 3" (m3.count = 3);
     let* hist = Store.history s in
     check "history has one commit per update (3)" (List.length hist = 3);
     let* undone = Store.undo s in
     let undo_ok =
       match undone with
       | Some m -> m.count = 2
       | None -> false
     in
     check "undo walks to previous commit -> count = 2" undo_ok;
     let* loaded = Store.load s in
     check "reload after undo -> count = 2" (loaded.count = 2);
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
       (delivered && List.map (fun m -> m.count) !seen1 = [ 1 ]);
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
       (fenced && List.map (fun m -> m.count) !seen2 = [ 3 ]);
     check "unwatch stops delivery (w1 recorded exactly one frame)"
       (List.length !seen1 = 1);
     let* () = Store.unwatch w2 in
     Printf.printf "\nStore watches deliver commits, and unwatch fences (step 3).\n%!";
     Lwt.return_unit)
