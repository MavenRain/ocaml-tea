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
