(** The pack-root preflight (roadmap step 12): an unusable TEA_ROOT must be a
    typed refusal, never an uncaught [Pack_error] killing the process. P1/P2
    pin that the working paths are bit-identical to [create]; P3-P7 drive
    every refusal arm, P7 being the only witness that the [Lwt.catch] seam
    exists (a garbage control file passes the preflight and dies inside
    irmin-pack); P8 pins that a refusal is effect-free. The serve_pack-level
    sibling claim (a refused root gains no [.guard]/[.secret]) is asserted in
    browser scenario B6, where the code actually can mint those - at this
    layer [open_root] never touches them, so a check here would be vacuous. *)

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let module Store = Tea_persist_pack.Store_pack.Make (Counter_app.App) in
     let module Root = Tea_persist_pack.Store_pack.Root in
     let check name cond =
       if cond then Printf.printf "ok   - %s\n%!" name
       else (
         Printf.printf "FAIL - %s\n%!" name;
         exit 1)
     in
     (* Every observation renders to a string, so an unexpected arm names
        itself in the FAIL line instead of dying unattributed. *)
     let show (r : (Store.t, Store.open_error) result) : string =
       Result.fold r
         ~ok:(fun (_ : Store.t) -> "ok")
         ~error:(fun (e : Store.open_error) ->
           match e with
           | Store.Root_parent_missing (_ : string) -> "parent-missing"
           | Store.Root_not_a_directory (_ : string) -> "not-a-directory"
           | Store.Root_not_a_pack_store (_ : string) -> "not-a-pack-store"
           | Store.Backend_failed (_ : string) -> "backend-failed")
     in
     let close_if_open (r : (Store.t, Store.open_error) result) : unit Lwt.t =
       Result.fold r ~ok:Store.close
         ~error:(fun (_ : Store.open_error) -> Lwt.return_unit)
     in
     let open_at (path : string) = Store.open_root (Root.v path) in
     let write_file (path : string) (contents : string) : unit =
       let oc = open_out path in
       output_string oc contents;
       close_out oc
     in

     (* P1: parent exists, leaf missing - today's working create path. *)
     let parent = Filename.temp_dir "ocaml-tea-packroot" "" in
     let fresh = Filename.concat parent "store" in
     let* r1 = open_at fresh in
     check "P1: parent-exists/leaf-missing root opens Ok" (String.equal (show r1) "ok");
     let* () = close_if_open r1 in

     (* P2: reopen the root P1 created (the flock was released by close). *)
     let* r2 = open_at fresh in
     check "P2: reopening a root it created opens Ok" (String.equal (show r2) "ok");
     let* () = close_if_open r2 in

     (* P3: an existing EMPTY directory - the exact shape [mkdtemp] hands out,
        and the exact shape that used to die as an uncaught
        [Pack_error "Invalid_layout"]. *)
     let empty_dir = Filename.temp_dir "ocaml-tea-packroot-empty" "" in
     let* r3 = open_at empty_dir in
     check "P3: existing empty dir -> Root_not_a_pack_store, not a raise"
       (String.equal (show r3) "not-a-pack-store");

     (* P4: an existing directory holding only a foreign file. *)
     let foreign_dir = Filename.temp_dir "ocaml-tea-packroot-foreign" "" in
     write_file (Filename.concat foreign_dir "notes.txt") "not a store\n";
     let* r4 = open_at foreign_dir in
     check "P4: dir with only a foreign file -> Root_not_a_pack_store"
       (String.equal (show r4) "not-a-pack-store");

     (* P5: the root's parent is absent, so the backend's single mkdir would
        ENOENT. *)
     let* r5 =
       open_at (Filename.concat (Filename.concat parent "absent") "store")
     in
     check "P5: missing parent -> Root_parent_missing, not a raise"
       (String.equal (show r5) "parent-missing");

     (* P6: the root path is a regular file. *)
     let file_root = Filename.concat parent "plain-file" in
     write_file file_root "";
     let* r6 = open_at file_root in
     check "P6: regular file -> Root_not_a_directory"
       (String.equal (show r6) "not-a-directory");

     (* P7: a garbage [store.control] PASSES the preflight (the file exists)
        and fails inside irmin-pack - the only case that proves the
        [Lwt.catch] seam is real rather than dead code behind a perfect
        preflight. *)
     let garbage_dir = Filename.temp_dir "ocaml-tea-packroot-garbage" "" in
     write_file
       (Filename.concat garbage_dir "store.control")
       "garbage bytes, not a control file";
     let* r7 = open_at garbage_dir in
     check "P7: garbage store.control -> Backend_failed (the catch seam exists)"
       (String.equal (show r7) "backend-failed");

     (* P8: refusal is effect-free - the refused dir is untouched and the
        missing-parent root was never created. *)
     check "P8: a refused empty dir is still empty (refusal is effect-free)"
       (Array.length (Sys.readdir empty_dir) = 0);
     check "P8: the missing-parent root was never created"
       (not (Sys.file_exists (Filename.concat parent "absent")));
     Lwt.return_unit)
