(** The D2 pack-backed Dream tier end to end (roadmap step 8): drive
    [Tea_server_pack.Make_pack]'s handler with [Dream.test] — no socket.

    Checks the Origin-gated [POST /admin/checkpoint] (cross-origin 403 leaves
    the store untouched; same-origin 200 checkpoints), and that a checkpoint
    driven through the pack server SURVIVES a close/reopen — the durability the
    [serve_pack] teardown close guarantees, and the anti-vacuity target for
    "remove Store.close at teardown". *)

module Server = Tea_server_pack.Make_pack (Counter_app.App)
module Store = Server.Store

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

let status response = Dream.status_to_int (Dream.status response)

(* POST /admin/checkpoint with an explicit Origin/Host pair. *)
let checkpoint_post handle ~origin ~host =
  handle
    (Dream.request ~method_:`POST ~target:"/admin/checkpoint"
       ~headers:[ ("Origin", origin); ("Host", host) ] "")

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let open Counter_app.App in
     let parent = Filename.temp_dir "ocaml-tea-pack-serve" "" in
     let root = Tea_server_pack.Root.v (Filename.concat parent "store") in
     let* t = Store.create ~now:(fun () -> 5L) root in
     let handle = Dream.test (Server.handler_pack t) in

     (* Seed committed work on the canonical branch the admin checkpoint acts
        on. Two increments -> count = 2. *)
     let* main = Store.main_session t in
     let* (_ : model) = Store.apply main Increment in
     let* (_ : model) = Store.apply main Increment in

     (* A cross-origin checkpoint POST is refused and must NOT mutate the
        store: no spine appears. *)
     let r_bad = checkpoint_post handle ~origin:"http://evil.example" ~host:"localhost" in
     check "cross-origin POST /admin/checkpoint is rejected (403)" (status r_bad = 403);
     let* spine_after_bad = Store.checkpoints_head t in
     check "a rejected checkpoint leaves the spine empty" (Option.is_none spine_after_bad);

     (* A same-origin checkpoint POST is accepted and creates the spine. *)
     let r_ok = checkpoint_post handle ~origin:"http://localhost" ~host:"localhost" in
     check "same-origin POST /admin/checkpoint is accepted (200)" (status r_ok = 200);
     let* spine_after_ok = Store.checkpoints_head t in
     check "an accepted checkpoint creates the spine" (Option.is_some spine_after_ok);
     let* spine_model =
       match spine_after_ok with
       | None -> Lwt.return (fst init)
       | Some cp -> Store.model_at t (Store.checkpoint_commit cp)
     in
     check "the checkpoint carries the committed model (count = 2)" (value spine_model = 2);

     (* Durability: close (flushing the pack suffix), reopen the same root, and
        the checkpoint driven through the server is still there. *)
     let* () = Store.close t in
     let* t2 = Store.create ~now:(fun () -> 6L) root in
     let* reopened = Store.checkpoints_head t2 in
     check "the server-driven checkpoint survives close+reopen" (Option.is_some reopened);
     let* reopened_model =
       match reopened with
       | None -> Lwt.return (fst init)
       | Some cp -> Store.model_at t2 (Store.checkpoint_commit cp)
     in
     check "the reopened checkpoint model is intact (count = 2)" (value reopened_model = 2);
     let* main2 = Store.main_session t2 in
     let* main2_model = Store.load main2 in
     check "the reopened main branch is intact (count = 2)" (value main2_model = 2);
     let* () = Store.close t2 in

     let rec rm_rf (path : string) : unit =
       if Sys.is_directory path then (
         Array.iter (fun entry -> rm_rf (Filename.concat path entry)) (Sys.readdir path);
         Sys.rmdir path)
       else Sys.remove path
     in
     rm_rf parent;
     Printf.printf "\nThe pack server checkpoints under the Origin gate and survives restart (D2).\n%!";
     Lwt.return_unit)
