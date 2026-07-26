(** The D5 archive GC on a lower-layered pack store (roadmap step 8).

    Built with [~lower_root], the pack config flips [Gc.behaviour] to
    [`Archive], so a GC that would DELETE pre-checkpoint history under the
    default config instead moves it to the lower layer, where it stays
    readable. Both observables are anti-vacuous against dropping [~lower_root]
    (behaviour would become [`Delete] and the old commit would vanish — exactly
    what [pack_test] pins for the no-lower case). *)

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

     let parent = Filename.temp_dir "ocaml-tea-archive" "" in
     let root = Tea_persist_pack.Store_pack.Root.v (Filename.concat parent "store") in
     let lower = Filename.concat parent "lower" in
     let* t = Store.create ~now:(fun () -> 7L) ~lower_root:lower root in

     (* The whole point of a lower layer: GC archives instead of deleting. *)
     check "a lower_root store selects Archive GC behaviour"
       (match Store.gc_behaviour t with
        | `Archive -> true
        | `Delete -> false);

     (* Baseline history, then capture the pre-checkpoint head key. *)
     let* main = Store.main_session t in
     let* (_ : model) = Store.apply main Increment in
     let* (_ : model) = Store.apply main Increment in
     let* m3 = Store.apply main Increment in
     check "archive baseline: three increments -> count = 3" (value m3 = 3);
     let* mainb = Store.S.main (Store.repo t) in
     let* head0 = Store.S.Head.find mainb in
     (* Probe by HASH, not by the direct (offset) key: archive GC relocates the
        object from the live suffix to the lower layer, so a stale offset key
        no longer resolves while the content-addressed hash still does. *)
     let h0 = Option.map Store.S.Commit.hash head0 in
     check "pre-checkpoint head captured" (Option.is_some h0);

     (* Squash to a checkpoint (severing the old chain), then archive-GC to it. *)
     let* cp_r = Store.checkpoint main ~label:"checkpoint" in
     let cp =
       match cp_r with
       | Ok cp -> Some cp
       | Error (Empty_branch | Branch_moved) -> None
     in
     check "checkpoint succeeds" (Option.is_some cp);
     let cp = Option.get cp in
     let* gc_r = Store.gc t ~retain:cp in
     let gc_ok =
       match gc_r with
       | Ok () -> true
       | Error (Gc_disallowed | Gc_already_running | Gc_failed _) -> false
     in
     check "archive gc ~retain:checkpoint succeeds" gc_ok;

     (* Head model is intact, history collapsed to the checkpoint. *)
     let* m_main = Store.load main in
     check "main's model is intact after archive gc (count = 3)" (value m_main = 3);
     let* hist_main = Store.history main in
     check "main's history is exactly the checkpoint after gc" (List.length hist_main = 1);

     (* The distinguishing observable of Archive vs Delete: the discarded
        prefix is PRESERVED in the lower layer rather than deleted. In Irmin
        3.11 archived objects are not surfaced through the public [Commit]
        read path (that needs the volume API the spec forbids), so the faithful
        observable is that the lower directory is populated. Under Delete mode
        (no [lower_root]) there is no lower directory at all — so both this and
        [gc_behaviour] flip when [~lower_root] is dropped. The [h0] hash is kept
        only to pin that a real pre-checkpoint commit existed to archive. *)
     let dir_nonempty (d : string) : bool = Sys.file_exists d && Array.length (Sys.readdir d) > 0 in
     ignore (h0 : Store.S.hash option);
     check "archive gc preserves the discarded prefix in the lower layer"
       (dir_nonempty lower);

     (* Reopen with the lower attached: the store is healthy, still Archive, and
        the retained checkpoint model is intact. *)
     let* () = Store.close t in
     let* t2 = Store.create ~now:(fun () -> 8L) ~lower_root:lower root in
     check "the reopened lower_root store still selects Archive behaviour"
       (match Store.gc_behaviour t2 with
        | `Archive -> true
        | `Delete -> false);
     let* main2 = Store.main_session t2 in
     let* m_re = Store.load main2 in
     check "the retained checkpoint model survives archive gc + reopen (count = 3)"
       (value m_re = 3);
     let* () = Store.close t2 in

     let rec rm_rf (path : string) : unit =
       if Sys.is_directory path then (
         Array.iter (fun entry -> rm_rf (Filename.concat path entry)) (Sys.readdir path);
         Sys.rmdir path)
       else Sys.remove path
     in
     rm_rf parent;
     Printf.printf "\nArchive GC preserves pre-checkpoint history in the lower layer (D5).\n%!";
     Lwt.return_unit)
