(** The DESIGN §5 dedup regression pin (roadmap step 6).

    Irmin commits are content-addressed and [Info] dates are one-second
    resolution, so before the monotonic commit clock two branches making the
    {i identical} edit (same parent, same tree, same frozen second) collapsed
    into one commit — pulling a later merge base off the true fork point and
    counting the shared delta once. With the clock, every commit date is
    strictly increasing per store handle, so the sibling edits stay distinct
    and the three-way merge sums both deltas. *)

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let open Shared_doc_app.App in
     let module Store = Tea_persist.Store.Make (Shared_doc_app.App) in
     let module Prim = Tea_core.Prim in
     let check name cond =
       if cond then Printf.printf "ok   - %s\n%!" name
       else (
         Printf.printf "FAIL - %s\n%!" name;
         exit 1)
     in
     let sid s = Option.get (Prim.Session_id.of_string s) in
     let ref_str r = Prim.Commit_ref.to_string r in
     (* Freeze the wall source: every commit in this test is minted "inside
        one second" — exactly the regime where commits used to collapse. *)
     let* repo = Store.create ~now:(fun () -> 42L) () in
     let* main = Store.main_session repo in
     let* base = Store.apply main Like in
     check "base doc carries one like (the shared ancestor is non-trivial)"
       (base.likes = 1);
     let* alice = Store.fork repo ~from:main (sid "alice") in
     let* bob = Store.fork repo ~from:main (sid "bob") in
     (* The IDENTICAL first edit on both branches, same frozen second. *)
     let* (_ : model) = Store.apply alice Like in
     let* (_ : model) = Store.apply bob Like in
     let* ha = Store.head_ref alice in
     let* hb = Store.head_ref bob in
     let heads_differ =
       match (ha, hb) with
       | Some a, Some b -> not (String.equal (ref_str a) (ref_str b))
       | Some _, None | None, Some _ | None, None -> false
     in
     check "identical same-second sibling edits stay two distinct commits" heads_differ;
     (* The commit dates themselves are what forced the split apart. *)
     let date_of name =
       let branch_name = Prim.Branch_name.(to_string (of_session (sid name))) in
       let* b = Store.S.of_branch (Store.repo repo) branch_name in
       let* head = Store.S.Head.find b in
       Lwt.return
         (Option.map (fun c -> Store.S.Info.date (Store.S.Commit.info c)) head)
     in
     let* da = date_of "alice" in
     let* db = date_of "bob" in
     let dates_apart =
       match (da, db) with
       | Some a, Some b -> Int64.abs (Int64.sub a b) >= 1L
       | Some _, None | None, Some _ | None, None -> false
     in
     check "their commit dates differ by at least one tick" dates_apart;
     (* And therefore the merge base is the true fork commit: both +1 deltas
        survive the three-way counter merge (1 base + 1 + 1 = 3). Before the
        clock, the deduped shared commit WAS the base, yielding 2. *)
     let* merged = Store.merge_into ~src:alice ~dst:bob in
     check "merge of the sibling branches succeeds" (Result.is_ok merged);
     let* reconciled = Store.load bob in
     check "the merge counts BOTH sibling likes (likes = 3, not 2)"
       (reconciled.likes = 3);
     (* Same collapse regime for checkpoints: two sessions squashing the
        identical tree in the same frozen second must mint distinct roots. *)
     let* carol = Store.fork repo ~from:main (sid "carol") in
     let* dave = Store.fork repo ~from:main (sid "dave") in
     let* cp_c = Store.checkpoint carol ~label:"checkpoint" in
     let* cp_d = Store.checkpoint dave ~label:"checkpoint" in
     let cps_differ =
       match (cp_c, cp_d) with
       | Ok c, Ok d ->
         not
           (String.equal
              (ref_str (Store.checkpoint_ref c))
              (ref_str (Store.checkpoint_ref d)))
       | Ok _, Error (Empty_branch | Branch_moved)
       | Error (Empty_branch | Branch_moved), Ok _
       | Error (Empty_branch | Branch_moved), Error (Empty_branch | Branch_moved) ->
         false
     in
     check "identical same-second checkpoints stay distinct commits" cps_differ;
     Printf.printf "\nThe dedup collapse is fixed: history keeps intended-distinct edits distinct (DESIGN §5).\n%!";
     Lwt.return_unit)
