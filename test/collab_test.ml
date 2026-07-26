(** The collaboration proof (thesis T2, roadmap step 4): two sessions edit the
    same document on their own Irmin branches and reconcile through
    [Store.merge_into], dispatching to the app's real {!Tea_core.Merge.record}
    policy. This is where "branch-per-session + three-way merge = built-in
    collaboration" stops being a claim about lifting and becomes an end-to-end
    result.

    Two scenarios, mirroring R2's two halves:
    - {b structural fields merge automatically} - a shared doc is forked to
      Alice and Bob, who like it and tag it concurrently (and only Bob renames
      it); the merge sums the likes, unions the tags, and takes Bob's lone title
      edit, with no conflict;
    - {b divergent free-text is surfaced, never guessed} - two sessions rename
      the {i same} doc differently, and the merge returns a labelled conflict
      instead of silently dropping one edit. *)

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let open Shared_doc_app.App in
     let module Store = Tea_persist.Store.Make (Shared_doc_app.App) in
     let failures = ref 0 in
     let check name cond =
       if cond then Printf.printf "ok   - %s\n%!" name
       else (
         Printf.printf "FAIL - %s\n%!" name;
         incr failures)
     in
     let sid s = Option.get (Tea_core.Prim.Session_id.of_string s) in
     (* Irmin may wrap a conflict string (e.g. with a recursive-ancestor note),
        so assert the field label is *present*, not that it leads. *)
     let contains ~needle s =
       let nl = String.length needle and sl = String.length s in
       let rec at i =
         if i + nl > sl then false
         else if String.equal (String.sub s i nl) needle then true
         else at (i + 1)
       in
       at 0
     in
     let* repo = Store.create () in

     (* A shared document exists on main: the common ancestor both sessions
        fork from. It already has one like, so the merge base is non-zero and
        the counter's [- ancestor] term is genuinely exercised (a merge that
        forgot the base, or flipped its sign, would still pass if the base were
        0). *)
     let* main = Store.main_session repo in
     let* (_ : model) = Store.apply main Like in
     let* (_ : model) = Store.apply main (Set_title "Draft") in

     (* Alice and Bob each fork the shared doc onto their own session branch.
        [Store.fork] copies main's head onto the fresh branch (a head move, not
        a merge), so the two sessions share main's commit as their single, clean
        common ancestor. *)
     let* alice = Store.fork repo ~from:main (sid "alice") in
     let* bob = Store.fork repo ~from:main (sid "bob") in
     let* alice_start = Store.load alice in
     check "the fork carries the shared state (title = Draft, likes = 1)"
       (String.equal alice_start.title "Draft" && alice_start.likes = 1);

     (* Concurrent edits on the two branches. The two sessions' first commits
        are deliberately different shapes for realism; since roadmap step 6
        the monotonic commit clock makes even *identical* same-second first
        edits stay distinct commits (the content-address dedup that used to
        pull the merge base off the true root is pinned fixed in
        [dedup_test.ml]). Only Bob edits the title. *)
     let* (_ : model) = Store.apply alice Like in
     let* (_ : model) = Store.apply alice (Add_tag "urgent") in
     let* (_ : model) = Store.apply bob (Add_tag "review") in
     let* (_ : model) = Store.apply bob Like in
     let* (_ : model) = Store.apply bob Like in
     let* (_ : model) = Store.apply bob (Set_title "Final") in
     (* Reconcile Bob's branch into Alice's. *)
     let* merged = Store.merge_into ~src:bob ~dst:alice in
     check "concurrent structural edits merge with no conflict" (merged = Ok ());
     let* doc = Store.load alice in
     check "likes: shared base 1 + Alice's +1 + Bob's +2 -> 4" (doc.likes = 4);
     check "tags: concurrent adds union -> [review; urgent]"
       (List.sort_uniq String.compare doc.tags = [ "review"; "urgent" ]);
     check "title: only Bob edited it, so his edit is taken" (String.equal doc.title "Final");
     check "body: untouched on both sides, stays empty" (String.equal doc.body "");

     (* Second scenario: both sides rename the same doc differently. The merge
        must surface the conflict, labelled by field - never pick a winner. *)
     let* carol = Store.fork repo ~from:main (sid "carol") in
     let* dave = Store.fork repo ~from:main (sid "dave") in
     let* (_ : model) = Store.apply carol (Set_title "Carol's title") in
     let* (_ : model) = Store.apply dave (Set_title "Dave's title") in
     let* carol_head_before = Store.head_ref carol in
     let* clash = Store.merge_into ~src:dave ~dst:carol in
     let labelled_title_conflict =
       match clash with
       | Ok () -> false
       | Error msg -> contains ~needle:"title" msg
     in
     check "divergent title edits surface a field-labelled conflict (R2 'don't guess')"
       labelled_title_conflict;
     (* Not just the content: the branch head must be byte-identical - a failed
        merge writes no commit at all (so this would also catch an ours-wins
        merge that quietly committed a head preserving Carol's title). *)
     let* carol_head_after = Store.head_ref carol in
     let* carol_doc = Store.load carol in
     check "a conflicting merge leaves the target branch untouched (head + content)"
       (carol_head_before = carol_head_after && String.equal carol_doc.title "Carol's title");

     (* Fork must never clobber a session that already holds committed work:
        re-forking Carol's populated branch from main returns it unchanged. *)
     let* refork = Store.fork repo ~from:main (sid "carol") in
     let* refork_head = Store.head_ref refork in
     let* refork_doc = Store.load refork in
     check "fork over an existing session preserves its head and work"
       (refork_head = carol_head_after && String.equal refork_doc.title "Carol's title");

     (* Third scenario: the merge must respect the app's own invariants. The
        shared doc floors likes at 0 (via [Unlike]'s clamp); the merge policy
        clamps too, so two concurrent unlikes of the 1-like ancestor reconcile
        to 0, never the -1 the raw delta-sum (0 + 0 - 1) would give. Each side
        adds a distinct tag first so the scenario reads as real divergence;
        the step-6 monotonic clock alone already keeps identical Unlike
        commits distinct (see [dedup_test.ml]). *)
     let* eve = Store.fork repo ~from:main (sid "eve") in
     let* frank = Store.fork repo ~from:main (sid "frank") in
     let* (_ : model) = Store.apply eve (Add_tag "e") in
     let* (_ : model) = Store.apply eve Unlike in
     let* (_ : model) = Store.apply frank (Add_tag "f") in
     let* (_ : model) = Store.apply frank Unlike in
     let* clamp_ok = Store.merge_into ~src:frank ~dst:eve in
     let* eve_doc = Store.load eve in
     check "merged likes never fall below 0 (concurrent unlikes clamp at 0)"
       (clamp_ok = Ok () && eve_doc.likes = 0);

     if !failures = 0 then (
       Printf.printf "\nTwo sessions reconciled a shared document (T2 proven end-to-end).\n%!";
       Lwt.return_unit)
     else (
       Printf.printf "\n%d collaboration check(s) FAILED.\n%!" !failures;
       exit 1))
