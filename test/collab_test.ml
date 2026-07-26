(** The collaboration proof (thesis T2), rebuilt on state-based CRDTs (roadmap
    step 8, D1). Two sessions edit the same document on their own Irmin branches
    and reconcile through [Store.merge_into], which now dispatches to the app's
    {!Tea_core.Merge_spec.Crdt_join} policy: a per-field least-upper-bound that
    discards the common ancestor. Convergence replaces conflict — the CvRDT join
    is idempotent, commutative and associative, so concurrent edits {i always}
    reconcile with no [Error].

    Three scenarios exercise the three field CRDTs:
    - {b PN-counter + OR-Set}: concurrent likes {i sum}, concurrent tag-adds
      {i union}, both add-wins;
    - {b LWW}: two concurrent title edits no longer conflict — the higher
      [(stamp, Session_id)] dot wins deterministically;
    - {b projection clamp}: two concurrent unlikes drive the raw PN value
      negative, and the app's [likes_of] projection floors it at 0. *)

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
     let* repo = Store.create () in

     (* A shared document exists on main: the common ancestor both sessions fork
        from. It already has one like, so the merge base is non-zero — a join
        that secretly folded the ancestor back in (the vacuity trap the 2-arg
        [Crdt_join] ctor forbids) would push the summed likes off 4. *)
     let* main = Store.main_session repo in
     let* (_ : model) = Store.apply main Like in
     let* (_ : model) = Store.apply main (Set_title "Draft") in

     (* Alice and Bob each fork the shared doc onto their own session branch. The
        branch name is the CRDT replica id, so their edits land on distinct
        PN-counter and LWW replicas. *)
     let* alice = Store.fork repo ~from:main (sid "alice") in
     let* bob = Store.fork repo ~from:main (sid "bob") in
     let* alice_start = Store.load alice in
     check "the fork carries the shared state (title = Draft, likes = 1)"
       (String.equal (title_of alice_start) "Draft" && likes_of alice_start = 1);

     (* Concurrent edits. Only Bob renames the doc, and he does so strictly after
        Draft was set, so his LWW dot dominates. *)
     let* (_ : model) = Store.apply alice Like in
     let* (_ : model) = Store.apply alice (Add_tag "urgent") in
     let* (_ : model) = Store.apply bob (Add_tag "review") in
     let* (_ : model) = Store.apply bob Like in
     let* (_ : model) = Store.apply bob Like in
     let* (_ : model) = Store.apply bob (Set_title "Final") in
     let* merged = Store.merge_into ~src:bob ~dst:alice in
     check "concurrent CRDT edits converge with no conflict" (merged = Ok ());
     let* doc = Store.load alice in
     check "likes: PN-counter sums shared base 1 + Alice's +1 + Bob's +2 -> 4" (likes_of doc = 4);
     check "tags: OR-Set unions concurrent adds -> [review; urgent]"
       (List.sort_uniq String.compare (tags_of doc) = [ "review"; "urgent" ]);
     check "title: LWW resolves to Bob's later (higher-stamp) edit" (String.equal (title_of doc) "Final");
     check "body: untouched on both sides, stays empty" (String.equal (body_of doc) "");

     (* Re-merging the same source must not double-count: the join is a LUB, so
        it is idempotent and base-independent. A merge arm that folded the
        ancestor in would move the likes on the second pass. *)
     let* merged2 = Store.merge_into ~src:bob ~dst:alice in
     let* doc2 = Store.load alice in
     check "re-merging the same source is idempotent (base-independent LUB)"
       (merged2 = Ok ()
       && likes_of doc2 = 4
       && String.equal (title_of doc2) "Final"
       && List.sort_uniq String.compare (tags_of doc2) = [ "review"; "urgent" ]);

     (* Second scenario: both sides rename the same doc differently. Under the
        step-4 three-way policy this was a conflict; the CvRDT LWW resolves it
        deterministically to the later writer (Dave applies after Carol, so his
        stamp is higher). No [Error], no guessing — a documented, in-scope
        reintroduction of last-writer-wins for free text. *)
     let* carol = Store.fork repo ~from:main (sid "carol") in
     let* dave = Store.fork repo ~from:main (sid "dave") in
     let* (_ : model) = Store.apply carol (Set_title "Carol's title") in
     let* (_ : model) = Store.apply dave (Set_title "Dave's title") in
     let* clash = Store.merge_into ~src:dave ~dst:carol in
     check "divergent title edits converge (LWW), no conflict" (clash = Ok ());
     let* carol_doc = Store.load carol in
     check "LWW picks the higher-(stamp,replica) writer: Dave's later edit"
       (String.equal (title_of carol_doc) "Dave's title");

     (* Fork must never clobber a session that already holds committed work:
        re-forking Carol's populated branch from main returns it unchanged. *)
     let* refork = Store.fork repo ~from:main (sid "carol") in
     let* refork_doc = Store.load refork in
     check "fork over an existing session preserves its work"
       (String.equal (title_of refork_doc) "Dave's title");

     (* Third scenario: two concurrent unlikes of the 1-like ancestor. The
        PN-counter has no floor — the raw value legitimately reaches -1 — and the
        app's [likes_of] projection is what clamps it to 0. Asserting both proves
        the clamp is real (not a vacuous constant): drop the [max 0] and this
        check reads -1. *)
     let* eve = Store.fork repo ~from:main (sid "eve") in
     let* frank = Store.fork repo ~from:main (sid "frank") in
     let* (_ : model) = Store.apply eve (Add_tag "e") in
     let* (_ : model) = Store.apply eve Unlike in
     let* (_ : model) = Store.apply frank (Add_tag "f") in
     let* (_ : model) = Store.apply frank Unlike in
     let* clamp_ok = Store.merge_into ~src:frank ~dst:eve in
     let* eve_doc = Store.load eve in
     check "concurrent unlikes: raw PN value is -1, projection floors it at 0"
       (clamp_ok = Ok () && Likes.value eve_doc.likes = -1 && likes_of eve_doc = 0);

     if !failures = 0 then (
       Printf.printf "\nTwo sessions converged a shared document via CvRDT joins (T2, SEC).\n%!";
       Lwt.return_unit)
     else (
       Printf.printf "\n%d collaboration check(s) FAILED.\n%!" !failures;
       exit 1))
