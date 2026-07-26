(** The exploded-tree proof (roadmap step 8, D6).

    Without a witness the model is one blob at [\["model"\]] and every edit
    rewrites all of it. With a witness it is scattered one Irmin path per CRDT
    field, so a single-field edit rewrites a single path — and reassembly is the
    CvRDT join over the leaves, which is why D6 could only land after D1 (P3).

    The load-bearing checks here are the two that a merely-plausible
    implementation would fail: every field survives a scatter/gather round trip
    (a projection that dropped a field, or a bottom that clobbered one, shows up
    immediately), and a one-field edit touches exactly {i one} path (cardinality
    1 — the whole point of the deferral; a witness whose paths collided, or a
    write that still rewrote the blob, reads as 4 or 0 here). *)

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    incr failures)

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let open Shared_doc_app.App in
     let module Prim = Tea_core.Prim in
     let module Mem = Tea_persist.Store.Make (Shared_doc_app.App) in
     let sid s = Option.get (Prim.Session_id.of_string s) in
     let branch_of (s : string) = Prim.Branch_name.(to_string (of_session (sid s))) in

     (* The witness the app's ingredients assemble into: one path per field. *)
     let paths =
       List.map (fun ((n : string), (_ : model -> model)) -> n) explode_fields
     in
     let field_key (n : string) = Tea_safe.Safe_key.(root (Step.v n)) in
     let mem_witness () =
       Mem.exploder ~bottom ~join:doc_join
         ~fields:
           (List.map
              (fun ((n : string), project) -> (field_key n, project))
              explode_fields)
     in

     check "the app offers one exploded path per CRDT field (title/body/likes/tags)"
       (paths = [ "title"; "body"; "likes"; "tags" ]);
     check "the exploded paths are pairwise distinct (colliding paths would fuse fields)"
       (List.length (List.sort_uniq String.compare paths) = List.length paths);

     (* ---- Round trip: every field survives scatter + gather ---------------- *)
     let* t = Mem.create ~now:(fun () -> 1L) ~exploded:(mem_witness ()) () in
     let* s = Mem.session t (sid "explodeaa") in
     let* (_ : model) = Mem.apply s (Set_title "Treatise") in
     let* (_ : model) = Mem.apply s (Set_body "First paragraph.") in
     let* (_ : model) = Mem.apply s Like in
     let* (_ : model) = Mem.apply s Like in
     let* (_ : model) = Mem.apply s (Add_tag "urgent") in
     let* (_ : model) = Mem.apply s (Add_tag "review") in
     let* m = Mem.load s in
     check "round trip: the LWW title survives the scatter/gather" (title_of m = "Treatise");
     check "round trip: the LWW body survives" (body_of m = "First paragraph.");
     check "round trip: the PN-counter likes survive (2)" (likes_of m = 2);
     check "round trip: the OR-Set tags survive (both, sorted)"
       (tags_of m = [ "review"; "urgent" ]);

     (* Every field lands at its own path, and each leaf is field-isolated: the
        title leaf read on its own carries the title and nothing else. *)
     let* branch = Mem.S.of_branch (Mem.repo t) (branch_of "explodeaa") in
     let* title_leaf = Mem.S.find branch (Tea_safe.Safe_key.to_steps (field_key "title")) in
     let* likes_leaf = Mem.S.find branch (Tea_safe.Safe_key.to_steps (field_key "likes")) in
     let* blob = Mem.S.find branch Mem.model_path_raw in
     check "each field is stored at its own path (title leaf present)"
       (Option.is_some title_leaf);
     check "each field is stored at its own path (likes leaf present)"
       (Option.is_some likes_leaf);
     check "an exploded store writes NO whole-model blob at [\"model\"]"
       (Option.is_none blob);
     check "the title leaf is field-isolated: it carries the title..."
       (Option.fold title_leaf ~none:false ~some:(fun l -> title_of l = "Treatise"));
     check "...and every other field of that leaf is at bottom (likes = 0)"
       (Option.fold title_leaf ~none:false ~some:(fun l -> likes_of l = 0));

     (* ---- Cardinality: a one-field edit rewrites exactly one path ---------- *)
     let head () = Mem.S.Branch.find (Mem.repo t) (branch_of "explodeaa") in
     let value_at (c : Mem.S.commit) (n : string) =
       let* at = Mem.S.of_commit c in
       Mem.S.find at (Tea_safe.Safe_key.to_steps (field_key n))
     in
     let* before = head () in
     let* (_ : model) = Mem.apply s Like in
     let* after = head () in
     let* changed =
       Lwt_list.filter_map_s
         (fun n ->
           match (before, after) with
           | Some b, Some a ->
             let* vb = value_at b n in
             let* va = value_at a n in
             let same =
               Option.fold vb ~none:false ~some:(fun vb ->
                   Option.fold va ~none:false ~some:(fun va ->
                       Repr.(unstage (equal model_t)) vb va))
             in
             Lwt.return (if same then None else Some n)
           | None, (None | Some _) | Some _, None -> Lwt.return (Some n))
         paths
     in
     check "a single-field edit (Like) rewrites EXACTLY one Irmin path"
       (List.length changed = 1);
     check "and that one path is the field actually edited (likes)" (changed = [ "likes" ]);

     (* Undo still walks commits, so exploding did not break history. *)
     let* undone = Mem.undo s in
     check "undo over an exploded tree restores the previous model (likes back to 2)"
       (Option.fold undone ~none:false ~some:(fun m -> likes_of m = 2));

     (* Two exploded sessions still converge: the per-path merge dispatches the
        per-field joins, which is the reason D6 waits on D1. *)
     let* alice = Mem.fork t ~from:s (sid "explodeab") in
     let* (_ : model) = Mem.apply alice (Set_body "Alice's paragraph.") in
     let* (_ : model) = Mem.apply s Like in
     let* merged = Mem.merge_into ~src:alice ~dst:s in
     check "an exploded fork merges without conflict" (Result.is_ok merged);
     let* m2 = Mem.load s in
     check "per-path merge keeps Alice's body edit" (body_of m2 = "Alice's paragraph.");
     check "per-path merge keeps the concurrent like (PN sums to 3)" (likes_of m2 = 3);
     check "per-path merge leaves untouched fields alone (title still Treatise)"
       (title_of m2 = "Treatise");
     let* () = Mem.close t in

     (* ---- Migration: a branch written before the witness still reads -------
        A pre-D6 branch carries a whole blob at ["model"] and no field paths at
        all, so [gather] must fall back to it rather than hand back a bottom
        model. The legacy state is planted directly at the blob path — that is
        exactly the on-disk shape an older build left behind — so the fallback
        is exercised without depending on a backend's reopen semantics. *)
     let* t2 = Mem.create ~now:(fun () -> 1L) ~exploded:(mem_witness ()) () in
     let* ls = Mem.session t2 (sid "explodead") in
     let* legacy_branch = Mem.S.of_branch (Mem.repo t2) (branch_of "explodead") in
     let planted = fst (update (Mem.ctx_of_session ls) (Set_title "Legacy") bottom) in
     let* () =
       Mem.S.set_exn legacy_branch Mem.model_path_raw planted
         ~info:(fun () -> Mem.S.Info.v ~author:"legacy" ~message:"pre-D6 write" 1L)
     in
     let* mu = Mem.load ls in
     check "migration: a witness-less whole blob is still read once a witness exists"
       (title_of mu = "Legacy");
     (* And the first exploded write carries that migrated state forward. *)
     let* (_ : model) = Mem.apply ls Like in
     let* mu2 = Mem.load ls in
     check "migration: the first exploded write preserves the migrated state"
       (title_of mu2 = "Legacy" && likes_of mu2 = 1);
     let* title_after = Mem.S.find legacy_branch (Tea_safe.Safe_key.to_steps (field_key "title")) in
     check "migration: that write scatters the model into field paths"
       (Option.is_some title_after);
     let* () = Mem.close t2 in

     if !failures = 0 then (
       print_string
         "\nThe exploded tree scatters one path per CRDT field and reassembles by join (D6).\n";
       Lwt.return_unit)
     else (
       Printf.printf "\n%d check(s) FAILED\n" !failures;
       exit 1))
