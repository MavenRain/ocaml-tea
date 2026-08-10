(** Commit coalescing end-to-end (roadmap step 6, R1): a run of chatty Msgs
    folds into ONE amended commit under a [Fold_run] policy, run boundaries
    and foreign commits break the run, [Keep_all] is bit-for-bit the
    historical behaviour, and undo works at whole-run granularity.

    One shared repo, one clock; every case runs on its own session branch
    (the mem backend's state is per functor application, so distinct
    "repos" from one [Store] module would share tables). *)

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let open Counter_app.App in
     let module Store = Tea_persist.Store.Make (Counter_app.App) in
     let module Spec = Tea_core.Coalesce_spec in
     let module Prim = Tea_core.Prim in
     let check name cond =
       if cond then Printf.printf "ok   - %s\n%!" name
       else (
         Printf.printf "FAIL - %s\n%!" name;
         exit 1)
     in
     let sid s = Option.get (Prim.Session_id.of_string s) in
     let fold_all = Spec.Fold_run (fun ~last:_ ~next -> Some next) in
     let* repo = Store.create ~now:(fun () -> 7L) () in

     (* (a) A burst folds into one commit; the model is the full fold. *)
     let* sa = Store.session repo (sid "aa") in
     let* (_ : model) = Store.apply sa Increment in
     let* base_hist = Store.history sa in
     let cz = Store.Coalescer.v fold_all in
     let* (_ : model) = Store.apply_coalesced cz sa Increment in
     let* (_ : model) = Store.apply_coalesced cz sa Increment in
     let* m = Store.apply_coalesced cz sa Increment in
     let* hist = Store.history sa in
     check "a 3-msg burst lands as exactly one extra commit"
       (List.length hist = List.length base_hist + 1);
     check "the folded model is the full 3-step fold (count = 4)" (value m = 4);

     (* (c) undo after the burst restores the pre-burst model. *)
     let* wa = Store.load_based sa in
     let* undone = Store.undo wa in
     let undo_ok =
       Result.fold undone
         ~ok:(fun u -> value u = 1)
         ~error:(fun (_ : Store.undo_error) -> false)
     in
     check "undo after a burst is whole-run (back to pre-burst count = 1)" undo_ok;

     (* (b) A policy boundary (None) starts a fresh commit. *)
     let boundary_on_reset =
       Spec.Fold_run
         (fun ~last:_ ~next ->
           match next with
           | Reset -> None
           | Increment | Decrement | Sync (_ : model) -> Some next)
     in
     let* sb = Store.session repo (sid "bb") in
     let cz2 = Store.Coalescer.v boundary_on_reset in
     let* (_ : model) = Store.apply_coalesced cz2 sb Increment in
     let* (_ : model) = Store.apply_coalesced cz2 sb Increment in
     let* (_ : model) = Store.apply_coalesced cz2 sb Reset in
     let* hist2 = Store.history sb in
     check "a policy None boundary yields two commits (run, then boundary msg)"
       (List.length hist2 = 2);
     let* wb = Store.load_based sb in
     let* undone2 = Store.undo wb in
     let one_back =
       Result.fold undone2
         ~ok:(fun u -> value u = 2)
         ~error:(fun (_ : Store.undo_error) -> false)
     in
     check "undo then steps exactly one boundary back (count = 2)" one_back;

     (* (d) Keep_all parity with persist_test's exact sequence. *)
     let* sd = Store.session repo (sid "dd") in
     let cz3 = Store.Coalescer.v Spec.Keep_all in
     let* (_ : model) = Store.apply_coalesced cz3 sd Increment in
     let* (_ : model) = Store.apply_coalesced cz3 sd Increment in
     let* m3 = Store.apply_coalesced cz3 sd Increment in
     let* hist3 = Store.history sd in
     check "Keep_all parity: three increments -> count = 3" (value m3 = 3);
     check "Keep_all parity: history has one commit per update (3)"
       (List.length hist3 = 3);
     let* wd = Store.load_based sd in
     let* u3 = Store.undo wd in
     let keep_all_undo =
       Result.fold u3
         ~ok:(fun u -> value u = 2)
         ~error:(fun (_ : Store.undo_error) -> false)
     in
     check "Keep_all parity: undo walks one commit (count = 2)" keep_all_undo;

     (* (e) Ownership guard: a foreign commit between two mergeable Msgs is
        never amended away — the next Msg appends on top of it. *)
     let* se = Store.session repo (sid "ee") in
     let cz4 = Store.Coalescer.v fold_all in
     let* (_ : model) = Store.apply_coalesced cz4 se Increment in
     let* (_ : model) = Store.apply se Increment in
     let* foreign_head = Store.head_ref se in
     let* (_ : model) = Store.apply_coalesced cz4 se Increment in
     let* hist4 = Store.history se in
     check "a foreign commit breaks the run (3 commits, nothing amended)"
       (List.length hist4 = 3);
     let foreign_survives =
       match foreign_head with
       | Some r ->
         List.exists
           (fun h ->
             String.equal (Prim.Commit_ref.to_string h) (Prim.Commit_ref.to_string r))
           hist4
       | None -> false
     in
     check "the foreign commit survives verbatim in history" foreign_survives;
     let* m4 = Store.load se in
     check "the model still folds every msg (1 + 1 + 1 = 3)" (value m4 = 3);

     (* (g) seal forces a run boundary. *)
     let* sg = Store.session repo (sid "gg") in
     let cz5 = Store.Coalescer.v fold_all in
     let* (_ : model) = Store.apply_coalesced cz5 sg Increment in
     Store.Coalescer.seal cz5;
     let* (_ : model) = Store.apply_coalesced cz5 sg Increment in
     let* hist5 = Store.history sg in
     check "seal forces the next mergeable msg to append (2 commits)"
       (List.length hist5 = 2);

     (* (f) The watch fires once per commit of a burst — frames are the live
        echo and are deliberately NOT coalesced away. *)
     let* sf = Store.session repo (sid "ff") in
     let seen = ref [] in
     let* w =
       Store.watch sf (fun mw ->
           seen := value mw :: !seen;
           Lwt.return_unit)
     in
     let cz6 = Store.Coalescer.v fold_all in
     let* (_ : model) = Store.apply_coalesced cz6 sf Increment in
     let* (_ : model) = Store.apply_coalesced cz6 sf Increment in
     let* (_ : model) = Store.apply_coalesced cz6 sf Increment in
     let* fired = Test_util.await (fun () -> List.length !seen >= 3) in
     check "the watch fires once per amend (3 frames, strictly newer models)"
       (fired && List.rev !seen = [ 1; 2; 3 ]);
     let* () = Store.unwatch w in

     (* (h) Fork mid-burst WAS the documented hazard: the next amend moved the
        merge base off the forked commit, so the step-4 three-way merge saw both
        sides changed and conflicted. Under the CvRDT merge (D1) that base shift
        no longer conflicts — the PN-counter join reconciles the two heads with
        no ancestor (D9 resolved). Both the mid-burst fork and the sealed fork
        now converge; the observable difference is only in commit count, which
        the earlier cases pin. *)
     let* sh = Store.session repo (sid "hh") in
     let cz7 = Store.Coalescer.v fold_all in
     let* (_ : model) = Store.apply sh Increment in
     let* (_ : model) = Store.apply_coalesced cz7 sh Increment in
     let* forked_hot = Store.fork repo ~from:sh (sid "midburst") in
     let* (_ : model) = Store.apply_coalesced cz7 sh Increment in
     let* merged_hot = Store.merge_into ~src:sh ~dst:forked_hot in
     check "fork mid-burst now converges via the CvRDT join (no conflict)"
       (Result.is_ok merged_hot);
     let* mhot = Store.load forked_hot in
     check "the mid-burst merge reconciles to the full run (count = 3)" (value mhot = 3);
     let* sh2 = Store.session repo (sid "hh2") in
     let cz8 = Store.Coalescer.v fold_all in
     let* (_ : model) = Store.apply sh2 Increment in
     let* (_ : model) = Store.apply_coalesced cz8 sh2 Increment in
     Store.Coalescer.seal cz8;
     let* forked_cold = Store.fork repo ~from:sh2 (sid "sealed") in
     let* (_ : model) = Store.apply_coalesced cz8 sh2 Increment in
     let* merged_cold = Store.merge_into ~src:sh2 ~dst:forked_cold in
     check "seal before fork converges too (fast-forward)" (Result.is_ok merged_cold);
     let* mf = Store.load forked_cold in
     check "the sealed-fork session sees the appended run (count = 3)" (value mf = 3);

     (* (i) Seeded property loop: whatever the (deterministically random)
        policy verdicts and msg mix, the final model equals the plain
        List.fold_left over A.update, on a fresh branch per scenario. *)
     let lcg_state = ref 99L in
     let lcg_bit () =
       Int64.equal
         (Int64.logand (Int64.shift_right_logical (Test_util.lcg_next lcg_state) 40) 1L)
         1L
     in
     let iters = 200 in
     (* A single-replica context for the reference fold: the counter's value is
        replica-agnostic (n increments read as n under any one replica), so this
        matches whatever replica the store applies each branch under. *)
     let prop_ctx =
       Tea_core.Crdt.Ctx.v
         ~clock:(Tea_core.Clock.create ~now:(fun () -> 0L))
         ~replica:(Tea_core.Crdt.Replica.v (Prim.Session_id.v "prop"))
     in
     let* prop_ok =
       List.fold_left
         (fun acc (i : int) ->
           let* ok = acc in
           if not ok then Lwt.return false
           else
             let policy =
               Spec.Fold_run
                 (fun ~last:_ ~next ->
                   match next with
                   | Increment | Decrement | Reset | Sync (_ : model) ->
                     if lcg_bit () then Some next else None)
             in
             let* si = Store.session repo (sid (Printf.sprintf "prop%04x" i)) in
             let czi = Store.Coalescer.v policy in
             let n = 1 + (i mod 8) in
             let msgs =
               List.init n (fun k -> if k mod 3 = 2 then Decrement else Increment)
             in
             let* final =
               List.fold_left
                 (fun acc_m msg ->
                   let* (_ : model) = acc_m in
                   Store.apply_coalesced czi si msg)
                 (Store.load si) msgs
             in
             let expected =
               List.fold_left (fun mm msg -> fst (update prop_ctx msg mm)) (fst init) msgs
             in
             Lwt.return (value final = value expected))
         (Lwt.return true)
         (List.init iters Fun.id)
     in
     check
       (Printf.sprintf
          "%d seeded scenarios: coalesced fold == plain List.fold_left over update"
          iters)
       prop_ok;
     Printf.printf
       "\nCommit coalescing folds runs, guards ownership, and preserves the fold (R1).\n%!";
     Lwt.return_unit)
