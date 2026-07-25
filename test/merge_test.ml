(** The merge-combinator laws (R2). The design calls for QCheck; this switch
    ships no property-test framework, so we run a small {b seeded} generator
    loop instead - same idea (hundreds of random cases, first counterexample
    printed), zero new dependencies, matching the repo's plain-[Printf] test
    style. Every {i automatic} combinator (atomic, counter, set, record -
    [conflict] is the deliberate non-automatic default) is checked for:

    - {b reflexivity}: [run m ~ancestor:(Some x) ~ours:x ~theirs:x = Ok x];
    - {b left/right identity}: a side unchanged from the ancestor yields the
      other side;
    - {b commutativity}: swapping [ours]/[theirs] gives an equivalent result
      (both [Ok] and equal, or both [Error]).

    Explicit value cases pin the actual reconciliation (counter sums, set
    union/remove-wins, atomic conflict, record field-independence + labelled
    conflict propagation). *)

open Tea_core

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    incr failures)

(* Two merge results are equivalent when both succeed with [eq] values, or both
   conflict. (We never assert a particular conflict {i string} in a law - only
   the shape - so commutativity holds for [atomic] too.) *)
let res_equiv eq r1 r2 =
  match r1, r2 with
  | Ok a, Ok b -> eq a b
  | Error _, Error _ -> true
  | Ok _, Error _ | Error _, Ok _ -> false

let cases = 500

(* Run [law] over [cases] seeded random inputs; report the first counterexample
   with the case index so a failure is reproducible under the fixed seed. *)
let prop name (law : int -> bool) =
  let rec loop i =
    if i >= cases then check name true
    else if law i then loop (i + 1)
    else (
      Printf.printf "FAIL - %s (counterexample at seeded case #%d)\n%!" name i;
      incr failures)
  in
  loop 0

(* The four automatic-merge laws for a combinator [m] over a type with equality
   [eq], sampling ancestors/values with [gen]. [atomic] can conflict, so the
   laws are stated as "equivalent results" and the identity laws only fire when
   one side is genuinely unchanged (which never conflicts). *)
let check_laws ~name ~(m : 'a Merge.t) ~(eq : 'a -> 'a -> bool) ~(gen : unit -> 'a) =
  let run = Merge.run m in
  prop (name ^ ": reflexivity") (fun _ ->
      let x = gen () in
      res_equiv eq (run ~ancestor:(Some x) ~ours:x ~theirs:x) (Ok x));
  prop (name ^ ": left identity (ours unchanged -> theirs)") (fun _ ->
      let a = gen () and b = gen () in
      res_equiv eq (run ~ancestor:(Some a) ~ours:a ~theirs:b) (Ok b));
  prop (name ^ ": right identity (theirs unchanged -> ours)") (fun _ ->
      let a = gen () and b = gen () in
      res_equiv eq (run ~ancestor:(Some a) ~ours:b ~theirs:a) (Ok b));
  prop (name ^ ": commutativity") (fun _ ->
      let ancestor = if Random.bool () then Some (gen ()) else None in
      let x = gen () and y = gen () in
      res_equiv eq (run ~ancestor ~ours:x ~theirs:y) (run ~ancestor ~ours:y ~theirs:x))

(* Generators (bounded, so counterexamples stay legible). *)
let rint () = Random.int 21 - 10

let rstr () =
  let opts = [| "a"; "b"; "c"; "" |] in
  opts.(Random.int (Array.length opts))

let rtags () =
  let pool = [ "x"; "y"; "z"; "w" ] in
  List.filter (fun _ -> Random.bool ()) pool

let set_eq xs ys =
  let n = List.sort_uniq String.compare in
  List.equal String.equal (n xs) (n ys)

(* A record combining two combinators, so [record] itself goes through the law
   harness (not just a couple of pinned cases): a [counter] field and an
   [atomic] field. *)
type rec2 =
  { ra : int
  ; rb : string
  }

let rgen () = { ra = rint (); rb = rstr () }
let rec2_eq x y = x.ra = y.ra && String.equal x.rb y.rb

let rec2_merge =
  Merge.(
    record
      [ field ~label:"ra" ~get:(fun r -> r.ra) ~set:(fun v r -> { r with ra = v }) counter
      ; field ~label:"rb" ~get:(fun r -> r.rb) ~set:(fun v r -> { r with rb = v })
          (atomic ~eq:String.equal)
      ])

let () =
  Random.init 424242;
  (* Laws, per combinator. *)
  check_laws ~name:"counter" ~m:Merge.counter ~eq:Int.equal ~gen:rint;
  check_laws ~name:"set" ~m:(Merge.set ~cmp:String.compare) ~eq:set_eq ~gen:rtags;
  check_laws ~name:"atomic(string)" ~m:(Merge.atomic ~eq:String.equal) ~eq:String.equal ~gen:rstr;
  check_laws ~name:"record" ~m:rec2_merge ~eq:rec2_eq ~gen:rgen;

  (* Explicit reconciliation values. *)
  let run m = Merge.run m in
  check "counter sums both deltas (5;7;8 -> 10)"
    (run Merge.counter ~ancestor:(Some 5) ~ours:7 ~theirs:8 = Ok 10);
  check "counter with no ancestor adds from 0 (3;4 -> 7)"
    (run Merge.counter ~ancestor:None ~ours:3 ~theirs:4 = Ok 7);
  check "map clamps a counter's result (base 1, both -> 0, raw -1 clamped to 0)"
    (run Merge.(map (max 0) counter) ~ancestor:(Some 1) ~ours:0 ~theirs:0 = Ok 0);

  let sset = Merge.set ~cmp:String.compare in
  check "set keeps an add and honours a concurrent remove (add c, remove a -> {b,c})"
    (run sset ~ancestor:(Some [ "a"; "b" ]) ~ours:[ "a"; "b"; "c" ] ~theirs:[ "b" ]
    = Ok [ "b"; "c" ]);
  check "set unions when there is no ancestor"
    (set_eq
       (match run sset ~ancestor:None ~ours:[ "x"; "y" ] ~theirs:[ "y"; "z" ] with
        | Ok v -> v
        | Error _ -> [])
       [ "x"; "y"; "z" ]);

  let satomic = Merge.atomic ~eq:String.equal in
  check "atomic takes the one side that changed"
    (run satomic ~ancestor:(Some "x") ~ours:"x" ~theirs:"z" = Ok "z");
  check "atomic conflicts when both sides changed apart"
    (match run satomic ~ancestor:(Some "x") ~ours:"y" ~theirs:"z" with
     | Error _ -> true
     | Ok _ -> false);
  check "atomic with no ancestor agrees or conflicts"
    (run satomic ~ancestor:None ~ours:"a" ~theirs:"a" = Ok "a"
    && (match run satomic ~ancestor:None ~ours:"a" ~theirs:"b" with
        | Error _ -> true
        | Ok _ -> false));

  (* Record: fields reconcile independently, and the first field conflict fails
     the whole record with its label leading the message. *)
  let module R = struct
    type t =
      { n : int
      ; s : string
      }
  end in
  let rmerge =
    Merge.(
      record
        [ field ~label:"n" ~get:(fun (r : R.t) -> r.n) ~set:(fun v r -> { r with R.n = v }) counter
        ; field ~label:"s" ~get:(fun (r : R.t) -> r.s) ~set:(fun v r -> { r with R.s = v })
            (atomic ~eq:String.equal)
        ])
  in
  let req = R.{ n = 0; s = "a" } in
  check "record merges each field independently (counter field sums; scalar unchanged)"
    (run rmerge ~ancestor:(Some req) ~ours:{ req with R.n = 1 } ~theirs:{ req with R.n = 2 }
    = Ok R.{ n = 3; s = "a" });
  check "record surfaces a field conflict, labelled by field"
    (match
       run rmerge ~ancestor:(Some req) ~ours:R.{ n = 1; s = "b" } ~theirs:R.{ n = 2; s = "c" }
     with
     | Error msg ->
       (* the label must lead, so a caller can point at the offending field *)
       String.length msg >= 2 && String.sub msg 0 2 = "s:"
     | Ok _ -> false);

  (* Two independently-conflicting fields: the record must fail on the FIRST
     one in list order (["p"] before ["q"]), not the last or an arbitrary one,
     so the surfaced label is deterministic. *)
  let module R2 = struct
    type t =
      { p : string
      ; q : string
      }
  end in
  let two_conflict =
    Merge.(
      record
        [ field ~label:"p" ~get:(fun (r : R2.t) -> r.p) ~set:(fun v r -> { r with R2.p = v })
            (atomic ~eq:String.equal)
        ; field ~label:"q" ~get:(fun (r : R2.t) -> r.q) ~set:(fun v r -> { r with R2.q = v })
            (atomic ~eq:String.equal)
        ])
  in
  let base2 = R2.{ p = "0"; q = "0" } in
  check "record fails on the first conflicting field in list order (p before q)"
    (match
       run two_conflict ~ancestor:(Some base2) ~ours:R2.{ p = "a"; q = "c" }
         ~theirs:R2.{ p = "b"; q = "d" }
     with
     | Error msg -> String.length msg >= 2 && String.sub msg 0 2 = "p:"
     | Ok _ -> false);

  if !failures = 0 then
    Printf.printf "\nAll merge-combinator laws hold (R2 combinator library).\n%!"
  else (
    Printf.printf "\n%d merge check(s) FAILED.\n%!" !failures;
    exit 1)
