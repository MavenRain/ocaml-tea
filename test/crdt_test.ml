(** The CvRDT join laws (roadmap step 8, D1). Each of the three CRDTs is a
    join-semilattice, so [join] must be {b idempotent} ([join x x = x]),
    {b commutative} ([join a b = join b a]) and {b associative}
    ([join (join a b) c = join a (join b c)]) — together the Strong Eventual
    Consistency guarantee. Two divergent replicas that exchange states must also
    {b converge} to the same value.

    The design calls for property tests; this switch ships no QCheck, so — as in
    [merge_test] / [coalesce_test] — we run a {b seeded} generator loop (the
    shared {!Test_util.lcg_next} LCG, seed [0x5eed]) with plain [Printf] checks.
    Every check reads a value the CRDT actually computes (a [value]/[mem]/joined
    state), so the tagged P3 mutations flip it red:
    - PN [join] using [p + q] instead of [max p q] breaks re-merge idempotence;
    - OR-Set [join] letting removes win breaks the concurrent add|remove check;
    - LWW dropping the replica from the tie-break, or using a non-strict stamp
      compare, breaks equal-stamp commutativity. *)

open Tea_core

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    incr failures)

let cases = 500

(* Run [law] over [cases] seeded inputs; print the first counterexample index so
   a failure is reproducible under the fixed seed. *)
let prop name (law : unit -> bool) =
  let rec loop i =
    if i >= cases then check name true
    else if law () then loop (i + 1)
    else (
      Printf.printf "FAIL - %s (counterexample at seeded case #%d)\n%!" name i;
      incr failures)
  in
  loop 0

(* --- seeded randomness ---------------------------------------------------- *)

let lcg = ref 0x5eedL

let rand_int n =
  let bits = Int64.shift_right_logical (Test_util.lcg_next lcg) 33 in
  Int64.to_int (Int64.rem (Int64.logand bits 0x3FFFFFFFL) (Int64.of_int n))

(* --- CRDT modules under test ---------------------------------------------- *)

module Pn = Crdt.Pn_counter

module OS = Crdt.Or_set (struct
  type t = string

  let compare = String.compare
  let t = Repr.string
end)

module LW = Crdt.Lww (struct
  type t = string

  let t = Repr.string
end)

(* --- replica / dot construction ------------------------------------------- *)

let rpool = [| "ra"; "rb"; "rc" |]
let epool = [| "x"; "y"; "z" |]

let rreplica i = Crdt.Replica.v (Prim.Session_id.v rpool.(i mod Array.length rpool))

(* A stamp with a chosen value: a fresh clock whose wall source reads [n] mints
   [n] as its first stamp (the [max (now ()) (last + 1)] contract). *)
let stamp_of (n : int64) : Clock.stamp = Clock.next (Clock.create ~now:(fun () -> n))

(* A dot with a SMALL, deliberately-colliding stamp — so LWW's [(stamp,
   replica)] tie-break is exercised, not bypassed. *)
let dot (n : int) (r : Crdt.Replica.t) : Crdt.Dot.t = Crdt.Dot.v (stamp_of (Int64.of_int n)) r

(* A globally-unique dot for OR-Set adds: an add must carry a distinct dot, so
   the stamp comes from a strictly increasing counter. *)
let stamp_ctr = ref 0L

let fresh_dot () : Crdt.Dot.t =
  let s = !stamp_ctr in
  stamp_ctr := Int64.add s 1L;
  dot (Int64.to_int s) (rreplica (Int64.to_int s))

(* --- generators ----------------------------------------------------------- *)

let gen_pn () : Pn.state =
  let k = 1 + rand_int 6 in
  List.fold_left
    (fun s (_ : int) ->
      let r = rreplica (rand_int 3) in
      if rand_int 2 = 0 then Pn.inc r s else Pn.dec r s)
    Pn.bottom
    (List.init k (fun i -> i))

let gen_orset () : OS.state =
  let k = rand_int 6 in
  List.fold_left
    (fun s (_ : int) ->
      let e = epool.(rand_int 3) in
      if rand_int 2 = 0 then OS.add (fresh_dot ()) e s else OS.remove e s)
    OS.bottom
    (List.init k (fun i -> i))

(* A dot maps to exactly one write, so the value is DERIVED from the dot: two
   states that happen to share a full dot must share a value, or [join] could not
   be commutative at a genuine tie. Colliding stamps across replicas still stress
   the [(stamp, replica)] tie-break. *)
let gen_lww () : LW.state =
  let k = rand_int 4 in
  List.fold_left
    (fun s (_ : int) ->
      let st = rand_int 3 and ri = rand_int 3 in
      LW.set (dot st (rreplica ri)) (Printf.sprintf "%d-%d" st ri) s)
    (LW.bottom "0")
    (List.init k (fun i -> i))

(* --- PN-counter ----------------------------------------------------------- *)

let () =
  prop "pn: idempotent (join x x = x)" (fun () ->
      let x = gen_pn () in
      Pn.equal (Pn.join x x) x);
  prop "pn: commutative" (fun () ->
      let a = gen_pn () and b = gen_pn () in
      Pn.equal (Pn.join a b) (Pn.join b a));
  prop "pn: associative" (fun () ->
      let a = gen_pn () and b = gen_pn () and c = gen_pn () in
      Pn.equal (Pn.join (Pn.join a b) c) (Pn.join a (Pn.join b c)));
  prop "pn: divergent replicas converge on the same value after exchange" (fun () ->
      let a = gen_pn () and b = gen_pn () in
      Pn.value (Pn.join a b) = Pn.value (Pn.join b a));

  let ra = rreplica 0 and rb = rreplica 1 in
  check "pn: re-merge does not double-count (join a a keeps value 1)"
    (Pn.value (Pn.join (Pn.inc ra Pn.bottom) (Pn.inc ra Pn.bottom)) = 1);
  check "pn: concurrent increments on distinct replicas sum to 2"
    (Pn.value (Pn.join (Pn.inc ra Pn.bottom) (Pn.inc rb Pn.bottom)) = 2);
  check "pn: a decrement is a negative unit"
    (Pn.value (Pn.dec ra Pn.bottom) = -1);
  check "pn: reset charges the current value back, zeroing it"
    (Pn.value (Pn.reset ra (Pn.inc ra (Pn.inc ra (Pn.inc ra Pn.bottom)))) = 0)

(* --- OR-Set (observed-remove, add-wins) ----------------------------------- *)

let () =
  prop "or_set: idempotent (join x x = x)" (fun () ->
      let x = gen_orset () in
      OS.equal (OS.join x x) x);
  prop "or_set: commutative" (fun () ->
      let a = gen_orset () and b = gen_orset () in
      OS.equal (OS.join a b) (OS.join b a));
  prop "or_set: associative" (fun () ->
      let a = gen_orset () and b = gen_orset () and c = gen_orset () in
      OS.equal (OS.join (OS.join a b) c) (OS.join a (OS.join b c)));
  prop "or_set: divergent replicas converge on the same set after exchange" (fun () ->
      let a = gen_orset () and b = gen_orset () in
      OS.value (OS.join a b) = OS.value (OS.join b a));

  let d1 = fresh_dot () and d2 = fresh_dot () in
  (* A observed [x] (dot d1) and removed it; B concurrently re-adds [x] (dot d2,
     which A never observed). Add-wins: the join keeps [x]. *)
  let a = OS.remove "x" (OS.add d1 "x" OS.bottom) in
  let b = OS.add d2 "x" OS.bottom in
  check "or_set: concurrent add|remove resolves add-wins"
    (OS.mem "x" (OS.join a b) && OS.value (OS.join a b) = [ "x" ]);
  check "or_set: add / remove / re-add leaves the element present"
    (let s = OS.add d2 "x" (OS.remove "x" (OS.add d1 "x" OS.bottom)) in
     OS.mem "x" s && OS.value s = [ "x" ]);
  check "or_set: removing the only observed add clears the element"
    (not (OS.mem "y" (OS.remove "y" (OS.add (fresh_dot ()) "y" OS.bottom))));
  check "or_set: concurrent adds of distinct elements both survive"
    (OS.value (OS.join (OS.add (fresh_dot ()) "y" OS.bottom) (OS.add (fresh_dot ()) "z" OS.bottom))
    = [ "y"; "z" ])

(* --- LWW-Register --------------------------------------------------------- *)

let () =
  prop "lww: idempotent (join x x = x)" (fun () ->
      let x = gen_lww () in
      LW.equal (LW.join x x) x);
  prop "lww: commutative (colliding stamps stress the tie-break)" (fun () ->
      let a = gen_lww () and b = gen_lww () in
      LW.equal (LW.join a b) (LW.join b a));
  prop "lww: associative" (fun () ->
      let a = gen_lww () and b = gen_lww () and c = gen_lww () in
      LW.equal (LW.join (LW.join a b) c) (LW.join a (LW.join b c)));

  let da = dot 1 (rreplica 0) and db = dot 1 (rreplica 1) in
  let a = LW.set da "A" (LW.bottom "0") and b = LW.set db "B" (LW.bottom "0") in
  check "lww: equal-stamp tie-break is order-independent (both joins agree)"
    (LW.equal (LW.join a b) (LW.join b a) && LW.value (LW.join a b) = LW.value (LW.join b a));
  check "lww: equal stamp -> the higher replica id wins the tie-break"
    (LW.value (LW.join a b) = "B" && LW.value (LW.join b a) = "B");

  let lo = LW.set (dot 1 (rreplica 0)) "lo" (LW.bottom "0") in
  let hi = LW.set (dot 2 (rreplica 0)) "hi" (LW.bottom "0") in
  check "lww: the higher stamp wins regardless of merge order"
    (LW.value (LW.join lo hi) = "hi" && LW.value (LW.join hi lo) = "hi");
  check "lww: any write dominates the untouched bottom"
    (LW.value (LW.join (LW.bottom "0") hi) = "hi" && LW.value (LW.join hi (LW.bottom "0")) = "hi")

let () =
  if !failures = 0 then
    Printf.printf "\nAll CvRDT join laws hold (SEC: idempotent, commutative, associative, convergent).\n%!"
  else (
    Printf.printf "\n%d CRDT check(s) FAILED.\n%!" !failures;
    exit 1)
