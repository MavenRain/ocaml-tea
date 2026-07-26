(** The merge policy, rebuilt for state-based CRDTs (roadmap step 8, D1). The
    step-4 three-way combinator laws (reflexivity / identity / commutativity over
    an ancestor) are retired; a CvRDT merge takes {b no} ancestor, so the laws
    that matter are the Strong Eventual Consistency triple on the whole-model
    [join]: {b idempotent}, {b commutative}, {b associative}. [crdt_test] proves
    them for each leaf CRDT; this suite proves the {!Tea_core.Crdt.record}
    combinator preserves them when it folds several field-joins into one model
    join, and pins the {!Tea_core.Merge_spec.Crdt_join} wiring the apps use.

    Seeded generator loop + plain [Printf], matching the repo idiom; every check
    reads a joined value the code computes, so the P3 merge mutations (a counter
    app registering [Last_write_wins] instead of the PN join, a [record] that
    drops a field) flip it red. *)

open Tea_core

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    incr failures)

let cases = 500

let prop name (law : unit -> bool) =
  let rec loop i =
    if i >= cases then check name true
    else if law () then loop (i + 1)
    else (
      Printf.printf "FAIL - %s (counterexample at seeded case #%d)\n%!" name i;
      incr failures)
  in
  loop 0

(* --- seeded randomness (shared LCG, seed 0x5eed) -------------------------- *)

let lcg = ref 0x5eedL

let rand_int n =
  let bits = Int64.shift_right_logical (Test_util.lcg_next lcg) 33 in
  Int64.to_int (Int64.rem (Int64.logand bits 0x3FFFFFFFL) (Int64.of_int n))

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

let rpool = [| "ra"; "rb"; "rc" |]
let rreplica i = Crdt.Replica.v (Prim.Session_id.v rpool.(i mod Array.length rpool))
let stamp_of (n : int64) : Clock.stamp = Clock.next (Clock.create ~now:(fun () -> n))
let dot (n : int) (r : Crdt.Replica.t) : Crdt.Dot.t = Crdt.Dot.v (stamp_of (Int64.of_int n)) r
let stamp_ctr = ref 0L

let fresh_dot () : Crdt.Dot.t =
  let s = !stamp_ctr in
  stamp_ctr := Int64.add s 1L;
  dot (Int64.to_int s) (rreplica (Int64.to_int s))

(* --- a three-CRDT record, joined field-by-field --------------------------- *)

type doc =
  { likes : Pn.state
  ; title : LW.state
  ; tags : OS.state
  }

let doc_join : doc -> doc -> doc =
  Crdt.record
    [ Crdt.field ~get:(fun d -> d.likes) ~set:(fun v d -> { d with likes = v }) ~join:Pn.join
    ; Crdt.field ~get:(fun d -> d.title) ~set:(fun v d -> { d with title = v }) ~join:LW.join
    ; Crdt.field ~get:(fun d -> d.tags) ~set:(fun v d -> { d with tags = v }) ~join:OS.join
    ]

let doc_eq (a : doc) (b : doc) : bool =
  Pn.equal a.likes b.likes && LW.equal a.title b.title && OS.equal a.tags b.tags

let gen_doc () : doc =
  let likes =
    List.fold_left
      (fun s (_ : int) ->
        let r = rreplica (rand_int 3) in
        if rand_int 2 = 0 then Pn.inc r s else Pn.dec r s)
      Pn.bottom
      (List.init (rand_int 5) (fun i -> i))
  in
  let title =
    List.fold_left
      (fun s (_ : int) ->
        let st = rand_int 3 and ri = rand_int 3 in
        (* value derived from the dot: a dot maps to one write *)
        LW.set (dot st (rreplica ri)) (Printf.sprintf "%d-%d" st ri) s)
      (LW.bottom "0")
      (List.init (rand_int 3) (fun i -> i))
  in
  let tags =
    List.fold_left
      (fun s (_ : int) ->
        let e = [| "x"; "y"; "z" |].(rand_int 3) in
        if rand_int 2 = 0 then OS.add (fresh_dot ()) e s else OS.remove e s)
      OS.bottom
      (List.init (rand_int 4) (fun i -> i))
  in
  { likes; title; tags }

(* --- SEC laws for the composite record join ------------------------------- *)

let () =
  prop "record join: idempotent (join x x = x)" (fun () ->
      let x = gen_doc () in
      doc_eq (doc_join x x) x);
  prop "record join: commutative" (fun () ->
      let a = gen_doc () and b = gen_doc () in
      doc_eq (doc_join a b) (doc_join b a));
  prop "record join: associative" (fun () ->
      let a = gen_doc () and b = gen_doc () and c = gen_doc () in
      doc_eq (doc_join (doc_join a b) c) (doc_join a (doc_join b c)));

  let ra = rreplica 0 and rb = rreplica 1 in
  let a =
    { likes = Pn.inc ra Pn.bottom
    ; title = LW.set (dot 1 ra) "a" (LW.bottom "0")
    ; tags = OS.add (fresh_dot ()) "p" OS.bottom
    }
  in
  let b =
    { likes = Pn.inc rb Pn.bottom
    ; title = LW.set (dot 2 rb) "b" (LW.bottom "0")
    ; tags = OS.add (fresh_dot ()) "q" OS.bottom
    }
  in
  let m = doc_join a b in
  check "record join reconciles every field (likes sum, higher-stamp title, tag union)"
    (Pn.value m.likes = 2 && LW.value m.title = "b" && OS.value m.tags = [ "p"; "q" ])

(* --- Merge_spec wiring: the ctor is exhaustive and the apps register joins - *)

(* Classify a merge policy without a wildcard arm (locally-abstract [m] spells
   the [Three_way] payload's ignored function type). *)
let join_of (type m) (merge : m Merge_spec.t) : (m -> m -> m) option =
  match merge with
  | Merge_spec.Crdt_join j -> Some j
  | Merge_spec.Last_write_wins -> None
  | Merge_spec.Three_way (_ : ancestor:m option -> ours:m -> theirs:m -> (m, string) result) -> None

let () =
  (* [Option.fold]'s [~none] is eager, so compute the bool first, then [check]. *)
  let smart_ok =
    Option.fold (join_of (Merge_spec.crdt_join doc_join)) ~none:false ~some:(fun j ->
        let a = gen_doc () and b = gen_doc () in
        doc_eq (j a b) (doc_join a b))
  in
  check "crdt_join smart ctor produces a Crdt_join wrapping exactly the supplied join" smart_ok;

  (* The counter app must register the PN join, not last-writer-wins: two
     replicas that each incremented once sum to 2, and re-merging is idempotent
     (still 1). [Last_write_wins] would land in the [None] arm (false). *)
  let ra = rreplica 0 and rb = rreplica 1 in
  let ca = { Counter_app.App.count = Pn.inc ra Pn.bottom } in
  let cb = { Counter_app.App.count = Pn.inc rb Pn.bottom } in
  let counter_ok =
    Option.fold (join_of Counter_app.App.merge) ~none:false ~some:(fun j ->
        Counter_app.App.value (j ca cb) = 2 && Counter_app.App.value (j ca ca) = 1)
  in
  check "counter app registers a Crdt_join summing concurrent increments (idempotent)" counter_ok

let () =
  if !failures = 0 then
    Printf.printf "\nCvRDT merge policy holds (record join SEC laws + Crdt_join wiring).\n%!"
  else (
    Printf.printf "\n%d merge check(s) FAILED.\n%!" !failures;
    exit 1)
