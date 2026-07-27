(** The server's replay guard (roadmap step 10, D15).

    Driven directly and purely — no Lwt, no store, no socket. This is the only
    place the two-tabs-on-one-replica configuration can be exercised
    deterministically in process: the browser can show its {i consequence} (a
    dropped click) but not the decision that caused it.

    The load-bearing asymmetry, stated once here and relied on throughout: a
    false [Duplicate] is a silent permanent loss, while a false [Fresh] is one
    visible, convergent double count. Every bound in this module therefore
    degrades toward accepting. *)

module Guard = Tea_server.Replay_guard
module Msg_seq = Tea_core.Prim.Msg_seq
module Tab_id = Tea_core.Prim.Tab_id

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

let replica name = Tea_core.Crdt.Replica.v (Tea_core.Prim.Session_id.v name)

(* [Option.fold]'s [~none:] is EAGER, so a failure branch written as a value
   runs on every call and this file would exit before its first check. Both
   branches here are closures and the application is the only thing that
   chooses, which is what makes a loud refusal safe. *)
let must (what : string) (o : 'a option) : 'a =
  Option.fold
    ~none:(fun () ->
      Printf.printf "FAIL - test setup: %s\n%!" what;
      exit 1)
    ~some:(fun x () -> x)
    o ()

(* A tab id from a 16-byte seed, through the same [of_bytes] mint the browser
   runtime uses — so a change to the mint's arity or range breaks this too. *)
let tab (n : int) : Tab_id.t =
  must "tab id mint refused a valid seed"
    (Tab_id.of_bytes (List.init 16 (fun i -> (n + i) land 0xff)))

let seq (n : int) : Msg_seq.t =
  must (Printf.sprintf "Msg_seq.of_int refused %d" n) (Msg_seq.of_int n)

let bound (n : int) : Guard.Bound.t =
  must (Printf.sprintf "Bound.of_int refused %d" n) (Guard.Bound.of_int n)

let is_fresh (v : Guard.verdict) ~(at : int) : bool =
  match v with
  | Guard.Fresh n -> Int.equal (Msg_seq.to_int n) at
  | Guard.Duplicate (_ : Msg_seq.t) -> false
  | Guard.Gapped -> false

let is_duplicate (v : Guard.verdict) ~(high : int) : bool =
  match v with
  | Guard.Duplicate n -> Int.equal (Msg_seq.to_int n) high
  | Guard.Fresh (_ : Msg_seq.t) -> false
  | Guard.Gapped -> false

let is_gapped (v : Guard.verdict) : bool =
  match v with
  | Guard.Gapped -> true
  | Guard.Fresh (_ : Msg_seq.t) -> false
  | Guard.Duplicate (_ : Msg_seq.t) -> false

let fresh_guard () = Guard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs

(* --- 1. The core decision ------------------------------------------------- *)

let () =
  let g = fresh_guard () in
  let r = replica "alpha" in
  let g, v1 = Guard.take g ~replica:r ~tab:(tab 1) ~seq:(seq 1) in
  check "a fresh guard admits the first seq" (is_fresh v1 ~at:1);
  let g, v2 = Guard.take g ~replica:r ~tab:(tab 1) ~seq:(seq 1) in
  check "the same seq twice is a duplicate carrying the high water"
    (is_duplicate v2 ~high:1);
  let (_ : Guard.t), v3 = Guard.take g ~replica:r ~tab:(tab 1) ~seq:(seq 2) in
  check "the next seq in order is fresh" (is_fresh v3 ~at:2)

(* --- 2. The trap: two tabs on one replica ---------------------------------

   One Dream session cookie is one session, one branch and ONE replica; both
   tabs number their own messages from 1. A guard keyed on the session alone
   answers [Duplicate] to the second tab's first edit and drops it forever. *)

let () =
  let g = fresh_guard () in
  let r = replica "shared-cookie" in
  let g, a1 = Guard.take g ~replica:r ~tab:(tab 1) ~seq:(seq 1) in
  let (_ : Guard.t), b1 = Guard.take g ~replica:r ~tab:(tab 2) ~seq:(seq 1) in
  check "two tabs on one replica each land their own seq 1"
    (is_fresh a1 ~at:1 && is_fresh b1 ~at:1)

(* --- 3. Gaps ------------------------------------------------------------- *)

let () =
  let g = fresh_guard () in
  let r = replica "gappy" in
  let t = tab 3 in
  let g, (_ : Guard.verdict) = Guard.take g ~replica:r ~tab:t ~seq:(seq 1) in
  let g, v = Guard.take g ~replica:r ~tab:t ~seq:(seq 7) in
  check "a seq beyond the high water + 1 is gapped and does not advance it"
    (is_gapped v
    && Guard.high_water g ~replica:r ~tab:t
       |> Option.fold ~none:false ~some:(fun n -> Int.equal (Msg_seq.to_int n) 1));
  let (_ : Guard.t), v' = Guard.take g ~replica:r ~tab:t ~seq:(seq 2) in
  check "a gapped seq still lands when it arrives in order" (is_fresh v' ~at:2)

(* --- 4. Absence accepts: the direction every degradation falls ------------ *)

let () =
  let g = fresh_guard () in
  let r = replica "evicted" in
  let (_ : Guard.t), v = Guard.take g ~replica:r ~tab:(tab 4) ~seq:(seq 41) in
  check "an absent entry accepts any seq, so an evicted tab resumes from its frontier"
    (is_fresh v ~at:41)

(* --- 5. The bounds and their eviction order ------------------------------- *)

let () =
  let g = Guard.v ~sessions:(bound 2) ~tabs:(bound 2) in
  let r = replica "crowded" in
  (* Tab 1 first, then tab 2, then tab 3: tab 1 is the least recently taken. *)
  let g, (_ : Guard.verdict) = Guard.take g ~replica:r ~tab:(tab 1) ~seq:(seq 1) in
  let g, (_ : Guard.verdict) = Guard.take g ~replica:r ~tab:(tab 2) ~seq:(seq 1) in
  let g, (_ : Guard.verdict) = Guard.take g ~replica:r ~tab:(tab 3) ~seq:(seq 1) in
  check "the tab bound evicts the least-recently-taken tab"
    (Int.equal (Guard.tabs g ~replica:r) 2
    && Option.is_none (Guard.high_water g ~replica:r ~tab:(tab 1))
    && Option.is_some (Guard.high_water g ~replica:r ~tab:(tab 3)));
  (* Now keep tab 2 warm with a DUPLICATE — a retry is exactly the traffic a
     tab whose entry matters produces — and add a fourth tab. The victim must
     be tab 3, not the tab that is actively retrying. *)
  let g, (_ : Guard.verdict) = Guard.take g ~replica:r ~tab:(tab 2) ~seq:(seq 1) in
  let g, (_ : Guard.verdict) = Guard.take g ~replica:r ~tab:(tab 4) ~seq:(seq 1) in
  check "a tab kept warm by duplicates is not the eviction victim"
    (Option.is_some (Guard.high_water g ~replica:r ~tab:(tab 2))
    && Option.is_none (Guard.high_water g ~replica:r ~tab:(tab 3)))

let () =
  let g = Guard.v ~sessions:(bound 2) ~tabs:(bound 4) in
  let g, (_ : Guard.verdict) = Guard.take g ~replica:(replica "s1") ~tab:(tab 1) ~seq:(seq 1) in
  let g, (_ : Guard.verdict) = Guard.take g ~replica:(replica "s2") ~tab:(tab 1) ~seq:(seq 1) in
  let g, (_ : Guard.verdict) = Guard.take g ~replica:(replica "s3") ~tab:(tab 1) ~seq:(seq 1) in
  check "the session bound evicts the least-recently-taken replica"
    (Int.equal (Guard.sessions g) 2
    && Option.is_none (Guard.high_water g ~replica:(replica "s1") ~tab:(tab 1))
    && Option.is_some (Guard.high_water g ~replica:(replica "s3") ~tab:(tab 1)))

(* A seeded LCG walk over random (replica, tab, seq) triples: the bounds are a
   memory claim, so they are asserted after every single take, not at the end. *)
let () =
  let sessions = 3 and tabs = 3 in
  let g = Guard.v ~sessions:(bound sessions) ~tabs:(bound tabs) in
  let state = ref 20260726L in
  let final, ok =
    List.init 400 Fun.id
    |> List.fold_left
         (fun ((g : Guard.t), (ok : bool)) (_ : int) ->
           (* [land max_int] not [abs]: [Int64.abs min_int] is still negative,
              and the truncation to [int] can land there. *)
           let s = Int64.to_int (Test_util.lcg_next state) land max_int in
           let r = replica (Printf.sprintf "r%d" (s mod 7)) in
           let g, (_ : Guard.verdict) =
             Guard.take g ~replica:r ~tab:(tab (s mod 5)) ~seq:(seq (1 + (s mod 3)))
           in
           ( g
           , ok
             && Guard.sessions g <= sessions
             && Guard.tabs g ~replica:r <= tabs ))
         (g, true)
  in
  check "the guard never exceeds either bound"
    (ok && Guard.sessions final <= sessions)

(* --- 6. forget, the reaper's obligation ----------------------------------- *)

let () =
  let g = fresh_guard () in
  let keep = replica "keep" and drop = replica "drop" in
  let g, (_ : Guard.verdict) = Guard.take g ~replica:drop ~tab:(tab 1) ~seq:(seq 1) in
  let g, (_ : Guard.verdict) = Guard.take g ~replica:keep ~tab:(tab 1) ~seq:(seq 1) in
  let g = Guard.forget g ~replica:drop in
  check "forget drops one replica's tabs and nothing else"
    (Option.is_none (Guard.high_water g ~replica:drop ~tab:(tab 1))
    && Option.is_some (Guard.high_water g ~replica:keep ~tab:(tab 1)))

(* --- 7. The newtypes the wire header is validated through ------------------ *)

let () =
  let err_is_wrong_length (r : (Tab_id.t, Tab_id.err) result) : bool =
    Result.fold r
      ~ok:(fun (_ : Tab_id.t) -> false)
      ~error:(fun (e : Tab_id.err) ->
        match e with
        | Tab_id.Wrong_length -> true
        | Tab_id.Invalid_char (_ : char) -> false)
  in
  let err_is_invalid_char (r : (Tab_id.t, Tab_id.err) result) : bool =
    Result.fold r
      ~ok:(fun (_ : Tab_id.t) -> false)
      ~error:(fun (e : Tab_id.err) ->
        match e with
        | Tab_id.Invalid_char (_ : char) -> true
        | Tab_id.Wrong_length -> false)
  in
  check "Tab_id.of_string rejects a wrong length and a non-hex char"
    (err_is_wrong_length (Tab_id.of_string "abc")
    && err_is_invalid_char (Tab_id.of_string (String.make 31 'a' ^ "Z"))
    && Result.is_ok (Tab_id.of_string (String.make 32 'a')));
  check "Tab_id.of_bytes refuses a wrong count or an out-of-range byte"
    (Option.is_none (Tab_id.of_bytes (List.init 15 (fun (_ : int) -> 0)))
    && Option.is_none (Tab_id.of_bytes (List.init 16 (fun (_ : int) -> 256)))
    && Option.is_some (Tab_id.of_bytes (List.init 16 (fun (_ : int) -> 255))));
  check "Msg_seq.of_int refuses zero and negatives"
    (Option.is_none (Msg_seq.of_int 0)
    && Option.is_none (Msg_seq.of_int (-1))
    && Option.is_some (Msg_seq.of_int 1));
  check "Msg_seq.next is None at the end of the seq space"
    (Option.is_none (Msg_seq.next (seq max_int))
    && Option.fold ~none:false
         ~some:(fun n -> Int.equal (Msg_seq.to_int n) 2)
         (Msg_seq.next (seq 1)))

(* --- 8. seed: adopting a high water this process never observed (D16) ------ *)

(* The pure half of durability. [seed] is the only way a fresh table can know
   about a sequence number some earlier process consumed, and it is the one
   direction in this module that can cause a LOSS, so its floor discipline is
   pinned here rather than left to the wiring that calls it. *)
let () =
  let r = replica "seed" in
  let t = tab 1 in
  let g = Guard.seed (fresh_guard ()) ~replica:r ~tab:t ~high:(seq 3) in
  check "seed on an empty table adopts the durable high water"
    (Option.fold ~none:false
       ~some:(fun h -> Int.equal (Msg_seq.to_int h) 3)
       (Guard.high_water g ~replica:r ~tab:t));
  let (_ : Guard.t), replayed = Guard.take g ~replica:r ~tab:t ~seq:(seq 3) in
  check "a seeded high water answers Duplicate to the seq a dead process consumed"
    (is_duplicate replayed ~high:3);
  let (_ : Guard.t), next = Guard.take g ~replica:r ~tab:t ~seq:(seq 4) in
  check "a seeded tab resumes in order at the seq after the durable one"
    (is_fresh next ~at:4)

let () =
  let r = replica "floor" in
  let t = tab 1 in
  (* The in-memory table is always at least as advanced as the durable record,
     because the durable write happens after the effect commits. A seed that
     would LOWER an entry is therefore stale by construction, and believing it
     would re-open a consumed sequence number. *)
  let g, (_ : Guard.verdict) = Guard.take (fresh_guard ()) ~replica:r ~tab:t ~seq:(seq 1) in
  let g, (_ : Guard.verdict) = Guard.take g ~replica:r ~tab:t ~seq:(seq 2) in
  let g = Guard.seed g ~replica:r ~tab:t ~high:(seq 1) in
  check "seed never lowers an entry the running process already advanced"
    (Option.fold ~none:false
       ~some:(fun h -> Int.equal (Msg_seq.to_int h) 2)
       (Guard.high_water g ~replica:r ~tab:t));
  let (_ : Guard.t), stale = Guard.take g ~replica:r ~tab:t ~seq:(seq 2) in
  check "a lowering seed does not re-open a consumed seq" (is_duplicate stale ~high:2);
  let g' = Guard.seed g ~replica:r ~tab:t ~high:(seq 5) in
  check "seed does raise an entry when the durable record is ahead"
    (Option.fold ~none:false
       ~some:(fun h -> Int.equal (Msg_seq.to_int h) 5)
       (Guard.high_water g' ~replica:r ~tab:t));
  (* Seeding one tab must not answer for its sibling: the whole reason the key is
     a pair is that two tabs on one cookie both start at one. *)
  let (_ : Guard.t), sibling = Guard.take g' ~replica:r ~tab:(tab 2) ~seq:(seq 1) in
  check "seeding one tab leaves its sibling's first seq fresh" (is_fresh sibling ~at:1)

let () =
  let r = replica "seedcell" in
  let t = tab 1 in
  let c = Guard.Cell.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs in
  Guard.Cell.seed c ~replica:r ~tab:t ~high:(seq 2);
  (* Two live sockets for one tab can both seed before either takes, so seeding
     twice must not admit the same sequence number twice. *)
  Guard.Cell.seed c ~replica:r ~tab:t ~high:(seq 2);
  let first = Guard.Cell.take c ~replica:r ~tab:t ~seq:(seq 3) in
  let second = Guard.Cell.take c ~replica:r ~tab:t ~seq:(seq 3) in
  check "Cell.seed is idempotent and take still admits a seq exactly once"
    (is_fresh first ~at:3 && is_duplicate second ~high:3)
