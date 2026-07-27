(** The two-layer replay guard (roadmap step 11, D16): the bounded in-memory
    Cell in front of a durable floor mirror, joined by a {!Tea_server.Guard_sink}.

    [replay_test.ml] pins the pure step-10 decision; this file pins what the
    durable layer adds and, just as importantly, what it must not change.

    - The floor algebra a journal folds into: [Advance] raises to the max and
      never lowers, [Forget] tombstones a whole replica, and [of_events] is
      exactly the left fold of [apply]. A replayed or out-of-order record that
      could lower a floor would re-open a consumed sequence number, which is
      the one thing the guard exists to prevent.
    - The restart story itself: a second life built only from the first life's
      recorded events answers [Duplicate] to a replayed seq at the durable
      high water. The same script over {!Tea_server.Guard_sink.null} answers
      [Fresh] again, which is step-10 (mem tier) behaviour unchanged and the
      proof that the restart test is not vacuous.
    - The mirror as what a Cell miss degrades to, seeded before the verdict,
      on both the restart miss and the LRU-eviction miss.
    - The failure direction: a failing append still raises the mirror
      ([persist]) and still scrubs the state ([forget]), so every degradation
      falls toward duplicating, never losing. *)

module Dg = Tea_server.Durable_guard
module Guard = Tea_server.Replay_guard
module Sink = Tea_server.Guard_sink
module Msg_seq = Tea_core.Prim.Msg_seq
module Tab_id = Tea_core.Prim.Tab_id

(** One assertion: TAP-ish line per check, exit nonzero on the first failure. *)
let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

(** A replica from a session-id name, the same derivation the server uses. *)
let replica name = Tea_core.Crdt.Replica.v (Tea_core.Prim.Session_id.v name)

(* [Option.fold]'s [~none:] is EAGER, so a failure branch written as a value
   would run on every call and this file would exit before its first check.
   Both branches here are closures and the application is the only thing that
   chooses, which is what makes a loud refusal safe. *)
let must (what : string) (o : 'a option) : 'a =
  Option.fold
    ~none:(fun () ->
      Printf.printf "FAIL - test setup: %s\n%!" what;
      exit 1)
    ~some:(fun x () -> x)
    o ()

(** A tab id from a 16-byte seed, through the same [of_bytes] mint the browser
    runtime uses, so a change to the mint's arity or range breaks this too. *)
let tab (n : int) : Tab_id.t =
  must "tab id mint refused a valid seed"
    (Tab_id.of_bytes (List.init 16 (fun i -> (n + i) land 0xff)))

(** A sequence number the mint accepted, or a loud setup failure. *)
let seq (n : int) : Msg_seq.t =
  must (Printf.sprintf "Msg_seq.of_int refused %d" n) (Msg_seq.of_int n)

(** A table bound the mint accepted, or a loud setup failure. *)
let bound (n : int) : Guard.Bound.t =
  must (Printf.sprintf "Bound.of_int refused %d" n) (Guard.Bound.of_int n)

(** [Fresh] carrying exactly [at]. *)
let is_fresh (v : Guard.verdict) ~(at : int) : bool =
  match v with
  | Guard.Fresh n -> Int.equal (Msg_seq.to_int n) at
  | Guard.Duplicate (_ : Msg_seq.t) -> false
  | Guard.Gapped -> false

(** [Duplicate] carrying exactly [high]. *)
let is_duplicate (v : Guard.verdict) ~(high : int) : bool =
  match v with
  | Guard.Duplicate n -> Int.equal (Msg_seq.to_int n) high
  | Guard.Fresh (_ : Msg_seq.t) -> false
  | Guard.Gapped -> false

(** [Gapped], and nothing else. *)
let is_gapped (v : Guard.verdict) : bool =
  match v with
  | Guard.Gapped -> true
  | Guard.Fresh (_ : Msg_seq.t) -> false
  | Guard.Duplicate (_ : Msg_seq.t) -> false

(** [Some] of exactly [at], for high waters and mirror floors alike. *)
let floor_is (o : Msg_seq.t option) ~(at : int) : bool =
  Option.fold ~none:false
    ~some:(fun (n : Msg_seq.t) -> Int.equal (Msg_seq.to_int n) at)
    o

(** [Floors.t] is abstract, so two floor maps are compared through their
    observable surface: total [cardinal] plus [find] on every key the test
    touched. *)
let floors_agree (a : Dg.Floors.t) (b : Dg.Floors.t)
    ~(keys : (Tea_core.Crdt.Replica.t * Tab_id.t) list) : bool =
  Int.equal (Dg.Floors.cardinal a) (Dg.Floors.cardinal b)
  && List.for_all
       (fun ((r : Tea_core.Crdt.Replica.t), (t : Tab_id.t)) ->
         Option.equal Int.equal
           (Option.map Msg_seq.to_int (Dg.Floors.find ~replica:r ~tab:t a))
           (Option.map Msg_seq.to_int (Dg.Floors.find ~replica:r ~tab:t b)))
       keys

(** The append outcome is [Error Io], with both [err] constructors spelled. *)
let is_io_error (r : (unit, Sink.err) result) : bool =
  Result.fold r
    ~ok:(fun () -> false)
    ~error:(fun (e : Sink.err) ->
      match e with
      | Sink.Io (_ : string) -> true
      | Sink.Sink_closed -> false)

(** A recorded event as a comparable tag, every constructor spelled, so the
    journal's order can be asserted with one list equality. *)
let event_tag (e : Sink.event) : string =
  match e with
  | Sink.Advance
      { replica = (_ : Tea_core.Crdt.Replica.t)
      ; tab = (_ : Tab_id.t)
      ; seq = s
      } -> Printf.sprintf "advance:%d" (Msg_seq.to_int s)
  | Sink.Forget { replica = (_ : Tea_core.Crdt.Replica.t) } -> "forget"

(** An [Advance] record, spelled once for the algebra tests. *)
let adv (r : Tea_core.Crdt.Replica.t) (t : Tab_id.t) (n : int) : Sink.event =
  Sink.Advance { replica = r; tab = t; seq = seq n }

(** The sink whose every append fails: the backend-down world, where the only
    acceptable degradation is the duplicate direction. *)
let failing_sink : Sink.t =
  { Sink.append = (fun (_ : Sink.event) -> Lwt.return (Error (Sink.Io "boom"))) }

(** A guard over the default bounds, for every test that is not about
    eviction. *)
let guard ~(sink : Sink.t) ~(floors : Dg.Floors.t) : Dg.t =
  Dg.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs ~sink ~floors

(* --- 1. The floor algebra a journal folds into ----------------------------- *)

let () =
  let r = replica "algebra" in
  let t = tab 1 in
  let f1 = Dg.Floors.apply Dg.Floors.empty (adv r t 5) in
  let f2 = Dg.Floors.apply f1 (adv r t 3) in
  check "a lower Advance never lowers a floor: the max wins"
    (floor_is (Dg.Floors.find ~replica:r ~tab:t f2) ~at:5);
  let f3 = Dg.Floors.apply f2 (adv r t 7) in
  check "a higher Advance raises the floor"
    (floor_is (Dg.Floors.find ~replica:r ~tab:t f3) ~at:7)

let () =
  let a = replica "forget-me" and b = replica "keep-me" in
  let f = Dg.Floors.of_events [ adv a (tab 1) 1; adv a (tab 2) 2; adv b (tab 1) 3 ] in
  check "of_events lands one floor per (replica, tab)"
    (Int.equal (Dg.Floors.cardinal f) 3);
  let f' = Dg.Floors.apply f (Sink.Forget { replica = a }) in
  check "Forget deletes every floor under the replica and nothing else"
    (Int.equal (Dg.Floors.cardinal f') 1
    && Option.is_none (Dg.Floors.find ~replica:a ~tab:(tab 1) f')
    && Option.is_none (Dg.Floors.find ~replica:a ~tab:(tab 2) f')
    && floor_is (Dg.Floors.find ~replica:b ~tab:(tab 1) f') ~at:3)

let () =
  (* [of_events] is contractually the left fold of [apply]; pin it against a
     record list that exercises raise, ignore-lower, tombstone and rebirth. *)
  let a = replica "fold-a" and b = replica "fold-b" in
  let evs =
    [ adv a (tab 1) 2
    ; adv b (tab 1) 1
    ; adv a (tab 1) 1
    ; Sink.Forget { replica = b }
    ; adv b (tab 2) 4
    ]
  in
  check "of_events equals the left fold of apply over the same records"
    (floors_agree
       (Dg.Floors.of_events evs)
       (List.fold_left Dg.Floors.apply Dg.Floors.empty evs)
       ~keys:[ (a, tab 1); (a, tab 2); (b, tab 1); (b, tab 2) ])

(* --- 2. The restart: exactly-once effect across a process boundary ---------

   Life 1 records through a memory sink; life 2 is a NEW guard, a NEW Cell,
   and nothing but the recorded journal folded into a mirror. The replayed
   frame must read Duplicate at the durable high water, in process. *)

let () =
  let r = replica "restart" in
  let t = tab 1 in
  let sink1, reader = Sink.memory () in
  let life1 = guard ~sink:sink1 ~floors:Dg.Floors.empty in
  let consume (n : int) : bool =
    is_fresh (Dg.take life1 ~replica:r ~tab:t ~seq:(seq n)) ~at:n
    && Result.is_ok (Lwt_main.run (Dg.persist life1 ~replica:r ~tab:t ~seq:(seq n)))
  in
  check "life 1 consumes and persists seqs 1..3" (List.for_all consume [ 1; 2; 3 ]);
  let life2 = guard ~sink:Sink.null ~floors:(Dg.Floors.of_events (reader ())) in
  check "the first frame after a restart reads Duplicate at the durable high water"
    (is_duplicate (Dg.take life2 ~replica:r ~tab:t ~seq:(seq 2)) ~high:3);
  check "the first unconsumed seq after a restart is fresh"
    (is_fresh (Dg.take life2 ~replica:r ~tab:t ~seq:(seq 4)) ~at:4)

(* --- 3. The null-sink converse: the mem tier is step 10 byte for byte ------

   The same script over Guard_sink.null with an empty mirror for life 2: the
   replayed seq reads Fresh again. That is the specified mem-tier behaviour
   AND the proof that section 2's Duplicate came from the journal, not from
   some hidden channel between the two lives. *)

let () =
  let r = replica "mem-tier" in
  let t = tab 1 in
  let life1 = guard ~sink:Sink.null ~floors:Dg.Floors.empty in
  let consume (n : int) : bool =
    is_fresh (Dg.take life1 ~replica:r ~tab:t ~seq:(seq n)) ~at:n
    && Result.is_ok (Lwt_main.run (Dg.persist life1 ~replica:r ~tab:t ~seq:(seq n)))
  in
  check "the null-sink life consumes and persists seqs 1..3 too"
    (List.for_all consume [ 1; 2; 3 ]);
  let life2 = guard ~sink:Sink.null ~floors:Dg.Floors.empty in
  check "over the null sink a restart forgets, so the replayed seq is fresh again"
    (is_fresh (Dg.take life2 ~replica:r ~tab:t ~seq:(seq 2)) ~at:2)

(* --- 4. A Cell miss asks the mirror BEFORE the verdict --------------------- *)

let () =
  let r = replica "seeded" in
  let t = tab 1 in
  let g = guard ~sink:Sink.null ~floors:(Dg.Floors.of_events [ adv r t 5 ]) in
  check "the mirror has the floor while the Cell has never heard of the key"
    (floor_is (Dg.Floors.find ~replica:r ~tab:t (Dg.floors g)) ~at:5
    && Option.is_none (Guard.high_water (Dg.snapshot g) ~replica:r ~tab:t));
  check "a first take at or below the floor is Duplicate at the mirror's water"
    (is_duplicate (Dg.take g ~replica:r ~tab:t ~seq:(seq 4)) ~high:5);
  check "the miss seeded the Cell: the snapshot now carries the floor"
    (floor_is (Guard.high_water (Dg.snapshot g) ~replica:r ~tab:t) ~at:5)

(* --- 5. An LRU eviction is rescued by the mirror ---------------------------

   The tab bound is 1, so taking tab 2 evicts tab 1: replay_test section 5
   pins the least-recently-taken policy, and with a bound of one there is
   exactly one possible victim, so the eviction here is deterministic. The
   mirror got tab 1's floor from persist, so the evicted key's consumed seq
   must still read Duplicate. *)

let () =
  let r = replica "evictee" in
  let sink, (_ : unit -> Sink.event list) = Sink.memory () in
  let g = Dg.v ~sessions:(bound 4) ~tabs:(bound 1) ~sink ~floors:Dg.Floors.empty in
  check "tab 1 lands its first seq and persists it"
    (is_fresh (Dg.take g ~replica:r ~tab:(tab 1) ~seq:(seq 1)) ~at:1
    && Result.is_ok (Lwt_main.run (Dg.persist g ~replica:r ~tab:(tab 1) ~seq:(seq 1))));
  check "taking tab 2 under a bound of one genuinely evicts tab 1 from the Cell"
    (is_fresh (Dg.take g ~replica:r ~tab:(tab 2) ~seq:(seq 1)) ~at:1
    && Option.is_none (Guard.high_water (Dg.snapshot g) ~replica:r ~tab:(tab 1)));
  check "the evicted tab's consumed seq still reads Duplicate: the mirror rescued it"
    (is_duplicate (Dg.take g ~replica:r ~tab:(tab 1) ~seq:(seq 1)) ~high:1)

(* --- 6. Gapped writes nothing, in either layer ----------------------------- *)

let () =
  let r = replica "gapped" in
  let t = tab 1 in
  let sink, reader = Sink.memory () in
  let g = guard ~sink ~floors:Dg.Floors.empty in
  check "the setup take and persist land"
    (is_fresh (Dg.take g ~replica:r ~tab:t ~seq:(seq 1)) ~at:1
    && Result.is_ok (Lwt_main.run (Dg.persist g ~replica:r ~tab:t ~seq:(seq 1))));
  check "a seq beyond high water + 1 is gapped"
    (is_gapped (Dg.take g ~replica:r ~tab:t ~seq:(seq 7)));
  (* The caller never persists a Gapped verdict, so with no persist call after
     it the Cell, the mirror and the journal must all still read exactly 1. *)
  check "Gapped raised neither the Cell's water, nor the mirror, nor the journal"
    (floor_is (Guard.high_water (Dg.snapshot g) ~replica:r ~tab:t) ~at:1
    && floor_is (Dg.Floors.find ~replica:r ~tab:t (Dg.floors g)) ~at:1
    && List.equal String.equal (List.map event_tag (reader ())) [ "advance:1" ])

(* --- 7. persist raises the mirror FIRST, even when the append fails --------

   The deliberate degradation: an Error from the sink means the record is not
   durable, but the mirror in THIS process is raised anyway, so the failure
   costs at worst a duplicate after the next restart, never a loss now. *)

let () =
  let r = replica "io-down" in
  let t = tab 1 in
  let g = guard ~sink:failing_sink ~floors:Dg.Floors.empty in
  check "the take is fresh before the backend has any say"
    (is_fresh (Dg.take g ~replica:r ~tab:t ~seq:(seq 1)) ~at:1);
  check "a failing append surfaces as Error Io"
    (is_io_error (Lwt_main.run (Dg.persist g ~replica:r ~tab:t ~seq:(seq 1))));
  check "the mirror was raised anyway: degradation falls toward Duplicate, not loss"
    (floor_is (Dg.Floors.find ~replica:r ~tab:t (Dg.floors g)) ~at:1)

(* --- 8. forget is tombstone-first and scrubs even on a failed append -------

   The caller is removing the branch either way, and a stale floor outliving
   its branch is the ONE loss-side path in the whole design, so the scrub must
   not be conditional on the append landing. *)

let () =
  let r = replica "scrubbed" in
  let t = tab 1 in
  let g = guard ~sink:failing_sink ~floors:Dg.Floors.empty in
  let (_ : Guard.verdict) = Dg.take g ~replica:r ~tab:t ~seq:(seq 1) in
  let (_ : (unit, Sink.err) result) =
    Lwt_main.run (Dg.persist g ~replica:r ~tab:t ~seq:(seq 1))
  in
  check "the failed-persist mirror floor is in place before the forget"
    (floor_is (Dg.Floors.find ~replica:r ~tab:t (Dg.floors g)) ~at:1);
  check "forget over a failing sink reports the Error"
    (is_io_error (Lwt_main.run (Dg.forget g ~replica:r)));
  check "yet the Cell and the mirror are scrubbed: a stale floor over a fresh branch is the loss path"
    (Option.is_none (Guard.high_water (Dg.snapshot g) ~replica:r ~tab:t)
    && Option.is_none (Dg.Floors.find ~replica:r ~tab:t (Dg.floors g)))

let () =
  let r = replica "tombstone" in
  let t = tab 1 in
  let sink, reader = Sink.memory () in
  let g = guard ~sink ~floors:Dg.Floors.empty in
  let (_ : Guard.verdict) = Dg.take g ~replica:r ~tab:t ~seq:(seq 1) in
  check "the memory-sink life persists then forgets, both Ok"
    (Result.is_ok (Lwt_main.run (Dg.persist g ~replica:r ~tab:t ~seq:(seq 1)))
    && Result.is_ok (Lwt_main.run (Dg.forget g ~replica:r)));
  check "the journal records Advance then Forget, and the tombstone is the last word"
    (List.equal String.equal (List.map event_tag (reader ())) [ "advance:1"; "forget" ])
