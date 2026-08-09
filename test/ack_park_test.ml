(** The duplicate-ack parking registry in isolation (roadmap step 17, D22):
    the pure row algebra whose end-to-end story [cancel_test]'s s8-s14
    ladders drive through live sockets.

    - A row opens at [register] and is exactly what [find] returns until the
      one [settle] that both wakes every waiter and removes it.
    - Rows are per-(replica, tab, seq): settling one key never perturbs a
      sibling seq's open row, and [parked_count] counts only its own tab.
    - [settle] on an absent key is a no-op, not a failure: an attempt no
      duplicate ever raced still settles.
    - Every [find] before the settle shares ONE settlement promise, and the
      promise resolves to exactly the settled outcome.
    - A second [register] over a standing row supersedes it: the old row's
      waiters wake [Released], the count does not double, and the old
      attempt's late [settle] - a stale {!Park.handle} - reaches nothing.
    - Both mutators write the map strictly before they wake: a waiter woken
      at callback depth zero re-enters the registry inline, and its writes
      must survive (no stale-snapshot clobber). *)

module Park = Tea_server.Ack_park
module Msg_seq = Tea_core.Prim.Msg_seq
module Tab_id = Tea_core.Prim.Tab_id

(** One assertion: TAP-ish line per check, exit nonzero on the first failure. *)
let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

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

(** A replica from a session-id name, the same derivation the server uses. *)
let replica name = Tea_core.Crdt.Replica.v (Tea_core.Prim.Session_id.v name)

(** A tab id from a 16-byte seed, through the same [of_bytes] mint the browser
    runtime uses. *)
let tab (n : int) : Tab_id.t =
  must "tab id mint refused a valid seed"
    (Tab_id.of_bytes (List.init 16 (fun i -> (n + i) land 0xff)))

(** A sequence number the mint accepted, or a loud setup failure. *)
let seq (n : int) : Msg_seq.t =
  must (Printf.sprintf "Msg_seq.of_int refused %d" n) (Msg_seq.of_int n)

(** Resolved to exactly the expected outcome: [~landed:true] demands [Landed],
    [~landed:false] demands [Released]; a pending or rejected promise is
    neither. *)
let resolved_to (p : Park.outcome Lwt.t) ~(landed : bool) : bool =
  match Lwt.state p with
  | Lwt.Return o ->
    (match o with
     | Park.Landed -> landed
     | Park.Released -> not landed)
  | Lwt.Fail (_ : exn) -> false
  | Lwt.Sleep -> false

let () =
  let t = Park.create () in
  let r = replica "parkone" in
  let r2 = replica "parktwo" in
  let tb = tab 1 in
  let tb2 = tab 2 in
  check "a fresh table holds no row and counts zero"
    (Option.is_none (Park.find t ~replica:r ~tab:tb ~seq:(seq 1))
    && Park.parked_count t ~replica:r ~tab:tb = 0);
  let h1 = Park.register t ~replica:r ~tab:tb ~seq:(seq 1) in
  let p1 =
    must "find after register" (Park.find t ~replica:r ~tab:tb ~seq:(seq 1))
  in
  check "register opens a row: find returns a still-pending settlement"
    (Lwt.is_sleeping p1);
  let h2 = Park.register t ~replica:r ~tab:tb ~seq:(seq 2) in
  check "parked_count counts rows under exactly its own (replica, tab)"
    (Park.parked_count t ~replica:r ~tab:tb = 2
    && Park.parked_count t ~replica:r ~tab:tb2 = 0
    && Park.parked_count t ~replica:r2 ~tab:tb = 0);
  let p2 = must "find seq2" (Park.find t ~replica:r ~tab:tb ~seq:(seq 2)) in
  Park.settle t ~replica:r ~tab:tb ~seq:(seq 2) ~handle:h2
    ~outcome:Park.Released;
  check
    "distinct seqs hold independent rows: settling one leaves the sibling \
     open"
    (Option.is_none (Park.find t ~replica:r ~tab:tb ~seq:(seq 2))
    && Option.is_some (Park.find t ~replica:r ~tab:tb ~seq:(seq 1))
    && Lwt.is_sleeping p1
    && Park.parked_count t ~replica:r ~tab:tb = 1);
  check "the settled promise resolved to Released for its waiters"
    (resolved_to p2 ~landed:false);
  let p1b =
    must "a second find shares the settlement"
      (Park.find t ~replica:r ~tab:tb ~seq:(seq 1))
  in
  Park.settle t ~replica:r ~tab:tb ~seq:(seq 1) ~handle:h1
    ~outcome:Park.Landed;
  check "settle Landed wakes EVERY waiter with Landed and removes the row"
    (resolved_to p1 ~landed:true
    && resolved_to p1b ~landed:true
    && Option.is_none (Park.find t ~replica:r ~tab:tb ~seq:(seq 1)));
  check "the emptied sub-maps are pruned back to a zero count"
    (Park.parked_count t ~replica:r ~tab:tb = 0);
  Park.settle t ~replica:r ~tab:tb ~seq:(seq 9) ~handle:h1
    ~outcome:Park.Landed;
  check "settle on an absent key is a no-op (an unraced attempt still settles)"
    (Park.parked_count t ~replica:r ~tab:tb = 0);
  let h3 = Park.register t ~replica:r ~tab:tb ~seq:(seq 3) in
  let p3 =
    must "find the first attempt's row"
      (Park.find t ~replica:r ~tab:tb ~seq:(seq 3))
  in
  let h3b = Park.register t ~replica:r ~tab:tb ~seq:(seq 3) in
  let p3b =
    must "find the superseding attempt's row"
      (Park.find t ~replica:r ~tab:tb ~seq:(seq 3))
  in
  check
    "a second register over a standing row supersedes it: the old waiters \
     wake Released, the new row is open, the count does not double"
    (resolved_to p3 ~landed:false
    && Lwt.is_sleeping p3b
    && Park.parked_count t ~replica:r ~tab:tb = 1);
  Park.settle t ~replica:r ~tab:tb ~seq:(seq 3) ~handle:h3
    ~outcome:Park.Landed;
  check
    "the superseded attempt's late settle is a no-op: its stale handle \
     reaches nothing and the successor row stays open"
    (Lwt.is_sleeping p3b && Park.parked_count t ~replica:r ~tab:tb = 1);
  Park.settle t ~replica:r ~tab:tb ~seq:(seq 3) ~handle:h3b
    ~outcome:Park.Landed;
  check "the superseding attempt still settles its own row"
    (resolved_to p3b ~landed:true
    && Park.parked_count t ~replica:r ~tab:tb = 0);
  let h4 = Park.register t ~replica:r ~tab:tb ~seq:(seq 4) in
  let p4 =
    must "find seq4" (Park.find t ~replica:r ~tab:tb ~seq:(seq 4))
  in
  let reentered = ref false in
  let (_ : unit Lwt.t) =
    Lwt.map
      (fun (_ : Park.outcome) ->
        let (_ : Park.handle) =
          Park.register t ~replica:r ~tab:tb ~seq:(seq 5)
        in
        reentered := true)
      p4
  in
  Park.settle t ~replica:r ~tab:tb ~seq:(seq 4) ~handle:h4
    ~outcome:Park.Landed;
  check
    "a waiter woken at depth zero re-enters the registry inline and its row \
     survives the settle (no stale-snapshot clobber)"
    (!reentered
    && Option.is_some (Park.find t ~replica:r ~tab:tb ~seq:(seq 5))
    && Park.parked_count t ~replica:r ~tab:tb = 1);
  print_endline "ack_park_test: all checks passed"
