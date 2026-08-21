(** The browser-local persistence protocol in isolation (roadmap step 25,
    D25/R28): the checkpoint codec round-trips and refuses corruption, the
    queue rebuilds only from a well-formed record, the boot gate buffers,
    adopts, withholds and prunes exactly as ruled, and the Flow degrades to
    memory-only on every failure arm - all natively, against the honest fake,
    whose own honesty (async delivery, the auto-commit boundary) is pinned
    here too. *)

module App = Counter_app.App
module Store = Tea_client.Local_store
module LS = Store.Make (App)
module Flow = LS.Flow (Fake_idb)
module Delivery = Tea_client.Delivery
module Prim = Tea_core.Prim
module Msg_seq = Prim.Msg_seq
module Tab_id = Prim.Tab_id
module Replica = Tea_core.Crdt.Replica

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

(* Total literal helper: [of_int] only refuses n < 1, so the default is
   unreachable for the literals below. *)
let seq (n : int) : Msg_seq.t =
  Option.value (Msg_seq.of_int n) ~default:Msg_seq.one

let msg_label (m : App.msg) : string =
  match m with
  | App.Increment -> "i"
  | App.Decrement -> "d"
  | App.Reset -> "r"
  | App.Sync (_ : App.model) -> "s"

let labels (d : App.msg Delivery.t) : (int * string) list =
  List.map
    (fun ((n, m) : Msg_seq.t * App.msg) -> (Msg_seq.to_int n, msg_label m))
    (Delivery.unacked d)

let gate_labels (g : LS.gate) : (int * string) list =
  LS.delivery g |> Option.fold ~none:[] ~some:labels

let eq_entries (a : (int * string) list) (b : (int * string) list) : bool =
  List.equal
    (fun ((n1, l1) : int * string) ((n2, l2) : int * string) ->
      Int.equal n1 n2 && String.equal l1 l2)
    a b

let err_label (e : Store.idb_error) : string =
  match e with
  | Store.Unsupported -> "unsupported"
  | Store.Blocked -> "blocked"
  | Store.Version_error -> "version"
  | Store.Quota_exceeded -> "quota"
  | Store.Not_found -> "notfound"
  | Store.Other s -> "other:" ^ s

let rep_a = Replica.v (Prim.Session_id.v "alpha")
let rep_b = Replica.v (Prim.Session_id.v "beta")
let tab_a = Tab_id.of_draws (fun () -> 3)
let tab_fresh = Tab_id.of_draws (fun () -> 9)
let mint () : Tab_id.t = tab_fresh
let m1 : App.model = { App.count = App.Count.inc rep_a App.Count.bottom }

let record_a : LS.record =
  { LS.replica = rep_a
  ; tab = tab_a
  ; next = seq 3
  ; queue = [ (seq 1, App.Increment); (seq 2, App.Decrement) ]
  ; clock_floor = 7L
  ; model = m1
  ; written_at_ms = 5.
  }

let () =
  (* --- codec ------------------------------------------------------------ *)
  check "codec-roundtrip"
    ((LS.of_json (LS.to_json record_a)
     |> Result.fold
          ~ok:(fun (r : LS.record) ->
            (* The queue length is asserted on the DECODED value: a string
               compare alone would bless an encoder that drops the queue on
               both sides of the trip. *)
            String.equal (LS.to_json r) (LS.to_json record_a)
            && Int.equal (List.length r.LS.queue) 2)
          ~error:(fun (Tea_core.Codec.Decode_failed (_ : string)) -> false))
    (* The empty queue is the COMMON persisted shape (every ack empties it)
       and Repr omits an empty list field on encode, so the round trip must
       hold with the field absent from the JSON. *)
    && (let empty = { record_a with LS.queue = [] } in
        LS.of_json (LS.to_json empty)
        |> Result.fold
             ~ok:(fun (r : LS.record) ->
               List.is_empty r.LS.queue
               && String.equal (LS.to_json r) (LS.to_json empty))
             ~error:(fun (Tea_core.Codec.Decode_failed (_ : string)) -> false)));
  check "codec-rejects-garbage" (Result.is_error (LS.of_json "{ not a record"));
  let record_b = { record_a with LS.written_at_ms = 2. } in
  check "decode-all-skips-bad-rows"
    (Int.equal
       (List.length
          (LS.decode_all
             [ LS.to_json record_a; "garbage"; LS.to_json record_b ]))
       2);
  let record_c = { record_a with LS.written_at_ms = 3. } in
  check "choose-picks-newest"
    (LS.choose [ record_c; record_a; record_b ]
    |> Option.fold ~none:false
         ~some:(fun (r : LS.record) -> Float.equal r.LS.written_at_ms 5.));
  check "choose-empty-none" (Option.is_none (LS.choose []));

  (* --- Delivery: persistence surface ------------------------------------ *)
  let d0 = Delivery.v ~tab:tab_a in
  let d1 = Delivery.record App.Increment d0 |> Option.fold ~none:d0 ~some:fst in
  let d2 = Delivery.record App.Decrement d1 |> Option.fold ~none:d1 ~some:fst in
  check "of-persisted-roundtrip"
    (Delivery.of_persisted ~tab:(Delivery.tab d2) ~next:(Delivery.next_seq d2)
       ~queue:(Delivery.unacked d2)
    |> Option.fold ~none:false
         ~some:(fun (d : App.msg Delivery.t) ->
           Int.equal (Tab_id.compare (Delivery.tab d) tab_a) 0
           && Int.equal
                (Msg_seq.to_int (Delivery.next_seq d))
                (Msg_seq.to_int (Delivery.next_seq d2))
           && eq_entries (labels d) (labels d2)));
  check "of-persisted-resumes-next"
    (Option.bind
       (Delivery.of_persisted ~tab:tab_a ~next:(seq 5) ~queue:[])
       (Delivery.record App.Increment)
    |> Option.fold ~none:false
         ~some:(fun
             ((_ : App.msg Delivery.t), ((n, (_ : App.msg)) : Msg_seq.t * App.msg))
           -> Int.equal (Msg_seq.to_int n) 5));
  check "of-persisted-rejects-seq-at-or-above-next"
    (Option.is_none
       (Delivery.of_persisted ~tab:tab_a ~next:(seq 5)
          ~queue:[ (seq 4, App.Increment); (seq 5, App.Increment) ])
    && Option.is_none
         (Delivery.of_persisted ~tab:tab_a ~next:(seq 2)
            ~queue:[ (seq 2, App.Increment) ]));
  check "of-persisted-rejects-disorder"
    (Option.is_none
       (Delivery.of_persisted ~tab:tab_a ~next:(seq 9)
          ~queue:[ (seq 2, App.Increment); (seq 1, App.Decrement) ])
    && Option.is_none
         (Delivery.of_persisted ~tab:tab_a ~next:(seq 9)
            ~queue:[ (seq 1, App.Increment); (seq 1, App.Decrement) ]));
  check "next-seq-peeks"
    (Int.equal (Msg_seq.to_int (Delivery.next_seq d2)) 3
    && Int.equal
         (Msg_seq.to_int (Delivery.next_seq d2))
         (Msg_seq.to_int (Delivery.next_seq d2)));

  (* --- the boot gate ---------------------------------------------------- *)
  let g_buf, sent_buf = LS.record_msg App.Increment LS.buffering in
  check "gate-buffers-before-resolve"
    (Option.is_none sent_buf
    && Option.is_none (LS.delivery g_buf)
    && not (LS.flushable g_buf));
  let g_buf2, (_ : (Msg_seq.t * App.msg) option) =
    LS.record_msg App.Decrement g_buf
  in
  let { LS.gate = g_no; paint = p_no; seed = s_no } =
    LS.resolve ~mint ~confirmed:None LS.No_record g_buf2
  in
  check "gate-resolve-no-record-replays-pending-in-order"
    (Option.is_none p_no
    && Option.is_none s_no
    && (LS.replay g_no
       |> Option.fold ~none:false
            ~some:(fun
                ((t, entries) :
                  Tab_id.t * (Msg_seq.t * App.msg) list)
              ->
              Int.equal (Tab_id.compare t tab_fresh) 0
              && eq_entries
                   (List.map
                      (fun ((n, m) : Msg_seq.t * App.msg) ->
                        (Msg_seq.to_int n, msg_label m))
                      entries)
                   [ (1, "i"); (2, "d") ])));
  let buffered_reset : LS.gate =
    let g, (_ : (Msg_seq.t * App.msg) option) =
      LS.record_msg App.Reset LS.buffering
    in
    g
  in
  let { LS.gate = g13; paint = p13; seed = s13 } =
    LS.resolve ~mint ~confirmed:None (LS.Adopt record_a) buffered_reset
  in
  check "gate-resolve-adopt-prefixes-persisted-then-pending"
    (eq_entries (gate_labels g13) [ (1, "i"); (2, "d"); (3, "r") ]);
  check "gate-resolve-adopt-seeds-paint-when-provisional"
    ((p13
     |> Option.fold ~none:false
          ~some:(fun (m : App.model) ->
            Int.equal (App.value m) (App.value m1)))
    && (s13 |> Option.fold ~none:false ~some:(Int64.equal 7L)));
  let { LS.gate = g16; paint = p16; seed = s16 } =
    LS.resolve ~mint ~confirmed:(Some rep_b) (LS.Adopt record_a) buffered_reset
  in
  let { LS.gate = g17; paint = p17; seed = s17 } =
    LS.resolve ~mint ~confirmed:(Some rep_a) (LS.Adopt record_a) buffered_reset
  in
  check "late-resolve-after-hello-never-paints"
    (Option.is_none p16 && Option.is_none p17
    (* The floor still rides both post-Hello adoptions: skipping it on the
       match arm would regress the durable floor and let two page lives
       mint colliding dots under one replica. *)
    && (s16 |> Option.fold ~none:false ~some:(Int64.equal 7L))
    && (s17 |> Option.fold ~none:false ~some:(Int64.equal 7L)));
  check "gate-resolve-post-resync-mismatch-discards"
    (eq_entries (gate_labels g16) [ (3, "r") ] && LS.flushable g16);
  check "gate-resolve-post-resync-match-adopts-confirmed"
    (eq_entries (gate_labels g17) [ (1, "i"); (2, "d"); (3, "r") ]
    && LS.flushable g17);
  check "gate-unconfirmed-blocks-flush"
    ((not (LS.flushable g13))
    && Option.is_none (LS.replay g13)
    && Option.is_some (LS.unconfirmed_replica g13));
  let g19, sent19 = LS.record_msg App.Increment g13 in
  check "gate-record-while-unconfirmed-withholds-send"
    (Option.is_none sent19
    && eq_entries (gate_labels g19) [ (1, "i"); (2, "d"); (3, "r"); (4, "i") ]);
  let g20, eff20 = LS.confirm ~announced:rep_a g19 in
  check "gate-confirm-match-flushes"
    ((match eff20 with
      | LS.Flush -> true
      | LS.Idle | LS.Prune_and_flush -> false)
    && LS.flushable g20
    && eq_entries (gate_labels g20) [ (1, "i"); (2, "d"); (3, "r"); (4, "i") ]);
  let g21, eff21 = LS.confirm ~announced:rep_b g19 in
  check "gate-confirm-mismatch-drops-adopted-prefix"
    ((match eff21 with
      | LS.Prune_and_flush -> true
      | LS.Idle | LS.Flush -> false)
    && eq_entries (gate_labels g21) [ (3, "r"); (4, "i") ]);
  check "gate-confirm-mismatch-keeps-pending-born"
    (LS.flushable g21
    && (LS.replay g21
       |> Option.fold ~none:false
            ~some:(fun
                ((t, entries) :
                  Tab_id.t * (Msg_seq.t * App.msg) list)
              ->
              Int.equal (Tab_id.compare t tab_a) 0
              && Int.equal (List.length entries) 2)));
  let g23, eff23 = LS.confirm ~announced:rep_a g20 in
  check "confirm-twice-stable"
    ((match eff23 with
      | LS.Idle -> true
      | LS.Flush | LS.Prune_and_flush -> false)
    && eq_entries (gate_labels g23) (gate_labels g20));
  let g24 = LS.ack (seq 4) LS.buffering in
  let (_ : LS.gate), sent24 = LS.record_msg App.Reset g24 in
  check "gate-ack-total-while-buffering"
    (Option.is_none (LS.delivery g24) && Option.is_none sent24);

  (* --- checkpoint ------------------------------------------------------- *)
  let d3 = Delivery.record App.Reset d2 |> Option.fold ~none:d2 ~some:fst in
  let dck = Delivery.ack (seq 1) d3 in
  let ck =
    LS.checkpoint_record ~replica:rep_a ~delivery:dck ~clock_floor:42L
      ~model:m1 ~now_ms:9.
  in
  check "checkpoint-encodes-exactly-unacked"
    (eq_entries
       (List.map
          (fun ((n, m) : Msg_seq.t * App.msg) ->
            (Msg_seq.to_int n, msg_label m))
          ck.LS.queue)
       [ (2, "d"); (3, "r") ]
    && Int.equal (Msg_seq.to_int ck.LS.next) 4
    && Int.equal (Tab_id.compare ck.LS.tab tab_a) 0);
  check "checkpoint-carries-clock-floor"
    (Int64.equal ck.LS.clock_floor 42L && Float.equal ck.LS.written_at_ms 9.);

  (* --- the page clock's floor ------------------------------------------- *)
  let c = Tea_core.Clock.create ~now:(fun () -> 100L) in
  let f0 = Tea_core.Clock.floor c in
  let f0' = Tea_core.Clock.floor c in
  let minted = (Tea_core.Clock.next c :> int64) in
  check "clock-floor-peeks-without-mint"
    (Int64.equal f0 Int64.min_int
    && Int64.equal f0' Int64.min_int
    && Int64.equal (Tea_core.Clock.floor c) minted);
  let c2 = Tea_core.Clock.create ~now:(fun () -> 0L) in
  Tea_core.Clock.seed c2 50L;
  check "clock-floor-reflects-seed" (Int64.equal (Tea_core.Clock.floor c2) 50L);

  (* --- classify --------------------------------------------------------- *)
  check "classify-quota"
    (String.equal (err_label (Store.classify "QuotaExceededError")) "quota");
  check "classify-version"
    (String.equal (err_label (Store.classify "VersionError")) "version");
  check "classify-notfound"
    (String.equal (err_label (Store.classify "NotFoundError")) "notfound");
  check "classify-unknown-other"
    (String.equal (err_label (Store.classify "Weird")) "other:Weird");

  (* --- the fake's own honesty ------------------------------------------- *)
  let f1 = Fake_idb.create () in
  let hit = ref false in
  Fake_idb.replace_all f1 ~store:Store.store_name ~key:"k" ~value:"v"
    ~ok:(fun () -> hit := true)
    ~err:(fun (_ : Store.idb_error) -> ());
  check "fake-async-delivery" ((not !hit) && Fake_idb.tick f1 && !hit);
  let f2 = Fake_idb.create () in
  let tx = Fake_idb.txn_open f2 in
  let (_ : bool) = Fake_idb.tick f2 in
  check "fake-autocommit-rejects-cross-tick-request"
    (Fake_idb.txn_request f2 tx ~op:`Clear
    |> Result.fold
         ~ok:(fun () -> false)
         ~error:(fun (e : Store.idb_error) ->
           String.equal (err_label e) "other:TransactionInactiveError"));

  (* --- Flow: boot and checkpoint over the fake --------------------------- *)
  let f3 = Fake_idb.create () in
  Fake_idb.inject f3 Store.Blocked;
  let hits = ref 0 in
  let got = ref None in
  Flow.boot ~title:"t"
    ~k:(fun ((c : Flow.conn option), (rs : LS.record list)) ->
      incr hits;
      got := Some (Option.is_some c, List.length rs));
  Fake_idb.drain f3;
  check "flow-boot-exactly-once-k" (Int.equal !hits 1);
  check "flow-boot-blocked-degrades"
    (!got
    |> Option.fold ~none:false
         ~some:(fun ((has_conn, n) : bool * int) ->
           (not has_conn) && Int.equal n 0));
  let boot_degrades (e : Store.idb_error) : bool =
    let f = Fake_idb.create () in
    Fake_idb.inject f e;
    let seen = ref None in
    Flow.boot ~title:"t"
      ~k:(fun ((c : Flow.conn option), (rs : LS.record list)) ->
        seen := Some (Option.is_none c && List.is_empty rs));
    Fake_idb.drain f;
    Option.value !seen ~default:false
  in
  check "flow-boot-unsupported-degrades" (boot_degrades Store.Unsupported);
  check "flow-boot-version-error-degrades" (boot_degrades Store.Version_error);
  let f4 = Fake_idb.create () in
  let conn4 = ref None in
  Flow.boot ~title:"t"
    ~k:(fun ((c : Flow.conn option), (_ : LS.record list)) -> conn4 := c);
  Fake_idb.drain f4;
  Flow.checkpoint !conn4 record_a ~k:(fun (_ : (unit, Store.idb_error) result) -> ());
  Fake_idb.drain f4;
  let again = ref [] in
  let conn5 = ref None in
  Flow.boot ~title:"t"
    ~k:(fun ((c : Flow.conn option), (rs : LS.record list)) ->
      conn5 := c;
      again := rs);
  Fake_idb.drain f4;
  check "flow-boot-reads-rows-roundtrip"
    (List.equal String.equal
       (List.map LS.to_json !again)
       [ LS.to_json record_a ]);
  Fake_idb.inject f4 Store.Quota_exceeded;
  let quota = ref None in
  Flow.checkpoint !conn4 record_b
    ~k:(fun (res : (unit, Store.idb_error) result) -> quota := Some res);
  Fake_idb.drain f4;
  let only_record_a () : bool =
    List.equal String.equal
      (List.map (fun ((_, v) : string * string) -> v) (Fake_idb.rows f4))
      [ LS.to_json record_a ]
  in
  check "flow-checkpoint-quota-reports-error-not-raise"
    (!quota
    |> Option.fold ~none:false
         ~some:
           (Result.fold
              ~ok:(fun () -> false)
              ~error:(fun (e : Store.idb_error) ->
                String.equal (err_label e) "quota"))
    && only_record_a ());
  (* [version_change] fires the LAST boot's closure, so the degrade lands on
     [conn5]; a checkpoint through it must no-op. *)
  Fake_idb.version_change f4;
  Fake_idb.drain f4;
  let noop = ref None in
  Flow.checkpoint !conn5 record_b
    ~k:(fun (res : (unit, Store.idb_error) result) -> noop := Some res);
  let none_conn = ref None in
  Flow.checkpoint None record_b
    ~k:(fun (res : (unit, Store.idb_error) result) -> none_conn := Some res);
  Fake_idb.drain f4;
  check "flow-checkpoint-degraded-conn-noop"
    (only_record_a ()
    && (!noop
       |> Option.fold ~none:false
            ~some:
              (Result.fold
                 ~ok:(fun () -> true)
                 ~error:(fun (_ : Store.idb_error) -> false)))
    && (!none_conn
       |> Option.fold ~none:false
            ~some:
              (Result.fold
                 ~ok:(fun () -> true)
                 ~error:(fun (_ : Store.idb_error) -> false))));
  let f5 = Fake_idb.create () in
  let conn6 = ref None in
  Flow.boot ~title:"t"
    ~k:(fun ((c : Flow.conn option), (_ : LS.record list)) -> conn6 := c);
  Fake_idb.drain f5;
  Flow.checkpoint !conn6 record_a ~k:(fun (_ : (unit, Store.idb_error) result) -> ());
  Fake_idb.drain f5;
  let inv = ref None in
  Flow.invalidate !conn6
    ~k:(fun (res : (unit, Store.idb_error) result) -> inv := Some res);
  Fake_idb.drain f5;
  Flow.checkpoint !conn6 record_b ~k:(fun (_ : (unit, Store.idb_error) result) -> ());
  Fake_idb.drain f5;
  check "flow-invalidate-clears-and-degrades"
    (List.is_empty (Fake_idb.rows f5)
    && (!inv
       |> Option.fold ~none:false
            ~some:
              (Result.fold
                 ~ok:(fun () -> true)
                 ~error:(fun (_ : Store.idb_error) -> false))));
  print_endline "local_store_test: all checks passed"
