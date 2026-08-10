(** {!Tea_server_pack.forget_into} against a REAL journal-backed guard over a
    REAL pack root (roadmap step 22, D24): the reaper's [?forget] hook exactly
    as {!Tea_server_pack.Make_pack.serve_pack} wires it, driven natively. The
    test passes the SAME [forget_into] the serve path passes (the
    [Rebase.absorb] precedent), so the two cannot drift.

    W1: a floored session is swept; its mirror floor dies with it, the journal
    carries the Forget tombstone across a reopen, and the returning client's
    replay reads Fresh (the rollback-plus-one-edit pin). Anti-vacuity: the
    floor is asserted to EXIST before the sweep, and the reopen filter is fed
    the PRE-SWEEP head snapshot, which still lists the victim's branch, so the
    floor's absence can only be the tombstone, never a [dropped_no_branch]
    coincidence.

    W2: with the journal CLOSED under the guard, a sweep of two stale victims
    continues past each sink [Error]: both are swept and both mirror floors
    are scrubbed. Anti-vacuity: a direct [persist] against the closed sink is
    asserted to refuse, so the forget path provably hit its [Error] arm.

    W4: every id the sweep hands [forget_into] is a session id; [main] and
    every reserved ref never reach it. *)

module Store = Tea_persist_pack.Store_pack.Make (Counter_app.App)
module Prim = Tea_core.Prim
module Replica = Tea_core.Crdt.Replica
module Durable_guard = Tea_server.Durable_guard
module Replay_guard = Tea_server.Replay_guard
module Guard_sink = Tea_server.Guard_sink
module Floors = Tea_server.Durable_guard.Floors
module Guard_file = Tea_server_pack.Guard_file

let check (name : string) (cond : bool) : unit =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

let die (what : string) : 'a =
  Printf.printf "FAIL - %s\n%!" what;
  exit 1

let sid (s : string) : Prim.Session_id.t =
  Option.fold (Prim.Session_id.of_string s)
    ~none:(fun () -> die ("session id rejected: " ^ s))
    ~some:(fun (v : Prim.Session_id.t) () -> v)
    ()

let seq_of (n : int) : Prim.Msg_seq.t =
  Option.fold (Prim.Msg_seq.of_int n)
    ~none:(fun () -> die (Printf.sprintf "Msg_seq.of_int rejects %d" n))
    ~some:(fun (s : Prim.Msg_seq.t) () -> s)
    ()

let tab_of (s : string) : Prim.Tab_id.t =
  Result.fold (Prim.Tab_id.of_string s)
    ~ok:(fun (t : Prim.Tab_id.t) -> t)
    ~error:(fun (_ : Prim.Tab_id.err) -> die ("tab id rejected: " ^ s))

let ttl : Prim.Ttl.t =
  Option.fold (Prim.Ttl.of_seconds 1000.)
    ~none:(fun () -> die "Ttl.of_seconds 1000. rejected")
    ~some:(fun (t : Prim.Ttl.t) () -> t)
    ()

let branch_of (s : string) : string =
  Prim.Branch_name.(to_string (of_session (sid s)))

(* The names [serve_pack] would derive from a root of [<parent>/store]. *)
let parent = Filename.temp_dir "ocaml-tea-reaper-wiring" ""
let root = Tea_persist_pack.Store_pack.Root.v (Filename.concat parent "store")
let guard_dir = Filename.concat parent "store.guard"

(* A frozen wall source: every commit below is stamped ~1000, so a sweep at a
   far-future [now] makes every session branch a victim, deterministically. *)
let now_r = ref 1000L
let t = Lwt_main.run (Store.create ~now:(fun () -> !now_r) root)

(* main and the W1 victim commit while the clock reads ~1000. The session
   handle is kept: the replica a session applies under is minted from its
   BRANCH name ([session-<sid>]), so the only honest derivation is the
   session's own ctx (the reaper_test PIN), never [Replica.v] over the raw
   session id. *)
let victim = Lwt_main.run (Store.session t (sid "victima"))

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let open Counter_app.App in
     let* main = Store.main_session t in
     let* (_ : model) = Store.apply main Increment in
     let* (_ : model) = Store.apply victim Increment in
     Lwt.return_unit)

(* ONE branch_waters read between the store open and the guard open, the
   serve_pack composition. Captured BEFORE the sweep on purpose: the reopen
   below is filtered against this snapshot, in which the victim's branch
   still stands. *)
let head_water_pre =
  Lwt_main.run (Lwt.map Guard_file.head_water_of_list (Store.branch_waters t))

let identity : Prim.Store_identity.binding =
  Prim.Store_identity.Bound (Prim.Store_identity.of_draws (fun () -> 0x2a))

let epoch0 : Prim.Store_epoch.binding * Prim.Store_epoch.binding =
  (Prim.Store_epoch.Bound Prim.Store_epoch.bottom, Prim.Store_epoch.Bound Prim.Store_epoch.bottom)

let { Tea_server_pack.ws = guard1; ws_journal = journal1; rpc = (_ : Durable_guard.t); rpc_journal = rpc_journal1 } =
  Tea_server_pack.open_guards ~guard_dir ~head_water:head_water_pre ~identity
    ~epoch:epoch0 ()

let victim_replica = Tea_core.Crdt.Ctx.replica (Store.ctx_of_session victim)
let tab1 = tab_of "a1b2c3d4e5f60718293a4b5c6d7e8f90"

let victim_water : Prim.Store_water.t =
  Option.fold (head_water_pre victim_replica)
    ~none:(fun () -> die "the victim's branch has no head water")
    ~some:(fun (w : Prim.Store_water.t) () -> w)
    ()

(* --- W1: floor, sweep through forget_into, reopen, replay ---------------- *)

let () =
  check "W1: the ws journal opened (the floor below is durable)"
    (Option.is_some journal1);
  Lwt_main.run
    (Durable_guard.persist guard1 ~replica:victim_replica ~tab:tab1
       ~seq:(seq_of 1) ~water:victim_water)
  |> Result.fold
       ~ok:(fun () -> ())
       ~error:(fun (_ : Guard_sink.err) -> die "W1: persist refused");
  check "W1 positive control: the floor EXISTS before the sweep"
    (Option.is_some
       (Floors.find_stamped ~replica:victim_replica ~tab:tab1
          (Durable_guard.floors guard1)))

let swept1 =
  Lwt_main.run
    (Store.reap ~forget:(Tea_server_pack.forget_into guard1) t ~ttl
       ~now:1_000_000L)

let () =
  check "W1: exactly the one victim branch is swept" (swept1 = 1);
  check "W1: the mirror floor is GONE once the sweep returns"
    (Option.is_none
       (Floors.find_stamped ~replica:victim_replica ~tab:tab1
          (Durable_guard.floors guard1)));
  let post = Lwt_main.run (Store.S.Branch.list (Store.repo t)) in
  check "W1: the victim branch is gone and main is kept"
    ((not (List.mem (branch_of "victima") post)) && List.mem "main" post)

(* Reopen the journals against the PRE-SWEEP snapshot. The filter would KEEP
   the victim's floor (its branch still stands in that snapshot, at the very
   water the floor claims), so its absence below is the appended Forget
   tombstone honored, nothing else. *)
let () =
  Lwt_main.run (Option.fold journal1 ~none:Lwt.return_unit ~some:Guard_file.close);
  Lwt_main.run
    (Option.fold rpc_journal1 ~none:Lwt.return_unit ~some:Guard_file.close)

let { Tea_server_pack.ws = guard2; ws_journal = journal2; rpc = (_ : Durable_guard.t); rpc_journal = rpc_journal2 } =
  Tea_server_pack.open_guards ~guard_dir ~head_water:head_water_pre ~identity
    ~epoch:epoch0 ()

let () =
  check "W1: the reopened journal shows NO floor for the victim (Forget honored)"
    (Option.is_none
       (Floors.find_stamped ~replica:victim_replica ~tab:tab1
          (Durable_guard.floors guard2)));
  let verdict =
    Durable_guard.take guard2 ~replica:victim_replica ~tab:tab1 ~seq:(seq_of 1)
  in
  check "W1: the returning client's replay of the same seq reads Fresh"
    (match verdict with
     | Replay_guard.Fresh (_ : Prim.Msg_seq.t) -> true
     | Replay_guard.Duplicate (_ : Prim.Msg_seq.t) -> false
     | Replay_guard.Gapped -> false)

(* --- W2 + W4: a closed sink mid-sweep; reserved refs ---------------------- *)

let w2_replicas : Replica.t list =
  Lwt_main.run
    (let open Lwt.Syntax in
     let open Counter_app.App in
     let* c = Store.session t (sid "victimc") in
     let* (_ : model) = Store.apply c Increment in
     let* d = Store.session t (sid "victimd") in
     let* (_ : model) = Store.apply d Increment in
     Lwt.return
       [ Tea_core.Crdt.Ctx.replica (Store.ctx_of_session c)
       ; Tea_core.Crdt.Ctx.replica (Store.ctx_of_session d)
       ])

let () =
  List.iter
    (fun (r : Replica.t) ->
      Lwt_main.run
        (Durable_guard.persist guard2 ~replica:r ~tab:tab1 ~seq:(seq_of 1)
           ~water:victim_water)
      |> Result.fold
           ~ok:(fun () -> ())
           ~error:(fun (_ : Guard_sink.err) -> die "W2: persist refused"))
    w2_replicas;
  check "W2 positive control: both floors EXIST before the sweep"
    (List.for_all
       (fun (r : Replica.t) ->
         Option.is_some
           (Floors.find_stamped ~replica:r ~tab:tab1
              (Durable_guard.floors guard2)))
       w2_replicas);
  (* Close the journal UNDER the guard: every later sink append must refuse. *)
  Lwt_main.run (Option.fold journal2 ~none:Lwt.return_unit ~some:Guard_file.close);
  check "W2 positive control: the closed sink refuses a direct persist"
    (Lwt_main.run
       (Durable_guard.persist guard2 ~replica:victim_replica ~tab:tab1
          ~seq:(seq_of 2) ~water:victim_water)
     |> Result.fold
          ~ok:(fun () -> false)
          ~error:(fun (e : Guard_sink.err) ->
            match e with
            | Guard_sink.Sink_closed -> true
            | Guard_sink.Io (_ : string) -> false))

let probed : Prim.Session_id.t list ref = ref []

let swept2 =
  Lwt_main.run
    (Store.reap
       ~forget:(fun (s : Prim.Session_id.t) ->
         probed := !probed @ [ s ];
         Tea_server_pack.forget_into guard2 s)
       t ~ttl ~now:2_000_000L)

let () =
  check "W2: the sweep continues past the sink Errors: BOTH victims are swept"
    (swept2 = 2);
  let final = Lwt_main.run (Store.S.Branch.list (Store.repo t)) in
  check "W2: both victim branches are gone, main survives"
    ((not (List.mem (branch_of "victimc") final))
    && (not (List.mem (branch_of "victimd") final))
    && List.mem "main" final);
  check "W2: both mirror floors are scrubbed even though every append failed"
    (List.for_all
       (fun (r : Replica.t) ->
         Option.is_none
           (Floors.find_stamped ~replica:r ~tab:tab1
              (Durable_guard.floors guard2)))
       w2_replicas);
  check "W4: the sweep handed forget_into exactly the two victims, never a reserved ref"
    (List.length !probed = 2
    && List.for_all
         (fun (s : Prim.Session_id.t) ->
           let n = Prim.Session_id.to_string s in
           (not (String.equal n "main"))
           && (not (String.starts_with ~prefix:"redo-" n))
           && not (String.starts_with ~prefix:"__" n))
         !probed)

let () =
  Lwt_main.run
    (Option.fold rpc_journal2 ~none:Lwt.return_unit ~some:Guard_file.close);
  Lwt_main.run (Store.close t);
  let rec rm_rf (path : string) : unit =
    if Sys.is_directory path then (
      Array.iter (fun entry -> rm_rf (Filename.concat path entry)) (Sys.readdir path);
      Sys.rmdir path)
    else Sys.remove path
  in
  rm_rf parent;
  Printf.printf
    "\nforget_into tombstones the swept floor, survives a closed sink, and never sees a reserved ref (D24).\n%!"
