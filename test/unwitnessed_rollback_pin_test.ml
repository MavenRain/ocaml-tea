(** The R15 unwitnessed>0 gap, pinned (roadmap step 20).

    {!Tea_server.Durable_guard.Floors.filter} never drops a bottom-water
    floor: a claim of nothing cannot be failed, so the boot filter adopts it
    on trust and says so through the verdict's [unwitnessed] count. R15's
    sentence is the contract pinned here: a boot that adopted any such floor
    is "NOT protected against a restored older pack root when
    unwitnessed > 0".

    This file is a {b characterization pin}, marked [PIN] in each check
    label, the same discipline as [predictor_test.ml]'s pins: it records the
    open gap on purpose. The double below mirrors [durable_guard_test.ml]'s
    life1/reopened restart: life 1 persists a real-water floor for tab A and
    a bottom-water (fuel-exhaustion shaped) floor for tab B; the reopened
    boot filters the rebuilt floors against a head rolled BEHIND tab A's
    water. Tab A's floor drops as [dropped_behind]; tab B's bottom floor
    stays kept AND counts as unwitnessed.

    A future change that closes the gap (one that drops or re-verifies
    bottom floors under a rolled-back head) turns this file red. That red is
    the ALARM working, not a regression to silence: revisit this file, do
    not patch it green. *)

module Dg = Tea_server.Durable_guard
module Guard = Tea_server.Replay_guard
module Sink = Tea_server.Guard_sink
module Msg_seq = Tea_core.Prim.Msg_seq
module Tab_id = Tea_core.Prim.Tab_id
module Water = Tea_core.Prim.Store_water

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
    runtime uses. *)
let tab (n : int) : Tab_id.t =
  must "tab id mint refused a valid seed"
    (Tab_id.of_bytes (List.init 16 (fun i -> (n + i) land 0xff)))

(** A sequence number the mint accepted, or a loud setup failure. *)
let seq (n : int) : Msg_seq.t =
  must (Printf.sprintf "Msg_seq.of_int refused %d" n) (Msg_seq.of_int n)

(** A water mint, for tab A's real witness and the rolled-back head. *)
let water (n : int) : Water.t = Water.of_date (Int64.of_int n)

(** [Fresh] carrying exactly [at]. *)
let is_fresh (v : Guard.verdict) ~(at : int) : bool =
  match v with
  | Guard.Fresh n -> Int.equal (Msg_seq.to_int n) at
  | Guard.Duplicate (_ : Msg_seq.t) -> false
  | Guard.Gapped -> false

(** [Some] of exactly [at], for the surviving floor. *)
let floor_is (o : Msg_seq.t option) ~(at : int) : bool =
  Option.fold ~none:false
    ~some:(fun (n : Msg_seq.t) -> Int.equal (Msg_seq.to_int n) at)
    o

(* --- The life1/reopened double, filtered against a rolled-back head --------

   Life 1 records through a memory sink: tab A's floor carries a real
   witness (its commit's water), tab B's carries [bottom], the shape a no-op
   or fuel-exhausted take mints. The reopened boot is nothing but the
   recorded journal folded into Floors, then filtered against a head
   strictly below tab A's water: the restored-older-pack-root world. Pure
   in-memory composition, no store. *)

let () =
  let r = replica "unwitnessed-pin" in
  let tab_a = tab 1 and tab_b = tab 2 in
  let sink1, reader = Sink.memory () in
  let life1 =
    Dg.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs ~sink:sink1
      ~floors:Dg.Floors.empty ()
  in
  let consume (t : Tab_id.t) (w : Water.t) : bool =
    is_fresh (Dg.take life1 ~replica:r ~tab:t ~seq:(seq 1)) ~at:1
    && Result.is_ok
         (Lwt_main.run (Dg.persist life1 ~replica:r ~tab:t ~seq:(seq 1) ~water:w))
  in
  check "life 1 persists tab A with a real water and tab B with bottom"
    (consume tab_a (water 100) && consume tab_b Water.bottom);
  let reopened = Dg.Floors.of_events (reader ()) in
  (* The reopened head: the branch is readable, but its water sits strictly
     below tab A's claimed 100. [head] is keyed by replica, so one answer
     serves both tabs. *)
  let rolled_back_head (_ : Tea_core.Crdt.Replica.t) : Water.t option =
    Some (water 50)
  in
  let survivors, verdict = Dg.Floors.filter ~head:rolled_back_head reopened in
  let { Dg.Floors.kept; dropped_behind; dropped_no_branch; unwitnessed } =
    verdict
  in
  check "PIN: the head behind tab A's water drops exactly that floor"
    (Int.equal dropped_behind 1
    && Int.equal dropped_no_branch 0
    && Option.is_none (Dg.Floors.find ~replica:r ~tab:tab_a survivors));
  check "PIN: tab B's bottom-water floor stays kept under the rolled-back head"
    (Int.equal kept 1
    && floor_is (Dg.Floors.find ~replica:r ~tab:tab_b survivors) ~at:1);
  check "PIN: the kept bottom floor counts as unwitnessed, R15's open gap"
    (Int.equal unwitnessed 1)
