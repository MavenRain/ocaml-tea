(** The commit fence (roadmap step 20, R11): [Durable_guard.persist] runs its
    [?fence] strictly between the mirror advance and the sink append, on every
    persist, so the composition site can force the floor's own commit bytes
    into the page cache before the floor itself can get there.

    [durable_guard_test.ml] pins the layer; this file pins the seam's order,
    with an instrumented fence and a spying sink sharing one tick log:

    - every persist reads [fence; append] in program order, for a stamped
      water and for [bottom] alike: the fence is unconditional and strictly
      precedes its own append;
    - a REJECTING fence resolves the same audible [Error Io] a rejecting sink
      does, with the floor never appended (the duplicate direction) and the
      mirror already advanced (the fence sits inside the catching seam, after
      the advance);
    - [forget] never runs the fence: a tombstone protects no commit;
    - with [?fence] omitted the guard is the pre-step-20 composition byte for
      byte: append ticks only, the same results, the same floors. *)

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

(** A non-bottom store water, for the stamped side of the script. *)
let water (n : int) : Water.t = Water.of_date (Int64.of_int n)

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

(** [Some] of exactly [at], for mirror floors. *)
let floor_is (o : Msg_seq.t option) ~(at : int) : bool =
  Option.fold ~none:false
    ~some:(fun (n : Msg_seq.t) -> Int.equal (Msg_seq.to_int n) at)
    o

(** The persist outcome is [Error Io], with both [err] constructors spelled. *)
let is_io_error (r : (unit, Sink.err) result) : bool =
  Result.fold r
    ~ok:(fun () -> false)
    ~error:(fun (e : Sink.err) ->
      match e with
      | Sink.Io (_ : string) -> true
      | Sink.Sink_closed -> false)

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

(** A recorded event as a comparable tag, every constructor spelled and the
    water's SHAPE kept, so one list equality pins both order and stamps. *)
let event_tag (e : Sink.event) : string =
  match e with
  | Sink.Advance
      { replica = (_ : Tea_core.Crdt.Replica.t)
      ; tab = (_ : Tab_id.t)
      ; seq = s
      ; water = w
      } ->
    Printf.sprintf "advance:%d:%s" (Msg_seq.to_int s)
      (if Water.equal w Water.bottom then "bottom" else "stamped")
  | Sink.Forget { replica = (_ : Tea_core.Crdt.Replica.t) } -> "forget"

(** The shared spy log: ticks pushed newest-first, read oldest-first. *)
let tick (log : string list ref) (t : string) : unit = log := t :: !log

let ticks (log : string list ref) : string list = List.rev !log

(** The spying sink: one tick per append, then delegate to the wrapped memory
    sink so the journal itself stays observable. The tick lands BEFORE the
    delegation, so a fence tick after an append tick could never be an
    artifact of the wrapper's own ordering. *)
let spy_sink (log : string list ref) (inner : Sink.t) : Sink.t =
  { Sink.append =
      (fun (e : Sink.event) ->
        tick log "append";
        inner.Sink.append e)
  }

(** One life of the shared script, everything a check needs to observe it. *)
type run =
  { fresh : bool  (** both takes read [Fresh] at their seq *)
  ; results : (unit, Sink.err) result list  (** persist outcomes, program order *)
  ; log : string list ref  (** the shared fence/append tick log *)
  ; events : unit -> Sink.event list  (** the wrapped memory sink's reader *)
  ; guard : Dg.t
  ; r : Tea_core.Crdt.Replica.t
  ; t : Tab_id.t
  }

(** The shared two-persist script: take-then-persist seq 1 under a real store
    water, then seq 2 under [bottom], on one (replica, tab). [fenced] wires
    the spying fence; [false] omits [?fence] entirely, the pre-step-20
    composition. The replica name is the same both ways so the resulting
    floors are comparable key for key. *)
let script (fenced : bool) : run =
  let log = ref [] in
  let inner, reader = Sink.memory () in
  let fence =
    if fenced then
      Some
        (fun () ->
          tick log "fence";
          Lwt.return_unit)
    else None
  in
  let g =
    Dg.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs ?fence
      ~sink:(spy_sink log inner) ~floors:Dg.Floors.empty ()
  in
  let r = replica "script" in
  let t = tab 1 in
  let consume (n : int) ~(water : Water.t) : bool * (unit, Sink.err) result =
    let fresh = is_fresh (Dg.take g ~replica:r ~tab:t ~seq:(seq n)) ~at:n in
    (fresh, Lwt_main.run (Dg.persist g ~replica:r ~tab:t ~seq:(seq n) ~water))
  in
  let fresh1, res1 = consume 1 ~water:(water 100) in
  let fresh2, res2 = consume 2 ~water:Water.bottom in
  { fresh = fresh1 && fresh2
  ; results = [ res1; res2 ]
  ; log
  ; events = reader
  ; guard = g
  ; r
  ; t
  }

let fenced_run = script true
let baseline_run = script false

(* --- 1. The fenced life: fence;append per persist, both water shapes ------- *)

let () =
  check "the fenced script takes Fresh and both persists resolve Ok"
    (fenced_run.fresh && List.for_all Result.is_ok fenced_run.results);
  check
    "the log reads fence;append;fence;append in program order: each fence \
     strictly precedes its own append"
    (List.equal String.equal (ticks fenced_run.log)
       [ "fence"; "append"; "fence"; "append" ]);
  check "the fence is unconditional: it ticked for the bottom-water persist too"
    (Int.equal
       (List.length
          (List.filter (String.equal "fence") (ticks fenced_run.log)))
       2);
  check "the fence blocked nothing: both advances reached the sink, stamps intact"
    (List.equal String.equal
       (List.map event_tag (fenced_run.events ()))
       [ "advance:1:stamped"; "advance:2:bottom" ])

(* --- 2. The negative control: ?fence omitted is the pre-step-20 baseline --- *)

let () =
  check "the baseline script takes Fresh and both persists resolve Ok"
    (baseline_run.fresh && List.for_all Result.is_ok baseline_run.results);
  check "the baseline log is append ticks only: no fence ever ran"
    (List.equal String.equal (ticks baseline_run.log) [ "append"; "append" ]);
  check "the baseline journal carries the same records as the fenced one"
    (List.equal String.equal
       (List.map event_tag (baseline_run.events ()))
       (List.map event_tag (fenced_run.events ())));
  check "the baseline floors agree with the fenced floors, key for key"
    (floors_agree
       (Dg.floors fenced_run.guard)
       (Dg.floors baseline_run.guard)
       ~keys:[ (fenced_run.r, fenced_run.t) ])

(* --- 3. The rejecting fence: inside the catch, before the append ----------

   The fence contract is total-return, but the closure is caller-supplied, so
   a rejection must be caught at the same seam a rejecting sink's is: after
   the mirror advance, before the append. The floor is NEVER appended, which
   is the duplicate direction, and the rejection never escapes [persist]. *)

let () =
  let log = ref [] in
  let inner, reader = Sink.memory () in
  let g =
    Dg.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
      ~fence:(fun () ->
        tick log "fence";
        Lwt.fail Stdlib.Exit)
      ~sink:(spy_sink log inner) ~floors:Dg.Floors.empty ()
  in
  let r = replica "rejecting-fence" in
  let t = tab 1 in
  check "the seq is taken Fresh before the poisoned persist"
    (is_fresh (Dg.take g ~replica:r ~tab:t ~seq:(seq 1)) ~at:1);
  let outcome =
    Lwt_main.run
      (Lwt.catch
         (fun () ->
           Lwt.map
             (fun (res : (unit, Sink.err) result) -> `Resolved res)
             (Dg.persist g ~replica:r ~tab:t ~seq:(seq 1) ~water:(water 7)))
         (fun (_ : exn) -> Lwt.return `Rejected))
  in
  check "a rejecting fence degrades persist to Error Io, never a rejection"
    (match outcome with
     | `Resolved res -> is_io_error res
     | `Rejected -> false);
  check "the fence ticked but the append never did: the floor was not appended"
    (List.equal String.equal (ticks log) [ "fence" ]
    && Int.equal (List.length (reader ())) 0);
  check "the mirror advanced before the fence: the floor survives in process"
    (floor_is (Dg.Floors.find ~replica:r ~tab:t (Dg.floors g)) ~at:1);
  check "the poisoned persist released nothing: the seq stays consumed"
    (is_duplicate (Dg.take g ~replica:r ~tab:t ~seq:(seq 1)) ~high:1)

(* --- 4. forget never runs the fence ---------------------------------------- *)

let () =
  let before = ticks fenced_run.log in
  let forgot = Lwt_main.run (Dg.forget fenced_run.guard ~replica:fenced_run.r) in
  check "forget appends its tombstone and never ticks the fence"
    (Result.is_ok forgot
    && List.equal String.equal (ticks fenced_run.log) (before @ [ "append" ])
    && List.equal String.equal
         (List.map event_tag (fenced_run.events ()))
         [ "advance:1:stamped"; "advance:2:bottom"; "forget" ])
