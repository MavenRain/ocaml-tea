(** Cancellation atomicity of the WS pump (roadmap step 16, R10d/D21).

    [exactly_once_test] pins the take-to-ack span against sockets that DIE;
    this file pins it against promises that are CANCELLED, which is the one
    force that can tear the span from the inside. Step 16 closed two distinct
    cancellation sources and each closure gets its own deterministic red here:

    - The INTERNAL race (W1): the old teardown raced the whole pump against
      the send loop with [Lwt.pick], so a send failure cancelled the pump mid
      [step_ws] — seq consumed, effect unapplied, no floor, replay acked as
      Duplicate without ever applying. Silent loss. The restructure makes the
      per-frame body ([handle_frame]) a plain bind chain that is never a
      cancel target; the sender is backgrounded and its death is a one-shot
      [died] signal observed only BETWEEN frames ([Lwt.choose], which never
      cancels the loser). Scenarios 1/2 drive the race with [?interpose] and
      the poisoned-send kill handle.

    - The EXTERNAL source: a caller cancelling the promise [live_session]
      returned. [Lwt.protected] wraps [step_ws] plus BOTH [persist_taken]
      arms in ONE body: the wrapper rejects [Canceled] immediately (so the
      pump never recurses past the death of its socket) while the body runs
      to natural completion as an orphan — at worst a licensed visible
      duplicate, never a silent loss. Scenarios 4/5 cancel mid-span with the
      body gate-held, for the success floor and the fuel bottom floor;
      scenario 6 pins the rejection barrier's Canceled discrimination by
      racing a replay against the still-parked orphan: it must read
      Duplicate, because a release firing on Canceled would hand it Fresh
      and a second apply.

    - The REJECTION door (rounds 2-3): [Lwt.protected] only converts a
      cancellation into a completing orphan; a rejection of its own body
      used to propagate with the seq consumed and no floor persisted - W1
      through another door. The [Lwt.catch] barrier mirrors the keyed HTTP
      tier's R27 and sits INSIDE the protected body (round 3): [protected]
      never rejects its body, so every exception the catch sees is
      body-originated, the body is dead with no floor coming, and the
      release is unconditionally its compensation - POSITION, not a match
      on the exception value, is the discrimination. Scenario 3 rejects
      [?interpose] and proves prompt unconditional teardown ([Lwt.finalize],
      not a bind that skips on rejection), no floor forged, and - through
      its reconnect ladder - the released seq applying exactly once.
      Scenario 7 drives exactly the two holes the round-2 shape (catch
      OUTSIDE protected, matching on the value) left open: cancel the
      wrapper FIRST, then reject the still-parked orphan with a
      body-internal [Canceled]; the release must still fire, from inside
      the orphan, and the reconnect replay must read Fresh and apply
      exactly once.

    Unlike its siblings this file ACCUMULATES failures instead of exiting on
    the first one, so one mutant's whole red set is visible in a single run;
    it still exits non-zero at the end if anything failed. *)

module Server = Tea_server.Make (Counter_app.App)
module Codec = Tea_core.Codec.Make (Counter_app.App)
module App = Counter_app.App
module Guard = Tea_server.Replay_guard
module Dguard = Tea_server.Durable_guard
module Sink = Tea_server.Guard_sink
module Park = Tea_server.Ack_park
module Msg_seq = Tea_core.Prim.Msg_seq
module Tab_id = Tea_core.Prim.Tab_id
module Replica = Tea_core.Crdt.Replica

(** The fuel-exhaustion fixture, now the shared [Fuel_app] (one copy with
    [fuel_durable_test]): [Spin]'s command re-emits [Spin] until the budget
    dies, [Bump] settles at once. Scenario 5 needs it because the Counter
    app's update can never reach the fuel arm. *)
module Fuel = Fuel_app

module _ : Tea_core.App.APP = Fuel
module Fserver = Tea_server.Make (Fuel)
module Fcodec = Tea_core.Codec.Make (Fuel)

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    incr failures;
    Printf.printf "FAIL - %s\n%!" name)

let await = Test_util.await

(* Both branches are closures: [Option.fold]'s [~none:] is eager, so a refusal
   written as a value would exit before the first check ran. *)
let must (what : string) (o : 'a option) : 'a =
  Option.fold
    ~none:(fun () ->
      Printf.printf "FAIL - test setup: %s\n%!" what;
      exit 1)
    ~some:(fun x () -> x)
    o ()

let sid name = must "session id" (Tea_core.Prim.Session_id.of_string name)
let tab_of (n : int) = Tab_id.of_draws (fun () -> n)

(* One socket's worth of transport, the [exactly_once_test] idiom extended by
   the kill handle the step-16 seam list names. [arm] hands the socket a gate:
   from then on a send parks on the gate and then REJECTS, exactly what a dead
   TCP peer does to Dream's send — so the test chooses the precise instant the
   backgrounded sender dies, without needing a store commit to wake it.
   [refused] counts the rejections so a test can await the moment the death
   actually happened rather than sleeping and hoping. *)
type send_mode =
  | Normal
  | Reject_after of unit Lwt.t

type link =
  { transport : Server.live_transport
  ; push : string option -> unit
  ; frames : unit -> string list
  ; arm : unit Lwt.t -> unit
  ; refused : unit -> int
  }

let link () : link =
  let incoming, push = Lwt_stream.create () in
  let sent = ref [] in
  let mode = ref Normal in
  let refused = ref 0 in
  { transport =
      { Server.send_frame =
          (fun f ->
            match !mode with
            | Normal ->
              sent := f :: !sent;
              Lwt.return_unit
            | Reject_after gate ->
              Lwt.bind gate (fun () ->
                  incr refused;
                  Lwt.fail Stdlib.Exit))
      ; receive_frame = (fun () -> Lwt_stream.get incoming)
      }
  ; push
  ; frames = (fun () -> List.rev !sent)
  ; arm = (fun gate -> mode := Reject_after gate)
  ; refused = (fun () -> !refused)
  }

(* The same socket shape over the fuel app's own functor instance: the
   transport records are per-functor types, so the builder cannot be shared. *)
type flink =
  { ftransport : Fserver.live_transport
  ; fpush : string option -> unit
  }

let flink () : flink =
  let incoming, push = Lwt_stream.create () in
  { ftransport =
      { Fserver.send_frame = (fun (_ : string) -> Lwt.return_unit)
      ; receive_frame = (fun () -> Lwt_stream.get incoming)
      }
  ; fpush = push
  }

let decoded (f : string) : App.model Tea_core.Wire.down option =
  Codec.down_of_json f |> Result.to_option

let acks (l : link) : int list =
  List.filter_map
    (fun f ->
      Option.bind (decoded f) (fun d ->
          match d with
          | Tea_core.Wire.Ack n -> Some (Msg_seq.to_int n)
          | Tea_core.Wire.Head (_ : App.model) -> None
          | Tea_core.Wire.Hello ((_ : Replica.t), (_ : App.model)) -> None))
    (l.frames ())

let frame ~(tab : Tab_id.t) ~(seq : int) (msg : App.msg) : string =
  Codec.up_to_json
    (Tea_core.Wire.Apply { tab = Tab_id.to_string tab; seq; msg })

let fframe ~(tab : Tab_id.t) ~(seq : int) (msg : Fuel.msg) : string =
  Fcodec.up_to_json
    (Tea_core.Wire.Apply { tab = Tab_id.to_string tab; seq; msg })

let is_advance_for ~(replica : Replica.t) ~(tab : Tab_id.t) ~(seq : int) :
    Sink.event -> bool = function
  | Sink.Advance
      { replica = r
      ; tab = t
      ; seq = n
      ; water = (_ : Tea_core.Prim.Store_water.t)
      } ->
    Replica.equal r replica
    && String.equal (Tab_id.to_string t) (Tab_id.to_string tab)
    && Msg_seq.to_int n = seq
  | Sink.Forget { replica = (_ : Replica.t) } -> false

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let* repo = Server.Store.create () in
     (* --- 1+2. The internal race (W1), closed by construction ------------- *)
     (* The death fires INSIDE the take-to-ack window: the transport is armed
        before the session opens, so the sender parks mid-send on the [Hello]
        frame, and [?interpose], running between the witnessed read and the
        commit, releases that send into its rejection while [step_ws] is still
        suspended. No competing commit is involved: the replica is minted from
        the branch name, so a same-branch foreign increment would join with
        the span's by max and make the apply count unmeasurable
        (contention_test.ml's Tags_app comment names this exact trap). The old
        [Lwt.pick] teardown cancelled the pump right here; the restructure
        must instead let the span run out, floor, and only then observe the
        death. *)
     let* s1 = Server.Store.session repo (sid "w1race") in
     let replica1 = Tea_core.Crdt.Ctx.replica (Server.Store.ctx_of_session s1) in
     let sink1, recorded1 = Sink.memory () in
     let guard1 =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
         ~sink:sink1 ~floors:Dguard.Floors.empty ()
     in
     let l1 = link () in
     let gate1, wake1 = Lwt.task () in
     let kill1, kill_wake1 = Lwt.task () in
     let in_window1 = ref false in
     let interpose1 () =
       (* One-shot by construction: this session carries exactly one frame,
          so the wakeup below cannot double-resolve. *)
       Lwt.wakeup_later kill_wake1 ();
       let* fired = await (fun () -> l1.refused () > 0) in
       in_window1 := fired;
       gate1
     in
     l1.arm kill1;
     let session1 =
       Server.live_session ~guard:guard1 ~interpose:interpose1 s1 l1.transport
     in
     let tab1 = tab_of 61 in
     l1.push (Some (frame ~tab:tab1 ~seq:1 App.Increment));
     let* windowed = await (fun () -> !in_window1) in
     check "s1: the send death fired inside the take-to-ack window" windowed;
     let* () = Lwt_unix.sleep 0.05 in
     check "s2: the session promise waits for the in-flight span (still pending)"
       (Lwt.is_sleeping session1);
     let* mid1 = Server.Store.load s1 in
     check "s1: inside the window the span's commit has not landed"
       (App.value mid1 = 0);
     check "s2: no floor is persisted before the span's own persist ran"
       (recorded1 () = []);
     Lwt.wakeup_later wake1 ();
     let* settled1 = await (fun () -> not (Lwt.is_sleeping session1)) in
     check "s1: the session settles once the sender's death is observed" settled1;
     check "s1: the session ended by observing death, not by an exception"
       (match Lwt.state session1 with
        | Lwt.Return () -> true
        | Lwt.Fail (_ : exn) -> false
        | Lwt.Sleep -> false);
     let* after1 = Server.Store.load s1 in
     check
       "s1: the edit still landed exactly once despite the mid-span death"
       (App.value after1 = 1);
     check "s1: the span's floor was persisted before the session settled"
       (match recorded1 () with
        | [ e ] -> is_advance_for ~replica:replica1 ~tab:tab1 ~seq:1 e
        | [] -> false
        | _ :: _ :: _ -> false);
     (* The reconnect ladder: the replay must read Duplicate only now, with
        the effect already applied — acked-without-applying is the W1 shape
        this whole scenario exists to refuse. *)
     let* hist1 = Server.Store.history s1 in
     let l1b = link () in
     let session1b = Server.live_session ~guard:guard1 s1 l1b.transport in
     l1b.push (Some (frame ~tab:tab1 ~seq:1 App.Increment));
     let* re1 = await (fun () -> acks l1b = [ 1 ]) in
     check "s1: the reconnect replay reads Duplicate after the span completed"
       re1;
     let* final1 = Server.Store.load s1 in
     let* hist1b = Server.Store.history s1 in
     check
       "s1: the replay was acked with the edit already applied (no second \
        apply, no loss)"
       (App.value final1 = 1 && List.length hist1b = List.length hist1);
     l1b.push None;
     let* (_ : bool) = await (fun () -> not (Lwt.is_sleeping session1b)) in
     (* --- 3. The rejection door: a handle_frame failure tears down
        unconditionally AND releases the seq for retry --------------------- *)
     (* A rejecting [?interpose] stands in for a store layer that throws mid
        span. The promise must settle promptly (finalize runs on rejection),
        no frame may be minted after teardown, and the rejection is OBSERVED
        through Lwt.catch, never raised. Round 2: the barrier inside the
        protected span releases the taken seq before re-failing (the keyed
        HTTP tier's R27, mirrored), so the reconnect ladder below must read
        Fresh and apply exactly once - Duplicate-acked-without-apply here is
        the W1 loss this scenario exists to refuse. *)
     let* s3 = Server.Store.session repo (sid "reject") in
     let replica3 = Tea_core.Crdt.Ctx.replica (Server.Store.ctx_of_session s3) in
     let* s3_b = Server.Store.session repo (sid "reject") in
     let ctx3_b = Server.Store.ctx_of_session s3_b in
     let sink3, recorded3 = Sink.memory () in
     let guard3 =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
         ~sink:sink3 ~floors:Dguard.Floors.empty ()
     in
     let l3 = link () in
     let interpose3 () = Lwt.fail Stdlib.Exit in
     let session3 =
       Server.live_session ~guard:guard3 ~interpose:interpose3 s3 l3.transport
     in
     let outcome3 =
       Lwt.catch
         (fun () -> Lwt.map (fun () -> `Fulfilled) session3)
         (fun (e : exn) ->
           Lwt.return
             (match e with
              | Stdlib.Exit -> `Exit
              | (_ : exn) -> `Other))
     in
     let tab3 = tab_of 62 in
     l3.push (Some (frame ~tab:tab3 ~seq:1 App.Increment));
     let* settled3 = await (fun () -> not (Lwt.is_sleeping outcome3)) in
     check "s3: a handle_frame failure still ends the session promptly" settled3;
     check "s3: the failure came through the promise, observed not raised"
       (match Lwt.state outcome3 with
        | Lwt.Return `Exit -> true
        | Lwt.Return `Fulfilled -> false
        | Lwt.Return `Other -> false
        | Lwt.Fail (_ : exn) -> false
        | Lwt.Sleep -> false);
     let frames3 = List.length (l3.frames ()) in
     let* foreign3 = Server.Store.load_based s3_b in
     let* (_ : Server.Store.committed) =
       Server.Store.commit_based foreign3 ~label:"after-teardown"
         (fst
            (App.update ctx3_b App.Increment
               (Server.Store.based_model foreign3)))
     in
     let* grew3 = await (fun () -> List.length (l3.frames ()) > frames3) in
     (* DECLARED EXCLUSION (round 2): this check proves "no frame is minted
        after teardown", NOT "unwatch ran". [push None] already closed the
        outgoing stream, so a surviving watch callback's push would raise
        Lwt_stream.Closed, which irmin's watch protect swallows and logs:
        the unwatch-dropped mutant is not killable at this public seam, and
        no public store API exposes the registration count. Named here so
        the check is not mistaken for coverage of the unwatch line. *)
     check "s3: no frame is minted after teardown (a post-teardown commit reaches no socket)"
       (not grew3);
     let* v3 = Server.Store.load s3 in
     check "s3: the rejected span applied nothing (rejection preceded the commit)"
       (App.value v3 = 1);
     (* Released for retry, not floored: [release] appends nothing to the
        journal, so before any reconnect the journal is still empty - the
        seq's fate is decided by the replay, never forged by the rejection. *)
     check "s3: the rejection released the seq without forging a floor"
       (recorded3 () = []);
     (* The reconnect ladder (round 2, s1's idiom): the barrier RELEASED the
        rejected seq, so the replay must read Fresh, re-apply, and only NOW
        persist the floor. The store already reads 1 from the foreign
        after-teardown commit, so the witness is the DELTA: one more
        increment and exactly one more commit. *)
     let* hist3 = Server.Store.history s3 in
     let l3b = link () in
     let session3b = Server.live_session ~guard:guard3 s3 l3b.transport in
     l3b.push (Some (frame ~tab:tab3 ~seq:1 App.Increment));
     let* re3 = await (fun () -> acks l3b = [ 1 ]) in
     check "s3: the released seq replays as Fresh and is acked on reconnect" re3;
     let* fin3 = Server.Store.load s3 in
     let* hist3b = Server.Store.history s3 in
     check
       "s3: the replay applied the effect exactly once (one increment, one \
        new commit)"
       (App.value fin3 = App.value v3 + 1
       && List.length hist3b = List.length hist3 + 1);
     check "s3: the floor is NOW persisted, by the replay the release licensed"
       (match recorded3 () with
        | [ e ] -> is_advance_for ~replica:replica3 ~tab:tab3 ~seq:1 e
        | [] -> false
        | _ :: _ :: _ -> false);
     l3b.push None;
     let* (_ : bool) = await (fun () -> not (Lwt.is_sleeping session3b)) in
     (* --- 4. External cancellation, Fresh arm: the protected orphan ------- *)
     (* [Lwt.cancel] on the promise [live_session] returned, with the span
        gate-held between step and commit. The wrapper must reject at once
        (the pump is dead, seq 2 stays unconsumed) while the body completes
        as an orphan: applied, floored, and NEVER acked — the licensed
        visible-duplicate shape, resolved by the reconnect ladder. *)
     let* s4 = Server.Store.session repo (sid "cancel") in
     let replica4 = Tea_core.Crdt.Ctx.replica (Server.Store.ctx_of_session s4) in
     let sink4, recorded4 = Sink.memory () in
     let guard4 =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
         ~sink:sink4 ~floors:Dguard.Floors.empty ()
     in
     let l4 = link () in
     let gate4, wake4 = Lwt.task () in
     let calls4 = ref 0 in
     let interpose4 () =
       incr calls4;
       if !calls4 = 1 then gate4 else Lwt.return_unit
     in
     let session4 =
       Server.live_session ~guard:guard4 ~interpose:interpose4 s4 l4.transport
     in
     let outcome4 =
       Lwt.catch
         (fun () -> Lwt.map (fun () -> `Fulfilled) session4)
         (fun (e : exn) ->
           Lwt.return
             (match e with
              | Lwt.Canceled -> `Canceled
              | (_ : exn) -> `Other))
     in
     let tab4 = tab_of 63 in
     l4.push (Some (frame ~tab:tab4 ~seq:1 App.Increment));
     let* held4 = await (fun () -> !calls4 = 1) in
     check "s4: the span is in flight when the cancel lands" held4;
     Lwt.cancel session4;
     let* cancelled4 = await (fun () -> not (Lwt.is_sleeping outcome4)) in
     check
       "s4: cancelling live_session settles its promise promptly (the wrapper \
        rejects, the body does not)"
       cancelled4;
     check "s4: the cancellation came through as Canceled, observed not raised"
       (match Lwt.state outcome4 with
        | Lwt.Return `Canceled -> true
        | Lwt.Return `Fulfilled -> false
        | Lwt.Return `Other -> false
        | Lwt.Fail (_ : exn) -> false
        | Lwt.Sleep -> false);
     let* mid4 = Server.Store.load s4 in
     check "s4: at cancel time the span's commit has not landed (orphan in flight)"
       (App.value mid4 = 0);
     l4.push (Some (frame ~tab:tab4 ~seq:2 App.Increment));
     Lwt.wakeup_later wake4 ();
     let* landed4 =
       await (fun () ->
           List.exists
             (is_advance_for ~replica:replica4 ~tab:tab4 ~seq:1)
             (recorded4 ()))
     in
     let* () = Lwt_unix.sleep 0.3 in
     let* v4 = Server.Store.load s4 in
     check "s4: the protected body ran to completion as an orphan: the edit applied"
       (landed4 && App.value v4 = 1);
     check "s4: the orphan persisted exactly one floor, for seq 1"
       (match recorded4 () with
        | [ e ] -> is_advance_for ~replica:replica4 ~tab:tab4 ~seq:1 e
        | [] -> false
        | _ :: _ :: _ -> false);
     check "s4: the pump did not recurse past the cancel (seq 2 unconsumed, unacked)"
       (App.value v4 = 1 && not (List.mem 2 (acks l4)));
     check
       "s4: no ack was minted for the orphaned span (a licensed visible \
        duplicate, never a silent loss)"
       (not (List.mem 1 (acks l4)));
     let l4b = link () in
     let session4b = Server.live_session ~guard:guard4 s4 l4b.transport in
     l4b.push (Some (frame ~tab:tab4 ~seq:1 App.Increment));
     let* re4 = await (fun () -> acks l4b = [ 1 ]) in
     let* fin4 = Server.Store.load s4 in
     check
       "s4: the reconnect replay is acked as Duplicate with the edit applied \
        exactly once"
       (re4 && App.value fin4 = 1);
     l4b.push None;
     let* (_ : bool) = await (fun () -> not (Lwt.is_sleeping session4b)) in
     (* --- 6. No release races the parked orphan --------------------------- *)
     (* A wrapper cancellation must release NOTHING: the orphan is still
        running and owns the seq's fate (round 3 makes this positional - the
        inner catch never even sees the wrapper's [Canceled]). The killer
        interleaving for the opposite mutant (a release firing at
        wrapper-cancel time, e.g. the catch hoisted back outside [protected]
        with an unconditional release) is a replay that arrives WHILE the
        orphan is parked mid-apply: a released seq would read Fresh and forge
        a second apply, so the replay must read Duplicate, be acked with no
        second take, and leave the floor to the orphan's own write. s4 cannot
        see this: there the orphan finishes and restores the water before the
        reconnect ladder runs. *)
     let* s6 = Server.Store.session repo (sid "cancelrace") in
     let replica6 = Tea_core.Crdt.Ctx.replica (Server.Store.ctx_of_session s6) in
     let sink6, recorded6 = Sink.memory () in
     let guard6 =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
         ~sink:sink6 ~floors:Dguard.Floors.empty ()
     in
     let l6 = link () in
     let gate6, wake6 = Lwt.task () in
     let calls6 = ref 0 in
     let interpose6 () =
       incr calls6;
       if !calls6 = 1 then gate6 else Lwt.return_unit
     in
     let session6 =
       Server.live_session ~guard:guard6 ~interpose:interpose6 s6 l6.transport
     in
     let outcome6 =
       Lwt.catch (fun () -> session6) (fun (_ : exn) -> Lwt.return_unit)
     in
     let tab6 = tab_of 65 in
     l6.push (Some (frame ~tab:tab6 ~seq:1 App.Increment));
     let* (_ : bool) = await (fun () -> !calls6 = 1) in
     let* hist6 = Server.Store.history s6 in
     Lwt.cancel session6;
     let* (_ : bool) = await (fun () -> not (Lwt.is_sleeping outcome6)) in
     (* The orphan is still parked on [gate6]; the replay races it. *)
     let l6b = link () in
     let session6b = Server.live_session ~guard:guard6 s6 l6b.transport in
     l6b.push (Some (frame ~tab:tab6 ~seq:1 App.Increment));
     (* Step 17 re-cut: the replay must PARK, not ack (F5'). The positive
        witness comes first - the row is open on the default [Server.park]
        (neither socket passed [?park]) - so the negative window after it
        cannot pass vacuously on a frame that never reached the Duplicate
        arm. A positive await on the ack here would burn its whole budget
        post-fix. *)
     let* parked6 =
       await (fun () ->
           Park.parked_count Server.park ~replica:replica6 ~tab:tab6 = 1)
     in
     let* () = Lwt_unix.sleep 0.05 in
     let* mid6 = Server.Store.load s6 in
     let* hist6mid = Server.Store.history s6 in
     check
       "s6: the canceled seq replays as Duplicate and PARKS on the in-flight \
        orphan (no ack yet, no second take, orphan still owns the seq)"
       (parked6
       && acks l6b = []
       && App.value mid6 = 0
       && List.length hist6mid = List.length hist6
       && recorded6 () = []);
     Lwt.wakeup_later wake6 ();
     let* landed6 =
       await (fun () ->
           List.exists
             (is_advance_for ~replica:replica6 ~tab:tab6 ~seq:1)
             (recorded6 ()))
     in
     let* () = Lwt_unix.sleep 0.3 in
     (* The conjunct the old check1 owned, moved here: the parked ack fires
        once the orphan LANDS, on the duplicate's own socket. *)
     let* ackd6 = await (fun () -> acks l6b = [ 1 ]) in
     let* fin6 = Server.Store.load s6 in
     let* hist6b = Server.Store.history s6 in
     check
       "s6: after the orphan unparks the effect applied exactly once and the \
        parked ack fires on the duplicate's own socket"
       (landed6 && ackd6
       && App.value fin6 = 1
       && List.length hist6b = List.length hist6 + 1);
     check
       "s6: the floor was persisted at the canceled seq by the orphan's own \
        write"
       (match recorded6 () with
        | [ e ] -> is_advance_for ~replica:replica6 ~tab:tab6 ~seq:1 e
        | [] -> false
        | _ :: _ :: _ -> false);
     l6b.push None;
     let* (_ : bool) = await (fun () -> not (Lwt.is_sleeping session6b)) in
     (* --- 7. Cancel, THEN the orphan rejects: the release still fires ----- *)
     (* The composition round 2 dropped (F1'), plus its cousin (F3'): the
        wrapper is cancelled first, so the outer promise is already settled,
        and only THEN does the parked orphan's body reject - here with a
        body-internal [Canceled], minted by cancelling the gate the orphan
        is parked on, so this one ladder kills both "catch hoisted back
        outside [protected]" (the orphan's late rejection would be dropped
        before any release) and "a Canceled arm reintroduced inside the
        catch" (the body-internal Canceled would be misread as
        orphan-alive). Under the round-3 barrier the inner catch fires IN
        the orphan, releases, and re-fails into [protected]'s already
        settled promise (dropped, by design - the release has already
        happened). No commit, no floor: the seq's fate belongs to the
        reconnect replay, which must read Fresh and apply exactly once.
        Check anatomy, so nobody overreads the first two: the no-forge
        check is a PRECONDITION (a still-parked orphan satisfies it too),
        and the reconnect ack cannot tell Fresh from Duplicate (both are
        acked) - the kill is carried by the apply-count and floor checks
        that close the ladder. *)
     let* s7 = Server.Store.session repo (sid "cancelreject") in
     let replica7 = Tea_core.Crdt.Ctx.replica (Server.Store.ctx_of_session s7) in
     let sink7, recorded7 = Sink.memory () in
     let guard7 =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
         ~sink:sink7 ~floors:Dguard.Floors.empty ()
     in
     let l7 = link () in
     let gate7, (_ : unit Lwt.u) = Lwt.task () in
     let calls7 = ref 0 in
     let interpose7 () =
       incr calls7;
       if !calls7 = 1 then gate7 else Lwt.return_unit
     in
     let session7 =
       Server.live_session ~guard:guard7 ~interpose:interpose7 s7 l7.transport
     in
     let outcome7 =
       Lwt.catch (fun () -> session7) (fun (_ : exn) -> Lwt.return_unit)
     in
     let tab7 = tab_of 66 in
     l7.push (Some (frame ~tab:tab7 ~seq:1 App.Increment));
     let* (_ : bool) = await (fun () -> !calls7 = 1) in
     let* hist7 = Server.Store.history s7 in
     Lwt.cancel session7;
     let* (_ : bool) = await (fun () -> not (Lwt.is_sleeping outcome7)) in
     (* The wrapper is dead; the orphan is parked on [gate7]. Now reject the
        orphan from inside: cancelling the gate rejects it with [Canceled],
        which the bind chain carries up through [step_ws]'s span - a
        body-internal Canceled, with no second orphan behind it. *)
     Lwt.cancel gate7;
     (* One scheduler turn for the rejection to propagate through the inner
        catch (the release itself is synchronous once the handler runs). *)
     let* () = Lwt.pause () in
     let* v7 = Server.Store.load s7 in
     let* hist7mid = Server.Store.history s7 in
     check
       "s7: the rejected orphan committed nothing and forged no floor \
        (release, not a claim)"
       (App.value v7 = 0
       && List.length hist7mid = List.length hist7
       && recorded7 () = []);
     (* The reconnect ladder: the release fired inside the orphan, so the
        replay must read Fresh - Duplicate-acked-without-apply here is
        exactly the W1 loss the round-2 barrier still allowed. *)
     let l7b = link () in
     let session7b = Server.live_session ~guard:guard7 s7 l7b.transport in
     l7b.push (Some (frame ~tab:tab7 ~seq:1 App.Increment));
     let* re7 = await (fun () -> acks l7b = [ 1 ]) in
     check "s7: the released seq replays as Fresh and is acked on reconnect" re7;
     let* fin7 = Server.Store.load s7 in
     let* hist7b = Server.Store.history s7 in
     check
       "s7: the replay applied the effect exactly once (one increment, one \
        new commit)"
       (App.value fin7 = 1 && List.length hist7b = List.length hist7 + 1);
     check "s7: the floor landed via the replay the release licensed"
       (match recorded7 () with
        | [ e ] -> is_advance_for ~replica:replica7 ~tab:tab7 ~seq:1 e
        | [] -> false
        | _ :: _ :: _ -> false);
     l7b.push None;
     let* (_ : bool) = await (fun () -> not (Lwt.is_sleeping session7b)) in
     (* --- 5. External cancellation, fuel arm: the once-ever bottom floor -- *)
     (* The fuel arm never reaches [?interpose] (interpose sits in the ok
        path), so the gate moves into the SINK: the bottom floor's append is
        held while the cancel lands, proving the persist itself sits inside
        the protected body and survives as part of the orphan. *)
     let* frepo = Fserver.Store.create () in
     let* s5 = Fserver.Store.session frepo (sid "fuelcancel") in
     let replica5 =
       Tea_core.Crdt.Ctx.replica (Fserver.Store.ctx_of_session s5)
     in
     let sink5, recorded5 = Sink.memory () in
     let gate5, wake5 = Lwt.task () in
     let attempted5 = ref false in
     let gated5 : Sink.t =
       { Sink.append =
           (fun (ev : Sink.event) ->
             attempted5 := true;
             let* () = gate5 in
             sink5.Sink.append ev)
       }
     in
     let guard5 =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
         ~sink:gated5 ~floors:Dguard.Floors.empty ()
     in
     let l5 = flink () in
     let session5 = Fserver.live_session ~guard:guard5 s5 l5.ftransport in
     let outcome5 =
       Lwt.catch
         (fun () -> Lwt.map (fun () -> `Fulfilled) session5)
         (fun (e : exn) ->
           Lwt.return
             (match e with
              | Lwt.Canceled -> `Canceled
              | (_ : exn) -> `Other))
     in
     let tab5 = tab_of 64 in
     (* The commit witness, captured BEFORE the poison push: fuel exhaustion
        must commit nothing, and the model is a vacuous observable for that
        (Fuel.init = 0 and Spin is model-identity), so the witness is the
        store history length staying flat across the whole poison span. *)
     let* fhist5 = Fserver.Store.history s5 in
     l5.fpush (Some (fframe ~tab:tab5 ~seq:1 Fuel.Spin));
     let* att5 = await (fun () -> !attempted5) in
     check "s5: the fuel arm reached its bottom-floor persist (append attempted)"
       att5;
     Lwt.cancel session5;
     let* cancelled5 = await (fun () -> not (Lwt.is_sleeping outcome5)) in
     check "s5: cancel during the fuel arm's persist settles the session promptly"
       cancelled5;
     check "s5: the fuel-arm cancellation came through as Canceled"
       (match Lwt.state outcome5 with
        | Lwt.Return `Canceled -> true
        | Lwt.Return `Fulfilled -> false
        | Lwt.Return `Other -> false
        | Lwt.Fail (_ : exn) -> false
        | Lwt.Sleep -> false);
     check "s5: the floor had not landed when the session settled"
       (recorded5 () = []);
     Lwt.wakeup_later wake5 ();
     let* landed5 = await (fun () -> recorded5 () <> []) in
     check "s5: the orphan persisted the once-ever bottom floor for the poison seq"
       (landed5
       && (match recorded5 () with
           | [ e ] -> is_advance_for ~replica:replica5 ~tab:tab5 ~seq:1 e
           | [] -> false
           | _ :: _ :: _ -> false));
     let* fhist5b = Fserver.Store.history s5 in
     check
       "s5: fuel exhaustion committed nothing: history unchanged across the \
        poison span (bottom floor, no claim)"
       (List.length fhist5b = List.length fhist5);
     let guard5b =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
         ~sink:Sink.null ~floors:(Dguard.Floors.of_events (recorded5 ())) ()
     in
     check "s5: a guard rebuilt from the journal refuses the poison seq (once ever)"
       (match
          Dguard.take guard5b ~replica:replica5 ~tab:tab5
            ~seq:(must "seq 1" (Msg_seq.of_int 1))
        with
        | Guard.Duplicate n -> Msg_seq.to_int n = 1
        | Guard.Fresh (_ : Msg_seq.t) -> false
        | Guard.Gapped -> false);
     (* --- 8. F5' closed, rejection side: the parked ack is DROPPED -------- *)
     (* The one loss window step 16 declared open (step 17, D22): a cancel
        orphans the attempt, the replay parks on it, the orphan rejects and
        releases - the old unconditional Duplicate ack asserted "consumed"
        for an effect that never landed, and the client dropped the message
        for good. The ladder proves the ack is dropped, the registry row is
        cleared, and the seq's fate belongs to the retry, which reads Fresh
        and applies exactly once. *)
     let* s8 = Server.Store.session repo (sid "fivereject") in
     let replica8 = Tea_core.Crdt.Ctx.replica (Server.Store.ctx_of_session s8) in
     let sink8, recorded8 = Sink.memory () in
     let guard8 =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
         ~sink:sink8 ~floors:Dguard.Floors.empty ()
     in
     let park8 = Park.create () in
     let l8 = link () in
     let gate8, (_ : unit Lwt.u) = Lwt.task () in
     let calls8 = ref 0 in
     let interpose8 () =
       incr calls8;
       if !calls8 = 1 then gate8 else Lwt.return_unit
     in
     let session8 =
       Server.live_session ~guard:guard8 ~park:park8 ~interpose:interpose8 s8
         l8.transport
     in
     let outcome8 =
       Lwt.catch (fun () -> session8) (fun (_ : exn) -> Lwt.return_unit)
     in
     let tab8 = tab_of 67 in
     l8.push (Some (frame ~tab:tab8 ~seq:1 App.Increment));
     let* (_ : bool) = await (fun () -> !calls8 = 1) in
     let* hist8 = Server.Store.history s8 in
     Lwt.cancel session8;
     let* (_ : bool) = await (fun () -> not (Lwt.is_sleeping outcome8)) in
     let l8b = link () in
     let session8b =
       Server.live_session ~guard:guard8 ~park:park8 s8 l8b.transport
     in
     l8b.push (Some (frame ~tab:tab8 ~seq:1 App.Increment));
     let* parked8 =
       await (fun () -> Park.parked_count park8 ~replica:replica8 ~tab:tab8 = 1)
     in
     let* () = Lwt_unix.sleep 0.05 in
     let* mid8 = Server.Store.load s8 in
     check
       "s8: the duplicate replay parks on the in-flight orphan (row open, no \
        ack, no apply, no floor)"
       (parked8 && acks l8b = [] && App.value mid8 = 0 && recorded8 () = []);
     Lwt.cancel gate8;
     let* () = Lwt.pause () in
     let* v8 = Server.Store.load s8 in
     let* hist8mid = Server.Store.history s8 in
     check
       "s8: the rejected orphan released the seq without applying or forging \
        a floor"
       (App.value v8 = 0
       && List.length hist8mid = List.length hist8
       && recorded8 () = []);
     let* () = Lwt_unix.sleep 0.05 in
     check
       "s8: the parked ack is DROPPED on the release and the row is cleared \
        (F5' closed: no consumed claim for an effect that never landed)"
       (acks l8b = []
       && Park.parked_count park8 ~replica:replica8 ~tab:tab8 = 0);
     let l8c = link () in
     let session8c =
       Server.live_session ~guard:guard8 ~park:park8 s8 l8c.transport
     in
     l8c.push (Some (frame ~tab:tab8 ~seq:1 App.Increment));
     let* re8 = await (fun () -> acks l8c = [ 1 ]) in
     check "s8: the retry reads Fresh after the release and is acked" re8;
     let* fin8 = Server.Store.load s8 in
     let* hist8b = Server.Store.history s8 in
     check
       "s8: the effect applied exactly once, via the retry (one increment, \
        one new commit)"
       (App.value fin8 = 1 && List.length hist8b = List.length hist8 + 1);
     check "s8: the floor landed via the retry the release licensed"
       (match recorded8 () with
        | [ e ] -> is_advance_for ~replica:replica8 ~tab:tab8 ~seq:1 e
        | [] -> false
        | _ :: _ :: _ -> false);
     l8b.push None;
     l8c.push None;
     let* (_ : bool) = await (fun () -> not (Lwt.is_sleeping session8b)) in
     let* (_ : bool) = await (fun () -> not (Lwt.is_sleeping session8c)) in
     (* --- 9. F5' success side: the parked ack FIRES, on the parking socket - *)
     let* s9 = Server.Store.session repo (sid "fivesuccess") in
     let replica9 = Tea_core.Crdt.Ctx.replica (Server.Store.ctx_of_session s9) in
     let sink9, recorded9 = Sink.memory () in
     let guard9 =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
         ~sink:sink9 ~floors:Dguard.Floors.empty ()
     in
     let park9 = Park.create () in
     let l9 = link () in
     let gate9, wake9 = Lwt.task () in
     let calls9 = ref 0 in
     let interpose9 () =
       incr calls9;
       if !calls9 = 1 then gate9 else Lwt.return_unit
     in
     let session9 =
       Server.live_session ~guard:guard9 ~park:park9 ~interpose:interpose9 s9
         l9.transport
     in
     let outcome9 =
       Lwt.catch (fun () -> session9) (fun (_ : exn) -> Lwt.return_unit)
     in
     let tab9 = tab_of 68 in
     l9.push (Some (frame ~tab:tab9 ~seq:1 App.Increment));
     let* (_ : bool) = await (fun () -> !calls9 = 1) in
     let* hist9 = Server.Store.history s9 in
     Lwt.cancel session9;
     let* (_ : bool) = await (fun () -> not (Lwt.is_sleeping outcome9)) in
     let l9b = link () in
     let session9b =
       Server.live_session ~guard:guard9 ~park:park9 s9 l9b.transport
     in
     l9b.push (Some (frame ~tab:tab9 ~seq:1 App.Increment));
     let* parked9 =
       await (fun () -> Park.parked_count park9 ~replica:replica9 ~tab:tab9 = 1)
     in
     let* () = Lwt_unix.sleep 0.05 in
     check "s9: the duplicate replay parks on the in-flight orphan (no early ack)"
       (parked9 && acks l9b = []);
     Lwt.wakeup_later wake9 ();
     let* ackd9 = await (fun () -> acks l9b = [ 1 ]) in
     check
       "s9: the parked ack fires on the duplicate's own socket once the \
        orphan lands"
       ackd9;
     let* fin9 = Server.Store.load s9 in
     let* hist9b = Server.Store.history s9 in
     check
       "s9: the effect applied exactly once, by the orphan's own commit \
        (never re-applied by the parked duplicate)"
       (App.value fin9 = 1
       && List.length hist9b = List.length hist9 + 1
       && (match recorded9 () with
           | [ e ] -> is_advance_for ~replica:replica9 ~tab:tab9 ~seq:1 e
           | [] -> false
           | _ :: _ :: _ -> false));
     check "s9: the settled row is removed (registry cleared, not leaked)"
       (Park.parked_count park9 ~replica:replica9 ~tab:tab9 = 0);
     l9b.push None;
     let* (_ : bool) = await (fun () -> not (Lwt.is_sleeping session9b)) in
     (* --- 10. Teardown while parked: the park is bounded by the attempt --- *)
     (* The parked socket's client goes away (stream end) while the wait is
        open. A stream end is only ever observed BETWEEN frames - the Fresh
        arm's own span semantics, unchanged by step 17 - so the session does
        not settle while the attempt is in flight; it settles promptly at
        the attempt's own exit, and the parked socket's death never perturbs
        the attempt. *)
     let* s10 = Server.Store.session repo (sid "parkteardown") in
     let replica10 =
       Tea_core.Crdt.Ctx.replica (Server.Store.ctx_of_session s10)
     in
     let sink10, recorded10 = Sink.memory () in
     let guard10 =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
         ~sink:sink10 ~floors:Dguard.Floors.empty ()
     in
     let park10 = Park.create () in
     let l10 = link () in
     let gate10, wake10 = Lwt.task () in
     let calls10 = ref 0 in
     let interpose10 () =
       incr calls10;
       if !calls10 = 1 then gate10 else Lwt.return_unit
     in
     let session10 =
       Server.live_session ~guard:guard10 ~park:park10 ~interpose:interpose10
         s10 l10.transport
     in
     let outcome10 =
       Lwt.catch (fun () -> session10) (fun (_ : exn) -> Lwt.return_unit)
     in
     let tab10 = tab_of 69 in
     l10.push (Some (frame ~tab:tab10 ~seq:1 App.Increment));
     let* (_ : bool) = await (fun () -> !calls10 = 1) in
     let* hist10 = Server.Store.history s10 in
     Lwt.cancel session10;
     let* (_ : bool) = await (fun () -> not (Lwt.is_sleeping outcome10)) in
     let l10b = link () in
     let session10b =
       Server.live_session ~guard:guard10 ~park:park10 s10 l10b.transport
     in
     l10b.push (Some (frame ~tab:tab10 ~seq:1 App.Increment));
     let* parked10 =
       await (fun () ->
           Park.parked_count park10 ~replica:replica10 ~tab:tab10 = 1)
     in
     check "s10: the duplicate is parked before its client goes away (row open)"
       parked10;
     l10b.push None;
     let* () = Lwt_unix.sleep 0.05 in
     check
       "s10: a stream end is observed between frames, so the parked session \
        does not settle while the attempt is in flight (the park is bounded \
        by the attempt, as the Fresh arm's own span bounds the pump)"
       (Lwt.is_sleeping session10b);
     Lwt.wakeup_later wake10 ();
     let* settled10 = await (fun () -> not (Lwt.is_sleeping session10b)) in
     let* fin10 = Server.Store.load s10 in
     let* hist10b = Server.Store.history s10 in
     check
       "s10: the attempt lands unperturbed and the parked socket's teardown \
        completes at the attempt's own exit (effect once, floor present)"
       (settled10
       && App.value fin10 = 1
       && List.length hist10b = List.length hist10 + 1
       && (match recorded10 () with
           | [ e ] -> is_advance_for ~replica:replica10 ~tab:tab10 ~seq:1 e
           | [] -> false
           | _ :: _ :: _ -> false));
     (* --- 11. The parked ack waits for the PERSIST, not local success ----- *)
     (* The settle sits after [persist_taken], and this ladder pins that
        ordering with a gated sink (the s5 idiom) on the ok arm: step_ws
        succeeding locally must not fire the parked ack. *)
     let* s11 = Server.Store.session repo (sid "gatedpersist") in
     let replica11 =
       Tea_core.Crdt.Ctx.replica (Server.Store.ctx_of_session s11)
     in
     let sink11, recorded11 = Sink.memory () in
     let gate11, wake11 = Lwt.task () in
     let attempted11 = ref false in
     let gated11 : Sink.t =
       { Sink.append =
           (fun (ev : Sink.event) ->
             attempted11 := true;
             let* () = gate11 in
             sink11.Sink.append ev)
       }
     in
     let guard11 =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
         ~sink:gated11 ~floors:Dguard.Floors.empty ()
     in
     let park11 = Park.create () in
     let l11 = link () in
     let session11 =
       Server.live_session ~guard:guard11 ~park:park11 s11 l11.transport
     in
     let tab11 = tab_of 70 in
     l11.push (Some (frame ~tab:tab11 ~seq:1 App.Increment));
     let* (_ : bool) = await (fun () -> !attempted11) in
     let l11b = link () in
     let session11b =
       Server.live_session ~guard:guard11 ~park:park11 s11 l11b.transport
     in
     l11b.push (Some (frame ~tab:tab11 ~seq:1 App.Increment));
     let* parked11 =
       await (fun () ->
           Park.parked_count park11 ~replica:replica11 ~tab:tab11 = 1)
     in
     let* () = Lwt_unix.sleep 0.05 in
     check
       "s11: no parked ack while the floor's append is still gated - step_ws \
        succeeding locally is not enough"
       (parked11 && acks l11b = []);
     Lwt.wakeup_later wake11 ();
     let* ackd11 = await (fun () -> acks l11b = [ 1 ]) in
     check
       "s11: the parked ack fires only once the persist actually lands \
        (floor present)"
       (ackd11
       && (match recorded11 () with
           | [ e ] -> is_advance_for ~replica:replica11 ~tab:tab11 ~seq:1 e
           | [] -> false
           | _ :: _ :: _ -> false));
     let* fin11 = Server.Store.load s11 in
     check "s11: the effect applied exactly once" (App.value fin11 = 1);
     l11.push None;
     l11b.push None;
     let* (_ : bool) = await (fun () -> not (Lwt.is_sleeping session11)) in
     let* (_ : bool) = await (fun () -> not (Lwt.is_sleeping session11b)) in
     (* --- 12. Below-water replays park on the WATER's row ----------------- *)
     (* [Duplicate] carries the high water ([Duplicate e.high] in
        replay_guard.ml), an ack that asserts "everything at or below the
        water is consumed". A below-water replay arriving while the water's
        own attempt is still in flight must therefore park on the WATER's
        row - acking 2 for a replayed 1 while 2 is unlanded is exactly the
        F5' overclaim - and fire, with the water's number, only once that
        attempt lands. *)
     let* s12 = Server.Store.session repo (sid "seqkeyed") in
     let replica12 =
       Tea_core.Crdt.Ctx.replica (Server.Store.ctx_of_session s12)
     in
     let guard12 =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
         ~sink:Sink.null ~floors:Dguard.Floors.empty ()
     in
     let park12 = Park.create () in
     let gate12, wake12 = Lwt.task () in
     let calls12 = ref 0 in
     let interpose12 () =
       incr calls12;
       if !calls12 = 2 then gate12 else Lwt.return_unit
     in
     let l12 = link () in
     let session12 =
       Server.live_session ~guard:guard12 ~park:park12 ~interpose:interpose12
         s12 l12.transport
     in
     let tab12 = tab_of 71 in
     l12.push (Some (frame ~tab:tab12 ~seq:1 App.Increment));
     let* (_ : bool) = await (fun () -> acks l12 = [ 1 ]) in
     l12.push (Some (frame ~tab:tab12 ~seq:2 App.Increment));
     let* (_ : bool) = await (fun () -> !calls12 = 2) in
     let l12b = link () in
     let session12b =
       Server.live_session ~guard:guard12 ~park:park12 s12 l12b.transport
     in
     l12b.push (Some (frame ~tab:tab12 ~seq:1 App.Increment));
     let* parked12 =
       await (fun () ->
           Park.parked_count park12 ~replica:replica12 ~tab:tab12 = 1)
     in
     let* () = Lwt_unix.sleep 0.05 in
     check
       "s12: a below-water replay parks on the WATER's open row (the \
        cumulative ack is withheld while the water's attempt is in flight)"
       (parked12 && acks l12b = []);
     Lwt.wakeup_later wake12 ();
     let* ack12 = await (fun () -> acks l12 = [ 1; 2 ]) in
     let* ackb12 = await (fun () -> acks l12b = [ 2 ]) in
     let* fin12 = Server.Store.load s12 in
     check
       "s12: the gated seq resolves and the parked ack fires with the WATER's \
        number (both acks on the live socket, the water ack on the parked \
        one, two applies, registry empty)"
       (ack12 && ackb12
       && App.value fin12 = 2
       && Park.parked_count park12 ~replica:replica12 ~tab:tab12 = 0);
     l12.push None;
     l12b.push None;
     let* (_ : bool) = await (fun () -> not (Lwt.is_sleeping session12)) in
     let* (_ : bool) = await (fun () -> not (Lwt.is_sleeping session12b)) in
     (* --- 13. Multi-waiter isolation on one shared settlement ------------- *)
     (* Two sockets park on the identical key and share ONE settlement
        promise. Cancelling one parked socket rejects only ITS [protected]
        wrapper: with the row minted by [Lwt.wait] the settlement itself is
        not cancelable, so the sibling still resolves - and the cancelled
        socket's own teardown completes promptly, before the attempt
        settles. *)
     let* s13 = Server.Store.session repo (sid "multiwaiter") in
     let replica13 =
       Tea_core.Crdt.Ctx.replica (Server.Store.ctx_of_session s13)
     in
     let sink13, recorded13 = Sink.memory () in
     let guard13 =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
         ~sink:sink13 ~floors:Dguard.Floors.empty ()
     in
     let park13 = Park.create () in
     let l13 = link () in
     let gate13, wake13 = Lwt.task () in
     let calls13 = ref 0 in
     let interpose13 () =
       incr calls13;
       if !calls13 = 1 then gate13 else Lwt.return_unit
     in
     let session13 =
       Server.live_session ~guard:guard13 ~park:park13 ~interpose:interpose13
         s13 l13.transport
     in
     let outcome13 =
       Lwt.catch (fun () -> session13) (fun (_ : exn) -> Lwt.return_unit)
     in
     let tab13 = tab_of 72 in
     l13.push (Some (frame ~tab:tab13 ~seq:1 App.Increment));
     let* (_ : bool) = await (fun () -> !calls13 = 1) in
     Lwt.cancel session13;
     let* (_ : bool) = await (fun () -> not (Lwt.is_sleeping outcome13)) in
     let l13b = link () in
     let session13b =
       Server.live_session ~guard:guard13 ~park:park13 s13 l13b.transport
     in
     let outcome13b =
       Lwt.catch (fun () -> session13b) (fun (_ : exn) -> Lwt.return_unit)
     in
     let l13c = link () in
     let session13c =
       Server.live_session ~guard:guard13 ~park:park13 s13 l13c.transport
     in
     l13b.push (Some (frame ~tab:tab13 ~seq:1 App.Increment));
     l13c.push (Some (frame ~tab:tab13 ~seq:1 App.Increment));
     let* parked13 =
       await (fun () ->
           Park.parked_count park13 ~replica:replica13 ~tab:tab13 = 1)
     in
     let* () = Lwt_unix.sleep 0.05 in
     check
       "s13: the row stays open while two duplicates park on it (no ack on \
        either while the orphan is in flight)"
       (parked13 && acks l13b = [] && acks l13c = []);
     Lwt.cancel session13b;
     let* settled13b = await (fun () -> not (Lwt.is_sleeping outcome13b)) in
     Lwt.wakeup_later wake13 ();
     let* ackd13 = await (fun () -> acks l13c = [ 1 ]) in
     let* fin13 = Server.Store.load s13 in
     check
       "s13: the surviving waiter still resolves from the shared settlement \
        (its ack fires; the cancelled sibling never acked)"
       (ackd13
       && acks l13b = []
       && App.value fin13 = 1
       && (match recorded13 () with
           | [ e ] -> is_advance_for ~replica:replica13 ~tab:tab13 ~seq:1 e
           | [] -> false
           | _ :: _ :: _ -> false));
     check
       "s13: the cancelled parked socket's own teardown completed promptly, \
        before the attempt settled"
       settled13b;
     l13c.push None;
     let* (_ : bool) = await (fun () -> not (Lwt.is_sleeping session13c)) in
     (* --- 14. A dead sender releases a parked socket ---------------------- *)
     (* The park is raced against [died] exactly as the pump's frame-wait is
        (D22): a settlement that never arrives must not hold the parked
        socket's teardown hostage once its send side is gone. The attempt
        stays gated past the death, so nothing but the race can settle the
        parked session - and the row itself survives, untouched, for any
        sibling waiter. *)
     let* s14 = Server.Store.session repo (sid "deadparked") in
     let replica14 =
       Tea_core.Crdt.Ctx.replica (Server.Store.ctx_of_session s14)
     in
     let sink14, _recorded14 = Sink.memory () in
     let guard14 =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
         ~sink:sink14 ~floors:Dguard.Floors.empty ()
     in
     let park14 = Park.create () in
     let l14 = link () in
     let gate14, wake14 = Lwt.task () in
     let calls14 = ref 0 in
     let interpose14 () =
       incr calls14;
       if !calls14 = 1 then gate14 else Lwt.return_unit
     in
     let session14 =
       Server.live_session ~guard:guard14 ~park:park14 ~interpose:interpose14
         s14 l14.transport
     in
     let tab14 = tab_of 91 in
     l14.push (Some (frame ~tab:tab14 ~seq:1 App.Increment));
     let* (_ : bool) = await (fun () -> !calls14 = 1) in
     let l14b = link () in
     let sgate14, swake14 = Lwt.task () in
     l14b.arm sgate14;
     let session14b =
       Server.live_session ~guard:guard14 ~park:park14 s14 l14b.transport
     in
     let outcome14b =
       Lwt.catch (fun () -> session14b) (fun (_ : exn) -> Lwt.return_unit)
     in
     l14b.push (Some (frame ~tab:tab14 ~seq:1 App.Increment));
     let* () = Lwt_unix.sleep 0.05 in
     check
       "s14: the socket is parked, not dead, while its sender is merely \
        armed (the session promise still sleeps on the open settlement)"
       (Lwt.is_sleeping outcome14b
       && Park.parked_count park14 ~replica:replica14 ~tab:tab14 = 1);
     Lwt.wakeup_later swake14 ();
     let* settled14b = await (fun () -> not (Lwt.is_sleeping outcome14b)) in
     check
       "s14: the dead sender releases the parked socket - teardown completes \
        through the death race while the settlement is still open"
       (settled14b
       && l14b.refused () >= 1
       && Park.parked_count park14 ~replica:replica14 ~tab:tab14 = 1);
     Lwt.wakeup_later wake14 ();
     let* (_ : bool) = await (fun () -> acks l14 = [ 1 ]) in
     l14.push None;
     let* (_ : bool) = await (fun () -> not (Lwt.is_sleeping session14)) in
     Lwt.return_unit)

let () =
  if !failures = 0 then print_endline "cancel_test: all checks passed"
  else (
    Printf.printf "cancel_test: %d check(s) FAILED\n%!" !failures;
    exit 1)
