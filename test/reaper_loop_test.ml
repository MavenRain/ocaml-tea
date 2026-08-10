(** {!Tea_server.Reaper.loop} (roadmap step 22, D24): the timing shell, driven
    deterministically through its injected timer and clock seams. No real time
    is involved: the timer is a queue of [Lwt.wait] pairs the test resolves,
    and the clock is a movable ref, so every check below is about ORDER, never
    about how fast this machine happens to run.

    Anti-vacuity: L2 fails if the loop never calls its sweep; L3 and L5 assert
    on the [Lwt.state] of the loop's own promise, so a hung loop is a FAIL,
    never a pass; L4 holds the sweep open on a gate the test resolves, so a
    loop that resolves early is caught as [Return] while its sweep still
    sleeps. *)

module Reaper = Tea_server.Reaper

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let check name cond =
       if cond then Printf.printf "ok   - %s\n%!" name
       else (
         Printf.printf "FAIL - %s\n%!" name;
         exit 1)
     in
     (* Enough passes through the scheduler's job queue that a wakeup, the
        choose it resolves, the sweep it triggers and the recursion behind it
        have all run. Three pauses is generous; the checks are about what has
        happened by then, not about how many turns it took. *)
     let settle () : unit Lwt.t =
       let* () = Lwt.pause () in
       let* () = Lwt.pause () in
       Lwt.pause ()
     in
     let resolved (p : unit Lwt.t) : bool =
       match Lwt.state p with
       | Lwt.Return () -> true
       | Lwt.Sleep -> false
       | Lwt.Fail (_ : exn) -> false
     in
     (* One injected timer per loop instance: [timer] queues a resolver per
        mint, [fire] resolves the oldest. A fire with no timer armed is its
        own FAIL: it means the loop stopped minting. *)
     let mk_timer () :
         unit Lwt.u list ref * (Reaper.Cadence.t -> unit Lwt.t) =
       let armed : unit Lwt.u list ref = ref [] in
       let timer (_ : Reaper.Cadence.t) : unit Lwt.t =
         let p, r = Lwt.wait () in
         armed := !armed @ [ r ];
         p
       in
       (armed, timer)
     in
     let fire (label : string) (armed : unit Lwt.u list ref) : unit =
       match !armed with
       | r :: rest ->
         armed := rest;
         Lwt.wakeup_later r ()
       | [] -> check (label ^ ": a timer is armed to fire") false
     in

     (* --- L6: the Cadence newtype's boundary ----------------------------- *)
     check "L6: of_seconds 0. is None (a zero cadence is a spin loop)"
       (Option.is_none (Reaper.Cadence.of_seconds 0.));
     check "L6: of_seconds (-1.) is None"
       (Option.is_none (Reaper.Cadence.of_seconds (-1.)));
     check "L6: of_seconds 0.5 is Some and roundtrips through to_seconds"
       (Option.fold (Reaper.Cadence.of_seconds 0.5) ~none:false
          ~some:(fun c -> Float.equal (Reaper.Cadence.to_seconds c) 0.5));
     (* Fail-loud mint: a [None] here must abort, not skip every later check
        as a vacuous pass. Both fold arms are closures applied once. *)
     let every : Reaper.Cadence.t =
       Option.fold (Reaper.Cadence.of_seconds 5.)
         ~none:(fun () ->
           Printf.printf "FAIL - of_seconds 5. mints a cadence\n%!";
           exit 1)
         ~some:(fun (c : Reaper.Cadence.t) () -> c)
         ()
     in

     (* --- L1/L2/L3: ticks sweep at their own clock; stop in a wait ------- *)
     let now_r = ref 100L in
     let sweeps : int64 list ref = ref [] in
     let sweep ~(now : int64) : int Lwt.t =
       sweeps := !sweeps @ [ now ];
       Lwt.return 0
     in
     let armed, timer = mk_timer () in
     let stop, wake_stop = Lwt.wait () in
     let loop_p = Reaper.loop ~sweep ~now:(fun () -> !now_r) ~timer ~every ~stop in
     let* () = settle () in
     check "L1: zero sweeps before the first tick" (List.length !sweeps = 0);
     check "L1: the first timer is armed" (List.length !armed = 1);
     now_r := 200L;
     fire "L2 first tick" armed;
     let* () = settle () in
     now_r := 300L;
     fire "L2 second tick" armed;
     let* () = settle () in
     now_r := 400L;
     fire "L2 third tick" armed;
     let* () = settle () in
     check "L2: three ticks, three sweeps, each at its own tick's clock"
       (List.equal Int64.equal !sweeps [ 200L; 300L; 400L ]);
     check "L2: the loop still runs between ticks (its promise sleeps)"
       (not (resolved loop_p));
     Lwt.wakeup_later wake_stop ();
     let* () = settle () in
     check "L3: stop during a wait resolves the loop promise" (resolved loop_p);
     check "L3: the sweep count is unchanged by the stop" (List.length !sweeps = 3);

     (* --- L4: stop lands DURING a sweep; the sweep completes first ------- *)
     let entered = ref 0 in
     let gate, open_gate = Lwt.wait () in
     let held_sweep ~now:(_ : int64) : int Lwt.t =
       entered := !entered + 1;
       let* () = gate in
       Lwt.return 0
     in
     let armed4, timer4 = mk_timer () in
     let stop4, wake4 = Lwt.wait () in
     let loop4 =
       Reaper.loop ~sweep:held_sweep ~now:(fun () -> 0L) ~timer:timer4 ~every
         ~stop:stop4
     in
     let* () = settle () in
     fire "L4 tick into the sweep" armed4;
     let* () = settle () in
     check "L4: the tick entered the sweep" (!entered = 1);
     Lwt.wakeup_later wake4 ();
     let* () = settle () in
     check "L4: the loop does NOT resolve while its sweep is in flight"
       (not (resolved loop4));
     Lwt.wakeup_later open_gate ();
     let* () = settle () in
     check "L4: once its sweep completes the loop resolves" (resolved loop4);
     check "L4: no further sweep ran after the stop" (!entered = 1);
     check "L4: no further timer was minted after the stop" (List.length !armed4 = 0);

     (* --- L5: stop already resolved at entry ----------------------------- *)
     let entered5 = ref 0 in
     let armed5, timer5 = mk_timer () in
     let loop5 =
       Reaper.loop
         ~sweep:(fun ~now:(_ : int64) ->
           entered5 := !entered5 + 1;
           Lwt.return 0)
         ~now:(fun () -> 0L) ~timer:timer5 ~every ~stop:Lwt.return_unit
     in
     check "L5: a stop already resolved at entry resolves the loop at once"
       (resolved loop5);
     check "L5: zero sweeps ran" (!entered5 = 0);
     check "L5: zero timers were minted" (List.length !armed5 = 0);

     Printf.printf
       "\nThe reaper loop sweeps per tick, never before the first, and stop always wins (D24).\n%!";
     Lwt.return_unit)
