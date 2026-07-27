(** The fuel arm of the durable replay guard (roadmap step 11, D16): a take is
    persisted even when the apply attempt dies of fuel exhaustion.

    [exactly_once_test] pins the WS pump's [Fresh] arm across a restart, but
    only its ok half: the Counter app's update never exhausts fuel, so the
    [error] half of [step_ws] (persist the take, end the session) was
    uncovered, and a mutation deleting that arm's [persist_taken] reddened
    nothing. This file is the missing red.

    The invariant is directional, and the direction is the whole point: the
    durable high water means "this seq was TAKEN", never "this seq was
    APPLIED". A server that only persisted applied messages would meet the
    replay of a fuel-poison message with an empty floor after a restart, call
    it Fresh, and burn a full fuel budget again, once per reconnect, forever;
    the poison message graduates from a one-time casualty to a permanent
    session killer. So the checks below pin both sides: the take IS recorded
    although nothing was applied (no commit, no ack), and a guard rebuilt
    from that record refuses the same seq as Duplicate, while a guard rebuilt
    from nothing calls it Fresh again, so the refusal provably came from the
    journal record and not from the app, the store, or the transport. The ok
    arm is then exercised on the SAME app through the SAME sink, so a
    regression that merely moved the persist from one arm to the other could
    not pass either. *)

module App = struct
  open Tea_core

  (** The smallest APP whose update can exhaust fuel: [Spin]'s command
      re-emits [Spin], so {!Tea_core.Loop}'s interpreter burns its whole
      budget and [step] returns [Error Fuel_exhausted]. [Bump] is the
      ordinary message that settles at once, for the ok arm. *)
  type model = int

  type msg =
    | Bump  (** apply: model + 1, command tail settles immediately *)
    | Spin  (** the fuel poison: its reply re-emits itself forever *)

  let model_t = Repr.int

  let msg_t =
    Repr.(
      variant "msg" (fun bump spin ->
        function
        | Bump -> bump
        | Spin -> spin)
      |~ case0 "Bump" Bump
      |~ case0 "Spin" Spin
      |> sealv)

  let init = (0, Cmd.none)

  let update (_ : Crdt.Ctx.t) (msg : msg) (m : model) =
    match msg with
    | Bump -> (m + 1, Cmd.none)
    | Spin -> (m, Cmd.emit Spin)

  let view (m : model) = Html.text (string_of_int m)
  let subscriptions (_ : model) = Sub.none
  let merge = Merge.(to_spec (atomic ~eq:Int.equal))
  let title = Prim.Title.v "fuel-probe"
  let url_of_model (_ : model) = None
  let msg_of_url (_ : Prim.Url.t) = None
end

module _ : Tea_core.App.APP = App
module Server = Tea_server.Make (App)
module Codec = Tea_core.Codec.Make (App)
module Guard = Tea_server.Replay_guard
module Dguard = Tea_server.Durable_guard
module Sink = Tea_server.Guard_sink
module Msg_seq = Tea_core.Prim.Msg_seq
module Tab_id = Tea_core.Prim.Tab_id
module Replica = Tea_core.Crdt.Replica

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

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

(* One socket's worth of transport, plus the handle to kill it; every frame
   the server sends is retained so a check can ask what the client saw. The
   [exactly_once_test] idiom, verbatim. *)
type link =
  { transport : Server.live_transport
  ; push : string option -> unit
  ; frames : unit -> string list
  }

let link () : link =
  let incoming, push = Lwt_stream.create () in
  let sent = ref [] in
  { transport =
      { Server.send_frame =
          (fun f ->
            sent := f :: !sent;
            Lwt.return_unit)
      ; receive_frame = (fun () -> Lwt_stream.get incoming)
      }
  ; push
  ; frames = (fun () -> List.rev !sent)
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

(* Terminated, and cleanly: a session promise that died of an exception is
   not "the fuel arm ended the pump", so [Fail] does not count. *)
let resolved (p : unit Lwt.t) : bool =
  match Lwt.state p with
  | Lwt.Return () -> true
  | Lwt.Fail (_ : exn) -> false
  | Lwt.Sleep -> false

let is_advance_for ~(replica : Replica.t) ~(tab : Tab_id.t) ~(seq : int) :
    Sink.event -> bool = function
  | Sink.Advance { replica = r; tab = t; seq = n } ->
    Replica.equal r replica
    && String.equal (Tab_id.to_string t) (Tab_id.to_string tab)
    && Msg_seq.to_int n = seq
  | Sink.Forget { replica = (_ : Replica.t) } -> false

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let* repo = Server.Store.create () in
     let* s = Server.Store.session repo (sid "fuelpoison") in
     let replica = Tea_core.Crdt.Ctx.replica (Server.Store.ctx_of_session s) in
     let tab = Tab_id.of_draws (fun () -> 7) in
     let sink, recorded = Sink.memory () in
     let guard_a =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs ~sink
         ~floors:Dguard.Floors.empty
     in
     (* --- 1. The fuel arm: attempted, not applied, and still taken -------- *)
     let l1 = link () in
     let session1 = Server.live_session ~guard:guard_a s l1.transport in
     l1.push (Some (frame ~tab ~seq:1 App.Spin));
     let* ended = await (fun () -> resolved session1) in
     check "a fuel-poison msg ends the session (the error arm ends the pump)"
       ended;
     (* No ack and no commit distinguish this from the ok arm below: if either
        showed up here, the "fuel" path would secretly be the success path and
        every later check would be about the wrong arm. *)
     check "the fuel arm mints no ack (taken is not confirmed)" (acks l1 = []);
     let* after_spin = Server.Store.load s in
     check "fuel exhaustion commits nothing to the store" (after_spin = 0);
     check
       "THE POINT: the take was persisted anyway, once, for this exact \
        (replica, tab, seq)"
       (match recorded () with
        | [ e ] -> is_advance_for ~replica ~tab ~seq:1 e
        | [] -> false
        | _ :: _ :: _ -> false);
     (* --- 2. The restart: the journal record alone refuses the seq -------- *)
     let seq1 = must "seq 1" (Msg_seq.of_int 1) in
     let guard_b =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs ~sink
         ~floors:(Dguard.Floors.of_events (recorded ()))
     in
     check
       "a guard restarted from the journal answers Duplicate: the fuel-killed \
        msg is not re-taken"
       (match Dguard.take guard_b ~replica ~tab ~seq:seq1 with
        | Guard.Duplicate n -> Msg_seq.to_int n = 1
        | Guard.Fresh (_ : Msg_seq.t) -> false
        | Guard.Gapped -> false);
     (* Same key, journal thrown away: if this were not Fresh, the Duplicate
        above would be proving something about the guard's defaults rather
        than about the record the fuel arm wrote. *)
     let guard_amnesiac =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
         ~sink:Sink.null ~floors:Dguard.Floors.empty
     in
     check "ANTI-VACUITY: with no journal the same seq IS Fresh again"
       (match Dguard.take guard_amnesiac ~replica ~tab ~seq:seq1 with
        | Guard.Fresh n -> Msg_seq.to_int n = 1
        | Guard.Duplicate (_ : Msg_seq.t) -> false
        | Guard.Gapped -> false);
     (* --- 3. The ok arm, same app, same sink ------------------------------ *)
     let l2 = link () in
     let session2 = Server.live_session ~guard:guard_b s l2.transport in
     l2.push (Some (frame ~tab ~seq:2 App.Bump));
     let* acked = await (fun () -> acks l2 = [ 2 ]) in
     check "the non-looping msg on the same app is acked (the ok arm lives)"
       acked;
     let* after_bump = Server.Store.load s in
     check "and it really applied: the model reads 1" (after_bump = 1);
     check "the ok arm persisted its take through the same sink"
       (List.length (recorded ()) = 2
       && Option.fold ~none:false
            ~some:(is_advance_for ~replica ~tab ~seq:2)
            (List.nth_opt (recorded ()) 1));
     l2.push None;
     let* (_ : unit) = session2 in
     Lwt.return_unit)

let () = print_endline "fuel_durable_test: all checks passed"
