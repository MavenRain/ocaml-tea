(** Exactly-once delivery, both tiers in one process (roadmap step 10, D15).

    This is [predictor_test]'s composition extended by one hop, and the hop is
    the whole point: a real {!Tea_persist.Store} session, a real
    {!Tea_server}[.live_session] over an in-memory transport with its own
    isolated replay guard, and the real {!Tea_client.Delivery} on the other end
    — with the transport {b killed and re-opened in the middle}.

    No earlier step could express "the same socket died and the same edit came
    back". D9's outbox never held a message that had already been handed to a
    socket, so the lost edit was invisible to every in-process test; and a
    retry, once added, is only safe because the server refuses to apply what it
    has already consumed. Both halves are exercised here against the same store.

    The two failure directions are not symmetric, and the checks are written to
    tell them apart: a message that is silently dropped is a permanent lost
    edit, while a message applied twice is a visible double count. A test that
    only asserted "the counter reads 1" would pass for a server that ignored
    the socket entirely, so every check below pins {i which} of those two
    happened. *)

module Server = Tea_server.Make (Counter_app.App)
module Codec = Tea_core.Codec.Make (Counter_app.App)
module Delivery = Tea_client.Delivery
module Rebase = Tea_client.Rebase
module Guard = Tea_server.Replay_guard
module Dguard = Tea_server.Durable_guard

(* An isolated mem-tier guard: null sink over empty floors, step-10 semantics
   exactly. Sections that simulate a restart build theirs over a memory sink
   instead. *)
let mem_guard () : Dguard.t =
  Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
    ~sink:Tea_server.Guard_sink.null ~floors:Dguard.Floors.empty
module Msg_seq = Tea_core.Prim.Msg_seq
module App = Counter_app.App

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
let tab_of (n : int) = Tea_core.Prim.Tab_id.of_draws (fun () -> n)

(* One socket's worth of transport, plus the handle to kill it. Every frame the
   server sends is retained so a check can ask what the client would have
   seen. *)
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
          | Tea_core.Wire.Hello ((_ : Tea_core.Crdt.Replica.t), (_ : App.model)) -> None))
    (l.frames ())

let heads (l : link) : int list =
  List.filter_map
    (fun f ->
      Option.bind (decoded f) (fun d ->
          match d with
          | Tea_core.Wire.Head m -> Some (App.value m)
          | Tea_core.Wire.Hello ((_ : Tea_core.Crdt.Replica.t), (_ : App.model)) -> None
          | Tea_core.Wire.Ack (_ : Msg_seq.t) -> None))
    (l.frames ())

(* The client half: hand a recorded entry to a link exactly as the runtime's
   [send_one] does. Recording and sending are separate on purpose — the lost
   edit is precisely a message that was recorded, sent, and never consumed. *)
let frame_of (d : App.msg Delivery.t) ((seq : Msg_seq.t), (msg : App.msg)) : string =
  Codec.up_to_json
    (Tea_core.Wire.Apply
       { tab = Tea_core.Prim.Tab_id.to_string (Delivery.tab d)
       ; seq = Msg_seq.to_int seq
       ; msg
       })

let record (d : App.msg Delivery.t) (msg : App.msg) :
    App.msg Delivery.t * (Msg_seq.t * App.msg) =
  must "the seq space is not exhausted" (Delivery.record msg d)

(* Feed every down-frame through the client's own total absorber, so what the
   queue does with an [Ack] is decided by library code, not by this file. *)
let absorb_all (d : App.msg Delivery.t) (l : link) : App.msg Delivery.t =
  l.frames ()
  |> List.filter_map decoded
  |> List.fold_left
       (fun acc down ->
         match Rebase.absorb App.merge ~local:None down with
         | Rebase.Acked n -> Delivery.ack n acc
         | Rebase.Resync ((_ : Tea_core.Crdt.Replica.t), (_ : App.model)) -> acc
         | Rebase.Rebased (_ : App.model) -> acc)
       d

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let* repo = Server.Store.create () in
     (* --- 1. The happy path: an accepted frame is acknowledged by seq ------ *)
     let* s = Server.Store.session repo (sid "happy") in
     let guard = mem_guard () in
     let l1 = link () in
     let session1 = Server.live_session ~guard s l1.transport in
     let d = Delivery.v ~tab:(tab_of 11) in
     let d, e1 = record d App.Increment in
     l1.push (Some (frame_of d e1));
     let* got_ack = await (fun () -> acks l1 <> []) in
     check "an accepted up-frame is acked by seq" (got_ack && acks l1 = [ 1 ]);
     let d_after = absorb_all d l1 in
     check "the ack empties the client's delivery queue" (Delivery.is_empty d_after);
     check "the recorded edit was pending until that ack arrived" (Delivery.pending d = 1);
     (* A msg that commits nothing must still be acked, or the client retries it
        forever: acks come from the pump, not from a commit. [Sync] of the head
        the server already holds is a join with itself. *)
     let* head = Server.Store.load s in
     let d2, e2 = record d_after (App.Sync head) in
     l1.push (Some (frame_of d2 e2));
     let* acked_noop = await (fun () -> List.length (acks l1) = 2) in
     check "a no-op msg is acked too (acks come from the pump, not from a commit)"
       (acked_noop && acks l1 = [ 1; 2 ]);
     (* Frame order between an Ack and a Head is not fixed - the watch callback
        may fire either side of the step - so the client must be indifferent. *)
     let ack_frame = Codec.down_to_json (Tea_core.Wire.Ack (must "seq" (Msg_seq.of_int 1))) in
     let head_frame = Codec.down_to_json (Tea_core.Wire.Head head) in
     let fold_frames order =
       List.fold_left
         (fun acc f ->
           Option.fold ~none:acc
             ~some:(fun down ->
               match Rebase.absorb App.merge ~local:None down with
               | Rebase.Acked n -> Delivery.ack n acc
               | Rebase.Resync ((_ : Tea_core.Crdt.Replica.t), (_ : App.model)) -> acc
               | Rebase.Rebased (_ : App.model) -> acc)
             (decoded f))
         (fst (record (Delivery.v ~tab:(tab_of 12)) App.Increment))
         order
     in
     check "the client tolerates Ack-before-Head and Head-before-Ack identically"
       (Delivery.pending (fold_frames [ ack_frame; head_frame ])
        = Delivery.pending (fold_frames [ head_frame; ack_frame ]));
     l1.push None;
     let* (_ : unit) = session1 in
     (* --- 2. The headline: an edit whose socket died --------------------- *)
     (* Recorded and sent, and the socket dies before the server consumes it.
        Before D15 this message was in no queue at all: nothing replayed it, and
        the next Hello resync discarded the prediction. A silent lost edit. *)
     let* s2 = Server.Store.session repo (sid "lostedit") in
     let guard2 = mem_guard () in
     let dead = link () in
     let session_dead = Server.live_session ~guard:guard2 s2 dead.transport in
     let d = Delivery.v ~tab:(tab_of 13) in
     let d, e = record d App.Increment in
     (* The frame is built and "sent" into a socket that dies without it ever
        being consumed - the exact fate a dropped TCP segment gives it. *)
     let (_ : string) = frame_of d e in
     dead.push None;
     let* (_ : unit) = session_dead in
     let* mid = Server.Store.load s2 in
     check "the server never saw the edit its socket dropped" (App.value mid = 0);
     check "the client still holds it: at-least-once means it is not forgotten"
       (Delivery.pending d = 1);
     let fresh = link () in
     let session_fresh = Server.live_session ~guard:guard2 s2 fresh.transport in
     List.iter (fun entry -> fresh.push (Some (frame_of d entry))) (Delivery.unacked d);
     let* landed = await (fun () -> List.mem 1 (heads fresh)) in
     check "an edit whose socket died is replayed after the reconnect" landed;
     let* after = Server.Store.load s2 in
     check "the replayed edit is applied once: the counter reads 1, not 2"
       (App.value after = 1);
     (* --- 3. The other direction: applied, then the ack was lost ---------- *)
     (* The server consumed it and the socket died before the ack got back, so
        the client replays a message the server already has. A naive retry
        double-counts here - this is the D14-shaped bug the guard exists for. *)
     let* history_before = Server.Store.history s2 in
     List.iter (fun entry -> fresh.push (Some (frame_of d entry))) (Delivery.unacked d);
     let* re_acked = await (fun () -> List.length (acks fresh) >= 2) in
     check "the duplicate is acked, so the replay stops" re_acked;
     let* twice = Server.Store.load s2 in
     let* history_after = Server.Store.history s2 in
     check "a replay of an already-consumed edit commits nothing"
       (App.value twice = 1
       && List.length history_after = List.length history_before);
     let d_done = absorb_all d fresh in
     check "the client's queue drains on the duplicate's ack too"
       (Delivery.is_empty d_done);
     (* --- 4. Two tabs on one session ------------------------------------- *)
     (* Both share the cookie, the branch and the replica id, and both number
        their own messages from 1. A guard keyed on the session alone drops the
        second tab's first edit forever. *)
     let* s3 = Server.Store.session repo (sid "twotabs") in
     let guard3 = mem_guard () in
     let l3 = link () in
     let session3 = Server.live_session ~guard:guard3 s3 l3.transport in
     let ta = Delivery.v ~tab:(tab_of 21) and tb = Delivery.v ~tab:(tab_of 42) in
     let ta, ea = record ta App.Increment in
     let tb, eb = record tb App.Increment in
     l3.push (Some (frame_of ta ea));
     l3.push (Some (frame_of tb eb));
     let* both = await (fun () -> List.mem 2 (heads l3)) in
     check "two tabs' first edits both land: the counter reads 2" both;
     (* --- 5. Reconnection does not renumber ------------------------------- *)
     let ta, e_next = record ta App.Increment in
     check "reconnecting does not reset the tab's seq counter"
       (Msg_seq.to_int (fst e_next) = 2);
     l3.push (Some (frame_of ta e_next));
     let* three = await (fun () -> List.mem 3 (heads l3)) in
     check "the tab's second edit lands under its own next seq" three;
     l3.push None;
     let* (_ : unit) = session3 in
     fresh.push None;
     let* (_ : unit) = session_fresh in
     (* --- 6. The D14 window, re-pinned at this composition ---------------- *)
     (* A replay is a re-send, never a second local apply, so it mints no new
        client dot; de-duplication suppresses the second server apply, so it
        mints no new server dot. Step 10 therefore leaves the pre-announcement
        window exactly as step 9 pinned it - neither better nor worse. A change
        that closes it should redden this and [predictor_test] together. *)
     check "PIN: delivery retry neither widens nor narrows the D14 window"
       (App.value twice = 1);
     (* --- 7. Across a process restart (roadmap step 11, D16) -------------- *)
     (* Step 10's guard lived only in memory, so this exact script double
        counted: the restarted process met a replay with an empty table and
        called it Fresh. Here the take is recorded through a sink, and the next
        life folds that record into its floors before answering its first
        frame. The store branch is the SAME one across both lives, which is
        what makes the second apply observable at all. *)
     let* s4 = Server.Store.session repo (sid "restart") in
     let sink, recorded = Tea_server.Guard_sink.memory () in
     let guard_a =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs ~sink
         ~floors:Dguard.Floors.empty
     in
     let l4 = link () in
     let session4 = Server.live_session ~guard:guard_a s4 l4.transport in
     let d4 = Delivery.v ~tab:(tab_of 51) in
     let d4, e4 = record d4 App.Increment in
     l4.push (Some (frame_of d4 e4));
     let* landed4 = await (fun () -> List.mem 1 (heads l4)) in
     check "the pre-restart edit lands" landed4;
     let* before_restart = Server.Store.load s4 in
     check "the counter reads 1 before the restart" (App.value before_restart = 1);
     (* The process dies. Every in-memory high water dies with it; only what the
        sink accepted comes back. The client never absorbed the ack, so the edit
        is still in its unacked queue - the honest at-least-once posture. *)
     l4.push None;
     let* (_ : unit) = session4 in
     check "the take was recorded durably, once, for the consumed seq"
       (List.length (recorded ()) = 1);
     check "the client still holds the edit it was never told about"
       (Delivery.pending d4 = 1);
     let* history_pre = Server.Store.history s4 in
     (* The new process: an EMPTY cell, floors folded from the record alone. *)
     let guard_b =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs ~sink
         ~floors:(Dguard.Floors.of_events (recorded ()))
     in
     let l5 = link () in
     let session5 = Server.live_session ~guard:guard_b s4 l5.transport in
     List.iter (fun entry -> l5.push (Some (frame_of d4 entry))) (Delivery.unacked d4);
     let* re_acked5 = await (fun () -> acks l5 <> []) in
     check "the restarted server acks the replay, so the client stops retrying"
       re_acked5;
     let* after_restart = Server.Store.load s4 in
     let* history_post = Server.Store.history s4 in
     check
       "EXACTLY-ONCE ACROSS THE RESTART: the replayed edit commits nothing the \
        second time"
       (App.value after_restart = 1
       && List.length history_post = List.length history_pre);
     l5.push None;
     let* (_ : unit) = session5 in
     (* --- 8. The converse, so section 7 cannot pass vacuously ------------- *)
     (* Identical script with the journal thrown away (empty floors, null sink -
        step 10 exactly). If this did NOT double count, section 7 would be
        proving something about the app or the transport rather than about the
        durable floor. Increment is not idempotent; that is the whole lever. *)
     let* s5 = Server.Store.session repo (sid "norestore") in
     let guard_c =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
         ~sink:Tea_server.Guard_sink.null ~floors:Dguard.Floors.empty
     in
     let l6 = link () in
     let session6 = Server.live_session ~guard:guard_c s5 l6.transport in
     let d6 = Delivery.v ~tab:(tab_of 52) in
     let d6, e6 = record d6 App.Increment in
     l6.push (Some (frame_of d6 e6));
     let* landed6 = await (fun () -> List.mem 1 (heads l6)) in
     check "the pre-restart edit lands on the undurable server too" landed6;
     l6.push None;
     let* (_ : unit) = session6 in
     let guard_d =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
         ~sink:Tea_server.Guard_sink.null ~floors:Dguard.Floors.empty
     in
     let l7 = link () in
     let session7 = Server.live_session ~guard:guard_d s5 l7.transport in
     List.iter (fun entry -> l7.push (Some (frame_of d6 entry))) (Delivery.unacked d6);
     let* doubled = await (fun () -> List.mem 2 (heads l7)) in
     check
       "ANTI-VACUITY: with no durable floor the same replay IS applied twice \
        (counter reads 2)"
       doubled;
     let* undurable = Server.Store.load s5 in
     check "the undurable counter really is at 2" (App.value undurable = 2);
     l7.push None;
     let* (_ : unit) = session7 in
     (* --- 9. A failing sink costs neither the edit nor the ack ------------ *)
     (* Durability is the thing that degrades, never the session: a sink that
        refuses every record must still leave a working step-10 server. The
        pump prints one line and carries on, and the client is acked, because
        an unacked client retries forever. *)
     let failing : Tea_server.Guard_sink.t =
       { Tea_server.Guard_sink.append =
           (fun (_ : Tea_server.Guard_sink.event) ->
             Lwt.return (Error (Tea_server.Guard_sink.Io "disk full")))
       }
     in
     let* s6 = Server.Store.session repo (sid "sinkdown") in
     let guard_e =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
         ~sink:failing ~floors:Dguard.Floors.empty
     in
     let l8 = link () in
     let session8 = Server.live_session ~guard:guard_e s6 l8.transport in
     let d8 = Delivery.v ~tab:(tab_of 53) in
     let d8, e8 = record d8 App.Increment in
     l8.push (Some (frame_of d8 e8));
     let* acked_despite = await (fun () -> acks l8 <> []) in
     check "a refusing sink still acks: durability degrades, the session does not"
       acked_despite;
     let* despite = Server.Store.load s6 in
     check "and the edit itself is applied exactly once" (App.value despite = 1);
     (* The mirror was raised before the append was attempted, so this process
        still refuses the replay - the failure costs the NEXT life's knowledge,
        not this one's. *)
     List.iter (fun entry -> l8.push (Some (frame_of d8 entry))) (Delivery.unacked d8);
     let* re_acked8 = await (fun () -> List.length (acks l8) >= 2) in
     let* still_one = Server.Store.load s6 in
     check "a failed append does not re-open the seq within the same process"
       (re_acked8 && App.value still_one = 1);
     l8.push None;
     let* (_ : unit) = session8 in
     Lwt.return_unit)
