(** The delivery queue and the widened absorber (roadmap step 10, D15).

    Pure and native: {!Tea_client.Delivery} is the only place a sequence number
    is ever assigned, and {!Tea_client.Rebase.absorb} is the one total function
    every down-frame passes through, so both belong in [tea_client] where a test
    can reach them without a browser.

    The wire forms are pinned as literal JSON. A round-trip check inside one
    process is satisfied by any encoding both halves agree on, including a
    renamed case — which would be a silent protocol break against a deployed
    peer. The [rpc_test] precedent. *)

module Delivery = Tea_client.Delivery
module Rebase = Tea_client.Rebase
module Codec = Tea_core.Codec.Make (Counter_app.App)
module Msg_seq = Tea_core.Prim.Msg_seq
module App = Counter_app.App

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

(* [~none:] is eager, so both branches are closures. *)
let must (what : string) (o : 'a option) : 'a =
  Option.fold
    ~none:(fun () ->
      Printf.printf "FAIL - test setup: %s\n%!" what;
      exit 1)
    ~some:(fun x () -> x)
    o ()

let seq (n : int) : Msg_seq.t = must "Msg_seq.of_int" (Msg_seq.of_int n)
let tab = Tea_core.Prim.Tab_id.of_draws (fun () -> 0xab)

let record (d : 'a Delivery.t) (m : 'a) : 'a Delivery.t * Msg_seq.t =
  let d', (n, (_ : 'a)) = must "record" (Delivery.record m d) in
  (d', n)

(* --- 1. Numbering ---------------------------------------------------------- *)

let () =
  let d = Delivery.v ~tab in
  let d, n1 = record d "a" in
  let d, n2 = record d "b" in
  let (_ : string Delivery.t), n3 = record d "c" in
  check "consecutive records get consecutive seqs"
    (Msg_seq.to_int n1 = 1 && Msg_seq.to_int n2 = 2 && Msg_seq.to_int n3 = 3)

let () =
  let d = Delivery.v ~tab in
  let d, (_ : Msg_seq.t) = record d "a" in
  check "a recorded msg stays unacked until it is acked" (Delivery.pending d = 1);
  let d = Delivery.ack (seq 1) d in
  check "the queue is empty once the newest seq is acked" (Delivery.is_empty d);
  (* The numbering must NOT restart when the queue empties: the server's high
     water is per tab and monotone, so a reused seq reads as a replay and the
     edit is dropped forever. *)
  let (_ : string Delivery.t), n = record d "next" in
  check "an emptied queue does not restart the numbering" (Msg_seq.to_int n = 2)

(* --- 2. Order and cumulative acks ------------------------------------------ *)

let () =
  let d = Delivery.v ~tab in
  let d, (_ : Msg_seq.t) = record d "a" in
  let d, (_ : Msg_seq.t) = record d "b" in
  let d, (_ : Msg_seq.t) = record d "c" in
  check "unacked replays in the order the edits were made"
    (List.map snd (Delivery.unacked d) = [ "a"; "b"; "c" ]);
  check "an ack drops every seq at or below it"
    (List.map snd (Delivery.unacked (Delivery.ack (seq 2) d)) = [ "c" ]);
  check "an ack for a seq this tab never sent leaves the queue untouched"
    (List.map snd (Delivery.unacked (Delivery.ack (seq 9) d)) = []);
  check "acking is idempotent"
    (Delivery.pending (Delivery.ack (seq 1) (Delivery.ack (seq 1) d))
     = Delivery.pending (Delivery.ack (seq 1) d));
  check "the tab id travels with the queue"
    (Tea_core.Prim.Tab_id.compare (Delivery.tab d) tab = 0)

(* --- 3. absorb: one total function, three arms ----------------------------- *)

let () =
  let m = fst App.init in
  let r = Tea_client.Identity.provisional in
  let arm (d : App.model Tea_core.Wire.down) : string =
    match Rebase.absorb App.merge ~local:(Some m) d with
    | Rebase.Resync ((_ : Tea_core.Crdt.Replica.t), (_ : App.model)) -> "resync"
    | Rebase.Rebased (_ : App.model) -> "rebased"
    | Rebase.Acked n -> Printf.sprintf "acked:%d" (Msg_seq.to_int n)
  in
  check "absorb turns an Ack into Acked carrying that seq, and yields no model"
    (arm (Tea_core.Wire.Ack (seq 7)) = "acked:7");
  check "absorb turns a Hello into Resync" (arm (Tea_core.Wire.Hello (r, m)) = "resync");
  check "absorb turns a Head into Rebased" (arm (Tea_core.Wire.Head m) = "rebased")

(* --- 4. The wire forms ----------------------------------------------------- *)

let () =
  let up = Tea_core.Wire.Apply { tab = "0f"; seq = 3; msg = App.Increment } in
  let round =
    Codec.up_of_json (Codec.up_to_json up)
    |> Result.to_option
    |> Option.map (fun (Tea_core.Wire.Apply { tab; seq; msg }) ->
           (tab, seq, Codec.msg_to_json msg))
  in
  check "an up-frame round-trips its tab, seq and msg"
    (round = Some ("0f", 3, Codec.msg_to_json App.Increment));
  check "the Ack frame's wire form is pinned"
    (Codec.down_to_json (Tea_core.Wire.Ack (seq 4)) = {|{"ack":4}|});
  check "the up-frame's wire form is pinned"
    (Codec.up_to_json up = {|{"apply":["0f",3,"Increment"]}|});
  (* A down-frame is not an up-frame: the tiers must not accept each other's
     shapes, or a drifted peer would look healthy. *)
  check "an Ack does not decode as an up-frame"
    (Result.is_error (Codec.up_of_json (Codec.down_to_json (Tea_core.Wire.Ack (seq 4)))));
  check "a bare msg is no longer a valid up-frame"
    (Result.is_error (Codec.up_of_json (Codec.msg_to_json App.Increment)));
  (* A crafted variant case must be an Error, not a raise: this is the step-9
     [Codec.of_json] fix, re-pinned on the new up surface, because the pump
     decodes untrusted frames through it. *)
  check "a crafted up-frame case is a decode Error, not a raise"
    (Result.is_error (Codec.up_of_json {|{"bogus":1}|}))
