(** {!Tea_client.Rpc_delivery}, the client's keyed-RPC queue (roadmap step 15,
    D20, I5). T12.

    Pure unit checks, off the browser: this module is the whole of the client
    half's correctness, because the runtime shell around it owns only WHEN to
    send, never under WHICH key. The property the server's exactly-once
    guarantee actually depends on is that a retry is the same key re-sent - so
    what is pinned here is that no operation in this module can hand a sender a
    number the entry was not recorded under, that acknowledging one entry
    neither renumbers nor evicts another, and that the 4xx rotation arm moves
    the stream identity without touching the numbering.

    The sequence space also has to stay dense per tab, or an honest client
    would trip the server's [Gapped] refusal on its own traffic. Density is
    structural here rather than asserted: {!Tea_client.Rpc_delivery.head} is
    the only way to reach an entry, so the second call cannot be sent before
    the first is acknowledged. *)

module Q = Tea_client.Rpc_delivery
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

(** A tab id from a 16-byte seed, through the same [of_bytes] mint the browser
    runtime's [of_draws] shares, so a change to the mint's arity or range
    breaks this too. *)
let tab (n : int) : Tab_id.t =
  must "tab id mint refused a valid seed"
    (Tab_id.of_bytes (List.init 16 (fun i -> (n + i) land 0xff)))

(* An entry whose [expect] returns the reply body verbatim, so a check can ask
   "is this the continuation I recorded?" of the value the queue handed back,
   rather than of a label stored beside it. *)
let entry (path : string) (body : string) : string Q.entry =
  { Q.path
  ; body
  ; expect =
      (fun (outcome : (string, Tea_core.Cmd.http_failure) result) ->
        Result.fold outcome ~ok:Fun.id ~error:(fun
            (_ : Tea_core.Cmd.http_failure) -> "transport"))
  }

let record (e : string Q.entry) (q : string Q.t) : string Q.t =
  must "the queue refused a record inside the sequence space" (Q.record e q)
  |> fst

(* Every [~none:] below is a constant already in hand, so the eager evaluation
   of [Option.fold]'s none arm costs nothing and hides nothing. A sentinel
   rather than a refusal: "no head" is a state these checks want to be able to
   OBSERVE, not one that should end the run. *)
let head_seq (q : string Q.t) : int =
  Q.head q
  |> Option.fold ~none:0
       ~some:(fun ((seq, (_ : string Q.entry)) : Msg_seq.t * string Q.entry) ->
         Msg_seq.to_int seq)

let head_body (q : string Q.t) : string =
  Q.head q
  |> Option.fold ~none:"<empty>"
       ~some:(fun (((_ : Msg_seq.t), e) : Msg_seq.t * string Q.entry) -> e.Q.body)

let ack_head (q : string Q.t) : string Q.t =
  Q.head q
  |> Option.fold ~none:q
       ~some:(fun ((seq, (_ : string Q.entry)) : Msg_seq.t * string Q.entry) ->
         Q.ack seq q)

let seq_int (n : int) : Msg_seq.t =
  must (Printf.sprintf "Msg_seq.of_int refused %d" n) (Msg_seq.of_int n)

(* --- An empty queue ------------------------------------------------------ *)

let () =
  let q = Q.v ~tab:(tab 1) in
  check "a fresh queue holds the tab it was minted with"
    (String.equal (Tab_id.to_string (Q.tab q)) (Tab_id.to_string (tab 1)));
  check "a fresh queue has nothing to send"
    (Q.is_empty q && Int.equal (Q.pending q) 0 && Option.is_none (Q.head q))

(* --- Recording, and the one-in-flight shape ------------------------------ *)

let () =
  let q = Q.v ~tab:(tab 1) in
  let q1 = record (entry "/rpc/append_tag" "first") q in
  check "the first recorded call is numbered one and is the head"
    (Int.equal (head_seq q1) 1
    && String.equal (head_body q1) "first"
    && Int.equal (Q.pending q1) 1);
  let q2 = record (entry "/rpc/append_tag" "second") q1 in
  (* The M7 anchor. A sender that asked twice - which is exactly what a retry
     does - must be told the same number both times, or the server would read
     the retry as a second call and apply the effect twice. *)
  check "head never renumbers: asking twice yields the same key"
    (Int.equal (head_seq q2) (head_seq q1) && Int.equal (head_seq q2) 1);
  check "a second record does not become sendable while the first is unacked"
    (String.equal (head_body q2) "first" && Int.equal (Q.pending q2) 2);
  check "the head carries back the continuation it was recorded with"
    (Q.head q2
    |> Option.fold ~none:false
         ~some:(fun (((_ : Msg_seq.t), e) : Msg_seq.t * string Q.entry) ->
           String.equal (e.Q.expect (Ok "the reply")) "the reply"
           && String.equal e.Q.path "/rpc/append_tag"))

(* --- Acknowledging ------------------------------------------------------- *)

let () =
  let q =
    Q.v ~tab:(tab 1)
    |> record (entry "/rpc/append_tag" "first")
    |> record (entry "/rpc/append_tag" "second")
  in
  let acked = ack_head q in
  check "acking the head exposes the next entry"
    (String.equal (head_body acked) "second" && Int.equal (Q.pending acked) 1);
  (* Dense per tab: the server refuses a gap, so the entry behind the acked one
     has to keep the number it was recorded under, not slide down to fill the
     hole. *)
  check "acking does not renumber the entry behind it"
    (Int.equal (head_seq acked) 2);
  check "acking the same entry twice is a no-op"
    (Int.equal (Q.pending (Q.ack (seq_int 1) acked)) 1);
  check "acking a sequence number this queue never sent is a no-op"
    (Int.equal (Q.pending (Q.ack (seq_int 99) acked)) 1
    && Int.equal (head_seq (Q.ack (seq_int 99) acked)) 2);
  let drained = ack_head acked in
  check "acking the last entry empties the queue"
    (Q.is_empty drained && Option.is_none (Q.head drained));
  (* Numbering advances with what was RECORDED, never with what is still held:
     resetting on an empty queue would make the next call look like a replay of
     the first one the server already took. *)
  check "an emptied queue does not restart its numbering"
    (Int.equal (head_seq (record (entry "/rpc/append_tag" "third") drained)) 3)

(* --- Rotation, the 4xx poison-recovery arm ------------------------------- *)

let () =
  let q =
    Q.v ~tab:(tab 1)
    |> record (entry "/rpc/append_tag" "first")
    |> record (entry "/rpc/append_tag" "second")
  in
  let rotated = Q.rotate ~tab:(tab 7) q in
  check "rotate adopts the new tab"
    (String.equal (Tab_id.to_string (Q.tab rotated)) (Tab_id.to_string (tab 7))
    && not
         (String.equal
            (Tab_id.to_string (Q.tab rotated))
            (Tab_id.to_string (tab 1))));
  (* Rotation is a change of stream identity, not of position in it. Nothing
     queued has ever been sent - only the head is ever sendable, and the head
     is what the 4xx just retired - so carrying the backlog over to the fresh
     tab cannot duplicate an effect, and it is what lets the backlog escape a
     poisoned stream instead of inheriting it. *)
  check "rotate renumbers nothing and drops nothing"
    (Int.equal (head_seq rotated) 1
    && String.equal (head_body rotated) "first"
    && Int.equal (Q.pending rotated) 2);
  check "rotate does not restart the numbering either"
    (Int.equal
       (head_seq (ack_head (ack_head (record (entry "/rpc/x" "third") rotated))))
       3)

let () = print_endline "rpc_delivery_test: all checks passed"
