(** {!Tea_server.Reply_cache}, the RPC reply store behind the exactly-once
    channel (roadmap step 15, D20.2).

    Pure unit checks: this is the one layer where the test author owns both
    sides of the take-to-settle window, so the [Pending] and eviction arms can
    be driven directly instead of through a race. The two laws under test are
    the ones the whole channel rests on. It never forges (a reply is answered
    only at exactly its own seq and endpoint) and it never loops (every way of
    losing an entry reads [Gone], which degrades to the typed [Replayed] arm,
    never to a 503 the client would retry forever). *)

module Rc = Tea_server.Reply_cache
module Guard = Tea_server.Replay_guard
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
    runtime uses, so a change to the mint's arity or range breaks this too. *)
let tab (n : int) : Tab_id.t =
  must "tab id mint refused a valid seed"
    (Tab_id.of_bytes (List.init 16 (fun i -> (n + i) land 0xff)))

let seq (n : int) : Msg_seq.t =
  must (Printf.sprintf "Msg_seq.of_int refused %d" n) (Msg_seq.of_int n)

let bound (n : int) : Guard.Bound.t =
  must (Printf.sprintf "Bound.of_int refused %d" n) (Guard.Bound.of_int n)

(* Wildcard-free readers for [found]: a fourth outcome breaks each of these,
   which is the point of matching the sum rather than testing for one arm. *)
let is_busy (f : Rc.found) : bool =
  match f with
  | Rc.Busy -> true
  | Rc.Original (_ : string) -> false
  | Rc.Gone -> false

let is_gone (f : Rc.found) : bool =
  match f with
  | Rc.Gone -> true
  | Rc.Busy -> false
  | Rc.Original (_ : string) -> false

let original_is (f : Rc.found) ~(body : string) : bool =
  match f with
  | Rc.Original got -> String.equal got body
  | Rc.Busy -> false
  | Rc.Gone -> false

let ep = "append_tag"

(* --- the settled arm: answered verbatim, and only at its own key --- *)

let () =
  let c = Rc.v ~entries:(bound 8) () in
  let c = Rc.settle c ~tab:(tab 1) ~seq:(seq 1) ~endpoint:ep ~body:"{\"count\":7}" in
  let (_ : Rc.t), found = Rc.find c ~tab:(tab 1) ~seq:(seq 1) ~endpoint:ep in
  check "a settled reply is answered verbatim at its own seq and endpoint"
    (original_is found ~body:"{\"count\":7}");
  (* The ghost-duplicate direction, in the small: seq 2's bytes must never
     answer a duplicate of seq 1. This is M4's unit half; its registered
     anchor is T2's ghost arm against the real route. *)
  let c = Rc.settle c ~tab:(tab 1) ~seq:(seq 2) ~endpoint:ep ~body:"{\"count\":8}" in
  let c, stale = Rc.find c ~tab:(tab 1) ~seq:(seq 1) ~endpoint:ep in
  check "a duplicate of an older seq reads Gone, never the newest bytes"
    (is_gone stale);
  let (_ : Rc.t), current = Rc.find c ~tab:(tab 1) ~seq:(seq 2) ~endpoint:ep in
  check "the newest seq still answers its own bytes"
    (original_is current ~body:"{\"count\":8}");
  (* T18's endpoint arm, the J3 pin and M15's anchor: the endpoint is stored
     beside the body and matched on find, so a rotation or queue bug degrades
     to Replayed instead of handing one endpoint's bytes to another's decoder. *)
  let (_ : Rc.t), crossed = Rc.find c ~tab:(tab 1) ~seq:(seq 2) ~endpoint:"doc_stats" in
  check "the reply cache never forges: an endpoint mismatch reads Gone"
    (is_gone crossed);
  let (_ : Rc.t), other_tab = Rc.find c ~tab:(tab 2) ~seq:(seq 2) ~endpoint:ep in
  check "another tab's lookup reads Gone (entries are per floor tab)"
    (is_gone other_tab)

(* --- the Pending arm: 503 while the window is open, never after --- *)

let () =
  let c = Rc.v ~entries:(bound 8) ~pending_grace:2 () in
  let c = Rc.mark_pending c ~tab:(tab 1) ~seq:(seq 1) in
  let c, inside = Rc.find c ~tab:(tab 1) ~seq:(seq 1) ~endpoint:ep in
  check "a Pending entry at the same seq reads Busy inside the grace"
    (is_busy inside);
  let c, other_seq = Rc.find c ~tab:(tab 1) ~seq:(seq 2) ~endpoint:ep in
  check "a Pending entry at a different seq reads Gone, not Busy"
    (is_gone other_seq);
  (* T19, re-cut by the step-15 adversarial review (F1): the grace is a
     PER-WINDOW poll budget, drained only by this tab's own Busy answers.
     Foreign traffic must never age a live window - the global-tick version
     turned ~32 unrelated deliveries into a spurious Replayed for a delivery
     still in flight, the D20.2 lie - and the window must still drain under
     its own client's polling, or a never-settling handler would pin its tab
     at 503 forever (G3's liveness rider). The arithmetic below also pins
     that a mismatched-seq lookup spends nothing: one poll went above, one
     goes to the check after the foreign storm, and the third read drains. *)
  let c, (_ : Rc.found) = Rc.find c ~tab:(tab 2) ~seq:(seq 1) ~endpoint:ep in
  let c, (_ : Rc.found) = Rc.find c ~tab:(tab 2) ~seq:(seq 1) ~endpoint:ep in
  let c = Rc.mark_pending c ~tab:(tab 3) ~seq:(seq 1) in
  let c = Rc.settle c ~tab:(tab 3) ~seq:(seq 1) ~endpoint:ep ~body:"noise" in
  let c, alive = Rc.find c ~tab:(tab 1) ~seq:(seq 1) ~endpoint:ep in
  check "foreign traffic never ages a live window: tab 1 still reads Busy"
    (is_busy alive);
  let (_ : Rc.t), drained = Rc.find c ~tab:(tab 1) ~seq:(seq 1) ~endpoint:ep in
  check "the window drains under its own polling: past the budget it reads Gone"
    (is_gone drained)

let () =
  let c = Rc.v ~entries:(bound 8) () in
  let c = Rc.mark_pending c ~tab:(tab 1) ~seq:(seq 1) in
  let c = Rc.settle c ~tab:(tab 1) ~seq:(seq 1) ~endpoint:ep ~body:"done" in
  let (_ : Rc.t), found = Rc.find c ~tab:(tab 1) ~seq:(seq 1) ~endpoint:ep in
  check "settle replaces the tab's Pending entry with the bytes"
    (original_is found ~body:"done");
  check "settle leaves one entry, not two" (Int.equal (Rc.size c) 1)

(* --- newest wins, enforced: a stale settle cannot destroy a newer window --- *)

let () =
  (* F3 of the step-15 adversarial review. Only a non-conforming pipelining
     caller can produce a settle carrying an older seq than the live entry;
     last-writer-wins would let that stale settle destroy the newer Pending
     marker, whose duplicate would then read Gone and be told a 200 Replayed
     while the newer effect is still in flight. The comparison makes the
     .mli's "newest wins" true by construction. *)
  let c = Rc.v ~entries:(bound 8) () in
  let c = Rc.mark_pending c ~tab:(tab 1) ~seq:(seq 2) in
  let c = Rc.settle c ~tab:(tab 1) ~seq:(seq 1) ~endpoint:ep ~body:"stale" in
  let c, still_busy = Rc.find c ~tab:(tab 1) ~seq:(seq 2) ~endpoint:ep in
  check "a stale settle is discarded: the newer Pending still reads Busy"
    (is_busy still_busy);
  let c, not_forged = Rc.find c ~tab:(tab 1) ~seq:(seq 1) ~endpoint:ep in
  check "the stale settle's bytes are never answered" (is_gone not_forged);
  let c = Rc.settle c ~tab:(tab 1) ~seq:(seq 2) ~endpoint:ep ~body:"current" in
  let (_ : Rc.t), settled = Rc.find c ~tab:(tab 1) ~seq:(seq 2) ~endpoint:ep in
  check "the newest seq's own settle still lands"
    (original_is settled ~body:"current")

(* --- the caps: every way of losing an entry reads Gone --- *)

let () =
  (* T18's body-cap arm: an over-cap body is not stored AND takes the Pending
     entry with it, so the retry is told Replayed rather than 503 for an
     effect that has already finished. *)
  let c = Rc.v ~entries:(bound 8) ~body_cap:4 () in
  let c = Rc.mark_pending c ~tab:(tab 1) ~seq:(seq 1) in
  let c = Rc.settle c ~tab:(tab 1) ~seq:(seq 1) ~endpoint:ep ~body:"12345" in
  let (_ : Rc.t), over = Rc.find c ~tab:(tab 1) ~seq:(seq 1) ~endpoint:ep in
  check "a body over body_cap is not stored and reads Gone" (is_gone over);
  check "the over-cap settle leaves no entry behind" (Int.equal (Rc.size c) 0);
  check "the over-cap body is not counted against the byte budget"
    (Int.equal (Rc.bytes c) 0);
  let c = Rc.settle c ~tab:(tab 1) ~seq:(seq 2) ~endpoint:ep ~body:"4444" in
  let (_ : Rc.t), at_cap = Rc.find c ~tab:(tab 1) ~seq:(seq 2) ~endpoint:ep in
  check "a body exactly at body_cap is stored" (original_is at_cap ~body:"4444")

let () =
  (* T18's byte-budget arms. Ten-byte bodies against a 30-byte budget: the
     fifth settle overruns, and eviction walks LRU-first past a Pending entry
     (weight 0, so dropping it does not help the budget) and on to the oldest
     Settled one. Both victims must read Gone, never Busy: a 503 for an entry
     the cache has thrown away is a retry loop with no end. *)
  let c = Rc.v ~entries:(bound 8) ~max_bytes:30 () in
  let c = Rc.mark_pending c ~tab:(tab 1) ~seq:(seq 1) in
  let c = Rc.settle c ~tab:(tab 2) ~seq:(seq 1) ~endpoint:ep ~body:"0123456789" in
  let c = Rc.settle c ~tab:(tab 3) ~seq:(seq 1) ~endpoint:ep ~body:"aaaaaaaaaa" in
  let c = Rc.settle c ~tab:(tab 4) ~seq:(seq 1) ~endpoint:ep ~body:"bbbbbbbbbb" in
  check "three bodies exactly at the budget are all kept" (Int.equal (Rc.bytes c) 30);
  let c = Rc.settle c ~tab:(tab 5) ~seq:(seq 1) ~endpoint:ep ~body:"cccccccccc" in
  check "the byte budget holds after the overrun" (Rc.bytes c <= 30);
  let c, evicted_pending = Rc.find c ~tab:(tab 1) ~seq:(seq 1) ~endpoint:ep in
  check "the reply cache never loops: an evicted Pending reads Gone, not Busy"
    (is_gone evicted_pending);
  let c, evicted_settled = Rc.find c ~tab:(tab 2) ~seq:(seq 1) ~endpoint:ep in
  check "byte-budget eviction of a Settled entry reads Gone"
    (is_gone evicted_settled);
  let (_ : Rc.t), newest = Rc.find c ~tab:(tab 5) ~seq:(seq 1) ~endpoint:ep in
  check "the entry that caused the overrun survives it"
    (original_is newest ~body:"cccccccccc")

let () =
  (* The entry bound, and the recency arm: a hit touches the entry, so the tab
     a client is actively retrying is not the one eviction takes. *)
  let c = Rc.v ~entries:(bound 2) () in
  let c = Rc.settle c ~tab:(tab 1) ~seq:(seq 1) ~endpoint:ep ~body:"one" in
  let c = Rc.settle c ~tab:(tab 2) ~seq:(seq 1) ~endpoint:ep ~body:"two" in
  let c, (_ : Rc.found) = Rc.find c ~tab:(tab 1) ~seq:(seq 1) ~endpoint:ep in
  let c = Rc.settle c ~tab:(tab 3) ~seq:(seq 1) ~endpoint:ep ~body:"three" in
  check "the entry bound holds" (Int.equal (Rc.size c) 2);
  let c, victim = Rc.find c ~tab:(tab 2) ~seq:(seq 1) ~endpoint:ep in
  check "the entry bound evicts the least recently used tab" (is_gone victim);
  let (_ : Rc.t), kept = Rc.find c ~tab:(tab 1) ~seq:(seq 1) ~endpoint:ep in
  check "a tab touched by a lookup survives the eviction"
    (original_is kept ~body:"one")

(* --- the Cell shell threads the same core --- *)

let () =
  let c = Rc.Cell.v ~entries:(bound 8) () in
  Rc.Cell.mark_pending c ~tab:(tab 1) ~seq:(seq 1);
  check "the Cell answers Busy inside the window"
    (is_busy (Rc.Cell.find c ~tab:(tab 1) ~seq:(seq 1) ~endpoint:ep));
  Rc.Cell.settle c ~tab:(tab 1) ~seq:(seq 1) ~endpoint:ep ~body:"settled";
  check "the Cell answers the settled bytes after the window closes"
    (original_is (Rc.Cell.find c ~tab:(tab 1) ~seq:(seq 1) ~endpoint:ep) ~body:"settled");
  check "the Cell's snapshot carries the entry"
    (Int.equal (Rc.size (Rc.Cell.snapshot c)) 1)

let () = print_endline "reply_cache_test: all checks passed"
