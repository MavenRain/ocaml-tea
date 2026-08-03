(** The RPC channel's exactly-once guarantee, driven against the REAL
    {!Shared_doc_serve.handler} over [Dream.test] (roadmap step 15, D20).

    The csrf_test doctrine applies here for the same reason it applies there:
    the assertions mean nothing against a second hand-written copy of the
    pipeline, so this suite posts at the same linked handler the binary serves
    and pairs every verdict with a read of the canonical branch.

    {b The precondition that makes any of it non-vacuous.} The de-duplication
    namespace is derived from the session cookie (D20.1), so a retry that does
    not carry the first response's [Set-Cookie] lands in a DIFFERENT namespace
    and is judged [Fresh] no matter how correct the guard is. Every scenario
    below threads the cookie explicitly; {!different_cookie_reapplies} is the
    check that proves the threading is load-bearing rather than decorative. *)

module Serve = Shared_doc_serve
module Server = Serve.Server
module App = Shared_doc_app.App
module R = Tea_rpc.Make (Shared_doc_rpc)

let failures = ref 0

let check (label : string) (ok : bool) : unit =
  if ok then Printf.printf "ok   - %s\n%!" label
  else (
    incr failures;
    Printf.printf "FAIL - %s\n%!" label)

let repo = Lwt_main.run (Server.Store.create ())
let driver = Dream.test (Serve.handler repo)
let body (response : Dream.response) : string = Lwt_main.run (Dream.body response)
let status (response : Dream.response) : int = Dream.status_to_int (Dream.status response)

(* The session cookie a response sets, reduced to a [Cookie:] request value.
   [String.split_on_char] rather than [String.sub]: the attribute list after the
   first ';' is what a browser drops too, and this stays total. *)
let cookie_of (response : Dream.response) : string =
  Dream.header response "Set-Cookie"
  |> Option.fold ~none:"" ~some:(fun (c : string) ->
         match String.split_on_char ';' c with
         | [] -> "" (* unreachable: split_on_char never returns [] *)
         | first :: (_ : string list) -> first)

let post ?(key : string option) ~(cookie : string) ~(path : string) (payload : string) :
    Dream.response =
  let optional (name : string) (v : string option) : (string * string) list =
    Option.fold v ~none:[] ~some:(fun (value : string) -> [ (name, value) ])
  in
  let cookie_header =
    if String.equal cookie "" then [] else [ ("Cookie", cookie) ]
  in
  let headers =
    [ ("Content-Type", "application/json")
    ; ("Origin", "http://example.com")
    ; ("Host", "example.com")
    ]
    @ optional Tea_rpc.key_header key
    @ cookie_header
  in
  driver (Dream.request ~method_:`POST ~target:path ~headers payload)

let append ?key ~(cookie : string) (tag : string) : Dream.response =
  post ?key ~cookie ~path:"/rpc/append_tag"
    (R.encode_req Shared_doc_rpc.Append_tag tag)

type answer =
  | Reply of int
  | Replayed
  | Undecodable

let answer_of (response : Dream.response) : answer =
  Tea_core.Codec.of_json
    (Tea_rpc.keyed_resp_t (Shared_doc_rpc.resp_t Shared_doc_rpc.Append_tag))
    (body response)
  |> Result.fold
       ~error:(fun (_ : Tea_core.Codec.err) -> Undecodable)
       ~ok:(fun (k : int Tea_rpc.keyed_resp) ->
         match k with
         | Tea_rpc.Reply n -> Reply n
         | Tea_rpc.Replayed -> Replayed)

let is_reply (a : answer) : bool =
  match a with
  | Reply (_ : int) -> true
  | Replayed | Undecodable -> false

let is_replayed (a : answer) : bool =
  match a with
  | Replayed -> true
  | Reply (_ : int) | Undecodable -> false

(* The canonical branch as the RPC handler writes it: the only honest witness
   that a refused or de-duplicated delivery changed nothing. *)
let tags () : string list =
  Lwt_main.run
    (Lwt.bind (Server.Store.main_session repo) (fun s ->
         Lwt.map App.tags_of (Server.Store.load s)))

let tab = "a1b2c3d4e5f60718293a4b5c6d7e8f90"
let key (seq : int) : string = tab ^ ":" ^ string_of_int seq

(* One warm-up post to mint the session the whole suite then speaks under. It
   is keyless, so it consumes no sequence number and leaves every floor free. *)
let cookie =
  let warm = append ~cookie:"" "warm-up" in
  let c = cookie_of warm in
  check "the test harness holds a session cookie to thread through retries"
    (not (String.equal c ""));
  c

(* --- T1: a fresh keyed delivery applies exactly once ----------------------- *)

let first = append ~key:(key 1) ~cookie "once"
let after_first = tags ()

let () =
  check "a fresh keyed append is served 200" (status first = 200);
  check "a fresh keyed append answers Reply, not Replayed" (is_reply (answer_of first));
  check "a fresh keyed append applies once" (List.mem "once" after_first)

(* --- T2: a retry with the process alive answers the original bytes --------- *)

let () =
  (* A competing writer bumps the count between the two deliveries, so a
     recompute and a replay are distinguishable: byte-equality here means the
     answer came from the cache, not from running the handler again. *)
  let (_ : Dream.response) = append ~cookie "competitor" in
  let retry = append ~key:(key 1) ~cookie "once" in
  check "a retry of a taken key is served 200" (status retry = 200);
  check "a retry answers the ORIGINAL bytes, not a recompute"
    (String.equal (body retry) (body first));
  check "a retry does not apply a second time"
    (List.length (List.filter (String.equal "once") (tags ())) = 1)

(* --- T4: a gap is refused without collateral ------------------------------- *)

let () =
  let before = tags () in
  let gapped = append ~key:(key 3) ~cookie "gap" in
  check "a gapped delivery is refused 400" (status gapped = 400);
  check "a gapped delivery applies nothing" (not (List.mem "gap" (tags ())));
  check "a gapped delivery consumes nothing else" (List.length (tags ()) = List.length before);
  (* The seq the gap skipped is still available: the refusal burned nothing. *)
  let filled = append ~key:(key 2) ~cookie "fills-the-gap" in
  check "the skipped sequence number is still Fresh after the gap"
    (is_reply (answer_of filled));
  check "the skipped sequence number applies" (List.mem "fills-the-gap" (tags ()))

(* --- T9 (partial): a malformed key consumes nothing ------------------------ *)

let () =
  let before = tags () in
  let bad = append ~key:"not-a-key" ~cookie "malformed" in
  check "a malformed delivery key is refused 400" (status bad = 400);
  check "a malformed delivery key applies nothing" (not (List.mem "malformed" (tags ())));
  check "a malformed delivery key consumes no sequence number"
    (List.length (tags ()) = List.length before);
  (* Seq 3, not 4: the floor stands at 2, and the refusals above burned
     nothing, so 3 is exactly the next dense position. Asking for 4 here would
     be a gap, and a green check would then only be saying the guard still
     refuses gaps. *)
  let fresh = append ~key:(key 3) ~cookie "after-malformed" in
  check "the next sequence number is still Fresh after a malformed key"
    (is_reply (answer_of fresh))

(* --- T13 (the namespace pin): the wire tab is not a floor key -------------- *)

(* The same client tab and the same seq under a DIFFERENT session apply under
   their OWN floor. This is what makes the derivation worth having: an attacker
   who learns a victim's tab id (DESIGN R14 documents that disclosure) still
   cannot reach the victim's floor, because the namespace half never rides the
   wire. It is also the anti-vacuity check for the cookie threading above: were
   the cookie ignored, the [Fresh] here would be indistinguishable from a guard
   that never deduplicated at all, which the T2 retry already refutes. *)
let different_cookie_reapplies () : unit =
  let stranger = append ~cookie:"" "stranger-warm" in
  let other = cookie_of stranger in
  check "a second session mints a different cookie" (not (String.equal other cookie));
  let forged = append ~key:(key 1) ~cookie:other "forged" in
  check "a forged client tab under another cookie is Fresh in its OWN namespace"
    (is_reply (answer_of forged));
  check "a forged client tab under another cookie applies under its own floor"
    (List.mem "forged" (tags ()));
  (* And the victim's own stream is untouched: seq 1 is still consumed for
     them. It answers [Replayed] rather than the original bytes because newer
     sequence numbers have settled over that floor tab's single cache entry
     since — the ghost-duplicate arm (M4's anchor). That is the designed
     degradation: effect certain, value lost, never another apply and never a
     newer delivery's bytes handed to an older seq. *)
  let before = tags () in
  let victim_retry = append ~key:(key 1) ~cookie "once" in
  check "the victim's consumed seq answers Replayed, never a newer seq's bytes"
    (is_replayed (answer_of victim_retry));
  check "the victim's consumed seq does not apply again"
    (List.length (tags ()) = List.length before)

let () = different_cookie_reapplies ()

(* --- T1's ordering arm: the 200 waits for the floor's append (M11) --------- *)

(* "Persist, then respond" and "respond, then persist" are indistinguishable to
   every check above: the guard's mirror is updated synchronously either way,
   and a journal append left un-awaited still completes on the next scheduler
   pump, long before any later read could miss it. The only honest observable
   is the response PROMISE itself: with the sink's append held open on a gate,
   the 200 must still be asleep, because the acknowledgement is defined to
   follow the persist attempt on both persist outcomes (D20). A pipeline that
   answered first resolves here with the gate still closed. Driven through
   [Serve.handler] like everything else in this file, but held as a promise
   rather than run through [Dream.test], which would block on exactly the
   sleep this arm exists to observe. *)
let ordering_arm () : unit =
  let gate, release = Lwt.wait () in
  let appended = ref 0 in
  let sink : Tea_server.Guard_sink.t =
    { Tea_server.Guard_sink.append =
        (fun (_ : Tea_server.Guard_sink.event) ->
          Lwt.bind gate (fun () ->
              incr appended;
              Lwt.return (Ok ())))
    }
  in
  let guard =
    Tea_server.Durable_guard.v
      ~sessions:Tea_server.Replay_guard.default_rpc_replicas
      ~tabs:Tea_server.Replay_guard.default_rpc_tabs ~sink
      ~floors:Tea_server.Durable_guard.Floors.empty ()
  in
  let once = Tea_server.Rpc_once.v ~guard ~floor_replica:Server.Store.main_replica in
  let gated_handler = Serve.handler ~once repo in
  let request =
    Dream.request ~method_:`POST ~target:"/rpc/append_tag"
      ~headers:
        [ ("Content-Type", "application/json")
        ; ("Origin", "http://example.com")
        ; ("Host", "example.com")
        ; (Tea_rpc.key_header, key 1)
        ; ("Cookie", cookie)
        ]
      (R.encode_req Shared_doc_rpc.Append_tag "t1-ordering")
  in
  let resolved (p : Dream.response Lwt.t) : bool =
    match Lwt.state p with
    | Lwt.Return (_ : Dream.response) -> true
    | Lwt.Fail (_ : exn) -> true
    | Lwt.Sleep -> false
  in
  let rec pump (n : int) : unit Lwt.t =
    if n = 0 then Lwt.return_unit
    else Lwt.bind (Lwt.pause ()) (fun () -> pump (n - 1))
  in
  Lwt_main.run
    (let resp_p = gated_handler request in
     Lwt.bind (pump 64) (fun () ->
         check "the keyed 200 is still unresolved while the floor's append is held open"
           (not (resolved resp_p));
         Lwt.wakeup release ();
         Lwt.bind resp_p (fun (resp : Dream.response) ->
             check "releasing the append lets the same delivery answer 200"
               (status resp = 200);
             check "and the acknowledgement followed exactly one append" (!appended = 1);
             Lwt.return_unit)))

let () = ordering_arm ()

let () =
  if !failures > 0 then (
    Printf.printf "rpc_once_test: %d check(s) FAILED\n%!" !failures;
    exit 1)
  else Printf.printf "rpc_once_test: all checks passed\n%!"
