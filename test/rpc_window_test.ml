(** The window between a [Fresh] verdict and its settle (roadmap step 15, D20.2):
    what a second delivery sees while the first one's effect is still running,
    and what the first one's reply says once a sibling has written under it.

    Separate from {!Rpc_once_test} for one mechanical reason. These checks have
    to deliver a SECOND request while the first is suspended, which means the
    driver must stay inside Lwt: [Dream.test] wraps each call in its own
    [Lwt_main.run], and a nested [Lwt_main.run] is a hard failure. So this suite
    applies the handler directly ([Dream.handler] is [request -> response Lwt.t])
    and runs each scenario as one Lwt computation, with the interposed delivery
    composed inside it.

    The seam is {!Tea_server.Rpc.routes_once}'s [?on_taken], threaded through
    {!Shared_doc_serve.handler}. It fires between the verdict (with
    [mark_pending] already recorded) and the handler call, which is the only
    instant at which a duplicate can observe a [Pending] entry. Driving the real
    exported handler rather than a rebuilt copy of the routes is the same
    doctrine {!Rpc_once_test} states: an assertion about a second hand-written
    pipeline is an assertion about the test.

    The cookie threading is load-bearing here for the reason it is there: the
    de-duplication namespace is derived from the session (D20.1), so a delivery
    that dropped the cookie would be [Fresh] however correct the guard is. *)

module Serve = Shared_doc_serve
module Server = Serve.Server
module App = Shared_doc_app.App
module R = Tea_rpc.Make (Shared_doc_rpc)

let ( let* ) = Lwt.bind

let failures = ref 0

let check (label : string) (ok : bool) : unit =
  if ok then Printf.printf "ok   - %s\n%!" label
  else (
    incr failures;
    Printf.printf "FAIL - %s\n%!" label)

(* The interposed delivery, as a one-shot. [route_once] takes [?on_taken] once,
   at route-construction time, so a suite that wants a different interposition
   per scenario has to indirect through a cell. One-shot rather than sticky
   because every scenario below wants exactly one interposition: a sticky hook
   would re-enter on the interposed delivery's own window if that delivery were
   ever [Fresh], and the re-entry would be silent. *)
let hook : (unit -> unit Lwt.t) ref = ref (fun () -> Lwt.return_unit)

let arm (f : unit -> unit Lwt.t) : unit =
  hook :=
    fun () ->
      hook := (fun () -> Lwt.return_unit);
      f ()

let on_taken () : unit Lwt.t = !hook ()
let repo = Lwt_main.run (Server.Store.create ())
let app : Dream.handler = Serve.handler ~on_taken repo

(* The session cookie a response sets, reduced to a [Cookie:] request value.
   [String.split_on_char] rather than [String.sub]: the attribute list after the
   first ';' is what a browser drops too, and this stays total. *)
let cookie_of (response : Dream.response) : string =
  Dream.header response "Set-Cookie"
  |> Option.fold ~none:"" ~some:(fun (c : string) ->
         match String.split_on_char ';' c with
         | [] -> "" (* unreachable: split_on_char never returns [] *)
         | first :: (_ : string list) -> first)

type answered =
  { status : int
  ; body : string
  ; set_cookie : string
  }

let request ?(key : string option) ~(cookie : string) ~(path : string) (payload : string)
    : Dream.request =
  let optional (name : string) (v : string option) : (string * string) list =
    Option.fold v ~none:[] ~some:(fun (value : string) -> [ (name, value) ])
  in
  let cookie_header = if String.equal cookie "" then [] else [ ("Cookie", cookie) ] in
  let headers =
    [ ("Content-Type", "application/json")
    ; ("Origin", "http://example.com")
    ; ("Host", "example.com")
    ]
    @ optional Tea_rpc.key_header key
    @ cookie_header
  in
  Dream.request ~method_:`POST ~target:path ~headers payload

let drive (r : Dream.request) : answered Lwt.t =
  let* response = app r in
  let* body = Dream.body response in
  Lwt.return
    { status = Dream.status_to_int (Dream.status response)
    ; body
    ; set_cookie = cookie_of response
    }

let append ?key ~(cookie : string) (tag : string) : answered Lwt.t =
  drive
    (request ?key ~cookie ~path:"/rpc/append_tag"
       (R.encode_req Shared_doc_rpc.Append_tag tag))

type answer =
  | Reply of int
  | Replayed
  | Undecodable

let answer_of (body : string) : answer =
  Tea_core.Codec.of_json
    (Tea_rpc.keyed_resp_t (Shared_doc_rpc.resp_t Shared_doc_rpc.Append_tag))
    body
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

let reply_count (a : answer) : int option =
  match a with
  | Reply n -> Some n
  | Replayed | Undecodable -> None

(* The canonical branch, the only honest witness that an interposed delivery
   changed nothing: the commit log for "how many writes landed", the tag set for
   "which ones". Both are read between scenarios, never inside a window. *)
let commits () : int =
  Lwt_main.run
    (Lwt.map List.length (Lwt.bind (Server.Store.main_session repo) Server.Store.history))

let tags () : string list =
  Lwt_main.run
    (Lwt.bind (Server.Store.main_session repo) (fun s ->
         Lwt.map App.tags_of (Server.Store.load s)))

let occurrences (tag : string) : int = List.length (List.filter (String.equal tag) (tags ()))
let tab = "c3d2e1f0a9b8c7d6e5f4a3b2c1d0e9f8"
let key (seq : int) : string = tab ^ ":" ^ string_of_int seq

(* One keyless warm-up to mint the session the whole suite speaks under. Keyless,
   so it consumes no sequence number and leaves every floor free. *)
let cookie =
  let warm = Lwt_main.run (append ~cookie:"" "warm-up") in
  check "the window suite holds a session cookie to thread through every delivery"
    (not (String.equal warm.set_cookie ""));
  warm.set_cookie

(* --- T7: a concurrent duplicate cannot double-commit ----------------------- *)

(* The RPC-side two-writer check the step-14 residual asked for. The duplicate
   is delivered while the Fresh apply is held, which is the interleave a guard
   consulted AFTER the handler would admit: both deliveries would find no floor,
   and both would commit. *)
let () =
  let interposed : answered option ref = ref None in
  arm (fun () ->
      let* a = append ~key:(key 1) ~cookie "t7" in
      interposed := Some a;
      Lwt.return_unit);
  let before = commits () in
  let original = Lwt_main.run (append ~key:(key 1) ~cookie "t7") in
  let after = commits () in
  check "the interposed delivery really landed inside the window"
    (Option.is_some !interposed);
  check "the taken delivery answers a reply" (is_reply (answer_of original.body));
  check "a duplicate delivered INSIDE the window lands NO second commit"
    (after - before = 1);
  check "and the tag it carried is on the branch exactly once" (occurrences "t7" = 1)

(* --- T14: the Pending window answers 503, then the original bytes ---------- *)

(* [Busy] is the one verdict the reply cache reports that is not an answer about
   the effect: it means "taken, fate not yet known". Answering [Replayed] here
   instead would be the single lie the envelope exists to prevent - it asserts
   the effect is complete while the handler is still running. *)
let () =
  let interposed : answered option ref = ref None in
  arm (fun () ->
      let* a = append ~key:(key 2) ~cookie "t14" in
      interposed := Some a;
      Lwt.return_unit);
  let before = commits () in
  let original = Lwt_main.run (append ~key:(key 2) ~cookie "t14") in
  let after = commits () in
  let retry = Lwt_main.run (append ~key:(key 2) ~cookie "t14") in
  let inside (f : answered -> bool) : bool = Option.fold ~none:false ~some:f !interposed in
  check "a duplicate inside the Pending window is refused 503, not answered 200"
    (inside (fun (a : answered) -> a.status = 503));
  check "the 503 carries no keyed reply body at all"
    (inside (fun (a : answered) ->
         match answer_of a.body with
         | Undecodable -> true
         | Reply (_ : int) | Replayed -> false));
  check "the taken delivery still answers 200 with its own reply"
    (original.status = 200 && is_reply (answer_of original.body));
  check "the retry after the window answers the ORIGINAL bytes"
    (retry.status = 200 && String.equal retry.body original.body);
  check "the whole window landed exactly one commit" (after - before = 1)

(* --- T2b: a sibling write in the window does not forge the reply ----------- *)

(* J2/J18, the honesty pin beyond M4. A sibling lands on the canonical branch
   between the take and the handler call, so the taken delivery commits over a
   branch that moved under it. Two things have to hold at once: the sibling is
   not erased (D19 test-and-set reconciles rather than overwrites), and the
   bytes the retry replays are the ones THAT delivery computed - not a recompute
   over whatever the branch holds at replay time. The later writer below is what
   separates those two readings; without it, a recompute and a replay would
   agree and the byte-equality check would be vacuous. *)
let () =
  arm (fun () ->
      (* Keyless, so it never reaches the guard and cannot re-enter this seam. *)
      let* (_ : answered) = append ~cookie "t2b-sibling" in
      Lwt.return_unit);
  let before = commits () in
  let original = Lwt_main.run (append ~key:(key 3) ~cookie "t2b") in
  let after = commits () in
  check "the sibling and the taken delivery each commit: no lost update in the window"
    (after - before = 2);
  check "the sibling's tag survived the taken delivery's commit"
    (List.mem "t2b-sibling" (tags ()));
  check "the taken delivery's reply counts the branch it actually committed"
    (Option.fold ~none:false
       ~some:(fun (n : int) -> n = List.length (tags ()))
       (reply_count (answer_of original.body)));
  let (_ : answered) = Lwt_main.run (append ~cookie "t2b-later") in
  let retry = Lwt_main.run (append ~key:(key 3) ~cookie "t2b") in
  check "a later writer moved the count, so the byte-equality below is not vacuous"
    (Option.fold ~none:false
       ~some:(fun (n : int) -> n <> List.length (tags ()))
       (reply_count (answer_of original.body)));
  check "the retry replays the taken delivery's OWN bytes, not a recompute"
    (String.equal retry.body original.body);
  check "and the retry applied nothing further" (occurrences "t2b" = 1)

(* --- T21: a rejecting apply is released, retried, and applied exactly once - *)

(* F2 of the step-15 adversarial review, the barrier-plus-release pin. The
   rejection is injected at the [?on_taken] seam, which lives INSIDE the
   barrier precisely so the whole take-to-settle window is covered: a rejected
   apply must not strand its consumed seq, or the client's retries drain
   through Busy into Gone and a 200 [Replayed] for an effect that never
   happened - the silent-loss direction the family forbids. With the release,
   the retry reads [Fresh], re-runs the handler, and lands the effect exactly
   once. *)
let () =
  arm (fun () -> Lwt.fail (Failure "window-test injected rejection"));
  let before = commits () in
  let failed = Lwt_main.run (append ~key:(key 4) ~cookie "t21") in
  let mid = commits () in
  let retry = Lwt_main.run (append ~key:(key 4) ~cookie "t21") in
  let after = commits () in
  check "a rejecting apply answers 500 on the transport channel, not a keyed 200"
    (failed.status = 500);
  check "the rejected delivery committed nothing" (mid - before = 0);
  check "the retry of a released delivery reads Fresh and answers its own reply"
    (retry.status = 200 && is_reply (answer_of retry.body));
  check "the released seq's effect landed exactly once, on the retry"
    (after - before = 1 && occurrences "t21" = 1)

(* --- T22: a duplicate inside a FAILING window is refused, never told 200 --- *)

(* The WS tier's F5' parity control (step 17, D22): while an attempt whose fate
   is "will reject" is still in flight, the keyed duplicate must be refused
   [Busy] - a 200 here, [Reply] or [Replayed], would assert an effect the
   release is about to disown, the exact overclaim [Ack_park] closes on the
   push channel. T14 pins the Busy window for a delivery that will land; this
   arm pins it for one that will not, so the HTTP tier cannot regress to a
   premature answer while the WS tier is being changed. *)
let () =
  let interposed : answered option ref = ref None in
  arm (fun () ->
      let* a = append ~key:(key 5) ~cookie "t22" in
      interposed := Some a;
      Lwt.fail (Failure "parity-test injected rejection"));
  let before = commits () in
  let failed = Lwt_main.run (append ~key:(key 5) ~cookie "t22") in
  let mid = commits () in
  let retry = Lwt_main.run (append ~key:(key 5) ~cookie "t22") in
  let after = commits () in
  let inside (f : answered -> bool) : bool =
    Option.fold ~none:false ~some:f !interposed
  in
  check "a duplicate inside a FAILING window is refused 503, never told 200"
    (inside (fun (a : answered) -> a.status = 503));
  check "the doomed delivery answers 500 and commits nothing"
    (failed.status = 500 && mid - before = 0);
  check "the retry reads Fresh after the release and answers its own reply"
    (retry.status = 200 && is_reply (answer_of retry.body));
  check "the effect landed exactly once, on the retry"
    (after - before = 1 && occurrences "t22" = 1)

let () =
  if !failures > 0 then (
    Printf.printf "rpc_window_test: %d check(s) FAILED\n%!" !failures;
    exit 1)
  else Printf.printf "rpc_window_test: all checks passed\n%!"
