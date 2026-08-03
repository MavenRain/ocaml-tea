(** The anti-CSRF gate on mutating RPC endpoints (roadmap step 8, D12).

    The gate is worth exactly as much as the store is: a 403 that still wrote
    the document would be no defence at all. So every check here is paired with
    a read of the canonical branch - the cross-origin cases assert the tag list
    is {e untouched}, the same-origin case asserts it actually grew.

    Driven against {!Shared_doc_serve}, the same handler [examples/shared_doc]'s
    binary serves, so [Append_tag] here really is one [Add_tag] Msg through
    {!Tea_server.Make.step}. A second hand-written handler in this file would
    make the store assertions meaningless.

    The negative case is not browser-testable - Chromium sets [Origin] itself
    and will not forge one - which is why it lives here rather than in the
    Playwright smoke test. *)

module Serve = Shared_doc_serve
module Server = Serve.Server
module App = Shared_doc_app.App
module R = Tea_rpc.Make (Shared_doc_rpc)

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

let body response = Lwt_main.run (Dream.body response)
let status response = Dream.status_to_int (Dream.status response)

let pinned_csp =
  "default-src 'self'; script-src 'self'; style-src 'self'; img-src 'self'; connect-src 'self'; \
   form-action 'self'; object-src 'none'; base-uri 'none'; frame-ancestors 'none'"

let has_security_headers response =
  Dream.header response "Content-Security-Policy" = Some pinned_csp
  && Dream.header response "X-Frame-Options" = Some "DENY"
  && Dream.header response "X-Content-Type-Options" = Some "nosniff"

let json = "application/json"
let repo = Lwt_main.run (Server.Store.create ())
let driver = Dream.test (Serve.handler repo)

(* [?origin]/[?host] omitted means the header is absent, which is how the
   Origin_missing / Host_missing / Both_missing denials are reached. *)
let post ?origin ?host ?(ct = json) ~path payload =
  let header name = Option.fold ~none:[] ~some:(fun v -> [ (name, v) ]) in
  let headers = (("Content-Type", ct) :: header "Origin" origin) @ header "Host" host in
  driver (Dream.request ~method_:`POST ~target:path ~headers payload)

let append_tag ?origin ?host ?ct tag =
  post ?origin ?host ?ct ~path:"/rpc/append_tag" (R.encode_req Shared_doc_rpc.Append_tag tag)

(* [Append_tag] is [Mutating], so from roadmap step 15 its 200 body rides the
   [keyed_resp] envelope: decode that, never a bare count. [Replayed] gets its
   own sentinel rather than folding into the decode failure, so a check that
   accidentally read a replay as a fresh count could not pass by looking like
   an ordinary miss. *)
let append_tag_resp_t = Shared_doc_rpc.resp_t Shared_doc_rpc.Append_tag

let count_of response =
  Tea_core.Codec.of_json (Tea_rpc.keyed_resp_t append_tag_resp_t) (body response)
  |> Result.fold
       ~error:(fun (_ : Tea_core.Codec.err) -> -1)
       ~ok:(fun (k : int Tea_rpc.keyed_resp) ->
         match k with
         | Tea_rpc.Reply n -> n
         | Tea_rpc.Replayed -> -2)

(* The canonical branch as the RPC handler sees it - the only honest witness
   that a refused request changed nothing. *)
let tags () =
  Lwt_main.run
    (Lwt.bind (Server.Store.main_session repo) (fun s ->
         Lwt.map App.tags_of (Server.Store.load s)))

(* --- the refusals: no proof, no dispatch, no write ------------------------- *)

let () =
  check "the canonical doc starts with no tags" (tags () = []);
  let cross = append_tag ~origin:"http://evil.example" ~host:"example.com" "urgent" in
  check "a cross-origin mutating POST is refused 403" (status cross = 403);
  check "the cross-origin refusal left the store untouched" (tags () = []);
  check "the 403 body is the pinned reason"
    (String.equal (body cross) "cross-origin mutating rpc rejected");
  check "the 403 refusal is text/plain; charset=utf-8 (unsniffable)"
    (Dream.header cross "Content-Type" = Some "text/plain; charset=utf-8");
  check "the 403 refusal still carries the security headers (middleware outermost)"
    (has_security_headers cross);
  (* The remaining three Origin_gate.denial arms, each a distinct absent
     header, all land on the same refusal - and none of them writes. *)
  check "a mutating POST with no Origin header is refused 403 (Origin_missing)"
    (status (append_tag ~host:"example.com" "urgent") = 403);
  check "a mutating POST with no Host header is refused 403 (Host_missing)"
    (status (append_tag ~origin:"http://example.com" "urgent") = 403);
  check "a mutating POST with neither header is refused 403 (Both_missing)"
    (status (append_tag "urgent") = 403);
  check "an origin that merely PREFIXES the host is refused 403 (no suffix trick)"
    (status (append_tag ~origin:"http://example.com.evil.test" ~host:"example.com" "urgent")
    = 403);
  check "none of the refused mutating POSTs wrote a tag" (tags () = []);
  (* Gate order: the origin check runs before the content-type and body gates,
     so a forged POST is refused without its body being read. Observable as the
     status a cross-origin request with a REFUSED content type gets - 403, the
     gate that ran first, not the 415 the same request would get same-origin. *)
  check "cross-origin with a bad content type is 403 (the origin gate runs first)"
    (status
       (append_tag ~origin:"http://evil.example" ~host:"example.com"
          ~ct:"application/x-www-form-urlencoded" "urgent")
    = 403);
  check "the same bad content type same-origin is 415 (so 403 above was the gate)"
    (status
       (append_tag ~origin:"http://example.com" ~host:"example.com"
          ~ct:"application/x-www-form-urlencoded" "urgent")
    = 415);
  check "neither content-type case wrote a tag" (tags () = [])

(* --- the pass side: a real store mutation through a real TEA step ---------- *)

let () =
  let ok = append_tag ~origin:"http://example.com" ~host:"example.com" "urgent" in
  check "a same-origin mutating POST is served 200" (status ok = 200);
  check "the reply is the resulting tag count" (count_of ok = 1);
  check "the tag is committed on the canonical branch" (tags () = [ "urgent" ]);
  check "the 200 is application/json; charset=utf-8"
    (Dream.header ok "Content-Type" = Some "application/json; charset=utf-8");
  (* https is same-origin too (the WS gate precedent), and a second distinct
     tag proves the endpoint accumulates rather than replaces. *)
  let ok2 = append_tag ~origin:"https://example.com" ~host:"example.com" "review" in
  check "an https same-origin POST is served 200" (status ok2 = 200);
  check "a second distinct tag brings the count to 2" (count_of ok2 = 2);
  check "both tags are present on the canonical branch"
    (List.sort String.compare (tags ()) = [ "review"; "urgent" ]);
  (* [tags] is an OR-Set, not an append log: re-adding an observed element is
     idempotent, so the count must not climb. *)
  let again = append_tag ~origin:"http://example.com" ~host:"example.com" "urgent" in
  check "re-appending an existing tag leaves the count at 2 (OR-Set, not a log)"
    (status again = 200 && count_of again = 2);
  (* Every accepted mutation is an ordinary commit, which is what puts it on
     the live-view watch every peer is subscribed to. *)
  let history =
    Lwt_main.run
      (Lwt.bind (Server.Store.main_session repo) Server.Store.history)
  in
  check "each accepted mutating POST landed as a commit in the event log"
    (List.length history >= 3)

(* --- concurrent mutation of the shared branch ------------------------------
   [Server.step] is load-step-commit with no compare-and-set, and this endpoint
   is the first to put DIFFERENT clients on ONE branch, so the handler holds a
   lock across the whole read-modify-write. Both calls below are created before
   the scheduler runs either, and both come from the SAME handler value (one
   lock), so this is the closest the suite gets to a real interleaving.

   HONEST LIMIT, established by mutation: deleting the lock leaves these checks
   GREEN. Under Irmin_mem with an [Add_tag] that emits [Cmd.none] there is no
   Lwt yield anywhere between load and commit, so two in-flight steps run to
   completion one after the other no matter what. What these checks pin is the
   OUTCOME (both appends land, the reply agrees with the branch); the lock
   itself is a defence against the yielding cases - a Cmd tail, or the pack
   backend - that this suite cannot construct. See DESIGN section 10, R10.

   NB: [Irmin_mem]'s backend is process-global, so a second [Store.create ()]
   would NOT give a clean slate - the counts below are deliberately relative to
   what the checks above already committed. *)

let () =
  let h = Serve.rpc_handler repo in
  let before = tags () in
  (* Since step 15 the app's handler is the KEYED one, so each answer arrives
     paired with the store water its own commit minted. The water is what the
     guarded route persists as that delivery's floor; these checks are about
     the counts, so they read [fst] off each pair. *)
  let counts =
    Lwt_main.run
      (Lwt.both
         (h.Serve.Rpc.handle_keyed Shared_doc_rpc.Append_tag "alpha")
         (h.Serve.Rpc.handle_keyed Shared_doc_rpc.Append_tag "beta"))
    |> fun ((a, (_ : Tea_core.Prim.Store_water.t)), (b, (_ : Tea_core.Prim.Store_water.t)))
      -> (a, b)
  in
  let after = tags () in
  check "two appends driven together both land on the shared branch"
    (List.mem "alpha" after && List.mem "beta" after);
  check "the shared branch grew by exactly the two tags"
    (List.length after = List.length before + 2);
  check "the later of the two replies counts every tag then present"
    (Stdlib.max (fst counts) (snd counts) = List.length after)

(* --- the 200 body shape is fixed by KIND, not by the request (step 15) ----- *)

(* A [Mutating] endpoint answers the [keyed_resp] envelope on 200 always, keyed
   request or not — these posts carry no delivery key at all and are still
   enveloped. The reason is on the client: [expect] is fixed when [call] builds
   the command and cannot see whether the runtime attached a key, so a body
   shape that varied per request would be undecodable by construction. Pinned
   in BOTH directions (enveloped decodes, bare does not) so a reclassification
   that moved an endpoint between the two shapes reds here rather than in a
   browser. The read-only half of the pair is the section below, which decodes
   the same handler's answer bare and reads the real computation out of it. *)
let () =
  let ok = append_tag ~origin:"http://example.com" ~host:"example.com" "shaped" in
  check "a keyless Mutating POST is still served 200" (status ok = 200);
  check "the Mutating 200 body decodes as keyed_resp Reply, carrying the count"
    (count_of ok > 0);
  check "the Mutating 200 body does NOT decode as a bare resp (it is enveloped)"
    (Result.is_error (R.decode_resp Shared_doc_rpc.Append_tag (body ok)));
  check "a keyless Mutating POST still applies (no guard yet, at-most-once)"
    (List.mem "shaped" (tags ()))

(* --- read-only endpoints stay ungated ------------------------------------- *)

let () =
  let before = List.sort String.compare (tags ()) in
  let stats_body =
    R.encode_req Shared_doc_rpc.Doc_stats { Shared_doc_rpc.title = "hello"; body = "a b c" }
  in
  let cross_stats =
    post ~origin:"http://evil.example" ~host:"example.com" ~path:"/rpc/doc_stats" stats_body
  in
  check "a cross-origin READ-ONLY POST is still served 200 (kind-driven gating)"
    (status cross_stats = 200);
  check "the ungated read-only reply is the real computation"
    (R.decode_resp Shared_doc_rpc.Doc_stats (body cross_stats)
     |> Result.fold
          ~error:(fun (_ : Tea_core.Codec.err) -> false)
          ~ok:(fun (s : Shared_doc_rpc.stats_resp) ->
            s.title_len = 5 && s.word_count = 3));
  check "a cross-origin history_count is still served 200"
    (status
       (post ~origin:"http://evil.example" ~host:"example.com" ~path:"/rpc/history_count"
          (R.encode_req Shared_doc_rpc.History_count ()))
    = 200);
  check "the read-only traffic wrote nothing"
    (List.sort String.compare (tags ()) = before);
  Printf.printf
    "\nMutating rpc endpoints dispatch only behind a same-origin proof (roadmap step 8, D12).\n%!"
