(** End-to-end proof of the Dream tier (roadmap step 1): drive the Counter
    over HTTP with [Dream.test] — no socket. Checks SSR, the On_click →
    form-post rewrite, per-session branch isolation, CSRF enforcement, undo,
    and the two client-error paths. Roadmap step 3 is proven the same way:
    [live_session] is driven over an in-memory [live_transport] — no
    WebSocket handshake — and [same_origin] over constructed requests. *)

module Server = Tea_server.Make (Counter_app.App)
module Codec = Tea_core.Codec.Make (Counter_app.App)

(* The typed-RPC dispatcher and its derived client codecs (roadmap step 7).
   The [Rpc] functor is App-independent — it is mounted onto the Counter
   [Server] here purely to reuse one [Store]; [History_count] counts the
   canonical branch, so the store-backed proof commits directly to [main]
   rather than coupling to a session cookie (deviation #2). *)
module Rpc_ep = Tea_server.Rpc (Shared_doc_rpc)
module R = Tea_rpc.Make (Shared_doc_rpc)

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

let contains ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec at i = i + n <= h && (String.sub haystack i n = needle || at (i + 1)) in
  at 0

(* The substring after [marker] up to the next '"'. Sufficient for pulling the
   CSRF token out of the rendered hidden field: Dream tokens are base64url, so
   HTML escaping never rewrites them. *)
let extract_after ~marker body =
  let ml = String.length marker in
  let rec find i =
    if i + ml > String.length body then None
    else if String.sub body i ml = marker then Some (i + ml)
    else find (i + 1)
  in
  find 0
  |> Option.map (fun start ->
         String.index_from_opt body start '"'
         |> Option.fold ~none:"" ~some:(fun stop -> String.sub body start (stop - start)))
  |> Option.value ~default:""

let csrf_of body = extract_after ~marker:"name=\"dream.csrf\" value=\"" body

(* The session cookie a response sets, as a Cookie: header value. *)
let cookie_of response =
  Dream.header response "Set-Cookie"
  |> Option.map (fun c ->
         String.index_opt c ';' |> Option.fold ~none:c ~some:(fun i -> String.sub c 0 i))
  |> Option.value ~default:""

let body response = Lwt_main.run (Dream.body response)
let status response = Dream.status_to_int (Dream.status response)

(* The exact policy [Tea_safe.Security_headers.strict] serialises to; pinned
   here so a drift in the compiled-in CSP string is a test failure, not a silent
   loosening. *)
let pinned_csp =
  "default-src 'self'; script-src 'self'; style-src 'self'; img-src 'self'; connect-src 'self'; \
   form-action 'self'; object-src 'none'; base-uri 'none'; frame-ancestors 'none'"

(* The [secure_headers] middleware is outermost, so every response, error and
   403 responses included, must carry the strict header set. *)
let has_security_headers response =
  Dream.header response "Content-Security-Policy" = Some pinned_csp
  && Dream.header response "X-Frame-Options" = Some "DENY"
  && Dream.header response "X-Content-Type-Options" = Some "nosniff"

let get handle ~cookie path =
  handle (Dream.request ~method_:`GET ~target:path ~headers:[ ("Cookie", cookie) ] "")

let post handle ~cookie ~token ~path fields =
  handle
    (Dream.request ~method_:`POST ~target:path
       ~headers:
         [ ("Cookie", cookie); ("Content-Type", "application/x-www-form-urlencoded") ]
       (Dream.to_form_urlencoded (("dream.csrf", token) :: fields)))

(* The click Msgs an element actually carries, for the invariant check. *)
let clicks_of (h : Counter_app.App.msg Tea_core.Html.t) : Counter_app.App.msg list =
  match h with
  | Tea_core.Html.Text _ -> []
  | Tea_core.Html.Element (_, attrs, _) ->
    List.filter_map
      (fun (a : Counter_app.App.msg Tea_core.Html.attr) ->
        match a with
        | Tea_core.Html.On_click m -> Some m
        | Tea_core.Html.Attr _ -> None
        | Tea_core.Html.On_input _ -> None)
      attrs

let () =
  let repo = Lwt_main.run (Server.Store.create ()) in
  let handle = Dream.test (Server.handler repo) in
  let inc = Codec.msg_to_json Counter_app.App.Increment in

  (* The Html invariant the form-post rewrite relies on: at most one On_click
     per element, first wins, enforced at construction. *)
  let doubled =
    Tea_core.Html.button
      ~attrs:
        [ Tea_core.Html.on_click Counter_app.App.Increment
        ; Tea_core.Html.on_click Counter_app.App.Decrement
        ]
      [ Tea_core.Html.text "x" ]
  in
  check "an element keeps only its first On_click"
    (clicks_of doubled = [ Counter_app.App.Increment ]);

  (* First visit: session established, SSR of the init model. *)
  let r0 = handle (Dream.request ~method_:`GET ~target:"/" "") in
  check "GET / responds 200" (status r0 = 200);
  check "GET / carries the pinned Content-Security-Policy"
    (Dream.header r0 "Content-Security-Policy" = Some pinned_csp);
  check "GET / denies framing (X-Frame-Options: DENY)"
    (Dream.header r0 "X-Frame-Options" = Some "DENY");
  check "GET / sends X-Content-Type-Options: nosniff"
    (Dream.header r0 "X-Content-Type-Options" = Some "nosniff");
  let cookie = cookie_of r0 in
  check "GET / establishes a session cookie" (cookie <> "");
  let b0 = body r0 in
  check "SSR shows the init model (count 0)" (contains ~needle:">0<" b0);
  check "On_click sites become POST forms" (contains ~needle:"action=\"/msg\"" b0);
  check "forms carry the Repr-JSON Msg" (contains ~needle:"Increment" b0);
  check "the page embeds the undo control" (contains ~needle:"action=\"/undo\"" b0);
  let token0 = csrf_of b0 in
  check "the page embeds a csrf token" (token0 <> "");

  (* Two increments through the form-post path. *)
  let r1 = post handle ~cookie ~token:token0 ~path:"/msg" [ ("msg", inc) ] in
  check "POST /msg redirects (303)" (status r1 = 303);
  let b1 = body (get handle ~cookie "/") in
  check "count is 1 after one Increment" (contains ~needle:">1<" b1);
  let r2 = post handle ~cookie ~token:(csrf_of b1) ~path:"/msg" [ ("msg", inc) ] in
  check "second POST /msg redirects" (status r2 = 303);
  let b2 = body (get handle ~cookie "/") in
  check "count is 2 after two Increments" (contains ~needle:">2<" b2);

  (* Branch-per-session: a cookie-less visitor sees a fresh model. *)
  let b_fresh = body (handle (Dream.request ~method_:`GET ~target:"/" "")) in
  check "a fresh session sees its own branch (count 0)" (contains ~needle:">0<" b_fresh);

  (* Undo walks the commit chain. *)
  let r3 = post handle ~cookie ~token:(csrf_of b2) ~path:"/undo" [] in
  check "POST /undo redirects" (status r3 = 303);
  let b3 = body (get handle ~cookie "/") in
  check "undo restores count 1" (contains ~needle:">1<" b3);

  (* Step 19 (R10b): a denied undo surfaces as its own redirect, never as a
     silent success. [~undo_interpose] lands a racing Increment between the
     handler's witness and the guarded move - the one window that matters -
     so the denial is driven in program order. A separate handler instance:
     memory sessions are per-handler, so this flow mints its own cookie. The
     rig needs TWO commits before the undo - a one-commit head refuses one
     arm earlier (At_root, a silent no-op redirect) without consulting the
     guard. *)
  let racer (s : Server.Store.session) : unit Lwt.t =
    let open Lwt.Syntax in
    let* w = Server.Store.load_based s in
    let ctx = Server.Store.ctx_of_session s in
    let* (_ : Server.Store.committed) =
      Server.Store.commit_based w ~label:"racer"
        (fst
           (Counter_app.App.update ctx Counter_app.App.Increment
              (Server.Store.based_model w)))
    in
    Lwt.return_unit
  in
  let handle_d = Dream.test (Server.handler ~undo_interpose:racer repo) in
  let rd0 = handle_d (Dream.request ~method_:`GET ~target:"/" "") in
  let cookie_d = cookie_of rd0 in
  let bd0 = body rd0 in
  let rd1 = post handle_d ~cookie:cookie_d ~token:(csrf_of bd0) ~path:"/msg" [ ("msg", inc) ] in
  check "denied-undo rig: first Increment lands" (status rd1 = 303);
  let bd1 = body (get handle_d ~cookie:cookie_d "/") in
  let rd2 = post handle_d ~cookie:cookie_d ~token:(csrf_of bd1) ~path:"/msg" [ ("msg", inc) ] in
  check "denied-undo rig: second Increment lands" (status rd2 = 303);
  let bd2 = body (get handle_d ~cookie:cookie_d "/") in
  let rd3 = post handle_d ~cookie:cookie_d ~token:(csrf_of bd2) ~path:"/undo" [] in
  check "a denied undo redirects with the denial signal, not as success"
    (status rd3 = 303 && Dream.header rd3 "Location" = Some "/?undo=denied");
  let bd3 = body (get handle_d ~cookie:cookie_d "/") in
  check "the racing commit survives the denied undo (count 3)"
    (contains ~needle:">3<" bd3);

  (* Rejection paths. *)
  let forged = post handle ~cookie ~token:"forged" ~path:"/msg" [ ("msg", inc) ] in
  check "a forged csrf token is rejected (403)" (status forged = 403);
  check "the 403 response still carries the security headers (middleware outermost)"
    (has_security_headers forged);
  let bad = post handle ~cookie ~token:(csrf_of b3) ~path:"/msg" [ ("msg", "not-a-msg") ] in
  check "an undecodable msg is rejected (400)" (status bad = 400);
  check "the undecodable msg is served as text/plain (unsniffable)"
    (Dream.header bad "Content-Type" = Some "text/plain; charset=utf-8");
  check "the undecodable-msg response also carries the security headers"
    (has_security_headers bad);
  let missing = post handle ~cookie ~token:(csrf_of b3) ~path:"/msg" [] in
  check "a missing msg field is rejected (400)" (status missing = 400);

  Printf.printf "\nThe Dream tier serves the shared app end-to-end (roadmap step 1).\n%!"

(* --- The WebSocket handshake gate, driven through the full handler --------- *)

(* Browsers do not enforce same-origin on WebSocket connections, so the
   handshake guard is load-bearing. Driving it through [Server.handler] (rather
   than the old exported bool) proves the whole Proof path: a mismatch or a
   missing Origin is a 403, and a same-origin handshake reaches [accept_ws] and
   upgrades (101). [Dream.request ~headers] carries Origin and Host exactly
   where [Origin_gate.check] reads them back. *)
let () =
  let repo = Lwt_main.run (Server.Store.create ()) in
  let handle = Dream.test (Server.handler repo) in
  let ws headers = handle (Dream.request ~method_:`GET ~target:Server.ws_path ~headers "") in
  let cross = ws [ ("Origin", "http://evil.example"); ("Host", "example.com") ] in
  check "a cross-origin WS handshake is rejected (403)" (status cross = 403);
  check "the cross-origin WS rejection still carries the security headers"
    (has_security_headers cross);
  check "a WS handshake with no Origin is rejected (403)"
    (status (ws [ ("Host", "example.com") ]) = 403);
  check "a same-origin http WS handshake upgrades through the Proof path (101)"
    (status (ws [ ("Origin", "http://example.com"); ("Host", "example.com") ]) = 101);
  check "a same-origin https WS handshake upgrades through the Proof path (101)"
    (status (ws [ ("Origin", "https://example.com"); ("Host", "example.com") ]) = 101)

(* --- live_session over a mock transport (roadmap step 3, no socket) ------- *)

let await = Test_util.await

(* [live_session] must return on peer close; bound the wait so a regression
   hangs the check, not the suite. *)
let closes_within session =
  let open Lwt.Syntax in
  Lwt.pick
    [ (let* () = session in
       Lwt.return true)
    ; (let* () = Lwt_unix.sleep 2.0 in
       Lwt.return false)
    ]

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let* repo = Server.Store.create () in
     let* s = Server.Store.main_session repo in
     let incoming, push = Lwt_stream.create () in
     let sent = ref [] in
     let transport =
       { Server.send_frame =
           (fun f ->
             sent := f :: !sent;
             Lwt.return_unit)
       ; receive_frame = (fun () -> Lwt_stream.get incoming)
       }
     in
     let session = Server.live_session s transport in
     let frames () = List.rev !sent in
     (* Down-frames are the {!Tea_core.Wire.down} sum since D14, so every
        assertion below says which {i kind} of frame it wants: a [Head] check
        that a [Hello] would satisfy is a check that cannot see the
        announcement go missing. *)
     let decoded f : Counter_app.App.model Tea_core.Wire.down option =
       Codec.down_of_json f
       |> Result.fold
            ~error:(fun (_ : Tea_core.Codec.err) -> None)
            ~ok:(fun d -> Some d)
     in
     let head_value f : int option =
       Option.bind (decoded f) (fun d ->
           match d with
           | Tea_core.Wire.Head m -> Some (Counter_app.App.value m)
           | Tea_core.Wire.Hello
               ((_ : Tea_core.Crdt.Replica.t), (_ : Counter_app.App.model)) ->
             None
           | Tea_core.Wire.Ack (_ : Tea_core.Prim.Msg_seq.t) -> None)
     in
     let announced_replica f : Tea_core.Crdt.Replica.t option =
       Option.bind (decoded f) (fun d ->
           match d with
           | Tea_core.Wire.Hello (r, (_ : Counter_app.App.model)) -> Some r
           | Tea_core.Wire.Head (_ : Counter_app.App.model) -> None
           | Tea_core.Wire.Ack (_ : Tea_core.Prim.Msg_seq.t) -> None)
     in
     (* Step 10 (D15): the acknowledgement carried by a frame, if it is one. *)
     let acked_seq f : Tea_core.Prim.Msg_seq.t option =
       Option.bind (decoded f) (fun d ->
           match d with
           | Tea_core.Wire.Ack n -> Some n
           | Tea_core.Wire.Hello
               ((_ : Tea_core.Crdt.Replica.t), (_ : Counter_app.App.model)) ->
             None
           | Tea_core.Wire.Head (_ : Counter_app.App.model) -> None)
     in
     let acks () = List.filter_map acked_seq (frames ()) in
     (* An up-frame now carries its delivery header (D15). The tab id is a
        literal of the real grammar, not a mint, so this file needs no browser
        entropy source. *)
     let tab_a = String.make 32 'a' in
     let up ?(tab = tab_a) (seq : int) (msg : Counter_app.App.msg) : string =
       Codec.up_to_json (Tea_core.Wire.Apply { tab; seq; msg })
     in
     (* (a) The session opens by announcing the replica id it applies under
        (D14) together with the current head. The expectation is built from the
        session's own context, so an announcement that named some other replica
        - a second derivation of the branch name, say - fails here. *)
     let* announced = await (fun () -> !sent <> []) in
     check "live_session announces this session's replica and head first"
       (announced
       && frames ()
          = [ Codec.down_to_json
                (Tea_core.Wire.Hello
                   ( Tea_core.Crdt.Ctx.replica (Server.Store.ctx_of_session s)
                   , fst Counter_app.App.init )) ]);
     (* (b) A Msg frame up is stepped, committed, and echoed down via the
        store watch as a [Head] frame - not a second announcement. *)
     push (Some (up 1 Counter_app.App.Increment));
     let* stepped =
       await (fun () -> List.exists (fun f -> head_value f = Some 1) (frames ()))
     in
     check "a Msg frame up yields the committed model as a Head frame down" stepped;
     check "the replica is announced once per session, not once per commit"
       (List.length (List.filter_map announced_replica (frames ())) = 1);
     (* (b') D15: the same frame again is de-duplicated above [A.update] — the
        model does not move, the branch gains no commit, and the duplicate is
        acknowledged anyway so the client's retry terminates. *)
     let* acked1 = await (fun () -> acks () <> []) in
     check "an accepted up-frame is acknowledged by seq"
       (acked1 && List.map Tea_core.Prim.Msg_seq.to_int (acks ()) = [ 1 ]);
     let* history_before = Server.Store.history s in
     push (Some (up 1 Counter_app.App.Increment));
     let* acked_twice =
       await (fun () -> List.length (acks ()) = 2)
     in
     check "a duplicate up-frame is acknowledged" acked_twice;
     let* model_after = Server.Store.load s in
     let* history_after = Server.Store.history s in
     check "a replayed up-frame commits exactly once"
       (Counter_app.App.value model_after = 1
       && List.length history_after = List.length history_before);
     (* (b'') A second tab on the SAME session shares the replica, so its own
        seq 1 must not be mistaken for the first tab's replay. *)
     push (Some (up ~tab:(String.make 32 'b') 1 Counter_app.App.Increment));
     let* second_tab =
       await (fun () -> List.exists (fun f -> head_value f = Some 2) (frames ()))
     in
     check "a second tab's own seq 1 is not read as the first tab's replay"
       second_tab;
     (* (c) Peer close ends the session. *)
     push None;
     let* closed = closes_within session in
     check "peer close (None) ends the live session" closed;
     (* (d) An undecodable frame ends the session — and commits nothing.
        On its own branch: [Irmin_mem.config ()] shares one in-process heap,
        so a fresh repo's main still sees the commit from (b). *)
     let sid2 = Option.get (Tea_core.Prim.Session_id.of_string "garbagesession") in
     let* s2 = Server.Store.session repo sid2 in
     let incoming2, push2 = Lwt_stream.create () in
     let sent2 = ref [] in
     let transport2 =
       { Server.send_frame =
           (fun f ->
             sent2 := f :: !sent2;
             Lwt.return_unit)
       ; receive_frame = (fun () -> Lwt_stream.get incoming2)
       }
     in
     push2 (Some "not json");
     let* closed2 = closes_within (Server.live_session s2 transport2) in
     check "an undecodable frame ends the live session" closed2;
     let* hist2 = Server.Store.history s2 in
     let* after2 = Server.Store.load s2 in
     check "a garbage frame commits nothing"
       (hist2 = [] && Counter_app.App.value after2 = 0);
     (* (e) A frame that is valid JSON but names a Msg constructor the app does
        not have. [Repr.of_json_string] answers that one by RAISING, so before
        {!Tea_core.Codec.of_json} was made total this crafted frame killed the
        pump with an exception instead of closing the socket - reachable by
        anyone who can open the ws. *)
     let sid3 = Option.get (Tea_core.Prim.Session_id.of_string "craftedcase") in
     let* s3 = Server.Store.session repo sid3 in
     let incoming3, push3 = Lwt_stream.create () in
     let transport3 =
       { Server.send_frame = (fun (_ : string) -> Lwt.return_unit)
       ; receive_frame = (fun () -> Lwt_stream.get incoming3)
       }
     in
     push3 (Some {|{"Bogus":1}|});
     let* closed3 = closes_within (Server.live_session s3 transport3) in
     check "a frame naming an unknown Msg case ends the session (no exception)"
       closed3;
     let* hist3 = Server.Store.history s3 in
     check "the crafted frame commits nothing" (hist3 = []);
     Printf.printf "\nThe live-view pump serves roadmap step 3, no socket needed.\n%!";
     Lwt.return_unit)

(* --- The typed RPC tier over Dream (roadmap step 7, DESIGN §8) ------------- *)

(* A raw POST: unlike the CSRF [post] above, RPC endpoints read [Dream.body]
   directly (no form token — read-only by policy), so these drive the exact
   wire the client emits: a same-origin JSON POST. *)
let rpc_request ?ct ~path payload =
  let headers = Option.fold ct ~none:[] ~some:(fun c -> [ ("Content-Type", c) ]) in
  Dream.request ~method_:`POST ~target:path ~headers payload

let () =
  let repo = Lwt_main.run (Server.Store.create ()) in
  (* The request-free rank-2 handler: [History_count] counts commits on the
     canonical branch (it cannot see the Dream session); [Doc_stats] is the
     pure transform. Distinct req/resp indices make a codec transposition a
     unification error. *)
  let rpc_handle : type a b. (a, b) Shared_doc_rpc.t -> a -> b Lwt.t =
   fun ep req ->
    match ep with
    | Shared_doc_rpc.History_count ->
      Lwt.bind (Server.Store.main_session repo) Server.Store.history |> Lwt.map List.length
    | Shared_doc_rpc.Doc_stats -> Lwt.return (Shared_doc_rpc.stats_of req)
    | Shared_doc_rpc.Append_tag ->
      (* This suite mounts the shared_doc contract onto the COUNTER app, so
         there is no doc to tag here. The arm exists ONLY to satisfy the GADT:
         it is unreachable in this suite, because the sweep below sends a
         same-origin POST that decodes as no [string], so the 400 lands before
         any handler runs. The real store-mutating handler and its origin gate
         are exercised against the shared_doc server in [csrf_test]. *)
      Lwt.return (String.length req)
  in
  let driver =
    Dream.test (Server.handler ~rpc:(Rpc_ep.routes { Rpc_ep.handle = rpc_handle }) repo)
  in
  let rpc_post ?ct ~path payload = driver (rpc_request ?ct ~path payload) in
  let json = "application/json" in
  let history_body = R.encode_req Shared_doc_rpc.History_count () in
  let history_count_of r =
    R.decode_resp Shared_doc_rpc.History_count (body r)
    |> Result.fold ~error:(fun (_ : Tea_core.Codec.err) -> -1) ~ok:Fun.id
  in
  (* (1) history_count happy path: 200 + an int body. *)
  let r_base = rpc_post ~ct:json ~path:"/rpc/history_count" history_body in
  check "POST /rpc/history_count (application/json) responds 200" (status r_base = 200);
  let baseline = history_count_of r_base in
  check "the /rpc/history_count body decodes via resp_t as an int" (baseline >= 0);
  check "the /rpc/history_count 200 is application/json; charset=utf-8"
    (Dream.header r_base "Content-Type" = Some "application/json; charset=utf-8");
  check "the /rpc/history_count 200 carries the security headers"
    (has_security_headers r_base);
  (* (2) store-backed proof: one direct commit to the canonical branch bumps
     the count by exactly 1 (deviation #2 — no session-cookie coupling). *)
  let () =
    Lwt_main.run
      (let open Lwt.Syntax in
       let* s = Server.Store.main_session repo in
       let* (_ : Counter_app.App.model) = Server.Store.apply s Counter_app.App.Increment in
       Lwt.return_unit)
  in
  let after = history_count_of (rpc_post ~ct:json ~path:"/rpc/history_count" history_body) in
  check "history_count increments by exactly 1 after one direct commit to main"
    (after = baseline + 1);
  (* (3) doc_stats happy path: unequal canned lengths so field-confusion moves
     the answer. *)
  let stats_body = R.encode_req Shared_doc_rpc.Doc_stats { Shared_doc_rpc.title = "hello"; body = "a b c d" } in
  let r_stats = rpc_post ~ct:json ~path:"/rpc/doc_stats" stats_body in
  check "POST /rpc/doc_stats responds 200" (status r_stats = 200);
  check "doc_stats returns exactly {title_len=5; word_count=4}"
    (R.decode_resp Shared_doc_rpc.Doc_stats (body r_stats)
     |> Result.fold ~error:(fun (_ : Tea_core.Codec.err) -> false)
          ~ok:(fun (s : Shared_doc_rpc.stats_resp) ->
            s.title_len = 5 && s.word_count = 4));
  (* (4) routing: unknown endpoint and wrong method both 404. *)
  check "POST /rpc/nope is a 404 (router fallback)"
    (status (rpc_post ~ct:json ~path:"/rpc/nope" "null") = 404);
  check "GET on an rpc path is a 404 (POST-only routes)"
    (status (driver (Dream.request ~method_:`GET ~target:"/rpc/doc_stats" "")) = 404);
  (* (5) malformed body: 400 with the pinned prefix and unsniffable text/plain. *)
  let r_bad = rpc_post ~ct:json ~path:"/rpc/doc_stats" "{" in
  check "a malformed rpc body responds 400" (status r_bad = 400);
  check "the 400 body carries the \"undecodable rpc request: \" prefix"
    (contains ~needle:"undecodable rpc request: " (body r_bad));
  check "the 400 refusal is text/plain; charset=utf-8"
    (Dream.header r_bad "Content-Type" = Some "text/plain; charset=utf-8");
  check "the 400 refusal carries the security headers" (has_security_headers r_bad);
  (* (6) content-type gate: strip params, case-fold; reject the rest. *)
  check "urlencoded Content-Type is refused 415"
    (status
       (rpc_post ~ct:"application/x-www-form-urlencoded" ~path:"/rpc/doc_stats" stats_body)
     = 415);
  let r_no_ct = rpc_post ~path:"/rpc/doc_stats" stats_body in
  check "an absent Content-Type is refused 415" (status r_no_ct = 415);
  check "the 415 refusal carries the security headers" (has_security_headers r_no_ct);
  check "application/json; charset=utf-8 passes the CT gate (param-strip)"
    (status (rpc_post ~ct:"application/json; charset=utf-8" ~path:"/rpc/doc_stats" stats_body)
    = 200);
  check "APPLICATION/JSON passes the CT gate (case-fold)"
    (status (rpc_post ~ct:"APPLICATION/JSON" ~path:"/rpc/doc_stats" stats_body) = 200);
  (* (7) body cap: refused before any decode. *)
  let over_cap = String.make (Rpc_ep.max_body_bytes + 1) 'x' in
  check "a body of max_body_bytes+1 is refused 413"
    (status (rpc_post ~ct:json ~path:"/rpc/doc_stats" over_cap) = 413);
  (* (8) reachability sweep: every derived path is routed (non-404). Sent
     same-origin, so a [Mutating] endpoint clears its origin gate and the check
     is about ROUTING - otherwise a 403 would satisfy "non-404" while proving
     only that the gate ran. *)
  let same_origin_headers =
    [ ("Content-Type", json); ("Origin", "http://example.com"); ("Host", "example.com") ]
  in
  check "every path derived from Shared_doc_rpc.all is reachable (non-404)"
    (List.for_all
       (fun (Shared_doc_rpc.Any e) ->
         let p = Tea_core.Prim.Rpc_path.to_string (R.path_of e) in
         let r =
           driver (Dream.request ~method_:`POST ~target:p ~headers:same_origin_headers "null")
         in
         status r <> 404 && status r <> 403)
       Shared_doc_rpc.all);
  Printf.printf "\nThe typed RPC tier serves both endpoints over Dream (roadmap step 7).\n%!"
