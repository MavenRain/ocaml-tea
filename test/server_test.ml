(** End-to-end proof of the Dream tier (roadmap step 1): drive the Counter
    over HTTP with [Dream.test] — no socket. Checks SSR, the On_click →
    form-post rewrite, per-session branch isolation, CSRF enforcement, undo,
    and the two client-error paths. Roadmap step 3 is proven the same way:
    [live_session] is driven over an in-memory [live_transport] — no
    WebSocket handshake — and [same_origin] over constructed requests. *)

module Server = Tea_server.Make (Counter_app.App)
module Codec = Tea_core.Codec.Make (Counter_app.App)

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
     (* (a) The session opens by announcing the current model. *)
     let* announced = await (fun () -> !sent <> []) in
     check "live_session announces the current model first"
       (announced
       && frames () = [ Codec.model_to_json { Counter_app.App.count = 0 } ]);
     (* (b) A Msg frame up is stepped, committed, and echoed down via the
        store watch as a full model frame. *)
     push (Some (Codec.msg_to_json Counter_app.App.Increment));
     let* stepped =
       await (fun () ->
           List.mem (Codec.model_to_json { Counter_app.App.count = 1 }) (frames ()))
     in
     check "a Msg frame up yields the committed model frame down" stepped;
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
       (hist2 = [] && after2.Counter_app.App.count = 0);
     Printf.printf "\nThe live-view pump serves roadmap step 3, no socket needed.\n%!";
     Lwt.return_unit)
