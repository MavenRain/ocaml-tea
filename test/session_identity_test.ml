(** Durable session identity across process lives (roadmap step 12, D17).

    The unit under test is not a function, it is a {i restart}. A "life" here is
    a freshly built [Dream.test (Server.handler ~sessions repo)] application over
    the {b same} repo: fresh middleware closures, fresh session tables, exactly
    what a restarted binary gets, minus the [execve]. The store deliberately
    survives, because the D17 defect is precisely that the store survived and the
    {i identity} did not: [Dream.memory_sessions] mints a new session id per
    process, that id names the Irmin branch {b and} is the CRDT replica id, so a
    reconnecting tab landed on a new branch under a new replica and the D16
    journal it was meant to consult was addressed by a floor nobody would ask
    for again.

    {b What this file can and cannot prove.} N1/N2/N3 alone do {b not} establish
    that the {i configured} secret is what carries identity: with no
    [Dream.set_secret] Dream falls back to a lazily minted process-global key, so
    two [Dream.test] stacks in one process would share identity anyway and N1
    would pass over a back end that ignored its argument entirely. The converses
    are the load-bearing half: N4/N5 (memory loses it) and N6/N7 (a {i different}
    secret loses it) fail unless the configured value is actually consulted.
    Identity across a real [execve] is B4+B5's exclusive job in the browser
    harness, which this file cannot and does not attempt.

    Shape follows [server_test.ml], not [pack_serve_test.ml]: every [Dream.test]
    handle is built and invoked at top level, and [Lwt_main.run] wraps only store
    calls, because [Dream.test] runs [Lwt_main.run] internally and this switch's
    [Lwt_main.run] raises on nesting. *)

module Server = Tea_server.Make (Counter_app.App)
module Codec = Tea_core.Codec.Make (Counter_app.App)
module S = Tea_server.Session_secret

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

let contains ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec at i = i + n <= h && (String.sub haystack i n = needle || at (i + 1)) in
  at 0

let extract_after ~marker body =
  let ml = String.length marker in
  let rec scan i =
    if i + ml > String.length body then None
    else if String.sub body i ml = marker then Some (i + ml)
    else scan (i + 1)
  in
  scan 0
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

let reissued response = Option.is_some (Dream.header response "Set-Cookie")
let body response = Lwt_main.run (Dream.body response)
let status response = Dream.status_to_int (Dream.status response)
let visit handle path = handle (Dream.request ~method_:`GET ~target:path "")

let get handle ~cookie path =
  handle (Dream.request ~method_:`GET ~target:path ~headers:[ ("Cookie", cookie) ] "")

let post handle ~cookie ~token ~path fields =
  handle
    (Dream.request ~method_:`POST ~target:path
       ~headers:
         [ ("Cookie", cookie); ("Content-Type", "application/x-www-form-urlencoded") ]
       (Dream.to_form_urlencoded (("dream.csrf", token) :: fields)))

(* --- The replica probe ------------------------------------------------- *)

(* [tea_server.ml] has no .mli, so [Handlers] is unsealed and a test can mount a
   route that answers the one question the HTTP surface never exposes: which
   replica id is this request applying under. That id is derived from the Dream
   session id, so it is the sharpest available witness that identity did or did
   not survive a life boundary: the SSR count can agree by accident (two
   sessions can both hold 0), a replica id cannot. *)
let probe_path = "/__probe/replica"

let probe_route (repo : Server.Store.t) : Dream.route =
  Dream.get probe_path (fun (request : Dream.request) ->
      Server.with_session repo request (fun (s : Server.Store.session) ->
          Dream.respond
            (Repr.to_string Tea_core.Crdt.Replica.t
               (Tea_core.Crdt.Ctx.replica (Server.Store.ctx_of_session s)))))

(* An observation carries its status, so a probe that failed can never be
   compared as if it had answered: [with_session]'s error path is a 500 whose
   body is prose, and two lives that both failed would otherwise report equal
   strings and pass N3 and N7 vacuously. *)
let probe handle ~cookie =
  let r = get handle ~cookie probe_path in
  Printf.sprintf "%d:%s" (status r) (body r)

(* Total, and deliberately not a match on the repr format: a real answer is a
   200 carrying one opaque token. The 500 body ("session id unavailable") is
   prose, so "holds no space" separates them without pinning how
   [Crdt.Replica.t] happens to render today. *)
let is_replica_id (o : string) : bool =
  String.starts_with ~prefix:"200:" o
  && String.length o > 20
  && not (contains ~needle:" " o)

(* --- Lives -------------------------------------------------------------- *)

(* A life: fresh middleware closures over an unchanged repo. Every life mounts
   the probe, so the two arms of a comparison differ only in [sessions]. *)
let life (sessions : S.t) (repo : Server.Store.t) : Dream.request -> Dream.response =
  Dream.test (Server.handler ~sessions ~rpc:[ probe_route repo ] repo)

let inc = Codec.msg_to_json Counter_app.App.Increment

(* Two secrets, minted rather than written out: nothing here depends on their
   bytes, only on same-versus-different. [Secret.t] is sealed with no
   elimination form, so [generate] is the only total way for a test to obtain
   one. *)
let secret_a = S.Secret.generate ()
let secret_b = S.Secret.generate ()

(* Two DISTINCT back-end values over the SAME secret. Passing one [S.t] to both
   lives would leave open the reading that identity travelled through a shared
   closure rather than through the secret. *)
let durable_a = S.durable secret_a
let durable_a' = S.durable secret_a
let durable_b = S.durable secret_b

(* Drive a life from a cold start up to count 2, and report the cookie, the
   rendered body, and the replica id it settled on. Each POST re-reads the CSRF
   token from the page it was rendered on, so a token lifetime rule can never
   masquerade as a session failure. *)
let raise_to_two handle =
  let r0 = visit handle "/" in
  let cookie = cookie_of r0 in
  let r1 = post handle ~cookie ~token:(csrf_of (body r0)) ~path:"/msg" [ ("msg", inc) ] in
  let b1 = body (get handle ~cookie "/") in
  let r2 = post handle ~cookie ~token:(csrf_of b1) ~path:"/msg" [ ("msg", inc) ] in
  (cookie, status r0, status r1, status r2, body (get handle ~cookie "/"))

let () =
  (* --- N1/N2/N3: the same secret carries identity across a life boundary --- *)
  let repo = Lwt_main.run (Server.Store.create ()) in
  let l1 = life durable_a repo in
  let cookie, s0, s1, s2, b_end = raise_to_two l1 in
  check "setup: life 1 establishes a session and takes two increments"
    (cookie <> "" && s0 = 200 && s1 = 303 && s2 = 303);
  check "setup: life 1 renders count 2" (contains ~needle:">2<" b_end);
  let replica_1 = probe l1 ~cookie in
  check "N3 the probe reports a real replica id, not a failure body"
    (is_replica_id replica_1);

  (* The restart. Same repo, same secret bytes, everything else rebuilt. *)
  let l2 = life durable_a' repo in
  let r_life2 = get l2 ~cookie "/" in
  check "N1 life 2 with the same secret keeps the session (count 2 survives)"
    (status r_life2 = 200 && contains ~needle:">2<" (body r_life2));
  check "N2 life 2 with the same secret ADOPTS the cookie without reissuing it"
    (not (reissued r_life2));
  check "N3 life 2 with the same secret reports the SAME replica id"
    (probe l2 ~cookie = replica_1);

  (* --- N8: within one life, a replayed cookie is one session ------------- *)
  let again_1 = probe l2 ~cookie in
  let again_2 = probe l2 ~cookie in
  check "N8 two requests replaying the cookie in one life share the session id"
    (is_replica_id again_1 && again_1 = again_2);

  (* --- N10: the CSRF form path still works under cookie sessions --------- *)
  let b_l2 = body (get l2 ~cookie "/") in
  let r_csrf = post l2 ~cookie ~token:(csrf_of b_l2) ~path:"/msg" [ ("msg", inc) ] in
  check "N10 a cookie-session life accepts its own CSRF token (303, count 3)"
    (status r_csrf = 303 && contains ~needle:">3<" (body (get l2 ~cookie "/")));

  (* --- N9: a cookie this life cannot read is a fresh session, not a raise -- *)
  (* Mangle the FIRST character of the value, never the last. The final
     base64url character of a value whose length is not a multiple of 4 carries
     "don't care" low bits, so flipping it can decode to the very same
     ciphertext and the cookie still opens: an earlier draft of this check
     passed or failed depending on the run's random secret for exactly that
     reason. The first character always carries significant bits. *)
  let value_at = 1 + Option.value ~default:(-1) (String.index_opt cookie '=') in
  let char_at (s : string) (i : int) : char =
    if i >= 0 && i < String.length s then String.get s i else 'A'
  in
  let mangle (replacement : char) =
    String.mapi (fun (j : int) (c : char) -> if j = value_at then replacement else c) cookie
  in
  (* AES-GCM authenticates, so a bit flip is a tag failure. *)
  let flipped = mangle (if char_at cookie value_at = 'A' then 'B' else 'A') in
  let r_flipped = get l2 ~cookie:flipped "/" in
  check "N9 a tampered ciphertext degrades to a fresh session with a 200"
    (flipped <> cookie && status r_flipped = 200 && contains ~needle:">0<" (body r_flipped));
  (* '*' is outside base64url: the value stops being decodable at all, which is
     a different failure path from a tag mismatch and must equally not raise. *)
  let r_illegal = get l2 ~cookie:(mangle '*') "/" in
  check "N9 a cookie that is not even base64url degrades to a fresh session with a 200"
    (status r_illegal = 200 && contains ~needle:">0<" (body r_illegal));
  (* A memory-era cookie: the exact value shape a step-11 process handed out,
     replayed at a step-12 server. This is the upgrade path, not a hypothetical. *)
  let memory_era =
    String.index_opt cookie '='
    |> Option.fold ~none:cookie ~some:(fun (i : int) ->
           String.sub cookie 0 (i + 1) ^ "0" ^ String.make 24 'A')
  in
  let r_era = get l2 ~cookie:memory_era "/" in
  check "N9 a 25-char memory-era cookie degrades to a fresh session with a 200"
    (status r_era = 200 && contains ~needle:">0<" (body r_era));

  (* --- N4/N5: the memory converse --------------------------------------- *)
  let repo_m = Lwt_main.run (Server.Store.create ()) in
  let m1 = life S.memory repo_m in
  let cookie_m, (_ : int), (_ : int), (_ : int), b_m = raise_to_two m1 in
  check "setup: the memory life also reaches count 2" (contains ~needle:">2<" b_m);
  let m2 = life S.memory repo_m in
  let r_m2 = get m2 ~cookie:cookie_m "/" in
  check "N4 memory sessions LOSE the session across lives (count resets to 0)"
    (status r_m2 = 200 && contains ~needle:">0<" (body r_m2));
  check "N5 memory sessions reissue a Set-Cookie in life 2" (reissued r_m2);

  (* --- N6/N7: the different-secret converse ------------------------------ *)
  let repo_d = Lwt_main.run (Server.Store.create ()) in
  let d1 = life durable_a repo_d in
  let cookie_d, (_ : int), (_ : int), (_ : int), b_d = raise_to_two d1 in
  check "setup: the secret-A life also reaches count 2" (contains ~needle:">2<" b_d);
  let replica_d = probe d1 ~cookie:cookie_d in
  let d2 = life durable_b repo_d in
  let r_d2 = get d2 ~cookie:cookie_d "/" in
  check "N6 a DIFFERENT secret loses the session across lives (count resets to 0)"
    (status r_d2 = 200 && contains ~needle:">0<" (body r_d2));
  let replica_d2 = probe d2 ~cookie:cookie_d in
  check "N7 a DIFFERENT secret mints a DIFFERENT replica id in life 2"
    (is_replica_id replica_d && is_replica_id replica_d2 && replica_d <> replica_d2);

  (* --- N11/N12: the rotation window is INERT, and the defect is upstream -- *)
  (* [?previous] is Dream's [~old_secrets], documented there as "tried for
     decryption and verification". It is not, for anything encrypted with
     associated data, which is every Dream cookie: [Cipher.decrypt] walks the
     secret list recursively and the recursive call drops its own
     [?associated_data] (cipher/cipher.ml:46), so the primary secret is tried
     with the AAD and every old secret is tried without it, failing the GCM tag
     whatever the key.

     These checks therefore pin the behaviour we have rather than the behaviour
     we want. That is deliberate. A rotation window that silently logs everyone
     out is the kind of defect an operator discovers by retiring the old secret
     and losing every session, so it is worth a red check that names it; and
     when dream ships the one-line fix, N11 and N12 both go red together and
     send whoever upgraded to [Session_secret.durable]'s doc comment. *)
  let d3 = life (S.durable ~previous:[ secret_a ] secret_b) repo_d in
  let r_d3 = get d3 ~cookie:cookie_d "/" in
  check
    "N11 UPSTREAM DEFECT (dream 1.0.0~alpha8): a rotation window does NOT open a \
     previous-secret cookie"
    (status r_d3 = 200 && contains ~needle:">0<" (body r_d3));
  check "N11 a rotation window is still plumbed through to previous_count"
    (S.previous_count (S.durable ~previous:[ secret_a ] secret_b) = 1
    && S.previous_count durable_b = 0);
  check "N11 describe says INERT out loud whenever a window is configured"
    (contains ~needle:"INERT" (S.describe (S.durable ~previous:[ secret_a ] secret_b))
    && not (contains ~needle:"INERT" (S.describe durable_b)));

  (* Raw Dream, with tea_server out of the path entirely, so N11 cannot be read
     as a defect in this module's composition order. Same three outcomes. *)
  let sa = Dream.to_base64url (Dream.random 32) in
  let sb = Dream.to_base64url (Dream.random 32) in
  let raw_life (s : string) (old : string list option) =
    Dream.test
      (Dream.set_secret ?old_secrets:old s
         (Dream.cookie_sessions (fun (request : Dream.request) ->
              Dream.respond (Dream.session_id request))))
  in
  let r_raw = visit (raw_life sa None) "/" in
  let raw_cookie = cookie_of r_raw in
  let raw_id = body r_raw in
  let id_under (s : string) (old : string list option) =
    body (get (raw_life s old) ~cookie:raw_cookie "/")
  in
  check "N12 raw Dream: the SAME secret opens the cookie in another life"
    (raw_id <> "" && id_under sa None = raw_id);
  check "N12 raw Dream reproduces the defect: ~old_secrets does NOT open it"
    (id_under sb (Some [ sa ]) <> raw_id);
  check "N12 raw Dream: a different secret with no window does not open it either"
    (id_under sb None <> raw_id);

  (* --- The back end reports itself honestly ------------------------------ *)
  check "durable is_durable, memory is not"
    (S.is_durable durable_a && not (S.is_durable S.memory));
  check "the same secret fingerprints alike, a different one does not"
    (S.fingerprint durable_a = S.fingerprint durable_a'
    && S.fingerprint durable_a <> S.fingerprint durable_b
    && S.fingerprint S.memory = None);
  Printf.printf "session identity: all checks passed\n%!"
