(** The tea_safe security boundaries (roadmap step 5, risk R7). Plain-[Printf]
    checks in the repo's house style (no qcheck/alcotest): each constructor's
    accept and reject corpus, plus two pins the wiring depends on, the exact
    strict-CSP serialization and the byte-identical [to_steps] of the store's
    model path. The Prim hardening corpora (Url, Attr_name, Tag, Branch_name),
    whose sinks live in tea_core, ride in the same executable. *)

open Tea_safe
module Prim = Tea_core.Prim

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    incr failures)

(* [ok r] is [true] when [r] is [Ok _], without matching a two-arm result. *)
let ok r = Result.fold ~ok:(fun _ -> true) ~error:(fun _ -> false) r

(* Substring test without pulling in [Str]. *)
let contains_sub s sub =
  let ls = String.length s and lsub = String.length sub in
  let rec go i = i + lsub <= ls && (String.equal (String.sub s i lsub) sub || go (i + 1)) in
  lsub = 0 || go 0

let () =
  (* --- Safe_key.Step: no namespace escape ------------------------------- *)
  check "step accepts a plain name" (ok (Safe_key.Step.of_string "model"));
  check "step accepts a hyphenated name" (ok (Safe_key.Step.of_string "users-1"));
  check "step accepts a dotted name" (ok (Safe_key.Step.of_string "a.b"));
  check "step rejects empty" (Safe_key.Step.of_string "" = Error Safe_key.Step.Empty);
  check "step rejects the current-dir name" (Safe_key.Step.of_string "." = Error Safe_key.Step.Dot);
  check "step rejects the parent name" (Safe_key.Step.of_string ".." = Error Safe_key.Step.Dot_dot);
  check "step rejects the path separator"
    (Safe_key.Step.of_string "a/b" = Error Safe_key.Step.Separator);
  check "step rejects a NUL"
    (Safe_key.Step.of_string "a\000b" = Error (Safe_key.Step.Control_char '\000'));
  check "step rejects a newline"
    (Safe_key.Step.of_string "a\nb" = Error (Safe_key.Step.Control_char '\n'));

  (* Byte-compat pin: the store's model path must stay exactly ["model"], and
     composition must preserve order. persist_test alone cannot catch a drifted
     constant (load and commit share it, so a changed constant is
     self-consistent); this pin is the real guard. *)
  check "safe_key model path is exactly [\"model\"]"
    (Safe_key.(to_steps (root (Step.v "model"))) = [ "model" ]);
  check "safe_key composes steps in order"
    (Safe_key.(to_steps (root (Step.v "model") / Step.v "field")) = [ "model"; "field" ]);

  (* --- Safe_path: cannot climb ------------------------------------------ *)
  check "safe_path rejects a parent segment"
    (Safe_path.of_string "../x" = Error Safe_path.Dot_dot_segment);
  check "safe_path rejects an embedded parent"
    (Safe_path.of_string "a/../b" = Error Safe_path.Dot_dot_segment);
  check "safe_path rejects a current segment"
    (Safe_path.of_string "./x" = Error Safe_path.Dot_segment);
  check "safe_path rejects an absolute path"
    (Safe_path.of_string "/etc/passwd" = Error Safe_path.Absolute);
  check "safe_path rejects an empty segment"
    (Safe_path.of_string "a//b" = Error Safe_path.Empty_segment);
  check "safe_path rejects a backslash"
    (Safe_path.of_string "a\\b" = Error Safe_path.Backslash);
  check "safe_path rejects a control byte"
    (Safe_path.of_string "a\000x" = Error (Safe_path.Control_char '\000'));
  check "safe_path rejects empty" (Safe_path.of_string "" = Error Safe_path.Empty);
  check "safe_path accepts a nested relative path" (ok (Safe_path.of_string "client/dist"));
  check "safe_path accepts a file path" (ok (Safe_path.of_string "a/b.txt"));
  let confined p =
    Result.fold
      ~ok:(fun t ->
        let s = Safe_path.to_string t in
        (not (String.length s > 0 && Char.equal s.[0] '/'))
        && not (List.mem ".." (Safe_path.segments t)))
      ~error:(fun _ -> false)
      (Safe_path.of_string p)
  in
  check "safe_path accepted paths never start with / nor carry a .. segment"
    (confined "client/dist" && confined "a/b.txt");

  (* --- Safe_cmd: exec argv, never a shell line -------------------------- *)
  check "program rejects empty" (Safe_cmd.Program.of_string "" = Error Safe_cmd.Program.Empty);
  check "program rejects a NUL" (Safe_cmd.Program.of_string "a\000" = Error Safe_cmd.Program.Nul);
  check "program accepts a name" (ok (Safe_cmd.Program.of_string "convert"));
  check "arg admits shell metacharacters as inert data"
    (ok (Safe_cmd.Arg.of_string "; rm -rf /")
    && ok (Safe_cmd.Arg.of_string "$(whoami)")
    && ok (Safe_cmd.Arg.of_string "`id`"));
  check "arg rejects a NUL" (Safe_cmd.Arg.of_string "a\000" = Error Safe_cmd.Arg.Nul);
  let prog = Safe_cmd.Program.v "convert" in
  let argv = List.filter_map (fun s -> Result.to_option (Safe_cmd.Arg.of_string s)) [ "-flatten"; "in.png" ] in
  let cmd = Safe_cmd.v prog argv in
  check "safe_cmd exposes program and argv as an exec array"
    (Safe_cmd.program cmd = "convert" && Safe_cmd.args cmd = [ "-flatten"; "in.png" ]);

  (* --- Header: no response splitting ------------------------------------ *)
  check "header name rejects empty" (Header.Name.of_string "" = Error Header.Name.Empty);
  check "header name rejects a space"
    (Header.Name.of_string "a b" = Error (Header.Name.Invalid_char ' '));
  check "header name rejects a colon"
    (Header.Name.of_string "X:Y" = Error (Header.Name.Invalid_char ':'));
  check "header name accepts a real header" (ok (Header.Name.of_string "Content-Security-Policy"));
  check "header value rejects a CRLF injection"
    (Header.Value.of_string "a\r\nSet-Cookie: x" = Error (Header.Value.Control_char '\r'));
  check "header value rejects a lone LF"
    (Header.Value.of_string "\n" = Error (Header.Value.Control_char '\n'));
  check "header value rejects a NUL"
    (Header.Value.of_string "\000" = Error (Header.Value.Control_char '\000'));
  check "header value rejects another control byte"
    (Header.Value.of_string "\001" = Error (Header.Value.Control_char '\001'));
  check "header value accepts a content type" (ok (Header.Value.of_string "text/plain; charset=utf-8"));
  check "header value accepts an HTAB" (ok (Header.Value.of_string "a\tb"));

  (* --- Security_headers: finite-sum policy, strict pin, no drift -------- *)
  let expected_csp =
    "default-src 'self'; script-src 'self'; style-src 'self'; img-src 'self'; connect-src 'self'; \
     form-action 'self'; object-src 'none'; base-uri 'none'; frame-ancestors 'none'"
  in
  let strict_hs = Security_headers.to_headers Security_headers.strict in
  let nth_name i = List.nth_opt strict_hs i |> Option.map Header.name in
  let nth_value i = List.nth_opt strict_hs i |> Option.map Header.value in
  check "strict emits exactly the three headers in order"
    (List.map Header.name strict_hs
    = [ "Content-Security-Policy"; "X-Frame-Options"; "X-Content-Type-Options" ]);
  check "strict CSP is the pinned policy" (nth_value 0 = Some expected_csp);
  check "strict frames are denied" (nth_name 1 = Some "X-Frame-Options" && nth_value 1 = Some "DENY");
  check "strict sends nosniff"
    (nth_name 2 = Some "X-Content-Type-Options" && nth_value 2 = Some "nosniff");
  let frames = [ Security_headers.Frame.Deny; Security_headers.Frame.Same_origin ] in
  let sources = [ Security_headers.Source.None_; Security_headers.Source.Self ] in
  let all_values_ctl_free =
    List.for_all
      (fun frame ->
        List.for_all
          (fun src ->
            let p =
              Security_headers.v ~frame ~default_src:src ~script_src:src ~style_src:src ~img_src:src
                ~connect_src:src ~form_action:src
            in
            List.for_all
              (fun h -> ok (Header.Value.of_string (Header.value h)))
              (Security_headers.to_headers p))
          sources)
      frames
  in
  check "every (frame, source) policy emits only control-byte-free header values"
    all_values_ctl_free;
  let same_origin_pol =
    Security_headers.(
      v ~frame:Frame.Same_origin ~default_src:Source.Self ~script_src:Source.Self
        ~style_src:Source.Self ~img_src:Source.Self ~connect_src:Source.Self
        ~form_action:Source.Self)
  in
  let so_hs = Security_headers.to_headers same_origin_pol in
  let so_csp = Option.value ~default:"" (List.nth_opt so_hs 0 |> Option.map Header.value) in
  let so_xfo = Option.value ~default:"" (List.nth_opt so_hs 1 |> Option.map Header.value) in
  check "Same_origin keeps frame-ancestors 'self' and X-Frame-Options SAMEORIGIN in lockstep"
    (contains_sub so_csp "frame-ancestors 'self'" && String.equal so_xfo "SAMEORIGIN");

  (* --- Redirect: anchored allowlist ------------------------------------- *)
  let allow = Redirect.Allowlist.v [ "https://docs.example.com" ] in
  let ext s = Redirect.external_of_string allow s in
  check "redirect accepts the exact origin" (ok (ext "https://docs.example.com"));
  check "redirect accepts a path under the origin" (ok (ext "https://docs.example.com/page?x=1"));
  check "redirect rejects the suffix trick"
    (ext "https://docs.example.com.evil.com/" = Error Redirect.Not_allowlisted);
  check "redirect rejects the userinfo trick"
    (ext "https://docs.example.com@evil.com" = Error Redirect.Not_allowlisted);
  check "redirect rejects an off-site host" (ext "https://evil.com/" = Error Redirect.Not_allowlisted);
  check "redirect rejects a protocol-relative target"
    (ext "//docs.example.com" = Error Redirect.Not_allowlisted);
  check "redirect rejects a CRLF-bearing target"
    (ext "https://docs.example.com/a\r\nb" = Error (Redirect.Control_char '\r'));
  check "redirect rejects empty" (ext "" = Error Redirect.Empty);

  (* --- Proof / Origin_gate: the same-origin capability ------------------ *)
  let og = Origin_gate.check in
  check "origin_gate mints on an http same origin" (ok (og ~origin:(Some "http://h") ~host:(Some "h")));
  check "origin_gate mints on an https same origin"
    (ok (og ~origin:(Some "https://h") ~host:(Some "h")));
  check "origin_gate rejects a cross origin"
    (og ~origin:(Some "http://evil") ~host:(Some "h") = Error Origin_gate.Origin_mismatch);
  check "origin_gate rejects a missing origin"
    (og ~origin:None ~host:(Some "h") = Error Origin_gate.Origin_missing);
  check "origin_gate rejects a missing host"
    (og ~origin:(Some "http://h") ~host:None = Error Origin_gate.Host_missing);
  check "origin_gate rejects both missing"
    (og ~origin:None ~host:None = Error Origin_gate.Both_missing);
  (* The generic capability functor: a sink demanding [Gate.cap Proof.t] is
     reachable only through [Gate.grant]; that the call is otherwise unwritable
     is the type-level demonstration, and R7 non-serializability is the absence
     of any function that could place the token in a [Repr.t] (nothing to run). *)
  let module Gate = Proof.Mint () in
  let requires_cap (_ : Gate.cap Proof.t) = true in
  check "a capability sink is reachable only via its mint" (requires_cap (Gate.grant ()));

  (* --- Prim hardening: XSS and open-redirect sinks live in tea_core -------- *)
  (* Url: relative-only, plus a backslash and control-byte scan (the '\' feeds
     browser '/' normalisation, control bytes split the Location header). *)
  check "url rejects a CRLF (response splitting)"
    (Prim.Url.of_string "/x\r\ny" = Error (Prim.Url.Control_char '\r'));
  check "url rejects a lone LF" (Prim.Url.of_string "/x\n" = Error (Prim.Url.Control_char '\n'));
  check "url rejects a NUL" (Prim.Url.of_string "/a\000" = Error (Prim.Url.Control_char '\000'));
  check "url rejects a backslash escape" (Prim.Url.of_string "/\\evil.com" = Error Prim.Url.Backslash);
  check "url rejects empty" (Prim.Url.of_string "" = Error Prim.Url.Empty);
  check "url rejects a protocol-relative target"
    (Prim.Url.of_string "//evil" = Error Prim.Url.Not_relative);
  check "url rejects an absolute target"
    (Prim.Url.of_string "http://evil" = Error Prim.Url.Not_relative);
  check "url accepts a relative path with a query" (ok (Prim.Url.of_string "/ok?q=1"));

  (* Attr_name: on* filter, then an [A-Za-z0-9_.:-] allowlist (names render
     UNescaped, so a space smuggles a live handler). *)
  check "attr_name rejects a smuggled handler (space)"
    (Prim.Attr_name.of_string "x onmouseover=alert(1) y" = Error (Prim.Attr_name.Invalid_char ' '));
  check "attr_name rejects an equals" (Prim.Attr_name.of_string "a=b" = Error (Prim.Attr_name.Invalid_char '='));
  check "attr_name rejects a quote" (Prim.Attr_name.of_string "a\"b" = Error (Prim.Attr_name.Invalid_char '"'));
  check "attr_name rejects an angle bracket"
    (Prim.Attr_name.of_string "a>b" = Error (Prim.Attr_name.Invalid_char '>'));
  check "attr_name rejects an on* handler"
    (Prim.Attr_name.of_string "onclick" = Error Prim.Attr_name.Event_handler);
  check "attr_name accepts the real-world charset"
    (ok (Prim.Attr_name.of_string "class")
    && ok (Prim.Attr_name.of_string "data-x")
    && ok (Prim.Attr_name.of_string "aria-label")
    && ok (Prim.Attr_name.of_string "xlink:href")
    && ok (Prim.Attr_name.of_string "viewBox"));

  (* Tag: [a-z][a-z0-9-]* allowlist (tag position has no escaping at render). *)
  check "tag rejects an injected close bracket"
    (Prim.Tag.of_string "script>alert(1)" = Error (Prim.Tag.Invalid_char '>'));
  check "tag rejects a space" (Prim.Tag.of_string "a b" = Error (Prim.Tag.Invalid_char ' '));
  check "tag rejects an uppercase lead" (Prim.Tag.of_string "SCRIPT" = Error (Prim.Tag.Invalid_char 'S'));
  check "tag rejects empty" (Prim.Tag.of_string "" = Error Prim.Tag.Empty);
  check "tag accepts an element and a custom element"
    (ok (Prim.Tag.of_string "div") && ok (Prim.Tag.of_string "my-widget"));

  (* Branch_name: [A-Za-z0-9._-]+ with no leading '-' or '.'; "main" and
     "session-<hex>" (the only session-derived mint) must still pass. *)
  check "branch_name rejects a path separator" (Prim.Branch_name.of_string "a/b" = None);
  check "branch_name rejects a space" (Prim.Branch_name.of_string "a b" = None);
  check "branch_name rejects a newline" (Prim.Branch_name.of_string "a\nb" = None);
  check "branch_name rejects a leading hyphen" (Prim.Branch_name.of_string "-x" = None);
  check "branch_name rejects a leading dot" (Prim.Branch_name.of_string ".x" = None);
  check "branch_name accepts main, a session branch, and a hyphenated name"
    (Option.is_some (Prim.Branch_name.of_string "main")
    && Option.is_some (Prim.Branch_name.of_string "session-0abc")
    && Option.is_some (Prim.Branch_name.of_string "docs-team"));

  if !failures = 0 then Printf.printf "\nAll tea_safe boundary checks hold (R7 security primitives).\n%!"
  else (
    Printf.printf "\n%d tea_safe check(s) FAILED.\n%!" !failures;
    exit 1)
