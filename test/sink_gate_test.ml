(** R7's residual made mechanical (roadmap step 23): the direct-sink
    discipline - never name raw [Dream]/[Irmin*]/[Unix]/[Lwt_unix] outside a
    designated sink module - stops being a review convention and becomes a
    gate the build runs. The scan walks every [.ml]/[.mli] under [lib/],
    [examples/] and [test/] (materialized by this stanza's [source_tree]
    deps), strips comments and string literals with a character-level state
    machine (byte-blanking, so [file:line] survives), then flags EVERY
    remaining occurrence of a sink module name - qualified use, [open],
    [include], [module X = Sink], functor argument: a bare capitalized
    occurrence is a violation however it is spelled - unless the (file,
    namespace) pair is in the pinned allowlist below. DEFAULT-DENY: a new
    file that names a sink and is not classified here FAILS with its
    file:line, so the allowlist is the audited surface.

    What this gate deliberately does NOT claim: the allowlisted sink modules
    themselves stay trusted (that is what their [.mli] boundaries are for);
    [Sys]/[out_channel]/[Filename] are ambient stdlib and OUT of scope (R7
    names Dream/Irmin/Unix); dune [libraries] stanzas are enforced by
    [(implicit_transitive_deps false)] at compile time, not here; and code
    assembled at runtime is unreachable by any static scan.

    Anti-vacuity is EXACT: the walk must see the known lib file count, the
    known occurrence count for one hot allowlisted pair, and every allowlist
    entry must match at least one real occurrence (a stale entry fails the
    gate rather than rotting). The unit tier pins the stripper and the
    scanner on in-string fixtures - including a planted RED fixture asserted
    at exactly its two known violations - which this file's own scan then
    proves are invisible once stripped, because this very file is inside the
    walk and is NOT allowlisted. *)

let failures : int ref = ref 0

let check (name : string) (cond : bool) : unit =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    failures := !failures + 1)

(* A guarded, total character accessor: the state machines below look ahead
   one or two bytes and the option absorbs every end-of-string. *)
let char_at (s : string) (i : int) : char option =
  if 0 <= i && i < String.length s then Some s.[i] (* @total-accessor *)
  else None

let is_ident_char (c : char) : bool =
  ('a' <= c && c <= 'z')
  || ('A' <= c && c <= 'Z')
  || ('0' <= c && c <= '9')
  || Char.equal c '_' || Char.equal c '\''

let is_lower_or_us (c : char) : bool =
  ('a' <= c && c <= 'z') || Char.equal c '_'

(* Does [lit] appear in [s] starting at [i]? Total: any overrun is false. *)
let match_at (s : string) (i : int) (lit : string) : bool =
  String.length lit
  |> (fun n ->
       i >= 0 && i + n <= String.length s
       && String.equal (String.sub s i n) lit)

module Strip = struct
  (* Byte-blanking scrub: comments (nested, tracking string literals AND
     char literals inside them, per OCaml's lexer - a quote packaged in a
     comment's char literal must not open a comment-string, or everything to
     the next stray quote is eaten as string and the machine never returns
     to code) and string literals (escapes, and [{id|...|id}] quoted blocks)
     become spaces; newlines survive so every later line number is the
     source's own. Character literals are skipped as code too, so a
     double-quote char literal cannot open a phantom string. Quoted blocks
     inside comments are not tracked (this tree holds exactly one - a few
     lines up in this very comment - and it is balanced; the comment-string
     and comment-char-literal cases the lexer does force are pinned in the
     unit tier). *)
  type mode =
    | Code
    | Com of int
    | ComStr of int
    | Str
    | Quo of string

  let ident_after (s : string) (i : int) : string =
    let rec len (j : int) : int =
      Option.fold (char_at s j) ~none:(j - i)
        ~some:(fun (c : char) -> if is_lower_or_us c then len (j + 1) else j - i)
    in
    String.sub s i (len i)

  let scrub (s : string) : string =
    let b = Buffer.create (String.length s) in
    let blank (c : char) : unit =
      Buffer.add_char b (if Char.equal c '\n' then '\n' else ' ')
    in
    let rec go (i : int) (m : mode) : unit =
      Option.fold (char_at s i) ~none:()
        ~some:(fun (c : char) ->
          match m with
          | Code ->
            if Char.equal c '(' && match_at s (i + 1) "*" then (
              blank c;
              blank '*';
              go (i + 2) (Com 1))
            else if Char.equal c '"' then (
              blank c;
              go (i + 1) Str)
            else if Char.equal c '{' then
              let id = ident_after s (i + 1) in
              if match_at s (i + 1 + String.length id) "|" then (
                blank c;
                String.iter blank (id ^ "|");
                go (i + 2 + String.length id) (Quo id))
              else (
                Buffer.add_char b c;
                go (i + 1) Code)
            else if
              Char.equal c '\''
              && Option.fold (char_at s (i + 1)) ~none:false ~some:(fun c1 ->
                     (* A char literal, ['x'] or ['\..']; a lone quote is an
                        ident/type-var character and stays code. *)
                     Char.equal c1 '\\'
                     || Option.fold (char_at s (i + 2)) ~none:false
                          ~some:(Char.equal '\''))
            then (
              (* Skip the literal wholesale: quote, payload (escaped payloads
                 run to the closing quote within a few bytes; ['\o377'], the
                 longest form, closes at i+6), quote. *)
              let close =
                if Option.fold (char_at s (i + 1)) ~none:false ~some:(Char.equal '\\')
                then
                  List.find_opt
                    (fun (j : int) ->
                      Option.fold (char_at s j) ~none:false ~some:(Char.equal '\''))
                    [ i + 3; i + 4; i + 5; i + 6 ]
                else Some (i + 2)
              in
              Option.fold close
                ~none:(fun () ->
                  Buffer.add_char b c;
                  go (i + 1) Code)
                ~some:(fun (j : int) () ->
                  String.iter blank (String.sub s i (j + 1 - i));
                  go (j + 1) Code)
                ())
            else (
              Buffer.add_char b c;
              go (i + 1) Code)
          | Com d ->
            if Char.equal c '(' && match_at s (i + 1) "*" then (
              blank c;
              blank '*';
              go (i + 2) (Com (d + 1)))
            else if Char.equal c '*' && match_at s (i + 1) ")" then (
              blank c;
              blank ')';
              if d = 1 then go (i + 2) Code else go (i + 2) (Com (d - 1)))
            else if
              Char.equal c '\''
              && Option.fold (char_at s (i + 1)) ~none:false ~some:(fun c1 ->
                     Char.equal c1 '\\'
                     || Option.fold (char_at s (i + 2)) ~none:false
                          ~some:(Char.equal '\''))
            then
              (* OCaml's COMMENT lexer recognises char literals too - that is
                 why [(* ... '"' ... *)] compiles - so a quote packaged in a
                 literal is skipped here, never handed to the string arm
                 below. Without this arm one such construct put the machine
                 in ComStr to the next stray quote and blanked most of the
                 file behind it (the adversarial pass's differential found
                 server_test.ml 94% blind exactly this way). Prose
                 apostrophes fall through: [won't] has no closing quote at
                 i+2 and no backslash at i+1. *)
              let close =
                if Option.fold (char_at s (i + 1)) ~none:false ~some:(Char.equal '\\')
                then
                  List.find_opt
                    (fun (j : int) ->
                      Option.fold (char_at s j) ~none:false ~some:(Char.equal '\''))
                    [ i + 3; i + 4; i + 5; i + 6 ]
                else Some (i + 2)
              in
              Option.fold close
                ~none:(fun () ->
                  blank c;
                  go (i + 1) (Com d))
                ~some:(fun (j : int) () ->
                  (* j proved in-range by [char_at] just above. *)
                  String.iter blank (String.sub s i (j + 1 - i)); (* @total-accessor *)
                  go (j + 1) (Com d))
                ()
            else if Char.equal c '"' then (
              blank c;
              go (i + 1) (ComStr d))
            else (
              blank c;
              go (i + 1) (Com d))
          | ComStr d ->
            if Char.equal c '\\' then (
              blank c;
              Option.fold (char_at s (i + 1)) ~none:() ~some:blank;
              go (i + 2) (ComStr d))
            else if Char.equal c '"' then (
              blank c;
              go (i + 1) (Com d))
            else (
              blank c;
              go (i + 1) (ComStr d))
          | Str ->
            if Char.equal c '\\' then (
              blank c;
              Option.fold (char_at s (i + 1)) ~none:() ~some:blank;
              go (i + 2) Str)
            else if Char.equal c '"' then (
              blank c;
              go (i + 1) Code)
            else (
              blank c;
              go (i + 1) Str)
          | Quo id ->
            if Char.equal c '|' && match_at s (i + 1) (id ^ "}") then (
              blank c;
              String.iter blank (id ^ "}");
              go (i + 2 + String.length id) Code)
            else (
              blank c;
              go (i + 1) (Quo id)))
    in
    go 0 Code;
    Buffer.contents b
end

module Namespace = struct
  (* Constructor names deliberately do NOT spell the sink module names: this
     file is inside its own walk and NOT allowlisted, so a bare [Dream]
     constructor here would (rightly) fail the gate. The [Ns_] spellings
     keep the file genuinely clean rather than specially exempted - which is
     what makes the own-file GREEN control below a real proof. [Ns_lwt_io]
     joined in the review round: [Lwt_io] is raw file I/O, the sibling of
     the audited [Lwt_unix], and journal I/O migrating onto it must not
     leave the gate green. *)
  type t =
    | Ns_dream
    | Ns_irmin_family
    | Ns_unix
    | Ns_lwt_unix
    | Ns_lwt_io

  let all : t list = [ Ns_dream; Ns_irmin_family; Ns_unix; Ns_lwt_unix; Ns_lwt_io ]

  let of_ident (id : string) : t option =
    if String.equal id "Dream" then Some Ns_dream
    else if String.equal id "Unix" then Some Ns_unix
    else if String.equal id "Lwt_unix" then Some Ns_lwt_unix
    else if String.equal id "Lwt_io" then Some Ns_lwt_io
    else if String.length id >= 5 && String.equal (String.sub id 0 5) "Irmin"
    then Some Ns_irmin_family
    else None

  let to_string (t : t) : string =
    match t with
    | Ns_dream -> "Dream"
    | Ns_irmin_family -> "Irmin*"
    | Ns_unix -> "Unix"
    | Ns_lwt_unix -> "Lwt_unix"
    | Ns_lwt_io -> "Lwt_io"

  let equal (a : t) (b : t) : bool =
    match (a, b) with
    | Ns_dream, Ns_dream -> true
    | Ns_dream, (Ns_irmin_family | Ns_unix | Ns_lwt_unix | Ns_lwt_io) -> false
    | Ns_irmin_family, Ns_irmin_family -> true
    | Ns_irmin_family, (Ns_dream | Ns_unix | Ns_lwt_unix | Ns_lwt_io) -> false
    | Ns_unix, Ns_unix -> true
    | Ns_unix, (Ns_dream | Ns_irmin_family | Ns_lwt_unix | Ns_lwt_io) -> false
    | Ns_lwt_unix, Ns_lwt_unix -> true
    | Ns_lwt_unix, (Ns_dream | Ns_irmin_family | Ns_unix | Ns_lwt_io) -> false
    | Ns_lwt_io, Ns_lwt_io -> true
    | Ns_lwt_io, (Ns_dream | Ns_irmin_family | Ns_unix | Ns_lwt_unix) -> false
end

module Scan = struct
  type violation =
    { file : string
    ; line : int
    ; ident : string
    ; ns : Namespace.t
    }

  (* Every occurrence of a sink module name in SCRUBBED text is a violation:
     [Sink.x] (qualified), [open Sink], [include Sink], [module X = Sink],
     [F (Sink)] - the spelling does not matter, a bare capitalized
     occurrence is the introduction. Boundary rules: the previous character
     is neither an identifier character (no [My_unix]) nor a dot (a dotted
     [Foo.Unix] is Foo's own submodule, not the stdlib one). *)
  let line_violations ~(file : string) ~(line : int) (text : string) :
      violation list =
    (* Both fold arms are CLOSURES applied once: [~none:] is eager, and an
       eager [at e acc] here re-scanned the line's remainder on every hit
       (2^k work for k sink occurrences on one line) while an eager
       [List.rev acc] allocated at every character - the review round's
       upheld finding. The thunks restore one tail call per character. *)
    let rec at (i : int) (acc : violation list) : violation list =
      Option.fold (char_at text i)
        ~none:(fun () -> List.rev acc)
        ~some:(fun (c : char) () ->
          let boundary_ok =
            Option.fold (char_at text (i - 1)) ~none:true
              ~some:(fun (p : char) ->
                (not (is_ident_char p)) && not (Char.equal p '.'))
          in
          if ('A' <= c && c <= 'Z') && boundary_ok then
            let rec ident_end (j : int) : int =
              Option.fold (char_at text j) ~none:j
                ~some:(fun (ic : char) ->
                  if is_ident_char ic then ident_end (j + 1) else j)
            in
            let e = ident_end i in
            let id = String.sub text i (e - i) in
            Option.fold (Namespace.of_ident id)
              ~none:(fun () -> at e acc)
              ~some:(fun (ns : Namespace.t) () ->
                at e ({ file; line; ident = id; ns } :: acc))
              ()
          else at (i + 1) acc)
        ()
    in
    at 0 []

  let violations ~(file : string) (scrubbed : string) : violation list =
    String.split_on_char '\n' scrubbed
    |> List.mapi (fun (idx : int) (l : string) ->
           line_violations ~file ~line:(idx + 1) l)
    |> List.concat
end

module Allowlist = struct
  (* The designated sinks, pinned as (repo-relative file, namespace). Every
     entry must match at least one real occurrence (checked below), so a
     module that stops touching its sink must also leave this table. *)
  let entries : (string * Namespace.t) list =
    [ ("lib/tea_server/tea_server.ml", Namespace.Ns_dream)
    ; ("lib/tea_server/tea_server.ml", Namespace.Ns_lwt_unix)
    ; ("lib/tea_server/session_secret.ml", Namespace.Ns_dream)
    ; ("lib/tea_server/session_secret.ml", Namespace.Ns_unix)
    ; ("lib/tea_server/session_secret.mli", Namespace.Ns_dream)
    ; ("lib/tea_server_pack/tea_server_pack.ml", Namespace.Ns_dream)
    ; ("lib/tea_server_pack/tea_server_pack.ml", Namespace.Ns_lwt_unix)
    ; ("lib/tea_server_pack/tea_server_pack.mli", Namespace.Ns_dream)
    ; ("lib/tea_server_pack/guard_file.ml", Namespace.Ns_unix)
    ; ("lib/tea_server_pack/guard_file.ml", Namespace.Ns_lwt_unix)
    ; ("lib/tea_persist/store.ml", Namespace.Ns_irmin_family)
    ; ("lib/tea_persist/store_core.ml", Namespace.Ns_irmin_family)
    ; ("lib/tea_persist/store_core.ml", Namespace.Ns_unix)
    ; ("lib/tea_persist/store_core.mli", Namespace.Ns_irmin_family)
    ; ("lib/tea_persist_pack/store_pack.ml", Namespace.Ns_irmin_family)
    ; ("lib/tea_persist_pack/store_pack.ml", Namespace.Ns_unix)
    ; ("examples/shared_doc/server/shared_doc_serve.ml", Namespace.Ns_dream)
      (* test/ is INSIDE the discipline (default-deny finds a new test file
         that grows a sink), with ordinary entries for its legitimate uses:
         the suite drives the SAME Dream handler the binaries serve
         (test/dune's own doctrine), corrupts journals and forks/kills real
         processes through Unix, and paces Lwt_unix timers. *)
    ; ("test/boot_epoch_test.ml", Namespace.Ns_lwt_unix)
    ; ("test/boot_epoch_test.ml", Namespace.Ns_unix)
    ; ("test/cancel_test.ml", Namespace.Ns_lwt_unix)
    ; ("test/contention_test.ml", Namespace.Ns_lwt_unix)
    ; ("test/contention_test.ml", Namespace.Ns_unix)
    ; ("test/csrf_test.ml", Namespace.Ns_dream)
    ; ("test/guard_file_test.ml", Namespace.Ns_unix)
    ; ("test/guard_identity_test.ml", Namespace.Ns_lwt_unix)
    ; ("test/guard_identity_test.ml", Namespace.Ns_unix)
    ; ("test/guard_water_test.ml", Namespace.Ns_lwt_unix)
    ; ("test/guard_water_test.ml", Namespace.Ns_unix)
    ; ("test/hot_copy_water_test.ml", Namespace.Ns_lwt_unix)
    ; ("test/hot_copy_water_test.ml", Namespace.Ns_unix)
    ; ("test/kill_durability_test.ml", Namespace.Ns_unix)
    ; ("test/pack_guards_test.ml", Namespace.Ns_unix)
    ; ("test/pack_serve_test.ml", Namespace.Ns_dream)
    ; ("test/retention_test.ml", Namespace.Ns_irmin_family)
    ; ("test/rpc_once_test.ml", Namespace.Ns_dream)
      (* server_test drives the full Dream pipeline; it was INVISIBLE to the
         gate until the comment-mode char-literal fix (its line-30 comment
         holds a '"' literal that put the old machine in a comment-string
         for 94% of the file - the adversarial differential's headline). *)
    ; ("test/server_test.ml", Namespace.Ns_dream)
    ; ("test/server_test.ml", Namespace.Ns_lwt_unix)
      (* Lwt_io: journal corruption/pacing I/O in the guard tests and the
         flush/close pair in guard_file itself. *)
    ; ("lib/tea_server_pack/guard_file.ml", Namespace.Ns_lwt_io)
    ; ("test/pack_guards_test.ml", Namespace.Ns_lwt_io)
    ; ("test/guard_water_test.ml", Namespace.Ns_lwt_io)
    ; ("test/guard_identity_test.ml", Namespace.Ns_lwt_io)
    ; ("test/boot_epoch_test.ml", Namespace.Ns_lwt_io)
    ; ("test/rpc_pack_once_test.ml", Namespace.Ns_dream)
    ; ("test/rpc_stream_test.ml", Namespace.Ns_dream)
    ; ("test/rpc_window_test.ml", Namespace.Ns_dream)
    ; ("test/session_identity_test.ml", Namespace.Ns_dream)
    ; ("test/session_secret_test.ml", Namespace.Ns_unix)
    ; ("test/store_identity_test.ml", Namespace.Ns_unix)
    ; ("test/store_pack_flush_test.ml", Namespace.Ns_lwt_unix)
    ; ("test/store_pack_flush_test.ml", Namespace.Ns_unix)
    ; ("test/test_util.ml", Namespace.Ns_lwt_unix)
    ]

  let is_allowed (file : string) (ns : Namespace.t) : bool =
    List.exists
      (fun ((f : string), (n : Namespace.t)) ->
        String.equal f file && Namespace.equal n ns)
      entries
end

module Walk = struct
  (* Guarded fold over a source tree: a path that is not a directory
     contributes itself (when it is a [.ml]/[.mli]) and nothing else; a
     directory folds over its entries. Dot-entries and [_build] are
     build-system noise, never sources. *)
  let wanted (path : string) : bool =
    Filename.check_suffix path ".ml" || Filename.check_suffix path ".mli"

  let rec files (path : string) : string list =
    if not (Sys.file_exists path) then []
    else if Sys.is_directory path then
      Sys.readdir path |> Array.to_list
      |> List.filter (fun (e : string) ->
             (not (String.starts_with ~prefix:"." e))
             && not (String.equal e "_build"))
      |> List.concat_map (fun (e : string) -> files (Filename.concat path e))
    else if wanted path then [ path ]
    else []
end

(* ------------------------------------------------------------------ *)
(* Unit tier: the stripper and the scanner on in-string fixtures.      *)
(* ------------------------------------------------------------------ *)

let () =
  let has_token (s : string) (tok : string) : bool =
    Scan.violations ~file:"fixture" (Strip.scrub s)
    |> List.exists (fun (v : Scan.violation) -> String.equal v.ident tok)
  in
  check "unit: a comment's sink mention is blanked"
    (not (has_token "let x = 1 (* Dream.get *)\n" "Dream"));
  check "unit: nested comments are tracked to their true close"
    (not (has_token "(* outer (* Unix.fork *) still comment Dream.run *) let y = 2\n" "Dream"));
  check "unit: a string INSIDE a comment cannot close the comment"
    (not (has_token "(* say \" *) \" and Unix.fork is still commented *) let z = 3\n" "Unix"));
  check
    "unit: a char-literal quote INSIDE a comment does not open a comment-string (code after the comment stays visible)"
    (has_token "(* up to the next '\"'. *) let u = Unix.getpid ()\n" "Unix");
  check "unit: a plain string's sink mention is blanked"
    (not (has_token "let s = \"calls Dream.run\"\n" "Dream"));
  check "unit: a quoted {|...|} block's sink mention is blanked"
    (not (has_token "let q = {|Dream.run|}\n" "Dream"));
  check "unit: an escaped quote does not end the string early"
    (not (has_token "let e = \"a \\\" b Unix.fork\"\n" "Unix"));
  check "unit: a real token BETWEEN two strings on one line survives the scrub"
    (has_token "let r = \"a\" ^ Dream.target \"b\"\n" "Dream");
  check "unit: a char-literal double quote does not open a string"
    (has_token "let c = '\"' let d = Unix.getpid ()\n" "Unix");
  (* The planted RED fixture: exactly two violations, at their known lines,
     via two different introductions (qualified path; module alias). *)
  let red = "let now = Unix.gettimeofday ()\nmodule D = Dream\n" in
  let vs = Scan.violations ~file:"red" (Strip.scrub red) in
  check "unit: the planted RED fixture yields EXACTLY its two violations"
    (List.length vs = 2);
  check "unit: the planted violations carry their exact lines and namespaces"
    (List.exists
       (fun (v : Scan.violation) ->
         v.line = 1 && Namespace.equal v.ns Namespace.Ns_unix)
       vs
    && List.exists
         (fun (v : Scan.violation) ->
           v.line = 2 && Namespace.equal v.ns Namespace.Ns_dream)
         vs);
  check "unit: open/include/let-open introductions are violations too"
    (List.length
       (Scan.violations ~file:"intros"
          (Strip.scrub "open Lwt_unix\ninclude Irmin_pack\nlet open Dream in ()\n"))
    = 3);
  check "unit: a dotted Foo.Unix is Foo's own submodule, never flagged"
    (not (has_token "let x = Foo.Unix.bar\n" "Unix"));
  check "unit: an identifier merely containing a sink name is not flagged"
    (not (has_token "let y = My_unix.go and z = Dreamer.spin\n" "Unix"));
  (* DEFAULT-DENY probe, hermetic and in-process. *)
  check "unit: default-deny - an unknown (file, namespace) pair is refused"
    (not (Allowlist.is_allowed "lib/tea_core/app.ml" Namespace.Ns_dream))

(* ------------------------------------------------------------------ *)
(* End-to-end tier: the real tree walk under the dune sandbox.         *)
(* ------------------------------------------------------------------ *)

(* Trees as this stanza materializes them, with their repo-relative names. *)
let roots : (string * string) list =
  [ ("../lib", "lib"); ("../examples", "examples"); (".", "test") ]

let read_file (path : string) : string =
  (* [In_channel.with_open_bin] + [really_input_string] with the honest
     length; a file that vanishes mid-walk reads as empty rather than
     raising. *)
  if Sys.file_exists path then
    In_channel.with_open_bin path (fun (ic : in_channel) ->
        In_channel.input_all ic)
  else ""

let display_name ~(root_fs : string) ~(root_name : string) (path : string) :
    string =
  let plen = String.length root_fs in
  if
    String.length path > plen + 1
    && String.equal (String.sub path 0 plen) root_fs
  then root_name ^ String.sub path plen (String.length path - plen)
  else path

let walked : (string * string) list =
  (* (repo-relative display name, file contents), the whole audited surface. *)
  List.concat_map
    (fun ((root_fs : string), (root_name : string)) ->
      Walk.files root_fs
      |> List.map (fun (p : string) ->
             (display_name ~root_fs ~root_name p, read_file p)))
    roots

let all_violations : Scan.violation list =
  List.concat_map
    (fun ((file : string), (contents : string)) ->
      Scan.violations ~file (Strip.scrub contents))
    walked

let () =
  let lib_count =
    List.length
      (List.filter
         (fun ((f : string), (_ : string)) ->
           String.starts_with ~prefix:"lib/" f)
         walked)
  in
  check
    (Printf.sprintf
       "e2e: the walk sees the whole lib/ surface (72 files; saw %d)" lib_count)
    (lib_count = 72);
  let occurrences (file : string) (ns : Namespace.t) : int =
    List.length
      (List.filter
         (fun (v : Scan.violation) ->
           String.equal v.file file && Namespace.equal v.ns ns)
         all_violations)
  in
  let ss_unix = occurrences "lib/tea_server/session_secret.ml" Namespace.Ns_unix in
  check
    (Printf.sprintf
       "e2e: the scan reaches real bytes (session_secret.ml Unix occurrences = 24; saw %d)"
       ss_unix)
    (ss_unix = 24);
  (* The gate itself: every violation must be allowlisted. One FAIL line per
     offender, so a new leak reads as a location, not a count. *)
  let offenders =
    List.filter
      (fun (v : Scan.violation) -> not (Allowlist.is_allowed v.file v.ns))
      all_violations
  in
  List.iter
    (fun (v : Scan.violation) ->
      check
        (Printf.sprintf "e2e: %s:%d: disallowed %s (namespace %s not allowlisted for this file)"
           v.file v.line v.ident
           (Namespace.to_string v.ns))
        false)
    offenders;
  check "e2e: zero un-allowlisted sink occurrences in lib/ + examples/ + test/"
    (List.length offenders = 0);
  (* Allowlist self-check: a pair with zero live occurrences is stale and
     fails the gate, so the table cannot rot wider than the code. *)
  List.iter
    (fun ((f : string), (ns : Namespace.t)) ->
      check
        (Printf.sprintf "e2e: allowlist entry (%s, %s) matches live code" f
           (Namespace.to_string ns))
        (occurrences f ns >= 1))
    Allowlist.entries;
  (* Real GREEN controls: files whose only sink mentions are documentation. *)
  check "e2e: tea_core's doc-comment Irmin mentions stay invisible"
    (List.for_all
       (fun (v : Scan.violation) ->
         not (String.starts_with ~prefix:"lib/tea_core/" v.file))
       all_violations);
  check "e2e: counter's server main.ml is clean (its one Dream mention is a doc comment)"
    (List.for_all
       (fun (v : Scan.violation) ->
         not (String.equal v.file "examples/counter/server/main.ml"))
       all_violations);
  check "e2e: this gate's own fixtures are invisible to its walk (strings scrub)"
    (List.for_all
       (fun (v : Scan.violation) ->
         not (String.equal v.file "test/sink_gate_test.ml"))
       all_violations);
  (* Namespace.all is the closed world this gate audits; pin its size so a
     grown enum must revisit the allowlist and these checks. *)
  check "e2e: the audited namespace world is exactly the R7 four plus Lwt_io"
    (List.length Namespace.all = 5);
  (* R8's arm-distinctness stays a review obligation (a textual arm pin is
     refactor-fragile and substring-unsafe - see the register); what IS
     pinned mechanically is the artifact that backs the review: the csrf
     suite must exist and carry a non-trivial check count, so the gate
     behind the obligation cannot be quietly deleted. *)
  let contains (l : string) (lit : string) : bool =
    let n = String.length l in
    let rec at (i : int) : bool = i <= n && (match_at l i lit || at (i + 1)) in
    at 0
  in
  let csrf =
    List.find_opt
      (fun ((f : string), (_ : string)) -> String.equal f "test/csrf_test.ml")
      walked
  in
  check "e2e: R8's backing artifact exists (test/csrf_test.ml is in the walk)"
    (Option.is_some csrf);
  let csrf_checks =
    Option.fold csrf ~none:0
      ~some:(fun ((_ : string), (contents : string)) ->
        Strip.scrub contents |> String.split_on_char '\n'
        |> List.filter (fun (l : string) -> contains l "check ")
        |> List.length)
  in
  check
    (Printf.sprintf
       "e2e: R8's backing artifact is non-trivial (csrf_test check-lines >= 30; saw %d)"
       csrf_checks)
    (csrf_checks >= 30)

let () =
  if !failures = 0 then (
    Printf.printf
      "\nThe R7 sink boundary is mechanical: %d files walked, every sink use allowlisted (roadmap step 23).\n%!"
      (List.length walked);
    exit 0)
  else (
    Printf.printf "\n%d sink-gate failure(s).\n%!" !failures;
    exit 1)
