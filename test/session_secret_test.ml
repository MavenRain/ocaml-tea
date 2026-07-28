(** The session secret and its resolution (roadmap step 12, D17).

    [session_identity_test] pins what a secret {i does} (that a cookie
    survives a fresh middleware stack); this file pins the thing that decides
    which secret exists at all, driven only through
    {!Tea_server.Session_secret}'s public seams plus raw byte and mode surgery
    on the secret file between boots.

    The asymmetry under test is the mirror of the guard journal's. A refused
    secret costs one outage the operator can read off stderr; a secret that is
    silently {i replaced}, regenerated on the losing side of a race,
    overwritten after an unreadable mount, quietly trimmed to a different
    spelling on one host, orphans every session branch minted under the old
    key, and does it looking exactly like a working boot. So every hostile
    shape here (a short file, an empty file, a control byte, group-readable
    modes, an unreadable path) must degrade to a {i named} [Error] with the
    file left byte-identical: never an exception, never a fresh secret, never
    a rewrite.

    Two harness rules make the mutation sweep meaningful. Nothing unwraps a
    [result] into a value that could fail the {i setup}: every observation is
    total and renders an unexpected [Error] as a marker string, so a mutation
    turns a check red under {b that check's own name} instead of under a
    setup line the sweep cannot attribute. And every check name is quoted
    verbatim by the sweep, so editing one here is editing the sweep. *)

module S = Tea_server.Session_secret
module Secret = S.Secret

(* --- Harness --------------------------------------------------------------- *)

(** One assertion: print TAP-ish, exit 1 on the first failure. *)
let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

(** Substring search: the "no secret byte reached this text" assertions are all
    negative, so they need a total [contains]. [&&] is short-circuit, which is
    what keeps [List.init] off a negative length. *)
let contains (hay : string) (needle : string) : bool =
  let n = String.length needle and h = String.length hay in
  n <= h
  && List.exists
       (fun (i : int) -> String.equal (String.sub hay i n) needle)
       (List.init (h - n + 1) Fun.id)

(* --- Total renderings of everything a resolution can produce --------------- *)

(** Every error shape, spelled exhaustively so a new constructor is a compile
    error here and not a silently unasserted case. *)
let err_str (e : S.err) : string =
  let secret_err (se : Secret.err) : string =
    match se with
    | Secret.Empty -> "Empty"
    | Secret.Too_short (n : int) -> Printf.sprintf "Too_short %d" n
    | Secret.Too_long (n : int) -> Printf.sprintf "Too_long %d" n
    | Secret.Bad_char (i : int) -> Printf.sprintf "Bad_char %d" i
  in
  match e with
  | S.Env_unusable ((name : string), (why : Secret.err)) ->
    Printf.sprintf "Env_unusable(%s,%s)" name (secret_err why)
  | S.File_not_regular (p : string) -> Printf.sprintf "File_not_regular(%s)" p
  | S.File_insecure ((p : string), (perm : int)) -> Printf.sprintf "File_insecure(%s,0o%o)" p perm
  | S.File_oversized ((p : string), (n : int)) -> Printf.sprintf "File_oversized(%s,%d)" p n
  | S.File_unreadable ((p : string), (r : string)) -> Printf.sprintf "File_unreadable(%s,%s)" p r
  | S.File_unusable ((p : string), (why : Secret.err)) ->
    Printf.sprintf "File_unusable(%s,%s)" p (secret_err why)
  | S.File_uncreatable ((p : string), (r : string)) -> Printf.sprintf "File_uncreatable(%s,%s)" p r
  | S.No_source -> "No_source"

let origin_str (o : S.origin) : string =
  match o with
  | S.Process -> "Process"
  | S.Configured -> "Configured"
  | S.Env (name : string) -> "Env " ^ name
  | S.File_read (p : string) -> "File_read " ^ p
  | S.File_created (p : string) -> "File_created " ^ p

type observed = (S.t, S.err) result

(** The fingerprint of a resolution, or a marker naming the error it produced
    instead. Two resolutions "agree" exactly when these strings are equal, so a
    mutation that swaps success for failure is a mismatch, not an exit. *)
let fp_of (r : observed) : string =
  r |> Result.fold ~ok:(fun (t : S.t) -> Option.value (S.fingerprint t) ~default:"<none>")
      ~error:(fun (e : S.err) -> "ERROR " ^ err_str e)

let origin_of (r : observed) : string =
  r
  |> Result.fold
       ~ok:(fun (t : S.t) -> origin_str (S.origin t))
       ~error:(fun (e : S.err) -> "ERROR " ^ err_str e)

let describe_of (r : observed) : string =
  r
  |> Result.fold ~ok:S.describe ~error:(fun (e : S.err) -> "ERROR " ^ err_str e)

(* -1 rather than 0: a mutation that erases the rotation window would otherwise
   read as a legitimate empty one. *)
let previous_of (r : observed) : int * int =
  r
  |> Result.fold
       ~ok:(fun (t : S.t) -> (S.previous_count t, S.previous_dropped t))
       ~error:(fun (_ : S.err) -> (-1, -1))

(** The error a resolution produced, or a marker naming the back end it wrongly
    produced instead, so "it succeeded" can never be mistaken for "it failed
    the way I expected". *)
let err_of (r : observed) : string =
  r
  |> Result.fold
       ~ok:(fun (t : S.t) -> Printf.sprintf "UNEXPECTED Ok(%s)" (origin_str (S.origin t)))
       ~error:err_str

(* --- Filesystem and environment scratch ------------------------------------ *)

let write_file ?(perm : int = 0o600) (path : string) (s : string) : unit =
  let fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] perm in
  let (_ : int) = Unix.write_substring fd s 0 (String.length s) in
  Unix.close fd;
  (* The create mode is masked by the process umask, so a mode a case means to
     test has to be set explicitly afterwards. *)
  Unix.chmod path perm

let read_file (path : string) : string = In_channel.with_open_bin path In_channel.input_all

(* [Unix.lstat], not [Sys.is_directory]: the symlink cases leave dangling links
   behind (S26 plants one, and S25's becomes one the moment its target is
   removed), and [Sys.is_directory] follows, so it raises ENOENT on exactly the
   entries this has to delete. *)
let rec rm_rf (path : string) : unit =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
    Array.iter (fun entry -> rm_rf (Filename.concat path entry)) (Sys.readdir path);
    Unix.rmdir path
  | Unix.S_REG | Unix.S_LNK | Unix.S_CHR | Unix.S_BLK | Unix.S_FIFO | Unix.S_SOCK ->
    Unix.unlink path

(** Scratch root per case, the [guard_file_test] idiom: [Filename.temp_dir]
    creates the parent, every artefact lives one component below it, and the
    parent is removed whole. *)
let in_scratch (f : string -> unit) : unit =
  let parent = Filename.temp_dir "ocaml-tea-secret" "" in
  f parent;
  rm_rf parent

(** There is no [Unix.unsetenv] in the stdlib, and there does not need to be:
    [Session_secret] reads an empty variable as an unset one precisely so a
    stray [export TEA_SECRET=] is not an explicit operator intent. That makes
    [""] the honest way to clear one here too. *)
let clear_env () : unit =
  List.iter
    (fun (name : string) -> Unix.putenv name "")
    [ S.env_var; S.file_env_var; S.previous_env_var ]

(* --- Fixtures -------------------------------------------------------------- *)

(* 43 characters each, the shape [Secret.generate] mints: past [min_chars] and
   drawn from the admitted alphabet. Distinct, so a check can tell which one a
   boot actually loaded. *)
let secret_a = "AAAABBBBCCCCDDDDEEEEFFFFGGGGHHHHIIIIJJJJKKK"
let secret_b = "ZZZZYYYYXXXXWWWWVVVVUUUUTTTTSSSSRRRRQQQQPPP"
let secret_c = "MMMMNNNNOOOOPPPPQQQQRRRRSSSSTTTTUUUUVVVVWWW"

(** A 12-character window of [secret_a]: the needle every "no secret byte
    escaped into this text" check looks for. *)
let secret_a_window = "EEEEFFFFGGGG"

(* Two rejects differing only in which byte is illegal, both at index 41. *)
let bad_space = "AAAABBBBCCCCDDDDEEEEFFFFGGGGHHHHIIIIJJJJK K"
let bad_ctrl = "AAAABBBBCCCCDDDDEEEEFFFFGGGGHHHHIIIIJJJJK\tK"

(** The fingerprint a raw secret resolves to, computed through the same seam
    under test rather than by re-implementing the digest here. *)
let fp_of_env (raw : string) : string =
  clear_env ();
  let r = (Unix.putenv S.env_var raw; S.resolve ()) in
  clear_env ();
  fp_of r

let env_origin : string = "Env " ^ S.env_var

(* --- 1. The environment wins, and reaches no file -------------------------- *)

let () =
  in_scratch (fun dir ->
      let path = Filename.concat dir "secret" in
      (* A file that already holds a DIFFERENT secret: if the env rung were
         skipped the resolution would still succeed, so only comparing the
         fingerprints can tell the two apart. *)
      write_file path secret_b;
      let file_fp = fp_of_env secret_b and env_fp = fp_of_env secret_a in
      clear_env ();
      Unix.putenv S.env_var secret_a;
      let r = S.resolve ~file:path () in
      check "S1 TEA_SECRET wins over an existing secret file (origin = Env)"
        (String.equal (origin_of r) env_origin
        && String.equal (fp_of r) env_fp
        && not (String.equal env_fp file_fp));
      (* Never read: the bytes on disk are still the file's own. Never created:
         a path that did not exist still does not. *)
      let untouched = String.equal (read_file path) secret_b in
      let missing = Filename.concat dir "never" in
      let r2 = S.resolve ~file:missing () in
      check "S2 an env secret never creates and never reads the secret file"
        (untouched && (not (Sys.file_exists missing)) && String.equal (origin_of r2) env_origin);
      clear_env ())

(* --- 2. First boot mints the file; second boot adopts it ------------------- *)

let () =
  in_scratch (fun dir ->
      clear_env ();
      let path = Filename.concat dir "secret" in
      (* Asserted BEFORE the call: "the file exists afterwards" is satisfied by
         a file the scratch dir was born with. *)
      let absent_first = not (Sys.file_exists path) in
      let r1 = S.resolve ~file:path () in
      check
        "S3 first boot creates the secret file (asserting non-existence FIRST) and origin = \
         File_created"
        (absent_first && Sys.file_exists path
        && String.equal (origin_of r1) ("File_created " ^ path));
      let st = Unix.stat path in
      check "S4 the created secret file is mode 0600" (Int.equal st.Unix.st_perm 0o600);
      check "S5 the created secret file grants nothing to group or other (st_perm land 0o077 = 0)"
        (Int.equal (st.Unix.st_perm land 0o077) 0);
      (* The round-trip witness for [Secret.generate]: these bytes were minted
         by the generator and are now being admitted by the validator. A padded
         encoder fails exactly here. *)
      let r2 = S.resolve ~file:path () in
      check
        "S6 second boot reads back a secret this process never generated (origin = File_read, \
         same fingerprint)"
        (String.equal (origin_of r2) ("File_read " ^ path) && String.equal (fp_of r2) (fp_of r1)))

(* --- 3. Line endings are hygiene, not key material ------------------------- *)

let () =
  in_scratch (fun dir ->
      clear_env ();
      let bare = Filename.concat dir "bare"
      and lf = Filename.concat dir "lf"
      and crlf = Filename.concat dir "crlf" in
      write_file bare secret_a;
      write_file lf (secret_a ^ "\n");
      write_file crlf (secret_a ^ "\r\n");
      let f (p : string) : string = fp_of (S.resolve ~file:p ()) in
      let bare_fp = f bare in
      check "S7 a secret file with a trailing newline round-trips to the same fingerprint, and CRLF too"
        (String.equal (f lf) bare_fp
        && String.equal (f crlf) bare_fp
        && String.equal bare_fp (fp_of_env secret_a)))

(* --- 4. Refusals: named, non-raising, and non-destructive ------------------ *)

let () =
  in_scratch (fun dir ->
      clear_env ();
      let path = Filename.concat dir "short" in
      write_file path "abc";
      check
        "S8 a 3-byte secret file is refused with File_unusable (Too_short 3) and the file is left \
         byte-identical"
        (String.equal (err_of (S.resolve ~file:path ()))
           (Printf.sprintf "File_unusable(%s,Too_short 3)" path)
        && String.equal (read_file path) "abc"))

let () =
  in_scratch (fun dir ->
      clear_env ();
      let path = Filename.concat dir "empty" in
      write_file path "";
      check "S9 an empty secret file is refused with File_unusable Empty"
        (String.equal (err_of (S.resolve ~file:path ()))
           (Printf.sprintf "File_unusable(%s,Empty)" path)))

let () =
  in_scratch (fun dir ->
      clear_env ();
      let space = Filename.concat dir "space" and ctrl = Filename.concat dir "ctrl" in
      write_file space bad_space;
      write_file ctrl bad_ctrl;
      let want (p : string) = Printf.sprintf "File_unusable(%s,Bad_char 41)" p in
      check "S10 a secret file holding a control byte or a space is refused with File_unusable (Bad_char i)"
        (String.equal (err_of (S.resolve ~file:space ())) (want space)
        && String.equal (err_of (S.resolve ~file:ctrl ())) (want ctrl)))

let () =
  in_scratch (fun dir ->
      clear_env ();
      let path = Filename.concat dir "locked" in
      write_file path secret_a;
      Unix.chmod path 0o000;
      (* root's read bypasses the mode entirely, so the case cannot fail there
         and must not silently pass either. *)
      if Int.equal (Unix.geteuid ()) 0 then
        Printf.printf
          "SKIP - S11 an unreadable secret path is Error File_unreadable (running as root: mode \
           0000 is still readable)\n\
           %!"
      else
        check "S11 an unreadable secret path is Error File_unreadable, never a raise"
          (contains
             (err_of (S.resolve ~file:path ()))
             (Printf.sprintf "File_unreadable(%s," path));
      Unix.chmod path 0o600)

let () =
  clear_env ();
  check "S12 no env and no ?file is Error No_source"
    (String.equal (err_of (S.resolve ())) "No_source")

let () =
  in_scratch (fun dir ->
      clear_env ();
      let path = Filename.concat dir "leaky" in
      write_file path bad_space;
      let text =
        S.resolve ~file:path ()
        |> Result.fold
             ~ok:(fun (t : S.t) -> "UNEXPECTED Ok " ^ origin_str (S.origin t))
             ~error:S.explain
      in
      check "S13 explain names the failure and echoes no byte of the rejected secret"
        (contains text path && contains text "41"
        && (not (contains text secret_a_window))
        && not (contains text bad_space)))

(* --- 5. The race: eight creators, one secret ------------------------------- *)

let () =
  in_scratch (fun dir ->
      clear_env ();
      let path = Filename.concat dir "raced" in
      let n = 8 in
      let out (i : int) = Filename.concat dir (Printf.sprintf "out.%d" i) in
      (* A starting gun. Without it the parent's fork loop SERIALISES the
         children: the first one has already created the file by the time the
         second is spawned, so every later child takes the [Sys.file_exists]
         read path and [O_EXCL] is never contended. Each child blocks reading
         the pipe and is released together by EOF when the parent drops the
         last write end. (S26 is the deterministic witness for the EEXIST arm;
         this one is a real race, so it is evidence about contention rather
         than about a particular branch.) *)
      let rfd, wfd = Unix.pipe () in
      (* Flushed before the fork so no child inherits a buffer it would rewrite,
         and [Unix._exit] so no child runs an [at_exit] or the rest of this
         file. *)
      flush stdout;
      let kids =
        List.init n (fun (i : int) ->
            match Unix.fork () with
            | 0 ->
              Unix.close wfd;
              let (_ : int) = Unix.read rfd (Bytes.create 1) 0 1 in
              Unix.close rfd;
              write_file (out i) (fp_of (S.resolve ~file:path ()));
              Unix._exit 0
            | (pid : int) -> pid)
      in
      Unix.close wfd;
      List.iter
        (fun (pid : int) ->
          let (_ : int * Unix.process_status) = Unix.waitpid [] pid in
          ())
        kids;
      Unix.close rfd;
      let seen =
        List.init n (fun (i : int) ->
            if Sys.file_exists (out i) then read_file (out i) else "MISSING")
      in
      let first = List.nth seen 0 in
      let agreed =
        (not (contains first "ERROR"))
        && (not (String.equal first "MISSING"))
        && List.for_all (fun (s : string) -> String.equal s first) seen
      in
      (* The winner's bytes are the ones on disk, so a ninth boot in this
         process must land on the very same secret. *)
      let after = S.resolve ~file:path () in
      (* One file, literally: the staging copies are published by [link] and
         then unlinked, so a survivor is both a leak and a second readable copy
         of the key. *)
      let leftovers =
        Sys.readdir dir |> Array.to_list
        |> List.filter (fun (e : string) -> contains e "raced" && not (String.equal e "raced"))
      in
      check
        "S14 eight forked creators agree on ONE secret and leave ONE file (Unix.fork before any \
         Lwt, children write their fingerprint to dir/out.<pid> and Unix._exit 0)"
        (agreed
        && String.equal (origin_of after) ("File_read " ^ path)
        && String.equal (fp_of after) first
        && List.length leftovers = 0))

(* --- 6. An explicit variable is an explicit intent ------------------------- *)

let () =
  in_scratch (fun dir ->
      clear_env ();
      let path = Filename.concat dir "secret" in
      Unix.putenv S.env_var "tooshort";
      (* The absent fall-through is the whole point: had it fallen through, the
         boot would have SUCCEEDED under a key from disk while the operator
         believed they had set one, which splits a fleet into what looks like a
         session bug rather than a typo. *)
      check
        "S15 TEA_SECRET shorter than min_chars is Error Env_unusable and does NOT fall through to \
         the file"
        (String.equal
           (err_of (S.resolve ~file:path ()))
           (Printf.sprintf "Env_unusable(%s,Too_short 8)" S.env_var)
        && not (Sys.file_exists path));
      clear_env ())

let () =
  in_scratch (fun dir ->
      clear_env ();
      let path = Filename.concat dir "group" in
      write_file ~perm:0o640 path secret_a;
      check
        "S16 a 0640 secret file is refused with File_insecure 0o640 and the file's bytes are \
         unchanged"
        (String.equal (err_of (S.resolve ~file:path ()))
           (Printf.sprintf "File_insecure(%s,0o640)" path)
        && String.equal (read_file path) secret_a))

(* --- 7. The rotation window ------------------------------------------------ *)

let () =
  clear_env ();
  Unix.putenv S.env_var secret_a;
  Unix.putenv S.previous_env_var (String.concat "," [ secret_b; "short"; secret_c ]);
  let mixed = previous_of (S.resolve ()) in
  let ten = List.init 10 (fun (i : int) -> String.make 43 (Char.chr (Char.code 'a' + i))) in
  Unix.putenv S.previous_env_var (String.concat "," ten);
  let capped = previous_of (S.resolve ()) in
  check
    "S17 TEA_SECRET_OLD with 2 valid and 1 invalid entry gives previous_count 2 and \
     previous_dropped 1; 10 entries give 8 and 2"
    (mixed = (2, 1) && capped = (8, 2));
  clear_env ()

(* --- 8. What may be printed, and what may not ----------------------------- *)

let () =
  clear_env ();
  Unix.putenv S.env_var secret_a;
  Unix.putenv S.previous_env_var (String.concat "," [ secret_b; "short"; secret_c ]);
  let r = S.resolve () in
  let d = describe_of r in
  check "S18 describe contains the fingerprint, the origin and the counts, and no byte of the secret"
    (contains d (fp_of r)
    && contains d ("$" ^ S.env_var)
    && contains d "2 old secret(s) configured but INERT"
    && contains d "1 dropped"
    && (not (contains d secret_a_window))
    && (not (contains d secret_b))
    && not (String.contains d '\n'));
  (* Dream's AES-256 key is exactly [SHA-256 secret]; an undomained digest of
     the raw key is still a function of the key, with no argument for why it is
     safe to paste into a ticket. *)
  check "S19 the fingerprint is domain-separated (it is NOT a prefix of Digest.to_hex (Digest.string secret))"
    (not (String.equal (fp_of r) (String.sub (Digest.to_hex (Digest.string secret_a)) 0 12)));
  clear_env ()

(* --- 9. The default back end is still step 11's ---------------------------- *)

let () =
  let m = S.memory in
  check "S20 memory has origin Process, is_durable false, fingerprint None"
    (String.equal (origin_str (S.origin m)) "Process"
    && (not (S.is_durable m))
    && Option.is_none (S.fingerprint m)
    && Int.equal (S.previous_count m) 0
    && Int.equal (S.previous_dropped m) 0
    && S.is_durable (S.durable (Secret.generate ())))

(* --- 10. Three seams the S-list leaves implicit ---------------------------- *)

let () =
  in_scratch (fun dir ->
      clear_env ();
      let asked = Filename.concat dir "asked" and forced = Filename.concat dir "forced" in
      write_file asked secret_b;
      write_file forced secret_a;
      Unix.putenv S.file_env_var forced;
      let r = S.resolve ~file:asked () in
      check "S21 TEA_SECRET_FILE overrides the caller's ?file path"
        (String.equal (origin_of r) ("File_read " ^ forced)
        && String.equal (fp_of r) (fp_of_env secret_a));
      clear_env ())

let () =
  in_scratch (fun dir ->
      clear_env ();
      let path = Filename.concat dir "huge" in
      write_file path (String.make (S.max_file_bytes + 1) 'A');
      (* The size gate fires off [lstat], before a byte is read, so it is a
         distinct refusal from the validator's own upper bound rather than a
         duplicate of it. *)
      check "S22 a secret file past max_file_bytes is refused with File_oversized, unread"
        (String.equal (err_of (S.resolve ~file:path ()))
           (Printf.sprintf "File_oversized(%s,%d)" path (S.max_file_bytes + 1))));
  check "S23 Secret.of_string admits exactly min_chars and max_chars and refuses one past each"
    (Result.is_ok (Secret.of_string (String.make Secret.max_chars 'A'))
    && Result.is_error (Secret.of_string (String.make (Secret.max_chars + 1) 'A'))
    && Result.is_ok (Secret.of_string (String.make Secret.min_chars 'A'))
    && Result.is_error (Secret.of_string (String.make (Secret.min_chars - 1) 'A')))

let () =
  (* That [generate]'s output round-trips through [of_string] is NOT observable
     here: [Secret.t] is sealed with no elimination form, by design. S6 is the
     witness: its second boot reads a MINTED file back through the validator.
     What is observable here is the other catastrophic shape: a [generate] that
     is not random at all. *)
  let fp_generated () : string =
    Option.value (S.fingerprint (S.durable (Secret.generate ()))) ~default:"<none>"
  in
  check "S24 Secret.generate mints a different secret each call (S6 is what proves it round-trips)"
    (not (String.equal (fp_generated ()) (fp_generated ())))

(* --- 11. A planted symlink is the one shape that could redirect a write ---- *)

let () =
  in_scratch (fun dir ->
      clear_env ();
      let real = Filename.concat dir "real" and link = Filename.concat dir "link" in
      write_file real secret_a;
      Unix.symlink real link;
      (* [lstat], not [stat]: following the link would report the TARGET's kind
         and mode, which is exactly the substitution this check exists to
         refuse. The link resolves to a perfectly valid 0600 secret, so nothing
         but the [lstat] can tell the two apart. *)
      check "S25 a symlink at the secret path is refused with File_not_regular, never followed"
        (String.equal (err_of (S.resolve ~file:link ()))
           (Printf.sprintf "File_not_regular(%s)" link)
        && String.equal (read_file real) secret_a))

let () =
  in_scratch (fun dir ->
      clear_env ();
      let target = Filename.concat dir "planted" and link = Filename.concat dir "dangling" in
      Unix.symlink target link;
      (* A DANGLING link is the deterministic witness for the [O_EXCL] arm, and
         the reason it has to be [O_EXCL]. [Sys.file_exists] follows the link
         and reports false, so resolution takes the CREATE path; POSIX then
         fails that open with EEXIST because the link itself exists, the
         [EEXIST] arm re-reads, and [lstat] refuses it. Drop [O_EXCL] and the
         open follows the link instead, writing this process's secret through
         to an attacker-chosen path, so "no file at the target" is the real
         assertion here, and it is unreachable by any in-process race. *)
      check
        "S26 a dangling symlink at the secret path is refused with File_not_regular and writes \
         nothing through it"
        (String.equal (err_of (S.resolve ~file:link ()))
           (Printf.sprintf "File_not_regular(%s)" link)
        && not (Sys.file_exists target)))

let () =
  in_scratch (fun dir ->
      clear_env ();
      let path = Filename.concat dir "secret" in
      let stale = Printf.sprintf "%s.tmp.%d" path (Unix.getpid ()) in
      let bytes = "left behind by a boot that died mid-mint" in
      write_file stale bytes;
      (* The staging name carries entropy rather than the pid, and this is the
         regression it exists for: under a pid-derived name this leftover
         collided under [O_EXCL] on EVERY later boot at the same pid, which in
         a container (pid 1) is every boot, so the mint failed permanently and
         identity silently fell back to per-process. [O_EXCL] still holds the
         line that matters: the leftover is never truncated and never adopted,
         it is simply not in the way. *)
      let resolved = S.resolve ~file:path () in
      check "S27 a staging leftover at the old pid-derived name does not wedge the mint"
        (Result.is_ok resolved && Sys.file_exists path);
      check "S27 the leftover is neither adopted nor truncated"
        (String.equal (read_file stale) bytes && not (String.equal (read_file path) bytes)))

let () =
  Printf.printf
    "\n\
     The session secret holds: env precedence, file mint/adopt, the O_EXCL race, and refusal \
     without rewrite (D17).\n\
     %!"
