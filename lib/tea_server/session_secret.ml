(* The implementation of {!Session_secret}. Everything here is total: the only
   raising surface is Unix, and every Unix call goes through [guard], the same
   catch-at-the-seam precedent [Store_core.commit_of_key_opt] and
   [Codec.of_json] already set. Nothing in this file prints - a failure is a
   value the wiring site renders with [explain], which keeps [resolve] testable
   without capturing stderr. *)

module Secret = struct
  (* [string] only inside this file: the .mli exports no elimination form, so
     the bytes cannot leave except through [Dream.set_secret] in [middleware]
     and through the domain-separated [fingerprint]. *)
  type t = string

  type err =
    | Empty
    | Too_short of int
    | Too_long of int
    | Bad_char of int

  let min_chars : int = 32
  let max_chars : int = 512
  let alphabet : string = "A-Za-z0-9_-"

  (* Unpadded base64url (RFC 4648 §5), exactly what [Dream.to_base64url] emits.
     ['='] is outside the alphabet on purpose: a padded and an unpadded
     spelling of one key are different strings, so Dream derives different AES
     keys from them, and admitting both would split a fleet silently. *)
  let in_alphabet (c : char) : bool =
    (c >= 'A' && c <= 'Z')
    || (c >= 'a' && c <= 'z')
    || (c >= '0' && c <= '9')
    || c = '-' || c = '_'

  let of_string (raw : string) : (t, err) result =
    let seen = String.length raw in
    if seen = 0 then Error Empty
    else if seen < min_chars then Error (Too_short seen)
    else if seen > max_chars then Error (Too_long seen)
    else
      String.to_seqi raw
      |> Seq.find (fun ((_ : int), (c : char)) -> not (in_alphabet c))
      |> Option.fold ~none:(Ok raw) ~some:(fun ((i : int), (_ : char)) -> Error (Bad_char i))

  (* Dream's CSPRNG, 32 bytes, rendered in the one alphabet [of_string] admits.
     The sealed [t] has no elimination form, so no test can hand a minted
     secret back to the validator directly; what keeps the sentence true is S6
     in session_secret_test, whose second boot reads a MINTED file back through
     [of_string]. A padded encoder would mint a secret this module's own
     validator rejects on the next boot, and S6 is where that shows up. *)
  let generate () : t = Dream.to_base64url (Dream.random 32)

  let err_to_string (e : err) : string =
    match e with
    | Empty -> "empty"
    | Too_short (seen : int) ->
      Printf.sprintf "%d characters, below the %d-character minimum" seen min_chars
    | Too_long (seen : int) ->
      Printf.sprintf "%d characters, above the %d-character maximum" seen max_chars
    | Bad_char (i : int) ->
      Printf.sprintf "character %d is outside the %s alphabet" i alphabet
end

type origin =
  | Process
  | Configured
  | Env of string
  | File_read of string
  | File_created of string

type t =
  | Memory
  | Durable of
      { secret : Secret.t
      ; previous : Secret.t list
      ; lifetime : float
      ; origin : origin
      ; previous_dropped : int
      }

type err =
  | Env_unusable of string * Secret.err
  | File_not_regular of string
  | File_insecure of string * int
  | File_oversized of string * int
  | File_unreadable of string * string
  | File_unusable of string * Secret.err
  | File_uncreatable of string * string
  | No_source

let env_var : string = "TEA_SECRET"
let file_env_var : string = "TEA_SECRET_FILE"
let previous_env_var : string = "TEA_SECRET_OLD"
let max_previous : int = 8
let max_file_bytes : int = 4096
let default_lifetime : float = 604800.
let fingerprint_domain : string = "tea/session-secret/fingerprint/v1\000"

let memory : t = Memory

let durable ?(previous : Secret.t list = []) ?(lifetime : float = default_lifetime)
    (secret : Secret.t) : t =
  Durable { secret; previous; lifetime; origin = Configured; previous_dropped = 0 }

(* The composition order is the whole point of sealing this: [Dream.set_secret]
   installs the key while the request travels IN, so a session back end wrapped
   around it would load with Dream's process-global fallback key and store with
   the configured one - a fresh session on every request, which is the D16
   failure made invisible. Eta-expanded over [inner] deliberately: a bare
   [Dream.cookie_sessions ~lifetime] would leave Dream's own optional argument
   unerased. *)
let middleware (t : t) : Dream.middleware =
  match t with
  | Memory -> fun (inner : Dream.handler) -> Dream.memory_sessions inner
  | Durable { secret; previous; lifetime; origin = (_ : origin); previous_dropped = (_ : int) }
    ->
    fun (inner : Dream.handler) ->
      Dream.set_secret ~old_secrets:previous secret (Dream.cookie_sessions ~lifetime inner)

let is_durable (t : t) : bool =
  match t with
  | Memory -> false
  | Durable _ -> true

let fingerprint (t : t) : string option =
  match t with
  | Memory -> None
  | Durable { secret; previous = (_ : Secret.t list); lifetime = (_ : float)
            ; origin = (_ : origin); previous_dropped = (_ : int) } ->
    Some (String.sub (Digest.to_hex (Digest.string (fingerprint_domain ^ secret))) 0 12)

let previous_count (t : t) : int =
  match t with
  | Memory -> 0
  | Durable { previous; secret = (_ : Secret.t); lifetime = (_ : float)
            ; origin = (_ : origin); previous_dropped = (_ : int) } ->
    List.length previous

let previous_dropped (t : t) : int =
  match t with
  | Memory -> 0
  | Durable { previous_dropped; secret = (_ : Secret.t); previous = (_ : Secret.t list)
            ; lifetime = (_ : float); origin = (_ : origin) } ->
    previous_dropped

let origin (t : t) : origin =
  match t with
  | Memory -> Process
  | Durable { origin; secret = (_ : Secret.t); previous = (_ : Secret.t list)
            ; lifetime = (_ : float); previous_dropped = (_ : int) } ->
    origin

let origin_to_string (o : origin) : string =
  match o with
  | Process -> "per-process"
  | Configured -> "configured by the caller"
  | Env (name : string) -> Printf.sprintf "from $%s" name
  | File_read (path : string) -> Printf.sprintf "read from %s" path
  | File_created (path : string) -> Printf.sprintf "minted into %s" path

let describe (t : t) : string =
  match t with
  | Memory ->
    "session identity: memory sessions, so identity dies with this process and a restart \
     lands every tab on a fresh branch under a fresh replica"
  | Durable { secret = (_ : Secret.t); previous; lifetime; origin = o; previous_dropped = dropped }
    ->
    (* "configured", never "accepted": on dream 1.0.0~alpha8 an old secret is
       never actually tried against a real cookie (see [durable]'s doc comment
       in the .mli), so an operator reading this line mid-rotation has to be
       told the window is inert BEFORE they retire the old secret, not after.
       Audible at the one moment it is still actionable, which is boot. *)
    Printf.sprintf
      "session identity: durable cookie sessions, fingerprint %s, %s, %d old secret(s) \
       configured%s, %d dropped, lifetime %.0fs"
      (Option.value (fingerprint t) ~default:"none")
      (origin_to_string o) (List.length previous)
      (if previous = [] then ""
       else " but INERT (dream 1.0.0~alpha8 drops the associated data on retry)")
      dropped lifetime

let explain (e : err) : string =
  match e with
  | Env_unusable ((name : string), (why : Secret.err)) ->
    Printf.sprintf "$%s is set but unusable (%s)" name (Secret.err_to_string why)
  | File_not_regular (path : string) ->
    Printf.sprintf "%s is not a regular file (symlink, directory, or device)" path
  | File_insecure ((path : string), (perm : int)) ->
    Printf.sprintf "%s is mode 0o%o; a secret must not be group- or world-accessible" path perm
  | File_oversized ((path : string), (bytes : int)) ->
    Printf.sprintf "%s holds %d bytes, above the %d-byte maximum" path bytes max_file_bytes
  | File_unreadable ((path : string), (reason : string)) ->
    Printf.sprintf "%s could not be read (%s)" path reason
  | File_unusable ((path : string), (why : Secret.err)) ->
    Printf.sprintf "%s holds no admissible secret (%s)" path (Secret.err_to_string why)
  | File_uncreatable ((path : string), (reason : string)) ->
    Printf.sprintf "%s could not be created (%s)" path reason
  | No_source ->
    Printf.sprintf "neither $%s nor $%s is set and no secret file was named" env_var
      file_env_var

(* --- resolution ---------------------------------------------------------- *)

(* The one place a raising dependency is admitted, mirroring
   [Store_core.commit_of_key_opt]: every Unix and channel call in this module
   goes through here, so no syscall failure can escape as an exception. *)
let guard (f : unit -> 'a) : ('a, string) result =
  match f () with
  | value -> Ok value
  | exception (e : exn) -> Error (Printexc.to_string e)

(* Unset and set-to-empty are the same thing: an empty variable is how a shell
   spells "I did not configure this", and treating it as an explicit intent
   would turn a stray [export TEA_SECRET=] into a refusal to boot. *)
let env_opt (name : string) : string option =
  Option.bind (Sys.getenv_opt name) (fun (v : string) -> if v = "" then None else Some v)

(* The rotation window. An invalid entry is DROPPED and counted rather than
   fatal: refusing to boot over a stale rotation entry is a worse outage than
   the one rotation exists to avoid. Truncation past [max_previous] counts as
   dropped too, so [previous_dropped] is "entries the operator wrote that Dream
   will not try". *)
let previous_of_env () : Secret.t list * int =
  env_opt previous_env_var
  |> Option.fold ~none:([], 0) ~some:(fun (raw : string) ->
         let written =
           String.split_on_char ',' raw
           |> List.map String.trim
           |> List.filter (fun (entry : string) -> entry <> "")
         in
         let admitted =
           List.filter_map
             (fun (entry : string) -> Result.to_option (Secret.of_string entry))
             written
         in
         let kept = List.filteri (fun (i : int) (_ : Secret.t) -> i < max_previous) admitted in
         (kept, List.length written - List.length kept))

(* Exactly one trailing newline, and one carriage return before it: what
   `echo secret > file` and its Windows-edited cousin leave behind. Anything
   further is the operator's whitespace and stays significant, because a secret
   that silently trims is a secret two hosts can spell differently. *)
let strip_eol (raw : string) : string =
  let n = String.length raw in
  let n = if n > 0 && raw.[n - 1] = '\n' then n - 1 else n in
  let n = if n > 0 && raw.[n - 1] = '\r' then n - 1 else n in
  String.sub raw 0 n

let is_regular (kind : Unix.file_kind) : bool =
  match kind with
  | Unix.S_REG -> true
  | Unix.S_DIR | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO | Unix.S_SOCK -> false

(* [lstat], not [stat]: a symlink pointing at someone else's file is exactly
   the substitution this check exists to refuse, and following it would report
   the target's mode instead. The residual TOCTOU between this and the open is
   documented (DESIGN R14) - OCaml's Unix has no [O_NOFOLLOW]. *)
let read_secret_file (path : string) : (Secret.t, err) result =
  Result.bind
    (guard (fun () -> Unix.lstat path)
    |> Result.map_error (fun (reason : string) -> File_unreadable (path, reason)))
    (fun (st : Unix.stats) ->
      if not (is_regular st.st_kind) then Error (File_not_regular path)
      else if st.st_perm land 0o077 <> 0 then Error (File_insecure (path, st.st_perm))
      else if st.st_size > max_file_bytes then Error (File_oversized (path, st.st_size))
      else
        Result.bind
          (guard (fun () -> In_channel.with_open_bin path In_channel.input_all)
          |> Result.map_error (fun (reason : string) -> File_unreadable (path, reason)))
          (fun (raw : string) ->
            Secret.of_string (strip_eol raw)
            |> Result.map_error (fun (why : Secret.err) -> File_unusable (path, why))))

let create_flags : Unix.open_flag list = [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ]

(* Two boots racing on one root must AGREE: a loser that overwrote the winner
   would orphan every session branch already minted under the old key.
   [O_EXCL] on the real path is not enough for that, and the gap is invisible
   without a genuine race - it makes the create exclusive but not ATOMIC, so
   between the winner's [openfile] and its [write] every other boot sees a
   zero-byte file and refuses with [File_unusable Empty]. One server boots and
   the rest wedge, on a path that no serialized test can reach.

   So the bytes land in a private [O_EXCL] temp file beside the target,
   fsynced while it is still nameless as far as any reader is concerned, and
   [Unix.link] publishes them under the real name in one indivisible step. The
   winner is whoever's link lands; every loser gets [EEXIST] and re-reads a
   file that was already complete before it had that name, exactly once and
   without retrying. [link] is also what preserves the planted-symlink
   refusal: it does not follow its second argument's final component, so a
   dangling link is an existing NAME rather than a path to write through. *)
let create_secret_file (path : string) : (Secret.t * origin, err) result =
  let minted = Secret.generate () in
  (* Same directory, so the [link] is intra-filesystem. The suffix needs BOTH
     halves, and each covers the other's blind spot.

     Entropy alone is not enough because [Dream.random]'s state is inherited
     across [Unix.fork]: eight children forked from one parent draw the SAME
     bytes, collide on the staging name, and seven of them fail at [O_EXCL]
     instead of reaching the [link]/EEXIST adoption path. S14 forks exactly
     that shape and catches it.

     The pid alone is not enough because a pid comes back around. A boot killed
     between the open and the unlink leaves a staging file behind (DESIGN R14),
     and under a purely pid-derived name that leftover was a permanent wedge
     rather than garbage: a container's server is pid 1, so one OOM-kill
     mid-mint left [.tmp.1] on the volume, every later boot collided with it
     before ever reaching the recovery path, and [serve_pack] degraded to
     per-process identity behind one stderr line, for good, with nothing to
     rotate the name. Two containers sharing one volume were the same story.

     Together: forked siblings differ by pid, and a boot that returns at the
     same pid differs by entropy. [O_EXCL] still carries its real weight, which
     is that this process created what it is about to write, so the refusal
     path never truncates or adopts whatever is sitting there. *)
  let tmp =
    Printf.sprintf "%s.tmp.%d.%s" path (Unix.getpid ()) (Dream.to_base64url (Dream.random 9))
  in
  match Unix.openfile tmp create_flags 0o600 with
  | (fd : Unix.file_descr) ->
    let staged =
      guard (fun () ->
          let n = Unix.write_substring fd minted 0 (String.length minted) in
          Unix.fsync fd;
          n)
    in
    let (_ : (unit, string) result) = guard (fun () -> Unix.close fd) in
    let published =
      Result.bind
        (staged |> Result.map_error (fun (reason : string) -> File_uncreatable (path, reason)))
        (fun (n : int) ->
          let want = String.length minted in
          if n <> want then
            Error (File_uncreatable (path, Printf.sprintf "short write (%d of %d bytes)" n want))
          else
            match Unix.link tmp path with
            | () -> Ok (minted, File_created path)
            | exception Unix.Unix_error (Unix.EEXIST, (_ : string), (_ : string)) ->
              read_secret_file path |> Result.map (fun (s : Secret.t) -> (s, File_read path))
            | exception (e : exn) -> Error (File_uncreatable (path, Printexc.to_string e)))
    in
    (* Scoped to THIS branch on purpose: the [O_EXCL] open above is the only
       proof that this process created [tmp], so the refusal branch must leave
       whatever is sitting there untouched. After a successful [link] the bytes
       have two names and this drops the private one; after a failed one it
       drops a file nothing refers to. Its own failure is not worth reporting
       over the result it would hide. *)
    let (_ : (unit, string) result) = guard (fun () -> Unix.unlink tmp) in
    published
  | exception (e : exn) -> Error (File_uncreatable (path, Printexc.to_string e))

let durable_of ~(origin : origin) (secret : Secret.t) : t =
  let previous, dropped = previous_of_env () in
  Durable { secret; previous; lifetime = default_lifetime; origin; previous_dropped = dropped }

let resolve ?(file : string option) () : (t, err) result =
  let from_path (path : string) : (t, err) result =
    if Sys.file_exists path then
      read_secret_file path
      |> Result.map (fun (s : Secret.t) -> durable_of ~origin:(File_read path) s)
    else
      create_secret_file path
      |> Result.map (fun (((s : Secret.t), (o : origin)) : Secret.t * origin) ->
             durable_of ~origin:o s)
  in
  (* Closures on both branches, applied once: [Option.fold]'s [~none:] is eager,
     so an inline file rung would stat and even MINT a secret file on a boot
     that was configured entirely through the environment. *)
  env_opt env_var
  |> Option.fold
       ~none:(fun () ->
         env_opt file_env_var
         |> Option.fold ~none:file ~some:Option.some
         |> Option.fold ~none:(fun () -> Error No_source) ~some:(fun (path : string) () ->
                from_path path)
         |> fun (k : unit -> (t, err) result) -> k ())
       ~some:(fun (raw : string) () ->
         Secret.of_string raw
         |> Result.fold
              ~ok:(fun (s : Secret.t) -> Ok (durable_of ~origin:(Env env_var) s))
              ~error:(fun (why : Secret.err) -> Error (Env_unusable (env_var, why))))
  |> fun (k : unit -> (t, err) result) -> k ()
