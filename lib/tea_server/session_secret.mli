(** Durable session identity (roadmap step 12, D17).

    [Tea_server.Handlers] derives {i everything} session-scoped from one
    string: [Dream.session_id] feeds [hex], [hex] feeds
    {!Tea_core.Prim.Session_id}, that names the Irmin branch, and the branch
    name {i is} the CRDT replica id, which is half of every durable-guard key.
    So the lifetime of that one string is the lifetime of the model, of the
    undo history, and of the replay floor. {!memory} makes it a process
    lifetime (step 11's behaviour, kept as the default everywhere a store is
    itself volatile); {!durable} makes it a cookie lifetime, which is the only
    thing that lets step 11's guard journal ever be consulted after a restart.

    Nothing here raises, nothing here exits, and nothing here prints: every
    failure is a value the caller folds over ({!explain} renders it), so the
    audible line lives at the wiring site next to the guard journal's, and
    {!resolve} stays testable without capturing stderr. *)

(** Key material for [Dream.set_secret]. *)
module Secret : sig
  type t
  (** Abstract with {b no elimination form}: there is no [to_string], no
      [equal], no [compare], no [Repr.t], and no CPS reveal. The bytes cannot
      reach a log line, a response, a commit, or a wire frame through this
      signature, and no serializer for [t] can be written outside
      [session_secret.ml] because no constructor is exported. The only
      observations are {!Session_secret.fingerprint} and
      {!Session_secret.describe}, both of which are domain-separated digests
      or counts. Honest limits: [Marshal] reaches any runtime value, and this
      type says nothing about how the bytes were generated (an operator can
      export 43 low-entropy characters and no constructor can tell). *)

  type err =
    | Empty  (** zero bytes: an unset, truncated, or empty source. *)
    | Too_short of int  (** character count seen, below {!min_chars}. *)
    | Too_long of int  (** character count seen, above {!max_chars}. *)
    | Bad_char of int
        (** 0-based INDEX of the first byte outside the alphabet. The index,
            never the byte: [Safe_key.err] and [Safe_path.err] may echo the
            offending character because they describe attacker or route data,
            but a mistyped secret is still a secret. *)

  val min_chars : int
  (** 32. Dream SHA-256s the secret into an AES-256 key whatever its length,
      so length is a proxy for entropy, not for key size; {!generate} mints 43
      characters (32 random bytes), matching Dream's own recommendation. *)

  val max_chars : int
  (** 512. Past 32 bytes SHA-256 absorbs no further entropy, so a longer value
      signals a pasted log line or a mis-set variable. *)

  val alphabet : string
  (** Documentation of the admitted alphabet: unpadded base64url,
      [A-Za-z0-9_-] (RFC 4648 section 5, what [Dream.to_base64url] emits).
      Padding ['='] is REJECTED rather than normalised: a padded and an
      unpadded spelling of the same key material are different strings and
      therefore different AES keys, and accepting both silently splits a
      fleet. *)

  val of_string : string -> (t, err) result
  (** Total. Checks in order: {!Empty}, {!Too_short}, {!Too_long},
      {!Bad_char}. Does no trimming: whitespace hygiene belongs to whoever
      read the file or the variable. *)

  val generate : unit -> t
  (** [Dream.to_base64url (Dream.random 32)], Dream's CSPRNG. Trusted mint,
      same contract as [Tea_core.Prim.Session_id.v]. *)

  val err_to_string : err -> string
  (** Diagnostic text. By construction it contains no byte of the rejected
      value, only counts and indices. *)
end

type t
(** A session back end: where a browser's identity lives and for how long. *)

val memory : t
(** [Dream.memory_sessions]: identity is minted per process and dies with it.
    This is step 11's behaviour verbatim and stays the default of every
    handler, so the mem tier and all six [Dream.test] suites are byte-for-byte
    unchanged. It is also the {i correct} pairing for a volatile store:
    identity durability must never exceed model durability, or a reconnecting
    tab lands on a durable branch name that resolves to an empty branch. *)

val default_lifetime : float
(** 604800.0 (7 days). Deliberately shorter than Dream's two-week default:
    [cookie_sessions] refreshes a half-expired session {i preserving the id},
    so the lifetime is a sliding window and it is the only dial that bounds an
    unrevocable bearer cookie (DESIGN R13). *)

val durable : ?previous:Secret.t list -> ?lifetime:float -> Secret.t -> t
(** [Dream.cookie_sessions] under [Dream.set_secret]. [?previous] is Dream's
    [~old_secrets]: decrypt/verify only, never encrypt, i.e. the rotation
    window. [?lifetime] defaults to {!default_lifetime}.

    The session PAYLOAD is out of bounds, by build gate (R16, roadmap step
    24): the client holds the whole cookie payload, so AEAD proves the server
    wrote {i some} version, never the {i latest} - monotone state put there
    rolls back on replay. The five payload accessors are member-banned
    repo-wide by [test/sink_gate_test]'s member layer, this module included;
    monotone state belongs on the session's Irmin branch.

    {b [?previous] is INERT on dream 1.0.0~alpha8, and that is an upstream
    defect rather than a wiring mistake here.} Dream's [Cipher.decrypt] walks
    the secret list recursively, and the recursive call drops its own
    [?associated_data] argument:

    {[
      | None -> decrypt cipher secrets ciphertext   (* cipher/cipher.ml:46 *)
    ]}

    Every Dream cookie is encrypted with [~associated_data:("dream.cookie-" ^
    name)], so the {i primary} secret is tried with the associated data and
    every {i old} secret is tried without it. The AES-256-GCM tag check then
    fails for all of them, whatever the key is. The operational consequence is
    that changing a secret logs every session out even with the old one listed,
    which is exactly what a rotation window exists to prevent: until dream is
    fixed, treat a secret change as a full logout of every tab, and expect the
    branches those tabs were editing to be orphaned rather than resumed.

    The surface is kept rather than removed because it becomes correct the day
    the one-line upstream fix lands, and because {!previous_count} and the
    {!previous_env_var} plumbing are already load-bearing for reporting: {!describe}
    says "INERT" out loud at boot whenever a window is configured. What keeps
    that claim honest is not this comment. [session_identity_test.ml] N11 and
    N12 {i pin the defect}, N12 through raw Dream with this module out of the
    path entirely, so the day dream honours [~old_secrets] those checks go red
    and whoever upgrades is sent here to correct this paragraph. *)

val middleware : t -> Dream.middleware
(** The {b only} way to use a back end, because the composition order is not
    optional and is silently catastrophic when inverted: [Dream.set_secret]
    installs the key by {i mutating the request on the way in}, so a session
    back end wrapped {i around} it loads with Dream's process-global fallback
    key and stores with the configured one, minting a fresh session on {i
    every} request (the D16 failure, only worse and invisible). Sealing the
    order in here makes the wrong nesting unwritable at a call site. *)

val is_durable : t -> bool
(** [false] exactly for {!memory}. *)

val fingerprint : t -> string option
(** 12 lowercase hex characters, safe to log and to paste in a ticket: the
    first 48 bits of [Digest.string (domain ^ bytes)] where [domain] is a
    constant tag. The domain separation is NOT decorative: Dream's AES-256-GCM
    key is exactly [SHA-256 secret], and an undomained fingerprint of a
    different hash is still a function of the raw key with no argument for why
    it is safe to print. Sized for the operator question "did these two hosts
    load the same secret", never for an adversarial one; MD5 is a label here,
    not a security primitive. [None] for {!memory}. *)

val previous_count : t -> int
(** How many [~old_secrets] Dream will try, in order. *)

val previous_dropped : t -> int
(** How many {!previous_env_var} entries were refused or truncated away. *)

(** Where the live secret came from. *)
type origin =
  | Process  (** {!memory}: no secret, no cookie, no durability. *)
  | Configured  (** {!durable} built by a caller, provenance unknown here. *)
  | Env of string  (** the variable NAME, never its value. *)
  | File_read of string  (** path; the file already existed and validated. *)
  | File_created of string  (** path; minted here, first boot. *)

val origin : t -> origin

val describe : t -> string
(** One newline-free line for the boot log. Contains the back end, the
    fingerprint, the origin, the old-secret counts, and the lifetime; it
    contains no byte of any secret. *)

(** Why {!resolve} could not produce a durable secret. Refusal never deletes,
    truncates, or rewrites the file: an unreadable secret may be a transient
    mount problem, and overwriting it would permanently orphan every existing
    session branch. *)
type err =
  | Env_unusable of string * Secret.err
      (** variable NAME and why. An explicitly-set variable is an explicit
          operator intent, so this does {i not} fall through to the file:
          serving under a different key from disk splits a fleet into a
          failure that looks like a session bug rather than a typo. *)
  | File_not_regular of string  (** [lstat] says symlink, directory, or device. *)
  | File_insecure of string * int  (** path and [st_perm]: group or world bits set. *)
  | File_oversized of string * int  (** path and byte count, above {!max_file_bytes}. *)
  | File_unreadable of string * string  (** path and [Printexc.to_string] at the syscall seam. *)
  | File_unusable of string * Secret.err  (** the file exists and is readable but holds no admissible secret. *)
  | File_uncreatable of string * string  (** path and errno text: parent missing, read-only, no permission. *)
  | No_source  (** no {!env_var}, no {!file_env_var}, and no [?file]. *)

val env_var : string
(** ["TEA_SECRET"]: the secret itself. *)

val file_env_var : string
(** ["TEA_SECRET_FILE"]: overrides the caller's [?file] path. *)

val previous_env_var : string
(** ["TEA_SECRET_OLD"]: a COMMA-separated rotation window, most recent first,
    matching the order [Dream.decrypt] tries them. Comma rather than colon
    because a colon-separated list reads as a [PATH] and invites file paths.
    Invalid entries are dropped (counted by {!previous_dropped}) rather than
    fatal: refusing to boot over a stale rotation entry is a worse outage than
    the thing rotation exists to avoid. Capped at {!max_previous}. *)

val max_previous : int
(** 8. Every extra old secret is one more AES-GCM attempt per undecryptable
    cookie, so an unbounded list is a self-inflicted CPU cost. *)

val max_file_bytes : int
(** 4096. *)

val resolve : ?file:string -> unit -> (t, err) result
(** Synchronous (plain [Unix], no Lwt, so it composes with [Dream.test] and
    with [serve_pack]'s existing [Lwt_main.run] nesting) and total.

    Rungs, first win takes it: (1) {!env_var}, where unset and set-to-empty
    are both "unset" and a set-but-invalid value is [Env_unusable] with no
    fall-through; (2) the file at {!file_env_var} if set and non-empty, else
    [?file], else [No_source]. The file is [lstat]ed (regular, no group/other
    permission bits, at most {!max_file_bytes}), read whole, stripped of
    exactly one trailing ["\n"] and one ["\r"] before it, and admitted through
    {!Secret.of_string}; if absent it is minted with {!Secret.generate} and
    written [O_WRONLY|O_CREAT|O_EXCL] mode [0o600] then [fsync]ed, so two
    boots racing on one root cannot clobber each other (the loser's [EEXIST]
    re-reads the winner's bytes, once).

    Callers pass [?file] only when the model is itself durable. *)

val explain : err -> string
(** Newline-free operator text for a failure, matching every constructor
    exhaustively, naming paths, modes, counts, and indices and never a secret
    byte. The wiring site prints [explain e]; keeping the match here means a
    new constructor is a compile error in one place instead of churning
    call sites. *)
