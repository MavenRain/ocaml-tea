(** Primitive newtypes for values that cross a trust or serialization boundary.

    No primitive obsession: every string/int that carries meaning is a distinct
    abstract type with a validating constructor. Untrusted input is admitted only
    through [of_string]-style constructors that return [result]/[option];
    framework-internal literals may use the trusted [v] constructors. *)

module Title : sig
  type t

  val v : string -> t
  val to_string : t -> string
end

module Url : sig
  type t

  type err =
    | Empty
    | Not_relative  (** absolute or protocol-relative ([//host]) URLs are rejected *)
    | Backslash
        (** a ['\\'] anywhere: browsers normalise it to ['/'], so [/\evil.com]
            would read as the protocol-relative [//evil.com] once the
            relative-only check has already passed *)
    | Control_char of char
        (** a byte below [0x20] or [0x7f]: a {!t} feeds the [Location] response
            header, where a [CR]/[LF] is response splitting, so control bytes
            are refused at admission rather than at the sink *)

  val of_string : string -> (t, err) result
  val to_string : t -> string
end

module Tag : sig
  type t

  type err =
    | Empty
    | Invalid_char of char  (** the first byte outside [\[a-z\]\[a-z0-9-\]*] *)

  (** Untrusted tag names, allowlisted to [\[a-z\]\[a-z0-9-\]*] (HTML elements
      and custom elements). The tag position carries no escaping at render, so
      this charset is the entire guard against markup injection through a tag. *)
  val of_string : string -> (t, err) result

  (** Framework-internal, compile-time-literal-only mint: tags come from [Html]
      view helpers, never from request or model data. *)
  val v : string -> t

  val to_string : t -> string
end

module Attr_name : sig
  type t

  type err =
    | Empty
    | Event_handler  (** [on*] names are rejected: closes the inline-handler XSS sink *)
    | Invalid_char of char
        (** the first byte outside the allowlist [\[A-Za-z0-9_.:-\]] (which
            covers [class], [data-*], [aria-*], [xlink:href], [viewBox]) *)

  (** Untrusted attribute names. Rejects [on*] handlers, then allowlists
      [\[A-Za-z0-9_.:-\]]: names are emitted UNescaped at render, so a single
      accepted name like [x onmouseover=alert(1) y] would smuggle a live event
      handler into the tag, and the charset is that guard. *)
  val of_string : string -> (t, err) result

  (** Framework-internal, compile-time-literal-only mint (for known-safe
      literals like ["class"]); never request- or model-derived data. *)
  val v : string -> t

  val to_string : t -> string
end

module Attr_value : sig
  type t

  (** Deliberately unvalidated: an attribute {i value} carries arbitrary text.
      It is safe ONLY paired with [Render_static]'s rendering, which emits every
      value inside double quotes and escapes both quote characters; never emit
      an [Attr_value.t] unquoted. *)
  val v : string -> t

  val to_string : t -> string
end

module Text : sig
  type t

  val v : string -> t
  val to_string : t -> string
end

module Input_text : sig
  type t

  val v : string -> t
  val to_string : t -> string
end

module Delay : sig
  type t

  val of_ms : int -> t option
  val to_ms : t -> int
end

module Interval : sig
  type t

  val of_ms : int -> t option
  val to_ms : t -> int
end

module Fuel : sig
  type t

  val default : t
  val of_int : int -> t option

  (** Consume one unit; [None] once exhausted. *)
  val burn : t -> t option

  val to_int : t -> int
end

module Session_id : sig
  type t

  val of_string : string -> t option

  (** Framework-internal, compile-time / branch-name-literal mint (mirrors the
      other newtypes' trusted [v]): the persistence layer builds a
      {!Crdt.Replica} from a session's own branch name, which is not untrusted
      input. Never call it on request- or model-derived data. *)
  val v : string -> t

  val to_string : t -> string

  val t : t Repr.t
  (** Repr witness (a plain string codec): {!Crdt.Replica} is a session id, and
      exposing the witness here lets the CRDT states carry it {i without} a
      [Repr.map] wrapper — irmin-pack's contents-length framing misframes a
      [map]-wrapped string, so the direct witness is load-bearing. *)

  val compare : t -> t -> int
  (** Total order on session ids ([String.compare] on {!to_string}); the
      deterministic tie-break for a CRDT LWW register when two replicas mint
      the same clock stamp (roadmap step 8, D1). *)
end

module Retention : sig
  (** How many checkpoints a retention ring keeps (roadmap step 8, D4). *)
  type t

  val of_int : int -> t option
  (** [None] for a non-positive count. *)

  val default : t
  (** Keep the last 8 checkpoints. *)

  val to_int : t -> int
end

module Ttl : sig
  (** A session-branch idle lifetime, in seconds, for the reaper (roadmap step
      8, D3). *)
  type t

  val of_seconds : float -> t option
  (** [None] for a non-positive duration. *)

  val to_seconds : t -> float
end

module Branch_name : sig
  type t

  val main : t
  val of_session : Session_id.t -> t
  val of_string : string -> t option
  val to_string : t -> string
end

module Commit_ref : sig
  type t

  val of_hash : string -> t
  val to_string : t -> string
end

module Rpc_path : sig
  type t

  type err =
    | Empty
    | Not_rooted  (** must begin with ['/'] *)
    | Invalid_char of char
        (** the first byte outside [\[A-Za-z0-9/_.-\]]: the path is spliced
            into the client's [XHR.open_] and matched byte-for-byte by the
            server router, so the charset closes URL-metachar smuggling
            ([? # \\]) and control-byte injection at admission *)

  val of_string : string -> (t, err) result

  (** Framework-internal, compile-time-literal-only mint: RPC paths come from
      [Tea_rpc.Make.path_of] over the closed [Name] charset, never from
      request or model data. *)
  val v : string -> t

  val to_string : t -> string
end

module Status : sig
  (** An HTTP status actually received: [100..599]. [of_int] rejects XHR's
      status [0] (no HTTP response at all: offline, DNS, abort) and junk, so
      classification downstream needs no magic-int tests. *)
  type t

  val of_int : int -> t option

  (** [200..299] *)
  val is_success : t -> bool

  val to_int : t -> int
end

module Tab_id : sig
  (** A per-page-load delivery identity: 16 random bytes as 32 lowercase hex
      characters (roadmap step 10, D15).

      One Dream session cookie is one session, one branch and {i one}
      {!Crdt.Replica} — the browser smoke test drives two tabs on exactly that
      configuration — so a session cannot name a sender, and two tabs both
      numbering their messages from {!Msg_seq.one} would collide. This can name
      one: it is minted once per page, rides every up-frame, and therefore
      survives socket loss without surviving a reload, which is exactly the
      lifetime the client's unacked queue has.

      It is a de-duplication key, {b not a credential}. Authority is the session
      cookie; a tab id is scoped strictly inside one already-authenticated
      session and confers nothing on its own. Unlike the framework-internal
      newtypes there is no trusted [v] mint: every inhabitant arrives either
      from the wire ({!of_string}) or from a browser entropy source
      ({!of_bytes}), and both validate. *)
  type t

  type err =
    | Wrong_length
    | Invalid_char of char

  val of_string : string -> (t, err) result
  (** The wire path: exactly 32 characters, each in [0-9a-f]. The fixed length
      is half of the server table's memory bound — a count bound over
      unbounded keys bounds nothing. *)

  val of_bytes : int list -> t option
  (** Exactly 16 integers, each in [0..255], hex-encoded. [None] otherwise — a
      caller that offered something unexpected gets a declined mint, never an
      exception. Used by tests, which want the refusal to be observable. *)

  val of_draws : (unit -> int) -> t
  (** The runtime's mint path: draw 16 bytes from an entropy source. Total by
      construction — the count is fixed here and each draw is masked into range
      — so the browser tier has no refusal to handle, and therefore no reason
      to reach for a constant id, which would be the tab-collision bug wearing
      a fallback's clothes. *)

  val to_string : t -> string

  val compare : t -> t -> int
  (** Total order ([String.compare] on {!to_string}), so the server's replay
      guard can key a map on tab ids. *)

  val t : t Repr.t
  (** Repr witness (a plain string codec, the {!Session_id.t} framing reason).
      Note it is {i total} on decode, which is why the pump validates with
      {!of_string} rather than trusting a witness-decoded field. *)
end

module Msg_seq : sig
  (** A per-tab message number, 1-based, assigned in the order the user made
      the edits and never reused (roadmap step 10, D15).

      Unique per {b tab}, never per session: two tabs on one cookie both start
      at {!one}, and a server that deduplicated on a sequence number alone
      would answer "already seen" to the second tab's first edit and drop it
      forever. *)
  type t

  val one : t

  val next : t -> t option
  (** [None] at the end of the sequence space (the {!Fuel} precedent: a defined
      arm, never a wraparound and never an exception). A tab that reaches it
      has stopped sending; on jsoo's 31-bit integers that is ~10^9 edits in one
      page life. *)

  val of_int : int -> t option
  (** The wire path: [n < 1] is [None]. *)

  val to_int : t -> int
  val compare : t -> t -> int
  val t : t Repr.t
end
