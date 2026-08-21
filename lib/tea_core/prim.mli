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
  (** [None] below one second. Commit dates are whole seconds, so a
      sub-second ttl would truncate to the sweep-everything cutoff (every
      branch not written in the current second), not a lifetime. *)

  val to_seconds : t -> float

  val whole_seconds : t -> int64
  (** The ENFORCED span: [ceil] of the ttl, in whole seconds. [reap]'s
      cutoff and the serve boot banners all read this one definition, so
      the announced policy and the enforced one can never disagree. *)
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

module Store_water : sig
  (** A session branch's commit high-water: the [Info] date standing at its
      head, minted by the store's monotonic clock (roadmap step 13, R20).

      Every commit on a branch carries a date strictly greater than its
      parent's ([Store_core.info_v] mints through [Clock.next], and a reopened
      handle reseeds from every head), so this value {b advances with commits}
      and can only fall when the branch's own bytes are older than they were:
      a restored snapshot, a hot copy whose commits never left irmin-pack's
      user-space buffer, a reaped or collected branch, an [undo]. That is the
      whole witness. A delivery floor records the water standing when it was
      taken, and a floor its branch no longer {!covers} is a floor over an
      effect the store does not have.

      Deliberately {i derived}, never stored: a witness that must be WRITTEN
      is a fourth thing that can be restored out of step with the three
      durability siblings, which is R20 recursed one level. This one is a
      function of the very bytes whose freshness is in question, so restoring
      the store restores its water atomically. And deliberately not a
      {i create-time} identity token: such a token is constant over the
      store's whole life, so every snapshot of one store carries it and it
      provably cannot see the rollback R20 names. That is why
      {!Store_identity} exists beside it rather than instead of it (step 18,
      D23): the token answers identity, this answers order, and R20 needs
      both. *)
  type t

  val bottom : t
  (** Below every real water, and the only inhabitant meaning "no claim".
      THREE distinct absences read as [bottom] and all three mean the same
      thing — nothing was committed that this floor could outlive: a branch
      with no commits, a floor recorded by a pre-step-13 binary, and a head
      read the backend refused. [covers ~head:bottom ~floor:bottom] is [true],
      so no absence ever manufactures a drop on its own. *)

  val of_date : int64 -> t
  (** An Irmin [Info] date read back off a branch head. Total: any int64 is a
      legal date, and ordering is all this type promises. *)

  val covers : head:t -> floor:t -> bool
  (** [covers ~head ~floor] is [true] when the branch head stands at or above
      the water a floor recorded, i.e. the store still holds a commit at
      least as new as the one that stood when the floor was taken.

      The {b only} comparison the production path may spell, and it is
      labelled so it cannot be written backwards: inverted, it drops every
      sound floor and keeps every stale one — the silent-loss direction.
      Equality passes, and that is load-bearing rather than incidental: after
      an orderly restart a floor's water EQUALS its branch head, so a strict
      comparison here would drop every floor on every clean restart and
      silently turn durable de-duplication off. *)

  val compare : t -> t -> int
  (** Total order, for assertions and bookkeeping. {!covers} is the
      production spelling; this one is not labelled and can be written
      backwards. *)

  val equal : t -> t -> bool
  val to_int64 : t -> int64

  val t : t Repr.t
  (** The journal payload witness ([Repr.int64]); the route back from bytes,
      so the newtype stays sealed. *)
end

module Store_identity : sig
  (** A pack root's create-once lineage token: 16 random bytes as 32 lowercase
      hex characters (roadmap step 18, D23, R20a).

      The identity half of the pair {!Store_water} is the order half. A water
      answers "did these bytes go backwards"; this answers "are these the same
      store's bytes at all". Neither substitutes for the other and both are
      needed: a water cannot see a DIFFERENT store whose same-named branch
      stands at a newer head (R20a), and a create-time token provably cannot
      see an older snapshot of the SAME store, which carries it unchanged.

      It is a de-duplication key over STORES, {b not a credential} - the same
      shape of thing {!Tab_id} is over tabs, one layer up. Its only job is
      collision avoidance across every root an operator will ever create, so
      128 bits is many orders of magnitude past the birthday bound that
      matters. Nothing is authorised by holding it, and an attacker who can
      write one into a root or a journal already owns the directory this whole
      guard family trusts.

      Encoding is hex, not {!Session_secret}'s base64url, because fixed-case
      hex has no padding and no equivalent-spelling ambiguity to police: two
      spellings of one value would compare unequal and manufacture a
      mismatch. There is deliberately no [Repr.t] witness - the journal header
      carries {!to_string} directly at fixed width, and an unused second route
      back from bytes is a seam with no caller. *)
  type t

  type err =
    | Wrong_length
    | Invalid_char of char

  val of_string : string -> (t, err) result
  (** Exactly 32 characters, each in [0-9a-f]. Uppercase is REFUSED, not
      normalised: a case-folding round-trip that silently produced a second
      spelling would read as a foreign store and wipe every floor. *)

  val of_bytes : int list -> t option
  (** Exactly 16 integers, each in [0..255], hex-encoded. [None] otherwise -
      a declined mint, never an exception. *)

  val of_draws : (unit -> int) -> t
  (** The mint path: draw 16 bytes from an entropy source. Total by
      construction (the count is fixed here, each draw masked into range), so
      there is no refusal a caller could answer with a constant token - which
      would be the store-collision bug wearing a fallback's clothes. *)

  val to_string : t -> string
  val equal : t -> t -> bool

  val compare : t -> t -> int
  (** Total order, for maps and assertions. *)

  type binding =
    | Bound of t
        (** This boot established the store's own identity, durably: read from
            or minted into [<root>/tea.identity]. Only this arm may be
            stamped into a journal. *)
    | Unresolved
        (** The store's identity could NOT be established this boot: the token
            is absent and unmintable, unreadable, or malformed. It is
            deliberately NOT an identity value and deliberately NOT a
            freshly-drawn stand-in - a stand-in would be stamped into every
            journal on every boot, so floors earned under one boot would be
            wiped by the next, and a journal whose root token was only
            transiently unreadable could never recover. *)
end

module Store_epoch : sig
  (** A pack root's boot counter: bumped in the root at each open and echoed
      into each journal (roadmap step 21, R20b).

      The third leg of the store-provenance family. {!Store_water} answers
      "did this branch go backwards"; {!Store_identity} answers "are these
      the same store's bytes at all"; this answers the question the other two
      provably cannot: "is this journal from THIS copy of that store". A
      create-time token is constant over a store's whole life, so a [cp -r],
      a filesystem snapshot, and a restored-then-advanced backup all carry it
      unchanged - only a value that MOVES at every boot can separate a store
      from its own divergent copy.

      The check is EQUALITY, never order: a journal ahead of the root (the
      root is the restored copy) and a journal behind it (the journal is)
      are the same verdict, so an ordering compare would reintroduce half of
      R20b. Divergence degrades accept-side - floors clear, replays land as
      visible duplicates - and never refuses service.

      Encoding is 16 lowercase hex characters, the {!Store_identity} rule
      one value down: fixed case and fixed width leave no second spelling of
      one stamp to police. There is deliberately no [Repr.t] witness - the
      journal header carries {!to_string} directly at fixed width. *)
  type t

  val bottom : t
  (** The mint value for a root that has never carried a counter. *)

  val succ : t -> t
  (** The bump. SATURATES at the top rather than wrap ({!Clock}'s rule): a
      wrap would re-mint an old stamp, and an old stamp is exactly what this
      counter exists to expose. Unreachable in practice - one boot per
      microsecond for two hundred millennia sits below the ceiling. At the
      ceiling itself the counter stops moving, so every same-lineage boot
      matches forever: divergence detection is OFF at saturation, a residue
      the magnitude argument prices and the R20b register names. *)

  val compare : t -> t -> int
  (** Total order, for maps and assertions - NOT for the boot check, which
      is {!equal} by design. *)

  val equal : t -> t -> bool

  val to_string : t -> string
  (** Exactly 16 characters, each in [0-9a-f], zero-padded. *)

  val of_string : string -> (t, [ `Malformed ]) result
  (** Exactly 16 characters, each in [0-9a-f]. Uppercase is REFUSED, not
      normalised, the {!Store_identity.of_string} reason: a second spelling
      of one stamp would compare unequal and manufacture a divergence.
      Values above {!succ}'s saturation point are REFUSED as malformed too:
      the mint never emits one, and accepting one would hand {!succ} a
      value it WRAPS through zero - a wrapped counter re-mints old stamps,
      the false-match direction this type exists to close. *)

  type binding =
    | Bound of t
        (** This boot established the root's counter durably: read from and
            re-written into [<root>/tea.epoch], or minted at {!bottom} on a
            root that never carried one. Only this arm may be stamped into a
            journal. *)
    | Unresolved
        (** The counter could NOT be established this boot: the file exists
            but is unreadable or malformed. Deliberately NOT a re-mint - a
            fresh baseline written over a transient read failure would turn
            every journal's next boot into a permanent false divergence. The
            bytes stay as found, floors clear for this boot's view only, and
            the next boot retries. *)
end
