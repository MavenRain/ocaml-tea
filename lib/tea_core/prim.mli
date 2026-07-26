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
  val to_string : t -> string
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
