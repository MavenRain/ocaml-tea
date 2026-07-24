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

  val of_string : string -> (t, err) result
  val to_string : t -> string
end

module Tag : sig
  type t

  (** Framework-internal: tags come from [Html] view helpers, never user input. *)
  val v : string -> t

  val to_string : t -> string
end

module Attr_name : sig
  type t

  type err =
    | Empty
    | Event_handler  (** [on*] names are rejected: closes the inline-handler XSS sink *)

  (** Untrusted attribute names. Rejects [on*] handlers. *)
  val of_string : string -> (t, err) result

  (** Framework-internal trusted mint (for known-safe literals like ["class"]). *)
  val v : string -> t

  val to_string : t -> string
end

module Attr_value : sig
  type t

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
