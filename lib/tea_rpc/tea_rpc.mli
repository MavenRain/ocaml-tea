(** The typed RPC contract linked verbatim by both tiers (DESIGN §8, the T3
    no-drift property): endpoint names, paths, and payload codecs have exactly
    one definition. This library links repr and tea_core only — no vdom, no
    dream, no lwt — so server, client, and tests all speak the same values. *)

(** URL-safe endpoint identifiers: non-empty, charset [\[a-z0-9_-\]]. *)
module Name : sig
  type t

  type err =
    | Empty
    | Invalid_char of char  (** the first byte outside [\[a-z0-9_-\]] *)

  (** Framework/app compile-time-literal-only mint (the [Prim.Tag.v]
      doctrine): names come from an [API]'s [name] witness, never from
      request or model data. Tests pin that every deployed literal satisfies
      {!of_string}. *)
  val v : string -> t

  (** Strict parse for untrusted input (tests and tooling; server dispatch
      never parses names — routes are fixed literals from [Make.path_of]). *)
  val of_string : string -> (t, err) result

  val to_string : t -> string
  val equal : t -> t -> bool
end

val prefix : string
(** ["/rpc/"]. Disjoint by construction from every reserved flat path
    ([/], [/msg], [/undo], [Wire.ws_path], [/app]). *)

(** Failure of one typed call as seen by the app's reply handler. App-level
    failure never appears here: a fallible endpoint declares
    ['resp = ('ok, 'app_err) result] in its GADT constructor and rides the
    200 channel through [resp_t]. *)
type error =
  | Transport of Tea_core.Cmd.http_failure
  | Decode of string  (** a 2xx body that does not parse as ['resp] *)

(** What an application supplies: one GADT of endpoints plus three
    per-endpoint witnesses, each an exhaustive wildcard-free match — adding a
    constructor without extending them is a compile error in both tiers. *)
module type API = sig
  type ('req, 'resp) t

  val name : ('req, 'resp) t -> Name.t
  val req_t : ('req, 'resp) t -> 'req Repr.t
  val resp_t : ('req, 'resp) t -> 'resp Repr.t

  type any = Any : ('req, 'resp) t -> any

  val all : any list
  (** Every constructor exactly once. Completeness is test-checked (cover
      witness + length equation + the server reachability sweep); at runtime
      a missing endpoint is a 404. *)
end

(** Plumbing derived from an [API]; both tiers call these exact closures, so
    path and codec choices cannot diverge. *)
module Make (A : API) : sig
  val path_of : ('req, 'resp) A.t -> Tea_core.Prim.Rpc_path.t
  (** [Rpc_path.v (prefix ^ Name.to_string (A.name e))] — total because the
      [Name] charset is closed under [Rpc_path] admission. The server mounts
      one fixed POST route per element of [A.all]; the client posts to the
      same value. No [:param] routes, no parsing, nothing raises. *)

  val of_name : Name.t -> A.any option
  (** Derived from [A.all] + [A.name] — cannot drift from [A.name] by
      construction. For tests and tooling. *)

  val encode_req : ('req, 'resp) A.t -> 'req -> string

  val decode_req :
    ('req, 'resp) A.t -> string -> ('req, Tea_core.Codec.err) result

  val encode_resp : ('req, 'resp) A.t -> 'resp -> string

  val decode_resp :
    ('req, 'resp) A.t -> string -> ('resp, Tea_core.Codec.err) result
  (** All four are [Tea_core.Codec.to_json]/[of_json] at [A.req_t]/[A.resp_t]
      — the same Repr codec Irmin Contents use. *)

  val call :
    ('req, 'resp) A.t ->
    'req ->
    reply:(('resp, error) result -> 'msg) ->
    'msg Tea_core.Cmd.t
  (** The typed client builder: encodes with [encode_req] and returns
      [Tea_core.Cmd.http] whose [expect] closure decodes with [decode_resp]
      and maps failures into {!error}. Do not retry synchronously in [update]
      on [Transport No_transport] — fuel bounds the server [Loop]. *)
end
