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

val key_header : string
(** ["x-tea-key"]. One definition for both tiers (T3); a one-byte drift is
    pinned red by a literal check. A custom header keeps a keyed cross-site
    POST non-simple independently of the content-type gate, hardening the CSRF
    preflight argument.

    The header carries only the CLIENT half of the delivery key — which
    stream, which position in it. The server derives the de-duplication
    namespace from the session cookie, and that half never rides the wire, so
    the header alone can never name someone else's floor (roadmap step 15,
    D20.1). *)

(** The delivery key's wire half: which client stream, and which position in
    that stream's dense per-tab call sequence. The RPC analog of the
    [(tab, seq)] header [Wire.up] carries. The guard never consumes the tab
    raw; see the floor-tab derivation in [Tea_server.Rpc]. *)
module Key : sig
  type t

  val v : tab:Tea_core.Prim.Tab_id.t -> seq:Tea_core.Prim.Msg_seq.t -> t
  (** Pair a client stream tab with a position in its dense sequence. *)

  val tab : t -> Tea_core.Prim.Tab_id.t
  (** The client-minted stream tab: an INPUT to the server's floor-tab
      derivation, never a floor key itself. *)

  val seq : t -> Tea_core.Prim.Msg_seq.t
  (** Position in the tab's dense stream. *)

  type err =
    | Bad_shape  (** not [<tab> ":" <seq>] *)
    | Bad_tab of Tea_core.Prim.Tab_id.err
        (** the tab half fails [Tab_id.of_string] *)
    | Bad_seq  (** the seq half is not a positive decimal integer *)

  val to_string : t -> string
  (** [<32 lowercase hex> ":" <decimal>]. *)

  val of_string : string -> (t, err) result
  (** Total strict parse for the untrusted header; reuses [Tab_id.of_string]
      and [Msg_seq.of_int] so this grammar cannot drift from the socket
      tier's. *)
end

(** The response contract of a [Mutating] endpoint (roadmap step 15): the
    reply, or the typed admission that the effect was already applied and the
    original reply bytes are unrecoverable. *)
type 'resp keyed_resp =
  | Reply of 'resp
      (** the handler's answer: fresh, or replayed verbatim from the reply
          cache, byte-identical to what the one taken delivery computed and
          never recomputed at replay time *)
  | Replayed
      (** the effect was applied exactly once by an earlier delivery of this
          key, and the reply bytes are gone (process restart, cache eviction,
          an endpoint mismatch, or a duplicate below the newest seq) *)

val keyed_resp_t : 'resp Repr.t -> 'resp keyed_resp Repr.t
(** A Repr variant, never a string-in-string envelope (the house wire law).

    [Mutating] endpoints answer 200 with this shape ALWAYS, keyed request or
    not, because [expect] is fixed when [Make.call] builds the command and
    sees neither headers nor the runtime's keying decision: a response whose
    shape varied with a header the closure cannot read would be undecodable
    by construction. [Read_only] endpoints are untouched and still answer a
    bare ['resp]. *)

(** Failure of one typed call as seen by the app's reply handler. App-level
    failure never appears here: a fallible endpoint declares
    ['resp = ('ok, 'app_err) result] in its GADT constructor and rides the
    200 channel through [resp_t]. *)
type error =
  | Transport of Tea_core.Cmd.http_failure
      (** the exchange failed; the effect's fate is UNKNOWN *)
  | Decode of string  (** a 2xx body that does not parse as ['resp] *)
  | Applied_reply_lost
      (** the server answered [Replayed]: the call's effect happened exactly
          once, but its reply cannot be recovered — re-read state if the value
          matters. Deliberately distinct from [Transport]: that one means
          "effect unknown", this one means "effect certain, value lost". It is
          the sentence an app reads before writing recovery code. *)

(** Whether an endpoint changes server state, and therefore whether the server
    demands a same-origin proof before dispatching it (roadmap step 8, D12).

    This is the {i declaration}, not an enforcement: nothing stops a
    [Read_only] handler from writing the store, and such a handler re-opens
    exactly the cross-site forgery hole the gate closes. Classify an endpoint
    [Mutating] the moment its handler can touch persistent state. *)
type endpoint_kind =
  | Read_only
      (** dispatched ungated (the [Content-Type] gate alone makes a cross-site
          POST non-simple, so a browser preflights it); the handler must not
          write the store *)
  | Mutating
      (** dispatched only behind the [same_origin] proof
          [Tea_safe.Origin_gate.check] mints; every other outcome is a 403 *)

(** What an application supplies: one GADT of endpoints plus four
    per-endpoint witnesses, each an exhaustive wildcard-free match — adding a
    constructor without extending them is a compile error in both tiers. *)
module type API = sig
  type ('req, 'resp) t

  val name : ('req, 'resp) t -> Name.t
  val req_t : ('req, 'resp) t -> 'req Repr.t
  val resp_t : ('req, 'resp) t -> 'resp Repr.t

  val kind : ('req, 'resp) t -> endpoint_kind
  (** Total and wildcard-free like the codec witnesses, so no endpoint can
      reach the router unclassified: a new constructor without a kind is a
      compile error before either tier builds. A wildcard arm here would
      silently default the next mutating endpoint to ungated. *)

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
  (** The typed client builder: encodes with [encode_req] and returns an
      [Http] command whose [expect] closure decodes the reply and maps
      failures into {!error}. Do not retry synchronously in [update] on
      [Transport No_transport] — fuel bounds the server [Loop].

      The endpoint's own [kind] witness picks the delivery contract, so there
      is no [call_keyed], no app-minted key, and no second entry point:
      [Read_only] builds [Cmd.http] and decodes a bare ['resp] exactly as
      before; [Mutating] builds [Cmd.http_keyed] and decodes
      [keyed_resp_t (resp_t e)], mapping [Reply r] to [Ok r] and [Replayed]
      to [Error Applied_reply_lost]. Classifying an endpoint [Mutating] is
      therefore the whole of what an app does to get exactly-once delivery. *)
end
