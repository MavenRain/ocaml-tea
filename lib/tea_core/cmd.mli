(** Effects as data.

    An [update] returns a new model plus a [Cmd.t] describing effects to run.
    The type is [private]: interpreters (server [Loop], client runtime) may
    pattern-match exhaustively over the constructors, but only the smart
    constructors below can build a value. [Http] carries the wire verb of the
    typed RPC layer (DESIGN §8); the persistence verbs ([Checkpoint],
    [Merge_session]) remain documented extension points (see DESIGN.md). *)

(** Failure of one [Http] command as the transport saw it. *)
type http_failure =
  | Http_status of Prim.Status.t
      (** the request completed with a non-2xx status *)
  | Network_error
      (** no HTTP response at all: offline, DNS, TLS, abort (XHR status 0) *)
  | No_transport
      (** the interpreting tier has no HTTP client: the Io-generic server
          [Loop] resolves every [Http] this way; the browser runtime never
          produces it *)

(** Which delivery contract an [Http] command rides (roadmap step 15, D20). A
    sum rather than a bool, so an interpreter matches it exhaustively and a
    third channel becomes a compile error everywhere instead of a silent
    default. *)
module Http_delivery : sig
  type t =
    | Bare
        (** today's semantics: no delivery key, no server-side dedup,
            at-most-once effect per HTTP exchange, retry safety the caller's
            problem. The only channel {!http} can build. *)
    | Keyed
        (** the exactly-once channel: the client runtime attaches a stable
            delivery key, owns the retry, and re-sends under the same key
            until a reply decodes — steps 10-13 transcribed onto HTTP. *)
end

type 'msg t = private
  | None_
  | Batch of 'msg t list
  | Emit of 'msg  (** feed a message straight back into [update] *)
  | After of Prim.Delay.t * 'msg  (** emit [msg] after a delay *)
  | Navigate of Prim.Url.t  (** push a new URL (client history / server redirect) *)
  | Http of
      { path : Prim.Rpc_path.t
      ; body : string  (** opaque wire payload; Repr-JSON by convention *)
      ; delivery : Http_delivery.t
            (** which delivery contract the interpreter must honour; the key
                itself is not here, because it is minted by the client runtime
                where the tab entropy lives, never by the app *)
      ; expect : (string, http_failure) result -> 'msg
            (** total continuation from the raw transport outcome; the typed
                decode lives in the closure [Tea_rpc.Make.call] builds *)
      }
      (** POST [body] to same-origin [path]; the untyped verb under
          [Tea_rpc.Make.call]. Interpreters fix the method to POST and the
          Content-Type to application/json. *)

val none : 'msg t
val batch : 'msg t list -> 'msg t
val emit : 'msg -> 'msg t
val after : Prim.Delay.t -> 'msg -> 'msg t
val navigate : Prim.Url.t -> 'msg t

val http :
  path:Prim.Rpc_path.t ->
  body:string ->
  expect:((string, http_failure) result -> 'msg) ->
  'msg t
(** Builds [delivery = Bare], today's semantics unchanged. *)

val http_keyed :
  path:Prim.Rpc_path.t ->
  body:string ->
  expect:((string, http_failure) result -> 'msg) ->
  'msg t
(** Builds [delivery = Keyed].

    {b Precondition (R24).} The path must be served by a guarded route
    ([Tea_server.Rpc.routes_once]). A keyed command against an unguarded POST
    path turns at-most-once into at-least-once with nothing on the server side
    to dedup it. In practice [Tea_rpc.Make.call] selects this constructor from
    the endpoint's total [kind] witness and apps keep calling [call]; the key
    is minted by the runtime's delivery queue, never here, so a hand-built
    keyed command is still keyed correctly. *)

val map : ('a -> 'b) -> 'a t -> 'b t
