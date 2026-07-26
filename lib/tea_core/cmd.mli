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

type 'msg t = private
  | None_
  | Batch of 'msg t list
  | Emit of 'msg  (** feed a message straight back into [update] *)
  | After of Prim.Delay.t * 'msg  (** emit [msg] after a delay *)
  | Navigate of Prim.Url.t  (** push a new URL (client history / server redirect) *)
  | Http of
      { path : Prim.Rpc_path.t
      ; body : string  (** opaque wire payload; Repr-JSON by convention *)
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

val map : ('a -> 'b) -> 'a t -> 'b t
