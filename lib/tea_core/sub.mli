(** Subscriptions: standing sources of messages driven by the runtime.

    [private] for the same reason as {!Cmd}. Two type parameters: ['model]
    because a {!Store_watch} subscription turns each newly committed store
    head into a message, ['msg] because that is what every subscription
    ultimately delivers; timers ignore the model parameter.

    Interpretation is tier-specific (DESIGN §7). The client runtime interprets
    [Every] with [setInterval] and [Store_watch] by opening the
    {!Tea_core.Wire.ws_path} WebSocket; the server pushes committed heads to
    every live socket regardless of the app's subscriptions (capability),
    while the client connects only when the app subscribes (policy). *)

type ('model, 'msg) t = private
  | None_
  | Batch of ('model, 'msg) t list
  | Every of Prim.Interval.t * (int -> 'msg)  (** [int] = posix milliseconds tick *)
  | Store_watch of ('model -> 'msg)
      (** deliver a message computed from each newly committed store head —
          the live-view / collaboration subscription (DESIGN §7) *)

val none : ('model, 'msg) t
val batch : ('model, 'msg) t list -> ('model, 'msg) t
val every : Prim.Interval.t -> (int -> 'msg) -> ('model, 'msg) t
val store_watch : ('model -> 'msg) -> ('model, 'msg) t
val map : ('a -> 'b) -> ('model, 'a) t -> ('model, 'b) t
