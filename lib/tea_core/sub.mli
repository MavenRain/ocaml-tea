(** Subscriptions: standing sources of messages driven by the runtime.

    [private] for the same reason as {!Cmd}. This scaffold ships timers; the
    [Store_watch] constructor (Irmin [S.watch_key] on the server, WebSocket push
    on the client) is the collaboration payoff and is documented in DESIGN.md. *)

type 'msg t = private
  | None_
  | Batch of 'msg t list
  | Every of Prim.Interval.t * (int -> 'msg)  (** [int] = posix milliseconds tick *)

val none : 'msg t
val batch : 'msg t list -> 'msg t
val every : Prim.Interval.t -> (int -> 'msg) -> 'msg t
val map : ('a -> 'b) -> 'a t -> 'b t
