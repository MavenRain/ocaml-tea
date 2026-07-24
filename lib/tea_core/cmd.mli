(** Effects as data.

    An [update] returns a new model plus a [Cmd.t] describing effects to run.
    The type is [private]: interpreters (server [Loop], client runtime) may
    pattern-match exhaustively over the constructors, but only the smart
    constructors below can build a value. This scaffold ships the pure/timer
    subset; the persistence verbs ([Checkpoint], [Merge_session]) and [Http]
    are documented extension points (see DESIGN.md). *)

type 'msg t = private
  | None_
  | Batch of 'msg t list
  | Emit of 'msg  (** feed a message straight back into [update] *)
  | After of Prim.Delay.t * 'msg  (** emit [msg] after a delay *)
  | Navigate of Prim.Url.t  (** push a new URL (client history / server redirect) *)

val none : 'msg t
val batch : 'msg t list -> 'msg t
val emit : 'msg -> 'msg t
val after : Prim.Delay.t -> 'msg -> 'msg t
val navigate : Prim.Url.t -> 'msg t
val map : ('a -> 'b) -> 'a t -> 'b t
