(** The product of an app and its client-local companion (roadmap step 8, D10
    client half): one message stream, two state halves, one view.

    The companion contract itself is {!Tea_core.Local.LOCAL}; this is the
    client-tier policy that consumes it. Pure and natively linkable - the
    browser only shows up in {!Tea_client_run.Start_local}, which mounts
    {!Tea_client.Make_local}'s vdom app over the state below. *)

(** Which half consumed a message. The runtime reads it as the forwarding
    decision, and it is a sum rather than a [bool] so that adding a third
    disposition later (say, "local but mirror anyway") is a compile error at
    every reader instead of a silently inverted flag. *)
type channel =
  | Local_only  (** claimed by the companion: do not mirror up the socket *)
  | Shared  (** declined: [App.update] ran, mirror as usual *)

module Make
    (A : Tea_core.App.APP)
    (L : Tea_core.Local.LOCAL with type shared = A.model and type msg = A.msg) : sig
  type state =
    { shared : A.model
    ; local : L.local
    }

  (** The outcome of one step: the new state, the command it raised, and which
      half raised it. *)
  type step =
    { state : state
    ; cmd : A.msg Tea_core.Cmd.t
    ; channel : channel
    }

  val init : state * A.msg Tea_core.Cmd.t

  (** Offer the message to the companion; run [A.update] only if it declines.
      The [Crdt.Ctx.t] reaches [A.update] alone - a claimed message mints no
      CRDT dot, because it writes nothing replicated. *)
  val update : Tea_core.Crdt.Ctx.t -> A.msg -> state -> step

  val view : state -> A.msg Tea_core.Html.t
end
