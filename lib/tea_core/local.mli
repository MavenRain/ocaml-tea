(** The client-local channel (roadmap step 8, D10 client half).

    A [LOCAL] companion is the home for state that is genuinely {i per client}
    and must never reach the replicated model: an RPC reply, a transient
    "saving..." flag, a collapsed/expanded pane. Before D10 such state either
    polluted the shared model (where it replicated to every peer and had to be
    given a CRDT join it does not deserve) or had nowhere to live at all - the
    reason [Shared_doc]'s [Got_stats] was reduced to identity in D10's shared
    half.

    The split is enforced by [update]'s return type. A companion answers
    [Some (local', cmd)] to claim a message: the shared model is untouched and,
    decisively, the message is {b not} forwarded up the live socket, so a peer
    never learns it happened. It answers [None] to decline, and the message
    falls through to [App.update] on the shared model and is mirrored as
    before. There is no third option, so every message is either local or
    shared, never both.

    Everything here is pure and links natively; the browser runtime
    ([Tea_client_run.Start_local]) only mounts it. *)

module type LOCAL = sig
  (** The replicated model the companion may {i read} - it sees the shared
      state on every step, so a local decision can depend on the document
      without owning any of it. *)
  type shared

  (** The app's message type: one message stream feeds both halves. *)
  type msg

  (** The per-client state. Never serialised, never merged, never replicated:
      it has no [Repr.t] on purpose. *)
  type local

  (** A companion has no subscriptions of its own: a subscription exists to
      feed the message stream, and [update] already sees that stream in full.
      [App.subscriptions] over the shared half stays the only source. *)
  val init : local

  (** [update msg shared local] is [Some (local', cmd)] when the companion
      claims [msg] (local-only: no shared write, no mirror up the socket) and
      [None] when it declines (the message falls through to [App.update]).

      A claiming arm may still return a [Cmd.t] - that is how an RPC request
      stays client-local end to end: the companion issues the call and claims
      the reply, and neither crosses the socket. *)
  val update : msg -> shared -> local -> (local * msg Cmd.t) option

  (** The view sees both halves. A companion that displays nothing extra
      returns [App.view shared] unchanged (see {!None_}). *)
  val view : shared -> local -> msg Html.t
end

(** The empty companion: no local state, every message declined, the app's own
    view. [Local_channel.Make (A) (None_ (A))] therefore behaves exactly as [A]
    did before D10 - the parity that lets [Tea_client_run.Start] stay a
    one-line alias of [Start_local] instead of a second, drifting code path.

    The product that consumes a companion lives in [Tea_client.Local_channel]:
    it is client-tier policy, and keeping it there leaves this module the one
    thing the shared tier needs, namely the contract itself. *)
module None_ (A : App.APP) :
  LOCAL with type shared = A.model and type msg = A.msg and type local = unit
