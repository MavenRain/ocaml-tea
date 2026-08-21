(** The client tier's pure half (roadmap step 2, DESIGN §7): total
    translations from the shared {!Tea_core.Html} / {!Tea_core.Cmd} types into
    ocaml-vdom's, plus {!Make} assembling the [Vdom.app] record for any [APP].

    This library depends only on [tea_core] and [vdom.base] (the pure [Vdom]
    module), so it links natively — the translations are unit-tested off the
    browser ([test/client_test]) — and compiles to JavaScript unchanged. The
    effectful half (mounting, command interpreters) lives in [Tea_client_run],
    which is js_of_ocaml-only. *)

(** {2 Pure client-tier logic}

    Four modules that own decisions the runtime used to make inline against
    live browser objects, and that are therefore now testable off the browser:
    when to reconnect a dropped socket, what to do with edits made while it was
    down, which edits the server has actually taken delivery of, and how a
    client-local companion splits the message stream. *)

module Reconnect = Reconnect
module Rebase = Rebase
module Delivery = Delivery
module Rpc_delivery = Rpc_delivery
module Local_channel = Local_channel
module Local_store = Local_store

(** {2 This tab's CRDT identity}

    Which replica the tab's optimistic edits are minted under (roadmap step 8,
    D1; rebound by the server in step 9, D14).

    The client is not a peer replica of the server - it is a {i predictor} of
    one. Minting under an id of its own made every locally-born edit apply
    twice under two different ids, and a [Pn_counter] join sums across replica
    slots, so the acting tab double-counted its own increment (D14). The server
    now announces the replica id it applies this session under
    ({!Tea_core.Wire.Hello}), the tab adopts it, and the two applies of one
    intent land in one slot where [join] is a [max].

    Page-global mutable state, deliberately: a browser page is one tab is one
    session is one identity, and threading it through [Vdom.app]'s update would
    put a wire concern in every app's model. It is settled once per socket, by
    the runtime, before the frame that carries it is dispatched. *)

module Identity : sig
  val provisional : Tea_core.Crdt.Replica.t
  (** The id used before any announcement arrives - and for a page that never
      opens a live socket at all, which is sound precisely because such a page
      never forwards a msg for the server to apply a second time. Edits made in
      that window keep a slot of their own; see {!Rebase.absorb}. *)

  val replica : unit -> Tea_core.Crdt.Replica.t
  val is_provisional : unit -> bool

  val adopt : Tea_core.Crdt.Replica.t -> unit
  (** Rebind the identity. Called by [Tea_client_run] on every [Hello],
      including a reconnect's: a session whose branch changed underneath the
      tab announces a different id, and the tab must follow it rather than keep
      predicting into a slot nobody is authoritative for. *)

  val ctx : unit -> Tea_core.Crdt.Ctx.t
  (** The context handed to [A.update] for a locally-born msg: the current
      replica over the page's one monotonic clock. The clock survives an
      {!adopt} - resetting it could let two dots minted either side of the
      announcement collide once the ids agree. *)

  val clock_floor : unit -> int64
  (** The page clock's current floor, a pure peek (step 25, D25): persisted
      with each checkpoint so a later page life can {!clock_seed} strictly
      above every stamp this one used. *)

  val clock_seed : int64 -> unit
  (** Raise the page clock's floor; it never lowers. Called once at hydration
      with the adopted record's floor, before any local mint, so an adopted
      replica's new dots cannot collide with the ones it minted in its
      earlier life. *)
end

(** {2 Client command extensions}

    [Vdom.Cmd.t] is an open (extensible) type whose built-in constructors
    cover only [Echo]/[Batch]/[Map]/[Bind]. The two effectful verbs of
    {!Tea_core.Cmd} become extension constructors here; [Tea_client_run.env]
    registers their interpreter ([setTimeout] / [history.pushState]). *)

type 'msg Vdom.Cmd.t +=
  | After of int * 'msg  (** emit ['msg] after the given delay, in ms *)
  | Navigate of string  (** push a relative URL onto the browser history *)
  | Http of
      { path : string  (** lowered at the vdom boundary, as [Navigate] lowers [Url] *)
      ; body : string
      ; delivery : Tea_core.Cmd.Http_delivery.t
            (** which delivery contract the interpreter owes this call
                (roadmap step 15, D20); carried across the boundary unchanged,
                because the two channels differ in effect, not in structure *)
      ; expect : (string, Tea_core.Cmd.http_failure) result -> 'msg
      }
      (** POST [body] to same-origin [path] (application/json); feed the raw
          transport outcome to [expect]. Interpreted by [Tea_client_run]'s
          XHR arm: [Bare] fires one request and forgets it, [Keyed] records
          into {!Rpc_delivery} and lets the runtime own the retries. *)

(** {2 Total translations}

    Structure-preserving and exhaustive over the (private) source variants:
    adding a constructor to [Html.attr], [Html.t], or [Cmd.t] breaks this
    library's build rather than silently degrading the client — the T3
    no-drift property. *)

(** [Attr] crosses as a DOM attribute exactly as {!Tea_core.Render_static}
    prints it, except ["value"], which becomes the DOM {e property} so a
    redraw tracks the model instead of leaving stale user edits visible.
    [On_click msg] becomes a ["click"] handler yielding the constant [msg]
    (no event decoding); [On_input f] a ["input"] handler feeding
    [target.value] through [f]. *)
val attr_to_vdom : 'msg Tea_core.Html.attr -> 'msg Vdom.attribute

(** [Text] becomes a DOM text node — {e unescaped}, because text nodes are
    inert by construction ([createTextNode]); escaping is only a
    string-rendering concern ({!Tea_core.Render_static}). *)
val html_to_vdom : 'msg Tea_core.Html.t -> 'msg Vdom.vdom

(** [None_ ↦ Batch []], [Batch ↦ Batch], [Emit ↦ Echo], [After ↦ {!After}],
    [Navigate ↦ {!Navigate}]. *)
val cmd_to_vdom : 'msg Tea_core.Cmd.t -> 'msg Vdom.Cmd.t

(** {2 Subscription planning}

    The pure half of the client's {!Tea_core.Sub} interpreter (roadmap step
    3): flatten a subscription tree into resource specs, key them by the
    runtime resource they need, and diff wanted against active keys. The
    effectful half — [setInterval], the live-view WebSocket — lives in
    [Tea_client_run]; keeping the planning here keeps it natively testable. *)

module Subs : sig
  (** One leaf subscription, with intervals already in plain milliseconds. *)
  type ('model, 'msg) spec =
    | Spec_every of int * (int -> 'msg)
    | Spec_store of ('model -> 'msg)

  (** Flatten [None_]/[Batch] structure away, preserving leaf order. *)
  val specs_of : ('model, 'msg) Tea_core.Sub.t -> ('model, 'msg) spec list

  (** The runtime resource a spec needs: one interval per distinct period,
      one WebSocket regardless of how many [Store_watch] leaves exist.
      Callbacks are deliberately not part of the key — the runtime looks
      handlers up from the {e current} model's subscriptions at fire time, so
      re-planning never has to compare functions. *)
  type key =
    | Key_every of int
    | Key_store

  val key_of_spec : ('model, 'msg) spec -> key
  val equal_key : key -> key -> bool

  (** Deduplicated keys of a spec list, first-occurrence order. *)
  val keys_of : ('model, 'msg) spec list -> key list

  (** [plan ~active ~wanted] is [(to_start, to_stop)]: the set differences
      both ways, preserving input order. Inputs are assumed deduplicated, as
      {!keys_of} produces them. *)
  val plan : active:key list -> wanted:key list -> key list * key list
end

(** {2 The client application} *)

module Make (A : Tea_core.App.APP) : sig
  (** [A]'s init/update/view lifted into ocaml-vdom's record (note the
      argument flip: vdom's update is [model -> msg], ours is [msg -> model]).
      Mount it with [Tea_client_run.Start] (or [Vdom_blit.run] directly). *)
  val app : (A.model, A.msg) Vdom.app
end

(** [A] paired with a client-local companion (roadmap step 8, D10): the same
    lift as {!Make}, but over {!Local_channel.Make}'s two-half state, and with
    the local/shared {!Local_channel.channel} exposed so the mounting runtime
    can decide whether to mirror a message up the live socket.

    [Make_local (A) (Tea_core.Local.None_ (A))] is {!Make} with an extra [unit]
    beside the model - which is why [Tea_client_run.Start] is defined as
    exactly that, instead of a second code path to keep in step. *)
module Make_local
    (A : Tea_core.App.APP)
    (L : Tea_core.Local.LOCAL with type shared = A.model and type msg = A.msg) : sig
  type state = Local_channel.Make(A)(L).state

  val shared : state -> A.model
  val local : state -> L.local

  (** One step, with the channel {!app} has nowhere to put. The runtime calls
      this rather than [app.update] so that a companion-claimed message is
      never mirrored to peers. *)
  val step : A.msg -> state -> state * A.msg Vdom.Cmd.t * Local_channel.channel

  val app : (state, A.msg) Vdom.app
end
