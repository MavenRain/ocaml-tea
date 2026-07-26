(** The client tier's pure half (roadmap step 2, DESIGN §7): total
    translations from the shared {!Tea_core.Html} / {!Tea_core.Cmd} types into
    ocaml-vdom's, plus {!Make} assembling the [Vdom.app] record for any [APP].

    This library depends only on [tea_core] and [vdom.base] (the pure [Vdom]
    module), so it links natively — the translations are unit-tested off the
    browser ([test/client_test]) — and compiles to JavaScript unchanged. The
    effectful half (mounting, command interpreters) lives in [Tea_client_run],
    which is js_of_ocaml-only. *)

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
      ; expect : (string, Tea_core.Cmd.http_failure) result -> 'msg
      }
      (** POST [body] to same-origin [path] (application/json); feed the raw
          transport outcome to [expect]. Interpreted by [Tea_client_run]'s
          XHR arm. *)

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
