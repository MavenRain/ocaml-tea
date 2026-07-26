(** The ONE endpoint definition both tiers link (DESIGN.md section 8).
    repr+tea_rpc only: no vdom, no dream, no lwt. Satisfies {!Tea_rpc.API}. *)

(** Distinct nominal payload types per endpoint: transposing [req_t]/[resp_t]
    or calling an endpoint at the wrong type fails to unify — by construction,
    not by test. *)
type stats_req =
  { title : string
  ; body : string
  }

type stats_resp =
  { title_len : int
  ; word_count : int
  }

val stats_req_t : stats_req Repr.t
val stats_resp_t : stats_resp Repr.t

val stats_of : stats_req -> stats_resp
(** The pure semantics of the {!Doc_stats} endpoint: [title_len] is the title
    byte length, [word_count] the number of maximal non-whitespace runs in the
    body (space, tab, newline, and CR all separate). Shared by the server
    handler and the tests so the two cannot compute it differently. *)

type ('req, 'resp) t =
  | History_count : (unit, int) t
      (** commits on the canonical branch — the seam [undo] proves *)
  | Doc_stats : (stats_req, stats_resp) t
      (** pure echo-with-transform over the doc fields *)
  | Append_tag : (string, int) t
      (** the store-mutating endpoint (roadmap step 8, D12): adds the requested
          tag to the canonical document through the ordinary TEA step (one
          [Add_tag] Msg, one labelled commit) and answers with the resulting tag
          count. [Tea_rpc.Mutating], so a cross-origin POST here is a 403 and
          never reaches the store. It does NOT push to live peers: they watch
          their own per-session branches, not the canonical one. See
          {!Shared_doc_serve.rpc_handler} for that limit and the concurrency
          one. *)

val name : ('req, 'resp) t -> Tea_rpc.Name.t
(** Total, wildcard-free; [Name.v "history_count"] / [Name.v "doc_stats"] /
    [Name.v "append_tag"]. *)

val req_t : ('req, 'resp) t -> 'req Repr.t
val resp_t : ('req, 'resp) t -> 'resp Repr.t
(** Total, wildcard-free: a new constructor without a codec is a compile error
    in this module, before either tier even builds. *)

val kind : ('req, 'resp) t -> Tea_rpc.endpoint_kind
(** Total, wildcard-free: {!Append_tag} is [Mutating]; the two query endpoints
    are [Read_only] and their handlers must stay that way (the server gates on
    this value alone). *)

type any = Any : ('req, 'resp) t -> any

val all : any list
(** Every endpoint exactly once; server routes are a fold over this list — no
    second name copy exists anywhere. *)
