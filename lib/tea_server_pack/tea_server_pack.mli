(** The durable pack-backed Dream tier (roadmap step 8, D2): the mem tier's
    handler bodies over {!Tea_persist_pack.Store_pack}, plus an Origin-gated
    [POST /admin/checkpoint]. Kept in its own library so irmin-pack stays out
    of [tea_server]'s (and the js_of_ocaml client's) dependency closure. *)

module Root = Tea_persist_pack.Store_pack.Root

module Make_pack (A : Tea_core.App.APP) : sig
  module Store : module type of Tea_persist_pack.Store_pack.Make (A)

  val handle_checkpoint :
    ?retention:Store.Retention.t -> Store.t -> Dream.request -> Dream.response Lwt.t
  (** [POST /admin/checkpoint] handler: same-origin only (a
      {!Tea_safe.Origin_gate.denial} answers 403), otherwise squash [main],
      link the checkpoint onto the retention spine, and GC to the anchor. *)

  val handler_pack :
    ?client_dir:string ->
    ?rpc:Dream.route list ->
    ?coalesce:A.msg Tea_core.Coalesce_spec.t ->
    ?retention:Store.Retention.t ->
    Store.t ->
    Dream.handler
  (** The mem tier's request pipeline with [POST /admin/checkpoint] folded into
      the RPC route list. Exposed so tests can drive it with [Dream.test]. *)

  val serve_pack :
    ?interface:string ->
    ?port:int ->
    ?client_dir:string ->
    ?rpc:Dream.route list ->
    ?coalesce:A.msg Tea_core.Coalesce_spec.t ->
    ?retention:Store.Retention.t ->
    ?lower_root:string ->
    root:Root.t ->
    unit ->
    unit
  (** Blocking entry point: open (or initialise) the pack store at [root],
      serve until SIGINT, then close the repo so the pack suffix is flushed. *)
end
