(** The application definition: the OCaml mirror of lean-tea's [WebApp] record.

    One [APP] is compiled to native (server) and to JavaScript (client). The
    [Repr.t] reprs ([model_t], [msg_t]) do triple duty: JSON wire codec (both
    tiers), Irmin [Contents] description (server store), and pretty-printer for
    commit messages / audit labels. [Repr] is [Irmin.Type]. *)

module type APP = sig
  type model
  type msg

  val model_t : model Repr.t
  val msg_t : msg Repr.t
  val init : model * msg Cmd.t
  val update : msg -> model -> model * msg Cmd.t
  val view : model -> msg Html.t
  val subscriptions : model -> msg Sub.t

  (** How two concurrently-diverged models on different session branches
      reconcile; lifted to [Irmin.Merge] by the persistence layer. *)
  val merge : model Merge_spec.t

  val title : Prim.Title.t

  (** lean-tea [viewToUrl]: derive a shareable URL from the model. *)
  val url_of_model : model -> Prim.Url.t option

  (** lean-tea [urlToMsg]: turn an incoming URL into an initial message. *)
  val msg_of_url : Prim.Url.t -> msg option
end
