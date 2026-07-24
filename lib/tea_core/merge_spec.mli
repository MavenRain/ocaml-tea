(** Pure, client-testable three-way merge policy for a model.

    The persistence layer lifts this into an [Irmin.Merge.t]:
    - [Last_write_wins] becomes [Irmin.Merge.default];
    - [Three_way f] becomes [Irmin.Merge.v model_t (fun ~old x y -> ...)].

    Keeping the policy pure here means merge behaviour can be property-tested
    (idempotence, commutativity, [merge ~ancestor:(Some a) ~ours:a ~theirs = theirs])
    without an Irmin repo in the loop. *)

type 'model t =
  | Last_write_wins
  | Three_way of
      (ancestor:'model option -> ours:'model -> theirs:'model -> ('model, string) result)
