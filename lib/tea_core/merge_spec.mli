(** Pure, client-testable three-way merge policy for a model.

    The persistence layer lifts this into an [Irmin.Merge.t]:
    - [Last_write_wins] becomes [Irmin.Merge.default];
    - [Three_way f] becomes [Irmin.Merge.v model_t (fun ~old x y -> ...)].

    Keeping the policy pure here means merge behaviour can be property-tested
    (idempotence, commutativity, [merge ~ancestor:(Some a) ~ours:a ~theirs = theirs])
    without an Irmin repo in the loop.

    [Crdt_join] (roadmap step 8, D1) is the state-based CRDT policy: a 2-way
    least-upper-bound over the whole model. It takes {b no} ancestor by
    construction — the base is unrepresentable, so a CvRDT merge can never
    consult it (the top vacuity trap: SEC laws pass in a pure harness while the
    live Irmin path secretly folds in the base and loses idempotence). The
    persistence layer lifts it as [Irmin.Merge.v model_t (fun ~old:_ ours theirs
    -> ok (join ours theirs))], discarding the base — valid precisely because
    [join] is idempotent, commutative and associative. *)

type 'model t =
  | Last_write_wins
  | Three_way of
      (ancestor:'model option -> ours:'model -> theirs:'model -> ('model, string) result)
  | Crdt_join of ('model -> 'model -> 'model)

(** Smart constructor for the CvRDT merge policy: register a model-wide [join].
    Its 2-argument shape is the point — there is no base parameter to consult. *)
val crdt_join : ('model -> 'model -> 'model) -> 'model t
