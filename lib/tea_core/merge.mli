(** Composable, sound three-way merge combinators - the R2 mitigation.

    A ['a t] is a pure three-way merge over a value of type ['a]: given the
    common [ancestor] (when one exists) and the two diverged sides [ours] and
    [theirs], it returns the reconciled value or a conflict message. It is
    exactly the payload carried by {!Merge_spec.Three_way}, so {!to_spec} drops
    a combinator straight into an [APP]'s [merge].

    The {i automatic} combinators ([atomic], [counter], [set], [record]) are
    built to satisfy the merge laws, checked in [test/merge_test]:
    - {b reflexivity}: [run m ~ancestor:(Some x) ~ours:x ~theirs:x = Ok x];
    - {b identity}: a side that did not change yields the other side unchanged
      ([run m ~ancestor:(Some a) ~ours:a ~theirs:b = Ok b], and symmetrically);
    - {b commutativity}: swapping [ours] and [theirs] gives the same result.

    Two provisos: [conflict] is the deliberate {i non}-automatic default and
    obeys none of these (it always fails); and for [set] the equality in the
    laws is the [cmp]-induced set equivalence, not structural list equality (the
    result is always normalised, so an already-normalised input round-trips
    exactly).

    The house policy is to {b never guess}. {!conflict} surfaces every
    concurrent touch, and {!atomic} conflicts whenever both sides moved a value
    to different places. Structure is what makes a merge automatic instead:
    {!counter} sums deltas, {!set} unions membership, {!record} reconciles a
    record field by field. Only a genuinely divergent free-form edit reaches
    the user as a conflict (a 409 + reconcile Msg), which is the whole point of
    R2 - a last-writer-wins default silently drops one of two concurrent
    edits; these combinators do not. *)

type 'a t

val run : 'a t -> ancestor:'a option -> ours:'a -> theirs:'a -> ('a, string) result
(** Reconcile two diverged values against their common ancestor (if any). *)

val to_spec : 'a t -> 'a Merge_spec.t
(** Lift a combinator into an [APP.merge] policy (always a [Three_way]). *)

(** {2 Leaf combinators} *)

val conflict : string -> 'a t
(** Always a conflict when both branches touched the path - the safest policy,
    since it never silently drops a concurrent edit. Use it for a value with no
    sound automatic reconciliation. *)

val atomic : eq:('a -> 'a -> bool) -> 'a t
(** Merge a value as one indivisible unit: whichever side changed from the
    ancestor wins, and if both sides changed it (to different values) that is a
    conflict. With no common ancestor it is agree-or-conflict. This is Irmin's
    [Merge.default], made total over the ancestor option. *)

val counter : int t
(** Additive reconciliation: [ours + theirs - ancestor], i.e. apply both sides'
    deltas to the shared base. Never conflicts. With no common ancestor the
    additive identity [0] is the base, so the two counts simply add. Assumes
    values stay within native [int] range; it does not guard against overflow
    (a like/vote counter never approaches it). Compose {!map} to bound the
    result (e.g. [map (max 0) counter] for a non-negative counter). *)

val set : cmp:('a -> 'a -> int) -> 'a list t
(** Three-way set merge over a list treated as a set (normalised by [cmp], so
    the result is sorted and duplicate-free): an element added on either side
    is kept; an element removed on either side {i relative to the ancestor} is
    dropped (remove-wins over an untouched keep). Never conflicts.

    [cmp] must be a total order under which [cmp]-equal elements are
    interchangeable (structural set membership). If two [cmp]-equal elements can
    carry {i different} data - a compare-by-key order over records with editable
    payloads - [set] keeps one of them arbitrarily (chosen by argument order,
    so commutativity would not hold) and is the wrong tool; reconcile such
    elements with a keyed, per-element merge instead. *)

val map : ('a -> 'a) -> 'a t -> 'a t
(** Post-process a merger's reconciled value with [f]. Commutativity is always
    preserved; reflexivity and identity survive only when [f] fixes the values
    in the combinator's valid domain (e.g. an idempotent clamp like [max 0] on
    a counter whose values are already non-negative). *)

(** {2 Records} *)

type 'r field

val field
  :  label:string
  -> get:('r -> 'a)
  -> set:('a -> 'r -> 'r)
  -> 'a t
  -> 'r field
(** One field of a {!record} merge: how to read it ([get]), how to write the
    reconciled value back ([set]), and the combinator that reconciles it.
    [label] names the field in a conflict message. *)

val record : 'r field list -> 'r t
(** Merge a record field by field: each field reconciles independently, and the
    first field to conflict fails the whole record (its [label] leads the
    message). {b Every reconcilable field must be listed} - a field left out is
    taken from [ours] unchanged, which is a silent last-writer-wins and exactly
    the R2 hazard this library exists to avoid. *)
