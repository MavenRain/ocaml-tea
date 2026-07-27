(** Surviving a disconnection without losing or clobbering edits (roadmap step
    8, D9). Two halves of one story, and neither is enough alone.

    {2 Outbox: replaced, in step 10}

    D9's [Outbox] held the messages born while the link was down and replayed
    them on reconnect. Its membership rule — "born while the link was down" —
    left a hole it could not see: a message handed to a socket that then died
    was in no outbox at all, so nothing replayed it. That hole is closed by
    {!Tea_client.Delivery}, which holds every shared message until the server
    acknowledges it, and this module no longer owns a queue.

    The safety argument moved with it. D9's replay was safe by {i send-once}: a
    buffered message was by construction one the server never received. A
    retried message is not, so send-once is gone as an argument, and what makes
    a retry safe now is the server's own de-duplication above [A.update]
    ([Tea_server.Replay_guard]). The sentence D14 falsified — that a
    re-delivered edit "joins idempotently rather than counting twice" — stays
    false: state {i join} is idempotent, applying an {i operation} twice is
    not, whatever the merge does. Sharing a replica id ({!absorb}) fixed the
    {i one} case where two applies are one intent; it buys nothing against a
    genuinely duplicated delivery, and never claimed to.

    {2 Rebase: the head that arrives on top of them}

    A pushed head reflects the server's state, which does {i not} include the
    edits still sitting in the outbox (nor an optimistic edit still in flight).
    Handing that head straight to the app's store-watch subscription is a
    clobber. {!reconcile} runs the app's own {!Tea_core.Merge_spec.t} first, so
    what the subscription sees is the head {i joined with} local state.

    Under [Crdt_join] this is exactly right and needs no ancestor: the join is
    idempotent, commutative and associative, so replaying a local edit onto a
    newer head converges however the two interleave. *)

(** [reconcile policy ~local ~incoming] folds a pushed store head into the
    model this tab is holding, under the app's own merge policy:

    - [Crdt_join join] is [join local incoming] - the CvRDT least upper bound,
      no ancestor, no loss;
    - [Three_way f] is [f ~ancestor:None ~ours:local ~theirs:incoming], and a
      conflict falls back to [incoming]: the server head is the authority (R6),
      so a policy that cannot reconcile yields to it rather than stranding the
      tab on a divergent local state;
    - [Last_write_wins] is [incoming] unchanged, which is that same authority
      rule stated directly - the honest reading of a policy that has no way to
      combine two states.

    Only the [Crdt_join] case genuinely preserves an outbox edit. That is not a
    limitation of this function but of the policy: a last-write-wins app has no
    operation that could keep both sides, and pretending otherwise would be the
    silent divergence D9 exists to remove. *)
val reconcile :
  'model Tea_core.Merge_spec.t -> local:'model -> incoming:'model -> 'model

(** What a down-frame turns into. A {b sum}, not a record: an [Ack] carries no
    model at all, and a shape that handed one back would have the runtime
    dispatch a spurious store-watch message into the app on every
    acknowledgement — for a [Crdt_join] app a join with itself, for an app
    whose sync handler calls [Lww.set] a fresh dot per ack. The mistake is a
    type error here instead of a review obligation there. *)
type 'model absorbed =
  | Resync of Tea_core.Crdt.Replica.t * 'model
      (** [Hello]: adopt the announced identity and take its head as-is. *)
  | Rebased of 'model
      (** [Head]: the app's own {!Tea_core.Merge_spec.t} run first, so what the
          subscription sees is the head {i joined with} local state. *)
  | Acked of Tea_core.Prim.Msg_seq.t
      (** [Ack]: no model changes; the delivery queue drops every message at or
          below this sequence number (roadmap step 10, D15). *)

(** [absorb policy ~local down] is what a live-socket down-frame does to this
    tab: the model the store-watch subscription should be handed, and the
    replica id to adopt if the frame announced one (roadmap step 9, D14).

    It is the whole boot-order decision, as one total function the runtime only
    executes:

    - {!Tea_core.Wire.Hello} - a (re)connect. Adopt the announced replica, and
      resync to the head it carries {i without} folding [local] in. Nothing is
      lost by that: an edit the server has not applied is either in the outbox
      (replayed right after) or in flight, and both come back as heads. What
      {i is} shed is a prediction made under the old identity, which is the
      point - after a reconnect this tab may be applying under a replica id it
      did not have before.
    - {!Tea_core.Wire.Head} - a commit on the session's branch, reconciled onto
      the local model exactly as before ({!reconcile}). [local] is [None] only
      before the app has mounted, where there is nothing to reconcile against.

    Note what this does {i not} do: it cannot erase dots the app already minted
    under the provisional identity, because the subscription hands the result
    to the app's own update, which joins. A CRDT state has no general
    "un-mint" ({!Tea_core.Crdt.Lww} could not implement one - it has no
    previous value to revert to), so the framework does not pretend to offer
    one. Edits made before the first [Hello] therefore keep a provisional slot
    for the life of the page; edits after it - which is every edit on a page
    that reached its server - are counted exactly once. *)
val absorb :
  'model Tea_core.Merge_spec.t ->
  local:'model option ->
  'model Tea_core.Wire.down ->
  'model absorbed
