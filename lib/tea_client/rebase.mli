(** Surviving a disconnection without losing or clobbering edits (roadmap step
    8, D9). Two halves of one story, and neither is enough alone.

    {2 Outbox: the edits made while the link was down}

    Before D9 a locally-born message met a socket that was not [Open] and was
    logged as "applied locally only": it changed the tab's model, the server
    never saw it, and the next pushed head quietly reverted it. {!Outbox} holds
    those messages until {!Reconnect} reaches [Up] and replays them in the
    order they were made.

    Replay is send-once. A buffered message is by construction one the server
    never received, so re-sending it cannot double-apply.

    That property is carried by send-once {b alone}. An earlier version of this
    doc added "and even if the classification were wrong, every replicated
    field is a CRDT (D1), so a re-delivered edit joins idempotently rather than
    counting twice" - D14 falsified exactly that sentence. State {i join} is
    idempotent; applying an {i operation} twice is not, whatever the merge
    does, because the second [inc] moves the replica's own slot again. Sharing
    a replica id with the server ({!absorb}) fixes the {i one} case where two
    applies are one intent (the client's prediction and the server's
    authoritative apply, which now land in the same slot and are reconciled by
    [max]); it buys nothing at all against a genuinely duplicated delivery. If
    a delivery-dedup story is ever wanted, it needs the seq-number acks §7
    records as [deferred_beyond], not the CRDT layer.

    {2 Rebase: the head that arrives on top of them}

    A pushed head reflects the server's state, which does {i not} include the
    edits still sitting in the outbox (nor an optimistic edit still in flight).
    Handing that head straight to the app's store-watch subscription is a
    clobber. {!reconcile} runs the app's own {!Tea_core.Merge_spec.t} first, so
    what the subscription sees is the head {i joined with} local state.

    Under [Crdt_join] this is exactly right and needs no ancestor: the join is
    idempotent, commutative and associative, so replaying a local edit onto a
    newer head converges however the two interleave. *)

module Outbox : sig
  (** Messages awaiting a live socket, newest first. [private] so the internal
      order can never be mistaken for send order - {!drain} is the only way
      out, and it is the only thing that knows to reverse. *)
  type 'msg t = private 'msg list

  val empty : 'msg t

  (** Prepend: O(1) per buffered message, which matters because this runs on
      every keystroke of an offline editing session. *)
  val buffer : 'msg -> 'msg t -> 'msg t

  (** [drain o] is the buffered messages in the order they were made (FIFO),
      paired with the emptied outbox. Returning the new outbox rather than
      mutating in place keeps the apply-once discipline checkable: a caller
      that forgets to store it has an outbox that still looks full. *)
  val drain : 'msg t -> 'msg list * 'msg t

  val pending : 'msg t -> int
  val is_empty : 'msg t -> bool
end

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
  'model * Tea_core.Crdt.Replica.t option
