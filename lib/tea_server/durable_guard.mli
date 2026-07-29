(** The two-layer replay guard (roadmap step 11, D16): the bounded in-memory
    {!Replay_guard.Cell} in front of a durable floor mirror, joined by a
    {!Guard_sink}.

    Layering, exactly: the {b Cell} stays the sole synchronous verdict source
    — {!take} does not yield, so consume-before-apply and the
    two-sockets-one-tab exclusion remain structural, verbatim from step 10.
    The {b mirror} is what this process learned from the journal at open (plus
    every floor it has persisted since); a Cell miss is seeded from it before
    the verdict, so a high water can survive both a process restart and an
    in-memory LRU eviction. The {b sink} is the only asynchronous part, and
    {!persist} the only writer through it, called strictly after the apply
    attempt.

    The mem tier instantiates this over {!Guard_sink.null} with
    {!Floors.empty}, which is step 10's behaviour byte for byte: the mirror
    never has anything to say and the sink never fails. *)

(** The durable floors: per [(replica, tab)] high waters as a pure map,
    rebuilt from a journal by folding {!apply} in journal order. *)
module Floors : sig
  type t

  val empty : t

  val apply : t -> Guard_sink.event -> t
  (** One journal record: [Advance] raises the key's floor to the max of the
      two (never lowers — an out-of-order or replayed record must not re-open
      a consumed sequence number), [Forget] deletes every floor under the
      replica. {!of_events} is the left fold of this over a record list, and
      the file journal uses [apply] directly so there is exactly one floor-
      merge in the codebase.

      The store witness rides along (roadmap step 13): on an equal-or-higher
      seq the INCOMING record's water is adopted — last-writer-wins, matching
      journal append order, so the stamp always names the commit the newest
      record for that key rests on; a lower seq changes nothing. *)

  val of_events : Guard_sink.event list -> t

  val find :
    replica:Tea_core.Crdt.Replica.t ->
    tab:Tea_core.Prim.Tab_id.t ->
    t ->
    Tea_core.Prim.Msg_seq.t option

  val find_stamped :
    replica:Tea_core.Crdt.Replica.t ->
    tab:Tea_core.Prim.Tab_id.t ->
    t ->
    (Tea_core.Prim.Msg_seq.t * Tea_core.Prim.Store_water.t) option
  (** {!find} plus the floor's store witness: the water of the commit its
      newest journal record claims, [bottom] when it claims none (a no-op
      take, a pre-step-13 record). Compaction rewrites floors through this,
      so no journal rewrite can weaken — or forge — a witness. *)

  val cardinal : t -> int
  (** Total [(replica, tab)] floor count, for assertions. *)

  type verdict =
    { kept : int  (** floors that survived the boot filter, {!unwitnessed}
                      included; counted BEFORE any cap enforcement. *)
    ; dropped_behind : int
          (** a READABLE branch head stood strictly below the floor's claim —
              the store's bytes are older than the journal's, which is the
              R20 rollback. The only counter that may say so. *)
    ; dropped_no_branch : int
          (** no branch for the floor's replica, or one whose head reads
              [bottom] (collected, reaped, or never written). Routine after
              GC; never evidence of a rollback. *)
    ; unwitnessed : int
          (** kept floors whose water is [bottom]: a claim of nothing (a
              pre-step-13 record, a no-op take) is adopted on trust, and a
              boot that adopted any is NOT protected against a restored
              older pack root. Always [unwitnessed <= kept]. *)
    }
  (** What the boot filter did, one int per outcome, so a caller asserts the
      split instead of grepping logs. *)

  val filter :
    head:(Tea_core.Crdt.Replica.t -> Tea_core.Prim.Store_water.t option) ->
    t ->
    t * verdict
  (** Drop every floor whose branch head no longer {!Tea_core.Prim.Store_water.covers}
      its claimed water (R20: a floor over an effect the store does not have
      would judge its replay Duplicate — silent loss; refusing the floor
      re-admits the message as Fresh — a visible duplicate, the sanctioned
      direction). [head] answers a replica's branch head water, [None] when
      the branch does not exist; bottom-water floors are never dropped (a
      claim of nothing cannot be failed) but are counted, see {!verdict}. *)
end

type t

val v :
  sessions:Replay_guard.Bound.t ->
  tabs:Replay_guard.Bound.t ->
  sink:Guard_sink.t ->
  floors:Floors.t ->
  t

val take :
  t ->
  replica:Tea_core.Crdt.Replica.t ->
  tab:Tea_core.Prim.Tab_id.t ->
  seq:Tea_core.Prim.Msg_seq.t ->
  Replay_guard.verdict
(** Synchronous, like the Cell it fronts. A Cell miss (no in-memory high water
    for the key) is first seeded from the mirror, so the first frame a
    restarted process hears reads its verdict against the durable floor, not
    against an empty table; then the Cell takes as in step 10. [Gapped]
    performs no Cell write, exactly as before. *)

val persist :
  t ->
  replica:Tea_core.Crdt.Replica.t ->
  tab:Tea_core.Prim.Tab_id.t ->
  seq:Tea_core.Prim.Msg_seq.t ->
  water:Tea_core.Prim.Store_water.t ->
  (unit, Guard_sink.err) result Lwt.t
(** Record "this sequence number was taken" durably: raise the mirror first
    (so an eviction between the append and the next frame still re-seeds at
    the true water), then append [Advance] through the sink. Called only
    after a [Fresh] verdict's apply attempt completed — on {i both} outcomes,
    success and fuel exhaustion, because the high water means "taken", never
    "applied" — and never on [Duplicate] or [Gapped]. An [Error] means the
    record is not durable: the caller degrades toward the duplicate
    direction, never the loss one.

    [water] is what {!Store_core.CORE.commit} returned for this take's
    commit, or {!Tea_core.Prim.Store_water.bottom} when the take minted none
    (a no-op update, fuel exhaustion) — the commit's own date, never a value
    read back off a head. *)

val forget :
  t -> replica:Tea_core.Crdt.Replica.t -> (unit, Guard_sink.err) result Lwt.t
(** Tombstone-first: append [Forget] through the sink, {i then} scrub the
    mirror and the Cell — the order that keeps a crash between the two steps
    on the duplicate side. The in-memory state is scrubbed even when the
    append fails: the caller is removing the branch either way, and a stale
    in-memory floor over a fresh branch is the loss path
    ({!Replay_guard.forget}'s precondition). *)

val snapshot : t -> Replay_guard.t
(** The in-memory table, for assertions. *)

val floors : t -> Floors.t
(** The current mirror, for assertions. *)
