(** Per-key parking of duplicate acknowledgements on the in-flight take-to-ack
    attempt (roadmap step 17; closes D21's recorded F5' loss window).

    The shape it closes: a wrapper cancellation orphans a [Fresh] attempt
    (D21), the client reconnects and replays the same seq while that orphan is
    still in flight, and the pump's [Duplicate] arm used to acknowledge
    immediately - an assertion of "consumed" that rides on the orphan's
    success. An orphan that then rejects releases the seq, but the client
    already dropped the message from its outbox on that ack: the effect never
    lands and no retransmission is coming. The registry makes the duplicate
    ack wait for the attempt's own verdict instead: {!Landed} lets the ack
    flow; {!Released} drops it, so the client's retry reads [Fresh] after the
    release and re-applies. The keyed HTTP tier already answers this window
    with [Reply_cache]'s {b Busy} 503 ("ask again", D20.2); this is the WS
    tier's analogue, shaped for a push channel that can defer an ack rather
    than answer a status.

    In-process only, deliberately (the [Reply_cache] reasoning): a process
    death loses the registry and every in-flight attempt together, and the
    durable floor then adjudicates the replay on its own. No bound and no
    eviction of its own: a row lives as long as its attempt, because every
    exit from the protected span - persist success, the fuel-exhaustion
    bottom floor, or the rejection catch - settles it (D21's three-case
    closure), so the table's size is bounded by concurrently in-flight spans,
    not by message volume. One path ends a row early: supersession. The
    replay guard's tab-LRU eviction may re-open a consumed seq while its
    first attempt is still in flight (an eviction is licensed to duplicate,
    never to lose), and the re-opened seq's own {!register} then replaces the
    standing row - see {!handle}. *)

type t

(** The attempt's verdict, from a parked waiter's point of view. Consumers
    match wildcard free, so a third outcome is a compile error at every
    decision site. *)
type outcome =
  | Landed
      (** The attempt persisted its floor: the effect is real, and a parked
          ack may assert "consumed" truthfully. *)
  | Released
      (** The attempt did not land its effect: either its rejection released
          the seq, or it exhausted its fuel (a path that never minted an ack
          to begin with), or its row was superseded by a later attempt on
          the same key. A parked ack must NOT fire. On the released side
          the client's retry reads [Fresh] and re-applies; on the fuel side
          the next plain replay finds no row and is acknowledged
          unconditionally, which is the once-ever poison contract
          unchanged; on the superseded side the retransmission parks on the
          new attempt's row and takes ITS verdict. *)

type handle
(** The capability to settle exactly one row: {!register} returns the row it
    opened, and {!settle} acts only when the standing row is that row. The
    gate is what keeps a superseded attempt's late settle away from the row
    that replaced it - without it, the first attempt's verdict would
    cross-wire onto the second attempt's waiters. *)

val create : unit -> t

val register :
  t
  -> replica:Tea_core.Crdt.Replica.t
  -> tab:Tea_core.Prim.Tab_id.t
  -> seq:Tea_core.Prim.Msg_seq.t
  -> handle
(** Open a row for a just-taken [Fresh] seq and return its settle
    capability. Synchronous, and called in the same continuation that
    received the [Fresh] verdict, before any Lwt yield point - the
    discipline [Cell.take]'s consume-before-apply already imposes - so a
    duplicate that arrives after the take can never miss the row. A
    standing row at the key is superseded, not kept: the new row is
    installed FIRST, then the old row's waiters are woken [Released] - they
    drop their ack, their clients retransmit, and the retransmission parks
    on the new attempt's row and takes the new attempt's verdict. The
    install-then-wake order is load-bearing: a waiter woken at callback
    depth zero runs inline and may re-enter the registry, so it must
    observe the new row already installed, and no write staged from a
    pre-wake snapshot may follow the wake. *)

val find :
  t
  -> replica:Tea_core.Crdt.Replica.t
  -> tab:Tea_core.Prim.Tab_id.t
  -> seq:Tea_core.Prim.Msg_seq.t
  -> outcome Lwt.t option
(** The settlement promise of the in-flight attempt at exactly this key, or
    [None] when no attempt is in flight - the caller then keeps the
    immediate-ack fast path (the attempt settled long ago, or never existed:
    an ordinary stale replay). The promise is shared by every waiter on the
    key and is minted by [Lwt.wait], so it is not cancelable: a parked
    socket dying cannot reject the settlement its sibling waiters share (the
    [died]/[mark_died] reasoning). A caller whose own teardown must stay
    prompt wraps its wait in [Lwt.protected]. *)

val settle :
  t
  -> replica:Tea_core.Crdt.Replica.t
  -> tab:Tea_core.Prim.Tab_id.t
  -> seq:Tea_core.Prim.Msg_seq.t
  -> handle:handle
  -> outcome:outcome
  -> unit
(** Wake every waiter with the attempt's verdict and remove the row - iff
    the standing row is the one [handle] names. A no-op when the key holds
    no row (an attempt no duplicate ever raced still settles - fine) and
    when the row was superseded (its waiters were already woken [Released]
    at replacement; the late settle must not touch its successor). The
    wake-at-most-once obligation on [wakeup_later] is structural: a row is
    woken either by its own settle, which removes it in the same synchronous
    step strictly BEFORE the wake, or by the {!register} that superseded it,
    which installed the replacement before its wake - and the handle gate
    keeps either wake from reaching the other's row. *)

val parked_count :
  t -> replica:Tea_core.Crdt.Replica.t -> tab:Tea_core.Prim.Tab_id.t -> int
(** How many seqs currently hold an open row under this tab. Test-only
    introspection (the [Reply_cache.size] precedent): production dispatch
    never reads it. *)
