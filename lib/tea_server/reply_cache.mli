(** The newest reply per RPC floor tab (roadmap step 15, D20.2).

    One entry per floor tab, because the client queue is one-in-flight over
    dense numbering: the newest seq is the only one a live client can still be
    waiting on, so anything older is {!Gone}. [Gone] is the degradation
    direction the whole family keeps, a visible {b duplicate-or-marker} rather
    than a forged reply or a silent loss: it becomes the typed [Replayed] arm,
    which the client surfaces as [Applied_reply_lost] ("the effect happened,
    its answer is gone"), never as success and never as a transport failure.

    An entry is a sum. {b Pending} while the [Fresh] window is open, so a
    duplicate that arrives mid-apply is answered a framework 503 and retried by
    the client runtime rather than being told anything about the effect;
    {b Settled} with the endpoint name and the encoded 200 body once the
    persist attempt has completed.

    In-process only, deliberately. A durable reply store would be a fourth
    restore-coherence artifact beside the store, the journal and the floors,
    which is R20 recursed, so it is refused here rather than half-built. The
    price is exactly the documented one: a restart in the retry gap answers
    [Replayed] instead of the original bytes. Conditional note for any future
    tier (G12): IF durable replies are ever added, the consumed marker and the
    reply bytes must land as ONE record, and that tier needs its own store
    water witness and boot filter, or it will answer a reply for an effect a
    rollback removed.

    Pure core plus a {!Cell} shell, the {!Replay_guard} pattern. No clock and
    no IO: recency is read off a monotone tick this module keeps itself, which
    makes the eviction order total and tie free, and the pending grace is a
    per-window poll budget, so neither couples dedup to wall time. *)

type t

(** What a duplicate's lookup found. Consumers match wildcard free, so a fourth
    outcome is a compile error at every decision site. *)
type found =
  | Busy
      (** The key's [Fresh] window is still open. The route answers a framework
          503, consumed by the client runtime's 5xx retry arm and never
          surfaced to [expect]: the request has been consumed but its effect is
          still in flight, and 503 is the one answer that says "ask again"
          without claiming anything about the effect (D20.2). *)
  | Original of string
      (** The settled bytes at exactly this seq and this endpoint, answered
          verbatim on the 200 channel. The retry sees the reply the taken
          delivery computed, never a recompute over later state. *)
  | Gone
      (** No usable entry: absent, an older seq, an endpoint mismatch, evicted
          by either bound, a body over the cap, or a {b Pending} whose poll
          budget is spent. The route answers the typed [Replayed] arm. *)

val v :
  ?entries:Replay_guard.Bound.t ->
  ?max_bytes:int ->
  ?body_cap:int ->
  ?pending_grace:int ->
  unit ->
  t
(** Defaults (D20.4, G14, G17): [entries] is {!Replay_guard.default_rpc_tabs}
    (4096, the live tab population the RPC guard's own Cell already sizes for);
    [max_bytes] is 16 MiB across all stored bodies, LRU evicted until under
    budget; [body_cap] is 64 KiB per body, its own knob, defaulted by analogy
    with the request cap but deliberately no longer coupled to it by
    construction; [pending_grace] is 64 [Busy] answers per window, a poll
    budget only the window's OWN tab spends: foreign traffic can never age a
    live window into a spurious [Replayed] (D20.2), and a polling client
    drains its own, so a wedged apply cannot pin its tab at 503 forever (G3).

    The byte budget exists because the two other bounds do not imply one: 4096
    entries at the body cap would be 256 MiB, which no default may imply. A non
    positive [max_bytes] or [body_cap] is not refused, it simply stores nothing
    and every retry reads [Gone], which is the visible direction. *)

val mark_pending : t -> tab:Tea_core.Prim.Tab_id.t -> seq:Tea_core.Prim.Msg_seq.t -> t
(** Open the [Fresh] window for [tab] at [seq], stamped with the cache's
    monotone tick and replacing whatever the tab held. Called synchronously in
    the same continuation that received the [Fresh] verdict, before any Lwt
    yield, so a concurrent duplicate can never observe a Fresh-taken key with
    no entry and read [Gone] where it should read [Busy] (D20.2). *)

val settle :
  t ->
  tab:Tea_core.Prim.Tab_id.t ->
  seq:Tea_core.Prim.Msg_seq.t ->
  endpoint:string ->
  body:string ->
  t
(** Replace the tab's entry with the settled bytes after the persist attempt.
    Newest wins, enforced: a settle carrying an older seq than the tab's live
    entry is discarded whole, so a non-conforming pipelining caller's stale
    settle can never destroy a newer window whose duplicate must keep reading
    [Busy]. A body over [body_cap] is NOT stored: the entry is
    dropped, so a retry degrades to [Replayed] rather than to a truncated or
    stale answer (G17). Evicts LRU past [entries], then until the total is
    under [max_bytes]; eviction of a {b Settled} entry and of a {b Pending} one
    both degrade to [Gone], never to a 503 the client would retry forever
    (G14). *)

val find :
  t ->
  tab:Tea_core.Prim.Tab_id.t ->
  seq:Tea_core.Prim.Msg_seq.t ->
  endpoint:string ->
  t * found
(** The duplicate arm's question. Returns the cache beside the answer, the
    {!Replay_guard.take} shape, because a hit touches recency: the entry a
    client is actively retrying is the one eviction must not take, and a pure
    core cannot record that in place.

    [Original] iff the tab's entry is {b Settled} at EXACTLY [seq] with EXACTLY
    [endpoint]. Storing the endpoint beside the body and matching it here is
    defense in depth against a rotation or queue bug (J3/J19): a mismatch
    degrades to [Gone], never to a reply from a different endpoint that would
    then fail to decode as this one's ['resp], or worse, succeed. It is NOT a
    key axis (G5): under dense one-in-flight numbering the seq already
    determines the endpoint, so this can only ever refuse.

    [Busy] iff the entry is {b Pending} at [seq] with poll budget left, and
    each [Busy] answer spends one unit of [pending_grace]. A drained window
    reads [Gone], so a handler promise that never settles cannot pin its tab
    at 503 forever (G3's liveness rider) - and only the tab's own polling
    drains a window, so foreign traffic can never turn a live one into a
    spurious [Replayed] (D20.2). *)

val size : t -> int
(** Live entries, {b Pending} and {b Settled} alike. Exists so the [entries]
    bound is a pinnable fact rather than an inference from [find]. *)

val bytes : t -> int
(** Stored body bytes, the quantity [max_bytes] bounds. *)

module Cell : sig
  (** One mutable cell per [once] value: the same synchronous shell
      {!Replay_guard.Cell} puts on its own core, so the take-to-settle sequence
      never has to thread a value through an Lwt bind and never has a yield
      between a verdict and the record of it. *)

  type cell

  val v :
    ?entries:Replay_guard.Bound.t ->
    ?max_bytes:int ->
    ?body_cap:int ->
    ?pending_grace:int ->
    unit ->
    cell

  val mark_pending :
    cell -> tab:Tea_core.Prim.Tab_id.t -> seq:Tea_core.Prim.Msg_seq.t -> unit

  val settle :
    cell ->
    tab:Tea_core.Prim.Tab_id.t ->
    seq:Tea_core.Prim.Msg_seq.t ->
    endpoint:string ->
    body:string ->
    unit

  val find :
    cell ->
    tab:Tea_core.Prim.Tab_id.t ->
    seq:Tea_core.Prim.Msg_seq.t ->
    endpoint:string ->
    found

  val snapshot : cell -> t
end
