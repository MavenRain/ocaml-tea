(** The server's replay guard (roadmap step 10, D15): one integer per sending
    tab, consulted {i above} [A.update].

    {2 What it is for}

    A live-socket message is delivered at least once, because the client now
    keeps every message until the server acknowledges it and replays whatever is
    left on reconnect. That is what stops a message handed to a dying socket
    from vanishing — but it means the server can be handed the same message
    twice, and applying a message twice is not harmless: state {i join} is
    idempotent, replaying an {i operation} is not, so a second [Pn_counter.inc]
    moves the replica's own slot again (§7, D14). This module is what turns
    at-least-once delivery into an exactly-once {i effect}: a message that was
    already consumed is acknowledged again and not applied.

    {2 Why one integer}

    A WebSocket delivers in order, so a dead socket truncates a {i suffix} of a
    tab's messages, never a middle; the client assigns sequence numbers in edit
    order and replays from its unacknowledged frontier. The set of sequence
    numbers consumed for a tab is therefore always a prefix, and a prefix is one
    number. The table is bounded by {i tab count}, never by message count, and a
    frame costs one lookup.

    {2 Why the key is (replica, tab) and not the session alone}

    One Dream session cookie is one session, one branch and one
    {!Tea_core.Crdt.Replica} — [test/browser/smoke.mjs] drives two tabs on
    exactly that configuration. Both tabs start at {!Tea_core.Prim.Msg_seq.one}.
    A session-keyed guard would answer [Duplicate] to the second tab's first
    edit and drop it forever: a false [Duplicate] is a silent permanent loss,
    i.e. the very bug this module exists to close, reintroduced by its own fix.
    The replica is asked from the session's own [ctx_of_session], the same
    single derivation the [Hello] announcement uses, so the key and the
    announcement cannot drift.

    {2 The direction every degradation falls}

    Forgetting an entry, by eviction or by process restart, can only ever
    produce a {b duplicate}, because an absent entry accepts any sequence
    number: the guard forgets toward acceptance. A false [Duplicate] costs a
    silent permanent edit; a false [Fresh] costs one visible, convergent double
    count. The loss side is reached only by {i remembering too much}: a high
    water outliving the effect it records. Within this module's own state there
    is exactly one such path, a stale entry outliving its branch, and it is
    stated as a precondition on {!forget}; the durable wiring adds a second,
    stated below.

    The guarantee this pure module states for itself: {b exactly-once effect
    within one server process lifetime and within this guard's bounds;
    at-least-once outside them.} On the mem tier that is the whole story. The
    durable wiring ({!Durable_guard} over {!Guard_sink}, journaled on the pack
    tier by [Tea_server_pack.Guard_file]; roadmap step 11, D16) extends the
    lifetime bound across an {i orderly} restart: a SIGINT/SIGTERM teardown
    closes the store and then the journal, and the read-back floors re-enter
    this table through {!seed}. It does not extend it across a hard kill. A
    [kill -9] can leave a journal floor more durable than the commit whose
    effect it records (the journal reaches the page cache per record,
    irmin-pack buffers commits in user space), and a seed adopting such a
    floor answers [Duplicate] against an effect that is gone: the
    unacknowledged in-flight tail is then silently lost, bounded by the pack's
    auto-flush lag. That is the second loss-side path, opened by durability
    itself, stated as out of scope on [Tea_server_pack.Guard_file] rather than
    closed by this module.

    {2 Why the bound is defensible}

    A hostile client gains nothing from evicting entries: it can already
    double-apply by sending the same edit under two fresh tab ids. This guard
    exists to suppress {i the framework's own retries}, not to police a peer. So
    it need only be at least as large as an honest client's unacknowledged
    window, and eviction under attack costs the attacker nothing it did not
    already have.

    Pure: no clock, no IO, no mutation. Recency is a monotone tick this module
    keeps itself, which makes the eviction order total and tie-free without a
    clock parameter. *)

module Bound : sig
  (** A table capacity: strictly positive, so an "empty" guard that rejects
      everything is not expressible. *)
  type t

  val of_int : int -> t option
  (** [None] for [n <= 0]. *)

  val to_int : t -> int
end

val default_sessions : Bound.t
(** 4096 replicas. The session id is cookie-derived and an attacker mints
    cookies freely, so the outer level is capped too. *)

val default_tabs : Bound.t
(** 8 tabs per replica. A browser does not usefully hold more live tabs on one
    cookie, and the cap is per replica so one session cannot starve another. *)

val default_rpc_replicas : Bound.t
(** 1, for the RPC channel's guard (roadmap step 15). The single-branch
    contract means there is one canonical replica per [once] value, so the
    outer level of that guard is not a population at all; a multi-canonical
    branch app passes its own bound (R26). *)

val default_rpc_tabs : Bound.t
(** 4096, for the RPC channel's guard: one floor tab per live client page, all
    of them under the one canonical replica. {!default_tabs}'s 8-per-replica is
    per cookie and would let a ninth page evict a live one permanently once the
    whole population shares a replica, so the RPC channel sizes its inner level
    the way the WS channel sizes its outer one. *)

type t

val v : sessions:Bound.t -> tabs:Bound.t -> t

type verdict =
  | Fresh of Tea_core.Prim.Msg_seq.t
      (** Not consumed before: apply it, then [Ack] the carried high water. *)
  | Duplicate of Tea_core.Prim.Msg_seq.t
      (** Already consumed: do {b not} apply it, but [Ack] the carried high
          water anyway — an unacknowledged duplicate is a replay loop that never
          terminates. *)
  | Gapped
      (** A sequence number beyond [high_water + 1]. Do not apply, do not
          advance, do not acknowledge, and do {b not} end the session: ending it
          would hand a same-session tab a socket-kill primitive against its
          sibling. Under the prefix invariant an honest client cannot produce
          this, so refusing it costs nothing and makes the invariant
          self-enforcing rather than assumed — in particular a forged high
          sequence number cannot poison a tab's future, because the high water
          advances by exactly one or not at all. A gapped frame leaves the table
          {i entirely} unchanged, recency included, so a forged frame cannot
          keep an entry warm at another tab's expense either. *)

val take :
  t ->
  replica:Tea_core.Crdt.Replica.t ->
  tab:Tea_core.Prim.Tab_id.t ->
  seq:Tea_core.Prim.Msg_seq.t ->
  t * verdict
(** Consume [seq] for [(replica, tab)]. An absent entry accepts any sequence
    number (that is what lets an honest tab resume after an eviction). Touches
    recency on a [Fresh] and on a [Duplicate], so a tab that is actively
    retrying — the only tab whose entry matters — is by construction the last to
    be evicted. *)

val seed :
  t ->
  replica:Tea_core.Crdt.Replica.t ->
  tab:Tea_core.Prim.Tab_id.t ->
  high:Tea_core.Prim.Msg_seq.t ->
  t
(** Adopt a high water this process did not observe — the durable record read
    back after a restart (roadmap step 11, D16).

    {b It raises the floor and never lowers it}, the {!Tea_persist.Clock.seed}
    discipline exactly: an in-memory entry is always at least as advanced as the
    durable one, because the durable write happens after the effect commits, so
    a seed that could lower an entry is reading something older than what this
    process already knows and must be ignored rather than believed. Lowering
    would re-open a consumed sequence number, i.e. re-apply an operation, which
    is the one thing this module exists to prevent.

    Seeding touches recency, because the only reason to seed is that a frame for
    [(replica, tab)] just arrived. *)

val release :
  t ->
  replica:Tea_core.Crdt.Replica.t ->
  tab:Tea_core.Prim.Tab_id.t ->
  seq:Tea_core.Prim.Msg_seq.t ->
  t
(** Un-take one [Fresh] verdict whose apply REJECTED before anything was
    persisted (roadmap step 15, D20.2's barrier; R27), so the retry reads
    [Fresh] and re-runs the apply instead of draining through the reply cache
    into a [Replayed] for an effect that never happened.

    Conditional by construction: it restores the pre-take high water only when
    the entry's high is still exactly [seq] - the one shape a failed take can
    have left - and is a no-op otherwise (a later take moved the water, or the
    entry was evicted, which already reads as released). Re-opening [seq] can
    only ever cause a visible duplicate, never a loss: a half-applied handler
    re-applies convergently, the one degradation direction the family keeps. *)

val forget : t -> replica:Tea_core.Crdt.Replica.t -> t
(** Drop every tab entry for one replica.

    {b Hard precondition for any reaper loop.}
    [Tea_persist.Store_core.CORE.reap] is wired into both entry points behind
    [?reaper] since step 22, and each wiring satisfies this by passing a
    [?forget] built on {!Durable_guard.forget}
    ([Tea_server_pack.forget_into], and {!Make.serve}'s local twin); any new
    sweep wiring {b must} do the same for each collected session. Otherwise: a laptop suspends past the ttl with a tab open
    and a non-empty unacknowledged queue, the branch is deleted, the reconnect
    ladder comes back, [session] recreates the branch at [A.init], and every
    replay reads [Duplicate] against a stale high water — total silent loss onto
    an empty model. That is the only path to the loss side of this module's
    degradation, so it is closed by contract rather than by hope. *)

val high_water :
  t ->
  replica:Tea_core.Crdt.Replica.t ->
  tab:Tea_core.Prim.Tab_id.t ->
  Tea_core.Prim.Msg_seq.t option
(** The last sequence number consumed for [(replica, tab)], for assertions. *)

val sessions : t -> int
(** Live replica count, for assertions about the bound. *)

val tabs : t -> replica:Tea_core.Crdt.Replica.t -> int
(** Live tab count under one replica, for assertions about the bound. *)

module Cell : sig
  (** The thin effectful shell the pump holds — the same pure-core /
      mutable-shell split {!Tea_client.Rebase} and the client runtime use. The
      new table is committed to the reference {i before} the verdict is
      returned, and {!take} is synchronous, so "consume before apply" is
      structural: there is no Lwt yield point between deciding a sequence number
      is fresh and recording it, and two live sockets for one tab therefore
      cannot both see it as fresh. *)
  type cell

  val v : sessions:Bound.t -> tabs:Bound.t -> cell

  val take :
    cell ->
    replica:Tea_core.Crdt.Replica.t ->
    tab:Tea_core.Prim.Tab_id.t ->
    seq:Tea_core.Prim.Msg_seq.t ->
    verdict

  val seed :
    cell ->
    replica:Tea_core.Crdt.Replica.t ->
    tab:Tea_core.Prim.Tab_id.t ->
    high:Tea_core.Prim.Msg_seq.t ->
    unit
  (** {!seed} on the shell. The pump calls this {i before} {!take}, on the frame
      that first mentions a [(replica, tab)] this process has not heard from, so
      the durable read is the only step that yields and {!take} remains the one
      synchronous decision point. Two sockets that both seed before either takes
      is harmless: seeding cannot lower an entry, and [take] still admits
      exactly one of them. *)

  val release :
    cell ->
    replica:Tea_core.Crdt.Replica.t ->
    tab:Tea_core.Prim.Tab_id.t ->
    seq:Tea_core.Prim.Msg_seq.t ->
    unit
  (** {!release} on the shell, synchronous like {!take}: the failure arm calls
      it in the same continuation that observed the rejection, so there is no
      yield between deciding the apply failed and re-opening its seq. *)

  val forget : cell -> replica:Tea_core.Crdt.Replica.t -> unit
  val snapshot : cell -> t
end
