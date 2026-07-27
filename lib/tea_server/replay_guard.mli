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

    Eviction and process restart can only ever produce a {b duplicate}, never a
    {b loss}, because the guard never invents a high water it did not observe:
    an absent entry accepts any sequence number. A false [Duplicate] costs a
    silent permanent edit; a false [Fresh] costs one visible, convergent double
    count. There is exactly one way to reach the loss side — a stale entry
    outliving its branch — and it is stated as a precondition on {!forget}.

    The guarantee, stated exactly: {b exactly-once effect within one server
    process lifetime and within this guard's bounds; at-least-once outside
    them.}

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

val forget : t -> replica:Tea_core.Crdt.Replica.t -> t
(** Drop every tab entry for one replica.

    {b Hard precondition for any future reaper loop.} If
    [Tea_persist.Store_core.CORE.reap] is ever wired into a server (today it is
    a library function nothing in [lib/] calls), it {b must} call this for each
    collected session. Otherwise: a laptop suspends past the ttl with a tab open
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

  val forget : cell -> replica:Tea_core.Crdt.Replica.t -> unit
  val snapshot : cell -> t
end
