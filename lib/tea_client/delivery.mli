(** The client's unacknowledged queue (roadmap step 10, D15). It {b replaces}
    [Rebase.Outbox], and the replacement is the point.

    [Outbox]'s membership rule was "born while the link was down". That rule
    {i was} the lost-edit bug: a message handed to a socket that then died was
    never in it, so nothing replayed it, and the following [Hello] resync
    discarded the prediction with no error and no trace. Here every shared
    message is recorded when it is made and leaves only when the server
    acknowledges its sequence number, so "born offline" and "in flight" stop
    being different cases — one queue, one FIFO order, one drain trigger.

    Keeping both would have been worse than keeping neither: the flush fires
    from the socket's open event while an unacknowledged replay naturally
    belongs to the reconnect, so two queues means offline-born messages reach
    the server {i ahead} of older already-sent ones. Under [Crdt_join] that
    converges; under [Last_write_wins] it flips the winner.

    Retrying is only safe because the server de-duplicates above [A.update]
    (see [Tea_server.Replay_guard]): replaying an {i operation} is not
    idempotent even where state {i join} is (§7, D14). The two halves are one
    mechanism and neither is sound alone.

    {2 What is not bounded}

    The queue is unbounded, exactly as [Outbox] was. A client that edits
    forever without ever reaching its server grows it forever. This is a named
    residual rather than an oversight: the alternative is dropping the oldest
    entry, which is the silent loss this step exists to remove, and it is also
    what lets the server refuse a gap outright — an honest client never skips a
    sequence number, because it never discards one. *)

type 'msg t

val v : tab:Tea_core.Prim.Tab_id.t -> 'msg t
(** A queue for one tab, empty, numbering from {!Tea_core.Prim.Msg_seq.one}. *)

val tab : 'msg t -> Tea_core.Prim.Tab_id.t

val record : 'msg -> 'msg t -> ('msg t * (Tea_core.Prim.Msg_seq.t * 'msg)) option
(** Assign the next sequence number and enqueue, yielding the frame to send.
    [None] when the sequence space is exhausted: the message is not recorded and
    the tab has stopped sending. A defined arm rather than a wraparound, and
    unreachable in a page life. *)

val ack : Tea_core.Prim.Msg_seq.t -> 'msg t -> 'msg t
(** Cumulative: drop every entry at or below [seq]. Idempotent and total, so a
    duplicated, late or out-of-order acknowledgement is harmless, and one for a
    sequence number this tab never sent leaves the queue untouched. *)

val unacked : 'msg t -> (Tea_core.Prim.Msg_seq.t * 'msg) list
(** Oldest first: the replay set, in the order the edits were made. Unlike
    [Outbox.drain] this does {b not} empty the queue — that is what
    at-least-once means, and it is also what deletes the old
    drain-then-re-buffer dance, because a send that cannot happen simply leaves
    the entry in place. *)

val pending : 'msg t -> int
val is_empty : 'msg t -> bool
