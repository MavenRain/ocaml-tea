(** The client's keyed-RPC queue (roadmap step 15, D20). The {!Delivery}
    discipline transcribed for the POST channel: every keyed call is recorded
    when it is made, leaves only when a 200 for its sequence number is decoded,
    and is re-sent under the {b same} key until then.

    One entry is in flight at a time. That is what keeps the server-side
    sequence space dense per tab, and therefore what keeps
    {!Tea_server.Replay_guard}'s [Gapped] arm unreachable for an honest client:
    a queue that never skips a number cannot produce a gap, and a queue that
    never sends the second call before the first is acknowledged cannot produce
    one by reordering either.

    {2 Why this is not [Delivery]}

    The websocket queue replays its whole backlog on reconnect and acknowledges
    cumulatively, because a socket delivers in order and one [Hello] covers
    everything sent before it. A POST channel has neither property: each call
    is its own exchange, responses can land out of order, and there is no
    reconnect event to hang a flush on. So {!head} exposes exactly one sendable
    frame instead of {!Delivery.unacked}'s list, and {!ack} drops one entry
    instead of every entry at or below it. Sharing a module between the two
    would mean one of the pair carrying the other's ordering assumption.

    {2 Shell invariant (G4, binding on the runtime interpreter)}

    AT MOST ONE LIVE REQUEST PER HEAD SEQ. The sender must not start a request
    for {!head} while one for the same head is outstanding, and a retry is
    {!head} re-sent, never a renumbered copy. Key stability is therefore
    structural rather than a convention the sender is trusted to keep: there is
    no operation here that assigns a second sequence number to a recorded
    entry.

    {2 What is not bounded}

    The queue, exactly as {!Delivery}'s is not, and for the same reason: the
    alternative to unbounded growth is dropping an entry, which is the silent
    loss the step exists to remove, and it is also what lets the server refuse a
    gap outright. *)

type 'msg t

(** One recorded call, kept whole so a retry re-sends the identical bytes to
    the identical path. [expect] is the typed layer's continuation, fired once
    per entry by the runtime. *)
type 'msg entry =
  { path : string  (** lowered at the vdom boundary, as {!Delivery}'s is *)
  ; body : string
  ; expect : (string, Tea_core.Cmd.http_failure) result -> 'msg
  }

val v : tab:Tea_core.Prim.Tab_id.t -> 'msg t
(** A queue for one page's RPC channel, empty, numbering from
    {!Tea_core.Prim.Msg_seq.one}. The tab MUST be a fresh mint
    ({!Tea_core.Prim.Tab_id.of_draws}), never the websocket tab: the two
    channels are independent streams with independent counters, and sharing an
    id would make each channel's first call look like a replay of the other's. *)

val tab : 'msg t -> Tea_core.Prim.Tab_id.t
(** The stream tab every entry of this queue is keyed under on the wire. Read
    at send time rather than stored per entry, so {!rotate} moves the whole
    queue at once. *)

val record :
  'msg entry -> 'msg t -> ('msg t * (Tea_core.Prim.Msg_seq.t * 'msg entry)) option
(** Assign the next sequence number and enqueue, yielding the frame that was
    recorded. [None] at sequence-space exhaustion: the call is not recorded and
    the tab has stopped sending. A defined arm rather than a wraparound, and
    unreachable in a page life. Note the yielded frame is not necessarily
    sendable - it is only {!head} when the queue was empty. *)

val head : 'msg t -> (Tea_core.Prim.Msg_seq.t * 'msg entry) option
(** The one sendable frame: the oldest unacknowledged entry, or [None] when
    nothing awaits an acknowledgement. One-in-flight is structural because this
    is the only way to reach an entry. *)

val ack : Tea_core.Prim.Msg_seq.t -> 'msg t -> 'msg t
(** Drop the entry at [seq], exposing the next head. Total and idempotent: a
    repeated, late or alien sequence number leaves the queue untouched. Not
    cumulative, unlike {!Delivery.ack} - there is no socket ordering here to
    make "everything at or below" mean "everything the server has seen". *)

val rotate : tab:Tea_core.Prim.Tab_id.t -> 'msg t -> 'msg t
(** Adopt a fresh tab id: the 4xx poison-recovery arm. Numbering is untouched,
    and so is the queue, so an entry still waiting is simply re-sent under the
    new tab at its existing sequence number. That needs no server cooperation
    and no renumbering, because an absent floor accepts any first sequence
    number (see [Tea_server.Replay_guard]); under the old tab that same number
    would sit below the floor the poisoned call left behind. *)

val pending : 'msg t -> int
(** Backlog size, exposed so an app can render it - the same honesty
    {!Delivery.pending} gives the websocket channel. *)

val is_empty : 'msg t -> bool
(** Whether nothing awaits an acknowledgement. *)
