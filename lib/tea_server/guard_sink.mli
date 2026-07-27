(** The durable sink seam for the replay guard (roadmap step 11, D16).

    Step 10's {!Replay_guard} promises exactly-once {i effect} within one
    server process lifetime: the high water lives in memory, so a restart
    forgets it, and a client that replays an unacknowledged message across
    that restart is applied a second time. This seam is where the high water
    goes to survive: the pump writes one {!event} through {!type-t} after
    every [Fresh] take, and a restarting server folds the record back into a
    floor mirror before it answers its first frame.

    Two properties carry the whole design:

    - {b Durability keys off the [Fresh] verdict, not off a commit.} A message
      whose update is a no-op mints no Irmin commit at all, yet its sequence
      number was consumed — so the durable record cannot ride the store
      (D15's no-op crux, dissolved by recording "taken", never "applied").
    - {b Every degradation of the record itself falls toward duplicating.} A
      torn tail, a failed append, a missing journal: each loses only the {i
      floor}, and an absent floor accepts anything. Two paths still reach the
      loss side, and neither is closed by this module: the
      stale-entry-outlives-its-branch case stated on {!Replay_guard.forget}
      (closed by ordering in [Store_core.reap]'s [?forget]), and a floor
      outliving its {i commit} after a hard kill, since this record reaches
      the page cache per append while the store buffers commits in user space
      ([Tea_server_pack.Guard_file]).

    The sink is a record of one function so the mem tier can stay byte-for-
    byte on step-10 semantics ({!null}), tests can observe exactly what was
    recorded ({!memory}), and the pack tier can swap in a file journal
    ([Tea_server_pack.Guard_file]) without this library learning about file
    IO. No IO happens in this module; [Lwt] appears only in the closure
    type. *)

type event =
  | Advance of
      { replica : Tea_core.Crdt.Replica.t
      ; tab : Tea_core.Prim.Tab_id.t
      ; seq : Tea_core.Prim.Msg_seq.t
      }
      (** [(replica, tab)] consumed [seq]: raise its durable floor. Written
          after the apply attempt completes (either outcome), before the
          acknowledgement. *)
  | Forget of { replica : Tea_core.Crdt.Replica.t }
      (** Tombstone: the replica's branch is about to be removed, so every
          floor under it must die with it — a floor outliving its branch is
          the silent-loss path. Written {i before} the branch removal. *)

type err =
  | Sink_closed  (** Appended after teardown; the record is lost (duplicate direction). *)
  | Io of string  (** The backend failed; the record is lost (duplicate direction). *)

type t = { append : event -> (unit, err) result Lwt.t }

val null : t
(** Accepts and discards everything — the mem tier, where "durable" has no
    meaning and step-10 semantics are the specified behaviour. *)

val memory : unit -> t * (unit -> event list)
(** A recording sink and its reader (events oldest-first): the test seam for
    simulated restarts — record a life, [Codec] or {!of_events} it into the
    next one. *)

(** The journal frame codec, pure and total: one {!event} per frame,
    length-prefixed and CRC-32-guarded, so a torn tail or a flipped byte is a
    classified verdict, never an exception. Framing (big-endian):
    [len:4][tag:1][payload:len-1][crc:4], where [len] counts tag plus payload
    and the CRC covers the same span. *)
module Codec : sig
  type decode_err =
    | Torn
        (** The frame claims more bytes than remain — the crash-in-flight
            shape. Decoding stops here; an earlier prefix stays valid. *)
    | Bad_crc  (** The frame is complete but its bytes lie. *)
    | Bad_tag of int  (** A tag byte this codec never wrote. *)
    | Bad_field of string  (** The payload decoded but a field failed validation. *)

  val to_bytes : event -> string

  val of_bytes : string -> pos:int -> (event * int, decode_err) result
  (** Decode one frame at [pos]; [Ok (event, next)] gives the offset of the
      following frame. Total: every corruption is a [decode_err]. *)
end
