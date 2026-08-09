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
      ([Tea_server_pack.Guard_file]). Roadmap step 13 narrows both at the
      {i next boot}: the [water] field on {!Advance} lets the pack tier drop
      a floor whose branch head no longer covers it, leaving exposed only
      [bottom]-water floors and divergence after the boot check.

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
      ; water : Tea_core.Prim.Store_water.t
      }
      (** [(replica, tab)] consumed [seq]: raise its durable floor. Written
          after the apply attempt completes (either outcome), before the
          acknowledgement.

          [water] (roadmap step 13) is the {!Tea_core.Prim.Store_water} of the
          commit this take minted — the floor's own claim about which store
          state it de-duplicates against, what the boot filter checks a
          restored pack root against — or [bottom] ("no claim") when the take
          minted none: a no-op update, fuel exhaustion, or a record decoded
          from a pre-step-13 journal. *)
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
    simulated restarts — record a life, [Codec] or
    [Durable_guard.Floors.of_events] it into the next one. *)

(** The journal frame codec, pure and total: one {!event} per frame,
    length-prefixed and CRC-32-guarded, so a torn tail or a flipped byte is a
    classified verdict, never an exception. Framing (big-endian):
    [len:4][tag:1][payload:len-1][crc:4], where [len] counts tag plus payload
    and the CRC covers the same span.

    Tags: [3] is the water-stamped [Advance] this codec writes (roadmap
    step 13); [2] is [Forget]; [1] is the pre-step-13 [Advance] - decoded
    forever at {!Tea_core.Prim.Store_water.bottom} so an upgrade keeps every
    floor it already earned, written never. [4] is RESERVED for
    {!Tea_server_pack.Guard_file}'s store-identity header (roadmap step 18,
    D23) and is never an {!event}: this codec neither writes nor decodes it,
    and {!unframe} is how that module reaches the framing without this one
    learning about identity. Tags are never recycled, and a future encoding
    change for any kind takes a NEW tag rather than an in-payload version
    byte, so this format has exactly one evolution mechanism. A binary meeting
    a tag it does not know reads [Bad_tag] and keeps only the preceding
    frames: the duplicate side, the sanctioned downgrade. *)
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

  val frame : tag:char -> payload:string -> string
  (** The shared encode side: [len:4][tag:1][payload][crc:4], [len] counting
      tag plus payload and the CRC covering the same span. The ONLY place the
      framing constants live; {!to_bytes} is this applied to an event body. *)

  val unframe : string -> pos:int -> (char * string * int, decode_err) result
  (** The shared decode side: [Ok (tag, payload, next)] where [next] is the
      offset of the following frame. [Torn] and [Bad_crc] exactly as
      {!of_bytes} raises them today; tag DISPATCH is left to the caller, so
      this never returns [Bad_tag] or [Bad_field] - an unrecognised tag is the
      caller's decision, not this codec's. That is what lets
      {!Tea_server_pack.Guard_file} own tag [4] (its store-identity header)
      without this module growing a second checksum scheme or a second
      framing. *)
end
