(** Browser-local persistence protocol (roadmap step 25, D25/R28).

    Pure: no [Ojs], no [Js_browser]. The jsoo shell lives in [tea_client_run]
    ([Idb], [Tab_lock]) and contains zero protocol decisions; everything that
    decides - the record shape, the codec, the boot gate, the adoption and
    confirm rules, the degrade arms - lives here, where the native suite
    drives it against a fake backend.

    The store holds ONE record per replica in one object store, rewritten
    whole on every checkpoint by a clear-then-put pair on a single
    transaction, so a torn or mixed-tab row is unrepresentable. Every failure
    arm degrades to the pre-step-25 memory-only live behaviour; persistence
    is never allowed to break the live path. *)

type idb_error =
  | Unsupported
  | Blocked
  | Version_error
  | Quota_exceeded
  | Not_found
  | Other of string
(** The closed failure surface of the browser store. [Unsupported] and
    [Blocked] are minted by the shell's open flow (no [indexedDB] object; an
    open held off by an old-version connection); the rest classify
    DOMException names. *)

val classify : string -> idb_error
(** DOMException name to variant. Total; unknown names land in [Other name],
    never a wildcard arm on the variant itself. *)

val db_name : title:string -> string
(** ["tea-local:" ^ title]: one database per app. *)

val lock_name : title:string -> string
(** ["tea-local-writer:" ^ title]: the Web Locks resource one tab holds
    exclusively for its page life; non-holders run memory-only. *)

val store_name : string
(** ["records"]. *)

val db_version : int
(** [1]. *)

(** What the jsoo shell must provide. Effectful and callback-based, because
    IndexedDB is; the shell does the browser calls and NOTHING else. *)
module type BACKEND = sig
  type db

  val open_db :
    name:string ->
    version:int ->
    on_versionchange:(unit -> unit) ->
    on_blocked:(unit -> unit) ->
    ok:(db -> unit) ->
    err:(idb_error -> unit) ->
    unit
  (** Open (and on first open create) the database. [on_versionchange] fires
      after the shell has already closed the connection on a later-version
      open elsewhere; [on_blocked] when this open is held off the same way.
      Success callbacks fire from a later task; an error arm may answer in
      the caller's own task (a synchronous throw is absorbed there), and
      {!Flow.boot} funnels both shapes through one once-guard. *)

  val read_all :
    db -> store:string -> ok:(string list -> unit) -> err:(idb_error -> unit) -> unit
  (** All record values of [store], one [getAll], order unspecified. *)

  val replace_all :
    db ->
    store:string ->
    key:string ->
    value:string ->
    ok:(unit -> unit) ->
    err:(idb_error -> unit) ->
    unit
  (** MUST issue clear then put on ONE readwrite transaction inside one
      synchronous turn (IndexedDB auto-commits at the end of the task, D25):
      the pair is atomic, so a crash can never split the stores' truth. *)

  val clear_all :
    db -> store:string -> ok:(unit -> unit) -> err:(idb_error -> unit) -> unit
  (** Clear the store on one readwrite transaction: the invalidation arm,
      same turn discipline as [replace_all]. *)
end

module Make (A : Tea_core.App.APP) : sig
  (** One checkpoint: everything a later page life needs to keep working.
      [queue] is oldest-first, exactly [Delivery.unacked]'s order; [next] is
      persisted separately because an acknowledgement that empties the queue
      must not reset the numbering. *)
  type record =
    { replica : Tea_core.Crdt.Replica.t
    ; tab : Tea_core.Prim.Tab_id.t
    ; next : Tea_core.Prim.Msg_seq.t
    ; queue : (Tea_core.Prim.Msg_seq.t * A.msg) list
    ; clock_floor : int64
    ; model : A.model
    ; written_at_ms : float
    }

  val record_t : record Repr.t
  (** Derived from [A.model_t]/[A.msg_t] and the [Prim]/[Crdt] witnesses: the
      snapshot codec cannot drift from the wire codec, and app authors add
      nothing. *)

  val to_json : record -> string

  val of_json : string -> (record, Tea_core.Codec.err) result
  (** Total. [Decode_failed] is the corruption gate: a row that does not
      decode is discarded whole, never partially trusted (the record is
      advisory, R28). *)

  val decode_all : string list -> record list
  (** Drops undecodable rows; total. *)

  val choose : record list -> record option
  (** Newest [written_at_ms] by a total fold; the earliest of an exact tie.
      [None] on the empty list. *)

  val key_of : record -> string
  (** The replica's stable key string (its Repr JSON): one row per replica,
      and the checkpoint's clear prunes every other replica's rows. *)

  val checkpoint_record :
    replica:Tea_core.Crdt.Replica.t ->
    delivery:A.msg Delivery.t ->
    clock_floor:int64 ->
    model:A.model ->
    now_ms:float ->
    record
  (** [queue := Delivery.unacked delivery]; [next := Delivery.next_seq
      delivery]: acked entries evict naturally because the record only ever
      carries unacked. *)

  (** The boot gate: numbering exists exactly once, only after the persisted
      record is adopted or ruled out. Nothing sends while buffering, and
      nothing ADOPTED sends until the first Hello confirms the replica. *)
  type gate

  val buffering : gate
  (** The initial state: pre-resolution edits buffer as payloads, unnumbered. *)

  type outcome =
    | No_record
    | Adopt of record

  type resolved =
    { gate : gate
    ; paint : A.model option
    ; seed : int64 option
    }
  (** [paint = Some model] exactly when [confirmed] was [None] and the
      outcome adopted: the one moment a stale local model may paint, marked
      provisional until the first Hello. The paint reaches the app through
      the same arm every server head uses (its [Store_watch] subscription);
      an app whose current subscriptions carry no store leaf receives no
      paint, and an LWW app can see a pre-resolution optimistic edit
      visually superseded until its queued payload flushes (R6, D25).
      [seed] is [Some clock_floor] on EVERY validated adoption, painting or
      not: a same-replica adoption that skipped the floor would regress it
      durably and let two page lives mint colliding dots. *)

  val resolve :
    mint:(unit -> Tea_core.Prim.Tab_id.t) ->
    confirmed:Tea_core.Crdt.Replica.t option ->
    outcome ->
    gate ->
    resolved
  (** Adoption resumes the persisted (tab, next, queue) verbatim when it
      validates ([Delivery.of_persisted]), then numbers the buffered payloads
      after it; a corrupt record falls back to a fresh mint. With [confirmed]
      already known, a matching replica is immediately flushable and a
      mismatch drops the adopted prefix on the spot. Idempotent on a live
      gate. *)

  type confirm_effect =
    | Idle
    | Flush
    | Prune_and_flush

  val confirm :
    announced:Tea_core.Crdt.Replica.t -> gate -> gate * confirm_effect
  (** The first Hello's verdict. Match: the withheld queue flushes. Mismatch:
      the adopted prefix is dropped by one cumulative [Delivery.ack] at the
      adopted high seq (everything born since survives), then flushes, and
      the caller checkpoints so the clear prunes the stale replica's row.
      Idempotent: a second confirm is [Idle]. *)

  val record_msg :
    A.msg -> gate -> gate * (Tea_core.Prim.Msg_seq.t * A.msg) option
  (** Buffering: the payload buffers, [None]. Live but unconfirmed: recorded,
      [None] (withheld until the verdict). Live and confirmed: recorded,
      [Some entry] to send now. *)

  val ack : Tea_core.Prim.Msg_seq.t -> gate -> gate
  (** Total; a no-op while buffering. *)

  val delivery : gate -> A.msg Delivery.t option

  val replay :
    gate ->
    (Tea_core.Prim.Tab_id.t * (Tea_core.Prim.Msg_seq.t * A.msg) list) option
  (** The flush set, tab plus oldest-first entries; [None] while buffering or
      unconfirmed. *)

  val flushable : gate -> bool

  val unconfirmed_replica : gate -> Tea_core.Crdt.Replica.t option

  (** The orchestration the runtime drives: open, read, decode, degrade. *)
  module Flow (B : BACKEND) : sig
    type conn

    val boot : title:string -> k:(conn option * record list -> unit) -> unit
    (** [open_db] + [read_all] + [decode_all]. [k] fires EXACTLY once on
        every arm; every failure arm (unsupported, blocked, version error,
        open error, read error) yields [(None, [])], the memory-only
        degrade. A version change after boot marks the connection degraded;
        the shell has already closed it. *)

    val checkpoint :
      conn option -> record -> k:((unit, idb_error) result -> unit) -> unit
    (** [replace_all] under [key_of record]; the clear prunes every other
        row. [None] or degraded connection: a no-op [Ok], answered in the
        caller's own task. Never gates the caller; [k] only logs. *)

    val invalidate :
      conn option -> k:((unit, idb_error) result -> unit) -> unit
    (** Best-effort clear of every row, and the connection degrades FIRST so
        no later checkpoint resurrects a stale record. A record whose
        checkpoints stopped landing lies about [next], and adopting that lie
        costs the next life its first edits: they number into the server's
        already-consumed range and are deduplicated away (R28). A clear that
        itself fails leaves that residual standing; [k] only logs. *)
  end
end
