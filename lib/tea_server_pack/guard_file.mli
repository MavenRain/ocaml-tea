(** The file-backed {!Tea_server.Guard_sink} (roadmap step 11, D16): an
    append-only journal of guard records at [<dir>/journal], CRC-framed by
    {!Tea_server.Guard_sink.Codec}.

    Reading is fold-until-broken: decode stops at the first torn, corrupt, or
    unknown frame and discards the remainder, so a crash mid-append (or a
    partial [flush]) loses only a {i suffix} of floors — and a lost floor can
    only re-open a duplicate, never invent a higher one (the accept
    direction). Appends are buffered and flushed per record; [fsync] happens
    only in {!close}, which serves the same orderly-shutdown boundary as
    [Store.close]: the guarantee is exactly-once across a {i graceful}
    restart. Records reach the page cache per append and so survive process
    death, while [irmin-pack] buffers commits in user space, so a [kill -9]
    can leave a floor whose commit is gone. Since roadmap step 13 that floor
    is {b dropped at the next open} rather than honoured: its recorded water
    stands above a branch head the store no longer carries, so the replay is
    re-admitted as a visible duplicate instead of a silent loss. What
    survives on the loss side: floors recorded at [Store_water.bottom]
    (takes that minted no commit, and legacy records), and everything past
    the check itself, which is a {i boot-time snapshot} - it says nothing
    about a concurrent second writer over the same root or about divergence
    that begins after {!open_} returns (R18). Teardown closes the store {i
    before} the journal so the orderly path cannot land a floor over a
    missing commit at all.

    Housekeeping: live keys are capped ([cap], drop-oldest by last append),
    and the journal is compacted — live floors rewritten to a temp file,
    renamed over the journal — when the file holds more than [4 * cap]
    records, and on any open where the boot filter dropped floors: a
    refused record left in the bytes would be re-adjudicated on every open,
    and the same branch head that stood behind the stale floor rises past
    it with one fresh wall-clock commit, silently un-dropping the floor.
    The open that fired the filter still reports the full verdict; it is
    the next open that finds nothing left to drop. This is the only module
    in the guard stack that touches the filesystem. *)

type t

type open_err =
  | Io of string
  | Bad_dir of string  (** [dir] could not be created or entered. *)

type verdict = Tea_server.Durable_guard.Floors.verdict =
  { kept : int
  ; dropped_behind : int
  ; dropped_no_branch : int
  ; unwitnessed : int
  }
(** {!Tea_server.Durable_guard.Floors.verdict}, re-exported so the boot
    report is named where it is produced. Only [dropped_behind] is evidence
    of the R20 rollback (pack root older than the floors); [dropped_no_branch]
    is routine after checkpoint GC or a reap; [unwitnessed > 0] means this
    boot adopted pre-step-13 floors on trust and is not protected against a
    restored older pack root. *)

val head_water_of_list :
  (Tea_core.Crdt.Replica.t * Tea_core.Prim.Store_water.t) list ->
  Tea_core.Crdt.Replica.t ->
  Tea_core.Prim.Store_water.t option
(** The boot filter's head lookup, from one [Store_core.CORE.branch_waters]
    read taken at open time: a map built once, closed over. [None] means the
    store has no branch for that replica at all; a branch with no readable
    head appears as [Some bottom] (the distinction is what keeps a GC'd
    branch from being reported as a rollback). *)

val open_ :
  dir:string ->
  cap:int ->
  head_water:(Tea_core.Crdt.Replica.t -> Tea_core.Prim.Store_water.t option) ->
  ( Tea_server.Guard_sink.t
    * Tea_server.Durable_guard.Floors.t
    * verdict
    * t
  , open_err )
  result
  Lwt.t
(** Open-or-create [<dir>/journal] ([dir] is created if absent, its parent is
    not), fold the valid frame prefix into floors, then drop every floor
    whose branch head no longer covers its claimed water
    ({!Tea_server.Durable_guard.Floors.filter}, R20): a floor the store has
    rolled out from under would judge its replay Duplicate against an effect
    that no longer exists — silent loss — while dropping it re-admits the
    message as a visible duplicate. The verdict says what happened; the
    returned floors are the admitted ones (after cap enforcement, whose
    drops are NOT in the verdict). Never truncates on open, and never
    rewrites the journal when the filter dropped anything. All errors arrive
    as [open_err]; the caller keeps serving on
    {!Tea_server.Guard_sink.null} rather than aborting — a server without
    durability beats no server. *)

val close : t -> unit Lwt.t
(** Flush, [fsync], close. Wire into the same SIGINT/SIGTERM teardown as
    [Store.close]; the sink answers [Sink_closed] afterwards. Never raises —
    teardown failures are one stderr line. *)
