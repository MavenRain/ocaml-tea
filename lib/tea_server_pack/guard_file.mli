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
    restart, and {b past a hard kill it is neither}. Records reach the page
    cache per append and so survive process death, while [irmin-pack] buffers
    commits in user space, so a [kill -9] can leave a floor whose commit is
    gone. The replay then reads [Duplicate] against an effect that no longer
    exists, which is a silent loss of the unacknowledged in-flight tail
    (bounded by the pack's auto-flush lag). Teardown closes the store {i
    before} the journal precisely so the orderly path cannot land that way.

    Housekeeping: live keys are capped ([cap], drop-oldest by last append),
    and when the file holds more than [4 * cap] records it is compacted —
    live floors rewritten to a temp file, renamed over the journal. This is
    the only module in the guard stack that touches the filesystem. *)

type t

type open_err =
  | Io of string
  | Bad_dir of string  (** [dir] could not be created or entered. *)

val open_ :
  dir:string ->
  cap:int ->
  (Tea_server.Guard_sink.t * Tea_server.Durable_guard.Floors.t * t, open_err)
  result
  Lwt.t
(** Open-or-create [<dir>/journal] ([dir] is created if absent, its parent is
    not), fold the valid frame prefix into the returned floors, and hand back
    the live sink. Never truncates on open. All errors arrive as [open_err];
    the caller keeps serving on {!Tea_server.Guard_sink.null} rather than
    aborting — a server without durability beats no server. *)

val close : t -> unit Lwt.t
(** Flush, [fsync], close. Wire into the same SIGINT/SIGTERM teardown as
    [Store.close]; the sink answers [Sink_closed] afterwards. Never raises —
    teardown failures are one stderr line. *)
