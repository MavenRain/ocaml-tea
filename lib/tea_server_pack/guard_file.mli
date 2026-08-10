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

val tag_identity : char
(** ['\004'], the store-identity header frame's tag, owned HERE rather than in
    {!Tea_server.Guard_sink}: it is a Guard_file-level structural frame, never
    a {!Tea_server.Guard_sink.event}, so {!Tea_server.Durable_guard.Floors}
    and the codec's tag dispatch never see it. Reserved permanently, the way
    tag [1] stays "decoded forever, written never" rather than being recycled.
    Tag [0] stays unassigned on purpose: a zero byte is what a sparse or
    not-yet-written region reads back as, and giving it meaning would invite a
    torn file to parse as a valid frame. *)

val tag_epoch : char
(** ['\005'], the boot-epoch stamp's tag (roadmap step 21, R20b), owned here
    for {!tag_identity}'s reason. The stamp is the frame DIRECTLY after the
    identity header and never exists without one: an epoch frame at byte 0
    would read as event bytes to every decoder, old and new, and the
    downgrade story belongs to the identity frame - a pre-step-21 binary
    meets tag [4] at byte 0, reads [Bad_tag], and keeps zero frames, exactly
    as before; the extra frame behind it changes nothing it can see. *)

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
    restored older pack root. Identity is reported BESIDE these four, never
    inside them: a whole-journal identity fact folded into a per-floor water
    count would read to an operator as ordinary rollback noise when it may be
    a much bigger incident. *)

type identity_outcome =
  | Matched
      (** Header present, decoded, equal to the caller's binding: today's
          path, byte for byte. *)
  | Freshly_bound
      (** The journal file is ABSENT or holds ZERO bytes: there was no claim to
          compare against, and nothing at all could be lost by binding it.
          Kept distinct from [Adopted_unbound 0] so a brand-new store's first
          boot is SILENT rather than printing an "adopted 0 floors on trust"
          line that reads as an alarm on a deployment where nothing is wrong.
          A file that holds bytes NEVER lands here, however little of it
          decodes: bytes that were written are floors somebody meant to keep,
          and reporting their loss as a fresh store's first boot is exactly
          the silence this arm must stay unique to earn. *)
  | Adopted_unbound of int
      (** A real journal carrying no readable header: a pre-step-18 file, one
          whose header was lost, or one whose first bytes will not unframe at
          all. [int] is the floors adopted on trust, counted at the same point
          [unwitnessed] is, post-{!Tea_server.Durable_guard.Floors.filter},
          which still runs in full, because the step-13 water check is
          completely orthogonal to the identity question.
          Trust-on-first-use, never a wholesale refusal: an absent claim is no
          claim, the exact structural twin of
          {!Tea_core.Prim.Store_water.bottom}. Treating it as a mismatch
          instead would wipe every floor on every upgrade boot. Under a
          {!Tea_core.Prim.Store_identity.Bound} caller the journal is stamped
          before {!open_} returns, so the next boot is protected; under
          {!Tea_core.Prim.Store_identity.Unresolved} there is no token to
          stamp WITH, so the journal stays unbound and the next boot adopts
          again. *)
  | Rebound of int
      (** A header frame that names a DIFFERENT store: this journal belongs to
          other bytes. [int] is the floors it held, all of them dropped. Two
          shapes reach this arm and both are DECIDABLE: a payload that decoded
          cleanly into some other store's token, and a payload that decodes
          into no token at all, which therefore equals no store's token
          either. What never reaches it is bytes that would not unframe: those
          carry no frame to read a claim out of, so "cannot read it" proves
          nothing about whose store they are and they fall into
          [Adopted_unbound] instead. *)
  | Unresolved_cleared of int
      (** The CALLER could not establish this store's own identity
          ({!Tea_core.Prim.Store_identity.Unresolved}), so the journal's claim
          cannot be confirmed. [int] floors are dropped (when in doubt this
          family pays a duplicate), and the file is HELD for as long as the
          returned handle lives: no compaction, no re-stamp, and no APPEND
          reaches it, not one byte rewritten. A boot that can read the token
          again therefore finds the original header and keeps its floors,
          instead of finding a stand-in this boot invented. The price is
          stated on purpose: floors this boot records live in memory only, so
          the next boot replays them as visible duplicates. That is the accept
          direction, and it is the same one a torn tail pays. *)
(** What {!open_} made of the journal's identity header, reported beside the
    water {!verdict} rather than folded into it. *)

type epoch_outcome =
  | Epoch_matched
      (** The journal's stamp equals this boot's pre-bump counter - the
          ordinary same-lineage reopen - or there was nothing to check
          because identity already cleared or held the journal, or the file
          was empty. The no-news arm: printers stay quiet on it. The journal
          is still REWRITTEN on a match (unlike identity's own [Matched]),
          because the stamp must advance to this boot's post-bump value. *)
  | Epoch_adopted
      (** Identity trusted the bytes and no epoch stamp is present: a
          pre-step-21 journal, adopted on trust and stamped before {!open_}
          returns - [Adopted_unbound]'s one-boot window, one family over.
          Absence is a version signal, never a numeric mismatch: treating it
          as divergence would wipe every floor on every upgrade boot. *)
  | Epoch_diverged of int
      (** The journal's stamp differs from this boot's pre-bump counter - in
          EITHER direction, deliberately one arm for both (R20b's rule: a
          root rolled back under a newer journal and a journal restored
          beside an advanced root are the same fact) - or the stamp is torn,
          which is corruption evidence and must not masquerade as the
          upgrade window. [int] is the floors the journal claimed, all of
          them cleared; the replays land as visible duplicates, and the
          journal is restamped with this boot's value. The R20b close: a
          [cp -r] freezes [<root>/tea.epoch] at copy time, every later boot
          on EITHER side moves exactly one of the two values, and equality
          sees the split that a constant create-time token provably
          cannot. *)
  | Epoch_unresolved
      (** The root's own counter could not be established
          ({!Tea_core.Prim.Store_epoch.Unresolved}). A journal CARRYING a
          stamp is held and its floors clear this boot
          ([Unresolved_cleared]'s strict hold, one family over: not one byte
          rewritten, so a boot that reads the counter again finds the
          original stamp); a journal carrying NO stamp keeps identity's
          verdict and stays unstamped - there is no counter to stamp it
          with, so the upgrade window stays open one more boot. *)
(** What {!open_} made of the journal's boot-epoch stamp, nested strictly
    INSIDE identity's trust: consulted only where identity kept the bytes,
    because a foreign journal's stamp answers a question about the wrong
    store. Reported beside {!identity_outcome}, never folded into it. *)

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
  identity:Tea_core.Prim.Store_identity.binding ->
  epoch:Tea_core.Prim.Store_epoch.binding * Tea_core.Prim.Store_epoch.binding ->
  ( Tea_server.Guard_sink.t
    * Tea_server.Durable_guard.Floors.t
    * verdict
    * identity_outcome
    * epoch_outcome
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
    durability beats no server.

    [~identity] is REQUIRED and never optional: an omitted argument defaulting
    to "no check" is a safety gate that can be disarmed by forgetting it. It
    is read ONCE by the caller and handed to every channel, exactly as
    [~head_water] is and for the same reason, and because each journal file
    carries and checks its OWN header, no channel's stamp can satisfy another
    channel's check, which makes the two channels identical by construction
    rather than by a rule someone has to remember.

    [~epoch] is REQUIRED for [~identity]'s exact reason, and it is the PAIR
    [(seen, now)] from one {!Tea_persist_pack} [bump_epoch] call: [seen] (the
    pre-bump counter, equal to the immediately preceding same-lineage boot's
    [now]) is what a journal's stamp is COMPARED against, [now] (the
    post-bump counter) is what every rewrite this boot performs STAMPS. Two
    separate reads could disagree with each other - the counter mutates at
    every boot - so the pair crosses this boundary together, and every
    channel of one boot must receive the same pair. The stamp is checked by
    EQUALITY, never order (see {!Epoch_diverged}).

    The identity header is the file's FIRST frame or it is absent; it is
    written ONLY as part of the compacting rewrite, which stages the whole
    file in [<path>.tmp] and cuts over with an atomic rename, so header and
    floors are always the same generation and the header can no more be torn
    on the live file than the first kept floor can. A mismatch clears the
    decoded events OUTRIGHT, before
    {!Tea_server.Durable_guard.Floors.of_events}; it must never be implemented
    by starving the water filter of heads, because that filter keeps a
    {!Tea_core.Prim.Store_water.bottom} floor UNCONDITIONALLY before it ever
    consults a head, so a stranger journal's legacy, no-op and fuel-exhausted
    records would survive and go on judging replays [Duplicate] against a
    store that never produced them. *)

val close : t -> unit Lwt.t
(** Flush, [fsync], close. Wire into the same SIGINT/SIGTERM teardown as
    [Store.close]; the sink answers [Sink_closed] afterwards. Never raises —
    teardown failures are one stderr line. *)
