(** The irmin-pack backend: a durable on-disk store with checkpoint + GC
    retention (roadmap step 6, R1). Same {!Tea_persist.Store_core.CORE}
    surface as the in-memory shim, plus [create] on a filesystem root and
    [gc]. *)

(** A pack store root directory. *)
module Root : sig
  type t

  val v : string -> t
  (** The store's root directory. The PARENT must already exist (only the
      final component is created on first open); existing data is always
      reopened, never truncated — there is deliberately no [fresh]. *)

  val to_string : t -> string
end

val framed : 'a Repr.t -> 'a -> 'a Repr.t
(** [framed inner fallback] re-encodes [inner] as a single length-prefixed
    [Repr.string], which is the codec shape irmin-pack requires of a contents
    type: a pack entry is [hash][kind][the codec's own bytes] and the reader
    recovers the entry's length by decoding a varint at the head of those bytes.
    A [Repr.record]'s leading varint frames only its {i first field}, so a
    multi-field model read back short and its decoder walked off the end of the
    buffer ([Invalid_argument "index out of bounds"]) — a single-field model
    satisfied the contract by accident, which is why the Counter-based pack
    tests never saw it.

    Exposed so the contract is testable on its own, and applied to every store
    this module builds. [fallback] is the corrupt-bytes arm (no exception in
    normal control flow); it cannot mask corruption, because irmin re-hashes
    what it decoded and a fallback value hashes to something other than the
    requested key. *)

module Make (A : Tea_core.App.APP) : sig
  include Tea_persist.Store_core.CORE with type model = A.model and type msg = A.msg

  val create :
    ?now:(unit -> int64) -> ?lower_root:string -> ?exploded:exploder -> Root.t -> t Lwt.t
  (** Open (or initialise) the pack store at [Root]. Always configured with
      [Indexing_strategy.minimal], so delete-mode GC stays allowed for the
      root's entire life — an [always]-indexed root is permanently poisoned
      against GC. Call {!close} before exit.

      [?lower_root] configures a lower-layer directory (its parent must exist);
      GC then archives discarded data there instead of deleting it, so
      pre-checkpoint commits stay readable ([Gc.behaviour] becomes [`Archive]).
      Omitted, GC deletes (D5). *)

  type open_error =
    | Root_parent_missing of string
        (** the root's parent directory is absent (payload: the parent), so
            the single [mkdir] the backend does would fail with ENOENT. *)
    | Root_not_a_directory of string
        (** the root path exists and is a regular file or other
            non-directory. *)
    | Root_not_a_pack_store of string
        (** the directory exists but holds neither [store.control] nor
            [store.pack], so irmin-pack answers [Invalid_layout] by raising. *)
    | Backend_failed of string
        (** [Printexc.to_string] of anything irmin-pack still raised past the
            preflight (corrupt control file, migration needed, a store from
            the future, or a lost classification race). The payload is a
            string for the same reason [Guard_file.open_err]'s is: irmin-pack's
            own error type is an OPEN polymorphic variant with 40-odd cases,
            which no exhaustive match can consume. *)

  val open_root :
    ?now:(unit -> int64) ->
    ?lower_root:string ->
    ?exploded:exploder ->
    Root.t ->
    (t, open_error) result Lwt.t
  (** {!create} with a total preflight and a catching seam, so an unusable
      root is a value rather than an uncaught [Pack_error] killing the
      process (roadmap step 12). Behaviour on every working path
      (parent-exists/leaf-missing create, reopen, an existing [lower_root])
      is bit-identical to {!create}, which is left in place unchanged for
      the tests that own their scratch roots.

      The preflight classifies with [Irmin_pack_unix.Io.Unix.classify_path]
      (total, never raises) and accepts an existing directory only if
      [store.control] or [store.pack] is a file - the [store.pack] arm keeps
      today's v1/v2 auto-migration reachable. The [Lwt.catch] is required
      {i as well as} the preflight: classification is racy and several
      irmin-pack failures are unclassifiable from outside. *)

  val explain : open_error -> string
  (** One operator-facing sentence per constructor, each naming the path it
      carries and the remedy it wants. Keeping the match here means a new
      constructor is a compile error in one place, and it keeps callers from
      pasting every payload into a single template: [Root_parent_missing]
      carries the PARENT, not the root, and the two directory refusals want
      opposite fixes. The sentence is a fragment, meant to be embedded (the
      caller supplies the subject and the consequence). *)

  type identity_origin =
    | Read  (** decoded from an existing [<root>/tea.identity] *)
    | Minted  (** absent; this boot drew it and published it *)
    | Adopted  (** lost the [O_EXCL]/[link] race; re-read the winner's bytes *)
    | Absent_unmintable of string  (** absent, and the mint failed; the reason *)
    | Unreadable of string  (** present, but lstat/open/read refused it; the reason *)
    | Malformed  (** present and read, but not 32 lowercase hex *)

  val identity_path : Root.t -> string
  (** [<root>/tea.identity]. Exposed so a test can delete, corrupt, or stat it
      without duplicating the filename literal, and so a check can assert it
      survives a checkpoint GC. *)

  val resolve_identity : Root.t -> Tea_core.Prim.Store_identity.binding * identity_origin
  (** The store's create-once lineage token (step 18, D23, R20a). Total,
      synchronous, and never raises: read [<root>/tea.identity] if it is there,
      else draw 16 bytes and publish them with the same [O_EXCL]-temp + [fsync]
      + [Unix.link] pattern {!Tea_server.Session_secret} mints its secret with -
      [link] rather than [rename] precisely because it fails [EEXIST] instead of
      overwriting, so a racing second boot re-reads the winner's bytes rather
      than installing a second identity for one store.

      Every failure yields [Unresolved] with the reason. There is no error
      result and no refusal arm BY CONSTRUCTION OF THE TYPE, not by convention:
      identity trouble must never be able to stop a boot, because the whole
      guard family's degradation direction is duplicate, never loss, and a
      server without durability beats no server.

      An existing file is NEVER rewritten, truncated, or deleted - not on a
      malformed read, not to heal it, not to migrate it. This is
      {!Tea_server.Session_secret}'s rule for the same reason one level over
      (R14): an unreadable token may be a transient mount problem, and
      overwriting it would permanently unbind every journal that already names
      it.

      ORDERING, load-bearing: call this only AFTER {!open_root} or {!create} has
      succeeded. The backend does exactly one [mkdir], and a [tea.identity]
      minted into a directory this function created would leave a [<root>]
      holding no [store.control] and no [store.pack] - which {!open_root}
      classifies [Root_not_a_pack_store] on the next boot.

      The token lives INSIDE [<root>], never as a fourth [<root>.identity]
      sibling: a token that does not travel atomically with the store's own
      bytes is R20 recursed one level, and only a file inside the tree is
      carried by the [cp -r]/tar/rsync/snapshot that creates R20 in the first
      place. The [tea.] prefix is chosen against irmin-pack's naming SCHEME, not
      its current filenames - [Irmin_pack.Layout.Classification.Upper.v] splits
      on ['.'] and every arm begins ["store"], so anything outside that
      namespace classifies [`Unknown], which [file_manager.ml]'s post-GC cleanup
      explicitly never removes. Never take a name under [store.] or
      [volume.]. *)

  val explain_identity : identity_origin -> string
  (** One sentence per constructor, matched HERE so a new constructor is a
      compile error in one place ({!explain}'s rule). *)

  val gc_behaviour : t -> [ `Archive | `Delete ]
  (** [`Archive] when a [lower_root] was configured (GC moves discarded data to
      the lower layer), [`Delete] otherwise. Observable without running a GC. *)

  type gc_error =
    | Gc_disallowed
    | Gc_already_running
    | Gc_failed of string

  val gc : t -> retain:checkpoint -> (unit, gc_error) result Lwt.t
  (** Discard every commit, tree, and blob older than [retain] and
      unreachable from it, blocking until the forked GC worker finalises.
      [retain]'s commit and full tree survive; its parents remain only as
      dangling stubs, so history/undo walks crossing the boundary degrade to
      truncation/[None]. Precondition: run with other sessions quiescent,
      merged, or forked after the checkpoint — branch heads written {i
      before} it become unreadable. *)

  val flush : t -> unit Lwt.t
  (** The commit fence (roadmap step 20, R11): force every byte irmin-pack
      still buffers for this store into the page cache. {!Tea_server_pack}
      wires this as {!Tea_server.Durable_guard}'s fence, so a delivery
      floor's own journal append can never reach the page cache before the
      commit bytes it witnesses — the D16 inequality as a LOCAL invariant
      of the persist path. The discovery this step pinned
      ([store_pack_flush_test]): irmin-pack 3.11 already ends every commit
      batch in a file-manager flush, so on the pump path this call meets a
      clean buffer and moves nothing; it exists because nothing in
      irmin-pack's interface promises that batch-end flush, and an
      upstream that dropped it must not be able to silently reopen R11.
      Rejects on failure: the one production composer,
      {!Tea_server.Durable_guard.persist}, runs the fence inside its own
      catch, so a failed flush becomes [Error (Io _)] with the floor never
      appended — an audible, at-least-once persist (the duplicate
      direction), never a floor ordered ahead of the commit bytes the
      flush failed to move. The deeper backstop, independent of the fence:
      the R20 boot filter drops any floor whose water the restored head no
      longer covers, so even an unfenced append degrades to a visible
      duplicate at the next boot rather than silent loss.
      Page-cache arrival only:
      [use_fsync] stays false, so a power cut is explicitly out of scope.
      Cheap when nothing is buffered (the empty-buffer short-circuit),
      which on the pump path is always. *)
end
