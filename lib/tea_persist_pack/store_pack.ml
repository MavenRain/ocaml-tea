(** The irmin-pack backend shim: see store_pack.mli. *)

open Lwt.Syntax

(* irmin-pack 3.11 has no [Conf.Default]; these values follow conf.mli's own
   guidance: 32 entries per inode, no stable-hash compatibility baggage for a
   fresh store, and a varint length header so contents of any repr-derived
   codec parse unambiguously. *)
module Conf : Irmin_pack.Conf.S = struct
  let entries = 32
  let stable_hash = 0
  let contents_length_header = Some `Varint
  let inode_child_order = `Hash_bits
  let forbid_empty_dir_persistence = false
end

module Root = struct
  type t = Root of string

  let v (dir : string) : t = Root dir
  let to_string (Root dir : t) : string = dir
end

(* THE CONTENTS FRAMING CONTRACT (this is what [contents_length_header] above
   actually promises irmin-pack).

   A pack entry is [hash][kind][the contents codec's own bytes]; nothing in
   [Pack_value.Of_contents.encode_bin] writes a length. The reader recovers the
   entry length by decoding a varint at the head of those bytes
   ([Pack_store.read_and_decode_entry_prefix]), so the contents codec MUST begin
   with one varint spanning everything after it — the property [Repr.string] has
   and a [Repr.record] does not: a record's leading varint frames only its FIRST
   field. A single-field model (the Counter) satisfies the contract by accident,
   which is why every existing pack test passed; any multi-field model (the D1
   CvRDT doc) made the reader stop short and the decoder walk off the end of the
   truncated buffer with [Invalid_argument "index out of bounds"].

   [framed] re-encodes the whole model as one [Repr.string], which restores the
   invariant for any app model whatsoever. It is deliberately here, beside the
   [Conf] that demands it, and not in the backend-generic [Store_core]: the mem
   backend has no entry framing and keeps the model's structural encoding.

   The [fallback] arm is the corrupt-bytes case, never taken for bytes we wrote
   (no exception in normal control flow). It cannot mask corruption either:
   irmin re-hashes what it decoded and a fallback model hashes to something else
   than the key that was asked for, so the read fails loudly as a corrupt store
   rather than silently returning [init]. *)
let framed (type a) (inner : a Repr.t) (fallback : a) : a Repr.t =
  let enc = Repr.unstage (Repr.to_bin_string inner) in
  let dec = Repr.unstage (Repr.of_bin_string inner) in
  Repr.map Repr.string
    (fun (s : string) -> Result.fold (dec s) ~ok:Fun.id ~error:(fun (_ : [ `Msg of string ]) -> fallback))
    enc

(* THE STORE IDENTITY TOKEN (roadmap step 18, D23, R20a).

   Free-standing on purpose: not folded into [create]/[open_root] and not added
   to [Store_core.t], because those signatures are load-bearing at every
   existing call site and [Session_secret.resolve] is the closer precedent - one
   synchronous resolution the composition site sequences itself. Nothing here
   depends on the app, so it lives at the top level and is [include]d into the
   functor rather than rebuilt per instantiation. *)
module Identity = struct
  module Store_identity = Tea_core.Prim.Store_identity

  type identity_origin =
    | Read
    | Minted
    | Adopted
    | Absent_unmintable of string
    | Unreadable of string
    | Malformed

  let identity_file : string = "tea.identity"

  let identity_path (root : Root.t) : string =
    Filename.concat (Root.to_string root) identity_file

  (* 32 hex characters plus, generously, a trailing newline an editor added.
     Hygiene in the spirit of [read_secret_file]'s 4096 cap: the token is fixed
     width, so anything larger is not one. *)
  let max_identity_bytes : int = 64
  let token_bytes : int = 16
  let salt_bytes : int = 4
  let entropy_source : string = "/dev/urandom"
  let create_flags : Unix.open_flag list = [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ]

  let unix_reason (e : exn) : string =
    match e with
    | Unix.Unix_error ((err : Unix.error), (fn : string), (arg : string)) ->
      Printf.sprintf "%s %s: %s" fn arg (Unix.error_message err)
    | (other : exn) -> Printexc.to_string other

  (* The one stdlib boundary in this module, and the only [try] in it: the house
     rule bans raising, not catching what Unix raises at the FFI edge.
     Everything above it is [result] and combinators. *)
  let guard : (unit -> 'a) -> ('a, string) result =
   fun (f : unit -> 'a) -> try Ok (f ()) with (e : exn) -> Error (unix_reason e)

  let is_regular (kind : Unix.file_kind) : bool =
    match kind with
    | Unix.S_REG -> true
    | Unix.S_DIR | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO | Unix.S_SOCK -> false

  (* Exactly one trailing newline, and only when it is the last byte: located
     with [rindex_opt] and dropped by consuming the string as a sequence, so no
     range is computed. Anything else is left in place, and a token with junk
     around it stays malformed rather than being trimmed into a value. *)
  let strip_nl (raw : string) : string =
    String.rindex_opt raw '\n'
    |> Option.fold ~none:raw ~some:(fun (i : int) ->
           if Int.equal i (String.length raw - 1) then
             String.to_seq raw |> Seq.take i |> String.of_seq
           else raw)

  (* Absence is the caller's mint arm rather than a refusal, so ENOENT is
     separated here instead of being recovered from a rendered message later. *)
  let lstat_opt (path : string) : (Unix.stats option, string) result =
    try Ok (Some (Unix.lstat path)) with
    | Unix.Unix_error (Unix.ENOENT, (_ : string), (_ : string)) -> Ok None
    | (e : exn) -> Error (unix_reason e)

  (* [lstat], not [stat]: a symlink standing in for the token is exactly the
     substitution this refuses, and following it would report the target's kind
     ([read_secret_file]'s rule, DESIGN R14). [Error None] means "there is no
     token yet" and is the ONLY arm that may mint; every [Error (Some _)] leaves
     the bytes exactly as found.

     No permission-bit check, and the file is mode 0644: the token confers no
     authority, and [<root>.guard/journal] beside it is already world-readable
     by R14's own accounting, so 0600 here would be a false promise about a
     value whose confidentiality is not load-bearing. *)
  let read_identity (path : string) : (Store_identity.t, identity_origin option) result =
    Result.bind
      (lstat_opt path
      |> Result.map_error (fun (reason : string) -> Some (Unreadable reason)))
      (fun (found : Unix.stats option) ->
        Option.fold found ~none:(Error None) ~some:(fun (st : Unix.stats) ->
            if not (is_regular st.Unix.st_kind) then
              Error (Some (Unreadable "not a regular file"))
            else if st.Unix.st_size > max_identity_bytes then
              Error (Some (Unreadable (Printf.sprintf "oversized (%d bytes)" st.Unix.st_size)))
            else
              Result.bind
                (guard (fun () -> In_channel.with_open_bin path In_channel.input_all)
                |> Result.map_error (fun (reason : string) -> Some (Unreadable reason)))
                (fun (raw : string) ->
                  Store_identity.of_string (strip_nl raw)
                  |> Result.map_error (fun (_ : Store_identity.err) -> Some Malformed))))

  let rec fill (fd : Unix.file_descr) (buf : bytes) ~(want : int) ~(got : int) :
      (unit, string) result =
    if got >= want then Ok ()
    else
      Result.bind (guard (fun () -> Unix.read fd buf got (want - got))) (fun (n : int) ->
          if n > 0 then fill fd buf ~want ~got:(got + n)
          else
            Error
              (Printf.sprintf "short read from %s (%d of %d bytes)" entropy_source got want))

  (* Exactly [n] bytes from the kernel, or nothing at all. There is deliberately
     no weaker fallback source: the one property this value must have is that
     two independently created roots never collide, and a degraded draw is how a
     constant token gets in. *)
  let draw_bytes (n : int) : (int list, string) result =
    Result.bind
      (guard (fun () -> Unix.openfile entropy_source [ Unix.O_RDONLY ] 0o000))
      (fun (fd : Unix.file_descr) ->
        let buf = Bytes.create n in
        let filled = fill fd buf ~want:n ~got:0 in
        let (_ : (unit, string) result) = guard (fun () -> Unix.close fd) in
        Result.map
          (fun () ->
            Bytes.to_string buf |> String.to_seq |> Seq.map Char.code |> List.of_seq)
          filled)

  let hex_of (bytes : int list) : string =
    String.concat "" (List.map (fun (b : int) -> Printf.sprintf "%02x" b) bytes)

  (* [link], not [rename]: two boots racing on one root must AGREE, and a rename
     would overwrite the winner and hand one store two identities. [link] fails
     EEXIST instead, so the loser re-reads bytes that were already complete
     before they had that name, exactly once and without retrying. *)
  let link_or_adopt ~(path : string) ~(tmp : string) (id : Store_identity.t) :
      (Store_identity.t * identity_origin, identity_origin) result =
    match Unix.link tmp path with
    | () -> Ok (id, Minted)
    | exception Unix.Unix_error (Unix.EEXIST, (_ : string), (_ : string)) ->
      read_identity path
      |> Result.fold
           ~ok:(fun (winner : Store_identity.t) -> Ok (winner, Adopted))
           ~error:(fun (refusal : identity_origin option) ->
             (* [EEXIST] PROVED the winner's file is there, so a readback that
                refuses it is [Unreadable] or [Malformed] - its own origin,
                carried through rather than relabelled. Only a file that
                vanished between the [link] and the read has no origin of its
                own, and that one really is absent. *)
             Error
               (Option.value refusal
                  ~default:
                    (Absent_unmintable
                       (Printf.sprintf
                          "another boot published %s first and it was gone again before it \
                           read back"
                          path))))
    | exception (e : exn) -> Error (Absent_unmintable (unix_reason e))

  (* The bytes land in a private [O_EXCL] staging file beside the target and are
     fsynced while still nameless to any reader, so no boot can see a zero-byte
     token. The staging name carries BOTH the pid and fresh entropy, for
     [Session_secret]'s stated reason: forked siblings share PRNG state, and a
     recycled pid collides with a staging file a killed boot left behind. *)
  let publish ~(path : string) ~(tmp : string) (id : Store_identity.t) :
      (Store_identity.t * identity_origin, identity_origin) result =
    let hex = Store_identity.to_string id in
    let want = String.length hex in
    Result.bind
      (guard (fun () -> Unix.openfile tmp create_flags 0o644)
      |> Result.map_error (fun (reason : string) -> Absent_unmintable reason))
      (fun (fd : Unix.file_descr) ->
        let staged =
          guard (fun () ->
              let n = Unix.write_substring fd hex 0 want in
              Unix.fsync fd;
              n)
        in
        let (_ : (unit, string) result) = guard (fun () -> Unix.close fd) in
        let published =
          Result.bind
            (staged |> Result.map_error (fun (reason : string) -> Absent_unmintable reason))
            (fun (n : int) ->
              if Int.equal n want then link_or_adopt ~path ~tmp id
              else
                Error
                  (Absent_unmintable (Printf.sprintf "short write (%d of %d bytes)" n want)))
        in
        (* Scoped to the branch whose [O_EXCL] open succeeded, the only proof
           this process created [tmp]. Its own failure is not worth reporting
           over the result it would hide. *)
        let (_ : (unit, string) result) = guard (fun () -> Unix.unlink tmp) in
        published)

  (* [of_bytes] declines only on a count or a range violation, and this call
     site fixes both; the impossible arm becomes an [Error] string rather than
     an assert, so the function stays total by construction. *)
  let mint (path : string) : (Store_identity.t * identity_origin, identity_origin) result =
    let unmintable (reason : string) : identity_origin = Absent_unmintable reason in
    Result.bind
      (draw_bytes token_bytes |> Result.map_error unmintable)
      (fun (drawn : int list) ->
        Result.bind
          (Store_identity.of_bytes drawn
          |> Option.to_result
               ~none:
                 (unmintable
                    (Printf.sprintf "%d drawn bytes are not a token" (List.length drawn))))
          (fun (id : Store_identity.t) ->
            Result.bind
              (draw_bytes salt_bytes |> Result.map_error unmintable)
              (fun (salt : int list) ->
                let tmp =
                  Printf.sprintf "%s.tmp.%d.%s" path (Unix.getpid ()) (hex_of salt)
                in
                publish ~path ~tmp id)))

  (* The failure origin is whatever the mint path decided, never a relabel:
     the adopt race's readback keeps [Unreadable]/[Malformed], everything else
     is genuinely [Absent_unmintable]. *)
  let mint_arm (path : string) () : Store_identity.binding * identity_origin =
    mint path
    |> Result.fold
         ~ok:(fun (((id : Store_identity.t), (origin : identity_origin)) :
                    Store_identity.t * identity_origin) ->
           (Store_identity.Bound id, origin))
         ~error:(fun (origin : identity_origin) -> (Store_identity.Unresolved, origin))

  let resolve_identity (root : Root.t) : Store_identity.binding * identity_origin =
    let path = identity_path root in
    read_identity path
    |> Result.fold
         ~ok:(fun (id : Store_identity.t) -> (Store_identity.Bound id, Read))
         ~error:(fun (refusal : identity_origin option) ->
           (* [Option.fold]'s [~none] is EAGER, so both arms are thunks and
              exactly one of them runs: minting on the refusal path would create
              the very file this function promises never to touch. *)
           Option.fold ~none:(mint_arm path)
             ~some:(fun (origin : identity_origin) () ->
               (Store_identity.Unresolved, origin))
             refusal
             ())

  (* One sentence per constructor, matched HERE so a new arm is a compile error
     in one place ([explain]'s rule). Each failure sentence names the
     consequence the constructor alone does not show, and it names it
     CONDITIONALLY, because an unresolved token does not do one thing to every
     journal: a journal that CARRIES a binding is held and its floors are
     cleared this boot, while a journal WITHOUT one keeps its floors and stays
     unbound. A flat "every journal's floors are cleared" is false on the
     second half. *)
  let unresolved_effect : string =
    "guard journals that carry a store-identity binding are held and their floors cleared \
     this boot (the affected replays re-apply as visible duplicates); journals carrying no \
     binding keep their floors but stay UNBOUND, because there is no token to stamp them \
     with"

  let explain_identity (o : identity_origin) : string =
    match o with
    | Read ->
      Printf.sprintf "read the store identity token from %s in the pack root" identity_file
    | Minted ->
      Printf.sprintf "minted a store identity token into %s (this root carried none)"
        identity_file
    | Adopted ->
      Printf.sprintf
        "adopted the %s a concurrent boot published first, so both booted on one identity for one store"
        identity_file
    | Absent_unmintable (reason : string) ->
      Printf.sprintf
        "the pack root carries no %s and one could not be published (%s); %s, until the token can be written"
        identity_file reason unresolved_effect
    | Unreadable (reason : string) ->
      Printf.sprintf
        "the store identity token %s is present but could not be read (%s); it is left exactly as found, and %s, until it reads"
        identity_file reason unresolved_effect
    | Malformed ->
      Printf.sprintf
        "the store identity token %s is not 32 lowercase hex characters; it is left exactly as found, and %s, until it reads"
        identity_file unresolved_effect
end

module Make (A : Tea_core.App.APP) = struct
  module Contents = struct
    include Tea_persist.Store_core.Contents (A)

    (* Same [type t = A.model] and same [merge]; only the wire framing of a
       stored entry differs from the mem backend's. *)
    let t : t Repr.t = framed A.model_t (fst A.init)
  end

  module Maker_kv = Irmin_pack_unix.KV (Conf)
  module Pack = Maker_kv.Make (Contents)
  include Tea_persist.Store_core.Make (A) (Pack)

  (* The commit fence (roadmap step 20, R11): [Pack.flush] forces irmin-pack's
     still-buffered suffix/dict bytes into the page cache, so a guard floor
     appended AFTER this resolves cannot reach the page cache before the
     commit bytes it witnesses. irmin-pack 3.11 already ends every commit
     batch in [File_manager.flush] (unix/store.ml [batch] on_success), so on
     the pump path this meets a clean buffer and is a no-op — it exists
     because no interface PROMISES that batch-end flush, and an upstream that
     dropped it must not be able to silently reopen R11. Page-cache arrival
     only — [use_fsync] stays false, so power loss stays out of scope. A
     failed flush REJECTS rather than degrade to stderr here: the one
     production composer, [Durable_guard.persist], runs the fence inside its
     own catch, where the rejection becomes [Error (Io _)] with the floor
     never appended — an audible, at-least-once persist, the duplicate
     direction. Swallowing the failure would append a floor over commit
     bytes the flush just failed to order, which is R11's loss direction at
     exactly the moment the fence exists for. *)
  let flush (t : t) : unit Lwt.t =
    Pack.flush (repo t);
    Lwt.return_unit

  (* ALWAYS minimal indexing: the default [always] strategy permanently
     poisons the root against delete-mode GC. And deliberately no [?fresh]:
     reopening must never silently truncate a durable store.

     [?lower_root] (D5): when set, the config carries a lower-layer directory,
     which flips [Gc.behaviour] to [`Archive] — GC then moves discarded data to
     the lower layer instead of deleting it, so pre-checkpoint commits stay
     readable. [Irmin_pack.config] cannot carry a lower root, so the config is
     built through [Irmin_pack.Conf.init] (a superset with the same defaults);
     the raising [add_volume]/[split] volume API is never touched. *)
  let create ?(now = default_now) ?lower_root ?exploded (root : Root.t) : t Lwt.t =
    let config =
      Irmin_pack.Conf.init
        ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal
        ~lower_root
        (Root.to_string root)
    in
    Lwt.bind (Pack.Repo.v config) (v ~now ?exploded)

  type open_error =
    | Root_parent_missing of string
    | Root_not_a_directory of string
    | Root_not_a_pack_store of string
    | Backend_failed of string

  (* The preflight cannot be complete (irmin-pack's failures are an open
     polymorphic variant, and the filesystem can change between classify and
     open), so the [Lwt.catch] seam is load-bearing even when every preflight
     arm passes: it is what turns the unclassifiable residue into a value
     instead of a process kill. *)
  let open_root ?now ?lower_root ?exploded (root : Root.t) :
      (t, open_error) result Lwt.t =
    let s = Root.to_string root in
    let classify = Irmin_pack_unix.Io.Unix.classify_path in
    let is_file (path : string) : bool =
      match classify path with
      | `File -> true
      | `Directory | `No_such_file_or_directory | `Other -> false
    in
    let attempt () : (t, open_error) result Lwt.t =
      Lwt.catch
        (fun () -> Lwt.map Result.ok (create ?now ?lower_root ?exploded root))
        (fun (exn : exn) ->
          Lwt.return (Error (Backend_failed (Printexc.to_string exn))))
    in
    match classify s with
    | `File | `Other -> Lwt.return (Error (Root_not_a_directory s))
    | `No_such_file_or_directory -> (
      (* The backend does exactly one [mkdir], so the parent must already be
         a directory for the create path to succeed. *)
      match classify (Filename.dirname s) with
      | `Directory -> attempt ()
      | `File | `Other | `No_such_file_or_directory ->
        Lwt.return (Error (Root_parent_missing (Filename.dirname s))))
    | `Directory ->
      (* An existing directory is a pack root only if it carries a control
         file (v3+) or a bare pack file (v1/v2, kept openable so irmin-pack's
         auto-migration stays reachable). Anything else - notably an EMPTY
         existing directory, the exact shape [mkdtemp] hands out - is the
         Invalid_layout raise waiting to happen. *)
      if
        is_file (Irmin_pack.Layout.V4.control ~root:s)
        || is_file (Irmin_pack.Layout.V1_and_v2.pack ~root:s)
      then attempt ()
      else Lwt.return (Error (Root_not_a_pack_store s))

  (* One sentence per constructor, and the match lives HERE so a new
     constructor is a compile error in one place rather than a caller that
     silently renders it as something else. Each arm names the path it carries,
     because the payloads are not all the same kind of path: [Root_parent_missing]
     carries the PARENT, and a caller that pasted every payload into one "pack
     root unusable (%s)" template would name a directory the operator never
     configured. The two directory refusals also want opposite remedies, so
     they must not render alike. *)
  let explain (e : open_error) : string =
    match e with
    | Root_parent_missing parent ->
      Printf.sprintf "its parent directory %s does not exist (create it, or point TEA_ROOT elsewhere)" parent
    | Root_not_a_directory path ->
      Printf.sprintf "%s exists and is not a directory (remove it, or point TEA_ROOT elsewhere)" path
    | Root_not_a_pack_store path ->
      Printf.sprintf
        "%s is an existing directory with no pack store in it (point TEA_ROOT at the store, or at a path that does not exist yet so it can be created)"
        path
    | Backend_failed reason -> Printf.sprintf "irmin-pack refused to open it (%s)" reason

  (* [identity_path], [resolve_identity] and [explain_identity], re-exported
     from the app-independent [Identity] above: the resolver is reachable as
     [Store.resolve_identity] at the composition site without the functor
     rebuilding one closure of it per app. *)
  include Identity

  (** Whether GC on this store archives discarded data to a lower layer
      ([`Archive], a [lower_root] was configured) or deletes it ([`Delete]).
      Exposed so archive retention is observable without running a GC. *)
  let gc_behaviour (t : t) : [ `Archive | `Delete ] = Pack.Gc.behaviour (repo t)

  type gc_error =
    | Gc_disallowed
    | Gc_already_running
    | Gc_failed of string

  (** [run] + [wait], never [start_exn]/[finalise_exn]: the result stays in
      [result] (house no-exceptions rule) and blocking until finalisation
      keeps tests deterministic. *)
  let gc (t : t) ~(retain : checkpoint) : (unit, gc_error) result Lwt.t =
    let r = repo t in
    if not (Pack.Gc.is_allowed r) then Lwt.return (Error Gc_disallowed)
    else
      let key = Pack.Commit.key (checkpoint_commit retain) in
      let* started = Pack.Gc.run r key in
      match started with
      | Error (`Msg m) -> Lwt.return (Error (Gc_failed m))
      | Ok false -> Lwt.return (Error Gc_already_running)
      | Ok true -> (
        let* finished = Pack.Gc.wait r in
        match finished with
        | Error (`Msg m) -> Lwt.return (Error (Gc_failed m))
        | Ok (_ : Irmin_pack_unix.Stats.Latest_gc.stats option) -> Lwt.return (Ok ()))
end
