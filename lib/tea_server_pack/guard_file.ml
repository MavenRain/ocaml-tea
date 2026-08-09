module Guard_sink = Tea_server.Guard_sink
module Durable_guard = Tea_server.Durable_guard
module Store_identity = Tea_core.Prim.Store_identity

module Key = struct
  type t = Tea_core.Crdt.Replica.t * Tea_core.Prim.Tab_id.t

  let compare ((r1, t1) : t) ((r2, t2) : t) : int =
    let c = Tea_core.Crdt.Replica.compare r1 r2 in
    if Int.equal c 0 then Tea_core.Prim.Tab_id.compare t1 t2 else c
end

module Key_map = Map.Make (Key)
module Rep_map = Map.Make (Tea_core.Crdt.Replica)
open Lwt.Syntax

type open_err =
  | Io of string
  | Bad_dir of string

type verdict = Durable_guard.Floors.verdict =
  { kept : int
  ; dropped_behind : int
  ; dropped_no_branch : int
  ; unwitnessed : int
  }

type t =
  { path : string
  ; cap : int
  ; mutable fd : Lwt_unix.file_descr
  ; mutable chan : Lwt_io.output_channel
  ; mutable count : int (* records currently in the file *)
  ; mutable floors : Durable_guard.Floors.t (* what compaction rewrites *)
  ; mutable touch : int Key_map.t (* last-append tick per live key *)
  ; mutable tick : int
  ; mutable header : Store_identity.t option (* stamped as frame 0 *)
  ; held : bool (* the strict hold: no write path may touch the file *)
  ; mutable closed : bool
  }

(* The store-identity header (roadmap step 18, D23): tag 4, the file's first
   frame or absent. It is framed by {!Guard_sink.Codec} but decoded HERE, so
   it is never a [Guard_sink.event] and cannot reach the floors fold, the
   [Key_map] or cap eviction. *)
let tag_identity = '\004'

type identity_outcome =
  | Matched
  | Freshly_bound
  | Adopted_unbound of int
  | Rebound of int
  | Unresolved_cleared of int

type header_status =
  | No_header of { empty : bool }
      (* [empty=true]: the file holds ZERO bytes, the only shape that carries
         no claim at all. [empty=false]: the file holds bytes but no header
         frame - a genuine event frame at byte 0, or bytes that would not
         unframe there at all. *)
  | Foreign_header of { consumed : int }
      (* A header frame that unframed cleanly and carries a payload that is
         not a token. [consumed] is the offset of the first frame after it. *)
  | Header of
      { at : Store_identity.t
      ; consumed : int (* offset of the first frame after the header *)
      }

(* Three answers, and what separates them is what the caller may then do. A
   header payload that will not decode is DECIDABLE: it is not 32 lowercase
   hex, so it equals no store's token and it names a different store just as
   surely as a readable stranger does, which is why it keeps its frame
   boundary and reaches the mismatch arm. Bytes that will not unframe at byte
   0 say nothing about whose store they are, so they stay [No_header] and are
   adopted on trust - but they are still BYTES, so they are not [empty], and
   the silent brand-new-store verdict stays unique to a zero-byte file. *)
let decode_header (s : string) : header_status =
  if Int.equal (String.length s) 0 then No_header { empty = true }
  else
    Result.fold
      (Guard_sink.Codec.unframe s ~pos:0)
      ~error:(fun (_ : Guard_sink.Codec.decode_err) -> No_header { empty = false })
      ~ok:(fun ((tag, payload, next) : char * string * int) ->
        if Char.equal tag tag_identity then
          Store_identity.of_string payload
          |> Result.fold
               ~ok:(fun (at : Store_identity.t) -> Header { at; consumed = next })
               ~error:(fun (_ : Store_identity.err) ->
                 Foreign_header { consumed = next })
        else No_header { empty = false })

(* Decode the valid prefix: every [decode_err] — torn, corrupt, unknown —
   stops the fold and discards the remainder. Frames before the break
   survive; a lost suffix only lowers floors, which is the accept
   direction. *)
let decode_prefix (s : string) : Guard_sink.event list * int =
  let rec go (pos : int) (acc : Guard_sink.event list) (n : int) :
      Guard_sink.event list * int =
    if pos >= String.length s then (List.rev acc, n)
    else
      Result.fold
        (Guard_sink.Codec.of_bytes s ~pos)
        ~ok:(fun ((e, next) : Guard_sink.event * int) -> go next (e :: acc) (n + 1))
        ~error:(fun (_ : Guard_sink.Codec.decode_err) -> (List.rev acc, n))
  in
  go 0 [] 0

let scrub_replica (replica : Tea_core.Crdt.Replica.t) (touch : int Key_map.t) :
    int Key_map.t =
  Key_map.filter
    (fun ((r, (_ : Tea_core.Prim.Tab_id.t)) : Key.t) (_ : int) ->
      not (Tea_core.Crdt.Replica.equal r replica))
    touch

let touch_of_events (events : Guard_sink.event list) : int Key_map.t * int =
  List.fold_left
    (fun ((touch, tick) : int Key_map.t * int) (e : Guard_sink.event) ->
      match e with
      | Guard_sink.Advance
          { replica
          ; tab
          ; seq = (_ : Tea_core.Prim.Msg_seq.t)
          ; water = (_ : Tea_core.Prim.Store_water.t)
          } ->
        (Key_map.add (replica, tab) tick touch, tick + 1)
      | Guard_sink.Forget { replica } -> (scrub_replica replica touch, tick + 1))
    (Key_map.empty, 0) events

(* The live floors as Advance records in ascending last-touch order, so a
   future open's drop-oldest sees the same age order this process did —
   rewritten at their stored waters, so no rewrite can weaken or forge a
   floor's witness. *)
let events_of_kept (floors : Durable_guard.Floors.t) (touch : int Key_map.t) :
    Guard_sink.event list =
  Key_map.bindings touch
  |> List.sort
       (fun (((_ : Key.t), a) : Key.t * int) (((_ : Key.t), b) : Key.t * int) ->
         Int.compare a b)
  |> List.filter_map (fun (((replica, tab), (_ : int)) : Key.t * int) ->
         Durable_guard.Floors.find_stamped ~replica ~tab floors
         |> Option.map
              (fun
                ((seq, water) :
                  Tea_core.Prim.Msg_seq.t * Tea_core.Prim.Store_water.t)
              -> Guard_sink.Advance { replica; tab; seq; water }))

(* Drop-oldest beyond [cap]: victims are the excess oldest keys by last
   append; the survivors' floors are rebuilt through the same
   [Floors.of_events] fold every other floor value passes through. *)
let enforce_cap ~(cap : int) (floors : Durable_guard.Floors.t)
    (touch : int Key_map.t) : Durable_guard.Floors.t * int Key_map.t =
  let excess = Key_map.cardinal touch - cap in
  if excess <= 0 then (floors, touch)
  else
    let victims =
      Key_map.bindings touch
      |> List.sort
           (fun (((_ : Key.t), a) : Key.t * int) (((_ : Key.t), b) : Key.t * int) ->
             Int.compare a b)
      |> List.filteri (fun (i : int) ((_ : Key.t), (_ : int)) -> i < excess)
      |> List.map fst
    in
    let touch' = List.fold_left (Fun.flip Key_map.remove) touch victims in
    (Durable_guard.Floors.of_events (events_of_kept floors touch'), touch')

let append_flags : Unix.open_flag list = [ Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT ]

let compact_now (t : t) : unit Lwt.t =
  let kept = events_of_kept t.floors t.touch in
  (* Stamped by the rewrite, so header and floors are always the same
     generation and the header can no more be torn on the live file than the
     first kept floor can. [None] emits nothing at all, which is the
     pre-step-18 file byte for byte. *)
  let header =
    Option.fold t.header ~none:"" ~some:(fun (id : Store_identity.t) ->
        Guard_sink.Codec.frame ~tag:tag_identity
          ~payload:(Store_identity.to_string id))
  in
  let tmp = t.path ^ ".tmp" in
  let* () =
    Lwt_io.with_file ~mode:Lwt_io.Output
      ~flags:[ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
      tmp
      (fun out ->
        let* () = Lwt_io.write out header in
        Lwt_list.iter_s
          (fun (ev : Guard_sink.event) ->
            Lwt_io.write out (Guard_sink.Codec.to_bytes ev))
          kept)
  in
  (* [Lwt_io.close] flushes and closes the underlying fd; the rename is the
     atomic cut-over. No fsync here: a crash mid-compaction can lose recent
     floors, which replays as duplicates — the accept direction, same as a
     torn tail. *)
  let* () = Lwt_io.close t.chan in
  let* () = Lwt_unix.rename tmp t.path in
  let* fd = Lwt_unix.openfile t.path append_flags 0o644 in
  t.fd <- fd;
  t.chan <- Lwt_io.of_fd ~mode:Lwt_io.Output fd;
  t.count <- List.length kept;
  let touch, tick = touch_of_events kept in
  t.touch <- touch;
  t.tick <- tick;
  Lwt.return_unit

(* EVERY rewrite goes through here, so the strict hold cannot be forgotten at
   a call site: a held journal answers the trigger and writes nothing. *)
let compact (t : t) : unit Lwt.t =
  if t.held then Lwt.return_unit else compact_now t

(* The in-memory half of an append: the floors, the touch clock, and cap
   eviction. It touches no byte on disk, so a held journal runs exactly this
   and nothing more, and this boot's own dedup keeps working. *)
let remember (t : t) (e : Guard_sink.event) : unit =
  t.floors <- Durable_guard.Floors.apply t.floors e;
  (match e with
   | Guard_sink.Advance
       { replica
       ; tab
       ; seq = (_ : Tea_core.Prim.Msg_seq.t)
       ; water = (_ : Tea_core.Prim.Store_water.t)
       } ->
     t.touch <- Key_map.add (replica, tab) t.tick t.touch
   | Guard_sink.Forget { replica } -> t.touch <- scrub_replica replica t.touch);
  t.tick <- t.tick + 1;
  if Key_map.cardinal t.touch > t.cap then (
    let floors, touch = enforce_cap ~cap:t.cap t.floors t.touch in
    t.floors <- floors;
    t.touch <- touch)

let append (t : t) (e : Guard_sink.event) : (unit, Guard_sink.err) result Lwt.t =
  if t.closed then Lwt.return (Error Guard_sink.Sink_closed)
  else if t.held then (
    (* The strict hold, the write half (see [open_]): the floors this boot
       learns stay in memory and reach no byte of the file, so a later boot
       that CAN read the token still finds the original header and its
       floors. The appends that are not persisted replay as duplicates on
       that boot, which is the accept direction. *)
    remember t e;
    Lwt.return (Ok ()))
  else
    Lwt.catch
      (fun () ->
        let* () = Lwt_io.write t.chan (Guard_sink.Codec.to_bytes e) in
        let* () = Lwt_io.flush t.chan in
        t.count <- t.count + 1;
        remember t e;
        let* () = if t.count > 4 * t.cap then compact t else Lwt.return_unit in
        Lwt.return (Ok ()))
      (fun (exc : exn) ->
        Lwt.return (Error (Guard_sink.Io (Printexc.to_string exc))))

let ensure_dir (dir : string) : (unit, open_err) result Lwt.t =
  Lwt.catch
    (fun () ->
      let* () = Lwt_unix.mkdir dir 0o755 in
      Lwt.return (Ok ()))
    (fun (exc : exn) ->
      match exc with
      | Unix.Unix_error (Unix.EEXIST, (_ : string), (_ : string)) ->
        Lwt.return (Ok ())
      | exc -> Lwt.return (Error (Bad_dir (Printexc.to_string exc))))

let read_if_exists (path : string) : string Lwt.t =
  Lwt.catch
    (fun () -> Lwt_io.with_file ~mode:Lwt_io.Input path Lwt_io.read)
    (fun (exc : exn) ->
      match exc with
      | Unix.Unix_error (Unix.ENOENT, (_ : string), (_ : string)) -> Lwt.return ""
      | exc -> Lwt.fail exc)

(* Build the lookup once, close over it: a boot consults it once per floor
   key, and a list scan per key would be quadratic in live sessions. *)
let head_water_of_list
    (waters : (Tea_core.Crdt.Replica.t * Tea_core.Prim.Store_water.t) list) :
    Tea_core.Crdt.Replica.t -> Tea_core.Prim.Store_water.t option =
  let by_replica =
    List.fold_left
      (fun (acc : Tea_core.Prim.Store_water.t Rep_map.t)
           ((replica, water) :
             Tea_core.Crdt.Replica.t * Tea_core.Prim.Store_water.t) ->
        Rep_map.add replica water acc)
      Rep_map.empty waters
  in
  fun (replica : Tea_core.Crdt.Replica.t) -> Rep_map.find_opt replica by_replica

let open_ ~(dir : string) ~(cap : int)
    ~(head_water :
       Tea_core.Crdt.Replica.t -> Tea_core.Prim.Store_water.t option)
    ~(identity : Store_identity.binding) :
    ( Guard_sink.t * Durable_guard.Floors.t * verdict * identity_outcome * t
    , open_err )
    result
    Lwt.t =
  let* made = ensure_dir dir in
  Result.fold made
    ~error:(fun (e : open_err) -> Lwt.return (Error e))
    ~ok:(fun () ->
      Lwt.catch
        (fun () ->
          let path = Filename.concat dir "journal" in
          let* contents = read_if_exists path in
          (* Peel before the prefix fold, so the header is structurally
             incapable of becoming an event. The remainder is dropped through
             a seq rather than a computed range: the peel is then total
             whatever offset the header reports, and the header never counts
             as a record, so the [4 * cap] trigger measures what it always
             did. *)
          let hdr = decode_header contents in
          let rest =
            match hdr with
            | No_header { empty = (_ : bool) } -> contents
            | Header { at = (_ : Store_identity.t); consumed }
            | Foreign_header { consumed } ->
              String.to_seq contents |> Seq.drop consumed |> String.of_seq
          in
          let events, count = decode_prefix rest in
          (* Today's path, unchanged: the boot filter (R20) runs on the FULL
             fold, before any cap housekeeping, so the verdict reports what
             the journal claimed against what the store holds, not what
             happened to fit under [cap]. Touch entries for refused floors go
             with them, or the cap would count — and could evict a live key in
             favour of — keys that no longer exist. *)
          let adopt () :
              Durable_guard.Floors.t * int Key_map.t * int * verdict =
            let floors0 = Durable_guard.Floors.of_events events in
            let admitted, verdict =
              Durable_guard.Floors.filter ~head:head_water floors0
            in
            let touch0, tick = touch_of_events events in
            let touch1 =
              Key_map.filter
                (fun ((replica, tab) : Key.t) (_ : int) ->
                  Durable_guard.Floors.find_stamped ~replica ~tab admitted
                  |> Option.is_some)
                touch0
            in
            let floors, touch = enforce_cap ~cap admitted touch1 in
            (floors, touch, tick, verdict)
          in
          (* Decode far enough to COUNT what a stranger claimed, then discard
             it outright. The cleared arms report the all-zero verdict rather
             than synthetic [dropped_behind] counts: there is nothing
             store-water-related to say, and forcing a number into those
             fields is the mislabel the separate [identity_outcome] exists to
             refuse. *)
          let stranger_count () : int =
            Durable_guard.Floors.of_events events
            |> Durable_guard.Floors.cardinal
          in
          let cleared :
              Durable_guard.Floors.t * int Key_map.t * int * verdict =
            ( Durable_guard.Floors.empty
            , Key_map.empty
            , 0
            , { kept = 0
              ; dropped_behind = 0
              ; dropped_no_branch = 0
              ; unwitnessed = 0
              } )
          in
          (* A total function of (header, binding). [Some id] is only ever a
             value the caller bound or a header this open actually decoded,
             never one this module invented. *)
          let outcome, (floors, touch, tick, verdict), header =
            match (hdr, identity) with
            | No_header { empty = true }, Store_identity.Bound id ->
              (Freshly_bound, adopt (), Some id)
            | No_header { empty = true }, Store_identity.Unresolved ->
              (Freshly_bound, adopt (), None)
            | No_header { empty = false }, Store_identity.Bound id ->
              let ((_ : Durable_guard.Floors.t), (_ : int Key_map.t), (_ : int), v) as
                  adopted =
                adopt ()
              in
              (Adopted_unbound v.kept, adopted, Some id)
            | No_header { empty = false }, Store_identity.Unresolved ->
              let ((_ : Durable_guard.Floors.t), (_ : int Key_map.t), (_ : int), v) as
                  adopted =
                adopt ()
              in
              (Adopted_unbound v.kept, adopted, None)
            | Header { at; consumed = (_ : int) }, Store_identity.Bound id ->
              if Store_identity.equal at id then (Matched, adopt (), Some id)
              else (Rebound (stranger_count ()), cleared, Some id)
            | Header { at; consumed = (_ : int) }, Store_identity.Unresolved ->
              (Unresolved_cleared (stranger_count ()), cleared, Some at)
            (* A header frame whose payload is not a token names a store this
               one is not, exactly as a readable stranger token does, so the
               two share their arms. It leaves no value to stand in for the
               [Unresolved] hold's header, and it needs none: the hold rewrites
               nothing, so nothing reads that field. *)
            | Foreign_header { consumed = (_ : int) }, Store_identity.Bound id ->
              (Rebound (stranger_count ()), cleared, Some id)
            | Foreign_header { consumed = (_ : int) }, Store_identity.Unresolved ->
              (Unresolved_cleared (stranger_count ()), cleared, None)
          in
          (* [held] is what makes an unresolvable identity a strict no-op on
             disk, and it is a FIELD rather than a local because the hold has
             to outlive [open_]: [compact] and [append] both consult it, so no
             later write path can rewrite a file whose token this boot could
             not confirm. A later boot that CAN read the token therefore finds
             the original header intact and keeps its floors. *)
          let held =
            match outcome with
            | Unresolved_cleared (_ : int) -> true
            | Matched | Freshly_bound | Adopted_unbound (_ : int)
            | Rebound (_ : int) ->
              false
          in
          let* fd = Lwt_unix.openfile path append_flags 0o644 in
          let t =
            { path
            ; cap
            ; fd
            ; chan = Lwt_io.of_fd ~mode:Lwt_io.Output fd
            ; count
            ; floors
            ; touch
            ; tick
            ; header
            ; held
            ; closed = false
            }
          in
          (* A fired filter compacts IMMEDIATELY (step 13, revised in
             review): a refused record left in the bytes is re-adjudicated
             from scratch on every open, and the same branch head that
             stood BEHIND the stale floor today rises past it with one
             fresh wall-clock commit — un-dropping the floor and re-arming
             the exact silent loss the filter refused. This open's verdict
             still reports the drop; the rewrite makes it durable, so the
             next open finds nothing left to drop. A crash mid-compaction
             loses floors, which replays as duplicates — the accept
             direction, same as a torn tail. *)
          let dropped = verdict.dropped_behind + verdict.dropped_no_branch in
          (* [restamp] forces the write on every newly-bound arm rather than
             waiting for a natural trigger, the same argument the filter-fired
             compaction above makes: deferring buys nothing (one small frame
             through plumbing that already exists) and every boot between
             detection and the next natural compaction still reads as
             unbound. The held arm needs no test of its own here: [compact]
             answers a held journal with [unit] and writes nothing, which is
             what a stranger journal carrying more than [4 * cap] records
             relies on. *)
          let restamp =
            match outcome with
            | Matched | Unresolved_cleared (_ : int) -> false
            | Freshly_bound | Adopted_unbound (_ : int) | Rebound (_ : int) ->
              Option.is_some t.header
          in
          let* () =
            if dropped > 0 || count > 4 * cap || restamp then compact t
            else Lwt.return_unit
          in
          Lwt.return
            (Ok ({ Guard_sink.append = append t }, t.floors, verdict, outcome, t)))
        (fun (exc : exn) -> Lwt.return (Error (Io (Printexc.to_string exc)))))

let close (t : t) : unit Lwt.t =
  if t.closed then Lwt.return_unit
  else (
    t.closed <- true;
    Lwt.catch
      (fun () ->
        let* () = Lwt_io.flush t.chan in
        let* () = Lwt_unix.fsync t.fd in
        Lwt_io.close t.chan)
      (fun (exc : exn) ->
        Printf.eprintf "guard_file: close failed (%s)\n%!" (Printexc.to_string exc);
        Lwt.return_unit))
