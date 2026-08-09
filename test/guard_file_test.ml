(** The file-backed guard journal (roadmap step 11, D16).

    [replay_test] pins the pure verdicts; this file pins the one module in
    the guard stack that touches the filesystem, [Tea_server_pack.Guard_file],
    driven only through its public seams ([open_], the returned sink,
    [close]) plus raw byte surgery on the journal between lives.

    The property under test is the same asymmetry the guard is built on: a
    lost floor merely re-admits one duplicate (visible, convergent), while a
    floor that survives when it should not, or a reopen that refuses to
    serve, is the loss direction. So every hostile shape here (a torn tail, a
    flipped CRC byte mid-file, a journal of pure garbage) must degrade to
    [Ok] with fewer floors: never an exception, never a higher floor, never a
    dead server. The friendly shapes (reopen, the cap, compaction, a
    journalled [Forget]) must preserve floors exactly. *)

module Guard_file = Tea_server_pack.Guard_file
module Guard_sink = Tea_server.Guard_sink
module Floors = Tea_server.Durable_guard.Floors
module Msg_seq = Tea_core.Prim.Msg_seq
module Tab_id = Tea_core.Prim.Tab_id
module Store_identity = Tea_core.Prim.Store_identity
open Lwt.Syntax

(* --- Harness --------------------------------------------------------------- *)

(** One assertion: print TAP-ish, exit 1 on the first failure. *)
let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

(* [Option.fold]'s [~none:] is EAGER, so a failure branch written as a value
   runs on every call and this file would exit before its first check. Both
   branches here are closures and the application is the only thing that
   chooses, which is what makes a loud refusal safe. *)
let must (what : string) (o : 'a option) : 'a =
  Option.fold
    ~none:(fun () ->
      Printf.printf "FAIL - test setup: %s\n%!" what;
      exit 1)
    ~some:(fun x () -> x)
    o ()

(** A replica from a session-id name, the same mint the server uses. *)
let replica name = Tea_core.Crdt.Replica.v (Tea_core.Prim.Session_id.v name)

(** A tab id from a 16-byte seed, through the same [of_bytes] mint the browser
    runtime uses. *)
let tab (n : int) : Tab_id.t =
  must "tab id mint refused a valid seed"
    (Tab_id.of_bytes (List.init 16 (fun i -> (n + i) land 0xff)))

(** A validated sequence number. *)
let seq (n : int) : Msg_seq.t =
  must (Printf.sprintf "Msg_seq.of_int refused %d" n) (Msg_seq.of_int n)

(** The [Advance] shape, the only floor-raising record; the file mechanics
    under test are stamp-independent, so these carry [bottom]. *)
let advance (r : Tea_core.Crdt.Replica.t) (t : Tab_id.t) (n : int) :
    Guard_sink.event =
  Guard_sink.Advance
    { replica = r
    ; tab = t
    ; seq = seq n
    ; water = Tea_core.Prim.Store_water.bottom
    }

(** The [Forget] tombstone. *)
let forget (r : Tea_core.Crdt.Replica.t) : Guard_sink.event =
  Guard_sink.Forget { replica = r }

(** Spell an [open_err] for a setup-failure line. *)
let open_err_name (e : Guard_file.open_err) : string =
  match e with
  | Guard_file.Io s -> Printf.sprintf "Io %S" s
  | Guard_file.Bad_dir s -> Printf.sprintf "Bad_dir %S" s

(** Spell a [Guard_sink.err] for a setup-failure line. *)
let sink_err_name (e : Guard_sink.err) : string =
  match e with
  | Guard_sink.Sink_closed -> "Sink_closed"
  | Guard_sink.Io s -> Printf.sprintf "Io %S" s

(** Unwrap [open_] or refuse loudly. [Result.fold]'s arms are functions of
    the payload, applied once to whichever arm is present, so the failing
    branch cannot run eagerly. *)
let opened (what : string)
    (r :
      ( Guard_sink.t
        * Floors.t
        * Guard_file.verdict
        * Guard_file.identity_outcome
        * Guard_file.t
      , Guard_file.open_err )
      result) :
    Guard_sink.t
    * Floors.t
    * Guard_file.verdict
    * Guard_file.identity_outcome
    * Guard_file.t =
  Result.fold r ~ok:Fun.id
    ~error:(fun (e : Guard_file.open_err) ->
      Printf.printf "FAIL - test setup: %s (%s)\n%!" what (open_err_name e);
      exit 1)

(* One store for the whole file: every open below names the SAME identity, so
   the first open of each scratch journal stamps the header and every later
   open reads it back as [Matched]. That is what keeps the file mechanics
   under test exactly the mechanics that shipped - the identity question is
   guard_identity_test's subject, not this file's. *)
let store_id : Store_identity.t = Store_identity.of_draws (fun () -> 0xa5)
let identity : Store_identity.binding = Store_identity.Bound store_id

(* The boot filter's null input: no branches at all. Every floor this file
   writes carries [bottom] (a claim of nothing cannot be failed), so under
   [no_heads] the filter adopts everything and this file stays a test of the
   FILE mechanics; the filter itself is guard_water_test's subject. *)
let no_heads : Tea_core.Crdt.Replica.t -> Tea_core.Prim.Store_water.t option =
  Guard_file.head_water_of_list []

(** Append through the sink or refuse loudly: setup appends are premises of a
    test, not its assertion. *)
let put (what : string) (sink : Guard_sink.t) (e : Guard_sink.event) :
    unit Lwt.t =
  let* r = sink.Guard_sink.append e in
  Result.fold r
    ~ok:(fun () -> Lwt.return_unit)
    ~error:(fun (err : Guard_sink.err) ->
      Printf.printf "FAIL - test setup: append (%s) failed (%s)\n%!" what
        (sink_err_name err);
      exit 1)

(** The floor for [(r, t)] is exactly [is]. *)
let floor_at (fl : Floors.t) (r : Tea_core.Crdt.Replica.t) (t : Tab_id.t)
    ~(is : int) : bool =
  Floors.find ~replica:r ~tab:t fl
  |> Option.fold ~none:false ~some:(fun n -> Int.equal (Msg_seq.to_int n) is)

(** No floor at all for [(r, t)]. *)
let no_floor (fl : Floors.t) (r : Tea_core.Crdt.Replica.t) (t : Tab_id.t) :
    bool =
  Option.is_none (Floors.find ~replica:r ~tab:t fl)

(** Raw journal bytes, read with plain [Stdlib] channels: the hostile shapes
    are made below the codec, exactly where a crash or a bad disk makes them. *)
let read_file (path : string) : string =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(** Overwrite a file with raw bytes, the writing half of the byte surgery. *)
let write_file (path : string) (s : string) : unit =
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

(** On-disk size, for the compaction and never-truncate assertions. *)
let size_of (path : string) : int = (Unix.stat path).Unix.st_size

(** Recursive delete, the same cleanup shape as [pack_test]. *)
let rec rm_rf (path : string) : unit =
  if Sys.is_directory path then (
    Array.iter (fun entry -> rm_rf (Filename.concat path entry)) (Sys.readdir path);
    Sys.rmdir path)
  else Sys.remove path

(** Scratch root per test, the pack-test idiom: [Filename.temp_dir] creates
    the parent, the guard dir lives one component below ([open_] makes the
    final component), and the parent is removed on the way out. A FAIL exits
    before cleanup, exactly as the pack tests do. *)
let in_scratch (f : string -> unit Lwt.t) : unit =
  let parent = Filename.temp_dir "ocaml-tea-guard" "" in
  Lwt_main.run (f (Filename.concat parent "guard"));
  rm_rf parent

(** The journal file inside a guard dir, [Guard_file]'s one on-disk name. *)
let journal_of (dir : string) : string = Filename.concat dir "journal"

(* --- 1. Reopen round-trip: the module's whole reason to exist -------------- *)

let () =
  in_scratch (fun dir ->
      let* r0 = Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity in
      let ( sink
          , floors0
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , h ) =
        opened "fresh dir" r0 in
      check "a fresh dir opens with empty floors"
        (Int.equal (Floors.cardinal floors0) 0);
      let alpha = replica "alpha" and beta = replica "beta" in
      let* () = put "alpha tab1 seq3" sink (advance alpha (tab 1) 3) in
      let* () = put "alpha tab2 seq5" sink (advance alpha (tab 2) 5) in
      let* () = put "beta tab1 seq7" sink (advance beta (tab 1) 7) in
      (* A stale lower seq for a key already ahead: the floor must not move,
         and must still not have moved once the restart replays the journal
         (a replayed record re-opening a consumed seq is the loss path). *)
      let* () = put "alpha tab1 seq2 (stale)" sink (advance alpha (tab 1) 2) in
      let* () = Guard_file.close h in
      let* r1 = Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity in
      let ( (_ : Guard_sink.t)
          , floors
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , h2 ) =
        opened "reopen" r1 in
      check "reopen hands back exactly the floors written before close"
        (Int.equal (Floors.cardinal floors) 3
        && floor_at floors alpha (tab 1) ~is:3
        && floor_at floors alpha (tab 2) ~is:5
        && floor_at floors beta (tab 1) ~is:7);
      Guard_file.close h2)

(* --- 2. Torn tail: the crash-in-flight shape ------------------------------- *)

let () =
  in_scratch (fun dir ->
      let r = replica "torn" in
      let* r0 = Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity in
      let ( sink
          , (_ : Floors.t)
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , h ) =
        opened "torn setup" r0 in
      let* () =
        Lwt_list.iter_s
          (put "three whole frames" sink)
          [ advance r (tab 1) 1; advance r (tab 2) 2; advance r (tab 3) 3 ]
      in
      let* () = Guard_file.close h in
      let path = journal_of dir in
      let whole = size_of path in
      (* Cut three bytes off the last frame's CRC: the frame now claims more
         bytes than remain, the exact shape a crash mid-append leaves. *)
      Unix.truncate path (whole - 3);
      let torn = size_of path in
      let* r1 = Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity in
      let ( (_ : Guard_sink.t)
          , floors
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , h2 ) =
        opened "reopen over a torn tail" r1 in
      check "a torn tail keeps the surviving prefix, drops only the torn record"
        (Int.equal (Floors.cardinal floors) 2
        && floor_at floors r (tab 1) ~is:1
        && floor_at floors r (tab 2) ~is:2
        && no_floor floors r (tab 3));
      check "reopen never truncates or rewrites the journal it read"
        (Int.equal (size_of path) torn);
      Guard_file.close h2)

(* --- 3. A flipped byte mid-file: fold-until-broken ------------------------- *)

let () =
  in_scratch (fun dir ->
      let r = replica "crc" in
      let e1 = advance r (tab 1) 1
      and e2 = advance r (tab 2) 2
      and e3 = advance r (tab 3) 3 in
      let* r0 = Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity in
      let ( sink
          , (_ : Floors.t)
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , h ) =
        opened "crc setup" r0 in
      let* () = Lwt_list.iter_s (put "three whole frames" sink) [ e1; e2; e3 ] in
      let* () = Guard_file.close h in
      let path = journal_of dir in
      (* Flip the last byte of frame 2 (its CRC): the frame is complete but
         its bytes lie, and the fold must stop there, discarding the still
         well-formed frame 3 behind it. Frame offsets come from the codec
         itself, not from hardcoded sizes. *)
      let cut =
        String.length (Guard_sink.Codec.to_bytes e1)
        + String.length (Guard_sink.Codec.to_bytes e2)
        - 1
      in
      let bytes = Bytes.of_string (read_file path) in
      Bytes.set bytes cut (Char.chr (Char.code (Bytes.get bytes cut) lxor 0xff));
      write_file path (Bytes.to_string bytes);
      let* r1 = Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity in
      let ( (_ : Guard_sink.t)
          , floors
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , h2 ) =
        opened "reopen over a bad CRC" r1 in
      check "floors stop at the corruption; later well-formed frames are discarded"
        (Int.equal (Floors.cardinal floors) 1
        && floor_at floors r (tab 1) ~is:1
        && no_floor floors r (tab 2)
        && no_floor floors r (tab 3));
      Guard_file.close h2)

(* --- 4. The cap: drop-oldest by last append -------------------------------- *)

let () =
  in_scratch (fun dir ->
      let r = replica "crowded" in
      let* r0 = Guard_file.open_ ~dir ~cap:4 ~head_water:no_heads ~identity in
      let ( sink
          , (_ : Floors.t)
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , h ) =
        opened "cap 4" r0 in
      (* Five distinct keys against cap 4, then key 1 touched again: by last
         append the age order is 2 < 3 < 4 < 5 < 1, so the one victim must be
         tab 2, not the re-touched tab 1. *)
      let* () =
        Lwt_list.iter_s
          (put "crowding" sink)
          [ advance r (tab 1) 1
          ; advance r (tab 2) 2
          ; advance r (tab 3) 3
          ; advance r (tab 4) 4
          ; advance r (tab 5) 5
          ; advance r (tab 1) 6
          ]
      in
      let* () = Guard_file.close h in
      let* r1 = Guard_file.open_ ~dir ~cap:4 ~head_water:no_heads ~identity in
      let ( (_ : Guard_sink.t)
          , floors
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , h2 ) =
        opened "reopen capped" r1 in
      check "the reopened floor count is bounded by cap"
        (Int.equal (Floors.cardinal floors) 4);
      check "the victim is the oldest key by last append, not the re-touched one"
        (no_floor floors r (tab 2)
        && floor_at floors r (tab 1) ~is:6
        && floor_at floors r (tab 3) ~is:3
        && floor_at floors r (tab 4) ~is:4
        && floor_at floors r (tab 5) ~is:5);
      Guard_file.close h2)

(* --- 5. Compaction at > 4 * cap --------------------------------------------

   The trigger sits inside [append]: the record that lifts the file's count
   over [4 * cap] is written first, then the live floors are rewritten to a
   temp file and renamed over the journal. So with cap 2 the eighth append
   is one shy, and the ninth trips it. *)

let () =
  in_scratch (fun dir ->
      let r = replica "compact" in
      let ta = tab 1 and tb = tab 2 in
      let* r0 = Guard_file.open_ ~dir ~cap:2 ~head_water:no_heads ~identity in
      let ( sink
          , (_ : Floors.t)
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , h ) =
        opened "cap 2" r0 in
      let* () =
        Lwt_list.iter_s
          (put "pre-compaction fill" sink)
          [ advance r ta 1
          ; advance r tb 1
          ; advance r ta 2
          ; advance r tb 2
          ; advance r ta 3
          ; advance r tb 3
          ; advance r ta 4
          ; advance r tb 4
          ]
      in
      let path = journal_of dir in
      let before = size_of path in
      let* () = put "the append that trips compaction" sink (advance r ta 5) in
      let after = size_of path in
      check "compaction shrinks the journal to the live floors" (after < before);
      (* The sink must stay live across the temp+rename cut-over. *)
      let* () = put "post-compaction append" sink (advance r tb 5) in
      let* () = Guard_file.close h in
      let* r1 = Guard_file.open_ ~dir ~cap:2 ~head_water:no_heads ~identity in
      let ( (_ : Guard_sink.t)
          , floors
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , h2 ) =
        opened "reopen after compaction" r1 in
      check "the floors survive compaction and the reopen unchanged"
        (Int.equal (Floors.cardinal floors) 2
        && floor_at floors r ta ~is:5
        && floor_at floors r tb ~is:5);
      Guard_file.close h2)

(* --- 6. Forget through the file: the tombstone must outlive the restart ---- *)

let () =
  in_scratch (fun dir ->
      let keep = replica "keep" and drop = replica "drop" in
      let* r0 = Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity in
      let ( sink
          , (_ : Floors.t)
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , h ) =
        opened "forget setup" r0 in
      let* () =
        Lwt_list.iter_s
          (put "two replicas then a tombstone" sink)
          [ advance keep (tab 1) 2
          ; advance drop (tab 1) 3
          ; advance drop (tab 2) 4
          ; forget drop
          ]
      in
      let* () = Guard_file.close h in
      let* r1 = Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity in
      let ( (_ : Guard_sink.t)
          , floors
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , h2 ) =
        opened "reopen after forget" r1 in
      check "a journalled Forget scrubs that replica's floors across restart"
        (Int.equal (Floors.cardinal floors) 1
        && floor_at floors keep (tab 1) ~is:2
        && no_floor floors drop (tab 1)
        && no_floor floors drop (tab 2));
      Guard_file.close h2)

(* --- 7. Close semantics and the open_ error path --------------------------- *)

let () =
  in_scratch (fun dir ->
      let r = replica "closing" in
      let* r0 = Guard_file.open_ ~dir ~cap:4 ~head_water:no_heads ~identity in
      let ( sink
          , (_ : Floors.t)
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , h ) =
        opened "close semantics" r0 in
      let* () = put "one record" sink (advance r (tab 1) 1) in
      let* () = Guard_file.close h in
      let* late = sink.Guard_sink.append (advance r (tab 1) 2) in
      let is_closed =
        Result.fold late
          ~ok:(fun () -> false)
          ~error:(fun (e : Guard_sink.err) ->
            match e with
            | Guard_sink.Sink_closed -> true
            | Guard_sink.Io (_ : string) -> false)
      in
      check "an append after close is exactly Error Sink_closed" is_closed;
      (* Reaching the next line at all is the assertion: close is a quiet
         no-op the second time, never a raise. *)
      let* () = Guard_file.close h in
      check "a second close returns instead of raising" true;
      Lwt.return_unit)

(* [dir] pointing at an existing REGULAR file: [mkdir] answers EEXIST (which
   [ensure_dir] forgives, a file and a dir look alike to it), so the failure
   surfaces one step later when [<dir>/journal] cannot be opened under a
   non-directory, and lands in [Io], not [Bad_dir]. Either way the contract
   under test is: an [open_err], never a raise. *)
let () =
  let parent = Filename.temp_dir "ocaml-tea-guard" "" in
  let file_not_dir = Filename.concat parent "plain" in
  write_file file_not_dir "not a directory";
  let r =
    Lwt_main.run
      (Guard_file.open_ ~dir:file_not_dir ~cap:4 ~head_water:no_heads ~identity)
  in
  let is_io =
    Result.fold r
      ~ok:(fun
          (( (_ : Guard_sink.t)
           , (_ : Floors.t)
           , (_ : Guard_file.verdict)
           , (_ : Guard_file.identity_outcome)
           , (_ : Guard_file.t) ) :
            Guard_sink.t
            * Floors.t
            * Guard_file.verdict
            * Guard_file.identity_outcome
            * Guard_file.t)
        -> false)
      ~error:(fun (e : Guard_file.open_err) ->
        match e with
        | Guard_file.Io (_ : string) -> true
        | Guard_file.Bad_dir (_ : string) -> false)
  in
  check "open_ over an existing regular file is Error Io, not a raise" is_io;
  rm_rf parent

(* --- 8. Totality on garbage: a server without durability beats no server --- *)

let () =
  in_scratch (fun dir ->
      Unix.mkdir dir 0o755;
      let state = ref 20260726L in
      (* The first four bytes are 0xff, so the first frame's length field
         claims about 4 GiB: a deterministic Torn at position zero whatever
         the seeded LCG tail contains. The tail is the garbage body. *)
      let garbage =
        String.init 256 (fun i ->
            if i < 4 then '\255'
            else Char.chr (Int64.to_int (Test_util.lcg_next state) land 0xff))
      in
      write_file (journal_of dir) garbage;
      let* r0 = Guard_file.open_ ~dir ~cap:4 ~head_water:no_heads ~identity in
      let sink, floors, (_ : Guard_file.verdict), (_ : Guard_file.identity_outcome), h =
        opened "garbage journal" r0 in
      check "a journal of pure garbage opens Ok with empty floors"
        (Int.equal (Floors.cardinal floors) 0);
      let* ap = sink.Guard_sink.append (advance (replica "phoenix") (tab 1) 1) in
      check "the sink over a wrecked journal still accepts appends"
        (Result.is_ok ap);
      Guard_file.close h)

let () =
  Printf.printf
    "\nThe file journal holds: reopen, torn/corrupt degradation, cap, compaction (D16).\n%!"
