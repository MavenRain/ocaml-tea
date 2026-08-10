(** The store witness and the boot filter (roadmap step 13, R20).

    Two halves, deliberately split (the spec's E-hand-built-floors rule):

    - W1-W6 drive the REAL water mints — a pack store's [commit] return,
      [head_water], [branch_waters] — including the fabricated rollback: an
      on-disk snapshot restored over a newer root. Nothing here hand-picks a
      water.
    - G1-G8 drive the boot filter through [Guard_file.open_] over journals
      whose floors are HAND-BUILT [Advance] records at explicitly chosen
      waters, so every verdict count is asserted against arithmetic, not
      against a store's clock.

    The property under test is R20's inversion: a floor whose branch no
    longer covers it would judge its replay Duplicate against an effect the
    store does not have — silent loss — so the filter must drop it (the
    replay re-applies, a visible duplicate), and must say so in a verdict a
    test can assert rather than a log line it would have to grep. *)

module App = struct
  open Tea_core

  (** The smallest persistable app: an int model, one message. The W checks
      never run [update] — they commit models directly — but the store
      functor wants a whole APP. *)
  type model = int

  type msg = Bump

  let model_t = Repr.int

  let msg_t =
    Repr.(
      variant "msg" (fun bump -> function Bump -> bump)
      |~ case0 "Bump" Bump
      |> sealv)

  let init = (0, Cmd.none)
  let update (_ : Crdt.Ctx.t) (Bump : msg) (m : model) = (m + 1, Cmd.none)
  let view (m : model) = Html.text (string_of_int m)
  let subscriptions (_ : model) = Sub.none
  let merge = Merge.(to_spec (atomic ~eq:Int.equal))
  let title = Prim.Title.v "water-probe"
  let url_of_model (_ : model) = None
  let msg_of_url (_ : Prim.Url.t) = None
end

module _ : Tea_core.App.APP = App
module Store = Tea_persist_pack.Store_pack.Make (App)
module Root = Tea_persist_pack.Store_pack.Root
module Guard_file = Tea_server_pack.Guard_file
module Guard_sink = Tea_server.Guard_sink
module Guard = Tea_server.Replay_guard
module Dguard = Tea_server.Durable_guard
module Floors = Tea_server.Durable_guard.Floors
module Msg_seq = Tea_core.Prim.Msg_seq
module Tab_id = Tea_core.Prim.Tab_id
module Water = Tea_core.Prim.Store_water
module Store_identity = Tea_core.Prim.Store_identity
module Replica = Tea_core.Crdt.Replica
open Lwt.Syntax

(* --- Harness ---------------------------------------------------------------
   Filesystem work is spelled entirely in [Lwt_unix]/[Lwt_io] with ONE
   [Lwt.catch] boundary ([fs]) where any failure becomes a loud setup FAIL:
   errors stay values, and no helper below can raise into a check. *)

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

let fs (what : string) (f : unit -> 'a Lwt.t) : 'a Lwt.t =
  Lwt.catch f (fun (exc : exn) ->
      Printf.printf "FAIL - test setup: %s (%s)\n%!" what
        (Printexc.to_string exc);
      exit 1)

(* [Option.fold]'s [~none:] is EAGER; both branches are closures so the
   refusal cannot run on the happy path. *)
let must (what : string) (o : 'a option) : 'a =
  Option.fold
    ~none:(fun () ->
      Printf.printf "FAIL - test setup: %s\n%!" what;
      exit 1)
    ~some:(fun x () -> x)
    o ()

let sid (name : string) : Tea_core.Prim.Session_id.t =
  must "session id" (Tea_core.Prim.Session_id.of_string name)

let replica (name : string) : Replica.t =
  Replica.v (Tea_core.Prim.Session_id.v name)

let tab (n : int) : Tab_id.t =
  must "tab id mint refused a valid seed"
    (Tab_id.of_bytes (List.init 16 (fun i -> (n + i) land 0xff)))

let seq (n : int) : Msg_seq.t =
  must (Printf.sprintf "Msg_seq.of_int refused %d" n) (Msg_seq.of_int n)

let water (n : int) : Water.t = Water.of_date (Int64.of_int n)

(** A hand-built floor record: the G checks choose every water. *)
let adv (r : Replica.t) (t : Tab_id.t) (n : int) (w : Water.t) :
    Guard_sink.event =
  Guard_sink.Advance { replica = r; tab = t; seq = seq n; water = w }

let read_file (path : string) : string Lwt.t =
  fs (Printf.sprintf "read %s" path) (fun () ->
      Lwt_io.with_file ~mode:Lwt_io.Input path Lwt_io.read)

let write_file (path : string) (s : string) : unit Lwt.t =
  fs (Printf.sprintf "write %s" path) (fun () ->
      Lwt_io.with_file ~mode:Lwt_io.Output
        ~flags:[ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
        path
        (fun out -> Lwt_io.write out s))

let size_of (path : string) : int Lwt.t =
  fs (Printf.sprintf "stat %s" path) (fun () ->
      let* st = Lwt_unix.stat path in
      Lwt.return st.Lwt_unix.st_size)

let entries_of (path : string) : string list Lwt.t =
  fs (Printf.sprintf "list %s" path) (fun () ->
      Lwt_stream.to_list (Lwt_unix.files_of_directory path)
      |> Lwt.map
           (List.filter (fun (e : string) ->
                not (String.equal e "." || String.equal e ".."))))

let mkdir (path : string) : unit Lwt.t =
  fs (Printf.sprintf "mkdir %s" path) (fun () -> Lwt_unix.mkdir path 0o755)

let rec rm_rf (path : string) : unit Lwt.t =
  fs (Printf.sprintf "remove %s" path) (fun () ->
      let* st = Lwt_unix.stat path in
      match st.Lwt_unix.st_kind with
      | Unix.S_DIR ->
        let* entries = entries_of path in
        let* () =
          Lwt_list.iter_s
            (fun (entry : string) -> rm_rf (Filename.concat path entry))
            entries
        in
        Lwt_unix.rmdir path
      | Unix.S_REG | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO
      | Unix.S_SOCK ->
        Lwt_unix.unlink path)

(** Byte-for-byte directory copy — the snapshot/restore fabrication W6
    certifies. Runs only between a clean [Store.close] and the next open, so
    there is no lock or in-flight buffer to lie about. *)
let rec copy_dir (src : string) (dst : string) : unit Lwt.t =
  fs (Printf.sprintf "copy %s" src) (fun () ->
      let* () = mkdir dst in
      let* entries = entries_of src in
      Lwt_list.iter_s
        (fun (entry : string) ->
          let s = Filename.concat src entry
          and d = Filename.concat dst entry in
          let* st = Lwt_unix.stat s in
          match st.Lwt_unix.st_kind with
          | Unix.S_DIR -> copy_dir s d
          | Unix.S_REG | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO
          | Unix.S_SOCK ->
            let* bytes = read_file s in
            write_file d bytes)
        entries)

(* A fresh scratch parent per test section: wall-clock micros + a counter,
   unique across a crashed prior run's leftovers without any raising
   temp-dir helper. *)
let scratch_counter : int ref = ref 0

let fresh_scratch () : string =
  let n = !scratch_counter in
  scratch_counter := n + 1;
  Filename.concat
    (Filename.get_temp_dir_name ())
    (Printf.sprintf "ocaml-tea-water-%.0f-%d" (Unix.gettimeofday () *. 1e6) n)

let in_scratch (f : string -> unit Lwt.t) : unit =
  Lwt_main.run
    (let parent = fresh_scratch () in
     let* () = mkdir parent in
     let* () = f (Filename.concat parent "guard") in
     rm_rf parent)

let in_scratch_pack (f : parent:string -> root:string -> unit Lwt.t) : unit =
  Lwt_main.run
    (let parent = fresh_scratch () in
     let* () = mkdir parent in
     let* () = f ~parent ~root:(Filename.concat parent "store") in
     rm_rf parent)

let journal_of (dir : string) : string = Filename.concat dir "journal"

(** Write a journal of framed events where no [open_] has run yet: the G
    checks' way of choosing the exact bytes a boot meets. *)
let plant (dir : string) (events : Guard_sink.event list) : unit Lwt.t =
  let* () = mkdir dir in
  write_file (journal_of dir)
    (String.concat "" (List.map Guard_sink.Codec.to_bytes events))

let open_err_name (e : Guard_file.open_err) : string =
  match e with
  | Guard_file.Io s -> Printf.sprintf "Io %S" s
  | Guard_file.Bad_dir s -> Printf.sprintf "Bad_dir %S" s

let opened (what : string)
    (r :
      ( Guard_sink.t
        * Floors.t
        * Guard_file.verdict
        * Guard_file.identity_outcome
        * Guard_file.epoch_outcome
        * Guard_file.t
      , Guard_file.open_err )
      result) :
    Guard_sink.t
    * Floors.t
    * Guard_file.verdict
    * Guard_file.identity_outcome
    * Guard_file.epoch_outcome
    * Guard_file.t =
  Result.fold r ~ok:Fun.id
    ~error:(fun (e : Guard_file.open_err) ->
      Printf.printf "FAIL - test setup: %s (%s)\n%!" what (open_err_name e);
      exit 1)

(* One store for every G check: each open below names the SAME identity, so a
   journal is stamped on the open that first adopts it and read back as
   [Matched] afterwards. The G arithmetic is therefore unchanged by step 18,
   and that invariance is itself the check that the header peel and the
   [Matched] arm leave the step-13 water filter alone. *)
let store_id : Store_identity.t = Store_identity.of_draws (fun () -> 0x5a)
let identity : Store_identity.binding = Store_identity.Bound store_id

let epoch0 :
    Tea_core.Prim.Store_epoch.binding * Tea_core.Prim.Store_epoch.binding =
  ( Tea_core.Prim.Store_epoch.Bound Tea_core.Prim.Store_epoch.bottom
  , Tea_core.Prim.Store_epoch.Bound Tea_core.Prim.Store_epoch.bottom )

let sink_err_name (e : Guard_sink.err) : string =
  match e with
  | Guard_sink.Sink_closed -> "Sink_closed"
  | Guard_sink.Io s -> Printf.sprintf "Io %S" s

let put (what : string) (sink : Guard_sink.t) (e : Guard_sink.event) :
    unit Lwt.t =
  let* r = sink.Guard_sink.append e in
  Result.fold r
    ~ok:(fun () -> Lwt.return_unit)
    ~error:(fun (err : Guard_sink.err) ->
      Printf.printf "FAIL - test setup: append (%s) failed (%s)\n%!" what
        (sink_err_name err);
      exit 1)

let floor_at (fl : Floors.t) (r : Replica.t) (t : Tab_id.t) ~(is : int) : bool =
  Floors.find ~replica:r ~tab:t fl
  |> Option.fold ~none:false ~some:(fun n -> Int.equal (Msg_seq.to_int n) is)

let no_floor (fl : Floors.t) (r : Replica.t) (t : Tab_id.t) : bool =
  Option.is_none (Floors.find ~replica:r ~tab:t fl)

(** Assert all four verdict counters at once: a filter that miscounts any
    outcome fails loudly with no grep involved. *)
let verdict_is (v : Guard_file.verdict) ~(kept : int) ~(behind : int)
    ~(no_branch : int) ~(unwitnessed : int) : bool =
  Int.equal v.Guard_file.kept kept
  && Int.equal v.dropped_behind behind
  && Int.equal v.dropped_no_branch no_branch
  && Int.equal v.unwitnessed unwitnessed

let heads (l : (Replica.t * Water.t) list) : Replica.t -> Water.t option =
  Guard_file.head_water_of_list l

let no_heads : Replica.t -> Water.t option = heads []

let now : unit -> int64 = fun () -> 1_000L

(* --- W1/W2: the mint itself ------------------------------------------------ *)

let () =
  in_scratch_pack (fun ~parent:(_ : string) ~root ->
      let* t = Store.create ~now (Root.v root) in
      let* s = Store.session t (sid "wfresh") in
      let* w0 = Store.head_water s in
      check "W1 a session branch with no commits reports bottom water"
        (Water.equal w0 Water.bottom);
      (* Strict increase across TWO commits kills both a constant water and
         "the FIRST commit's date" — a create-time token in disguise that a
         single-commit check would wave through. *)
      let* wa = Store.commit s ~label:"w2-first" 1 in
      let* r1 = Store.head_water s in
      let* wb = Store.commit s ~label:"w2-second" 2 in
      let* r2 = Store.head_water s in
      check "W2 each commit lifts the session's head water strictly"
        (Water.compare w0 r1 < 0 && Water.compare r1 r2 < 0);
      (* The covers-equality design rests on this agreement: after an orderly
         restart a floor stamped with commit's RETURN must EQUAL the head. *)
      check "W2 the water commit returns IS the head water standing after it"
        (Water.equal wa r1 && Water.equal wb r2);
      Store.close t)

(* --- W3: per-branch granularity -------------------------------------------- *)

let () =
  in_scratch_pack (fun ~parent:(_ : string) ~root ->
      let* t = Store.create ~now (Root.v root) in
      let* a = Store.session t (sid "wthree-a") in
      let* b = Store.session t (sid "wthree-b") in
      let* (_ : Water.t) = Store.commit a ~label:"a1" 1 in
      let* (_ : Water.t) = Store.commit b ~label:"b1" 5 in
      let* a1 = Store.head_water a in
      let* b1 = Store.head_water b in
      let* (_ : Water.t) = Store.commit a ~label:"a2" 2 in
      let* (_ : Water.t) = Store.commit a ~label:"a3" 3 in
      let* a2 = Store.head_water a in
      let* b2 = Store.head_water b in
      (* THE structural check: a store-wide max over heads passes everything
         else in this file and fails exactly here, and without it the design
         degenerates into a global scalar blind to a selective restore. *)
      check
        "W3 waters are per-branch: committing on A raises A's and leaves B's \
         exactly where it stood"
        (Water.compare a1 a2 < 0
        && Water.equal b1 b2
        && Water.compare Water.bottom b1 < 0);
      Store.close t)

(* --- W4: branch_waters speaks the pump's replica language ------------------ *)

let () =
  in_scratch_pack (fun ~parent:(_ : string) ~root ->
      let* t = Store.create ~now (Root.v root) in
      let* s = Store.session t (sid "wfour") in
      let* (_ : Water.t) = Store.commit s ~label:"w4" 1 in
      let* waters = Store.branch_waters t in
      let lookup = Guard_file.head_water_of_list waters in
      let rep = Tea_core.Crdt.Ctx.replica (Store.ctx_of_session s) in
      let* hw = Store.head_water s in
      check
        "W4 branch_waters names the session under the SAME replica mint the \
         pump persists under"
        (lookup rep |> Option.fold ~none:false ~some:(Water.equal hw));
      (* The converse arm: a lookup that defaulted an unknown replica to
         something covering would make the filter inert on exactly the
         floors it exists to question. *)
      check "W4 an unknown replica has no water at all (None, not a default)"
        (Option.is_none (lookup (replica "never-a-session")));
      Store.close t)

(* --- W5: the water survives an orderly restart ------------------------------ *)

let () =
  in_scratch_pack (fun ~parent:(_ : string) ~root ->
      let* t = Store.create ~now (Root.v root) in
      let* s = Store.session t (sid "wfive") in
      let* w1 = Store.commit s ~label:"w5" 7 in
      let* () = Store.close t in
      let* t2 = Store.create ~now (Root.v root) in
      let* s2 = Store.session t2 (sid "wfive") in
      let* hw = Store.head_water s2 in
      (* Both conjuncts: a reopen that reported bottom would satisfy any
         one-sided ordering assertion while destroying the cross-life
         comparison the whole step rests on. *)
      check
        "W5 a closed and reopened root reports the same head water, strictly \
         above bottom"
        (Water.equal hw w1 && Water.compare Water.bottom hw < 0);
      Store.close t2)

(* --- W6: the fabrication itself is certified -------------------------------- *)

let () =
  in_scratch_pack (fun ~parent ~root ->
      let snap = Filename.concat parent "snap" in
      let* t = Store.create ~now (Root.v root) in
      let* s = Store.session t (sid "wsix") in
      let* (_ : Water.t) = Store.commit s ~label:"older" 1 in
      let* () = Store.close t in
      let* () = copy_dir root snap in
      let* t2 = Store.create ~now (Root.v root) in
      let* s2 = Store.session t2 (sid "wsix") in
      let* (_ : Water.t) = Store.commit s2 ~label:"newer" 2 in
      let* hw2 = Store.head_water s2 in
      let* () = Store.close t2 in
      let* () = rm_rf root in
      let* () = copy_dir snap root in
      let* t3 = Store.create ~now (Root.v root) in
      let* s3 = Store.session t3 (sid "wsix") in
      let* hw3 = Store.head_water s3 in
      let* m3 = Store.load s3 in
      (* The model conjunct is load-bearing: an empty or corrupt restore
         would also read a lower water, and G2 would then be green for the
         wrong reason. This check certifies the rollback is a real, older,
         readable store. *)
      check
        "W6 a root restored from an older snapshot reads a STRICTLY LOWER \
         water AND the older model"
        (Water.compare hw3 hw2 < 0 && Int.equal m3 1);
      Store.close t3)

(* --- G1/G2: same journal bytes, two stores ---------------------------------- *)

let () =
  in_scratch (fun dir ->
      let ra = replica "g-a" and rb = replica "g-b" in
      let* () =
        plant dir [ adv ra (tab 1) 3 (water 100); adv rb (tab 1) 7 (water 50) ]
      in
      let* r1 =
        Guard_file.open_ ~dir ~cap:8
          ~head_water:(heads [ (ra, water 100); (rb, water 90) ])
          ~identity ~epoch:epoch0
      in
      let ( (_ : Guard_sink.t)
          , fl1
          , v1
          , (_ : Guard_file.identity_outcome)
          , (_ : Guard_file.epoch_outcome)
          , h1 ) =
        opened "G1" r1 in
      (* Equality passes and is load-bearing: ra's head EQUALS its floor's
         water, the exact state of every floor after an orderly restart. *)
      check "G1 floors are adopted under covering waters (equality included)"
        (floor_at fl1 ra (tab 1) ~is:3
        && floor_at fl1 rb (tab 1) ~is:7
        && verdict_is v1 ~kept:2 ~behind:0 ~no_branch:0 ~unwitnessed:0);
      let* () = Guard_file.close h1 in
      let* r2 =
        Guard_file.open_ ~dir ~cap:8
          ~head_water:(heads [ (ra, water 60); (rb, water 90) ])
          ~identity ~epoch:epoch0
      in
      let ( (_ : Guard_sink.t)
          , fl2
          , v2
          , (_ : Guard_file.identity_outcome)
          , (_ : Guard_file.epoch_outcome)
          , h2 ) =
        opened "G2" r2 in
      check
        "G2 the SAME journal bytes drop the rolled-back floor under the \
         restored store's waters, and count it"
        (no_floor fl2 ra (tab 1)
        && floor_at fl2 rb (tab 1) ~is:7
        && verdict_is v2 ~kept:1 ~behind:1 ~no_branch:0 ~unwitnessed:0);
      Guard_file.close h2)

(* --- G3: the legacy journal, both adopt arms, one drop beside it ------------
   The tag-'\001' frame is crafted below the codec with a second CRC
   implementation (the guard_sink_test idiom), because the codec itself will
   never write one again. *)

let local_crc32 (s : string) : int32 =
  let step (c : int32) : int32 =
    let shifted = Int32.shift_right_logical c 1 in
    if Int32.equal (Int32.logand c 1l) 1l then Int32.logxor shifted 0xedb88320l
    else shifted
  in
  let over_byte (acc : int32) (ch : char) : int32 =
    List.fold_left
      (fun (c : int32) (_ : int) -> step c)
      (Int32.logxor acc (Int32.of_int (Char.code ch)))
      (List.init 8 Fun.id)
  in
  Int32.lognot (String.fold_left over_byte 0xffffffffl s)

let frame_of_body (b : string) : string =
  let head = Bytes.create 4 in
  Bytes.set_int32_be head 0 (Int32.of_int (String.length b));
  let tail = Bytes.create 4 in
  Bytes.set_int32_be tail 0 (local_crc32 b);
  Bytes.to_string head ^ b ^ Bytes.to_string tail

let legacy_body_t : (Replica.t * string * int) Repr.t =
  Repr.(triple Replica.t string int)

let encode_legacy_raw = Repr.unstage (Repr.to_bin_string legacy_body_t)

let () =
  in_scratch (fun dir ->
      let rl = replica "g-legacy" and rq = replica "g-quad" in
      let legacy =
        frame_of_body
          ("\001" ^ encode_legacy_raw (rl, Tab_id.to_string (tab 1), 5))
      in
      let* () = mkdir dir in
      let* () =
        write_file (journal_of dir)
          (legacy ^ Guard_sink.Codec.to_bytes (adv rq (tab 1) 9 (water 200)))
      in
      (* Arm one, the in-place upgrade: a live store with real waters. The
         legacy floor (bottom, a claim of nothing) is adopted and counted
         unwitnessed; the quad floor's REAL claim of 200 against a head at
         150 drops as the rollback — both outcomes in one open, so neither
         an always-adopt nor an always-drop filter can pass. *)
      let* r1 =
        Guard_file.open_ ~dir ~cap:8
          ~head_water:(heads [ (rl, water 10); (rq, water 150) ])
          ~identity ~epoch:epoch0
      in
      let ( (_ : Guard_sink.t)
          , fl1
          , v1
          , (_ : Guard_file.identity_outcome)
          , (_ : Guard_file.epoch_outcome)
          , h1 ) =
        opened "G3 upgrade arm" r1 in
      check
        "G3 a legacy floor is adopted at a real head while an uncovered real \
         claim drops in the SAME open"
        (floor_at fl1 rl (tab 1) ~is:5
        && no_floor fl1 rq (tab 1)
        && verdict_is v1 ~kept:1 ~behind:1 ~no_branch:0 ~unwitnessed:1);
      let* () = Guard_file.close h1 in
      (* Arm two, a fresh store meeting a legacy journal: no branches at all.
         The legacy floor is STILL adopted (no absence manufactures a drop),
         and the real claim now reads no-branch — routine, not rollback.
         Arm one's filtering open compacted the refused record away (the
         durable drop G6 pins), so the journal is REPLANTED with the same
         bytes: this arm is about a fresh store's verdict, not about what
         survived arm one. *)
      let* () =
        write_file (journal_of dir)
          (legacy ^ Guard_sink.Codec.to_bytes (adv rq (tab 1) 9 (water 200)))
      in
      let* r2 = Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity ~epoch:epoch0 in
      let ( (_ : Guard_sink.t)
          , fl2
          , v2
          , (_ : Guard_file.identity_outcome)
          , (_ : Guard_file.epoch_outcome)
          , h2 ) =
        opened "G3 fresh arm" r2 in
      check
        "G3 a fresh store still adopts the legacy floor; the real claim reads \
         no-branch, not rollback"
        (floor_at fl2 rl (tab 1) ~is:5
        && no_floor fl2 rq (tab 1)
        && verdict_is v2 ~kept:1 ~behind:0 ~no_branch:1 ~unwitnessed:1);
      Guard_file.close h2)

(* --- G4: a missing branch drops beside a kept floor ------------------------- *)

let () =
  in_scratch (fun dir ->
      let live = replica "g-live" and ghost = replica "g-ghost" in
      let* () =
        plant dir
          [ adv live (tab 1) 3 (water 40); adv ghost (tab 1) 5 (water 40) ]
      in
      let* r0 =
        Guard_file.open_ ~dir ~cap:8
          ~head_water:(heads [ (live, water 80) ])
          ~identity ~epoch:epoch0
      in
      let ( (_ : Guard_sink.t)
          , fl
          , v
          , (_ : Guard_file.identity_outcome)
          , (_ : Guard_file.epoch_outcome)
          , h ) =
        opened "G4" r0 in
      check
        "G4 a floor whose replica names NO branch drops in the same open \
         where a covered floor is kept"
        (floor_at fl live (tab 1) ~is:3
        && no_floor fl ghost (tab 1)
        && verdict_is v ~kept:1 ~behind:0 ~no_branch:1 ~unwitnessed:0);
      Guard_file.close h)

(* --- G5: compaction re-emits each floor's water ------------------------------
   Compaction is tripped by APPENDS before close (the guard_file_test §5
   recipe): this open is clean, so only the record count can trip it. If the
   rewrite forgot the waters, every floor would degrade to bottom — covered
   by everything — and the drop arm below would go green-blind with no
   symptom until a real rollback. *)

let () =
  in_scratch (fun dir ->
      let ra = replica "g-compact-a" and rb = replica "g-compact-b" in
      let* r0 = Guard_file.open_ ~dir ~cap:2 ~head_water:no_heads ~identity ~epoch:epoch0 in
      let ( sink
          , (_ : Floors.t)
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , (_ : Guard_file.epoch_outcome)
          , h ) =
        opened "G5" r0
      in
      let* () =
        Lwt_list.iter_s
          (put "pre-compaction fill" sink)
          [ adv ra (tab 1) 1 (water 100)
          ; adv rb (tab 1) 1 (water 30)
          ; adv ra (tab 1) 2 (water 100)
          ; adv rb (tab 1) 2 (water 30)
          ; adv ra (tab 1) 3 (water 100)
          ; adv rb (tab 1) 3 (water 30)
          ; adv ra (tab 1) 4 (water 100)
          ; adv rb (tab 1) 4 (water 30)
          ]
      in
      let* before = size_of (journal_of dir) in
      let* () =
        put "the append that trips compaction" sink
          (adv ra (tab 1) 5 (water 100))
      in
      let* after = size_of (journal_of dir) in
      check "G5 the ninth append tripped compaction (the journal shrank)"
        (after < before);
      let* () = Guard_file.close h in
      let* r1 =
        Guard_file.open_ ~dir ~cap:2
          ~head_water:(heads [ (ra, water 50); (rb, water 90) ])
          ~identity ~epoch:epoch0
      in
      let ( (_ : Guard_sink.t)
          , fl
          , v
          , (_ : Guard_file.identity_outcome)
          , (_ : Guard_file.epoch_outcome)
          , h2 ) =
        opened "G5 reopen" r1 in
      check
        "G5 compaction re-emitted each floor's water: the rolled-back key \
         still drops, the covered key keeps its exact seq"
        (no_floor fl ra (tab 1)
        && floor_at fl rb (tab 1) ~is:4
        && verdict_is v ~kept:1 ~behind:1 ~no_branch:0 ~unwitnessed:0);
      Guard_file.close h2)

(* --- G6: a filtering open compacts — the drop is durable, and cannot un-drop --
   The first cut left the bytes alone so a second open of the same tree
   would report the same verdict; review showed the refused record then
   un-drops itself: the branch head that stood behind the stale floor rises
   past it with one fresh wall-clock commit, and the next open re-admits
   the floor as [kept] — re-arming the exact silent loss the filter
   refused. So the drop is made durable in the same open that reports it,
   and the un-drop trap below (a head risen PAST the stale water) must
   find nothing left to admit. *)

let () =
  in_scratch (fun dir ->
      let ra = replica "g-frozen-a" and rb = replica "g-frozen-b" in
      let* () =
        plant dir
          [ adv ra (tab 1) 1 (water 100)
          ; adv ra (tab 1) 2 (water 100)
          ; adv ra (tab 1) 3 (water 100)
          ; adv ra (tab 1) 4 (water 100)
          ; adv ra (tab 1) 5 (water 100)
          ; adv rb (tab 1) 1 (water 20)
          ; adv rb (tab 1) 2 (water 20)
          ; adv rb (tab 1) 3 (water 20)
          ; adv rb (tab 1) 4 (water 20)
          ]
      in
      let* size0 = size_of (journal_of dir) in
      let under = heads [ (rb, water 60) ] in
      let* r1 = Guard_file.open_ ~dir ~cap:2 ~head_water:under ~identity ~epoch:epoch0 in
      let ( (_ : Guard_sink.t)
          , fl1
          , v1
          , (_ : Guard_file.identity_outcome)
          , (_ : Guard_file.epoch_outcome)
          , h1 ) =
        opened "G6 first open" r1 in
      let* size1 = size_of (journal_of dir) in
      check
        "G6 a filtering open compacts the refused records away in the open \
         that reports them"
        (size1 < size0
        && floor_at fl1 rb (tab 1) ~is:4
        && no_floor fl1 ra (tab 1)
        && verdict_is v1 ~kept:1 ~behind:0 ~no_branch:1 ~unwitnessed:0);
      let* () = Guard_file.close h1 in
      let* r2 =
        Guard_file.open_ ~dir ~cap:2
          ~head_water:(heads [ (ra, water 500); (rb, water 60) ])
          ~identity ~epoch:epoch0
      in
      let ( (_ : Guard_sink.t)
          , fl2
          , v2
          , (_ : Guard_file.identity_outcome)
          , (_ : Guard_file.epoch_outcome)
          , h2 ) =
        opened "G6 second open" r2 in
      check
        "G6 the drop is durable: a head risen past the stale water cannot \
         un-drop the floor"
        (no_floor fl2 ra (tab 1)
        && floor_at fl2 rb (tab 1) ~is:4
        && verdict_is v2 ~kept:1 ~behind:0 ~no_branch:0 ~unwitnessed:0);
      Guard_file.close h2)

(* --- G7: the drop is BEHAVIOURAL ---------------------------------------------
   The spec sketches this as "the dropped key's old seq answers Fresh", but
   under the prefix invariant a floorless key at seq 6 answers Gapped, so the
   arms are spelled at the seqs that discriminate: the dropped key's seq 1 is
   Fresh (a surviving floor at 6 would answer Duplicate), and the kept key's
   old seq 4 is Duplicate (a dropped floor would answer Gapped). A filter
   that drops everything fails the Duplicate half; one that drops nothing
   fails the Fresh half. No constant passes. *)

let is_fresh_at (v : Guard.verdict) ~(at : int) : bool =
  match v with
  | Guard.Fresh n -> Int.equal (Msg_seq.to_int n) at
  | Guard.Duplicate (_ : Msg_seq.t) -> false
  | Guard.Gapped -> false

let is_duplicate_at (v : Guard.verdict) ~(at : int) : bool =
  match v with
  | Guard.Duplicate n -> Int.equal (Msg_seq.to_int n) at
  | Guard.Fresh (_ : Msg_seq.t) -> false
  | Guard.Gapped -> false

let () =
  in_scratch (fun dir ->
      let rd = replica "g-dropped" and rk = replica "g-kept" in
      let* () =
        plant dir [ adv rd (tab 1) 6 (water 100); adv rk (tab 1) 4 (water 30) ]
      in
      let* r0 =
        Guard_file.open_ ~dir ~cap:8
          ~head_water:(heads [ (rk, water 80) ])
          ~identity ~epoch:epoch0
      in
      let ( sink
          , fl
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , (_ : Guard_file.epoch_outcome)
          , h ) =
        opened "G7" r0
      in
      let guard =
        Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
          ~sink ~floors:fl ()
      in
      check
        "G7 the drop is behavioural: the dropped key re-admits (Fresh) while \
         the kept key still de-duplicates (Duplicate)"
        (is_fresh_at
           (Dguard.take guard ~replica:rd ~tab:(tab 1) ~seq:(seq 1))
           ~at:1
        && is_duplicate_at
             (Dguard.take guard ~replica:rk ~tab:(tab 1) ~seq:(seq 4))
             ~at:4);
      Guard_file.close h)

(* --- G8: the verdict counts water drops only, never cap evictions ----------- *)

let () =
  in_scratch (fun dir ->
      let r1 = replica "g-cap-one"
      and r2 = replica "g-cap-two"
      and r3 = replica "g-cap-three" in
      let three =
        [ adv r1 (tab 1) 2 (water 10)
        ; adv r2 (tab 1) 3 (water 20)
        ; adv r3 (tab 1) 4 (water 30)
        ]
      in
      let* () = plant dir three in
      let* ra =
        Guard_file.open_ ~dir ~cap:1
          ~head_water:
            (heads [ (r1, water 50); (r2, water 50); (r3, water 50) ])
          ~identity ~epoch:epoch0
      in
      let ( (_ : Guard_sink.t)
          , fla
          , va
          , (_ : Guard_file.identity_outcome)
          , (_ : Guard_file.epoch_outcome)
          , ha ) =
        opened "G8 covered arm" ra in
      check
        "G8 the verdict counts water drops only: three kept in the verdict, \
         one survives the cap"
        (verdict_is va ~kept:3 ~behind:0 ~no_branch:0 ~unwitnessed:0
        && Int.equal (Floors.cardinal fla) 1
        && floor_at fla r3 (tab 1) ~is:4);
      let* () = Guard_file.close ha in
      (* Arm one met a headerless journal and STAMPED it, and a stamping open
         rewrites the file to the floors that survived the cap (step 18). So
         the journal is REPLANTED with the same three records, the G3 arm-two
         recipe: this arm is about a fresh store's verdict over those bytes,
         not about what survived arm one. *)
      let* () =
        write_file (journal_of dir)
          (String.concat "" (List.map Guard_sink.Codec.to_bytes three))
      in
      (* The companion arm: one REAL drop beside the same cap eviction. The
         survivor also pins that the cap ranks only ADMITTED keys — a cap
         that still counted the dropped r3's touch would evict r2 and leave
         nothing. *)
      let* rb =
        Guard_file.open_ ~dir ~cap:1
          ~head_water:(heads [ (r1, water 50); (r2, water 50); (r3, water 5) ])
          ~identity ~epoch:epoch0
      in
      let ( (_ : Guard_sink.t)
          , flb
          , vb
          , (_ : Guard_file.identity_outcome)
          , (_ : Guard_file.epoch_outcome)
          , hb ) =
        opened "G8 companion arm" rb in
      check
        "G8 companion: the water drop is counted, the cap eviction still is \
         not, and the cap ranks only admitted keys"
        (verdict_is vb ~kept:2 ~behind:1 ~no_branch:0 ~unwitnessed:0
        && Int.equal (Floors.cardinal flb) 1
        && floor_at flb r2 (tab 1) ~is:3);
      Guard_file.close hb)

let () =
  Printf.printf
    "\nThe store witness holds: real waters (W1-W6) and the boot filter's \
     verdict (G1-G8), R20.\n%!"
