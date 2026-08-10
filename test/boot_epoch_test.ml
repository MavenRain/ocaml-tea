(** The boot epoch, journal half (roadmap step 21, R20b).

    [store_pack]'s [bump_epoch] proves the ROOT counter; this file proves what
    the counter does to a JOURNAL: the four
    {!Tea_server_pack.Guard_file.epoch_outcome} arms, and with them the one
    property a create-time token provably cannot carry - a store told apart
    from its own divergent COPY.

    E1 and E2 are the pair that matters, and they share one rig. A real pack
    root is created, booted, and copied whole (so the copy carries
    [tea.identity] AND a frozen [tea.epoch]); the original is then advanced.
    E1 opens the ADVANCED journal beside the FROZEN root, E2 opens the FROZEN
    journal beside the ADVANCED root. Both read [Epoch_diverged], and the two
    stamps sit on OPPOSITE sides of their root's counter, so a check written
    as "the journal lagged" passes E1 and silently fails E2. Identity reads
    [Matched] through both, which is R20b stated as a test: the token cannot
    see this split.

    E3 repeats a clean reboot three times, because a swapped bump order keeps
    the first cycle green and breaks every cycle after it. E4 is the upgrade
    window with its REQUIRED second boot: boot 1 keeps the floors either way,
    so only boot 2 separates "stamped" from "still unstamped". E5 is torn
    state on both sides, the root file and the journal frame; its frame half
    separates a stamp that unframes cleanly but carries no counter, where the
    floors behind it are still reachable and so still counted, from bytes that
    will not unframe at all, where nothing behind the damage is readable and
    the count is honestly zero. E6 pins the
    register's own named trade: a boot that advances the root and dies before
    it restamps costs one noisy floor wipe, never a refusal. E7 drives the
    real {!Tea_server_pack.open_guards} composition and asks for per-channel
    independence off ONE shared pair.

    Every verdict is asserted as a COUNT or a byte compare, never grepped from
    a log line, and this file reports every failure and exits at the END
    (pack_guards_test's rule): the properties are independent and a mutation
    sweep must see exactly which one a mutant breaks. Setup failures still
    exit at once, because a broken fixture attributes to itself. Nothing is
    imported from a sibling test binary: [copy_dir], the scratch mint and the
    frame builders are re-implemented here. *)

module App = struct
  open Tea_core

  (** The smallest persistable app (guard_identity_test's): no E check runs
      [update], but the store functor wants a whole APP. *)
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
  let title = Prim.Title.v "boot-epoch-probe"
  let url_of_model (_ : model) = None
  let msg_of_url (_ : Prim.Url.t) = None
end

module _ : Tea_core.App.APP = App
module Store = Tea_persist_pack.Store_pack.Make (App)
module Root = Tea_persist_pack.Store_pack.Root
module Guard_file = Tea_server_pack.Guard_file
module Guard_sink = Tea_server.Guard_sink
module Dguard = Tea_server.Durable_guard
module Floors = Tea_server.Durable_guard.Floors
module Bound = Tea_server.Replay_guard.Bound
module Msg_seq = Tea_core.Prim.Msg_seq
module Tab_id = Tea_core.Prim.Tab_id
module Water = Tea_core.Prim.Store_water
module Id = Tea_core.Prim.Store_identity
module Epoch = Tea_core.Prim.Store_epoch
module Replica = Tea_core.Crdt.Replica
open Lwt.Syntax

(* --- Harness ---------------------------------------------------------------
   Property failures accumulate; setup failures exit at once. Filesystem work
   goes through ONE [Lwt.catch] boundary ([fs]) so errors stay values and no
   helper can raise into a check. *)

let failures : int ref = ref 0

let check (name : string) (cond : bool) : unit =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    incr failures)

let section (name : string) : unit = Printf.printf "\n-- %s\n%!" name

let fs (what : string) (f : unit -> 'a Lwt.t) : 'a Lwt.t =
  Lwt.catch f (fun (exc : exn) ->
      Printf.printf "FAIL - test setup: %s (%s)\n%!" what (Printexc.to_string exc);
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

let replica (name : string) : Replica.t =
  Replica.v (Tea_core.Prim.Session_id.v name)

let tab (n : int) : Tab_id.t =
  must "tab id mint refused a valid seed"
    (Tab_id.of_bytes (List.init 16 (fun (i : int) -> (n + i) land 0xff)))

let seq (n : int) : Msg_seq.t =
  must (Printf.sprintf "Msg_seq.of_int refused %d" n) (Msg_seq.of_int n)

let adv (r : Replica.t) (t : Tab_id.t) (n : int) (w : Water.t) : Guard_sink.event =
  Guard_sink.Advance { replica = r; tab = t; seq = seq n; water = w }

let read_file (path : string) : string Lwt.t =
  fs (Printf.sprintf "read %s" path) (fun () ->
      Lwt_io.with_file ~mode:Lwt_io.Input path Lwt_io.read)

let write_file (path : string) (s : string) : unit Lwt.t =
  fs (Printf.sprintf "write %s" path) (fun () ->
      Lwt_io.with_file ~mode:Lwt_io.Output
        ~flags:[ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
        path
        (fun (out : Lwt_io.output_channel) -> Lwt_io.write out s))

let mkdir (path : string) : unit Lwt.t =
  fs (Printf.sprintf "mkdir %s" path) (fun () -> Lwt_unix.mkdir path 0o755)

let entries_of (path : string) : string list Lwt.t =
  fs (Printf.sprintf "list %s" path) (fun () ->
      Lwt_stream.to_list (Lwt_unix.files_of_directory path)
      |> Lwt.map
           (List.filter (fun (e : string) ->
                not (String.equal e "." || String.equal e ".."))))

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

(** The [cp -r] that makes R20b, re-implemented locally: test binaries carry no
    [.mli] and import nothing from each other. Every regular file is carried
    byte for byte, and anything that is neither a directory nor a regular file
    is a broken fixture rather than a silent skip. *)
let rec copy_dir (src : string) (dst : string) : unit Lwt.t =
  let* () = mkdir dst in
  let* entries = entries_of src in
  Lwt_list.iter_s
    (fun (entry : string) ->
      let s = Filename.concat src entry and d = Filename.concat dst entry in
      let* st = fs (Printf.sprintf "lstat %s" s) (fun () -> Lwt_unix.lstat s) in
      match st.Lwt_unix.st_kind with
      | Unix.S_DIR -> copy_dir s d
      | Unix.S_REG ->
        let* bytes = read_file s in
        write_file d bytes
      | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO | Unix.S_SOCK ->
        Printf.printf
          "FAIL - test setup: %s is neither a directory nor a regular file\n%!" s;
        exit 1)
    entries

let scratch_counter : int ref = ref 0

let fresh_scratch () : string =
  let n = !scratch_counter in
  scratch_counter := n + 1;
  Filename.concat
    (Filename.get_temp_dir_name ())
    (Printf.sprintf "ocaml-tea-boot-epoch-%.0f-%d" (Unix.gettimeofday () *. 1e6) n)

(** Run [f] over a fresh parent directory; [f] carves what it needs. *)
let in_parent (f : string -> unit Lwt.t) : unit =
  Lwt_main.run
    (let parent = fresh_scratch () in
     let* () = mkdir parent in
     let* () = f parent in
     rm_rf parent)

let journal_of (dir : string) : string = Filename.concat dir "journal"

(* --- The journal's two structural frames -----------------------------------
   Built through the PUBLIC [Codec.frame] and the public tags, so a fixture
   and the writer cannot drift apart without one of them changing the shared
   seam. Layout: [len:4][tag:1][payload][crc:4]. *)

let ident_frame (i : Id.t) : string =
  Guard_sink.Codec.frame ~tag:Guard_file.tag_identity ~payload:(Id.to_string i)

let epoch_frame (e : Epoch.t) : string =
  Guard_sink.Codec.frame ~tag:Guard_file.tag_epoch ~payload:(Epoch.to_string e)

let id_a : Id.t = Id.of_draws (fun () -> 0x11)
let bound_a : Id.binding = Id.Bound id_a

(* Offsets are COMPUTED from the codec, never hardcoded: the identity frame's
   own length, plus the [len:4][tag:1] prelude of the frame behind it. *)
let ident_len : int = String.length (ident_frame id_a)
let epoch_payload_at : int = ident_len + 5

let stamped_token (s : string) : string option =
  if String.length s >= 37 then Some (String.sub s 5 32) (* @total-accessor *)
  else None

let stamped_epoch (s : string) : string option =
  if String.length s >= epoch_payload_at + 16 then
    Some (String.sub s epoch_payload_at 16) (* @total-accessor *)
  else None

(** Byte [i] of [s], with no index anywhere. *)
let byte_at (s : string) (i : int) : char option =
  String.to_seq s |> Seq.drop i |> Seq.uncons |> Option.map fst

(** Whether the frame BEHIND the identity frame is an epoch stamp at all,
    read off its tag byte at [len:4]. {!stamped_epoch} slices a fixed window
    and cannot answer this on its own: behind a step-20 journal's identity
    frame sit event bytes, which are long enough to fill that window and would
    read as a stamp that is not there. *)
let has_epoch_frame (s : string) : bool =
  Option.fold
    (byte_at s (ident_len + 4))
    ~none:false
    ~some:(Char.equal Guard_file.tag_epoch)

(** Byte [i] turned into a byte it certainly is not, through [String.mapi] so
    nothing is read out by index. *)
let corrupt_at (s : string) (i : int) : string =
  String.mapi
    (fun (j : int) (c : char) ->
      if Int.equal j i then if Char.equal c 'a' then 'b' else 'a' else c)
    s

let contains (hay : string) (needle : string) : bool =
  let n = String.length needle and h = String.length hay in
  n <= h
  && List.exists
       (fun (i : int) -> String.equal (String.sub hay i n) needle (* @total-accessor *))
       (List.init (h - n + 1) Fun.id)

(** A journal in the step-20 shape: identity frame, then events, NO epoch
    frame. This is what every pre-step-21 binary wrote. *)
let plant_step20 (dir : string) (i : Id.t) (events : Guard_sink.event list) :
    unit Lwt.t =
  let* () = mkdir dir in
  write_file (journal_of dir)
    (ident_frame i ^ String.concat "" (List.map Guard_sink.Codec.to_bytes events))

(** Chosen bytes at [<dir>/journal], the directory carved first. E5b builds a
    STAMPED journal this way, because the bytes it wants are damaged ones. *)
let plant_raw (dir : string) (bytes : string) : unit Lwt.t =
  let* () = mkdir dir in
  write_file (journal_of dir) bytes

(* --- Every finite sum spelled out ------------------------------------------
   A new constructor is a compile error here and not a case nothing asserts. *)

let open_err_name (e : Guard_file.open_err) : string =
  match e with
  | Guard_file.Io (s : string) -> Printf.sprintf "Io %S" s
  | Guard_file.Bad_dir (s : string) -> Printf.sprintf "Bad_dir %S" s

let id_outcome_str (o : Guard_file.identity_outcome) : string =
  match o with
  | Guard_file.Matched -> "Matched"
  | Guard_file.Freshly_bound -> "Freshly_bound"
  | Guard_file.Adopted_unbound (n : int) -> Printf.sprintf "Adopted_unbound %d" n
  | Guard_file.Rebound (n : int) -> Printf.sprintf "Rebound %d" n
  | Guard_file.Unresolved_cleared (n : int) ->
    Printf.sprintf "Unresolved_cleared %d" n

let epoch_outcome_str (o : Guard_file.epoch_outcome) : string =
  match o with
  | Guard_file.Epoch_matched -> "Epoch_matched"
  | Guard_file.Epoch_adopted -> "Epoch_adopted"
  | Guard_file.Epoch_diverged (n : int) -> Printf.sprintf "Epoch_diverged %d" n
  | Guard_file.Epoch_unresolved -> "Epoch_unresolved"

let origin_str (o : Store.epoch_origin) : string =
  match o with
  | Store.Epoch_bumped -> "Epoch_bumped"
  | Store.Epoch_minted -> "Epoch_minted"
  | Store.Epoch_write_failed (_ : string) -> "Epoch_write_failed"
  | Store.Epoch_unreadable (_ : string) -> "Epoch_unreadable"
  | Store.Epoch_malformed -> "Epoch_malformed"

let binding_str (b : Epoch.binding) : string =
  match b with
  | Epoch.Bound (e : Epoch.t) -> Epoch.to_string e
  | Epoch.Unresolved -> "Unresolved"

let bound_epoch (b : Epoch.binding) : Epoch.t option =
  match b with
  | Epoch.Bound (e : Epoch.t) -> Some e
  | Epoch.Unresolved -> None

let bound_token (b : Id.binding) : Id.t option =
  match b with
  | Id.Bound (i : Id.t) -> Some i
  | Id.Unresolved -> None

let replay_str (v : Tea_server.Replay_guard.verdict) : string =
  match v with
  | Tea_server.Replay_guard.Fresh (_ : Msg_seq.t) -> "Fresh"
  | Tea_server.Replay_guard.Duplicate (_ : Msg_seq.t) -> "Duplicate"
  | Tea_server.Replay_guard.Gapped -> "Gapped"

let id_is (o : Guard_file.identity_outcome) (want : string) : bool =
  String.equal (id_outcome_str o) want

let epoch_is (o : Guard_file.epoch_outcome) (want : string) : bool =
  String.equal (epoch_outcome_str o) want

(* --- Opening, appending, judging -------------------------------------------- *)

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

let sink_err_name (e : Guard_sink.err) : string =
  match e with
  | Guard_sink.Sink_closed -> "Sink_closed"
  | Guard_sink.Io (s : string) -> Printf.sprintf "Io %S" s

let put (what : string) (sink : Guard_sink.t) (e : Guard_sink.event) : unit Lwt.t =
  let* r = sink.Guard_sink.append e in
  Result.fold r
    ~ok:(fun () -> Lwt.return_unit)
    ~error:(fun (err : Guard_sink.err) ->
      Printf.printf "FAIL - test setup: append (%s) failed (%s)\n%!" what
        (sink_err_name err);
      exit 1)

let no_heads (_ : Replica.t) : Water.t option = None

let floor_at (fl : Floors.t) (r : Replica.t) (t : Tab_id.t) ~(is : int) : bool =
  Floors.find ~replica:r ~tab:t fl
  |> Option.fold ~none:false ~some:(fun (n : Msg_seq.t) ->
         Int.equal (Msg_seq.to_int n) is)

let no_floor (fl : Floors.t) (r : Replica.t) (t : Tab_id.t) : bool =
  Option.is_none (Floors.find ~replica:r ~tab:t fl)

let verdict_is (v : Guard_file.verdict) ~(kept : int) ~(behind : int)
    ~(no_branch : int) ~(unwitnessed : int) : bool =
  Int.equal v.Guard_file.kept kept
  && Int.equal v.dropped_behind behind
  && Int.equal v.dropped_no_branch no_branch
  && Int.equal v.unwitnessed unwitnessed

(** A guard over exactly the floors an open admitted: what a returning client's
    replay is actually judged against. A cleared floor must re-admit its
    message as [Fresh], which is the visible duplicate the whole family trades
    for, and a kept floor must still answer [Duplicate]. *)
let guard_over (sink : Guard_sink.t) (floors : Floors.t) : Dguard.t =
  let b = must "Bound.of_int refused 64" (Bound.of_int 64) in
  Dguard.v ~sessions:b ~tabs:b ~sink ~floors ()

let replay_of (sink : Guard_sink.t) (floors : Floors.t) (r : Replica.t)
    (t : Tab_id.t) (n : int) : string =
  replay_str (Dguard.take (guard_over sink floors) ~replica:r ~tab:t ~seq:(seq n))

(* --- Real pack roots --------------------------------------------------------- *)

let open_store (path : string) : Store.t Lwt.t =
  let* r = Store.open_root ~now:(fun () -> 5L) (Root.v path) in
  Result.fold r ~ok:Lwt.return ~error:(fun (e : Store.open_error) ->
      Printf.printf "FAIL - test setup: open_root %s (%s)\n%!" path
        (Store.explain e);
      exit 1)

(** A real pack root, opened through the total path and closed again: the
    counter and the token are resolved only AFTER the backend's one [mkdir]
    (both resolvers' load-bearing ordering rule). *)
let make_root (path : string) : unit Lwt.t =
  let* t = open_store path in
  Store.close t

let identity_of (path : string) : Id.binding =
  let b, (_ : Store.identity_origin) = Store.resolve_identity (Root.v path) in
  b

let token_of (path : string) : Id.t =
  must (Printf.sprintf "%s resolved a Bound identity" path)
    (bound_token (identity_of path))

let bump (path : string) : (Epoch.binding * Epoch.binding) * Store.epoch_origin =
  Store.bump_epoch (Root.v path)

let close_journal (j : Guard_file.t option) : unit Lwt.t =
  Option.fold ~none:Lwt.return_unit ~some:Guard_file.close j

(* --- E1, E1b and E2: the divergent copy, both directions --------------------
   One rig. A real root A is created, committed to, booted once (which mints
   its counter and stamps its journal), then copied WHOLE - so the copy B
   carries A's [tea.identity] byte for byte and a [tea.epoch] frozen at copy
   time. A is then advanced by two more boots.

   E1 puts A's ADVANCED journal beside B's FROZEN root; E2 puts B's FROZEN
   journal beside A's ADVANCED root. The floors are recorded at
   [Water.bottom], which [Floors.filter] keeps unconditionally before it ever
   consults a head, so the water filter can neither manufacture nor mask the
   clear this case is about. *)

let () =
  section "E1/E1b/E2: a store told apart from its own cp -r copy, both ways";
  in_parent (fun parent ->
      let root_a = Filename.concat parent "store-a" in
      let root_b = Filename.concat parent "store-b" in
      let guard_a = Filename.concat parent "store-a.guard" in
      let r1 = replica "e1" and t1 = tab 1 in
      (* A real store: created, committed to, closed. *)
      let* ta = open_store root_a in
      let* sa = Store.main_session ta in
      let* (_ : Water.t) = Store.commit sa ~label:"a1" 1 in
      let* () = Store.close ta in
      let id_binding = identity_of root_a in
      let (seen1, now1), org1 = bump root_a in
      (* Boot 1: an absent journal, stamped with this boot's post-bump value. *)
      let* rr1 =
        Guard_file.open_ ~dir:guard_a ~cap:8 ~head_water:no_heads
          ~identity:id_binding ~epoch:(seen1, now1)
      in
      let sink1, (_ : Floors.t), (_ : Guard_file.verdict), o1, eo1, h1 =
        opened "E1 boot 1" rr1
      in
      let* () = put "E1 boot 1 floor" sink1 (adv r1 t1 1 Water.bottom) in
      let* () = Guard_file.close h1 in
      let* raw1 = read_file (journal_of guard_a) in
      check
        "E1 A's first boot MINTS the root counter at bottom, opens a \
         Freshly_bound journal, and leaves it stamped with that boot's \
         POST-bump value"
        (String.equal (origin_str org1) "Epoch_minted"
        && String.equal (binding_str seen1) (Epoch.to_string Epoch.bottom)
        && id_is o1 "Freshly_bound"
        && epoch_is eo1 "Epoch_matched"
        && Option.fold (stamped_epoch raw1) ~none:false
             ~some:(String.equal (binding_str now1)));
      (* The cp -r, mid-rig. B freezes both root files at this instant. *)
      let* () = copy_dir root_a root_b in
      let* a_epoch_file = read_file (Store.epoch_path (Root.v root_a)) in
      let* b_epoch_file = read_file (Store.epoch_path (Root.v root_b)) in
      check
        "E1 the cp -r copy carries the SAME identity token and a FROZEN copy of \
         the counter file"
        (Id.equal (token_of root_a) (token_of root_b)
        && String.equal a_epoch_file b_epoch_file);
      (* Two more boots on A. Each is an ordinary same-lineage reopen. *)
      let* advance =
        Lwt_list.fold_left_s
          (fun (ok : bool) (_ : int) ->
            let (seen, now), (_ : Store.epoch_origin) = bump root_a in
            let* rr =
              Guard_file.open_ ~dir:guard_a ~cap:8 ~head_water:no_heads
                ~identity:id_binding ~epoch:(seen, now)
            in
            let sink, fl, (_ : Guard_file.verdict), o, eo, h =
              opened "E1 advance" rr
            in
            let held =
              id_is o "Matched" && epoch_is eo "Epoch_matched"
              && Int.equal (Floors.cardinal fl) 1
              && floor_at fl r1 t1 ~is:1
              && String.equal (replay_of sink fl r1 t1 1) "Duplicate"
            in
            let* () = Guard_file.close h in
            Lwt.return (ok && held))
          true [ 2; 3 ]
      in
      check
        "E1 A's two later boots stay Matched and Epoch_matched, keep the floor, \
         and that floor still judges the replay Duplicate (the positive control \
         the clear below is measured against)"
        advance;
      let* raw3 = read_file (journal_of guard_a) in
      (* E1: the ADVANCED journal, beside the FROZEN root. *)
      let (seen_b, now_b), org_b = bump root_b in
      check
        "E1 the copy's own first boot BUMPS the frozen counter: its pre-bump \
         value is still A's boot-1 stamp, three boots later"
        (String.equal (origin_str org_b) "Epoch_bumped"
        && String.equal (binding_str seen_b) (binding_str now1));
      let dir_e1 = Filename.concat parent "probe-e1" in
      let* () = plant_raw dir_e1 raw3 in
      let* rr_e1 =
        Guard_file.open_ ~dir:dir_e1 ~cap:8 ~head_water:no_heads
          ~identity:(identity_of root_b) ~epoch:(seen_b, now_b)
      in
      let sink_e1, fl_e1, v_e1, o_e1, eo_e1, h_e1 = opened "E1 probe" rr_e1 in
      check
        "E1 the advanced journal beside the frozen root reads identity Matched: \
         the create-time token cannot see the split (R20b, stated as a test)"
        (id_is o_e1 "Matched");
      check
        "E1 and reads Epoch_diverged 1, clearing every floor and reporting the \
         all-zero water verdict rather than synthetic rollback counts"
        (epoch_is eo_e1 "Epoch_diverged 1"
        && Int.equal (Floors.cardinal fl_e1) 0
        && no_floor fl_e1 r1 t1
        && verdict_is v_e1 ~kept:0 ~behind:0 ~no_branch:0 ~unwitnessed:0);
      check
        "E1 the stamp that diverged stood AHEAD of that root's counter (E2 \
         supplies the other direction)"
        (Option.fold (stamped_epoch raw3) ~none:false ~some:(fun (s : string) ->
             Option.fold (bound_epoch seen_b) ~none:false
               ~some:(fun (b : Epoch.t) ->
                 Result.fold (Epoch.of_string s) ~error:(fun `Malformed -> false)
                   ~ok:(fun (j : Epoch.t) -> Epoch.compare j b > 0))));
      check
        "E1 the cleared floor re-admits its message as Fresh: a visible \
         duplicate, never a silent loss and never a refusal"
        (String.equal (replay_of sink_e1 fl_e1 r1 t1 1) "Fresh");
      let* () = Guard_file.close h_e1 in
      let* raw_e1 = read_file (journal_of dir_e1) in
      check
        "E1 the diverged journal leaves restamped to THIS boot's post-bump \
         counter, so the wipe cannot repeat"
        (Option.fold (stamped_epoch raw_e1) ~none:false
           ~some:(String.equal (binding_str now_b)));
      (* E1b: the boot AFTER the divergence. One noisy wipe, then quiet. *)
      let (seen_b2, now_b2), (_ : Store.epoch_origin) = bump root_b in
      let* rr_e1b =
        Guard_file.open_ ~dir:dir_e1 ~cap:8 ~head_water:no_heads
          ~identity:(identity_of root_b) ~epoch:(seen_b2, now_b2)
      in
      let ( (_ : Guard_sink.t)
          , (_ : Floors.t)
          , (_ : Guard_file.verdict)
          , o_e1b
          , eo_e1b
          , h_e1b ) =
        opened "E1b" rr_e1b
      in
      check
        "E1b the boot after the divergence matches its own restamp: one wipe, \
         and the copy stabilises instead of clearing forever"
        (id_is o_e1b "Matched"
        && epoch_is eo_e1b "Epoch_matched"
        && String.equal (binding_str seen_b2) (binding_str now_b));
      check "E1b and that boot earns no operator line at all"
        (Option.is_none
           (Tea_server_pack.explain_epoch_outcome ~channel:"websocket" eo_e1b));
      let* () = Guard_file.close h_e1b in
      (* E2: the FROZEN journal, beside the ADVANCED root. *)
      let dir_e2 = Filename.concat parent "probe-e2" in
      let* () = plant_raw dir_e2 raw1 in
      let (seen_a4, now_a4), (_ : Store.epoch_origin) = bump root_a in
      let* rr_e2 =
        Guard_file.open_ ~dir:dir_e2 ~cap:8 ~head_water:no_heads
          ~identity:id_binding ~epoch:(seen_a4, now_a4)
      in
      let ( (_ : Guard_sink.t)
          , fl_e2
          , v_e2
          , o_e2
          , eo_e2
          , h_e2 ) =
        opened "E2 probe" rr_e2
      in
      check
        "E2 the frozen copy's journal beside the ADVANCED original also reads \
         identity Matched: same store, same token, both times"
        (id_is o_e2 "Matched");
      check
        "E2 and reads Epoch_diverged 1 with the same all-zero verdict: the \
         other direction of one fact, one arm"
        (epoch_is eo_e2 "Epoch_diverged 1"
        && Int.equal (Floors.cardinal fl_e2) 0
        && verdict_is v_e2 ~kept:0 ~behind:0 ~no_branch:0 ~unwitnessed:0);
      check
        "E2 this stamp stood BEHIND its root's counter, so E1 and E2 together \
         pin EQUALITY and not lag: a < compare passes E1 and fails here"
        (Option.fold (stamped_epoch raw1) ~none:false ~some:(fun (s : string) ->
             Option.fold (bound_epoch seen_a4) ~none:false
               ~some:(fun (b : Epoch.t) ->
                 Result.fold (Epoch.of_string s) ~error:(fun `Malformed -> false)
                   ~ok:(fun (j : Epoch.t) -> Epoch.compare j b < 0))));
      let* () = Guard_file.close h_e2 in
      let (seen_a5, now_a5), (_ : Store.epoch_origin) = bump root_a in
      let* rr_e2b =
        Guard_file.open_ ~dir:dir_e2 ~cap:8 ~head_water:no_heads
          ~identity:id_binding ~epoch:(seen_a5, now_a5)
      in
      let ( (_ : Guard_sink.t)
          , (_ : Floors.t)
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , eo_e2b
          , h_e2b ) =
        opened "E2 follow-up" rr_e2b
      in
      check
        "E2 follow-up: the restamped journal is quiet on the very next boot too"
        (epoch_is eo_e2b "Epoch_matched"
        && String.equal (binding_str seen_a5) (binding_str now_a4));
      Guard_file.close h_e2b)

(* --- E3: three clean close-and-reopen cycles keep every floor ----------------
   The false-positive guard. A swapped bump order (the root file written with
   the NEW value before the compare baseline is read) keeps cycle 1 green and
   breaks every cycle after it, so one cycle cannot see it. Each cycle also
   pins [seen] against the PREVIOUS cycle's [now], which is the bookkeeping
   the swap breaks. *)

let () =
  section "E3: three clean reboots, no copy, zero floor loss";
  in_parent (fun parent ->
      let root = Filename.concat parent "store" in
      let dir = Filename.concat parent "store.guard" in
      let r3 = replica "e3" and t3 = tab 3 in
      let* () = make_root root in
      let id_binding = identity_of root in
      let (seen0, now0), org0 = bump root in
      let* rr0 =
        Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity:id_binding
          ~epoch:(seen0, now0)
      in
      let sink0, (_ : Floors.t), (_ : Guard_file.verdict), o0, eo0, h0 =
        opened "E3 boot 1" rr0
      in
      check
        "E3 the first boot mints the counter at bottom and opens a \
         Freshly_bound journal quietly"
        (String.equal (origin_str org0) "Epoch_minted"
        && id_is o0 "Freshly_bound"
        && epoch_is eo0 "Epoch_matched");
      let* () = put "E3 floor" sink0 (adv r3 t3 1 Water.bottom) in
      let* () = Guard_file.close h0 in
      let* last =
        Lwt_list.fold_left_s
          (fun (prev_now : Epoch.binding) (cycle : int) ->
            let (seen, now), org = bump root in
            let* rr =
              Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads
                ~identity:id_binding ~epoch:(seen, now)
            in
            let sink, fl, (_ : Guard_file.verdict), o, eo, h =
              opened "E3 cycle" rr
            in
            check
              (Printf.sprintf
                 "E3 cycle %d: Matched and Epoch_matched, the floor survives, \
                  the replay still reads Duplicate, and this boot's pre-bump \
                  value is exactly the previous boot's post-bump value"
                 cycle)
              (id_is o "Matched" && epoch_is eo "Epoch_matched"
              && String.equal (origin_str org) "Epoch_bumped"
              && Int.equal (Floors.cardinal fl) 1
              && floor_at fl r3 t3 ~is:1
              && String.equal (replay_of sink fl r3 t3 1) "Duplicate"
              && String.equal (binding_str seen) (binding_str prev_now));
            let* () = Guard_file.close h in
            Lwt.return now)
          now0 [ 1; 2; 3 ]
      in
      let* raw = read_file (journal_of dir) in
      check
        "E3 the journal leaves the third cycle carrying THAT cycle's own \
         post-bump counter, so the stamp advanced on every clean open"
        (Option.fold (stamped_epoch raw) ~none:false
           ~some:(String.equal (binding_str last)));
      Lwt.return_unit)

(* --- E4: the upgrade window, with its REQUIRED second boot -------------------
   A step-20-shaped journal: identity frame, event frames, no epoch frame.
   Boot 1 keeps the floors whether or not it stamps them, so a restamp
   omission is invisible until boot 2 reads the file back. *)

let () =
  section "E4: a pre-step-21 journal is adopted on trust, then stamped";
  in_parent (fun parent ->
      let dir = Filename.concat parent "guard" in
      let r4 = replica "e4" and ta = tab 4 and tb = tab 5 in
      let e1 = Epoch.succ Epoch.bottom in
      let e2 = Epoch.succ e1 in
      let e3 = Epoch.succ e2 in
      let* () =
        plant_step20 dir id_a
          [ adv r4 ta 1 Water.bottom; adv r4 tb 1 Water.bottom ]
      in
      let* rr1 =
        Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity:bound_a
          ~epoch:(Epoch.Bound e1, Epoch.Bound e2)
      in
      let ( (_ : Guard_sink.t)
          , fl1
          , (_ : Guard_file.verdict)
          , o1
          , eo1
          , h1 ) =
        opened "E4 boot 1" rr1
      in
      check
        "E4 boot 1: an identity-bound journal carrying NO epoch frame is \
         Epoch_adopted, never Epoch_diverged - absence is a version signal, \
         not a numeric mismatch"
        (id_is o1 "Matched" && epoch_is eo1 "Epoch_adopted");
      check "E4 boot 1 keeps every floor it claimed, so no upgrade boot wipes"
        (Int.equal (Floors.cardinal fl1) 2
        && floor_at fl1 r4 ta ~is:1
        && floor_at fl1 r4 tb ~is:1);
      let* () = Guard_file.close h1 in
      let* raw = read_file (journal_of dir) in
      check
        "E4 boot 1 wrote the stamp before it returned: the identity frame is \
         still first and this boot's post-bump value sits behind it"
        (Option.fold (stamped_token raw) ~none:false
           ~some:(String.equal (Id.to_string id_a))
        && has_epoch_frame raw
        && Option.fold (stamped_epoch raw) ~none:false
             ~some:(String.equal (Epoch.to_string e2)));
      let* rr2 =
        Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity:bound_a
          ~epoch:(Epoch.Bound e2, Epoch.Bound e3)
      in
      let ( (_ : Guard_sink.t)
          , fl2
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , eo2
          , h2 ) =
        opened "E4 boot 2" rr2
      in
      check
        "E4 boot 2 is an ordinary Epoch_matched reopen: the window closed after \
         exactly one boot, which only a SECOND boot can prove"
        (epoch_is eo2 "Epoch_matched");
      check "E4 boot 2 still holds both adopted floors"
        (Int.equal (Floors.cardinal fl2) 2);
      Guard_file.close h2)

(* --- E5a: a torn or garbage counter file in the ROOT -------------------------
   The counter cannot be established, so no journal stamp can be checked. A
   stamped journal is HELD with its floors cleared for this boot's view only,
   and not one byte is rewritten, which is what lets the floors come back when
   the file reads again. An unstamped journal has nothing to check and no
   counter to stamp with, so it keeps its floors and stays unstamped. *)

let () =
  section "E5a: an unreadable root counter holds the journal, then heals";
  in_parent (fun parent ->
      let root = Filename.concat parent "store" in
      let dir = Filename.concat parent "store.guard" in
      let dir2 = Filename.concat parent "step20.guard" in
      let r5 = replica "e5" and t5 = tab 6 in
      let* () = make_root root in
      let id_binding = identity_of root in
      let epoch_file = Store.epoch_path (Root.v root) in
      let (seen1, now1), (_ : Store.epoch_origin) = bump root in
      let* rr1 =
        Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity:id_binding
          ~epoch:(seen1, now1)
      in
      let sink1, (_ : Floors.t), (_ : Guard_file.verdict),
          (_ : Guard_file.identity_outcome), (_ : Guard_file.epoch_outcome), h1 =
        opened "E5a boot 1" rr1
      in
      let* () = put "E5a floor" sink1 (adv r5 t5 1 Water.bottom) in
      let* () = Guard_file.close h1 in
      let* good_bytes = read_file epoch_file in
      let garbage = "not-a-counter\n" in
      let* () = write_file epoch_file garbage in
      let (seen_g, now_g), org_g = bump root in
      let* still = read_file epoch_file in
      check
        "E5a a counter file that is not 16 lowercase hex yields (Unresolved, \
         Unresolved) with origin Epoch_malformed, and the bytes are left \
         EXACTLY as found: a re-mint here would manufacture a permanent false \
         divergence"
        (String.equal (origin_str org_g) "Epoch_malformed"
        && String.equal (binding_str seen_g) "Unresolved"
        && String.equal (binding_str now_g) "Unresolved"
        && String.equal still garbage);
      let* raw_before = read_file (journal_of dir) in
      let* rr_g =
        Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity:id_binding
          ~epoch:(seen_g, now_g)
      in
      let ( (_ : Guard_sink.t)
          , fl_g
          , (_ : Guard_file.verdict)
          , o_g
          , eo_g
          , h_g ) =
        opened "E5a held boot" rr_g
      in
      check
        "E5a a STAMPED journal under an unresolved counter is Epoch_unresolved \
         with its floors cleared for this boot's view, and the boot still \
         serves rather than refusing"
        (id_is o_g "Matched"
        && epoch_is eo_g "Epoch_unresolved"
        && Int.equal (Floors.cardinal fl_g) 0);
      let* () = Guard_file.close h_g in
      let* raw_after = read_file (journal_of dir) in
      check
        "E5a the hold is strict: not one byte of that journal was rewritten, \
         appended to, or compacted away"
        (String.equal raw_after raw_before);
      let* () = write_file epoch_file (String.make 64 'a') in
      let (seen_o, now_o), org_o = bump root in
      check
        "E5a an OVERSIZED counter file is refused by the size cap as \
         Epoch_unreadable, on the same (Unresolved, Unresolved) pair"
        (String.equal (origin_str org_o) "Epoch_unreadable"
        && String.equal (binding_str seen_o) "Unresolved"
        && String.equal (binding_str now_o) "Unresolved");
      let* () = write_file epoch_file good_bytes in
      let (seen_h, now_h), org_h = bump root in
      let* rr_h =
        Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity:id_binding
          ~epoch:(seen_h, now_h)
      in
      let ( (_ : Guard_sink.t)
          , fl_h
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , eo_h
          , h_h ) =
        opened "E5a healed boot" rr_h
      in
      check
        "E5a the hold's whole point: once the counter file reads again the \
         floors are still there, matched against the stamp the held boots \
         never touched"
        (String.equal (origin_str org_h) "Epoch_bumped"
        && epoch_is eo_h "Epoch_matched"
        && Int.equal (Floors.cardinal fl_h) 1
        && floor_at fl_h r5 t5 ~is:1);
      let* () = Guard_file.close h_h in
      (* The other arm: no stamp to check, and no counter to stamp with. *)
      let* () =
        plant_step20 dir2 (token_of root)
          [ adv r5 t5 1 Water.bottom; adv r5 (tab 7) 1 Water.bottom ]
      in
      let* () = write_file epoch_file garbage in
      let (seen_u, now_u), (_ : Store.epoch_origin) = bump root in
      let* rr_u =
        Guard_file.open_ ~dir:dir2 ~cap:8 ~head_water:no_heads
          ~identity:id_binding ~epoch:(seen_u, now_u)
      in
      let ( (_ : Guard_sink.t)
          , fl_u
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , eo_u
          , h_u ) =
        opened "E5a unstamped boot" rr_u
      in
      let* () = Guard_file.close h_u in
      let* raw_u = read_file (journal_of dir2) in
      check
        "E5a an UNSTAMPED journal under an unresolved counter keeps its floors \
         and stays unstamped: the upgrade window simply waits one more boot"
        (epoch_is eo_u "Epoch_unresolved"
        && Int.equal (Floors.cardinal fl_u) 2
        && not (has_epoch_frame raw_u));
      Lwt.return_unit)

(* --- E5b: a torn epoch FRAME in the journal ---------------------------------
   Damage is not the upgrade window. A stamp that carries no readable counter
   makes no claim at all, and collapsing that into "adopt on trust" would keep
   floors nothing can justify. Two damage shapes, and they report DIFFERENT
   counts on purpose:

   (i) the frame unframes cleanly and only its PAYLOAD is no counter, so the
   event fold resumes AFTER the frame and every floor behind it is still
   reachable - the divergence report therefore counts what it drops;

   (ii) the bytes will not unframe at all, so the fold stops at the damage by
   construction, nothing behind it is readable, and the count is honestly zero
   rather than a number nobody can stand behind.

   Both clear their floors, which is the part that matters. Every offset is
   computed from the codec (the identity frame's own length plus the
   [len][tag] prelude), never hardcoded. *)

let () =
  section "E5b: a torn stamp is corruption, never the upgrade window";
  in_parent (fun parent ->
      let dir_i = Filename.concat parent "guard-payload" in
      let dir_ii = Filename.concat parent "guard-crc" in
      let r5 = replica "e5b" in
      let ta = tab 8 and tb = tab 9 in
      let e1 = Epoch.succ Epoch.bottom in
      let e2 = Epoch.succ e1 in
      let floors_bytes =
        String.concat ""
          (List.map Guard_sink.Codec.to_bytes
             [ adv r5 ta 1 Water.bottom; adv r5 tb 1 Water.bottom ])
      in
      (* (i) A well-formed tag-5 frame carrying 16 characters that are not 16
         lowercase hex. The frame's width is unchanged, so the floors behind it
         sit at exactly the offset a real stamp would put them at. *)
      let no_counter = "nothexnothexnot!" in
      let planted_i =
        ident_frame id_a
        ^ Guard_sink.Codec.frame ~tag:Guard_file.tag_epoch ~payload:no_counter
        ^ floors_bytes
      in
      let* () = plant_raw dir_i planted_i in
      check
        "E5b (i) the fixture is a WELL-FORMED epoch frame whose payload is no \
         counter, the same width as a real stamp, with two decodable floors \
         standing behind it"
        (has_epoch_frame planted_i
        && Option.fold (stamped_epoch planted_i) ~none:false
             ~some:(String.equal no_counter)
        && Result.fold (Epoch.of_string no_counter)
             ~ok:(fun (_ : Epoch.t) -> false)
             ~error:(fun `Malformed -> true)
        && Int.equal
             (String.length planted_i)
             (String.length (ident_frame id_a ^ epoch_frame e1 ^ floors_bytes)));
      let* rr_i =
        Guard_file.open_ ~dir:dir_i ~cap:8 ~head_water:no_heads
          ~identity:bound_a ~epoch:(Epoch.Bound e1, Epoch.Bound e2)
      in
      let ( (_ : Guard_sink.t)
          , fl_i
          , (_ : Guard_file.verdict)
          , o_i
          , eo_i
          , h_i ) =
        opened "E5b (i)" rr_i
      in
      check
        "E5b (i) a cleanly unframed stamp that is no counter is Epoch_diverged \
         2, never Epoch_adopted: the fold resumes past the frame, so the report \
         counts the floors it is about to drop"
        (id_is o_i "Matched" && epoch_is eo_i "Epoch_diverged 2");
      let* () = Guard_file.close h_i in
      let* raw_i = read_file (journal_of dir_i) in
      check
        "E5b (i) and it drops every one of them, leaving a well-formed stamp at \
         this boot's value"
        (Int.equal (Floors.cardinal fl_i) 0
        && no_floor fl_i r5 ta
        && no_floor fl_i r5 tb
        && has_epoch_frame raw_i
        && Option.fold (stamped_epoch raw_i) ~none:false
             ~some:(String.equal (Epoch.to_string e2)));
      (* (ii) The same journal with the CRC broken instead: one byte flipped
         inside the stamp's payload, so the frame will not unframe at all. *)
      let planted_ii = ident_frame id_a ^ epoch_frame e1 ^ floors_bytes in
      let torn = corrupt_at planted_ii epoch_payload_at in
      let* () = plant_raw dir_ii torn in
      check
        "E5b (ii) the fixture damaged the EPOCH frame and nothing else: the \
         identity frame in front of it still reads back byte for byte"
        (Option.equal String.equal (stamped_token torn)
           (stamped_token planted_ii)
        && not
             (Option.equal String.equal (stamped_epoch torn)
                (stamped_epoch planted_ii)));
      let* rr_ii =
        Guard_file.open_ ~dir:dir_ii ~cap:8 ~head_water:no_heads
          ~identity:bound_a ~epoch:(Epoch.Bound e1, Epoch.Bound e2)
      in
      let ( (_ : Guard_sink.t)
          , fl_ii
          , (_ : Guard_file.verdict)
          , o_ii
          , eo_ii
          , h_ii ) =
        opened "E5b (ii)" rr_ii
      in
      let* () = Guard_file.close h_ii in
      let* raw_ii = read_file (journal_of dir_ii) in
      check
        "E5b (ii) bytes that will not unframe are Epoch_diverged 0, never \
         Epoch_matched and never Epoch_adopted: the fold stops at the damage, \
         nothing behind it is readable, and the floors clear all the same"
        (id_is o_ii "Matched"
        && epoch_is eo_ii "Epoch_diverged 0"
        && Int.equal (Floors.cardinal fl_ii) 0
        && has_epoch_frame raw_ii
        && Option.fold (stamped_epoch raw_ii) ~none:false
             ~some:(String.equal (Epoch.to_string e2)));
      Lwt.return_unit)

(* --- E6: the crash between the root bump and the journal restamp -------------
   The register names this one itself: a journal whose stamp lagged a boot
   false-positives into a floor wipe, accept-side but noisy. A real fork and
   SIGKILL cannot reliably hit that window, so the POST-crash state is
   constructed directly - the same trade the torn-tail reasoning already
   accepts. This test exists to LOCK the behaviour in, so a later change that
   turns it into a refusal is caught. *)

let () =
  section "E6: a lagged stamp costs one noisy wipe, never a refusal";
  in_parent (fun parent ->
      let root = Filename.concat parent "store" in
      let dir = Filename.concat parent "store.guard" in
      let r6 = replica "e6" and t6 = tab 10 in
      let* () = make_root root in
      let id_binding = identity_of root in
      let (seen1, now1), (_ : Store.epoch_origin) = bump root in
      let* rr1 =
        Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity:id_binding
          ~epoch:(seen1, now1)
      in
      let sink1, (_ : Floors.t), (_ : Guard_file.verdict),
          (_ : Guard_file.identity_outcome), (_ : Guard_file.epoch_outcome), h1 =
        opened "E6 boot 1" rr1
      in
      let* () = put "E6 floor" sink1 (adv r6 t6 1 Water.bottom) in
      let* () = Guard_file.close h1 in
      (* The crashed boot: it advanced the root counter and died before it
         opened, and therefore before it restamped, any journal. *)
      let (seen2, now2), (_ : Store.epoch_origin) = bump root in
      let* raw_lagged = read_file (journal_of dir) in
      check
        "E6 the crashed boot advanced the root counter past the journal's \
         stamp, which is exactly the post-crash state being constructed"
        (String.equal (binding_str seen2) (binding_str now1)
        && Option.fold (stamped_epoch raw_lagged) ~none:false
             ~some:(fun (s : string) ->
               String.equal s (binding_str now1)
               && not (String.equal s (binding_str now2))));
      let (seen3, now3), (_ : Store.epoch_origin) = bump root in
      let* rr3 =
        Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity:id_binding
          ~epoch:(seen3, now3)
      in
      let sink3, fl3, (_ : Guard_file.verdict), o3, eo3, h3 =
        opened "E6 next boot" rr3
      in
      check
        "E6 the next boot reads Epoch_diverged 1 on a same-lineage store: the \
         register's own named false positive, pinned here as CORRECT"
        (id_is o3 "Matched" && epoch_is eo3 "Epoch_diverged 1");
      check
        "E6 the wipe degrades accept-side: open_ answered Ok, the floors are \
         gone, and the replay re-applies as a visible duplicate"
        (Int.equal (Floors.cardinal fl3) 0
        && String.equal (replay_of sink3 fl3 r6 t6 1) "Fresh");
      let* () = put "E6 floor after the wipe" sink3 (adv r6 t6 1 Water.bottom) in
      let* () = Guard_file.close h3 in
      let (seen4, now4), (_ : Store.epoch_origin) = bump root in
      let* rr4 =
        Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity:id_binding
          ~epoch:(seen4, now4)
      in
      let ( (_ : Guard_sink.t)
          , fl4
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , eo4
          , h4 ) =
        opened "E6 recovery boot" rr4
      in
      check
        "E6 appends after the wipe are still accepted, and the boot after it is \
         quiet again with the new floor standing"
        (epoch_is eo4 "Epoch_matched"
        && Int.equal (Floors.cardinal fl4) 1
        && floor_at fl4 r6 t6 ~is:1);
      Guard_file.close h4)

(* --- E7: one boot's pair, both channels, independent verdicts ----------------
   Through the real [Tea_server_pack.open_guards] composition. The counter is
   resolved ONCE per boot and shared, so both journals must leave a boot
   carrying the SAME stamp; a per-open bump would stamp the second channel one
   value past the first and clear it on every ordinary boot. Then a stale file
   is swapped in on ONE channel: that channel clears and the other, through
   the same boot, keeps its floor.

   [open_guards] is SYNCHRONOUS (it runs its own [Lwt_main.run], as the
   composition site calls it), so the Lwt setup and teardown are sequenced in
   separate slices AROUND the call: a nested run is a fatal error. *)

let () =
  section "E7: one shared (seen, now) pair, two channels, one verdict each";
  let parent = fresh_scratch () in
  let root = Filename.concat parent "store" in
  let guard_dir = Filename.concat parent "store.guard" in
  let ws_file = journal_of guard_dir in
  let rpc_file = journal_of (Filename.concat guard_dir "rpc") in
  let r7 = replica "e7" in
  let ws_tab = tab 11 and rpc_tab = tab 12 in
  let heads7 (_ : Replica.t) : Water.t option = Some (Water.of_date 900L) in
  let () = Lwt_main.run (let* () = mkdir parent in make_root root) in
  let id_binding = identity_of root in
  let pair1, (_ : Store.epoch_origin) = bump root in
  let { Tea_server_pack.ws = ws1; ws_journal = wsj1; rpc = rpc1; rpc_journal = rpcj1 }
      =
    Tea_server_pack.open_guards ~guard_dir ~head_water:heads7
      ~identity:id_binding ~epoch:pair1 ()
  in
  let persist_at (what : string) (g : Dguard.t) (t : Tab_id.t) (w : int) : unit =
    Lwt_main.run
      (Dguard.persist g ~replica:r7 ~tab:t ~seq:(seq 1)
         ~water:(Water.of_date (Int64.of_int w)))
    |> Result.fold
         ~ok:(fun () -> ())
         ~error:(fun (e : Guard_sink.err) ->
           Printf.printf "FAIL - test setup: %s (%s)\n%!" what (sink_err_name e);
           exit 1)
  in
  let () =
    check "E7 both channels open on a bare root off ONE (seen, now) pair"
      (Option.is_some wsj1 && Option.is_some rpcj1);
    persist_at "E7 ws floor" ws1 ws_tab 5;
    persist_at "E7 rpc floor" rpc1 rpc_tab 7
  in
  let () = Lwt_main.run (let* () = close_journal wsj1 in close_journal rpcj1) in
  (* The stale file, captured before the next boot restamps it. *)
  let ws_stale = Lwt_main.run (read_file ws_file) in
  let pair2, (_ : Store.epoch_origin) = bump root in
  let { Tea_server_pack.ws = ws2; ws_journal = wsj2; rpc = rpc2; rpc_journal = rpcj2 }
      =
    Tea_server_pack.open_guards ~guard_dir ~head_water:heads7
      ~identity:id_binding ~epoch:pair2 ()
  in
  let () =
    check
      "E7 after the clean restart each channel still holds its OWN floor and \
       not the other's"
      (floor_at (Dguard.floors ws2) r7 ws_tab ~is:1
      && floor_at (Dguard.floors rpc2) r7 rpc_tab ~is:1
      && no_floor (Dguard.floors ws2) r7 rpc_tab
      && no_floor (Dguard.floors rpc2) r7 ws_tab)
  in
  let () = Lwt_main.run (let* () = close_journal wsj2 in close_journal rpcj2) in
  let ws_now, rpc_now =
    Lwt_main.run
      (let* a = read_file ws_file in
       let* b = read_file rpc_file in
       Lwt.return (a, b))
  in
  let () =
    check
      "E7 both journals left that boot carrying the SAME stamp, and it is the \
       boot's post-bump value: the counter is resolved once per BOOT, not once \
       per open_journal call"
      (Option.equal String.equal (stamped_epoch ws_now) (stamped_epoch rpc_now)
      && Option.fold (stamped_epoch ws_now) ~none:false
           ~some:(String.equal (binding_str (snd pair2))))
  in
  (* The divergence, on ONE channel only. *)
  let () = Lwt_main.run (write_file ws_file ws_stale) in
  let pair3, (_ : Store.epoch_origin) = bump root in
  let { Tea_server_pack.ws = ws3; ws_journal = wsj3; rpc = rpc3; rpc_journal = rpcj3 }
      =
    Tea_server_pack.open_guards ~guard_dir ~head_water:heads7
      ~identity:id_binding ~epoch:pair3 ()
  in
  let () =
    check
      "E7 the channel whose journal was swapped for a STALE one comes up empty"
      (Int.equal (Floors.cardinal (Dguard.floors ws3)) 0
      && no_floor (Dguard.floors ws3) r7 ws_tab);
    check
      "E7 and the OTHER channel, through the very same boot and the very same \
       pair, keeps its floor: the stamp is checked per journal"
      (floor_at (Dguard.floors rpc3) r7 rpc_tab ~is:1
      && Int.equal (Floors.cardinal (Dguard.floors rpc3)) 1)
  in
  let () = Lwt_main.run (let* () = close_journal wsj3 in close_journal rpcj3) in
  let names (channel : string) (o : Guard_file.epoch_outcome) (needle : string) :
      bool =
    Tea_server_pack.explain_epoch_outcome ~channel o
    |> Option.fold ~none:false ~some:(fun (line : string) ->
           contains line channel && contains line needle
           && Option.fold
                (byte_at line (String.length line - 1))
                ~none:false ~some:(Char.equal '\n'))
  in
  let () =
    check
      "E7 explain_epoch_outcome stays silent on Epoch_matched: the ordinary \
       reopen is the no-news arm"
      (Option.is_none
         (Tea_server_pack.explain_epoch_outcome ~channel:"websocket"
            Guard_file.Epoch_matched));
    check
      "E7 and every other arm answers one newline-terminated line that names \
       the channel, the count where there is one, and the duplicate direction"
      (names "websocket" Guard_file.Epoch_adopted "kept on trust"
      && names "rpc" (Guard_file.Epoch_diverged 3) "ALL 3 delivery floor(s)"
      && names "rpc" (Guard_file.Epoch_diverged 3) "visible duplicates"
      && names "websocket" Guard_file.Epoch_unresolved "HELD")
  in
  Lwt_main.run (rm_rf parent)

(* --- E8: the counter's own staging files are swept before the read -----------
   The bump writes through an [O_EXCL] temp beside the counter, so a boot that
   dies between the create and the rename strands one
   [tea.epoch.tmp.<pid>.<entropy>] file INSIDE the pack root. Nothing else
   removes it, and a root that collects one per crash leaks entries into the
   directory irmin-pack owns. The sweep runs BEFORE the read, and it matches
   the prefix EXACTLY, so the counter itself and any neighbour that merely
   starts the same way are left alone. *)

let () =
  section "E8: stranded staging files are swept, by exact prefix";
  in_parent (fun parent ->
      let root = Filename.concat parent "store" in
      let* () = make_root root in
      let epoch_file = Store.epoch_path (Root.v root) in
      let stray = epoch_file ^ ".tmp.999.deadbeef" in
      let decoy = epoch_file ^ "X.tmp.1.aa" in
      let decoy_bytes = "not a staging file of this counter\n" in
      let ((_ : Epoch.binding), now1), (_ : Store.epoch_origin) = bump root in
      let* () = write_file stray "a dead boot left this behind" in
      let* () = write_file decoy decoy_bytes in
      let* before = entries_of root in
      let (seen2, now2), org2 = bump root in
      let* after = entries_of root in
      let held (entries : string list) (path : string) : bool =
        List.exists (String.equal (Filename.basename path)) entries
      in
      check
        "E8 a stranded tea.epoch.tmp.* file is there before the bump and gone \
         after it, so the sweep runs and the fixture is not vacuous"
        (held before stray && not (held after stray));
      check
        "E8 and the bump behaved exactly as an ordinary boot's does through \
         the sweep: bumped, one step, off the previous post-bump value"
        (String.equal (origin_str org2) "Epoch_bumped"
        && String.equal (binding_str seen2) (binding_str now1)
        && Option.fold (bound_epoch seen2) ~none:false ~some:(fun (s : Epoch.t) ->
               String.equal (binding_str now2) (Epoch.to_string (Epoch.succ s))));
      let* decoy_after = read_file decoy in
      check
        "E8 the prefix is matched EXACTLY: the counter itself and a \
         tea.epochX.tmp.* neighbour both survive, the neighbour byte for byte"
        (held after epoch_file && held after decoy
        && String.equal decoy_after decoy_bytes);
      Lwt.return_unit)

(* --- E9: the counter's hex domain stops below the saturation point -----------
   [succ] saturates rather than wraps, because a wrap would re-mint an old
   stamp and an old stamp is exactly what this counter exists to expose. That
   makes the saturation point the last value a journal may carry as a LIVE
   stamp, so [of_string] refuses everything above it instead of admitting a
   counter whose successor is itself. Pure: no root and no journal. *)

let () =
  section "E9: of_string refuses the counters whose succ could not move";
  let max_ok =
    must "Store_epoch.of_string accepts 7fffffffffffffff"
      (Result.to_option (Epoch.of_string "7fffffffffffffff"))
  in
  check
    "E9 of_string REFUSES 16 lowercase hex above the saturation point, where \
     succ would have to wrap"
    (Result.fold
       (Epoch.of_string "ffffffffffffffff")
       ~ok:(fun (_ : Epoch.t) -> false)
       ~error:(fun `Malformed -> true));
  check "E9 and accepts the saturation point itself, at its own spelling"
    (String.equal (Epoch.to_string max_ok) "7fffffffffffffff");
  check "E9 succ SATURATES there instead of wrapping round to an old stamp"
    (Epoch.equal (Epoch.succ max_ok) max_ok);
  check
    "E9 so the saturated successor still round-trips through to_string and \
     of_string, and lands back on the same counter"
    (Result.fold
       (Epoch.of_string (Epoch.to_string (Epoch.succ max_ok)))
       ~ok:(fun (e : Epoch.t) -> Epoch.equal e max_ok)
       ~error:(fun `Malformed -> false))

(* --- Verdict ---------------------------------------------------------------- *)

let () =
  if Int.equal !failures 0 then
    Printf.printf
      "\n\
       The boot epoch closes R20b: a cp -r copy is told apart from its origin \
       in BOTH directions by EQUALITY and not by lag (E1, E2), one wipe then \
       quiet (E1b), three clean reboots keep every floor (E3), a pre-step-21 \
       journal is adopted then stamped within one boot (E4), an unreadable \
       root counter holds the bytes and heals (E5a), a torn stamp is \
       corruption and not the upgrade window (E5b), the crash between bump and \
       restamp costs one noisy accept-side wipe and never a refusal (E6), one \
       boot's shared pair still earns each channel its own verdict (E7), a \
       dead boot's staging file is swept by exact prefix (E8), and the hex \
       domain stops below the point where succ could no longer move (E9), \
       R20b.\n\
       %!"
  else (
    Printf.printf "\n%d of the boot epoch properties FAILED.\n%!" !failures;
    exit 1)
