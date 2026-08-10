(** The store identity token, journal half (roadmap step 18, D23, R20a).

    {!store_identity_test} proves the newtype and the resolver; this file
    proves what the token DOES to a journal: the five
    {!Tea_server_pack.Guard_file.identity_outcome} arms, driven through
    [Guard_file.open_] over journals whose header and floors are hand-built
    bytes (guard_water_test's G rule: every verdict count is asserted against
    arithmetic, not a store's clock), plus one end-to-end fixture (I3) where
    the mismatch a water cannot see is manufactured out of two REAL pack
    stores, and one composition fixture (I10) where the ONE binding
    [serve_pack] resolves is handed to both channels through the real
    [Tea_server_pack.open_guards].

    Every verdict is asserted as a COUNT, never grepped from a log line (the
    [unwitnessed] precedent). The file reports every failure and exits at the
    END (pack_guards_test's rule): the properties are independent, and a
    mutation sweep must see exactly which one a mutant breaks. Setup failures
    still exit immediately - a broken fixture attributes to itself, not to
    whichever property it starved.

    I11, I12 and I13 come from the step-18 review and each one is a boundary
    the first cut got wrong: the hold has to cover a held boot's APPENDS and
    not only its open (I11); a header frame whose payload is not a token keeps
    its frame boundary, because "not a token" is decidable while "will not
    unframe" is not (I12); and an unframeable byte 0 in a file that HOLDS
    bytes is an adoption, never the silence a brand-new store earns (I13). *)

module App = struct
  open Tea_core

  (** The smallest persistable app (guard_water_test's): the I checks never
      run [update], but the store functor wants a whole APP. *)
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
  let title = Prim.Title.v "identity-probe"
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
module Msg_seq = Tea_core.Prim.Msg_seq
module Tab_id = Tea_core.Prim.Tab_id
module Water = Tea_core.Prim.Store_water
module Id = Tea_core.Prim.Store_identity
module Replica = Tea_core.Crdt.Replica
open Lwt.Syntax

(* --- Harness ---------------------------------------------------------------
   Property failures accumulate; setup failures exit at once. Filesystem work
   is spelled in [Lwt_unix]/[Lwt_io] with ONE [Lwt.catch] boundary ([fs]) so
   errors stay values and no helper can raise into a check. *)

let failures : int ref = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    incr failures)

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

let replica (name : string) : Replica.t =
  Replica.v (Tea_core.Prim.Session_id.v name)

let tab (n : int) : Tab_id.t =
  must "tab id mint refused a valid seed"
    (Tab_id.of_bytes (List.init 16 (fun i -> (n + i) land 0xff)))

let seq (n : int) : Msg_seq.t =
  must (Printf.sprintf "Msg_seq.of_int refused %d" n) (Msg_seq.of_int n)

let water (n : int) : Water.t = Water.of_date (Int64.of_int n)

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

let scratch_counter : int ref = ref 0

let fresh_scratch () : string =
  let n = !scratch_counter in
  scratch_counter := n + 1;
  Filename.concat
    (Filename.get_temp_dir_name ())
    (Printf.sprintf "ocaml-tea-identity-%.0f-%d" (Unix.gettimeofday () *. 1e6) n)

(** Run [f] over a fresh parent directory; [f] carves what it needs. *)
let in_parent (f : string -> unit Lwt.t) : unit =
  Lwt_main.run
    (let parent = fresh_scratch () in
     let* () = mkdir parent in
     let* () = f parent in
     rm_rf parent)

let journal_of (dir : string) : string = Filename.concat dir "journal"

(** The identity header exactly as [Guard_file] writes it: the token's 32
    characters framed under {!Guard_file.tag_identity}. Built through the
    PUBLIC [Codec.frame] so this fixture and the implementation cannot drift
    apart without one of them changing the shared seam. *)
let header (id : Id.t) : string =
  Guard_sink.Codec.frame ~tag:Guard_file.tag_identity ~payload:(Id.to_string id)

(** A journal no [open_] has touched: chosen bytes, headerless. *)
let plant (dir : string) (events : Guard_sink.event list) : unit Lwt.t =
  let* () = mkdir dir in
  write_file (journal_of dir)
    (String.concat "" (List.map Guard_sink.Codec.to_bytes events))

(** The same, stamped: header first, floors after, one generation. *)
let plant_bound (dir : string) (id : Id.t) (events : Guard_sink.event list) :
    unit Lwt.t =
  let* () = mkdir dir in
  write_file (journal_of dir)
    (header id ^ String.concat "" (List.map Guard_sink.Codec.to_bytes events))

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

(** Every arm spelled, so a new constructor is a compile error here and not a
    case nothing asserts. *)
let outcome_str (o : Guard_file.identity_outcome) : string =
  match o with
  | Guard_file.Matched -> "Matched"
  | Guard_file.Freshly_bound -> "Freshly_bound"
  | Guard_file.Adopted_unbound (n : int) -> Printf.sprintf "Adopted_unbound %d" n
  | Guard_file.Rebound (n : int) -> Printf.sprintf "Rebound %d" n
  | Guard_file.Unresolved_cleared (n : int) ->
    Printf.sprintf "Unresolved_cleared %d" n

let outcome_is (o : Guard_file.identity_outcome) (want : string) : bool =
  String.equal (outcome_str o) want

let floor_at (fl : Floors.t) (r : Replica.t) (t : Tab_id.t) ~(is : int) : bool =
  Floors.find ~replica:r ~tab:t fl
  |> Option.fold ~none:false ~some:(fun n -> Int.equal (Msg_seq.to_int n) is)

let no_floor (fl : Floors.t) (r : Replica.t) (t : Tab_id.t) : bool =
  Option.is_none (Floors.find ~replica:r ~tab:t fl)

let verdict_is (v : Guard_file.verdict) ~(kept : int) ~(behind : int)
    ~(no_branch : int) ~(unwitnessed : int) : bool =
  Int.equal v.Guard_file.kept kept
  && Int.equal v.dropped_behind behind
  && Int.equal v.dropped_no_branch no_branch
  && Int.equal v.unwitnessed unwitnessed

let heads (l : (Replica.t * Water.t) list) : Replica.t -> Water.t option =
  Guard_file.head_water_of_list l

let no_heads : Replica.t -> Water.t option = heads []

(** Byte [i] of [s], with no index anywhere: the I9 raw-layout probe walks a
    sequence instead. *)
let byte_at (s : string) (i : int) : char option =
  String.to_seq s |> Seq.drop i |> Seq.uncons |> Option.map fst

(** The 32 token characters a stamped journal carries at offset 5 (after
    [len:4][tag:1]), declined on anything shorter. *)
let stamped_token (s : string) : string option =
  if String.length s >= 37 then Some (String.sub s 5 32) (* @total-accessor *)
  else None

(* One token per store of this file's little world; a constant draw is a
   valid mint and keeps every fixture reproducible byte for byte. *)
let id_a : Id.t = Id.of_draws (fun () -> 0x11)
let id_b : Id.t = Id.of_draws (fun () -> 0x22)
let bound_a : Id.binding = Id.Bound id_a
let bound_b : Id.binding = Id.Bound id_b

let epoch0 :
    Tea_core.Prim.Store_epoch.binding * Tea_core.Prim.Store_epoch.binding =
  ( Tea_core.Prim.Store_epoch.Bound Tea_core.Prim.Store_epoch.bottom
  , Tea_core.Prim.Store_epoch.Bound Tea_core.Prim.Store_epoch.bottom )

(* --- I1: a bound journal met by a different store's binding ----------------- *)

let () =
  in_parent (fun parent ->
      let dir = Filename.concat parent "guard" in
      let ra = replica "i1-a" and rb = replica "i1-b" in
      let* () =
        plant_bound dir id_a
          [ adv ra (tab 1) 3 (water 100); adv rb (tab 1) 7 (water 50) ]
      in
      let* r =
        Guard_file.open_ ~dir ~cap:8
          ~head_water:(heads [ (ra, water 100); (rb, water 90) ])
          ~identity:bound_b ~epoch:epoch0
      in
      let (_ : Guard_sink.t), fl, v, o, (_ : Guard_file.epoch_outcome), h =
        opened "I1" r
      in
      check "I1 a header naming A opened under Bound B is Rebound 2"
        (outcome_is o "Rebound 2");
      check "I1 the rebound journal admits no floors (cardinal 0)"
        (Int.equal (Floors.cardinal fl) 0);
      (* The mislabel check: the identity fact must not be smuggled into the
         per-floor water counters, where it would read as rollback noise. *)
      check "I1 the water verdict is the ALL-ZERO record beside a Rebound"
        (verdict_is v ~kept:0 ~behind:0 ~no_branch:0 ~unwitnessed:0);
      Guard_file.close h)

(* --- I2: the legacy upgrade, a headerless journal is adopted ---------------- *)

let () =
  in_parent (fun parent ->
      let dir = Filename.concat parent "guard" in
      let ra = replica "i2-a" and rb = replica "i2-b" in
      let* () =
        plant dir [ adv ra (tab 1) 3 (water 100); adv rb (tab 1) 7 (water 50) ]
      in
      let* r =
        Guard_file.open_ ~dir ~cap:8
          ~head_water:(heads [ (ra, water 100); (rb, water 90) ])
          ~identity:bound_a ~epoch:epoch0
      in
      let ( (_ : Guard_sink.t)
          , fl
          , (_ : Guard_file.verdict)
          , o
          , (_ : Guard_file.epoch_outcome)
          , h ) =
        opened "I2" r
      in
      (* The exact count is load-bearing twice over: adopted-on-trust is 2
         (not 0, not "some"), and both floors stand. *)
      check "I2 a headerless journal under Bound A is Adopted_unbound 2"
        (outcome_is o "Adopted_unbound 2");
      check "I2 both adopted floors stand (cardinal 2, each at its seq)"
        (Int.equal (Floors.cardinal fl) 2
        && floor_at fl ra (tab 1) ~is:3
        && floor_at fl rb (tab 1) ~is:7);
      Guard_file.close h)

(* --- I3: the R20a kill, out of two REAL stores ------------------------------
   A floor taken against store A whose water sits BELOW independent store B's
   same-named branch head: the shape no water can see (R20a). The companion
   arm runs FIRST: under A's own binding the floor is KEPT, proving the
   fixture genuinely slips past the water filter, before the Bound B arm is
   allowed to claim the token caught it. *)

let () =
  in_parent (fun parent ->
      let root_a = Filename.concat parent "store-a" in
      let root_b = Filename.concat parent "store-b" in
      let* ta = Store.create ~now:(fun () -> 5L) (Root.v root_a) in
      let* sa = Store.main_session ta in
      let* w_a = Store.commit sa ~label:"a1" 1 in
      let* () = Store.close ta in
      let* tb = Store.create ~now:(fun () -> 9L) (Root.v root_b) in
      let* sb = Store.main_session tb in
      let* (_ : Water.t) = Store.commit sb ~label:"b1" 1 in
      let* (_ : Water.t) = Store.commit sb ~label:"b2" 2 in
      let* w_b = Store.head_water sb in
      let* b_waters = Store.branch_waters tb in
      let* () = Store.close tb in
      check "I3 setup is non-vacuous: B's main head stands STRICTLY above W"
        (Water.compare w_a w_b < 0);
      let dir = Filename.concat parent "rpc" in
      let* () =
        plant_bound dir id_a [ adv Store.main_replica (tab 3) 4 w_a ]
      in
      let b_heads = Guard_file.head_water_of_list b_waters in
      (* Companion arm: same bytes, the OWNING binding. *)
      let* r_own =
        Guard_file.open_ ~dir ~cap:8 ~head_water:b_heads ~identity:bound_a ~epoch:epoch0
      in
      let ( (_ : Guard_sink.t)
          , fl_own
          , (_ : Guard_file.verdict)
          , o_own
          , (_ : Guard_file.epoch_outcome)
          , h_own ) =
        opened "I3 companion" r_own
      in
      check
        "I3 companion: under Bound A the stranger floor is KEPT (Matched, \
         cardinal 1) - B's head really does cover it"
        (outcome_is o_own "Matched"
        && Int.equal (Floors.cardinal fl_own) 1
        && floor_at fl_own Store.main_replica (tab 3) ~is:4);
      let* () = Guard_file.close h_own in
      (* The kill: same bytes, the OTHER store's binding. *)
      let* r_other =
        Guard_file.open_ ~dir ~cap:8 ~head_water:b_heads ~identity:bound_b ~epoch:epoch0
      in
      let ( (_ : Guard_sink.t)
          , fl_other
          , (_ : Guard_file.verdict)
          , o_other
          , (_ : Guard_file.epoch_outcome)
          , h_other )
          =
        opened "I3 kill" r_other
      in
      check
        "I3 under Bound B the floor the water filter admits is Rebound 1, \
         cardinal 0: the token sees what no water can"
        (outcome_is o_other "Rebound 1" && Int.equal (Floors.cardinal fl_other) 0);
      Guard_file.close h_other)

(* --- I4: the bottom-water stranger ------------------------------------------
   [Floors.filter] keeps a bottom floor UNCONDITIONALLY before consulting any
   head, so a mismatch implemented by starving the filter of heads returns
   cardinal 3 here; only clearing OUTRIGHT returns 0. *)

let () =
  in_parent (fun parent ->
      let dir = Filename.concat parent "guard" in
      let ra = replica "i4-a" and rb = replica "i4-b" in
      let* () =
        plant_bound dir id_a
          [ adv ra (tab 1) 1 Water.bottom
          ; adv ra (tab 2) 2 Water.bottom
          ; adv rb (tab 1) 3 Water.bottom
          ]
      in
      let* r =
        Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity:bound_b ~epoch:epoch0
      in
      let (_ : Guard_sink.t), fl, v, o, (_ : Guard_file.epoch_outcome), h =
        opened "I4" r
      in
      check
        "I4 three bottom floors under a stranger binding are Rebound 3 with \
         cardinal 0, not survivors of a starved head lookup"
        (outcome_is o "Rebound 3" && Int.equal (Floors.cardinal fl) 0);
      check "I4 nothing leaks into unwitnessed (the all-zero verdict again)"
        (verdict_is v ~kept:0 ~behind:0 ~no_branch:0 ~unwitnessed:0);
      Guard_file.close h)

(* --- I5 + I9: compaction preserves the binding, header at byte 0 ------------ *)

let () =
  in_parent (fun parent ->
      let dir = Filename.concat parent "guard" in
      let ra = replica "i5-a" and rb = replica "i5-b" in
      let* () =
        plant_bound dir id_a
          [ adv ra (tab 1) 3 (water 100); adv rb (tab 2) 7 (water 50) ]
      in
      (* rb's floor stands above its head, so dropped_behind = 1 fires the
         existing filtering-open compaction. *)
      let* r1 =
        Guard_file.open_ ~dir ~cap:8
          ~head_water:(heads [ (ra, water 100); (rb, water 10) ])
          ~identity:bound_a ~epoch:epoch0
      in
      let (_ : Guard_sink.t), fl1, v1, o1, (_ : Guard_file.epoch_outcome), h1 =
        opened "I5 boot 1" r1
      in
      check "I5 boot 1 matches its own header while the water filter drops"
        (outcome_is o1 "Matched"
        && verdict_is v1 ~kept:1 ~behind:1 ~no_branch:0 ~unwitnessed:0
        && Int.equal (Floors.cardinal fl1) 1);
      let* () = Guard_file.close h1 in
      (* I9, on the compacted bytes: the header is the FIRST frame. Framing is
         [len:4][tag:1][payload][crc:4], so the tag byte sits at offset 4 and
         a bare header is 41 bytes; a header PLUS the kept floor is strictly
         longer, so a header-at-the-end mutant cannot pass on an empty file. *)
      let* raw = read_file (journal_of dir) in
      check "I9 the compacted journal opens with the tag-4 identity frame"
        (Option.fold (byte_at raw 4) ~none:false ~some:(Char.equal '\004'));
      check "I9 the header carries exactly A's token at offset 5"
        (Option.fold (stamped_token raw) ~none:false
           ~some:(String.equal (Id.to_string id_a)));
      check "I9 the file holds the header AND the kept floor (length > 41)"
        (String.length raw > 41);
      let* r2 =
        Guard_file.open_ ~dir ~cap:8
          ~head_water:(heads [ (ra, water 100); (rb, water 10) ])
          ~identity:bound_a ~epoch:epoch0
      in
      let ( (_ : Guard_sink.t)
          , fl2
          , (_ : Guard_file.verdict)
          , o2
          , (_ : Guard_file.epoch_outcome)
          , h2 ) =
        opened "I5 boot 2" r2
      in
      check
        "I5 boot 2 finds the binding the compaction preserved (Matched, the \
         surviving floor at its stored seq)"
        (outcome_is o2 "Matched"
        && Int.equal (Floors.cardinal fl2) 1
        && floor_at fl2 ra (tab 1) ~is:3);
      Guard_file.close h2)

(* --- I6: adopting forces the restamp ---------------------------------------- *)

let () =
  in_parent (fun parent ->
      let dir = Filename.concat parent "guard" in
      let ra = replica "i6-a" in
      let* () = plant dir [ adv ra (tab 1) 3 (water 100) ] in
      (* Zero drops and a count far below 4*cap: nothing but the forced
         restamp can rewrite this file. *)
      let* r1 =
        Guard_file.open_ ~dir ~cap:8
          ~head_water:(heads [ (ra, water 100) ])
          ~identity:bound_a ~epoch:epoch0
      in
      let ( (_ : Guard_sink.t)
          , fl1
          , (_ : Guard_file.verdict)
          , o1
          , (_ : Guard_file.epoch_outcome)
          , h1 ) =
        opened "I6 boot 1" r1
      in
      check "I6 boot 1 adopts the headerless journal (Adopted_unbound 1)"
        (outcome_is o1 "Adopted_unbound 1" && Int.equal (Floors.cardinal fl1) 1);
      let* () = Guard_file.close h1 in
      let* r2 =
        Guard_file.open_ ~dir ~cap:8
          ~head_water:(heads [ (ra, water 100) ])
          ~identity:bound_a ~epoch:epoch0
      in
      let ( (_ : Guard_sink.t)
          , fl2
          , (_ : Guard_file.verdict)
          , o2
          , (_ : Guard_file.epoch_outcome)
          , h2 ) =
        opened "I6 boot 2" r2
      in
      check
        "I6 boot 2 is Matched with the same floor: the adopt was stamped \
         durably, not re-adopted on every boot"
        (outcome_is o2 "Matched"
        && Int.equal (Floors.cardinal fl2) 1
        && floor_at fl2 ra (tab 1) ~is:3);
      Guard_file.close h2)

(* --- I7: the orderly restart ------------------------------------------------ *)

let () =
  in_parent (fun parent ->
      let dir = Filename.concat parent "guard" in
      let ra = replica "i7-a" in
      let* r1 =
        Guard_file.open_ ~dir ~cap:8
          ~head_water:(heads [ (ra, water 42) ])
          ~identity:bound_a ~epoch:epoch0
      in
      let sink, fl1, (_ : Guard_file.verdict), o1, (_ : Guard_file.epoch_outcome), h1 =
        opened "I7 boot 1" r1
      in
      check "I7 a brand-new journal is Freshly_bound and empty"
        (outcome_is o1 "Freshly_bound" && Int.equal (Floors.cardinal fl1) 0);
      let* () = put "I7 advance" sink (adv ra (tab 4) 9 (water 42)) in
      let* () = Guard_file.close h1 in
      let* r2 =
        Guard_file.open_ ~dir ~cap:8
          ~head_water:(heads [ (ra, water 42) ])
          ~identity:bound_a ~epoch:epoch0
      in
      let ( (_ : Guard_sink.t)
          , fl2
          , (_ : Guard_file.verdict)
          , o2
          , (_ : Guard_file.epoch_outcome)
          , h2 ) =
        opened "I7 boot 2" r2
      in
      check
        "I7 a clean restart under the SAME binding is Matched with the floor \
         intact: a re-minting resolver would flip this into a mass wipe"
        (outcome_is o2 "Matched"
        && Int.equal (Floors.cardinal fl2) 1
        && floor_at fl2 ra (tab 4) ~is:9);
      Guard_file.close h2)

(* --- I8: Unresolved is a strict hold ---------------------------------------- *)

let () =
  in_parent (fun parent ->
      let dir = Filename.concat parent "guard" in
      let ra = replica "i8-a" and rb = replica "i8-b" in
      let* () =
        plant_bound dir id_a
          [ adv ra (tab 5) 2 Water.bottom; adv rb (tab 6) 4 Water.bottom ]
      in
      let* before = read_file (journal_of dir) in
      let* r1 =
        Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads
          ~identity:Id.Unresolved ~epoch:epoch0
      in
      let ( (_ : Guard_sink.t)
          , fl1
          , (_ : Guard_file.verdict)
          , o1
          , (_ : Guard_file.epoch_outcome)
          , h1 ) =
        opened "I8 unresolved" r1
      in
      check
        "I8 an unresolvable caller clears the floors it cannot confirm \
         (Unresolved_cleared 2, cardinal 0)"
        (outcome_is o1 "Unresolved_cleared 2" && Int.equal (Floors.cardinal fl1) 0);
      let* () = Guard_file.close h1 in
      let* after = read_file (journal_of dir) in
      check
        "I8 the hold is strict: not one byte of the journal was rewritten \
         (no compaction, no re-stamp)"
        (String.equal before after);
      (* The recovery arm, the whole reason the hold exists: a boot that can
         read the token again finds the ORIGINAL header and its floors. *)
      let* r2 =
        Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity:bound_a ~epoch:epoch0
      in
      let ( (_ : Guard_sink.t)
          , fl2
          , (_ : Guard_file.verdict)
          , o2
          , (_ : Guard_file.epoch_outcome)
          , h2 ) =
        opened "I8 recovery" r2
      in
      check "I8 a later boot that reads the token recovers BOTH floors"
        (outcome_is o2 "Matched"
        && Int.equal (Floors.cardinal fl2) 2
        && floor_at fl2 ra (tab 5) ~is:2
        && floor_at fl2 rb (tab 6) ~is:4);
      Guard_file.close h2)

(* --- I10: both journals, ONE binding, through the real composition ----------
   [open_guards] hands the single resolved binding to both channels; each
   journal carries and checks its OWN header, so neither channel's stamp can
   satisfy the other's check. Stated as the presence/absence pair
   pack_guards_test demands. *)

let close_journal (j : Guard_file.t option) : unit Lwt.t =
  Option.fold ~none:Lwt.return_unit ~some:Guard_file.close j

(* [open_guards] is SYNCHRONOUS (it runs its own [Lwt_main.run], exactly as
   the composition site calls it), so this fixture sequences its Lwt setup and
   teardown in separate [Lwt_main.run] slices AROUND the call rather than
   inside one - a nested run is a fatal error, not a style choice. *)
let () =
  let r_ws = replica "i10-ws" and r_rpc = replica "i10-rpc" in
  let plant_tree (guard_dir : string) : unit Lwt.t =
    let* () = plant_bound guard_dir id_a [ adv r_ws (tab 7) 5 Water.bottom ] in
    plant_bound (Filename.concat guard_dir "rpc") id_a
      [ adv r_rpc (tab 8) 6 Water.bottom ]
  in
  let parent = fresh_scratch () in
  (* Owning arm: each channel holds its OWN floor and not the other's. *)
  let dir_own = Filename.concat parent "guard-own" in
  let () =
    Lwt_main.run
      (let* () = mkdir parent in
       plant_tree dir_own)
  in
  let { Tea_server_pack.ws; ws_journal; rpc; rpc_journal } =
    Tea_server_pack.open_guards ~guard_dir:dir_own ~head_water:no_heads
      ~identity:bound_a ~epoch:epoch0 ()
  in
  let ws_fl = Dguard.floors ws and rpc_fl = Dguard.floors rpc in
  check
    "I10 under the owning binding each channel keeps its OWN floor at its \
     stored seq"
    (floor_at ws_fl r_ws (tab 7) ~is:5 && floor_at rpc_fl r_rpc (tab 8) ~is:6);
  check "I10 and neither channel holds the OTHER channel's floor"
    (no_floor ws_fl r_rpc (tab 8) && no_floor rpc_fl r_ws (tab 7));
  let () =
    Lwt_main.run
      (let* () = close_journal ws_journal in
       close_journal rpc_journal)
  in
  (* Stranger arm: the SAME single binding clears BOTH ledgers, and both
     files leave restamped to the new store. *)
  let dir_other = Filename.concat parent "guard-other" in
  let () = Lwt_main.run (plant_tree dir_other) in
  let { Tea_server_pack.ws = ws2; ws_journal = wsj2; rpc = rpc2; rpc_journal = rpcj2 } =
    Tea_server_pack.open_guards ~guard_dir:dir_other ~head_water:no_heads
      ~identity:bound_b ~epoch:epoch0 ()
  in
  check "I10 under a stranger binding BOTH channels come up empty"
    (Int.equal (Floors.cardinal (Dguard.floors ws2)) 0
    && Int.equal (Floors.cardinal (Dguard.floors rpc2)) 0);
  Lwt_main.run
    (let* () = close_journal wsj2 in
     let* () = close_journal rpcj2 in
     let* ws_raw = read_file (journal_of dir_other) in
     let* rpc_raw = read_file (journal_of (Filename.concat dir_other "rpc")) in
     check
       "I10 both journals were restamped to B through the one composition \
        call (each header carries B's token)"
       (Option.fold (stamped_token ws_raw) ~none:false
          ~some:(String.equal (Id.to_string id_b))
       && Option.fold (stamped_token rpc_raw) ~none:false
            ~some:(String.equal (Id.to_string id_b)));
     rm_rf parent)

(* --- I11: the hold covers the APPENDS, not just the open --------------------
   The hold used to suppress the open-time compaction only, so a held boot's
   own appends still wrote this boot's floors into a file whose token belongs
   to some other store, and one append past [4 * cap] re-stamped it outright.
   Either way a restore that puts the journal back beside its TRUE store would
   Match and adopt floors taken against the wrong one, which is silent loss.
   The assertion is the journal BYTE FOR BYTE, before and after appends driven
   well past the compaction trigger. *)

let () =
  in_parent (fun parent ->
      let dir = Filename.concat parent "guard" in
      let ra = replica "i11-a" and rb = replica "i11-b" in
      let* () =
        plant_bound dir id_a
          [ adv ra (tab 5) 2 Water.bottom; adv rb (tab 6) 4 Water.bottom ]
      in
      let* before = read_file (journal_of dir) in
      let* r1 =
        Guard_file.open_ ~dir ~cap:2 ~head_water:no_heads ~identity:Id.Unresolved ~epoch:epoch0
      in
      let ( sink
          , (_ : Floors.t)
          , (_ : Guard_file.verdict)
          , o1
          , (_ : Guard_file.epoch_outcome)
          , h1 ) =
        opened "I11 unresolved" r1
      in
      check "I11 the unresolvable boot clears the claim it cannot confirm"
        (outcome_is o1 "Unresolved_cleared 2");
      (* At cap 2 the compaction trigger is 8 records in the file, so thirty
         appends is well past it, and four distinct tabs also drive the
         in-memory cap eviction the held path still has to run. *)
      let* () =
        Lwt_list.iter_s
          (fun (n : int) ->
            put "I11 held append" sink (adv ra (tab (n land 3)) n Water.bottom))
          (List.init 30 (fun (i : int) -> i + 1))
      in
      let* () = Guard_file.close h1 in
      let* after = read_file (journal_of dir) in
      check
        "I11 thirty appends past the 4*cap trigger rewrote NOT ONE byte of the \
         held journal"
        (String.equal before after);
      (* The whole reason the hold is strict: the true store's boot still finds
         its own header and its own floors, not this boot's. *)
      let* r2 =
        Guard_file.open_ ~dir ~cap:2 ~head_water:no_heads ~identity:bound_a ~epoch:epoch0
      in
      let ( (_ : Guard_sink.t)
          , fl2
          , (_ : Guard_file.verdict)
          , o2
          , (_ : Guard_file.epoch_outcome)
          , h2 ) =
        opened "I11 recovery" r2
      in
      check
        "I11 the boot that reads the token again Matches and recovers the \
         ORIGINAL floors, none of the held boot's"
        (outcome_is o2 "Matched"
        && Int.equal (Floors.cardinal fl2) 2
        && floor_at fl2 ra (tab 5) ~is:2
        && floor_at fl2 rb (tab 6) ~is:4);
      Guard_file.close h2)

(* --- I12: an unreadable header PAYLOAD keeps its frame boundary -------------
   A tag-4 frame with a correct length and a correct CRC whose 32 bytes are
   not a token. The frame is readable, so its boundary is known and the events
   behind it are countable; the payload is decidable, because no store's token
   is spelled in capital Z. Dropping the boundary instead made [open_] decode
   from byte 0, meet the unknown tag, count zero events and report
   [Freshly_bound] - and the forced restamp then erased every intact event
   behind the header. *)

(** The 41-byte header shape ([len:4][tag:1][payload:32][crc:4]) built through
    the PUBLIC codec, so only the payload is hostile. *)
let foreign_header : string =
  Guard_sink.Codec.frame ~tag:Guard_file.tag_identity ~payload:(String.make 32 'Z')

let () =
  in_parent (fun parent ->
      let dir = Filename.concat parent "guard" in
      let ra = replica "i12-a" and rb = replica "i12-b" in
      let events =
        [ adv ra (tab 1) 3 Water.bottom; adv rb (tab 2) 7 Water.bottom ]
      in
      let* () = mkdir dir in
      let* () =
        write_file (journal_of dir)
          (foreign_header
          ^ String.concat "" (List.map Guard_sink.Codec.to_bytes events))
      in
      check "I12 the planted header really is the 41-byte frame shape"
        (Int.equal (String.length foreign_header) 41);
      let* r1 =
        Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity:bound_b ~epoch:epoch0
      in
      let (_ : Guard_sink.t), fl1, v1, o1, (_ : Guard_file.epoch_outcome), h1 =
        opened "I12" r1
      in
      check
        "I12 a header payload that decodes to no token is Rebound 2: the two \
         events behind it are counted, not lost to a Freshly_bound"
        (outcome_is o1 "Rebound 2");
      check
        "I12 the rebound journal admits no floors and reports the all-zero \
         water verdict"
        (Int.equal (Floors.cardinal fl1) 0
        && verdict_is v1 ~kept:0 ~behind:0 ~no_branch:0 ~unwitnessed:0);
      let* () = Guard_file.close h1 in
      let* raw = read_file (journal_of dir) in
      check "I12 the mismatch restamped the journal with B's own token"
        (Option.fold (stamped_token raw) ~none:false
           ~some:(String.equal (Id.to_string id_b)));
      let* r2 =
        Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity:bound_b ~epoch:epoch0
      in
      let ( (_ : Guard_sink.t)
          , fl2
          , (_ : Guard_file.verdict)
          , o2
          , (_ : Guard_file.epoch_outcome)
          , h2 ) =
        opened "I12 boot 2" r2
      in
      check "I12 the next boot Matches the token the rebind wrote"
        (outcome_is o2 "Matched" && Int.equal (Floors.cardinal fl2) 0);
      Guard_file.close h2)

(* --- I13: a corrupt byte 0 in a NON-EMPTY journal is not a fresh store ------
   Every unframe failure at byte 0 used to read as "empty", so a real journal
   whose first frame went bad reported [Freshly_bound]: total floor loss with
   a boot log identical to a brand-new store's first boot. Bytes were written,
   so bytes were meant to be kept; the loss must be audible. *)

(** Byte [i] turned into a byte it certainly is not, through [String.mapi] so
    nothing is read out by index and no partial [Char] accessor is used. *)
let corrupt_at (s : string) (i : int) : string =
  String.mapi
    (fun (j : int) (c : char) ->
      if Int.equal j i then if Char.equal c 'a' then 'b' else 'a' else c)
    s

let () =
  in_parent (fun parent ->
      let dir = Filename.concat parent "guard" in
      let ra = replica "i13-a" and rb = replica "i13-b" in
      let f1 = Guard_sink.Codec.to_bytes (adv ra (tab 1) 3 Water.bottom)
      and f2 = Guard_sink.Codec.to_bytes (adv rb (tab 2) 7 Water.bottom) in
      let planted = corrupt_at (f1 ^ f2) (String.length f1 - 1) in
      let* () = mkdir dir in
      let* () = write_file (journal_of dir) planted in
      check
        "I13 the fixture is non-empty, its first frame is broken in the CRC, \
         and a whole valid frame stands behind it"
        (String.length planted > String.length f1
        && not (String.equal planted (f1 ^ f2)));
      let* r =
        Guard_file.open_ ~dir ~cap:8 ~head_water:no_heads ~identity:bound_a ~epoch:epoch0
      in
      let ( (_ : Guard_sink.t)
          , fl
          , (_ : Guard_file.verdict)
          , o
          , (_ : Guard_file.epoch_outcome)
          , h ) =
        opened "I13" r
      in
      check
        "I13 a corrupt frame at byte 0 of a NON-EMPTY journal is \
         Adopted_unbound 0, never the brand-new-store silence of Freshly_bound"
        (outcome_is o "Adopted_unbound 0");
      check "I13 nothing decoded behind the break (cardinal 0)"
        (Int.equal (Floors.cardinal fl) 0);
      Guard_file.close h)

(* --- Verdict ---------------------------------------------------------------- *)

let () =
  if Int.equal !failures 0 then
    Printf.printf
      "\n\
       The journal token holds: mismatch clears (I1), upgrade adopts (I2), the \
       R20a shape is caught end to end (I3), bottom floors do not survive a \
       stranger (I4), compaction and adoption stamp durably (I5, I6, I9), a \
       clean restart matches (I7), Unresolved holds and recovers (I8), one \
       binding rules both channels (I10), the hold covers the appends too \
       (I11), an unreadable payload keeps its frame boundary (I12), and a \
       corrupt byte 0 is never read as a fresh store (I13), D23.\n\
       %!"
  else (
    Printf.printf "\n%d of the identity properties FAILED.\n%!" !failures;
    exit 1)
