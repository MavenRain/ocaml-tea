(** {!Tea_server_pack.open_guards}: the two delivery channels' guard journals,
    driven directly (roadmap step 15, D20.3).

    The rest of the step-15 suite composes the two journals BY HAND, which
    proves the guard stack behaves once it is wired but says nothing about the
    wiring itself. Everything checked here is a property of the composition and
    of nothing else: which directory each channel's journal is opened at, that
    the two are opened in an order that works on a bare root, and that each
    channel's mirror bound is derived from its OWN cap. Inside [serve_pack]
    none of it is observable, because that function blocks on [Dream.serve] and
    returns only at shutdown; that is why the composition is a named function.

    The load-bearing check is the pair after the restart. Two floors are
    recorded, one per channel, under different tabs; the journals are closed and
    reopened; and each channel must come back holding its OWN floor and NOT the
    other's. Written as a pair deliberately: "the websocket ledger is empty of
    the rpc floor" is satisfied just as well by a ledger that is empty because
    nothing was ever written, so every absence here is stated beside a presence
    that must hold in the same breath. Point both channels at one file and the
    absences fail while the presences stay green. *)

module Guard_file = Tea_server_pack.Guard_file
module Durable_guard = Tea_server.Durable_guard
module Guard_sink = Tea_server.Guard_sink
module Floors = Durable_guard.Floors
module Bound = Tea_server.Replay_guard.Bound
module Replica = Tea_core.Crdt.Replica
module Session_id = Tea_core.Prim.Session_id
module Tab_id = Tea_core.Prim.Tab_id
module Msg_seq = Tea_core.Prim.Msg_seq
module Store_water = Tea_core.Prim.Store_water
module Store_identity = Tea_core.Prim.Store_identity

(* This file reports every failure and exits at the END, where its siblings stop
   at the first. The properties here are INDEPENDENT (layout, open order, bound
   derivation, ledger separation) and a mutation sweep needs to see which of
   them a given mutant breaks: stopping at the first red would report the
   layout and hide whether the separation checks below it are live at all. *)
let failures = ref 0

let check (name : string) (cond : bool) : unit =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    incr failures;
    Printf.printf "FAIL - %s\n%!" name)

(* Still immediate: [die] has no value to return, so nothing downstream could
   run anyway. *)
let die (what : string) : 'a =
  Printf.printf "FAIL - %s\n%!" what;
  exit 1

let tab_of (s : string) : Tab_id.t =
  Result.fold
    ~error:(fun (_ : Tab_id.err) -> die (Printf.sprintf "Tab_id.of_string accepts %S" s))
    ~ok:(fun (t : Tab_id.t) -> t)
    (Tab_id.of_string s)

let seq_of (n : int) : Msg_seq.t =
  Option.fold
    ~none:(fun () -> die (Printf.sprintf "Msg_seq.of_int accepts %d" n))
    ~some:(fun (s : Msg_seq.t) () -> s)
    (Msg_seq.of_int n) ()

(* The names [serve_pack] would derive from a root of [<parent>/store]. Spelled
   out rather than taken from the code under test, so a change to the layout
   fails here instead of following the test along. *)
let parent = Filename.temp_dir "ocaml-tea-pack-guards" ""
let guard_dir = Filename.concat parent "store.guard"
let rpc_dir = Filename.concat guard_dir "rpc"
let replica = Replica.v (Session_id.v "pack-guards-canonical")

(* [head] stands above every floor written below, so the R20 boot filter keeps
   all of them. This file asks which LEDGER a floor lands in, and a filter drop
   would empty a ledger for a reason that has nothing to do with that. *)
let head = Store_water.of_date 900L
let rpc_water = Store_water.of_date 5L
let ws_water = Store_water.of_date 7L

let head_water (r : Replica.t) : Store_water.t option =
  if Replica.equal r replica then Some head else None

(* ONE binding for both channels and both boots, the shape the composition site
   passes: the token names the STORE, so a per-channel value would make the two
   ledgers differ for a reason this file is not asking about. *)
let store_id : Store_identity.t = Store_identity.of_draws (fun () -> 0x1f)
let identity : Store_identity.binding = Store_identity.Bound store_id

let rpc_tab = tab_of "a1b2c3d4e5f60718293a4b5c6d7e8f90"
let ws_tab = tab_of "0f9e8d7c6b5a4938271605f4e3d2c1b0"

let put (what : string) (g : Durable_guard.t) ~(tab : Tab_id.t) ~(seq : int)
    ~(water : Store_water.t) : unit =
  Lwt_main.run (Durable_guard.persist g ~replica ~tab ~seq:(seq_of seq) ~water)
  |> Result.fold
       ~ok:(fun () -> ())
       ~error:(fun (_ : Guard_sink.err) -> die what)

let close_journal (what : string) (j : Guard_file.t option) : unit =
  Option.fold
    ~none:(fun () -> die what)
    ~some:(fun (jf : Guard_file.t) () -> Lwt_main.run (Guard_file.close jf))
    j ()

let stamped (g : Durable_guard.t) (tab : Tab_id.t) :
    (Msg_seq.t * Store_water.t) option =
  Floors.find_stamped ~replica ~tab (Durable_guard.floors g)

(* Nothing exists under [parent] yet: not the guard directory, not the rpc
   subdirectory. This is a first boot on a step-14 root, or a fresh install. *)
let { Tea_server_pack.ws = ws1; ws_journal = ws_j1; rpc = rpc1; rpc_journal = rpc_j1 } =
  Tea_server_pack.open_guards ~guard_dir ~head_water ~identity

let () =
  check "the websocket channel opens a journal on a bare root"
    (Option.is_some ws_j1);
  (* The ORDER check. [Guard_file.open_] creates its own directory but never
     the parent, so opening the rpc channel first would have tried to mkdir
     <guard_dir>/rpc under a <guard_dir> that did not exist, failed ENOENT, and
     degraded this channel to a null sink - at-least-once delivery, silently,
     on every fresh install. *)
  check "and so does the rpc channel, whose PARENT directory did not exist either"
    (Option.is_some rpc_j1);
  check "the websocket journal is the .guard directory's own file"
    (Sys.file_exists (Filename.concat guard_dir "journal"));
  check "the rpc journal is a file UNDER .guard/rpc, so one restore keeps both"
    (Sys.file_exists (Filename.concat rpc_dir "journal"))

(* D20.4: the mirror is derived at the composition site from the channel's own
   cap, never a constant. The rpc channel's outer bound is 1 replica, so
   4 * sessions * tabs is tiny and its journal cap (16384) is what clears the
   bound; the websocket channel's 4 * 4096 * 8 dominates its own cap. A single
   shared bound cannot satisfy both lines. *)
let () =
  check "the rpc channel's mirror clears its own journal cap"
    (Bound.to_int (Durable_guard.mirror_bound rpc1) >= 16384);
  check "the websocket channel's mirror clears its own, much larger population"
    (Bound.to_int (Durable_guard.mirror_bound ws1) >= 4 * 4096 * 8);
  check "so the two channels were not handed one shared bound"
    (Bound.to_int (Durable_guard.mirror_bound ws1)
    <> Bound.to_int (Durable_guard.mirror_bound rpc1))

(* One floor per channel, under different tabs, through each channel's own
   guard. *)
let () =
  put "the rpc guard records its floor" rpc1 ~tab:rpc_tab ~seq:1 ~water:rpc_water;
  put "the websocket guard records its floor" ws1 ~tab:ws_tab ~seq:1 ~water:ws_water

(* In-memory positive control, and only that: the two guards are separate
   values with separate mirrors whatever file they write to, so these hold even
   when both journals are the same file. They are here to prove the writes
   happened at all, which is what the disk checks below need in order to mean
   anything. *)
let () =
  check "the rpc guard's mirror holds the floor it just recorded"
    (Option.is_some (stamped rpc1 rpc_tab));
  check "the websocket guard's mirror holds the floor it just recorded"
    (Option.is_some (stamped ws1 ws_tab))

let () =
  close_journal "the websocket journal closes" ws_j1;
  close_journal "the rpc journal closes" rpc_j1

(* The restart. Everything from here is read off the FILES. *)
let { Tea_server_pack.ws = ws2; ws_journal = ws_j2; rpc = rpc2; rpc_journal = rpc_j2 } =
  Tea_server_pack.open_guards ~guard_dir ~head_water ~identity

let stamped_is (g : Durable_guard.t) (tab : Tab_id.t) (seq : int)
    (water : Store_water.t) : bool =
  Option.fold ~none:false
    ~some:(fun ((s : Msg_seq.t), (w : Store_water.t)) ->
      Msg_seq.to_int s = seq && Store_water.equal w water)
    (stamped g tab)

let () =
  check "the reopened rpc channel adopts the floor it wrote, witness intact"
    (stamped_is rpc2 rpc_tab 1 rpc_water);
  check "the reopened websocket channel adopts the floor IT wrote, witness intact"
    (stamped_is ws2 ws_tab 1 ws_water);
  (* The separation itself. Each absence sits beside the presence above it, so
     neither can be satisfied by a ledger that simply came back empty. *)
  check "the rpc channel did not inherit the websocket channel's floor"
    (Option.is_none (stamped rpc2 ws_tab));
  check "the websocket channel did not inherit the rpc channel's floor"
    (Option.is_none (stamped ws2 rpc_tab));
  check "each ledger holds exactly the one floor its own channel wrote"
    (Floors.cardinal (Durable_guard.floors rpc2) = 1
    && Floors.cardinal (Durable_guard.floors ws2) = 1)

let () =
  close_journal "the reopened websocket journal closes" ws_j2;
  close_journal "the reopened rpc journal closes" rpc_j2

(* --- R20a: one journal's binding cannot speak for the other's ---------------
   [open_guards] is handed ONE binding, and that shape invites a single verdict
   at the composition site. It is not one verdict: each journal carries and
   checks its OWN header frame, so a header that drifts under one channel must
   clear THAT channel's ledger and leave the other whole. The drift is
   manufactured on the websocket side alone - a third store's token over the
   same floors - and both channels are then reopened under the SAME binding
   that wrote them, so the only thing that differs between the two verdicts is
   which file the header sits in.

   Stated as the presence/absence pair this file demands everywhere else, for
   the same reason: "the websocket ledger is empty" is satisfied just as well
   by a boot that lost both ledgers, so the emptiness is asserted beside the
   rpc floor that has to survive the very same boot. Point both channels at one
   file and the presence fails while the absence stays green. *)

let ws_journal_file = Filename.concat guard_dir "journal"
let rpc_journal_file = Filename.concat rpc_dir "journal"

(* The identity header is the journal's FIRST frame: [len:4][tag:1]
   [payload:32][crc:4], 41 bytes. Built through the PUBLIC [Codec.frame] and
   the public tag, so this fixture and the writer cannot drift apart without
   one of them changing the shared seam. *)
let header_len = 41

let header_of (id : Store_identity.t) : string =
  Guard_sink.Codec.frame ~tag:Guard_file.tag_identity
    ~payload:(Store_identity.to_string id)

let read_journal (path : string) : string =
  Lwt_main.run
    (Lwt.catch
       (fun () -> Lwt_io.with_file ~mode:Lwt_io.Input path Lwt_io.read)
       (fun (_ : exn) -> die (Printf.sprintf "the journal at %s reads" path)))

let rewrite_journal (path : string) (bytes : string) : unit =
  Lwt_main.run
    (Lwt.catch
       (fun () ->
         Lwt_io.with_file ~mode:Lwt_io.Output
           ~flags:[ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
           path
           (fun (out : Lwt_io.output_channel) -> Lwt_io.write out bytes))
       (fun (_ : exn) -> die (Printf.sprintf "the journal at %s rewrites" path)))

(* Everything after the header frame: the floors, untouched. Declined on a file
   too short to hold a header, so nothing here has to trust a length. *)
let floors_after_header (s : string) : string option =
  if String.length s >= header_len then
    Some (String.sub s header_len (String.length s - header_len)) (* @total-accessor *)
  else None

(* The 32 token characters a stamped journal carries at offset 5, after
   [len:4][tag:1]. *)
let stamped_token (s : string) : string option =
  if String.length s >= 37 then Some (String.sub s 5 32) (* @total-accessor *)
  else None

let token_is (path : string) (id : Store_identity.t) : bool =
  Option.fold (stamped_token (read_journal path)) ~none:false
    ~some:(String.equal (Store_identity.to_string id))

(* A THIRD store, met by neither boot above: what a journal restored from
   another deployment, or copied in beside a store it never belonged to, looks
   like to this open. *)
let other_id : Store_identity.t = Store_identity.of_draws (fun () -> 0x2c)

let () =
  check "both journals left the restart carrying the OWNING store's token"
    (token_is ws_journal_file store_id && token_is rpc_journal_file store_id)

(* The rpc journal's bytes as they stand before the drift. A [Matched] open
   rewrites nothing, so this is what has to come back byte for byte. *)
let rpc_raw_before = read_journal rpc_journal_file

(* ONLY the websocket header moves. Its floor records are carried across byte
   for byte, so what the boot below refuses is the BINDING and not a damaged
   record - the two would be indistinguishable if the floors were disturbed. *)
let () =
  Option.fold
    ~none:(fun () -> die "the websocket journal is long enough to hold a header")
    ~some:(fun (floors : string) () ->
      rewrite_journal ws_journal_file (header_of other_id ^ floors))
    (floors_after_header (read_journal ws_journal_file))
    ()

let () =
  check "the drift landed: the websocket header now names a store neither boot bound"
    (token_is ws_journal_file other_id);
  check "and the rpc journal was not touched by it"
    (String.equal (read_journal rpc_journal_file) rpc_raw_before)

(* The SAME binding both earlier boots used. *)
let { Tea_server_pack.ws = ws3; ws_journal = ws_j3; rpc = rpc3; rpc_journal = rpc_j3 } =
  Tea_server_pack.open_guards ~guard_dir ~head_water ~identity

let () =
  (* The presence. The rpc header still names this store, so that channel is
     Matched and its floor is still adjudicating replays. *)
  check "the rpc channel, whose header did not drift, KEEPS its floor, witness intact"
    (stamped_is rpc3 rpc_tab 1 rpc_water);
  check "and holds exactly that one, adopting nothing from next door"
    (Floors.cardinal (Durable_guard.floors rpc3) = 1);
  (* The absence, in the same breath. The websocket header names a stranger, so
     that channel is Rebound and its ledger is empty - and empty for THAT
     reason, because the rpc ledger beside it came through the same boot. *)
  check "the websocket channel, whose header drifted to a THIRD store, comes up empty"
    (Floors.cardinal (Durable_guard.floors ws3) = 0);
  check "so the websocket channel lost its OWN floor, and never held the rpc channel's"
    (Option.is_none (stamped ws3 ws_tab) && Option.is_none (stamped ws3 rpc_tab))

let () =
  close_journal "the rebound websocket journal closes" ws_j3;
  close_journal "the matched rpc journal closes" rpc_j3

(* Durable, not merely in-memory: Rebound restamps and compacts, Matched holds.
   Without this pair a boot that had simply failed to read the websocket file
   would look the same from the ledgers alone. *)
let () =
  check "the rebound websocket journal was restamped to the binding it opened under"
    (token_is ws_journal_file store_id);
  check "and nothing but that header survived the rebind"
    (String.length (read_journal ws_journal_file) = header_len);
  check "while the matched rpc journal was not rewritten by one byte"
    (String.equal (read_journal rpc_journal_file) rpc_raw_before)

let rec rm_rf (path : string) : unit =
  if Sys.is_directory path then (
    Array.iter (fun (entry : string) -> rm_rf (Filename.concat path entry)) (Sys.readdir path);
    Sys.rmdir path)
  else Sys.remove path

let () =
  rm_rf parent;
  if !failures > 0 then (
    Printf.printf "\n%d check(s) failed.\n%!" !failures;
    exit 1);
  Printf.printf
    "\n\
     The two delivery channels are composed into SEPARATE ledgers, in an order \
     that works on a bare root (D20.3), and the ONE binding they share is \
     checked per journal: a header that drifts under one channel clears that \
     channel alone (R20a, D23).\n\
     %!"
