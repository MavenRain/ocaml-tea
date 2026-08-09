(** The RPC channel's own journal, and the composition rule that sizes its
    mirror (roadmap step 15, D20.3 and D20.4, wiring increment I4c).

    {!Tea_server_pack.Make_pack.serve_pack} now opens TWO journals under one
    pack root: the websocket channel keeps [<root>.guard] byte for byte, and
    the keyed RPC channel gets [<root>.guard/rpc]. This file pins the three
    facts that wiring rests on, none of which the type checker carries:

    - the nested directory is only creatable in ONE order, so [serve_pack]'s
      open order is load-bearing rather than incidental;
    - the two journals are genuinely separate ledgers, so a floor taken on one
      channel is invisible to the other's boot fold (the alternative, one file
      split at boot, is exactly what D20.3 refused);
    - the mirror bound each channel is composed with clears both obligations
      the derivation exists to meet (T17), computed here from the same
      arguments the composition site passes rather than restated as a
      constant. *)

module Guard_file = Tea_server_pack.Guard_file
module Guard_sink = Tea_server.Guard_sink
module Durable_guard = Tea_server.Durable_guard
module Replay_guard = Tea_server.Replay_guard
module Floors = Tea_server.Durable_guard.Floors
module Bound = Tea_server.Replay_guard.Bound
module Msg_seq = Tea_core.Prim.Msg_seq
module Tab_id = Tea_core.Prim.Tab_id
module Store_identity = Tea_core.Prim.Store_identity

let ( let* ) = Lwt.bind

let failures = ref 0

let check (label : string) (ok : bool) : unit =
  if ok then Printf.printf "ok   - %s\n%!" label
  else (
    incr failures;
    Printf.printf "FAIL - %s\n%!" label)

(* [Option.fold]'s [~none:] is EAGER, so both arms are closures and the
   application is the only thing that chooses. *)
let must (what : string) (o : 'a option) : 'a =
  Option.fold
    ~none:(fun () ->
      Printf.printf "FAIL - test setup: %s\n%!" what;
      exit 1)
    ~some:(fun x () -> x)
    o ()

let replica (name : string) : Tea_core.Crdt.Replica.t =
  Tea_core.Crdt.Replica.v (Tea_core.Prim.Session_id.v name)

let tab (n : int) : Tab_id.t =
  must "tab id mint refused a valid seed"
    (Tab_id.of_bytes (List.init 16 (fun i -> (n + i) land 0xff)))

let seq (n : int) : Msg_seq.t =
  must (Printf.sprintf "Msg_seq.of_int refused %d" n) (Msg_seq.of_int n)

let advance (r : Tea_core.Crdt.Replica.t) (t : Tab_id.t) (n : int) : Guard_sink.event =
  Guard_sink.Advance
    { replica = r; tab = t; seq = seq n; water = Tea_core.Prim.Store_water.bottom }

(* The boot filter's null input: every floor here carries [bottom], a claim of
   nothing, so the filter adopts everything and this file stays a test of the
   two-ledger wiring rather than of the filter (guard_water_test's subject). *)
let no_heads : Tea_core.Crdt.Replica.t -> Tea_core.Prim.Store_water.t option =
  Guard_file.head_water_of_list []

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
    ~error:(fun (_ : Guard_file.open_err) ->
      Printf.printf "FAIL - test setup: %s\n%!" what;
      exit 1)

(* One store for both channels, the way the composition site reads it once and
   hands it to each. Every floor here still carries [bottom], so the identity
   arm changes nothing this file asserts. *)
let store_id : Store_identity.t = Store_identity.of_draws (fun () -> 0x3c)
let identity : Store_identity.binding = Store_identity.Bound store_id

let put (what : string) (sink : Guard_sink.t) (e : Guard_sink.event) : unit Lwt.t =
  let* r = sink.Guard_sink.append e in
  Result.fold r
    ~ok:(fun () -> Lwt.return_unit)
    ~error:(fun (_ : Guard_sink.err) ->
      Printf.printf "FAIL - test setup: append (%s) failed\n%!" what;
      exit 1)

let floor_at (fl : Floors.t) (r : Tea_core.Crdt.Replica.t) (t : Tab_id.t) ~(is : int) :
    bool =
  Floors.find ~replica:r ~tab:t fl
  |> Option.fold ~none:false ~some:(fun (n : Msg_seq.t) -> Int.equal (Msg_seq.to_int n) is)

let rec rm_rf (path : string) : unit =
  if Sys.is_directory path then (
    Array.iter (fun entry -> rm_rf (Filename.concat path entry)) (Sys.readdir path);
    Sys.rmdir path)
  else Sys.remove path

let in_scratch (f : string -> unit Lwt.t) : unit =
  let parent = Filename.temp_dir "ocaml-tea-rpc-journal" "" in
  Lwt_main.run (f (Filename.concat parent "store.guard"));
  rm_rf parent

let is_bad_dir (r : ('a, Guard_file.open_err) result) : bool =
  Result.fold r
    ~ok:(fun (_ : 'a) -> false)
    ~error:(fun (e : Guard_file.open_err) ->
      match e with
      | Guard_file.Bad_dir (_ : string) -> true
      | Guard_file.Io (_ : string) -> false)

(* --- 1. The nested directory, and why serve_pack's open order is binding --- *)

(* [Guard_file] creates ONE directory component ([ensure_dir] is a single
   [mkdir], EEXIST-tolerant). The RPC journal therefore cannot be opened before
   the websocket journal has made [.guard], which is exactly the order
   [serve_pack] uses. Pinning it here means a future reordering fails a check
   instead of degrading the RPC channel to a null sink on every boot, where the
   only symptom would be duplicates surviving a restart. *)
let () =
  in_scratch (fun guard_dir ->
      let rpc_dir = Filename.concat guard_dir "rpc" in
      let* early = Guard_file.open_ ~dir:rpc_dir ~cap:16 ~head_water:no_heads ~identity in
      check "the rpc journal cannot be opened before .guard exists"
        (is_bad_dir early);
      let* ws = Guard_file.open_ ~dir:guard_dir ~cap:16 ~head_water:no_heads ~identity in
      let ( (_ : Guard_sink.t)
          , (_ : Floors.t)
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , wsh ) =
        opened "websocket journal in a fresh parent" ws
      in
      let* late = Guard_file.open_ ~dir:rpc_dir ~cap:16 ~head_water:no_heads ~identity in
      check "the rpc journal opens once the websocket journal has made .guard"
        (Result.is_ok late);
      let ( (_ : Guard_sink.t)
          , (_ : Floors.t)
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , rpch ) =
        opened "rpc journal under an existing .guard" late
      in
      check "the two journals are distinct files on disk"
        (Sys.file_exists (Filename.concat guard_dir "journal")
        && Sys.file_exists (Filename.concat rpc_dir "journal"));
      let* () = Guard_file.close wsh in
      Guard_file.close rpch)

(* --- 2. Two ledgers, not one file split at boot (D20.3) -------------------- *)

(* The SAME replica and the SAME tab carry different floors on the two
   channels, and each boot fold sees only its own. A shared ledger would let a
   websocket delivery raise the RPC channel's floor for that tab, which is
   silent loss: the next keyed POST would read Duplicate against a delivery the
   RPC channel never took. *)
let () =
  in_scratch (fun guard_dir ->
      let rpc_dir = Filename.concat guard_dir "rpc" in
      let r = replica "canonical" in
      let t = tab 1 in
      let* ws = Guard_file.open_ ~dir:guard_dir ~cap:16 ~head_water:no_heads ~identity in
      let ( ws_sink
          , (_ : Floors.t)
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , wsh ) =
        opened "websocket journal" ws
      in
      let* rpc = Guard_file.open_ ~dir:rpc_dir ~cap:16 ~head_water:no_heads ~identity in
      let ( rpc_sink
          , (_ : Floors.t)
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , rpch ) =
        opened "rpc journal" rpc
      in
      let* () = put "ws floor 3" ws_sink (advance r t 3) in
      let* () = put "rpc floor 9" rpc_sink (advance r t 9) in
      let* () = Guard_file.close wsh in
      let* () = Guard_file.close rpch in
      let* ws2 = Guard_file.open_ ~dir:guard_dir ~cap:16 ~head_water:no_heads ~identity in
      let ( (_ : Guard_sink.t)
          , ws_floors
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , wsh2 ) =
        opened "websocket reopen" ws2
      in
      let* rpc2 = Guard_file.open_ ~dir:rpc_dir ~cap:16 ~head_water:no_heads ~identity in
      let ( (_ : Guard_sink.t)
          , rpc_floors
          , (_ : Guard_file.verdict)
          , (_ : Guard_file.identity_outcome)
          , rpch2 ) =
        opened "rpc reopen" rpc2
      in
      check "the websocket journal reopens holding only its OWN floor"
        (floor_at ws_floors r t ~is:3 && Int.equal (Floors.cardinal ws_floors) 1);
      check "the rpc journal reopens holding only its OWN floor"
        (floor_at rpc_floors r t ~is:9 && Int.equal (Floors.cardinal rpc_floors) 1);
      let* () = Guard_file.close wsh2 in
      Guard_file.close rpch2)

(* --- 3. T17: the composition-site mirror derivation ------------------------ *)

(* The wiring rule is [mirror >= max (journal cap) (4 * Cell capacity)]. It is
   pinned here rather than asserted at runtime (house law: no assert), and it
   is COMPUTED from the same arguments serve_pack passes, so a change to either
   default moves both sides together and a change to the derivation moves only
   one. *)
let rpc_cap = 16384
let ws_cap = 32768

let rpc_mirror =
  Durable_guard.default_mirror ~sessions:Replay_guard.default_rpc_replicas
    ~tabs:Replay_guard.default_rpc_tabs ~journal_cap:rpc_cap ()

let ws_mirror =
  Durable_guard.default_mirror ~sessions:Replay_guard.default_sessions
    ~tabs:Replay_guard.default_tabs ~journal_cap:ws_cap ()

let () =
  check "the rpc mirror out-remembers its Cell by the factor of four"
    (Bound.to_int rpc_mirror
    >= 4
       * Bound.to_int Replay_guard.default_rpc_replicas
       * Bound.to_int Replay_guard.default_rpc_tabs);
  check "the rpc mirror holds every floor its journal can replay at boot"
    (Bound.to_int rpc_mirror >= rpc_cap);
  check "the websocket mirror holds every floor its journal can replay at boot"
    (Bound.to_int ws_mirror >= ws_cap);
  (* The guard actually composed the way serve_pack composes it reports that
     same bound: the derivation is not merely computable, it is what the
     instance carries. *)
  let composed =
    Durable_guard.v ~sessions:Replay_guard.default_rpc_replicas
      ~tabs:Replay_guard.default_rpc_tabs ~mirror:rpc_mirror ~sink:Guard_sink.null
      ~floors:Durable_guard.Floors.empty ()
  in
  check "a guard composed as serve_pack composes it carries the derived bound"
    (Int.equal (Bound.to_int (Durable_guard.mirror_bound composed))
       (Bound.to_int rpc_mirror));
  (* And the mem tier's value, which passes no cap at all, is the sinkless
     derivation: still bounded, still out-remembering its Cell, with no journal
     to clear. Nothing here may silently fall back to the websocket channel's
     population, whose per-replica tab bound is eight. *)
  let mem = Tea_server.Rpc_once.mem ~floor_replica:(replica "canonical") in
  check "the mem tier's rpc guard is bounded by the sinkless derivation"
    (Int.equal
       (Bound.to_int (Durable_guard.mirror_bound mem.Tea_server.Rpc_once.guard))
       (Bound.to_int
          (Durable_guard.default_mirror ~sessions:Replay_guard.default_rpc_replicas
             ~tabs:Replay_guard.default_rpc_tabs ())))

let () =
  if !failures > 0 then (
    Printf.printf "rpc_journal_test: %d check(s) FAILED\n%!" !failures;
    exit 1)
  else Printf.printf "rpc_journal_test: all checks passed\n%!"
