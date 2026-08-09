(** The keyed RPC channel across a RESTART, on the tier where that sentence can
    be true (roadmap step 15, D20, wiring increment I6).

    [rpc_once_test] drives the same guarantee over the mem tier, and the mem
    tier can only ever state the half that lives inside one process: its floors
    and its document die together, so a retry that crosses a boot re-applies and
    the honest claim is at-least-once. Everything step 15 adds beyond that is a
    claim about what a restart preserves, and this file is where it is read.

    Two lives over ONE pack root and ONE rpc journal, composed exactly the way
    {!Tea_server_pack.Make_pack.serve_pack} composes them - the same journal
    directory, the same cap, the same [Replay_guard] bounds, the same
    branch-water boot filter, the same durable session secret beside the root -
    and driven through {!Shared_doc_serve.Pack.rpc_once}, the very builder the
    binary hands to [serve_pack]. A second hand-written wiring here would assert
    about a pipeline nothing serves.

    {b The two preconditions that would make it vacuous, both checked rather
    than assumed.}

    - The de-duplication namespace is derived from the session cookie (D20.1),
      so life 2 must decrypt life 1's cookie or the replay lands in a different
      namespace and reads [Fresh] however correct the guard is. That is why both
      lives resolve the secret from the same [<root>.secret] file, and why the
      cookie minted in life 1 is threaded through the restart by hand.
    - A floor whose store witness cannot be matched against this boot's branch
      heads is DROPPED by the boot filter, and a dropped floor also reads
      [Fresh]. So the reopened journal's verdict is asserted directly: the floor
      is KEPT, with nothing dropped behind and nothing dropped for a missing
      branch. Without that assertion a "Replayed" could not be told from a
      guard that had simply forgotten nothing because it had kept nothing.

    The anti-vacuity partner of the replay is the last scenario: a FRESH key
    after the restart must still apply and still raise the commit count. Without
    it, a server that answered [Replayed] to everything - or one whose store
    came back empty - would pass every other check in the file. *)

module App = Shared_doc_app.App
module Pack = Shared_doc_serve.Pack
module Store = Pack.Server.Store
module Rpc = Shared_doc_serve.Rpc
module R = Tea_rpc.Make (Shared_doc_rpc)
module Guard_file = Tea_server_pack.Guard_file
module Guard_sink = Tea_server.Guard_sink
module Durable_guard = Tea_server.Durable_guard
module Replay_guard = Tea_server.Replay_guard
module Floors = Durable_guard.Floors
module Tab_id = Tea_core.Prim.Tab_id
module Msg_seq = Tea_core.Prim.Msg_seq
module Store_water = Tea_core.Prim.Store_water
module Store_identity = Tea_core.Prim.Store_identity

let failures = ref 0

let check (label : string) (ok : bool) : unit =
  if ok then Printf.printf "ok   - %s\n%!" label
  else (
    incr failures;
    Printf.printf "FAIL - %s\n%!" label)

(* A setup step that cannot be recovered from leaves no test to run, so it stops
   the file rather than letting every later verdict describe a world that was
   never built. Distinct from [check], which records and continues. *)
let die (what : string) : 'a =
  Printf.printf "FAIL - %s\n%!" what;
  exit 1

(* The pack root and its two durability siblings, named the way [serve_pack]
   names them: the guard directory beside the root, the rpc channel's journal
   one level inside it (D20.3), the secret file beside both. *)
let parent = Filename.temp_dir "ocaml-tea-rpc-pack-once" ""
let root = Tea_server_pack.Root.v (Filename.concat parent "store")
let guard_dir = Filename.concat parent "store.guard"
let rpc_dir = Filename.concat guard_dir "rpc"
let secret_file = Filename.concat parent "store.secret"

(* [serve_pack]'s rpc-channel cap. Carried here rather than rounded off because
   it is also the [journal_cap] the mirror bound is derived from (D20.4), and a
   test that sized the mirror differently would not be exercising the shipped
   composition. *)
let journal_cap = 16384

(* One store, one identity, every life: the binding [serve_pack] will read once
   from the root and hand to both channels. Fixed here rather than re-minted per
   life, so the restart below meets its OWN journals rather than a stranger's. *)
let store_id : Store_identity.t = Store_identity.of_draws (fun () -> 0x7e)
let identity : Store_identity.binding = Store_identity.Bound store_id

type life =
  { driver : Dream.request -> Dream.response
  ; repo : Store.t
  ; journal : Guard_file.t
  ; ws_journal : Guard_file.t
  ; verdict : Guard_file.verdict
  ; floors : Floors.t
        (** The floor set the boot filter handed this life, read BEFORE any
            traffic. [verdict] counts what survived; this is what survived. *)
  ; sink : Guard_sink.t
        (** The very sink the guard appends through. Held so T6 can write a
            floor the store cannot witness without opening a second handle to
            the same journal file behind the framework's back. *)
  ; written : Guard_sink.event list ref
        (** Newest first. What the guard actually recorded this life. *)
  }

(* One server life over the shared root: open the store, read the branch waters
   ONCE, fold BOTH journals against them, and mount this app's contract on the
   channel the rpc fold produced. The order is [serve_pack]'s order, and the
   websocket journal is opened here for a reason that is not symmetry: its
   directory is the rpc journal's PARENT, so opening the rpc channel first fails
   with ENOENT (rpc_journal_test pins exactly that). A test that reached for the
   inner journal alone would be composing a wiring [serve_pack] cannot produce -
   and would have to mkdir behind the framework's back to run at all.

   [?binding] is the store token this life hands to both journals. It defaults
   to the one binding above, which is the shipped shape - [serve_pack] reads it
   once from the root and every life over that root sees the same value. The
   parameter exists for the last scenario only, where a life meets the journals
   under a token that is NOT theirs (R20a), and it is a parameter rather than a
   second boot function so that the mismatched life is otherwise the same
   wiring, opened the same way, by the same code. *)
let boot ?(binding : Store_identity.binding = identity) () : life =
  let repo = Lwt_main.run (Store.create root) in
  let head_water =
    Lwt_main.run (Lwt.map Guard_file.head_water_of_list (Store.branch_waters repo))
  in
  let open_journal ~(what : string) ~(dir : string) ~(cap : int) :
      Guard_sink.t
      * Durable_guard.Floors.t
      * Guard_file.verdict
      * Guard_file.identity_outcome
      * Guard_file.t =
    Lwt_main.run (Guard_file.open_ ~dir ~cap ~head_water ~identity:binding)
    |> Result.fold
         ~ok:(fun
             ((s, f, v, o, j) :
               Guard_sink.t
               * Durable_guard.Floors.t
               * Guard_file.verdict
               * Guard_file.identity_outcome
               * Guard_file.t)
           -> (s, f, v, o, j))
         ~error:(fun (e : Guard_file.open_err) ->
           die
             (match e with
             | Guard_file.Bad_dir (d : string) ->
               Printf.sprintf "the %s journal opens (unusable directory: %s)" what d
             | Guard_file.Io (r : string) ->
               Printf.sprintf "the %s journal opens (io: %s)" what r))
  in
  let ( (_ : Guard_sink.t)
      , (_ : Durable_guard.Floors.t)
      , (_ : Guard_file.verdict)
      , (_ : Guard_file.identity_outcome)
      , ws_journal ) =
    open_journal ~what:"websocket" ~dir:guard_dir ~cap:32768
  in
  let file_sink, floors, verdict, (_ : Guard_file.identity_outcome), journal =
    open_journal ~what:"rpc" ~dir:rpc_dir ~cap:journal_cap
  in
  (* Every record the guard appends, in write order. A recorder rather than a
     re-derivation: the floor's tab is a session-scoped digest computed inside
     the server (D20.1), so a test that recomputed it would be asserting against
     its own copy of the namespace rule - and one that guessed the raw header
     value instead would look up a floor that is never there, and would then be
     free to "pass" every question it asked about it. Tee'd rather than
     substituted, so the journal on disk is still the one the shipped writer
     wrote and the restart below still reads that file. *)
  let written : Guard_sink.event list ref = ref [] in
  let sink : Guard_sink.t =
    { Guard_sink.append =
        (fun (ev : Guard_sink.event) ->
          written := ev :: !written;
          file_sink.Guard_sink.append ev)
    }
  in
  (* The durable identity. Not a detail: the namespace this channel dedupes in
     is the session's, so an identity that died with life 1 would take the
     guarantee with it (D17's "identity durability must never exceed model
     durability", read in the other direction). *)
  let identity =
    Tea_server.Session_secret.resolve ~file:secret_file ()
    |> Result.fold
         ~ok:(fun (s : Tea_server.Session_secret.t) -> s)
         ~error:(fun (_ : Tea_server.Session_secret.err) ->
           die "the durable session secret resolves from <root>.secret")
  in
  let sessions = Replay_guard.default_rpc_replicas in
  let tabs = Replay_guard.default_rpc_tabs in
  let guard =
    Durable_guard.v ~sessions ~tabs
      ~mirror:(Durable_guard.default_mirror ~sessions ~tabs ~journal_cap ())
      ~sink ~floors ()
  in
  let once = Tea_server.Rpc_once.v ~guard ~floor_replica:Store.main_replica in
  { driver =
      Dream.test
        (Pack.Server.handler_pack ~rpc:(Pack.rpc_once repo once) ~sessions:identity repo)
  ; repo
  ; journal
  ; ws_journal
  ; verdict
  ; floors
  ; sink
  ; written
  }

(* The REPO first, then the journal: [serve_pack]'s teardown order, and the
   reason it is that way round is the property this file exists to read. Closing
   the journal first would leave floors durable over commits still in the pack's
   buffer, and the replay below would then be answered [Replayed] against a
   commit that never reached disk - silent loss dressed as the guarantee. *)
let shutdown (l : life) : unit =
  Lwt_main.run (Store.close l.repo);
  Lwt_main.run (Guard_file.close l.journal);
  Lwt_main.run (Guard_file.close l.ws_journal)

let body (response : Dream.response) : string = Lwt_main.run (Dream.body response)

let cookie_of (response : Dream.response) : string =
  Dream.header response "Set-Cookie"
  |> Option.fold ~none:"" ~some:(fun (c : string) ->
         match String.split_on_char ';' c with
         | [] -> "" (* unreachable: split_on_char never returns [] *)
         | first :: (_ : string list) -> first)

let append (l : life) ?(key : string option) ~(cookie : string) (tag : string) :
    Dream.response =
  let optional (name : string) (v : string option) : (string * string) list =
    Option.fold v ~none:[] ~some:(fun (value : string) -> [ (name, value) ])
  in
  let headers =
    [ ("Content-Type", "application/json")
    ; ("Origin", "http://example.com")
    ; ("Host", "example.com")
    ]
    @ optional Tea_rpc.key_header key
    @ optional "Cookie" (if String.equal cookie "" then None else Some cookie)
  in
  l.driver
    (Dream.request ~method_:`POST ~target:"/rpc/append_tag" ~headers
       (R.encode_req Shared_doc_rpc.Append_tag tag))

type answer =
  | Reply of int
  | Replayed
  | Undecodable

let answer_of (response : Dream.response) : answer =
  Tea_core.Codec.of_json
    (Tea_rpc.keyed_resp_t (Shared_doc_rpc.resp_t Shared_doc_rpc.Append_tag))
    (body response)
  |> Result.fold
       ~error:(fun (_ : Tea_core.Codec.err) -> Undecodable)
       ~ok:(fun (k : int Tea_rpc.keyed_resp) ->
         match k with
         | Tea_rpc.Reply (n : int) -> Reply n
         | Tea_rpc.Replayed -> Replayed)

let is_reply (a : answer) : bool =
  match a with
  | Reply (_ : int) -> true
  | Replayed | Undecodable -> false

let is_replayed (a : answer) : bool =
  match a with
  | Replayed -> true
  | Reply (_ : int) | Undecodable -> false

(* The canonical branch as the handler writes it: the only honest witness that a
   de-duplicated delivery changed nothing. Commit COUNT rather than tag set,
   because the tag set is an Or_set - adding one tag twice yields one element
   whether or not the guard suppressed the second, so the set alone cannot tell
   the guaranteed world from the broken one. The count can. *)
let commits (l : life) : int =
  Lwt_main.run
    (Lwt.bind (Store.main_session l.repo) (fun s ->
         Lwt.map (fun (h : Tea_core.Prim.Commit_ref.t list) -> List.length h)
           (Store.history s)))

let tags (l : life) : string list =
  Lwt_main.run
    (Lwt.bind (Store.main_session l.repo) (fun s -> Lwt.map App.tags_of (Store.load s)))

let tab = "b7c6d5e4f3a2918070615243342516f0"
let key (seq : int) : string = tab ^ ":" ^ string_of_int seq

let seq_of (n : int) : Msg_seq.t =
  Option.fold
    ~none:(fun () -> die (Printf.sprintf "Msg_seq.of_int accepts %d" n))
    ~some:(fun (s : Msg_seq.t) () -> s)
    (Msg_seq.of_int n) ()

(* The floor records this life appended, oldest first. [Forget] carries no
   sequence, so it is not a floor claim and drops out here. *)
type record =
  { rtab : Tab_id.t
  ; rseq : Msg_seq.t
  ; rwater : Store_water.t
  }

let records (l : life) : record list =
  List.rev !(l.written)
  |> List.filter_map (fun (ev : Guard_sink.event) ->
         match ev with
         | Guard_sink.Advance
             { replica = (_ : Tea_core.Crdt.Replica.t); tab; seq; water } ->
           Some { rtab = tab; rseq = seq; rwater = water }
         | Guard_sink.Forget { replica = (_ : Tea_core.Crdt.Replica.t) } -> None)

let sole_record (what : string) (l : life) : record =
  match records l with
  | [ (one : record) ] -> one
  | [] -> die (Printf.sprintf "%s wrote a floor record" what)
  | (_ : record) :: (_ : record) :: (_ : record list) ->
    die (Printf.sprintf "%s wrote exactly ONE floor record" what)

let last_record (what : string) (l : life) : record =
  match List.rev (records l) with
  | (newest : record) :: (_ : record list) -> newest
  | [] -> die (Printf.sprintf "%s wrote a floor record" what)

let put (what : string) (s : Guard_sink.t) (ev : Guard_sink.event) : unit =
  Lwt_main.run (s.Guard_sink.append ev)
  |> Result.fold ~ok:(fun () -> ()) ~error:(fun (_ : Guard_sink.err) -> die what)

(* The branch heads as THIS life's store reports them right now, which is the
   same lookup the boot filter is built from. Read live rather than cached at
   boot, because every keyed delivery moves main's head and a stale copy would
   make the comparisons below agree for the wrong reason. *)
let head_water (l : life) : Tea_core.Crdt.Replica.t -> Store_water.t option =
  Lwt_main.run (Lwt.map Guard_file.head_water_of_list (Store.branch_waters l.repo))

let main_head (l : life) : Store_water.t =
  (* Thunked on both sides: [Option.fold]'s [~none:] is EAGER, so a bare [die]
     there would stop the file the first time this is merely called. *)
  Option.fold
    ~none:(fun () -> die "the canonical branch has a head water")
    ~some:(fun (w : Store_water.t) () -> w)
    (head_water l Store.main_replica)
    ()

(* --- Life 1: the delivery whose response is about to be lost -------------- *)

let life1 = boot ()

let cookie =
  let warm = append life1 ~cookie:"" "warm-up" in
  let c = cookie_of warm in
  check "life 1 issues a session cookie to thread through the restart"
    (not (String.equal c ""));
  c

let commits_before = commits life1
let first = answer_of (append life1 ~key:(key 1) ~cookie "durable")
let commits_after_first = commits life1

let () =
  check "a fresh keyed delivery is answered with a reply" (is_reply first);
  check "and it really committed (the canonical branch grew by one)"
    (commits_after_first = commits_before + 1);
  check "the tag it carried is on the branch" (List.mem "durable" (tags life1))

(* --- The restart ---------------------------------------------------------- *)

let () = shutdown life1
let life2 = boot ()

let () =
  let { Guard_file.kept; dropped_behind; dropped_no_branch; unwitnessed } =
    life2.verdict
  in
  (* Read before any traffic: this is the state the restart handed over, and
     every verdict below is only worth something if the floor is in it. *)
  check "the restart adopts the keyed floor from the journal" (kept >= 1);
  check "with its store witness matched against this boot's branch head"
    (dropped_behind = 0 && dropped_no_branch = 0 && unwitnessed = 0);
  check "the reopened branch still holds the committed tag"
    (List.mem "durable" (tags life2));
  check "and still holds its commit" (commits life2 = commits_after_first)

(* --- T5: the floor's water is the canonical commit's own mint -------------- *)

(* The single-branch contract (section 2.7). A floor carries a store witness so
   the boot filter can ask "does the store still have the effect this floor is
   standing over?", and that question is only answerable if the witness is the
   commit's OWN water rather than a number the guard minted on a clock of its
   own. Read here, before life 2 takes any traffic, because that is the one
   moment at which main's head is still exactly the commit life 1 wrote.

   The bottom check is the anti-vacuity partner: [bottom] is what a no-op take
   or a pre-step-13 record leaves, and two bottoms compare equal, so without it
   a guard that had recorded no witness at all would pass the line above it. *)
let life1_floor = sole_record "life 1's keyed delivery" life1

let () =
  check "life 1 recorded the sequence it consumed, and only that one"
    (Msg_seq.to_int life1_floor.rseq = 1);
  check "the floor's witness is not the bottom claim of nothing"
    (not (Store_water.equal life1_floor.rwater Store_water.bottom));
  check "and the witness IS the canonical commit's own water, not a second clock"
    (Store_water.equal life1_floor.rwater (main_head life2));
  let stamped =
    Floors.find_stamped ~replica:Store.main_replica ~tab:life1_floor.rtab life2.floors
  in
  let stamped_is (f : Msg_seq.t * Store_water.t -> bool) : bool =
    Option.fold ~none:false ~some:f stamped
  in
  check "the restart hands that floor back out of the journal, witness intact"
    (stamped_is (fun ((s : Msg_seq.t), (w : Store_water.t)) ->
         Msg_seq.to_int s = 1 && Store_water.equal w life1_floor.rwater));
  check "so this boot's head covers the floor it just adopted"
    (stamped_is (fun ((_ : Msg_seq.t), (w : Store_water.t)) ->
         Store_water.covers ~head:(main_head life2) ~floor:w));
  check "and that is the whole of what the restart adopted"
    (Floors.cardinal life2.floors = 1)

(* --- The retry that crosses the boot -------------------------------------- *)

let replay = answer_of (append life2 ~key:(key 1) ~cookie "durable")
let commits_after_replay = commits life2

let () =
  (* [Replayed] rather than the reply bytes, and that is not a weaker answer but
     the only truthful one: the reply cache is in-process (G12), so life 2 knows
     the delivery was applied and cannot know what it answered. The client turns
     this into {!Tea_rpc.Applied_reply_lost}, the marker an app surfaces to say
     "your write landed, its receipt did not" - which is what the browser
     harness's B10 reads off the page. *)
  check "the retry that crosses the restart is refused as a replay"
    (is_replayed replay);
  check "and it applied nothing: the commit count is unmoved"
    (commits_after_replay = commits_after_first);
  check "the tag is still on the branch exactly once"
    (List.length (List.filter (String.equal "durable") (tags life2)) = 1)

(* --- Anti-vacuity: the channel still works after the restart -------------- *)

let fresh = answer_of (append life2 ~key:(key 2) ~cookie "after-restart")

let () =
  check "a FRESH key after the restart is still applied" (is_reply fresh);
  check "and it commits (so the replay above was de-duplication, not a dead channel)"
    (commits life2 = commits_after_first + 1);
  check "its tag reaches the branch" (List.mem "after-restart" (tags life2))

(* --- T6: a rollback drops the witnessed floor and the retry re-applies ----- *)

(* R20, the direction the whole witness mechanism exists to choose. If the store
   comes back WITHOUT the effect a floor stands over - a restore from an older
   snapshot, a pack rolled back under the journal - then honouring that floor
   would answer the retry [Duplicate] for a write the store does not hold:
   silent loss. Refusing it re-admits the message as [Fresh], which is a visible
   duplicate, and a visible duplicate is the sanctioned failure.

   The rollback is staged from the floor side rather than by mutilating the pack,
   because the two are the same relation seen from opposite ends: a floor above
   main's head is exactly what a store rolled back beneath its floor looks like
   to the filter, and it is the only half a test can build without reaching
   inside the store's file format. The record goes in through the guard's OWN
   sink, so what is read back at the next boot is a journal the shipped writer
   wrote. *)

(* A second tab, so the filter has something to KEEP. Without it the rollback
   would empty the floor set, and a boot that had simply lost the journal would
   pass every check below: everything would re-apply, which is exactly what the
   R20 direction looks like from one tab's point of view. *)

let other_tab = "d4c3b2a1908f7e6d5c4b3a2918070615"

let () =
  check "a second tab's keyed delivery applies, giving the filter a floor to keep"
    (is_reply (answer_of (append life2 ~key:(other_tab ^ ":1") ~cookie "other-tab")))

let other_floor = last_record "the second tab's delivery" life2

let () =
  check "the two tabs really landed in DIFFERENT floors, not one shared namespace"
    (Tab_id.compare other_floor.rtab life1_floor.rtab <> 0)

(* The rollback itself: raise the FIRST tab's floor witness above anything this
   store can hold, through the guard's own sink and under the tab the server
   itself derived. Same seq, so [Floors.apply]'s last-writer-wins adopts the new
   witness rather than the floor being re-opened by a lower number. *)
let () =
  let above (w : Store_water.t) : Store_water.t =
    Store_water.of_date (Int64.add (Store_water.to_int64 w) 1L)
  in
  put "the guard's own sink accepts the rolled-back floor" life2.sink
    (Guard_sink.Advance
       { replica = Store.main_replica
       ; tab = life1_floor.rtab
       ; seq = seq_of 2
       ; water = above (main_head life2)
       })

let () = shutdown life2
let life3 = boot ()

let () =
  let { Guard_file.kept; dropped_behind; dropped_no_branch; unwitnessed } = life3.verdict in
  check "the boot filter drops the floor this store cannot witness" (dropped_behind = 1);
  check "and drops ONLY that one: the witnessed floor is still adopted"
    (kept = 1 && dropped_no_branch = 0 && unwitnessed = 0);
  check "the rolled-back floor is gone from the adopted set, not merely counted"
    (Option.is_none
       (Floors.find_stamped ~replica:Store.main_replica ~tab:life1_floor.rtab life3.floors));
  check "while the second tab's floor came through beside it"
    (Option.is_some
       (Floors.find_stamped ~replica:Store.main_replica ~tab:other_floor.rtab life3.floors))

let commits_before_rollback = commits life3
let rollback_retry = answer_of (append life3 ~key:(key 2) ~cookie "after-restart")
let commits_after_rollback = commits life3

let () =
  check "the retry over the dropped floor RE-APPLIES: a visible duplicate, not silent loss"
    (is_reply rollback_retry);
  check "and it really committed (the re-application reached the branch)"
    (commits_after_rollback = commits_before_rollback + 1);
  (* The pair that makes this the R20 choice rather than a broken guard: the
     identically-shaped cross-boot retry earlier in this file, whose floor the
     store COULD witness, was refused as a replay. Same channel, same restart,
     same key shape - only the witness differs. *)
  check "the tab whose floor survived is still refused its replay"
    (is_replayed (answer_of (append life3 ~key:(other_tab ^ ":1") ~cookie "other-tab")));
  check "and that refusal applied nothing"
    (commits life3 = commits_before_rollback + 1);
  (* Re-armed: the drop re-admitted the message once, it did not disarm the tab.
     In-process this is the reply cache answering, so the count is the witness. *)
  check "a second delivery of the re-applied key does not apply AGAIN"
    (commits
       (let (_ : answer) = answer_of (append life3 ~key:(key 2) ~cookie "after-restart") in
        life3)
    = commits_before_rollback + 1)

(* --- R20a: a mismatched binding re-admits a REAL take --------------------- *)

(* The seam itself, and the only check in the suite that watches a client
   message cross it. Every other identity fixture drives journals whose header
   and floors are bytes a test wrote, which proves the FILTER and says nothing
   about whether the shipped channel is wired to it: a [serve_pack] that
   resolved the token and then never handed it to [Guard_file.open_] passes all
   of them. Here the floor is recorded by a real keyed [Rpc_once] take through
   the real sink, the process goes away, the journals are reopened under a
   token that is NOT theirs, and the SAME keyed call is delivered again.

   The direction is R20a's, and it is the R20 direction read through the token
   instead of the water. A journal whose header names another store is a
   journal this store cannot vouch for, so honouring its floors would answer
   [Duplicate] for writes this store may never have made: silent loss. Clearing
   them re-admits the message as [Fresh], which is a visible duplicate, and a
   visible duplicate is the sanctioned failure.

   Three things make the re-admission attributable to the token and to nothing
   else, and all three are asserted rather than assumed: the STORE is untouched
   across the boot (same commit count, same tag), so the floor is gone and its
   effect is not; the de-duplication NAMESPACE is unchanged (the re-take lands
   under the very tab the first take derived), so this is not a keyed call that
   simply missed its floor; and the channel is still de-duplicating afterwards,
   so it is not answering [Fresh] to everything. *)

let seam_tab = "1122334455667788990aabbccddeeff0"
let seam_key = seam_tab ^ ":1"
let commits_before_seam = commits life3
let seam_take = answer_of (append life3 ~key:seam_key ~cookie "seam")
let commits_after_seam = commits life3

let () =
  check "a real keyed take on a third tab applies before the mismatched boot"
    (is_reply seam_take);
  check "and it really committed (the take reached the branch)"
    (commits_after_seam = commits_before_seam + 1)

(* Read off the guard's own recorder: this is the floor the shipped writer put
   in the shipped journal, not one the test composed. *)
let seam_floor = last_record "the seam take" life3

let () =
  check "the take recorded its floor through the real sink, at the sequence it consumed"
    (Msg_seq.to_int seam_floor.rseq = 1);
  check "with a store witness, not the bottom claim of nothing"
    (not (Store_water.equal seam_floor.rwater Store_water.bottom));
  check "in its OWN tab namespace, distinct from the tabs already in the journal"
    (Tab_id.compare seam_floor.rtab life1_floor.rtab <> 0
    && Tab_id.compare seam_floor.rtab other_floor.rtab <> 0)

(* The process goes away, and the journals come back under a token neither of
   them was ever stamped with. A THIRD store, met by no life above: what a
   journal restored beside a store it never belonged to looks like to this
   boot. *)
let () = shutdown life3
let stranger_id : Store_identity.t = Store_identity.of_draws (fun () -> 0x3d)
let life4 = boot ~binding:(Store_identity.Bound stranger_id) ()

let () =
  let { Guard_file.kept; dropped_behind; dropped_no_branch; unwitnessed } = life4.verdict in
  check "the mismatched boot adopts NO floor from the journal"
    (kept = 0 && Floors.cardinal life4.floors = 0);
  (* Cleared by the token, before the water filter ever ran: a drop counted
     here would be the R20 path, which is a different refusal with a different
     cause. *)
  check "and it cleared them on the TOKEN, not by dropping them on their witnesses"
    (dropped_behind = 0 && dropped_no_branch = 0 && unwitnessed = 0);
  check "the seam floor in particular is gone"
    (Option.is_none
       (Floors.find_stamped ~replica:Store.main_replica ~tab:seam_floor.rtab life4.floors));
  (* The paired presence. The floor died, the WRITE it stood over did not, so
     an honoured floor here would have answered [Duplicate] over a store that
     still holds the effect and would still have been the wrong answer. *)
  check "while the store came back whole: the take's commit is still on the branch"
    (commits life4 = commits_after_seam);
  check "and its tag with it" (List.mem "seam" (tags life4))

let commits_before_retake = commits life4
let retake = answer_of (append life4 ~key:seam_key ~cookie "seam")

let () =
  check "the SAME keyed call is TAKEN AGAIN across the mismatched binding, not answered from the stale floor"
    (is_reply retake);
  check "and it applied a second time: a visible duplicate, with a second commit"
    (commits life4 = commits_before_retake + 1);
  (* The namespace check, and the reason this is a positive control rather than
     an accident. The de-duplication tab is derived from the session cookie
     (D20.1), so a life that had failed to decrypt life 3's cookie would land
     in a different namespace and read [Fresh] however well the guard worked.
     The re-take lands under the very tab the first take derived, so the floor
     that was cleared IS the floor this delivery would have been refused by. *)
  check "the re-take landed in the SAME namespace the first take did, so it really met that floor's absence"
    (Tab_id.compare (last_record "the re-take" life4).rtab seam_floor.rtab = 0);
  (* Re-armed: the mismatch re-admitted the message once, it did not disarm the
     channel. In-process this is the reply cache answering, so the count is the
     witness. *)
  check "a second delivery of the re-taken key does not apply AGAIN"
    (commits
       (let (_ : answer) = answer_of (append life4 ~key:seam_key ~cookie "seam") in
        life4)
    = commits_before_retake + 1)

(* --- Teardown ------------------------------------------------------------- *)

let () =
  shutdown life4;
  let rec rm_rf (path : string) : unit =
    if Sys.is_directory path then (
      Array.iter (fun entry -> rm_rf (Filename.concat path entry)) (Sys.readdir path);
      Sys.rmdir path)
    else Sys.remove path
  in
  rm_rf parent;
  if !failures = 0 then
    Printf.printf
      "\n\
       The keyed RPC channel is exactly-once ACROSS a restart: the floor rides the \
       journal, the retry is refused, a fresh key still applies (D20), and a \
       journal reopened under another store's token re-admits the take rather \
       than answering it from a floor this store cannot vouch for (R20a, D23).\n\
       %!"
  else Printf.printf "\n%d check(s) failed.\n%!" !failures;
  exit (if !failures = 0 then 0 else 1)
