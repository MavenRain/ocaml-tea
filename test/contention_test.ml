(** The witnessed write path under contention (roadmap step 14, D19).

    Every check here is about one window: the gap between a writer's read and
    its commit. Before D19 the store closed that gap by re-reading the head and
    testing against {i that} read, so the test passed while the stale model
    landed on the racer: R10's "preserves history, not content". The fix is a
    token: {!Store_core.CORE.load_based} mints the head it observed together
    with the model read through that very commit, and
    {!Store_core.CORE.commit_based} tests against it, reconciling on denial
    instead of refusing (a refusal would travel to a WS pump that has already
    taken the message's sequence number, which is the D16 silent loss).

    The interleave is PROGRAM ORDER throughout: A reads, B reads-edits-commits,
    then A commits the model it witnessed. Nothing here sleeps to arrange a
    race and nothing assumes a scheduler, because a check whose red depends on
    which promise the runtime happens to pick is not a check. The one schedule
    program order cannot express on its own is C7's two-round contention, and
    that one is landed through the only synchronous seam inside the reconcile
    loop, the app's own merge function, rather than through Lwt concurrency;
    see [mid_merge].

    Filesystem work (the two pack checks) goes through the [fs] boundary
    {!Guard_water_test} established, so a backend raise becomes a loud FAIL
    line rather than a bare non-zero exit the mutation driver cannot classify. *)

module Prim = Tea_core.Prim
module Crdt = Tea_core.Crdt
module Water = Prim.Store_water
module Msg_seq = Prim.Msg_seq
module Tab_id = Prim.Tab_id
module Guard = Tea_server.Replay_guard
module Dguard = Tea_server.Durable_guard
module Floors = Tea_server.Durable_guard.Floors
module Guard_sink = Tea_server.Guard_sink
open Lwt.Syntax

(* --- Harness ----------------------------------------------------------------
   Plain [Printf] checks, one [ok   - <label>] line each, and exactly one
   [Lwt.catch] boundary ([fs]) where a filesystem or backend failure becomes a
   setup FAIL: errors stay values, and no helper below can raise into a check. *)

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

let fs (what : string) (f : unit -> 'a Lwt.t) : 'a Lwt.t =
  Lwt.catch f (fun (exc : exn) ->
      Printf.printf "FAIL - test setup: %s (%s)\n%!" what (Printexc.to_string exc);
      exit 1)

(* [Option.fold]'s [~none:] is EAGER; both arms are closures so the refusal
   cannot run on the happy path. *)
let must (what : string) (o : 'a option) : 'a =
  Option.fold
    ~none:(fun () ->
      Printf.printf "FAIL - test setup: %s\n%!" what;
      exit 1)
    ~some:(fun x () -> x)
    o ()

let sid (name : string) : Prim.Session_id.t =
  must (Printf.sprintf "Session_id.of_string refused %S" name)
    (Prim.Session_id.of_string name)

let branch_of (name : string) : string =
  Prim.Branch_name.(to_string (of_session (sid name)))

let seq (n : int) : Msg_seq.t =
  must (Printf.sprintf "Msg_seq.of_int refused %d" n) (Msg_seq.of_int n)

(* The wire tab id is a literal of the real grammar (32 hex chars), so this
   file needs no browser entropy source, and the SAME literal is what the floor
   assertions look the guard up by. *)
let tab_hex : string = String.make 32 'a'

let tab_id : Tab_id.t =
  Result.fold (Tab_id.of_string tab_hex)
    ~ok:Fun.id
    ~error:(fun (_ : Tab_id.err) ->
      Printf.printf "FAIL - test setup: the literal tab id is not in the grammar\n%!";
      exit 1)

(* Substring search that never indexes: the needle is carried as a set of
   partially-matched suffixes and every haystack character advances them. The
   obvious [String.sub] loop is what this replaces, and it is partial. *)
let contains ~(needle : string) (hay : string) : bool =
  let n = List.of_seq (String.to_seq needle) in
  let step ((partials : char list list), (found : bool)) (c : char) =
    let advanced =
      List.filter_map
        (fun (rest : char list) ->
          match rest with
          | [] -> None
          | h :: tl -> if Char.equal h c then Some tl else None)
        (n :: partials)
    in
    (advanced, found || List.exists List.is_empty advanced)
  in
  List.is_empty n || snd (String.fold_left step ([], false) hay)

(* Where a commit sits on the first-parent walk, newest first: [Some 0] is the
   head, [Some 1] is the head's parent. A fold rather than an index lookup
   because [List.nth] is partial and the parentage checks want a POSITION, not
   mere membership: "theirs is reachable somewhere" is satisfied by a history
   that simply kept both commits, which is the pre-D19 behaviour. *)
let position (needle : Prim.Commit_ref.t) (l : Prim.Commit_ref.t list) : int option =
  fst
    (List.fold_left
       (fun ((found : int option), (i : int)) (r : Prim.Commit_ref.t) ->
         ( (if
              Option.is_none found
              && String.equal (Prim.Commit_ref.to_string r) (Prim.Commit_ref.to_string needle)
            then Some i
            else found)
         , i + 1 ))
       (None, 0) l)

let parented_on (r : Prim.Commit_ref.t) (hist : Prim.Commit_ref.t list) : bool =
  Option.equal Int.equal (position r hist) (Some 1)

(* The guard verdicts C10 reads, spelled as predicates so a check asserts the
   CONSTRUCTOR and not merely a boolean derived from it. *)
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

(** Drive a promise the in-memory backend settles without yielding, from a
    synchronous context. The state is INSPECTED rather than assumed: a promise
    still sleeping would land its write at some unspecified later bind, which is
    exactly the scheduler assumption this file refuses to make, so it is
    reported as a setup failure instead of silently becoming a flake. *)
let now_or_never (what : string) (p : unit Lwt.t) : unit =
  match Lwt.state p with
  | Lwt.Return () -> ()
  | Lwt.Sleep ->
    Printf.printf "FAIL - test setup: %s did not settle synchronously\n%!" what;
    exit 1
  | Lwt.Fail (exc : exn) ->
    Printf.printf "FAIL - test setup: %s raised (%s)\n%!" what (Printexc.to_string exc);
    exit 1

(** The reconcile loop's one synchronous seam. [resolve] calls the app's own
    merge between the head a denied round re-observed and the test-and-set of
    the round after it (store_core.ml:500-503), so a third writer landed from
    inside a merge lands in precisely the window a two-round schedule needs.
    Set for C7 alone and one-shot, so [Doc3]'s merge is a pure function
    everywhere else in this file. *)
let mid_merge : (unit -> unit) option ref = ref None

let fire_mid_merge () : unit =
  Option.fold !mid_merge ~none:() ~some:(fun f ->
      mid_merge := None;
      f ())

(* --- The apps under test ---------------------------------------------------- *)

(** [Tags]: shared_doc's own CRDT shape (shared_doc_app.ml:34, :201-209) cut
    down to two fields. An [Or_set] is what C1 needs and a [Pn_counter] is not:
    the replica is minted from the BRANCH name (store_core.ml:559-563), R10's
    premise is one branch, and two increments under one replica join to one
    max, so a counter app would pass C1 against code that dropped a writer
    outright. [likes] is here for the amend checks and for the exploded pack
    witness, where a second field is what makes the framing contract load
    bearing. *)
module Tags_app = struct
  open Tea_core

  module Set = Crdt.Or_set (struct
    type t = string

    let compare = String.compare
    let t = Repr.string
  end)

  module Likes = Crdt.Pn_counter

  type model =
    { tags : Set.state
    ; likes : Likes.state
    }

  type msg =
    | Add_tag of string
    | Like

  let model_t =
    Repr.(
      record "tagdoc" (fun tags likes -> { tags; likes })
      |+ field "tags" Set.t (fun m -> m.tags)
      |+ field "likes" Likes.t (fun m -> m.likes)
      |> sealr)

  let msg_t =
    Repr.(
      variant "msg" (fun add_tag like -> function
        | Add_tag t -> add_tag t
        | Like -> like)
      |~ case1 "Add_tag" string (fun t -> Add_tag t)
      |~ case0 "Like" Like
      |> sealv)

  let bottom : model = { tags = Set.bottom; likes = Likes.bottom }
  let init = (bottom, Cmd.none)

  let update (ctx : Crdt.Ctx.t) (msg : msg) (m : model) =
    match msg with
    | Add_tag t -> ({ m with tags = Set.add (Crdt.Ctx.dot ctx) t m.tags }, Cmd.none)
    | Like -> ({ m with likes = Likes.inc (Crdt.Ctx.replica ctx) m.likes }, Cmd.none)

  let view (m : model) = Html.text (String.concat "," (Set.value m.tags))
  let subscriptions (_ : model) = Sub.none

  let doc_join =
    Crdt.record
      [ Crdt.field ~get:(fun m -> m.tags) ~set:(fun v m -> { m with tags = v }) ~join:Set.join
      ; Crdt.field ~get:(fun m -> m.likes) ~set:(fun v m -> { m with likes = v }) ~join:Likes.join
      ]

  let merge = Merge_spec.crdt_join doc_join
  let title = Prim.Title.v "tags"
  let url_of_model (_ : model) = None
  let msg_of_url (_ : Prim.Url.t) = None

  (** The D6 layout, one store path per CRDT field: C12's exploded twin. *)
  let explode_fields : (string * (model -> model)) list =
    [ ("tags", fun m -> { bottom with tags = m.tags })
    ; ("likes", fun m -> { bottom with likes = m.likes })
    ]

  let tags_of (m : model) : string list = Set.value m.tags
  let likes_of (m : model) : int = Likes.value m.likes
  let tags_equal (a : model) (b : model) : bool = Set.equal a.tags b.tags
end

module _ : Tea_core.App.APP = Tags_app

(** [Doc3]: a three-way app over a record with one removable field, merging
    field by field and returning [Ok] whenever only one side moved. The
    removable field is the whole point: a delete and a re-add are what
    distinguish a re-based ancestor from a frozen one, and an app whose fields
    all merely overwrite cannot tell them apart. *)
module Doc3_app = struct
  open Tea_core

  type model =
    { note : string
    ; x : string option
    }

  type msg =
    | Set_note of string
    | Drop_x
    | Put_x of string

  let model_t =
    Repr.(
      record "doc3" (fun note x -> { note; x })
      |+ field "note" string (fun m -> m.note)
      |+ field "x" (option string) (fun m -> m.x)
      |> sealr)

  let msg_t =
    Repr.(
      variant "msg" (fun set_note drop_x put_x -> function
        | Set_note s -> set_note s
        | Drop_x -> drop_x
        | Put_x s -> put_x s)
      |~ case1 "Set_note" string (fun s -> Set_note s)
      |~ case0 "Drop_x" Drop_x
      |~ case1 "Put_x" string (fun s -> Put_x s)
      |> sealv)

  (* [x = None] at init is load bearing for C10: the absent-head arm must
     commit ours as a fresh root rather than merge against this value, and a
     merge against it would read as "theirs deleted x". *)
  let init = ({ note = "n0"; x = None }, Cmd.none)

  let update (_ : Crdt.Ctx.t) (msg : msg) (m : model) =
    match msg with
    | Set_note s -> ({ m with note = s }, Cmd.none)
    | Drop_x -> ({ m with x = None }, Cmd.none)
    | Put_x s -> ({ m with x = Some s }, Cmd.none)

  let view (m : model) = Html.text m.note
  let subscriptions (_ : model) = Sub.none

  (* One field's three-way rule: whichever side moved off the ancestor wins,
     agreement wins, and a genuine divergence is declared. With no ancestor
     (the store never hands one for a fresh root) only agreement is safe. *)
  let pick ~(eq : 'a -> 'a -> bool) ~(ancestor : 'a option) ~(ours : 'a) ~(theirs : 'a) :
      ('a, string) result =
    Option.fold ancestor
      ~none:(fun () -> if eq ours theirs then Ok ours else Error "no common ancestor")
      ~some:(fun (a : 'a) () ->
        if eq ours a then Ok theirs
        else if eq theirs a then Ok ours
        else if eq ours theirs then Ok ours
        else Error "both sides moved")
      ()

  let three_way ~(ancestor : model option) ~(ours : model) ~(theirs : model) :
      (model, string) result =
    fire_mid_merge ();
    Result.bind
      (pick ~eq:String.equal
         ~ancestor:(Option.map (fun (a : model) -> a.note) ancestor)
         ~ours:ours.note ~theirs:theirs.note)
      (fun (note : string) ->
        Result.map
          (fun (x : string option) -> { note; x })
          (pick
             ~eq:(Option.equal String.equal)
             ~ancestor:(Option.map (fun (a : model) -> a.x) ancestor)
             ~ours:ours.x ~theirs:theirs.x))

  let merge = Merge_spec.Three_way three_way
  let title = Prim.Title.v "doc3"
  let url_of_model (_ : model) = None
  let msg_of_url (_ : Prim.Url.t) = None
end

module _ : Tea_core.App.APP = Doc3_app

(** The two non-CRDT arms need an app each and nothing else, so they share one
    body and differ only in the {!Tea_core.Merge_spec.t} they register. Both
    arms keep [ours] and part with [theirs]: that is the honest limit of the
    closure (R10c), and these two apps are what make it a claim a run can
    falsify rather than a sentence in a design record. *)
module Text_app (P : sig
  val merge : string Tea_core.Merge_spec.t
  val name : string
end) =
struct
  open Tea_core

  type model = string
  type msg = Say of string

  let model_t = Repr.string

  let msg_t =
    Repr.(
      variant "msg" (fun say -> function Say s -> say s)
      |~ case1 "Say" string (fun s -> Say s)
      |> sealv)

  let init = ("", Cmd.none)
  let update (_ : Crdt.Ctx.t) (Say s : msg) (_ : model) = (s, Cmd.none)
  let view (m : model) = Html.text m
  let subscriptions (_ : model) = Sub.none
  let merge = P.merge
  let title = Prim.Title.v P.name
  let url_of_model (_ : model) = None
  let msg_of_url (_ : Prim.Url.t) = None
end

(** [Bad3]: a three-way app that always declares. Its reason is the string the
    commit label must carry: an app-declared loss recorded nowhere is a loss
    that dies with the process. *)
module Bad3_app = Text_app (struct
  let merge =
    Tea_core.Merge_spec.Three_way
      (fun ~ancestor:(_ : string option) ~ours:(_ : string) ~theirs:(_ : string) ->
        Error "declared")

  let name = "bad3"
end)

module _ : Tea_core.App.APP = Bad3_app

(** [Lww]: the last-write-wins arm, R10c's other half. *)
module Lww_app = Text_app (struct
  let merge = Tea_core.Merge_spec.Last_write_wins
  let name = "lww"
end)

module _ : Tea_core.App.APP = Lww_app

module Tags_store = Tea_persist.Store.Make (Tags_app)
module Tags_pack = Tea_persist_pack.Store_pack.Make (Tags_app)
module Tags_server = Tea_server.Make (Tags_app)
module Doc3_store = Tea_persist.Store.Make (Doc3_app)
module Bad3_store = Tea_persist.Store.Make (Bad3_app)
module Lww_store = Tea_persist.Store.Make (Lww_app)
module Codec = Tea_core.Codec.Make (Tags_app)
module Wire = Tea_core.Wire

let model_eq : Tags_app.model -> Tags_app.model -> bool =
  Repr.(unstage (equal Tags_app.model_t))

let has (t : string) (m : Tags_app.model) : bool =
  List.exists (String.equal t) (Tags_app.tags_of m)

let frozen (n : int) : unit -> int64 = fun () -> Int64.of_int n

(* --- C1-C5: the flagship interleave ------------------------------------------
   One branch, two writers, program order: A reads; B reads, edits and commits;
   A edits the model IT witnessed and commits. Before D19 A's commit re-read the
   head, tested against that read, and landed a model that never saw "beta". *)

let () =
  Lwt_main.run
    (let* t = Tags_store.create ~now:(frozen 1_000) () in
     let* s = Tags_store.session t (sid "interleave") in
     let ctx = Tags_store.ctx_of_session s in
     let* a = Tags_store.load_based s in
     let* b = Tags_store.load_based s in
     let m_b = fst (Tags_app.update ctx (Tags_app.Add_tag "beta") (Tags_store.based_model b)) in
     let* landed_b = Tags_store.commit_based b ~label:"beta" m_b in
     let* b_head = Tags_store.head_ref s in
     let m_a = fst (Tags_app.update ctx (Tags_app.Add_tag "alpha") (Tags_store.based_model a)) in
     let* landed_a = Tags_store.commit_based a ~label:"alpha" m_a in
     let* head = Tags_store.load s in
     (* The regression this catches is R10 itself: a last-write-wins commit
        derived from A's stale read leaves the head holding "alpha" alone,
        with B's already-acked edit gone from content while still sitting in
        history. *)
     check "both writers' content survives an interleaved commit"
       (has "alpha" head && has "beta" head && Int.equal (List.length (Tags_app.tags_of head)) 2);
     (* C1 cannot see a dot collision, because its two writers add DIFFERENT
        elements and a join keeps both however they are tagged. So the same two
        writers, from the same models, add the SAME element here: the states
        differ only in the dot each minted. A refactor giving each writer its
        own clock re-collides them and reddens exactly this line. *)
     let same_from (base : Tags_app.model) =
       fst (Tags_app.update ctx (Tags_app.Add_tag "same") base)
     in
     let dot_a = same_from (Tags_store.based_model a) in
     let dot_b = same_from (Tags_store.based_model b) in
     check "the two interleaved writers minted distinct dots"
       (not (Tags_app.tags_equal dot_a dot_b));
     let* hist = Tags_store.history s in
     (* Reconciled, not rebased away: the loser's commit is the winner's first
        parent, so "both writers survive" is a statement about the graph too and
        not only about the blob at the head. *)
     check "the reconciled commit is parented on the winner"
       (parented_on (must "B's commit ref" b_head) hist
       && Int.equal (List.length hist) 2);
     (* The water a floor may claim is the WINNING round's mint, never the first
        attempt's: a floor stamped from a denied round names a commit the store
        does not have, which is the forged witness D18 exists to prevent. *)
     check "the winning round's water is the newest mint"
       (Water.covers ~head:landed_a.Tags_store.water ~floor:landed_b.Tags_store.water
       && not (Water.covers ~head:landed_b.Tags_store.water ~floor:landed_a.Tags_store.water));
     (* Also the anti-hang pin: an unbudgeted loop whose progress cannot be
        observed can only fail as a hang, and a hang is not a test result. *)
     check "the interleave resolves in exactly one round"
       (Int.equal landed_a.Tags_store.rounds 1 && Int.equal landed_b.Tags_store.rounds 0);
     Tags_store.close t)

(* --- C6: the uncontended recipe ---------------------------------------------
   The eager-[Option.fold] hazard on a two-armed witness would mint a commit per
   arm; there is one arm today, and this pins it. *)

let () =
  Lwt_main.run
    (let* t = Tags_store.create ~now:(frozen 2_000) () in
     let* based_s = Tags_store.session t (sid "uncontended") in
     let* plain_s = Tags_store.session t (sid "uncontendedplain") in
     let* b = Tags_store.load_based based_s in
     let m =
       fst
         (Tags_app.update
            (Tags_store.ctx_of_session based_s)
            (Tags_app.Add_tag "solo")
            (Tags_store.based_model b))
     in
     let* landed = Tags_store.commit_based b ~label:"solo" m in
     let* (_ : Water.t) = Tags_store.commit plain_s ~label:"solo" m in
     let* based_hist = Tags_store.history based_s in
     let* plain_hist = Tags_store.history plain_s in
     let* based_head = Tags_store.load based_s in
     let* plain_head = Tags_store.load plain_s in
     check "an uncontended based commit mints exactly one commit"
       (Int.equal (List.length based_hist) 1
       && Int.equal (List.length plain_hist) 1
       && model_eq based_head plain_head
       && Int.equal landed.Tags_store.rounds 0);
     Tags_store.close t)

(* --- C7: two contention rounds ----------------------------------------------
   The schedule A-G6 describes: A holds x, B deletes it, C re-adds it, and A's
   SECOND round must read theirs as an add. It needs the head to move twice
   inside one loop, which program order alone cannot arrange, so C's commit is
   fired from inside A's first merge: the one point in the loop that runs
   synchronously between a re-observed head and the next test-and-set. *)

let () =
  Lwt_main.run
    (let* t = Doc3_store.create ~now:(frozen 3_000) () in
     let* s = Doc3_store.session t (sid "ancestor") in
     let* seed = Doc3_store.load_based s in
     let* (_ : Doc3_store.committed) =
       Doc3_store.commit_based seed ~label:"seed" { Doc3_app.note = "n0"; x = Some "x" }
     in
     let* a = Doc3_store.load_based s in
     let* b = Doc3_store.load_based s in
     let* (_ : Doc3_store.committed) =
       Doc3_store.commit_based b ~label:"b-drop"
         (fst (Doc3_app.update (Doc3_store.ctx_of_session s) Doc3_app.Drop_x
                 (Doc3_store.based_model b)))
     in
     mid_merge :=
       Some
         (fun () ->
           now_or_never "C's re-add"
             (let* c = Doc3_store.load_based s in
              let* (_ : Doc3_store.committed) =
                Doc3_store.commit_based c ~label:"c-readd"
                  (fst
                     (Doc3_app.update
                        (Doc3_store.ctx_of_session s)
                        (Doc3_app.Put_x "x")
                        (Doc3_store.based_model c)))
              in
              Lwt.return_unit));
     let* landed =
       Doc3_store.commit_based a ~label:"a-note"
         (fst (Doc3_app.update (Doc3_store.ctx_of_session s) (Doc3_app.Set_note "a")
                 (Doc3_store.based_model a)))
     in
     (* With the ancestor frozen at A's own read, round two compares an [ours]
        that already absorbed round one's delete against a base that predates
        it, reads C's re-add as "ours deleted it", and drops x: an item
        vanishing seconds after its writer saw it acked, with no writer having
        deleted it. *)
     check "two contention rounds use the true ancestor"
       (Int.equal landed.Doc3_store.rounds 2
       && Option.equal String.equal landed.Doc3_store.model.Doc3_app.x (Some "x")
       && String.equal landed.Doc3_store.model.Doc3_app.note "a");
     Doc3_store.close t)

(* --- C8: a declared conflict -------------------------------------------------
   The app refuses the merge. Ours is what the pump is about to acknowledge, so
   ours must land; theirs stays reachable as the parent; and the app's REASON
   travels into the commit message, because a loss recorded only by an eprintf
   dies with the process while the branch is this system's audit surface. *)

let () =
  Lwt_main.run
    (let* t = Bad3_store.create ~now:(frozen 4_000) () in
     let* s = Bad3_store.session t (sid "declared") in
     let* a = Bad3_store.load_based s in
     let* b = Bad3_store.load_based s in
     let* (_ : Bad3_store.committed) = Bad3_store.commit_based b ~label:"theirs" "theirs" in
     let* b_head = Bad3_store.head_ref s in
     let* landed = Bad3_store.commit_based a ~label:"c8-edit" "ours" in
     let* head = Bad3_store.load s in
     let* hist = Bad3_store.history s in
     let* branch = Bad3_store.S.of_branch (Bad3_store.repo t) (branch_of "declared") in
     let* head_commit = Bad3_store.S.Head.find branch in
     let message =
       Option.fold head_commit ~none:""
         ~some:(fun (c : Bad3_store.S.commit) ->
           Bad3_store.S.Info.message (Bad3_store.S.Commit.info c))
     in
     check "a declared conflict keeps ours, keeps theirs as parent, and says why"
       (String.equal head "ours"
       && String.equal landed.Bad3_store.model "ours"
       && parented_on (must "the refused writer's commit ref" b_head) hist
       && contains ~needle:"declared" message);
     Bad3_store.close t)

(* --- C9: last write wins ----------------------------------------------------
   The R10c pin. A [Last_write_wins] app drops the loser's content by policy,
   and the only thing D19 can promise it is that the loser stays in history
   instead of being erased by an unconditional set. *)

let () =
  Lwt_main.run
    (let* t = Lww_store.create ~now:(frozen 5_000) () in
     let* s = Lww_store.session t (sid "lastwrite") in
     let* a = Lww_store.load_based s in
     let* b = Lww_store.load_based s in
     let* (_ : Lww_store.committed) = Lww_store.commit_based b ~label:"theirs" "theirs" in
     let* b_head = Lww_store.head_ref s in
     let* (_ : Lww_store.committed) = Lww_store.commit_based a ~label:"ours" "ours" in
     let* head = Lww_store.load s in
     let* hist = Lww_store.history s in
     check "last-write-wins keeps ours with theirs as parent"
       (String.equal head "ours"
       && parented_on (must "the overwritten writer's commit ref" b_head) hist
       && Int.equal (List.length hist) 2);
     Lww_store.close t)

(* --- C10: a reap between the read and the commit -----------------------------
   A denied test-and-set does not imply a competitor: the reaper removes whole
   branches, and [gather] reports an absent branch as the app's INITIAL model,
   which a three-way policy reads as "theirs deleted everything". The loop must
   commit ours as a fresh root instead. The reaper's own precondition (?forget
   before removal) is wired, so the returning client's replay reads Fresh
   rather than Duplicate against a floor over a branch that no longer exists. *)

let () =
  Lwt_main.run
    (let* t = Doc3_store.create ~now:(frozen 6_000) () in
     let* s = Doc3_store.session t (sid "reaped") in
     let replica = Crdt.Ctx.replica (Doc3_store.ctx_of_session s) in
     let guard =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
         ~sink:Guard_sink.null ~floors:Floors.empty ()
     in
     let* seed = Doc3_store.load_based s in
     let* (_ : Doc3_store.committed) =
       Doc3_store.commit_based seed ~label:"seed" { Doc3_app.note = "n0"; x = Some "seeded" }
     in
     let* water = Doc3_store.head_water s in
     let* (_ : (unit, Guard_sink.err) result) =
       Dguard.persist guard ~replica ~tab:tab_id ~seq:(seq 4) ~water
     in
     (* The verdict reports the FLOOR it de-duplicated against, not the seq it
        was asked about (replay_guard.ml:110), so both numbers here are 4. *)
     let deduplicated_before =
       is_duplicate_at (Dguard.take guard ~replica ~tab:tab_id ~seq:(seq 4)) ~at:4
     in
     let* a = Doc3_store.load_based s in
     let* swept =
       Doc3_store.reap
         ~forget:(fun (victim : Prim.Session_id.t) ->
           Lwt.map
             (fun (_ : (unit, Guard_sink.err) result) -> ())
             (Dguard.forget guard ~replica:(Crdt.Replica.v victim)))
         t
         ~ttl:(must "Ttl.of_seconds refused 60." (Prim.Ttl.of_seconds 60.))
         ~now:9_000_000L
     in
     (* The precondition the rest of the check rests on, asserted rather than
        assumed: the branch really is gone under A's witness. The sweep COUNT is
        only a lower bound here, because [Irmin_mem] hands every handle in this
        process the same backing store, so branches from earlier sections of
        this file are legitimate victims too. *)
     let* gone = Doc3_store.head_ref s in
     let ours =
       fst
         (Doc3_app.update (Doc3_store.ctx_of_session s) (Doc3_app.Set_note "a")
            (Doc3_store.based_model a))
     in
     let* landed = Doc3_store.commit_based a ~label:"a-after-reap" ours in
     let* head = Doc3_store.load s in
     let* hist = Doc3_store.history s in
     (* [x] is the discriminator: a loop that resolved the absent head against
        [fst init] (whose x is None) would read theirs as a delete and drop it,
        and a loop that refused outright would leave the branch empty. A fresh
        ROOT is the third answer, and the only loss-free one. *)
     check "a reap between the read and the commit neither wipes nor resurrects blindly"
       (Int.compare swept 1 >= 0
       && Option.is_none gone
       && deduplicated_before
       && Option.equal String.equal head.Doc3_app.x (Some "seeded")
       && String.equal head.Doc3_app.note "a"
       && Int.equal (List.length hist) 1
       && Int.equal landed.Doc3_store.rounds 1
       && is_fresh_at (Dguard.take guard ~replica ~tab:tab_id ~seq:(seq 1)) ~at:1);
     Doc3_store.close t)

(* --- C11/C12: the pack durability of the based write path (U1) ---------------
   The whole-blob arm of plain [commit] is a [set_exn] transaction whose pack
   durability is proven; the based path mints [S.Commit.v] over a tree built
   from the WITNESS's tree instead, and store_core.ml:515-522 records that a
   root tree rebuilt from [S.tree] and re-saved does NOT survive a pack reopen.
   These two are the decision point: if a whole-blob based commit cannot be
   read back after a close/reopen, the based path needs Irmin's contents-level
   test_and_set door rather than this one. *)

let scratch_counter : int ref = ref 0
let () = Random.self_init ()

(* A fresh scratch parent per pack check: a per-run random tag plus a counter,
   so a crashed prior run's leftovers cannot collide, and no raising temp-dir
   helper is involved. *)
let fresh_scratch () : string =
  let n = !scratch_counter in
  scratch_counter := n + 1;
  Filename.concat
    (Filename.get_temp_dir_name ())
    (Printf.sprintf "ocaml-tea-contention-%x-%d" (Random.bits ()) n)

let mkdir (path : string) : unit Lwt.t =
  fs (Printf.sprintf "mkdir %s" path) (fun () -> Lwt_unix.mkdir path 0o755)

let entries_of (path : string) : string list Lwt.t =
  fs (Printf.sprintf "list %s" path) (fun () ->
      Lwt_stream.to_list (Lwt_unix.files_of_directory path)
      |> Lwt.map
           (List.filter (fun (e : string) -> not (String.equal e "." || String.equal e ".."))))

let rec rm_rf (path : string) : unit Lwt.t =
  fs (Printf.sprintf "remove %s" path) (fun () ->
      let* st = Lwt_unix.stat path in
      match st.Lwt_unix.st_kind with
      | Unix.S_DIR ->
        let* entries = entries_of path in
        let* () =
          Lwt_list.iter_s (fun (entry : string) -> rm_rf (Filename.concat path entry)) entries
        in
        Lwt_unix.rmdir path
      | Unix.S_REG | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO | Unix.S_SOCK ->
        Lwt_unix.unlink path)

let in_scratch_pack (f : root:string -> unit Lwt.t) : unit =
  Lwt_main.run
    (let parent = fresh_scratch () in
     let* () = mkdir parent in
     let* () = f ~root:(Filename.concat parent "store") in
     rm_rf parent)

(* One arm of the pack pin, run twice: once with no witness (the whole-blob
   write) and once with the D6 witness registered (the exploded write). Both
   run inside [fs], so a decode raise on the reopen is a FAIL line the mutation
   driver can match rather than a bare non-zero exit. *)
let pack_round_trip ~(label : string) ~(exploded : Tags_pack.exploder option) ~(root : string)
    : unit Lwt.t =
  fs label (fun () ->
      let open_at (now : int) : Tags_pack.t Lwt.t =
        Option.fold exploded
          ~none:(fun () ->
            Tags_pack.create ~now:(frozen now) (Tea_persist_pack.Store_pack.Root.v root))
          ~some:(fun (x : Tags_pack.exploder) () ->
            Tags_pack.create ~now:(frozen now) ~exploded:x
              (Tea_persist_pack.Store_pack.Root.v root))
          ()
      in
      let* t = open_at 1_000 in
      let* s = Tags_pack.session t (sid "packbased") in
      let* b = Tags_pack.load_based s in
      let* (_ : Tags_pack.committed) =
        Tags_pack.commit_based b ~label:"first"
          (fst
             (Tags_app.update (Tags_pack.ctx_of_session s) (Tags_app.Add_tag "durable")
                (Tags_pack.based_model b)))
      in
      let* () = Tags_pack.close t in
      let* t2 = open_at 2_000 in
      let* s2 = Tags_pack.session t2 (sid "packbased") in
      let* reopened = Tags_pack.load s2 in
      (* A further based commit AFTER the reopen: the first one could survive
         on bytes the writing process still had in hand, while this one has to
         build its tree from a commit decoded off disk. *)
      let* b2 = Tags_pack.load_based s2 in
      let* (_ : Tags_pack.committed) =
        Tags_pack.commit_based b2 ~label:"second"
          (fst
             (Tags_app.update (Tags_pack.ctx_of_session s2) (Tags_app.Add_tag "second")
                (Tags_pack.based_model b2)))
      in
      let* final = Tags_pack.load s2 in
      let* hist = Tags_pack.history s2 in
      check label
        (has "durable" reopened
        && has "durable" final
        && has "second" final
        && Int.equal (List.length hist) 2);
      Tags_pack.close t2)

let () =
  in_scratch_pack (fun ~root ->
      pack_round_trip ~label:"a whole-blob based commit survives pack close and reopen"
        ~exploded:None ~root)

let () =
  in_scratch_pack (fun ~root ->
      let witness =
        Tags_pack.exploder ~bottom:Tags_app.bottom ~join:Tags_app.doc_join
          ~fields:
            (List.map
               (fun ((n : string), (project : Tags_app.model -> Tags_app.model)) ->
                 (* Spelled out rather than through a local open: the scratch
                    directory in scope is also called [root]. *)
                 (Tea_safe.Safe_key.root (Tea_safe.Safe_key.Step.v n), project))
               Tags_app.explode_fields)
      in
      pack_round_trip ~label:"an exploded based commit survives pack close and reopen"
        ~exploded:(Some witness) ~root)

(* --- C13/C14: the coalescer under contention ---------------------------------
   The pre-D19 append retried a stale model against a freshly read head three
   times and then fell back to a plain [commit], an unconditional last write
   after three losses. Both paths now go through the witnessed loop. *)

(* Only a run of [Like]s folds. Every pair is enumerated so a new Msg is a
   compile error here rather than a silently unfolded run. *)
let fold_likes ~(last : Tags_app.msg) ~(next : Tags_app.msg) : Tags_app.msg option =
  match (last, next) with
  | Tags_app.Like, Tags_app.Like -> Some Tags_app.Like
  | Tags_app.Like, Tags_app.Add_tag (_ : string) -> None
  | Tags_app.Add_tag (_ : string), Tags_app.Like -> None
  | Tags_app.Add_tag (_ : string), Tags_app.Add_tag (_ : string) -> None

let () =
  Lwt_main.run
    (let* t = Tags_store.create ~now:(frozen 7_000) () in
     let* s = Tags_store.session t (sid "coalesceappend") in
     let ctx = Tags_store.ctx_of_session s in
     let cz = Tags_store.Coalescer.v (Tea_core.Coalesce_spec.Fold_run fold_likes) in
     let* first = Tags_store.load_based s in
     let* (_ : Tags_store.committed) =
       Tags_store.commit_coalesced cz first ~msg:(Tags_app.Add_tag "one")
         (fst (Tags_app.update ctx (Tags_app.Add_tag "one") (Tags_store.based_model first)))
     in
     (* A reads, a foreign writer lands, and A appends: the policy refuses to
        fold two tag adds, so this is the append path with a stale witness. *)
     let* a = Tags_store.load_based s in
     let* foreign = Tags_store.load_based s in
     let* (_ : Tags_store.committed) =
       Tags_store.commit_based foreign ~label:"beta"
         (fst (Tags_app.update ctx (Tags_app.Add_tag "beta") (Tags_store.based_model foreign)))
     in
     let* landed =
       Tags_store.commit_coalesced cz a ~msg:(Tags_app.Add_tag "alpha")
         (fst (Tags_app.update ctx (Tags_app.Add_tag "alpha") (Tags_store.based_model a)))
     in
     let* head = Tags_store.load s in
     check "a coalesced append reconciles instead of clobbering"
       (has "one" head && has "beta" head && has "alpha" head
       && Int.equal landed.Tags_store.rounds 1);
     Tags_store.close t)

let () =
  Lwt_main.run
    (let* t = Tags_store.create ~now:(frozen 8_000) () in
     let* s = Tags_store.session t (sid "coalesceamend") in
     let ctx = Tags_store.ctx_of_session s in
     let cz = Tags_store.Coalescer.v (Tea_core.Coalesce_spec.Fold_run fold_likes) in
     let* first = Tags_store.load_based s in
     let* (_ : Tags_store.committed) =
       Tags_store.commit_coalesced cz first ~msg:Tags_app.Like
         (fst (Tags_app.update ctx Tags_app.Like (Tags_store.based_model first)))
     in
     (* A witnesses the commit this coalescer minted, so the run is amendable;
        then a foreign writer lands. The amend's test-and-set is denied, the
        run is sealed, and the append that follows still reconciles against the
        witness A is holding rather than re-reading a head. *)
     let* a = Tags_store.load_based s in
     let* foreign = Tags_store.load_based s in
     let* (_ : Tags_store.committed) =
       Tags_store.commit_based foreign ~label:"beta"
         (fst (Tags_app.update ctx (Tags_app.Add_tag "beta") (Tags_store.based_model foreign)))
     in
     let* foreign_head = Tags_store.head_ref s in
     let* landed =
       Tags_store.commit_coalesced cz a ~msg:Tags_app.Like
         (fst (Tags_app.update ctx Tags_app.Like (Tags_store.based_model a)))
     in
     let* head = Tags_store.load s in
     let* hist = Tags_store.history s in
     (* Three commits, not two: a successful amend would have rewritten the
        run's own commit in place and left the history one shorter, so the
        length is what says the amend was actually refused. *)
     check "an interrupted amend re-folds on the new head"
       (has "beta" head
       && Int.equal (Tags_app.likes_of head) 2
       && Int.equal (Tags_app.likes_of landed.Tags_store.model) 2
       && Int.equal (List.length hist) 3
       && parented_on (must "the foreign writer's commit ref" foreign_head) hist);
     Tags_store.close t)

(* --- C15: R10b closed (step 19) ----------------------------------------------
   Until step 19 this was a labelled CHARACTERIZATION: [undo] moved the head
   with a plain [S.Head.set] while [checkpoint] moved by test-and-set, so the
   CAS a based commit had just won bought nothing against it - the acked
   commit was erased outright and the delivery floor stamped from its water
   was left claiming a commit the store no longer held, the forged witness
   the whole guard family exists to prevent. [undo] now takes the caller's
   [based] witness and test-and-sets the head against it, so the same stale
   undo REFUSES with [Branch_moved]: the acked commit keeps its place on the
   branch and its floor stays covered. This check fails on erasure.

   The witness sits on a NON-ROOT commit ("kept") on purpose: a witness at
   the root would refuse one arm earlier ([At_root], before the test-and-set
   is ever consulted), and a reverted guard would then still pass. Only the
   non-root shape makes the erasure observable, so only it kills the
   [S.Head.set] revertant. *)

let () =
  Lwt_main.run
    (let* t = Tags_store.create ~now:(frozen 9_000) () in
     let* s = Tags_store.session t (sid "undorace") in
     let ctx = Tags_store.ctx_of_session s in
     let* seed = Tags_store.load_based s in
     let* (_ : Tags_store.committed) =
       Tags_store.commit_based seed ~label:"seed"
         (fst (Tags_app.update ctx (Tags_app.Add_tag "seed") (Tags_store.based_model seed)))
     in
     let* w = Tags_store.load_based s in
     let* (_ : Tags_store.committed) =
       Tags_store.commit_based w ~label:"kept"
         (fst (Tags_app.update ctx (Tags_app.Add_tag "kept") (Tags_store.based_model w)))
     in
     (* The undo's witness: taken while "kept" stands, BEFORE the racer. *)
     let* a = Tags_store.load_based s in
     let* landed =
       Tags_store.commit_based a ~label:"acked"
         (fst (Tags_app.update ctx (Tags_app.Add_tag "acked") (Tags_store.based_model a)))
     in
     let* refused = Tags_store.undo a in
     let* head = Tags_store.load s in
     let* water = Tags_store.head_water s in
     let denied =
       Result.fold refused
         ~ok:(fun (_ : Tags_app.model) -> false)
         ~error:(fun (e : Tags_store.undo_error) ->
           match e with
           | Tags_store.Branch_moved -> true
           | Tags_store.At_root -> false)
     in
     check "undo racing a based commit refuses and erases nothing (R10b closed)"
       (denied
       && has "kept" head
       && has "acked" head
       && Water.covers ~head:water ~floor:landed.Tags_store.water);
     (* The refusal left no trace: the redo slot was never stamped. This is
        the step-19 ordering reversal made observable - a write-ref-first
        undo would answer [Branch_moved] here (a real, stale pointer), not
        [Nothing_to_redo]. *)
     let* after = Tags_store.redo s in
     check "a refused undo leaves the redo slot empty (Nothing_to_redo)"
       (Result.fold after
          ~ok:(fun (_ : Tags_app.model) -> false)
          ~error:(fun (e : Tags_store.redo_error) ->
            match e with
            | Tags_store.Nothing_to_redo -> true
            | Tags_store.Branch_moved -> false));
     Tags_store.close t)

(* --- C16: redo refuses past intervening work ---------------------------------
   The redo pointer is single-slot and durable, and until step 19 [redo]
   consumed it with a plain [S.Head.set]: undo, then any ordinary commit, then
   redo silently discarded the commit - no concurrency required. [redo] now
   test-and-sets against the pointer target's own first parent, exactly the
   head the pointer expects, so a redo following intervening work refuses with
   [Branch_moved] (and NOT [Nothing_to_redo]: the pointer is real, merely
   stale) and the intervening commit survives. *)

let () =
  Lwt_main.run
    (let* t = Tags_store.create ~now:(frozen 9_300) () in
     let* s = Tags_store.session t (sid "redointervene") in
     let ctx = Tags_store.ctx_of_session s in
     let* w0 = Tags_store.load_based s in
     let* (_ : Tags_store.committed) =
       Tags_store.commit_based w0 ~label:"one"
         (fst (Tags_app.update ctx (Tags_app.Add_tag "one") (Tags_store.based_model w0)))
     in
     let* w1 = Tags_store.load_based s in
     let* (_ : Tags_store.committed) =
       Tags_store.commit_based w1 ~label:"two"
         (fst (Tags_app.update ctx (Tags_app.Add_tag "two") (Tags_store.based_model w1)))
     in
     let* w2 = Tags_store.load_based s in
     let* undone = Tags_store.undo w2 in
     check "C16 rig: a fresh-witness undo of \"two\" lands (Ok)" (Result.is_ok undone);
     let* w3 = Tags_store.load_based s in
     let* (_ : Tags_store.committed) =
       Tags_store.commit_based w3 ~label:"three"
         (fst (Tags_app.update ctx (Tags_app.Add_tag "three") (Tags_store.based_model w3)))
     in
     let* redone = Tags_store.redo s in
     let* head = Tags_store.load s in
     let stale_pointer =
       Result.fold redone
         ~ok:(fun (_ : Tags_app.model) -> false)
         ~error:(fun (e : Tags_store.redo_error) ->
           match e with
           | Tags_store.Branch_moved -> true
           | Tags_store.Nothing_to_redo -> false)
     in
     check "redo past intervening work refuses; the intervening commit survives"
       (stale_pointer && has "three" head && has "one" head && not (has "two" head));
     Tags_store.close t)

(* --- C17: fork's write is an atomic absent-check -----------------------------
   fork's no-op premise ("the destination is empty") was checked and then
   acted on in two separate steps; a racer's commit landing on the destination
   between them was silently overwritten by fork's copy of [from]'s head.
   Step 19 makes the write a [test_and_set ~test:None] - Irmin's own
   must-be-absent idiom - so the racer's commit survives and it is fork's
   copy that is discarded, the same arm the non-empty observation takes.
   [?interpose] fires in exactly that window, in program order. *)

let () =
  Lwt_main.run
    (let* t = Tags_store.create ~now:(frozen 9_600) () in
     let* src = Tags_store.session t (sid "forksrc") in
     let src_ctx = Tags_store.ctx_of_session src in
     let* w = Tags_store.load_based src in
     let* (_ : Tags_store.committed) =
       Tags_store.commit_based w ~label:"src"
         (fst (Tags_app.update src_ctx (Tags_app.Add_tag "src") (Tags_store.based_model w)))
     in
     let dst_sid = sid "forkdst" in
     (* A second handle onto the destination branch: the racer. *)
     let* racer = Tags_store.session t dst_sid in
     let racer_ctx = Tags_store.ctx_of_session racer in
     let interpose () =
       let* rw = Tags_store.load_based racer in
       let* (_ : Tags_store.committed) =
         Tags_store.commit_based rw ~label:"racer"
           (fst
              (Tags_app.update racer_ctx (Tags_app.Add_tag "racer")
                 (Tags_store.based_model rw)))
       in
       Lwt.return_unit
     in
     let* forked = Tags_store.fork ~interpose t ~from:src dst_sid in
     let* head = Tags_store.load forked in
     check "fork discards its own copy when a racer lands first (racer survives)"
       (has "racer" head && not (has "src" head));
     Tags_store.close t)

(* --- S1: the server step seam -----------------------------------------------
   [?interpose] fires between the witnessed read and the commit, the one
   window that matters, so a foreign commit lands there without a sleep and
   without a scheduler assumption. It is deliberately NOT an injectable commit
   function: a commit seam would let an injected closure ignore the witness and
   call the last-write-wins door, which is a compile-clean way to disable D19,
   and tea_server has no .mli in which to forbid it. *)

let () =
  Lwt_main.run
    (let* repo = Tags_server.Store.create () in
     (* A named session rather than [main]: [Irmin_mem] backs every handle in
        this process with one store, so two sections sharing [main] would share
        a branch and each would start on the other's commits. *)
     let* s = Tags_server.Store.session repo (sid "stepseam") in
     let ctx = Tags_server.Store.ctx_of_session s in
     let interpose () =
       let* foreign = Tags_server.Store.load_based s in
       let* (_ : Tags_server.Store.committed) =
         Tags_server.Store.commit_based foreign ~label:"beta"
           (fst
              (Tags_app.update ctx (Tags_app.Add_tag "beta")
                 (Tags_server.Store.based_model foreign)))
       in
       Lwt.return_unit
     in
     let* stepped = Tags_server.step ~interpose s (Tags_app.Add_tag "alpha") in
     Result.fold stepped
       ~error:(fun (Tags_server.Loop.Fuel_exhausted : Tags_server.Loop.err) ->
         check "a mid-step writer is not erased by a Cmd-tail step" false;
         Lwt.return_unit)
       ~ok:(fun (o : Tags_server.step_outcome) ->
         let* head = Tags_server.Store.load s in
         let* water = Tags_server.Store.head_water s in
         (* The outcome carries the COMMITTED model, not the caller's own
            transition: a caller reporting state to a user (the SSR render, the
            delivery floor) must report what landed. *)
         check "a mid-step writer is not erased by a Cmd-tail step"
           (has "alpha" head && has "beta" head
           && has "alpha" o.Tags_server.model
           && has "beta" o.Tags_server.model
           && Water.equal water o.Tags_server.water);
         Lwt.return_unit))

(* --- S2: the pump's floor and ack -------------------------------------------
   The same seam through [live_session] over in-memory queues: no socket, no
   handshake. The floor a taken message is stamped with must be the water of
   the commit that actually landed, because a floor claiming a state it did not
   de-duplicate against is a forged witness, and a reconciled round's commit is
   not the one the caller's first attempt minted. *)

let await = Test_util.await

(* [live_session] must return on peer close; bound the wait so a regression
   hangs the check, not the suite. *)
let closes_within session =
  Lwt.pick
    [ (let* () = session in
       Lwt.return true)
    ; (let* () = Lwt_unix.sleep 2.0 in
       Lwt.return false)
    ]

let () =
  Lwt_main.run
    (let* repo = Tags_server.Store.create () in
     let* s = Tags_server.Store.session repo (sid "livepump") in
     let ctx = Tags_server.Store.ctx_of_session s in
     let replica = Crdt.Ctx.replica ctx in
     let guard =
       Dguard.v ~sessions:Guard.default_sessions ~tabs:Guard.default_tabs
         ~sink:Guard_sink.null ~floors:Floors.empty ()
     in
     let incoming, push = Lwt_stream.create () in
     let sent : string list ref = ref [] in
     let transport =
       { Tags_server.send_frame =
           (fun f ->
             sent := f :: !sent;
             Lwt.return_unit)
       ; receive_frame = (fun () -> Lwt_stream.get incoming)
       }
     in
     (* One-shot: only the first step is contended, so the ack count below is
        about the pump and not about a writer that kept firing. *)
     let pending = ref true in
     let interpose () =
       if !pending then (
         pending := false;
         let* foreign = Tags_server.Store.load_based s in
         let* (_ : Tags_server.Store.committed) =
           Tags_server.Store.commit_based foreign ~label:"beta"
             (fst
                (Tags_app.update ctx (Tags_app.Add_tag "beta")
                   (Tags_server.Store.based_model foreign)))
         in
         Lwt.return_unit)
       else Lwt.return_unit
     in
     let session = Tags_server.live_session ~guard ~interpose s transport in
     let acked_seq (f : string) : Msg_seq.t option =
       Option.bind
         (Result.fold (Codec.down_of_json f)
            ~error:(fun (_ : Tea_core.Codec.err) -> None)
            ~ok:(fun (d : Tags_app.model Wire.down) -> Some d))
         (fun (d : Tags_app.model Wire.down) ->
           match d with
           | Wire.Ack n -> Some n
           | Wire.Hello ((_ : Crdt.Replica.t), (_ : Tags_app.model)) -> None
           | Wire.Head (_ : Tags_app.model) -> None)
     in
     let acks () = List.filter_map acked_seq (List.rev !sent) in
     push
       (Some
          (Codec.up_to_json
             (Wire.Apply { tab = tab_hex; seq = 1; msg = Tags_app.Add_tag "alpha" })));
     (* A bounded poll for the ack to travel, not an ordering device: the
        interleave itself is [interpose], which is program order. *)
     let* delivered = await (fun () -> not (List.is_empty (acks ()))) in
     push None;
     let* closed = closes_within session in
     let* head = Tags_server.Store.load s in
     let* water = Tags_server.Store.head_water s in
     let floor = Floors.find_stamped ~replica ~tab:tab_id (Dguard.floors guard) in
     check "the pump still floors and acks the water of what landed"
       (delivered && closed
       && List.equal Int.equal (List.map Msg_seq.to_int (acks ())) [ 1 ]
       && has "alpha" head && has "beta" head
       && Option.fold floor ~none:false ~some:(fun ((n : Msg_seq.t), (w : Water.t)) ->
              Int.equal (Msg_seq.to_int n) 1 && Water.equal w water));
     Lwt.return_unit)

let () =
  Printf.printf
    "\n\
     The witnessed write path holds under contention: both writers survive \
     (C1-C6), the ancestor is re-based (C7), the declared arms are legible \
     (C8-C10), the pack round trip stands (C11-C12), the coalescer reconciles \
     (C13-C14), R10b is closed (C15-C17: undo, redo and fork refuse instead\n\
     of erasing), and the server seam carries it (S1-S2).\n\
     %!"
