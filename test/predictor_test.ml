(** The client is a {i predictor}, not a replica (roadmap step 9, D14).

    D13's browser smoke test found what every in-process test had missed: one
    click on the acting tab displayed 2 while an observer tab displayed 1,
    because the locally-born msg was applied twice under two {i different}
    replica ids — optimistically on the client (the constant id ["client"]) and
    authoritatively on the server (the session branch's id) — and a
    [Pn_counter] join sums across replica slots.

    No in-process test could see it, because no in-process test ever ran both
    applications of one intent. This file does exactly that: it drives a real
    {!Tea_persist.Store} session for the server half and {!Tea_client.Identity}
    for the client half, and folds the pushed head back through the app's own
    subscription handler, which is the composition the browser exercises.

    Two checks below are {b characterization pins}, marked [PIN]: they record
    behaviour that is {i not} blessed (an edit made before the announcement
    keeps its provisional slot; the app's own join re-admits a prediction
    {!Tea_client.Rebase.absorb} shed). A change that improves either one turns
    the pin red, which is the alarm asking for this file to be revisited — the
    same discipline as [test/browser/smoke.mjs]'s [pin].

    Ordering is load-bearing: {!Tea_client.Identity} is page-global mutable
    state with no reset, so every check that needs a tab which has not yet been
    told who it is comes before the first [adopt]. *)

open Tea_core
module App = Counter_app.App
module Identity = Tea_client.Identity
module Rebase = Tea_client.Rebase
module Codec = Tea_core.Codec.Make (Counter_app.App)

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

(* --- 1. The identity of a tab nobody has spoken to yet -------------------- *)

let () =
  check "a fresh tab mints under the provisional replica" (Identity.is_provisional ());
  check "the provisional ctx carries the provisional replica"
    (Crdt.Replica.equal (Crdt.Ctx.replica (Identity.ctx ())) Identity.provisional)

(* --- 2. The mechanism, as plain CRDT algebra ------------------------------

   Two increments of one intent under two ids sum; under one id they are the
   same slot and [join]'s per-slot [max] reconciles them. That single asymmetry
   is the whole of D14 and the whole of its fix, so it is pinned here without
   any client, server or wire in the way. *)

let () =
  let module Count = Crdt.Pn_counter in
  let a = Crdt.Replica.v (Prim.Session_id.v "tier-a") in
  let b = Crdt.Replica.v (Prim.Session_id.v "tier-b") in
  let under r = Count.inc r Count.bottom in
  check "one intent applied under TWO replica ids joins to 2 (the D14 defect)"
    (Count.value (Count.join (under a) (under b)) = 2);
  check "one intent applied under ONE replica id joins to 1 (the D14 fix)"
    (Count.value (Count.join (under a) (under a)) = 1)

(* --- 3. The full loop: optimistic apply, server apply, pushed head --------- *)

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let module Store = Tea_persist.Store.Make (App) in
     let* repo = Store.create () in
     let sid name = Option.get (Prim.Session_id.of_string name) in
     let click ctx model =
       let model', (_ : App.msg Cmd.t) = App.update ctx App.Increment model in
       model'
     in
     let sync ctx head model =
       let model', (_ : App.msg Cmd.t) = App.update ctx (App.Sync head) model in
       model'
     in
     (* (a) The pre-announcement window. A tab that acts before its [Hello]
        arrives has no way to know which slot to predict into, so it mints
        under the provisional replica and the old double count stands for that
        edit alone. Pinned, not blessed: closing this window needs a way to
        un-mint a dot, which {!Tea_client.Rebase.absorb} documents as
        unavailable ([Lww] has no previous value to revert to). *)
     let* pre = Store.session repo (sid "preannounce") in
     let local_pre = click (Identity.ctx ()) (fst App.init) in
     let* head_pre = Store.apply pre App.Increment in
     let folded_pre, (_ : Crdt.Replica.t option) =
       Rebase.absorb App.merge ~local:(Some local_pre) (Wire.Head head_pre)
     in
     check "PIN: an edit made before the announcement keeps its provisional slot"
       (App.value (sync (Identity.ctx ()) folded_pre local_pre) = 2);
     (* (b) The announcement, and the fix. The replica is taken from the
        session's own context — the same value {!Tea_server}'s [Hello] carries
        and the same one its steps apply under. *)
     let* s = Store.session repo (sid "announced") in
     let announced = Crdt.Ctx.replica (Store.ctx_of_session s) in
     Identity.adopt announced;
     check "adopting the announcement clears the provisional identity"
       (not (Identity.is_provisional ()));
     check "the adopted ctx mints under the announced replica"
       (Crdt.Replica.equal (Crdt.Ctx.replica (Identity.ctx ())) announced);
     let local = click (Identity.ctx ()) (fst App.init) in
     check "the optimistic click shows 1 before any reply comes back"
       (App.value local = 1);
     let* head = Store.apply s App.Increment in
     check "the server's own state counts that one intent once" (App.value head = 1);
     let head', adopted = Rebase.absorb App.merge ~local:(Some local) (Wire.Head head) in
     check "a Head frame announces no replica" (Option.is_none adopted);
     let displayed = sync (Identity.ctx ()) head' local in
     check "the ACTING tab settles at 1, not 2 (D14 fixed)" (App.value displayed = 1);
     (* An observer tab is the control: same head, no local action. Before the
        fix this read half the acting tab. *)
     let observer = sync (Identity.ctx ()) (fst (Rebase.absorb App.merge
                                                   ~local:(Some (fst App.init))
                                                   (Wire.Head head)))
         (fst App.init)
     in
     check "the acting tab and the observer tab agree"
       (App.value observer = App.value displayed);
     (* The max is a reconciliation, not a clamp: a second intent still moves. *)
     let local2 = click (Identity.ctx ()) displayed in
     let* head2 = Store.apply s App.Increment in
     let head2', (_ : Crdt.Replica.t option) =
       Rebase.absorb App.merge ~local:(Some local2) (Wire.Head head2)
     in
     check "a second click settles at 2 (the shared slot is not clamped)"
       (App.value (sync (Identity.ctx ()) head2' local2) = 2);
     (* (c) A [Hello] resync: an unconfirmed local edit (value 3 against a
        server head of 2) is shed by [absorb]… *)
     let ahead = click (Identity.ctx ()) (sync (Identity.ctx ()) head2' local2) in
     let resynced, adopted2 =
       Rebase.absorb App.merge ~local:(Some ahead) (Wire.Hello (announced, head2))
     in
     check "a Hello hands back a replica to adopt"
       (Option.fold ~none:false ~some:(Crdt.Replica.equal announced) adopted2);
     check "a Hello resyncs to the announced head, shedding the prediction"
       (App.value resynced = 2);
     (* …and re-admitted by the app's own sync handler, which joins. Under a
        shared replica id that is right (the shed edit is in flight and will
        land in the same slot), but it is also why (a) cannot be closed here. *)
     check "PIN: the app's own join re-admits the shed prediction (no un-mint)"
       (App.value (sync (Identity.ctx ()) resynced ahead) = 3);
     Printf.printf
       "\nThe client predicts the server's replica; one intent, one slot (D14).\n%!";
     Lwt.return_unit)

(* --- 4. [absorb] as a total function, off any tier ------------------------ *)

let () =
  let head_only (d : App.model Wire.down) = fst (Rebase.absorb App.merge ~local:None d) in
  let m1 = { App.count = Crdt.Pn_counter.inc (Crdt.Replica.v (Prim.Session_id.v "x")) Crdt.Pn_counter.bottom } in
  check "before the app mounts a Head passes straight through"
    (App.value (head_only (Wire.Head m1)) = 1);
  check "before the app mounts a Hello passes its head straight through"
    (App.value (head_only (Wire.Hello (Identity.provisional, m1))) = 1);
  (* The D9 property this phase must not regress: a Head is reconciled onto the
     local model, so a local edit the head does not carry survives it. *)
  let local = { App.count = Crdt.Pn_counter.inc (Crdt.Replica.v (Prim.Session_id.v "y")) Crdt.Pn_counter.bottom } in
  let kept, (_ : Crdt.Replica.t option) =
    Rebase.absorb App.merge ~local:(Some local) (Wire.Head m1)
  in
  check "a Head is reconciled onto the local model (D9 outbox edit survives)"
    (App.value kept = 2);
  let shed, (_ : Crdt.Replica.t option) =
    Rebase.absorb App.merge ~local:(Some local) (Wire.Hello (Identity.provisional, m1))
  in
  check "a Hello ignores the local model entirely" (App.value shed = 1)

(* --- 5. The wire: two frame kinds that cannot be confused ----------------- *)

let () =
  let m = { App.count = Crdt.Pn_counter.inc (Crdt.Replica.v (Prim.Session_id.v "z")) Crdt.Pn_counter.bottom } in
  let r = Crdt.Replica.v (Prim.Session_id.v "session-branch") in
  let round (d : App.model Wire.down) : App.model Wire.down option =
    Codec.down_of_json (Codec.down_to_json d)
    |> Result.fold ~error:(fun (_ : Codec.err) -> None) ~ok:(fun x -> Some x)
  in
  let as_hello (d : App.model Wire.down option) =
    Option.bind d (fun d ->
        match d with
        | Wire.Hello (r, m) -> Some (r, App.value m)
        | Wire.Head (_ : App.model) -> None)
  in
  let as_head (d : App.model Wire.down option) =
    Option.bind d (fun d ->
        match d with
        | Wire.Head m -> Some (App.value m)
        | Wire.Hello ((_ : Crdt.Replica.t), (_ : App.model)) -> None)
  in
  check "a Hello round-trips with its replica and head intact"
    (as_hello (round (Wire.Hello (r, m)))
     |> Option.fold ~none:false ~some:(fun (r', v) -> Crdt.Replica.equal r r' && v = 1));
  check "a Head round-trips" (as_head (round (Wire.Head m)) = Some 1);
  check "a Hello does not decode as a Head" (Option.is_none (as_head (round (Wire.Hello (r, m)))));
  check "a Head does not decode as a Hello" (Option.is_none (as_hello (round (Wire.Head m))));
  (* A pre-D14 server sent the bare model. The client must reject that rather
     than treat an unannounced frame as a head, or the wire change would
     degrade silently instead of failing loudly. *)
  let refuses (s : string) : bool =
    Codec.down_of_json s
    |> Result.fold ~error:(fun (_ : Codec.err) -> true)
         ~ok:(fun (_ : App.model Wire.down) -> false)
  in
  check "a bare model frame is no longer a valid down-frame"
    (refuses (Codec.model_to_json m));
  (* Found by the check above: [Repr.of_json_string] does not merely answer
     [Error] for an object naming an unknown variant case, it RAISES — so a
     decode of untrusted input was only total by luck. {!Tea_core.Codec.of_json}
     now catches it, and these pin that, on both the frame witness and the msg
     witness the ws pump and the form post feed from. *)
  check "an unknown variant case is refused, not raised" (refuses {|{"bogus":1}|});
  check "a bare JSON array is refused" (refuses {|[]|});
  check "a msg decode is total against an unknown case too"
    (Codec.msg_of_json {|{"Bogus":1}|}
     |> Result.fold ~error:(fun (_ : Codec.err) -> true) ~ok:(fun (_ : App.msg) -> false))

(* --- 6. The page clock outlives the identity ------------------------------

   Last, because it adopts a replica nothing else should see. *)

let () =
  let before = Crdt.Ctx.dot (Identity.ctx ()) in
  Identity.adopt (Crdt.Replica.v (Prim.Session_id.v "a-later-session"));
  let after = Crdt.Ctx.dot (Identity.ctx ()) in
  check "adopting a new replica does not reset the page clock"
    (Int64.compare after.Crdt.Dot.stamp before.Crdt.Dot.stamp > 0);
  Printf.printf "\nOne tab, one clock, whichever replica it is speaking as.\n%!"
