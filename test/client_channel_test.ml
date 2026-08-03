(** Native proof of the client-local channel (roadmap step 8, D10 client half).

    Two obligations, and the second is the one that matters:

    1. a companion-claimed message changes only the local half and reports
       [Local_only], which is what stops [Tea_client_run] mirroring it up the
       live socket - a per-client RPC round trip must not become a peer's
       problem;
    2. the empty companion is a genuine identity, so [Tea_client_run.Start]
       being defined as [Start_local (A) (Local_none (A))] costs the plain
       mount nothing.

    Both are checked against [Local_channel.Make] directly: the vdom wrapper in
    [Tea_client.Make_local] adds nothing but a type change, and the browser
    adds nothing at all. *)

module Channel = Tea_client.Local_channel
module Cmd = Tea_core.Cmd
module Html = Tea_core.Html
module Prim = Tea_core.Prim
module App = Shared_doc_app.App
module Doc = Channel.Make (Shared_doc_app.App) (Shared_doc_app.Local)

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

(* One context for the whole file, exactly as a tab has one: distinct dots per
   edit, one replica id. *)
let ctx =
  let clock = Tea_core.Clock.create ~now:(fun () -> 0L) in
  let replica = Tea_core.Crdt.Replica.v (Prim.Session_id.v "channel-test") in
  Tea_core.Crdt.Ctx.v ~clock ~replica

(* [Repr.equal] is staged: unstage once here rather than at every call site. *)
let doc_eq : App.model -> App.model -> bool = Repr.unstage (Repr.equal App.model_t)

let is_local (s : Doc.step) =
  match s.Doc.channel with
  | Channel.Local_only -> true
  | Channel.Shared -> false

let is_local_channel (c : Channel.channel) =
  match c with
  | Channel.Local_only -> true
  | Channel.Shared -> false

(* Every text node of a view, so a readout can be looked for without a DOM. *)
let rec texts : type m. m Html.t -> string list =
 fun h ->
  match h with
  | Html.Text t -> [ Prim.Text.to_string t ]
  | Html.Element ((_ : Prim.Tag.t), (_ : m Html.attr list), children) ->
    List.concat_map texts children

let shows (h : 'm Html.t) (needle : string) : bool =
  List.exists (fun t -> String.equal t needle) (texts h)

let is_http_to (cmd : 'm Cmd.t) (path : string) : bool =
  match cmd with
  | Cmd.Http { path = p; delivery = (_ : Cmd.Http_delivery.t); body = (_ : string); expect = (_ : (string, Cmd.http_failure) result -> 'm) }
    -> String.equal (Prim.Rpc_path.to_string p) path
  | Cmd.None_ | Cmd.Batch (_ : 'm Cmd.t list) | Cmd.Emit (_ : 'm) -> false
  | Cmd.After ((_ : Prim.Delay.t), (_ : 'm)) -> false
  | Cmd.Navigate (_ : Prim.Url.t) -> false

(* --- the local half claims its own messages ------------------------------ *)

let () =
  print_endline "\n--- D10: the companion claims the per-client messages ---";
  let state0, (_ : App.msg Cmd.t) = Doc.init in
  let asked = Doc.update ctx App.Request_stats state0 in
  check "Request_stats is claimed by the companion" (is_local asked);
  check "Request_stats issues the RPC from the local half"
    (is_http_to asked.Doc.cmd "/rpc/doc_stats");
  check "a claimed message leaves the shared model physically untouched"
    (asked.Doc.state.Doc.shared == state0.Doc.shared);
  check "the local half records that the request is in flight"
    (shows (Doc.view asked.Doc.state) "asking...");
  let answered =
    Doc.update ctx
      (App.Got_stats (Ok { Shared_doc_rpc.title_len = 8; word_count = 3 }))
      asked.Doc.state
  in
  check "Got_stats is claimed too" (is_local answered);
  check "the reply is rendered into the local readout"
    (shows (Doc.view answered.Doc.state) "8 chars in the title, 3 words in the body");
  check "the reply raises no further command"
    (match answered.Doc.cmd with
    | Cmd.None_ -> true
    | Cmd.Batch (_ : App.msg Cmd.t list) -> false
    | Cmd.Emit (_ : App.msg) -> false
    | Cmd.After ((_ : Prim.Delay.t), (_ : App.msg)) -> false
    | Cmd.Navigate (_ : Prim.Url.t) -> false
    | Cmd.Http
        { delivery = (_ : Cmd.Http_delivery.t)
        ; path = (_ : Prim.Rpc_path.t)
        ; body = (_ : string)
        ; expect = (_ : (string, Cmd.http_failure) result -> App.msg)
        } -> false);
  check "an RPC failure degrades to a readable line, not a crash"
    (shows
       (Doc.view
          (Doc.update ctx
             (App.Got_stats (Error (Tea_rpc.Decode "bad json")))
             asked.Doc.state)
             .Doc.state)
       "stats unavailable (undecodable reply)");
  (* And the shared half is genuinely untouched by the whole exchange: the
     document a peer would see never learned any of it happened. *)
  check "no shared field moved across the full request/reply exchange"
    (doc_eq answered.Doc.state.Doc.shared (fst App.init))

(* --- the shared half still behaves exactly as before --------------------- *)

let () =
  print_endline "\n--- D10: declined messages fall through and are mirrored ---";
  let state0, (_ : App.msg Cmd.t) = Doc.init in
  let edited = Doc.update ctx (App.Set_title "Draft") state0 in
  check "a document edit is NOT claimed (so the runtime mirrors it)"
    (not (is_local edited));
  check "a declined message reaches App.update"
    (String.equal (App.title_of edited.Doc.state.Doc.shared) "Draft");
  check "a declined message leaves the local half alone"
    (shows (Doc.view edited.Doc.state) "no stats yet");
  check "the companion still sees the shared doc when it later claims"
    (is_http_to (Doc.update ctx App.Request_stats edited.Doc.state).Doc.cmd
       "/rpc/doc_stats");
  (* The view is the product's, not the app's: it shows the shared document
     AND the local readout in one tree. *)
  let both = Doc.update ctx App.Request_stats edited.Doc.state in
  check "one view renders both halves"
    (shows (Doc.view both.Doc.state) "Shared document"
    && shows (Doc.view both.Doc.state) "asking...")

(* --- a claimed message must not run App.update AT ALL --------------------- *)

(* The subtle failure this pins: a dispatch written with [Option.fold], whose
   [~none:] is a VALUE, evaluates the [A.update] branch even when the companion
   claims the message. The claimed step still returns the local state, so the
   shared model looks untouched and every check above still passes - but the
   discarded [A.update] has already minted a CRDT dot from the shared context,
   and every later edit is stamped one dot further along.

   So: claim a message [A.update] would have stamped ([Add_tag] mints a dot),
   then make a real edit that also stamps, and compare against a run where the
   claimed message never happened. The models agree iff no dot was stolen. *)
module Claim_tags = struct
  type shared = App.model
  type msg = App.msg
  type local = int

  let init = 0

  let update (m : msg) (_ : shared) (claimed : local) : (local * msg Cmd.t) option =
    match m with
    | App.Add_tag (_ : string) -> Some (claimed + 1, Cmd.none)
    | App.Set_title (_ : string)
    | App.Set_body (_ : string)
    | App.Like
    | App.Unlike
    | App.Remove_tag (_ : string)
    | App.Sync_doc (_ : App.model)
    | App.Request_stats
    | App.Got_stats (_ : (Shared_doc_rpc.stats_resp, Tea_rpc.error) result)
    | App.Publish_tag (_ : string)
    | App.Got_tag_count (_ : (int, Tea_rpc.error) result) -> None

  let view (shared : shared) (_ : local) : msg Html.t = App.view shared
end

module Claim = Channel.Make (Shared_doc_app.App) (Claim_tags)

let fresh_ctx label =
  Tea_core.Crdt.Ctx.v
    ~clock:(Tea_core.Clock.create ~now:(fun () -> 0L))
    ~replica:(Tea_core.Crdt.Replica.v (Prim.Session_id.v label))

let () =
  print_endline "\n--- D10: claiming SKIPS App.update, it does not merely discard it ---";
  let st0, (_ : App.msg Cmd.t) = Claim.init in
  let ctx_claimed = fresh_ctx "dots" in
  let claimed = Claim.update ctx_claimed (App.Add_tag "urgent") st0 in
  check "the tag message is claimed" (is_local_channel claimed.Claim.channel);
  check "no tag reached the shared document"
    (List.is_empty (App.tags_of claimed.Claim.state.Claim.shared));
  let after = Claim.update ctx_claimed (App.Set_title "Draft") claimed.Claim.state in
  (* The control: the same real edit on a context that never saw the claimed
     message. *)
  let control = Claim.update (fresh_ctx "dots") (App.Set_title "Draft") st0 in
  check "a claimed message consumes no CRDT dot (App.update never ran)"
    (doc_eq after.Claim.state.Claim.shared control.Claim.state.Claim.shared);
  check "the local half did count the claim"
    (after.Claim.state.Claim.local = 1)

(* --- the empty companion is an identity ---------------------------------- *)

module Counter = Counter_app.App
module Plain = Channel.Make (Counter_app.App) (Tea_core.Local.None_ (Counter_app.App))

let counter_eq : Counter.model -> Counter.model -> bool =
  Repr.unstage (Repr.equal Counter.model_t)

let () =
  print_endline "\n--- D10: Local_none is a true identity (Start parity) ---";
  let state0, cmd0 = Plain.init in
  let model0, app_cmd0 = Counter.init in
  check "init agrees with the app's own"
    (counter_eq state0.Plain.shared model0);
  check "init raises the app's own command"
    (match (cmd0, app_cmd0) with
    | Cmd.None_, Cmd.None_ -> true
    | ( ( Cmd.Batch (_ : Counter.msg Cmd.t list)
        | Cmd.Emit (_ : Counter.msg)
        | Cmd.After ((_ : Prim.Delay.t), (_ : Counter.msg))
        | Cmd.Navigate (_ : Prim.Url.t)
        | Cmd.Http
            { delivery = (_ : Cmd.Http_delivery.t)
        ; path = (_ : Prim.Rpc_path.t)
            ; body = (_ : string)
            ; expect = (_ : (string, Cmd.http_failure) result -> Counter.msg)
            }
        | Cmd.None_ )
      , (_ : Counter.msg Cmd.t) ) -> false);
  (* Drive the same message sequence through both and compare at every step:
     an identity that held only on the first message would be no identity. *)
  let msgs = [ Counter.Increment; Counter.Increment; Counter.Decrement; Counter.Increment ] in
  let ctx_plain =
    Tea_core.Crdt.Ctx.v
      ~clock:(Tea_core.Clock.create ~now:(fun () -> 0L))
      ~replica:(Tea_core.Crdt.Replica.v (Prim.Session_id.v "parity"))
  in
  let ctx_bare =
    Tea_core.Crdt.Ctx.v
      ~clock:(Tea_core.Clock.create ~now:(fun () -> 0L))
      ~replica:(Tea_core.Crdt.Replica.v (Prim.Session_id.v "parity"))
  in
  let agreed, all_shared =
    List.fold_left
      (fun ((state, bare, ok, shared) : Plain.state * Counter.model * bool * bool) msg ->
        let step = Plain.update ctx_plain msg state in
        let bare', (_ : Counter.msg Cmd.t) = Counter.update ctx_bare msg bare in
        ( step.Plain.state
        , bare'
        , ok && counter_eq step.Plain.state.Plain.shared bare'
        , shared
          &&
          match step.Plain.channel with
          | Channel.Shared -> true
          | Channel.Local_only -> false ))
      (state0, model0, true, true) msgs
    |> fun ((_ : Plain.state), (_ : Counter.model), ok, shared) -> (ok, shared)
  in
  check "every step agrees with the bare app, model for model" agreed;
  check "the empty companion claims nothing, so every msg is still mirrored"
    all_shared;
  let final =
    List.fold_left (fun st msg -> (Plain.update ctx_plain msg st).Plain.state) state0 msgs
  in
  check "the product's view is the app's view unchanged"
    (texts (Plain.view final) = texts (Counter.view final.Plain.shared))

let () =
  print_endline
    "\nD10 holds: per-client state lives in the companion and never crosses the \n\
     socket, the shared document is untouched by it, and the empty companion is \n\
     an identity so the plain mount is the same code path."
