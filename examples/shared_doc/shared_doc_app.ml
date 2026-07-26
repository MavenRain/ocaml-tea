(** A collaboratively-edited document: the app that proves thesis T2
    (branch-per-session + convergent merge = built-in collaboration). Roadmap
    step 8 (D1) rebuilds every replicated field as a state-based CRDT
    ({!Tea_core.Crdt}), so concurrent edits {i converge} with no ancestor and no
    conflict (Strong Eventual Consistency):
    - [likes] is a {!Tea_core.Crdt.Pn_counter}: concurrent likes {i sum};
    - [tags] is a {!Tea_core.Crdt.Or_set} (observed-remove, add-wins): concurrent
      adds both survive, and an add concurrent with a remove wins;
    - [title]/[body] are {!Tea_core.Crdt.Lww} registers: two concurrent free-text
      edits resolve deterministically to the one with the higher [(stamp,
      Session_id)] dot (the documented residual: LWW silently drops the loser;
      char-level RGA is the out-of-scope escape hatch).

    D10 (shared half) removes the old shared [last_stats] field from the
    replicated model: an RPC reply is genuinely per-client, and its home is the
    client-local companion module (a later phase). [Request_stats]/[Got_stats]
    stay in the msg type but are now identity on the shared model.

    Like the Counter, this single definition is compiled to the native server
    ([Tea_server.Make]) and to the browser ([Tea_client_run.Start]). *)

module App = struct
  open Tea_core

  (** The typed RPC client builder for this app's one contract (DESIGN §8). *)
  module R = Tea_rpc.Make (Shared_doc_rpc)

  module Text = Crdt.Lww (struct
    type t = string

    let t = Repr.string
  end)

  module Tags = Crdt.Or_set (struct
    type t = string

    let compare = String.compare
    let t = Repr.string
  end)

  module Likes = Crdt.Pn_counter

  type model =
    { title : Text.state
    ; body : Text.state
    ; likes : Likes.state
    ; tags : Tags.state
    }

  (* Observable projections. [likes_of] floors the PN-counter value at 0 so a
     net-negative (two concurrent Unlikes of a 1-like doc) reads as 0, a state
     the UI can display; the raw CRDT value is left signed so the counter stays
     a proper grow-only-halves lattice. *)
  let title_of (m : model) : string = Text.value m.title
  let body_of (m : model) : string = Text.value m.body
  let likes_of (m : model) : int = max 0 (Likes.value m.likes)
  let tags_of (m : model) : string list = Tags.value m.tags

  let model_t =
    Repr.(
      record "doc" (fun title body likes tags -> { title; body; likes; tags })
      |+ field "title" Text.t (fun m -> m.title)
      |+ field "body" Text.t (fun m -> m.body)
      |+ field "likes" Likes.t (fun m -> m.likes)
      |+ field "tags" Tags.t (fun m -> m.tags)
      |> sealr)

  type msg =
    | Set_title of string
    | Set_body of string
    | Like
    | Unlike
    | Add_tag of string
    | Remove_tag of string
    | Sync_doc of model
        (** reconcile from a pushed store head (live view): join every field in,
            so an unconfirmed local edit is not clobbered *)
    | Request_stats
        (** ask the [Doc_stats] endpoint about the current title/body *)
    | Got_stats of (Shared_doc_rpc.stats_resp, Tea_rpc.error) result
        (** the reply delivered by {!R.call}'s continuation. D10 shared-half:
            the reply is genuinely per-client, and the shared model has no field
            for it, so this is identity here; the client-local companion that
            actually displays it lands in a later phase *)

  (* [Got_stats] is still serialised (it is a total msg), so [msg_t] must encode
     it; [Tea_rpc.error] wraps the abstract Status and has no faithful wire form,
     so the projection is deliberately lossy on the error side — any transport
     error round-trips as the canonical [Decode _]. *)
  let stats_reply_t : (Shared_doc_rpc.stats_resp, Tea_rpc.error) result Repr.t =
    Repr.map
      (Repr.option Shared_doc_rpc.stats_resp_t)
      (Option.fold
         ~none:(Error (Tea_rpc.Decode "rpc reply has no wire form"))
         ~some:(fun s -> Ok s))
      (Result.fold ~ok:Option.some ~error:(fun (_ : Tea_rpc.error) -> None))

  let msg_t =
    Repr.(
      variant "msg"
        (fun set_title set_body like unlike add_tag remove_tag sync request_stats
             got_stats ->
        function
        | Set_title s -> set_title s
        | Set_body s -> set_body s
        | Like -> like
        | Unlike -> unlike
        | Add_tag t -> add_tag t
        | Remove_tag t -> remove_tag t
        | Sync_doc d -> sync d
        | Request_stats -> request_stats
        | Got_stats r -> got_stats r)
      |~ case1 "Set_title" string (fun s -> Set_title s)
      |~ case1 "Set_body" string (fun s -> Set_body s)
      |~ case0 "Like" Like
      |~ case0 "Unlike" Unlike
      |~ case1 "Add_tag" string (fun t -> Add_tag t)
      |~ case1 "Remove_tag" string (fun t -> Remove_tag t)
      |~ case1 "Sync_doc" model_t (fun d -> Sync_doc d)
      |~ case0 "Request_stats" Request_stats
      |~ case1 "Got_stats" stats_reply_t (fun r -> Got_stats r)
      |> sealv)

  let init : model * msg Cmd.t =
    ( { title = Text.bottom "Untitled"; body = Text.bottom ""; likes = Likes.bottom; tags = Tags.bottom }
    , Cmd.none )

  let update ctx msg model =
    let r = Crdt.Ctx.replica ctx in
    match msg with
    | Set_title s -> ({ model with title = Text.set (Crdt.Ctx.dot ctx) s model.title }, Cmd.none)
    | Set_body s -> ({ model with body = Text.set (Crdt.Ctx.dot ctx) s model.body }, Cmd.none)
    | Like -> ({ model with likes = Likes.inc r model.likes }, Cmd.none)
    | Unlike -> ({ model with likes = Likes.dec r model.likes }, Cmd.none)
    | Add_tag t -> ({ model with tags = Tags.add (Crdt.Ctx.dot ctx) t model.tags }, Cmd.none)
    | Remove_tag t -> ({ model with tags = Tags.remove t model.tags }, Cmd.none)
    | Sync_doc d ->
      ( { title = Text.join model.title d.title
        ; body = Text.join model.body d.body
        ; likes = Likes.join model.likes d.likes
        ; tags = Tags.join model.tags d.tags
        }
      , Cmd.none )
    | Request_stats ->
      ( model
      , R.call Shared_doc_rpc.Doc_stats
          { Shared_doc_rpc.title = title_of model; body = body_of model }
          ~reply:(fun reply -> Got_stats reply) )
    | Got_stats (_ : (Shared_doc_rpc.stats_resp, Tea_rpc.error) result) -> (model, Cmd.none)

  (* The one seam the D10 companion needs: everything about the page is shared
     except the stats readout, which is per-client. Parameterising the view
     here (rather than letting the companion rebuild the page) keeps ONE view
     definition  -  a second copy would drift, and the point of D10 is that local
     state costs an extra argument, not an extra UI. *)
  let view_with ~(stats_line : msg Html.t) model =
    let open Html in
    let tag_li t = li [ text t; button ~attrs:[ on_click (Remove_tag t) ] [ text " ×" ] ] in
    let add_tag_button t = button ~attrs:[ on_click (Add_tag t) ] [ text ("+ " ^ t) ] in
    div
      ~attrs:[ class_ "doc" ]
      [ h1 [ text "Shared document" ]
      ; div
          ~attrs:[ class_ "field" ]
          [ span [ text "Title: " ]
          ; input ~attrs:[ class_ "title"; value_ (title_of model); on_input (fun s -> Set_title s) ] ()
          ]
      ; div
          ~attrs:[ class_ "field" ]
          [ span [ text "Body: " ]
          ; input ~attrs:[ class_ "body"; value_ (body_of model); on_input (fun s -> Set_body s) ] ()
          ]
      ; div
          ~attrs:[ class_ "likes" ]
          [ button ~attrs:[ on_click Unlike ] [ text "-" ]
          ; span ~attrs:[ class_ "count" ] [ text (string_of_int (likes_of model) ^ " likes") ]
          ; button ~attrs:[ on_click Like ] [ text "+" ]
          ]
      ; ul ~attrs:[ class_ "tags" ] (List.map tag_li (tags_of model))
      ; div ~attrs:[ class_ "add-tags" ] (List.map add_tag_button [ "urgent"; "review"; "done" ])
      ; div
          ~attrs:[ class_ "stats" ]
          [ button ~attrs:[ on_click Request_stats ] [ text "Stats" ]
          ; span ~attrs:[ class_ "stats-line" ] [ stats_line ]
          ]
      ]

  (* The [APP] view: no companion, so no stats to show. A tab mounted with
     {!Local} below overrides exactly this one node. *)
  let view model =
    view_with ~stats_line:(Html.text "stats are a per-client readout") model

  let subscriptions _model = Sub.store_watch (fun m -> Sync_doc m)

  (* The CvRDT merge (D1): a per-field least-upper-bound, no ancestor. Every
     replicated field owns a join, so concurrent edits converge with no
     conflict — the SEC discipline replacing the step-4 three-way combinators. *)
  (* The model-wide CvRDT least-upper-bound: one join per field, no ancestor.
     Named because D6 needs the very same function twice — once as the merge
     policy, once as the exploded tree's reassembly operator. *)
  let doc_join =
    Crdt.record
      [ Crdt.field ~get:(fun m -> m.title) ~set:(fun v m -> { m with title = v }) ~join:Text.join
      ; Crdt.field ~get:(fun m -> m.body) ~set:(fun v m -> { m with body = v }) ~join:Text.join
      ; Crdt.field ~get:(fun m -> m.likes) ~set:(fun v m -> { m with likes = v }) ~join:Likes.join
      ; Crdt.field ~get:(fun m -> m.tags) ~set:(fun v m -> { m with tags = v }) ~join:Tags.join
      ]

  let merge = Merge_spec.crdt_join doc_join

  (** The lattice bottom this app reassembles from: exactly its initial model,
      so a field no session has ever written re-reads as its {!init} default
      rather than as some second, subtly different "empty". *)
  let bottom : model = fst init

  (** The D6 exploded-tree layout: one store path per CRDT field, paired with
      the projection isolating that field ([bottom] everywhere else). Exported
      as ingredients rather than as a finished witness because the witness type
      belongs to an instantiated store, which the app tier does not have. The
      server (or a test) feeds these to [Store.exploder]. *)
  let explode_fields : (string * (model -> model)) list =
    [ ("title", fun m -> { bottom with title = m.title })
    ; ("body", fun m -> { bottom with body = m.body })
    ; ("likes", fun m -> { bottom with likes = m.likes })
    ; ("tags", fun m -> { bottom with tags = m.tags })
    ]

  let title = Prim.Title.v "Shared document"
  let url_of_model _model = None
  let msg_of_url _url = None
end

(* Left unsealed above so servers/clients/tests can name the Msg constructors,
   but statically proven to satisfy the framework contract: *)
module _ : Tea_core.App.APP = App

(** The client-local companion (roadmap step 8, D10 client half): the home the
    stats readout never had.

    D1 removed [last_stats] from the replicated model, because an RPC reply
    computed from {i this} tab's title and body is not a fact about the
    document  -  replicating it would push one user's readout into every peer's
    view and demand a CRDT join for a value that has no meaningful one. The
    intervening phases left [Request_stats]/[Got_stats] as identity on the
    shared model, which is to say: the button did nothing. This module is where
    they land instead.

    Both messages are claimed, so neither crosses the live socket: the request
    is issued from here (the companion can read the shared doc, which is all
    {!Shared_doc_rpc.Doc_stats} needs), and the reply is consumed here. A peer
    never learns that this tab asked. *)
module Local = struct
  (* Aliases rather than [open Tea_core]: the framework has an [App] module of
     its own, and opening it here would shadow this file's [App] with a
     signature that has no [model]. *)
  module Cmd = Tea_core.Cmd
  module Html = Tea_core.Html

  type shared = App.model
  type msg = App.msg

  (** The readout as displayed, plus nothing else. Deliberately a rendered
      string rather than a [stats_resp option]: there is no state machine here
      to get wrong, and "asked but not answered" is a third display value that
      an [option] would have to encode as [None] alongside "never asked". *)
  type local = { stats_line : string }

  let init = { stats_line = "no stats yet" }

  let rendered (reply : (Shared_doc_rpc.stats_resp, Tea_rpc.error) result) : string =
    Result.fold reply
      ~ok:(fun (s : Shared_doc_rpc.stats_resp) ->
        Printf.sprintf "%d chars in the title, %d words in the body" s.title_len
          s.word_count)
      ~error:(fun (e : Tea_rpc.error) ->
        match e with
        | Tea_rpc.Transport (_ : Cmd.http_failure) -> "stats unavailable (transport)"
        | Tea_rpc.Decode (_ : string) -> "stats unavailable (undecodable reply)")

  (* The prior [local] is never read: every claimed msg replaces the readout
     outright, so this companion is a function of the message alone. A
     companion that accumulated (a request counter, a history) would use it. *)
  let update (m : msg) (shared : shared) (_ : local) : (local * msg Cmd.t) option =
    match m with
    | App.Request_stats ->
      Some
        ( { stats_line = "asking..." }
        , App.R.call Shared_doc_rpc.Doc_stats
            { Shared_doc_rpc.title = App.title_of shared; body = App.body_of shared }
            ~reply:(fun reply -> App.Got_stats reply) )
    | App.Got_stats reply -> Some ({ stats_line = rendered reply }, Cmd.none)
    (* Every shared edit declines: it belongs to the document, and declining is
       what forwards it to peers. Listed constructor by constructor so a new
       Msg is a compile error here, not a silently unmirrored edit. *)
    | App.Set_title (_ : string)
    | App.Set_body (_ : string)
    | App.Like
    | App.Unlike
    | App.Add_tag (_ : string)
    | App.Remove_tag (_ : string)
    | App.Sync_doc (_ : App.model) -> None

  let view (shared : shared) (local : local) : msg Html.t =
    App.view_with ~stats_line:(Html.text local.stats_line) shared
end

module _ : Tea_core.Local.LOCAL with type shared = App.model and type msg = App.msg =
  Local
