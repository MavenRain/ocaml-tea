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

  let view model =
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
          ; span
              ~attrs:[ class_ "stats-line" ]
              [ text "per-client stats land in a later phase" ]
          ]
      ]

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
