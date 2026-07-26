(** A collaboratively-edited document: the app that proves thesis T2
    (branch-per-session + three-way merge = built-in collaboration) with a
    {i real} {!Tea_core.Merge.record} policy, not the Counter's placeholder
    [Last_write_wins].

    Two sessions edit the same doc on their own Irmin branches and reconcile
    through [Store.merge_into] ([test/collab_test]). The merge is structural, so
    concurrent edits combine instead of clobbering:
    - [likes] is a {!Tea_core.Merge.counter}: concurrent likes {i sum};
    - [tags] is a {!Tea_core.Merge.set}: concurrent adds union, removes win;
    - [title]/[body] are {!Tea_core.Merge.atomic}: an edit on one side alone is
      taken, but two divergent free-text edits {i conflict} rather than silently
      dropping one (the R2 discipline).

    Like the Counter, this single definition is compiled to the native server
    ([Tea_server.Make]) and to the browser ([Tea_client_run.Start]). *)

module App = struct
  open Tea_core

  (** The typed RPC client builder for this app's one contract: [R.call]
      encodes a request with {!Shared_doc_rpc}'s codecs and returns a
      {!Tea_core.Cmd.http} whose reply the app folds back into a msg (DESIGN
      §8). Linking the same [Make] both tiers use is what makes name, path,
      and codec identical by construction. *)
  module R = Tea_rpc.Make (Shared_doc_rpc)

  type model =
    { title : string
    ; body : string
    ; likes : int
    ; tags : string list
    ; last_stats : Shared_doc_rpc.stats_resp option
        (** the most recent {!Shared_doc_rpc.Doc_stats} reply — a {e shared}
            field of the document, not a per-client one: it rides [model_t]
            below, so a click that recomputes it is committed and broadcast to
            every live editor exactly like a title or tag edit. [None] before
            the first round-trip; reset to [None] by a failed one. *)
    }

  let model_t =
    Repr.(
      record "doc" (fun title body likes tags last_stats ->
        { title; body; likes; tags; last_stats })
      |+ field "title" string (fun m -> m.title)
      |+ field "body" string (fun m -> m.body)
      |+ field "likes" int (fun m -> m.likes)
      |+ field "tags" (list string) (fun m -> m.tags)
      |+ field "last_stats" (option Shared_doc_rpc.stats_resp_t) (fun m -> m.last_stats)
      |> sealr)

  type msg =
    | Set_title of string
    | Set_body of string
    | Like
    | Unlike
    | Add_tag of string
    | Remove_tag of string
    | Sync_doc of model
        (** reconcile from a pushed store head (live view): the committed
            document is the authority *)
    | Request_stats
        (** ask the [Doc_stats] endpoint about the current title/body *)
    | Got_stats of (Shared_doc_rpc.stats_resp, Tea_rpc.error) result
        (** the reply delivered by {!R.call}'s continuation, folded into the
            shared [last_stats] field; lossily projected on the error side
            when the live mirror serialises it (see [stats_reply_t]) *)

  (* WHY THIS APP FOLDS AN RPC REPLY INTO THE REPLICATED MODEL.
     A [Doc_stats] call is server-read-only (the endpoint never writes the
     store), but this app CHOOSES to store its reply in [last_stats], a
     [model_t] field. The live-view runtime mirrors every locally-born msg up
     the socket, so [Got_stats] travels the wire, the server commits it, and
     the new head broadcasts to every editor: the recomputed stats become a
     {e shared} badge, updated on demand by whoever clicks — and, like any
     msg in this every-update-is-a-commit framework, the click adds a commit
     (which [History_count] counts and [undo] walks; step-6 coalescing is the
     answer to that volume). A genuinely per-client reply would need a
     client-local state channel this single-replicated-model framework does
     not yet have (DESIGN §8, deferred). Because [Got_stats] is serialised,
     [msg_t] must encode it; [{!Tea_rpc.error}] wraps the abstract
     {!Tea_core.Prim.Status.t} and has no faithful wire form, so the
     projection is deliberately lossy on the error side — a mirrored
     [Got_stats (Error (Transport ...))] reaches the server as the canonical
     [Error (Decode _)] below, collapsing WHICH transport failure occurred.
     Both tiers fold any [Error] to [last_stats = None], so the [Ok] side is
     wire-faithful and the shared badge never disagrees between tiers. *)
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

  let init =
    ( { title = "Untitled"; body = ""; likes = 0; tags = []; last_stats = None }
    , Cmd.none )

  let update msg model =
    match msg with
    | Set_title s -> ({ model with title = s }, Cmd.none)
    | Set_body s -> ({ model with body = s }, Cmd.none)
    | Like -> ({ model with likes = model.likes + 1 }, Cmd.none)
    | Unlike -> ({ model with likes = max 0 (model.likes - 1) }, Cmd.none)
    | Add_tag t ->
      let tags = if List.mem t model.tags then model.tags else t :: model.tags in
      ({ model with tags }, Cmd.none)
    | Remove_tag t ->
      ({ model with tags = List.filter (fun x -> not (String.equal x t)) model.tags }, Cmd.none)
    | Sync_doc d -> (d, Cmd.none)
    | Request_stats ->
      ( model
      , R.call Shared_doc_rpc.Doc_stats
          { Shared_doc_rpc.title = model.title; body = model.body }
          ~reply:(fun r -> Got_stats r) )
    | Got_stats r ->
      ( { model with
          last_stats =
            Result.fold r ~ok:Option.some ~error:(fun (_ : Tea_rpc.error) -> None)
        }
      , Cmd.none )

  let view model =
    let open Html in
    let tag_li t =
      li [ text t; button ~attrs:[ on_click (Remove_tag t) ] [ text " ×" ] ]
    in
    let add_tag_button t = button ~attrs:[ on_click (Add_tag t) ] [ text ("+ " ^ t) ] in
    div
      ~attrs:[ class_ "doc" ]
      [ h1 [ text "Shared document" ]
      ; div
          ~attrs:[ class_ "field" ]
          [ span [ text "Title: " ]
          ; input ~attrs:[ class_ "title"; value_ model.title; on_input (fun s -> Set_title s) ] ()
          ]
      ; div
          ~attrs:[ class_ "field" ]
          [ span [ text "Body: " ]
          ; input ~attrs:[ class_ "body"; value_ model.body; on_input (fun s -> Set_body s) ] ()
          ]
      ; div
          ~attrs:[ class_ "likes" ]
          [ button ~attrs:[ on_click Unlike ] [ text "-" ]
          ; span ~attrs:[ class_ "count" ] [ text (string_of_int model.likes ^ " likes") ]
          ; button ~attrs:[ on_click Like ] [ text "+" ]
          ]
      ; ul ~attrs:[ class_ "tags" ] (List.map tag_li model.tags)
      ; div ~attrs:[ class_ "add-tags" ] (List.map add_tag_button [ "urgent"; "review"; "done" ])
      ; div
          ~attrs:[ class_ "stats" ]
          [ button ~attrs:[ on_click Request_stats ] [ text "Stats" ]
          ; span
              ~attrs:[ class_ "stats-line" ]
              [ text
                  (Option.fold model.last_stats ~none:"no stats yet"
                     ~some:(fun (s : Shared_doc_rpc.stats_resp) ->
                       Printf.sprintf "title %d chars, %d words" s.title_len
                         s.word_count))
              ]
          ]
      ]

  let subscriptions _model = Sub.store_watch (fun m -> Sync_doc m)

  (* The real Three_way merge (T2): structural fields reconcile automatically;
     only divergent free-text edits surface as a conflict (R2). *)
  let merge =
    Merge.(
      to_spec
        (record
           [ field ~label:"title" ~get:(fun m -> m.title) ~set:(fun v m -> { m with title = v })
               (atomic ~eq:String.equal)
           ; field ~label:"body" ~get:(fun m -> m.body) ~set:(fun v m -> { m with body = v })
               (atomic ~eq:String.equal)
           ; field ~label:"likes" ~get:(fun m -> m.likes) ~set:(fun v m -> { m with likes = v })
               (map (max 0) counter)             (* clamp: [Unlike] floors likes at 0 per session, so the merge must
                too - else two concurrent unlikes of a 1-like doc reconcile to -1,
                a state no update can reach *)
           ; field ~label:"tags" ~get:(fun m -> m.tags) ~set:(fun v m -> { m with tags = v })
               (set ~cmp:String.compare)
           ]))

  let title = Prim.Title.v "Shared document"
  let url_of_model _model = None
  let msg_of_url _url = None
end

(* Left unsealed above so servers/clients/tests can name the Msg constructors,
   but statically proven to satisfy the framework contract: *)
module _ : Tea_core.App.APP = App
