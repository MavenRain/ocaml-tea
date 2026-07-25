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

  type model =
    { title : string
    ; body : string
    ; likes : int
    ; tags : string list
    }

  let model_t =
    Repr.(
      record "doc" (fun title body likes tags -> { title; body; likes; tags })
      |+ field "title" string (fun m -> m.title)
      |+ field "body" string (fun m -> m.body)
      |+ field "likes" int (fun m -> m.likes)
      |+ field "tags" (list string) (fun m -> m.tags)
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

  let msg_t =
    Repr.(
      variant "msg" (fun set_title set_body like unlike add_tag remove_tag sync ->
        function
        | Set_title s -> set_title s
        | Set_body s -> set_body s
        | Like -> like
        | Unlike -> unlike
        | Add_tag t -> add_tag t
        | Remove_tag t -> remove_tag t
        | Sync_doc d -> sync d)
      |~ case1 "Set_title" string (fun s -> Set_title s)
      |~ case1 "Set_body" string (fun s -> Set_body s)
      |~ case0 "Like" Like
      |~ case0 "Unlike" Unlike
      |~ case1 "Add_tag" string (fun t -> Add_tag t)
      |~ case1 "Remove_tag" string (fun t -> Remove_tag t)
      |~ case1 "Sync_doc" model_t (fun d -> Sync_doc d)
      |> sealv)

  let init = ({ title = "Untitled"; body = ""; likes = 0; tags = [] }, Cmd.none)

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
