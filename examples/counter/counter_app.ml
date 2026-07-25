(** The Counter: the smallest complete ocaml-tea app. Model/Msg/update/view are
    written once here and instantiated by both the native server
    ([Tea_server.Make (Counter_app.App)]) and the js_of_ocaml client
    ([Tea_client.Make (Counter_app.App)]). *)

module App = struct
  open Tea_core

  type model = { count : int }

  let model_t =
    Repr.(record "counter" (fun count -> { count }) |+ field "count" int (fun m -> m.count) |> sealr)

  type msg =
    | Increment
    | Decrement
    | Reset
    | Sync of int
        (** reconcile from a pushed store head (live view, roadmap step 3):
            the server's committed count is the authority *)

  let msg_t =
    Repr.(
      variant "msg" (fun increment decrement reset sync -> function
        | Increment -> increment
        | Decrement -> decrement
        | Reset -> reset
        | Sync n -> sync n)
      |~ case0 "Increment" Increment
      |~ case0 "Decrement" Decrement
      |~ case0 "Reset" Reset
      |~ case1 "Sync" int (fun n -> Sync n)
      |> sealv)

  let init = ({ count = 0 }, Cmd.none)

  let update msg model =
    match msg with
    | Increment -> ({ count = model.count + 1 }, Cmd.none)
    | Decrement -> ({ count = model.count - 1 }, Cmd.none)
    | Reset -> ({ count = 0 }, Cmd.none)
    | Sync n -> ({ count = n }, Cmd.none)

  let view model =
    let open Html in
    div
      ~attrs:[ class_ "counter" ]
      [ button ~attrs:[ on_click Decrement ] [ text "-" ]
      ; span ~attrs:[ class_ "count" ] [ text (string_of_int model.count) ]
      ; button ~attrs:[ on_click Increment ] [ text "+" ]
      ; button ~attrs:[ on_click Reset ] [ text "reset" ]
      ]

  let subscriptions _model = Sub.store_watch (fun m -> Sync m.count)
  let merge = Merge_spec.Last_write_wins
  let title = Prim.Title.v "Counter"
  let url_of_model _model = None
  let msg_of_url _url = None
end

(* Left unsealed above so servers/clients/tests can name the Msg constructors,
   but statically proven to satisfy the framework contract: *)
module _ : Tea_core.App.APP = App
