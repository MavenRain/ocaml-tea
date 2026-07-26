(** The Counter: the smallest complete ocaml-tea app. Model/Msg/update/view are
    written once here and instantiated by both the native server
    ([Tea_server.Make (Counter_app.App)]) and the js_of_ocaml client
    ([Tea_client.Make (Counter_app.App)]).

    Roadmap step 8 (D1) makes the count a state-based CRDT: a {!Tea_core.Crdt}
    PN-counter, so two replicas that increment concurrently on their own
    branches converge to the {i sum} under [merge], never clobbering one
    another. [Sync] joins a pushed store head into the local state (converge)
    instead of overwriting it. *)

module App = struct
  open Tea_core

  module Count = Crdt.Pn_counter

  type model = { count : Count.state }

  (** The observable count: the PN-counter's value. *)
  let value (m : model) : int = Count.value m.count

  let model_t =
    Repr.(record "counter" (fun count -> { count }) |+ field "count" Count.t (fun m -> m.count) |> sealr)

  type msg =
    | Increment
    | Decrement
    | Reset
    | Sync of model
        (** reconcile from a pushed store head (live view, roadmap step 3): join
            the committed state in rather than clobber, so an unconfirmed local
            increment is not lost *)

  let msg_t =
    Repr.(
      variant "msg" (fun increment decrement reset sync -> function
        | Increment -> increment
        | Decrement -> decrement
        | Reset -> reset
        | Sync m -> sync m)
      |~ case0 "Increment" Increment
      |~ case0 "Decrement" Decrement
      |~ case0 "Reset" Reset
      |~ case1 "Sync" model_t (fun m -> Sync m)
      |> sealv)

  let init = ({ count = Count.bottom }, Cmd.none)

  let update ctx msg model =
    let r = Crdt.Ctx.replica ctx in
    match msg with
    | Increment -> ({ count = Count.inc r model.count }, Cmd.none)
    | Decrement -> ({ count = Count.dec r model.count }, Cmd.none)
    | Reset -> ({ count = Count.reset r model.count }, Cmd.none)
    | Sync d -> ({ count = Count.join model.count d.count }, Cmd.none)

  let view model =
    let open Html in
    div
      ~attrs:[ class_ "counter" ]
      [ button ~attrs:[ on_click Decrement ] [ text "-" ]
      ; span ~attrs:[ class_ "count" ] [ text (string_of_int (value model)) ]
      ; button ~attrs:[ on_click Increment ] [ text "+" ]
      ; button ~attrs:[ on_click Reset ] [ text "reset" ]
      ]

  let subscriptions _model = Sub.store_watch (fun m -> Sync m)

  (* The CvRDT merge (D1): a per-field join, no ancestor. Concurrent increments
     on sibling branches sum; re-merging is idempotent. *)
  let merge =
    Merge_spec.crdt_join
      (Crdt.record
         [ Crdt.field ~get:(fun m -> m.count) ~set:(fun v (_ : model) -> { count = v }) ~join:Count.join ])

  let title = Prim.Title.v "Counter"
  let url_of_model _model = None
  let msg_of_url _url = None
end

(* Left unsealed above so servers/clients/tests can name the Msg constructors,
   but statically proven to satisfy the framework contract: *)
module _ : Tea_core.App.APP = App
