(** The shared fuel-exhaustion fixture: the smallest APP whose update can
    exhaust fuel. [Spin]'s command re-emits [Spin], so {!Tea_core.Loop}'s
    interpreter burns its whole budget and [step] returns
    [Error Fuel_exhausted]; [Bump] is the ordinary message that settles at
    once, for the ok arm. One copy, linked into every test executable like
    [Test_util]: [fuel_durable_test] pins the durable fuel arm with it, and
    [cancel_test] drives the same arm under cancellation. *)

open Tea_core

type model = int

type msg =
  | Bump  (** apply: model + 1, command tail settles immediately *)
  | Spin  (** the fuel poison: its reply re-emits itself forever *)

let model_t = Repr.int

let msg_t =
  Repr.(
    variant "msg" (fun bump spin ->
      function
      | Bump -> bump
      | Spin -> spin)
    |~ case0 "Bump" Bump
    |~ case0 "Spin" Spin
    |> sealv)

let init = (0, Cmd.none)

let update (_ : Crdt.Ctx.t) (msg : msg) (m : model) =
  match msg with
  | Bump -> (m + 1, Cmd.none)
  | Spin -> (m, Cmd.emit Spin)

let view (m : model) = Html.text (string_of_int m)
let subscriptions (_ : model) = Sub.none
let merge = Merge.(to_spec (atomic ~eq:Int.equal))
let title = Prim.Title.v "fuel-probe"
let url_of_model (_ : model) = None
let msg_of_url (_ : Prim.Url.t) = None
