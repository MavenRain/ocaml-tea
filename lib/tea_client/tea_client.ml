module Html = Tea_core.Html
module Cmd = Tea_core.Cmd
module Prim = Tea_core.Prim

type 'msg Vdom.Cmd.t +=
  | After of int * 'msg
  | Navigate of string
  | Http of
      { path : string  (* lowered at the vdom boundary, as [Navigate] lowers [Url] *)
      ; body : string
      ; expect : (string, Cmd.http_failure) result -> 'msg
      }

(* ["value"] must track the model on every redraw, so it crosses as the DOM
   *property* (an attribute would only seed the initial value and leave stale
   user edits visible). Everything else crosses as a plain attribute, exactly
   as [Render_static] prints it — the two tiers render one view one way. *)
let attr_to_vdom = function
  | Html.Attr (name, value) ->
    let n = Prim.Attr_name.to_string name in
    let v = Prim.Attr_value.to_string value in
    if String.equal n "value" then Vdom.value v else Vdom.attr n v
  (* [On_click] carries a constant Msg, so no event decoding is needed:
     [Decoder.const] both says exactly that and keeps the handler evaluable
     off the browser (the fidelity tests recover the Msg natively). *)
  | Html.On_click msg -> Vdom.on "click" (Vdom.Decoder.const (Some msg))
  | Html.On_input f -> Vdom.oninput (fun s -> f (Prim.Input_text.v s))

let rec html_to_vdom = function
  | Html.Text t -> Vdom.text (Prim.Text.to_string t)
  | Html.Element (tag, attrs, children) ->
    Vdom.elt
      (Prim.Tag.to_string tag)
      ~a:(List.map attr_to_vdom attrs)
      (List.map html_to_vdom children)

let rec cmd_to_vdom = function
  | Cmd.None_ -> Vdom.Cmd.Batch []
  | Cmd.Batch cmds -> Vdom.Cmd.Batch (List.map cmd_to_vdom cmds)
  | Cmd.Emit msg -> Vdom.Cmd.Echo msg
  | Cmd.After (delay, msg) -> After (Prim.Delay.to_ms delay, msg)
  | Cmd.Navigate url -> Navigate (Prim.Url.to_string url)
  | Cmd.Http { path; body; expect } ->
    Http { path = Prim.Rpc_path.to_string path; body; expect }

module Subs = struct
  type ('model, 'msg) spec =
    | Spec_every of int * (int -> 'msg)
    | Spec_store of ('model -> 'msg)

  let rec specs_of (s : ('model, 'msg) Tea_core.Sub.t) : ('model, 'msg) spec list =
    match s with
    | Tea_core.Sub.None_ -> []
    | Tea_core.Sub.Batch xs -> List.concat_map specs_of xs
    | Tea_core.Sub.Every (i, f) -> [ Spec_every (Prim.Interval.to_ms i, f) ]
    | Tea_core.Sub.Store_watch f -> [ Spec_store f ]

  type key =
    | Key_every of int
    | Key_store

  let key_of_spec (s : ('model, 'msg) spec) : key =
    match s with
    | Spec_every (ms, (_ : int -> 'msg)) -> Key_every ms
    | Spec_store (_ : 'model -> 'msg) -> Key_store

  let equal_key (a : key) (b : key) : bool =
    match (a, b) with
    | Key_every x, Key_every y -> Int.equal x y
    | Key_store, Key_store -> true
    | Key_every (_ : int), Key_store -> false
    | Key_store, Key_every (_ : int) -> false

  let keys_of (specs : ('model, 'msg) spec list) : key list =
    List.fold_left
      (fun acc s ->
        let k = key_of_spec s in
        if List.exists (equal_key k) acc then acc else k :: acc)
      [] specs
    |> List.rev

  let plan ~(active : key list) ~(wanted : key list) : key list * key list =
    ( List.filter (fun k -> not (List.exists (equal_key k) active)) wanted
    , List.filter (fun k -> not (List.exists (equal_key k) wanted)) active )
end

module Make (A : Tea_core.App.APP) = struct
  (* The client tab's single CRDT context (roadmap step 8, D1): one replica id
     for every locally-born edit, minting dots from a local monotonic clock.
     The wall source is [0] here (this half links natively and has no browser
     clock); server stamps are real wall-seconds and therefore dominate, so a
     pushed [Sync] head is authoritative on any LWW field (R6). A future phase
     (D8/D9, tea_client_run) seeds this clock from the browser and owns
     reconnect/rebase. *)
  let ctx =
    let clock = Tea_core.Clock.create ~now:(fun () -> 0L) in
    let replica = Tea_core.Crdt.Replica.v (Tea_core.Prim.Session_id.v "client") in
    Tea_core.Crdt.Ctx.v ~clock ~replica

  let app =
    let model, cmd = A.init in
    Vdom.app
      ~init:(model, cmd_to_vdom cmd)
      ~update:(fun model msg ->
        let model', cmd = A.update ctx msg model in
        (model', cmd_to_vdom cmd))
      ~view:(fun model -> html_to_vdom (A.view model))
      ()
end
