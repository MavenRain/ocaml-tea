module Html = Tea_core.Html
module Cmd = Tea_core.Cmd
module Prim = Tea_core.Prim

type 'msg Vdom.Cmd.t +=
  | After of int * 'msg
  | Navigate of string

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

module Make (A : Tea_core.App.APP) = struct
  let app =
    let model, cmd = A.init in
    Vdom.app
      ~init:(model, cmd_to_vdom cmd)
      ~update:(fun model msg ->
        let model', cmd = A.update msg model in
        (model', cmd_to_vdom cmd))
      ~view:(fun model -> html_to_vdom (A.view model))
      ()
end
