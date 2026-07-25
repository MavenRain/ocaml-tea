(** Native proof of the client tier's pure half (roadmap step 2): the
    [Html.t → vdom] and [Cmd.t → Vdom.Cmd.t] translations are checked
    structurally, and click-handler fidelity is checked by {e evaluating} the
    handler's decoder off the browser — [On_click] translates to a
    [Const]/[Bind]-shaped decoder by construction, so the exact Msg is
    recoverable without a DOM. *)

module App = Counter_app.App
module Html = Tea_core.Html
module Cmd = Tea_core.Cmd
module Prim = Tea_core.Prim

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

(* --- evaluating decoders natively ---------------------------------------- *)

(* The browser-independent fragment of the decoder language: [Const] and the
   binds/tries built on it evaluate to a value; every constructor that needs a
   live JS object evaluates to [None]. *)
let rec eval : type a. a Vdom.Decoder.t -> a option = function
  | Vdom.Decoder.Const x -> Some x
  | Vdom.Decoder.Bind (f, d) -> Option.bind (eval d) (fun a -> eval (f a))
  | Vdom.Decoder.Try d -> Some (eval d)
  | Vdom.Decoder.Fail _ -> None
  | Vdom.Decoder.Field (_, _) -> None
  | Vdom.Decoder.Method (_, _, _) -> None
  | Vdom.Decoder.Factor _ -> None
  | Vdom.Decoder.String -> None
  | Vdom.Decoder.Int -> None
  | Vdom.Decoder.Float -> None
  | Vdom.Decoder.Bool -> None
  | Vdom.Decoder.Object -> None
  | Vdom.Decoder.List _ -> None

let handler_msg (type msg) (event : string) (h : msg Vdom.event_handler) :
    msg option =
  match h with
  | Vdom.Decoder { event_type; decoder; map = to_msg } ->
    if String.equal event_type event then
      Option.bind (eval decoder) (fun opts -> to_msg opts.Vdom.msg)
    else None
  | Vdom.CustomEvent _ -> None

let attr_msg event = function
  | Vdom.Handler h -> handler_msg event h
  | Vdom.Property (_, _) -> None
  | Vdom.Style (_, _) -> None
  | Vdom.Attribute (_, _) -> None

let click_msg attrs = List.find_map (attr_msg "click") attrs

let is_handler_for event = function
  | Vdom.Handler (Vdom.Decoder { event_type; decoder = _; map = _ }) ->
    String.equal event_type event
  | Vdom.Handler (Vdom.CustomEvent _) -> false
  | Vdom.Property (_, _) -> false
  | Vdom.Style (_, _) -> false
  | Vdom.Attribute (_, _) -> false

(* --- walking the vdom ----------------------------------------------------- *)

(* Our translation only ever emits [Element] and [Text]: the two classifiers
   below carry the one exhaustive match each, and every accessor is an
   [Option] projection off them. *)
let as_element = function
  | Vdom.Element { tag; attributes; children; key = _; ns = _ } ->
    Some (tag, attributes, children)
  | Vdom.Text _ -> None
  | Vdom.Fragment _ -> None
  | Vdom.Map _ -> None
  | Vdom.Memo _ -> None
  | Vdom.Custom _ -> None

let text_of = function
  | Vdom.Text { txt; key = _ } -> Some txt
  | Vdom.Element _ -> None
  | Vdom.Fragment _ -> None
  | Vdom.Map _ -> None
  | Vdom.Memo _ -> None
  | Vdom.Custom _ -> None

let child i v =
  Option.bind (as_element v) (fun (_, _, children) -> List.nth_opt children i)

let dig path v = List.fold_left (fun acc i -> Option.bind acc (child i)) (Some v) path
let tag_of v = Option.map (fun (tag, _, _) -> tag) (as_element v)

let attrs_of v =
  Option.fold ~none:[] ~some:(fun (_, attributes, _) -> attributes) (as_element v)

let child_count v =
  Option.fold ~none:0 ~some:(fun (_, _, children) -> List.length children)
    (as_element v)

let has_attribute name value attrs =
  List.exists
    (function
      | Vdom.Attribute (n, v) -> String.equal n name && String.equal v value
      | Vdom.Property (_, _) -> false
      | Vdom.Style (_, _) -> false
      | Vdom.Handler _ -> false)
    attrs

let has_string_property name value attrs =
  List.exists
    (function
      | Vdom.Property (n, Vdom.String v) ->
        String.equal n name && String.equal v value
      | Vdom.Property (_, Vdom.Int _) -> false
      | Vdom.Property (_, Vdom.Float _) -> false
      | Vdom.Property (_, Vdom.Bool _) -> false
      | Vdom.Attribute (_, _) -> false
      | Vdom.Style (_, _) -> false
      | Vdom.Handler _ -> false)
    attrs

(* [Vdom.Cmd.t] is an open (extensible) type, so these classifiers carry the
   catch-all the type system requires; it is not a shortcut over a finite
   sum. *)
let batch_parts = function
  | Vdom.Cmd.Batch parts -> Some parts
  | other ->
    ignore other;
    None

let is_echo_of msg = function
  | Vdom.Cmd.Echo m -> m = msg
  | other ->
    ignore other;
    false

let is_after_of ms msg = function
  | Tea_client.After (n, m) -> n = ms && m = msg
  | other ->
    ignore other;
    false

let is_navigate_to url = function
  | Tea_client.Navigate u -> String.equal u url
  | other ->
    ignore other;
    false

(* --- the checks ----------------------------------------------------------- *)

let counter_vdom = Tea_client.html_to_vdom (App.view { App.count = 3 })

let () =
  check "root is <div class=\"counter\"> with 4 children"
    (tag_of counter_vdom = Some "div"
    && has_attribute "class" "counter" (attrs_of counter_vdom)
    && child_count counter_vdom = 4);
  check "child order: -, count, +, reset"
    (Option.bind (dig [ 0; 0 ] counter_vdom) text_of = Some "-"
    && Option.bind (dig [ 1; 0 ] counter_vdom) text_of = Some "3"
    && Option.bind (dig [ 2; 0 ] counter_vdom) text_of = Some "+"
    && Option.bind (dig [ 3; 0 ] counter_vdom) text_of = Some "reset");
  check "span keeps its class attribute"
    (Option.fold ~none:false
       ~some:(fun s -> has_attribute "class" "count" (attrs_of s))
       (dig [ 1 ] counter_vdom));
  check "each button's click handler yields its exact Msg, natively"
    (Option.map attrs_of (dig [ 0 ] counter_vdom) |> Option.map click_msg
     = Some (Some App.Decrement)
    && Option.map attrs_of (dig [ 2 ] counter_vdom) |> Option.map click_msg
       = Some (Some App.Increment)
    && Option.map attrs_of (dig [ 3 ] counter_vdom) |> Option.map click_msg
       = Some (Some App.Reset))

(* Text nodes cross unescaped: [createTextNode] is inert by construction, and
   escaping twice would corrupt user-visible text. Contrast [Render_static],
   which escapes because its output is a *string* spliced into a page. *)
let () =
  check "text is not string-escaped in the vdom"
    (text_of (Tea_client.html_to_vdom (Html.text "<b>&amp;\"")) = Some "<b>&amp;\"")

(* The one attribute translated to a DOM property: a controlled input must
   track the model on redraw, not keep the user's stale keystrokes. *)
let () =
  let input =
    Tea_client.html_to_vdom
      (Html.input ~attrs:[ Html.value_ "seed"; Html.class_ "box" ] ())
  in
  check "value_ becomes the DOM property, class_ stays an attribute"
    (has_string_property "value" "seed" (attrs_of input)
    && has_attribute "class" "box" (attrs_of input)
    && not (has_attribute "value" "seed" (attrs_of input)))

let () =
  let input =
    Tea_client.html_to_vdom
      (Html.input ~attrs:[ Html.on_input (fun s -> `Typed s) ] ())
  in
  check "on_input becomes an \"input\" handler"
    (List.exists (is_handler_for "input") (attrs_of input))

(* Functor fidelity: a mapped view's click handler yields the mapped Msg. *)
type wrapped = Wrapped of App.msg

let () =
  let mapped =
    Tea_client.html_to_vdom
      (Html.map (fun m -> Wrapped m) (App.view { App.count = 0 }))
  in
  check "Html.map composes with translation on click Msgs"
    (Option.map attrs_of (dig [ 0 ] mapped) |> Option.map click_msg
    = Some (Some (Wrapped App.Decrement)))

(* Construction already keeps only the first On_click (DOM semantics, and the
   invariant the server's form rewrite relies on); the translation must
   preserve exactly that one handler — tier parity. *)
let () =
  let button =
    Tea_client.html_to_vdom
      (Html.button
         ~attrs:[ Html.on_click App.Increment; Html.on_click App.Decrement ]
         [ Html.text "twice" ])
  in
  check "first-On_click-wins survives translation"
    (List.length (List.filter (is_handler_for "click") (attrs_of button)) = 1
    && click_msg (attrs_of button) = Some App.Increment)

(* --- Cmd translation ------------------------------------------------------ *)

let () =
  check "Cmd.none is an empty batch"
    (batch_parts (Tea_client.cmd_to_vdom Cmd.none) = Some []);
  check "Cmd.emit is Echo"
    (is_echo_of App.Increment (Tea_client.cmd_to_vdom (Cmd.emit App.Increment)));
  check "Cmd.after carries delay and Msg into After"
    (Option.fold ~none:false
       ~some:(fun d ->
         is_after_of 250 App.Reset
           (Tea_client.cmd_to_vdom (Cmd.after d App.Reset)))
       (Prim.Delay.of_ms 250));
  check "Cmd.navigate carries the URL into Navigate"
    (Result.fold
       ~ok:(fun url ->
         is_navigate_to "/next" (Tea_client.cmd_to_vdom (Cmd.navigate url)))
       ~error:(fun (_ : Prim.Url.err) -> false)
       (Prim.Url.of_string "/next"));
  check "Cmd.batch translates elementwise, in order"
    (Option.fold ~none:false
       ~some:(fun parts ->
         match parts with
         | [ echo; nested ] ->
           is_echo_of App.Increment echo && batch_parts nested = Some []
         | [] -> false
         | [ _ ] -> false
         | _ :: _ :: _ :: _ -> false)
       (batch_parts
          (Tea_client.cmd_to_vdom (Cmd.batch [ Cmd.emit App.Increment; Cmd.none ]))))

(* --- Make: the app record wiring ------------------------------------------ *)

module Client = Tea_client.Make (App)

let () =
  let model0, cmd0 = Client.app.Vdom.init in
  check "Make wires init (model and Cmd)"
    (model0.App.count = 0 && batch_parts cmd0 = Some []);
  let model1, cmd1 = Client.app.Vdom.update model0 App.Increment in
  let model2, _cmd2 = Client.app.Vdom.update model1 App.Increment in
  check "Make flips update to vdom's model-then-msg order"
    (model1.App.count = 1 && model2.App.count = 2 && batch_parts cmd1 = Some []);
  check "Make's view renders through the translation"
    (Option.bind (dig [ 1; 0 ] (Client.app.Vdom.view model2)) text_of = Some "2")

let () = print_endline "client_test: all checks passed"
