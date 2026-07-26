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

(* --- Subs: the pure subscription planner (roadmap step 3) ----------------- *)

module Subs = Tea_client.Subs
module Sub = Tea_core.Sub

(* Test-side [Interval] construction: [of_ms] rejects only non-positive input,
   so unwrapping the literals below is safe. *)
let interval ms = Option.get (Prim.Interval.of_ms ms)

(* Spec classifiers, one exhaustive match each; the callbacks are checked by
   {e applying} them, the same off-browser discipline as the decoders above. *)
let spec_interval = function
  | Subs.Spec_every (ms, _) -> Some ms
  | Subs.Spec_store _ -> None

let every_msg tick = function
  | Subs.Spec_every (_, f) -> Some (f tick)
  | Subs.Spec_store _ -> None

let store_msg model = function
  | Subs.Spec_store g -> Some (g model)
  | Subs.Spec_every (_, _) -> None

let specs =
  Subs.specs_of
    (Sub.batch
       [ Sub.none
       ; Sub.every (interval 1000) (fun tick -> App.Sync tick)
       ; Sub.batch
           [ Sub.store_watch (fun (m : App.model) -> App.Sync m.count); Sub.none ]
       ; Sub.every (interval 250) (fun _tick -> App.Reset)
       ])

let () =
  check "specs_of flattens None_/Batch, preserving leaf order"
    (List.length specs = 3 && List.filter_map spec_interval specs = [ 1000; 250 ]);
  check "the Store_watch leaf sits between the two timers"
    (Option.bind (List.nth_opt specs 1) (store_msg { App.count = 9 })
    = Some (App.Sync 9));
  check "Spec_every callbacks cross intact (applied to a tick)"
    (List.filter_map (every_msg 7) specs = [ App.Sync 7; App.Reset ]);
  check "the Counter subscribes to the store, reconciling via Sync"
    (List.filter_map (store_msg { App.count = 5 })
       (Subs.specs_of (App.subscriptions { App.count = 0 }))
    = [ App.Sync 5 ])

(* Functor fidelity again: [Sub.map] must reach under [Store_watch]. *)
let () =
  let mapped =
    Sub.map
      (fun m -> Wrapped m)
      (Sub.store_watch (fun (m : App.model) -> App.Sync m.count))
  in
  check "Sub.map composes under Store_watch"
    (List.filter_map (store_msg { App.count = 4 }) (Subs.specs_of mapped)
    = [ Wrapped (App.Sync 4) ])

let () =
  check "key_of_spec keys timers by period and any watch as the one socket"
    (List.map Subs.key_of_spec specs
     = [ Subs.Key_every 1000; Subs.Key_store; Subs.Key_every 250 ]
    && Subs.equal_key Subs.Key_store Subs.Key_store
    && not (Subs.equal_key (Subs.Key_every 1000) (Subs.Key_every 250))
    && not (Subs.equal_key (Subs.Key_every 1000) Subs.Key_store));
  check "keys_of dedups by resource, first-occurrence order"
    (Subs.keys_of
       (Subs.specs_of
          (Sub.batch
             [ Sub.every (interval 500) (fun tick -> App.Sync tick)
             ; Sub.every (interval 500) (fun _tick -> App.Reset)
             ; Sub.store_watch (fun (m : App.model) -> App.Sync m.count)
             ; Sub.every (interval 250) (fun _tick -> App.Increment)
             ; Sub.store_watch (fun (m : App.model) -> App.Sync (m.count + 1))
             ]))
    = [ Subs.Key_every 500; Subs.Key_store; Subs.Key_every 250 ])

let () =
  let to_start, to_stop =
    Subs.plan
      ~active:[ Subs.Key_every 1000; Subs.Key_store ]
      ~wanted:[ Subs.Key_store; Subs.Key_every 250 ]
  in
  check "plan diffs both ways, preserving input order"
    (to_start = [ Subs.Key_every 250 ] && to_stop = [ Subs.Key_every 1000 ]);
  check "plan from nothing starts the wanted key"
    (Subs.plan ~active:[] ~wanted:[ Subs.Key_store ] = ([ Subs.Key_store ], []));
  check "plan to nothing stops the active key"
    (Subs.plan ~active:[ Subs.Key_store ] ~wanted:[] = ([], [ Subs.Key_store ]))

(* --- The typed RPC verb, lowered and interpreted (roadmap step 7, DESIGN §8)

   Discipline: NEVER apply the polymorphic [(=)] to a [Tea_core.Cmd.t] (or a
   lowered [Vdom.Cmd.t]) that may carry [Http] — its [expect] field is a
   closure and structural comparison over a function raises. Every check below
   observes a command by matching its constructor to pull [expect] out and
   {e evaluating} it on canned frames; only closure-free msg *payloads* are
   ever compared with [(=)]. *)

module SR = Shared_doc_rpc
module Sd = Shared_doc_app.App
module R = Tea_rpc.Make (Shared_doc_rpc)

(* Pull the lowered [Http] extension out of a [Vdom.Cmd.t]; the type is open,
   so the catch-all is required by the type system, not a shortcut. *)
let http_of = function
  | Tea_client.Http { path; body; expect } -> Some (path, body, expect)
  | other ->
    ignore other;
    None

let stats_resp_eq = Repr.(unstage (equal SR.stats_resp_t))

(* Recover a [Got_stats] payload from the app msg, or [None] otherwise —
   wildcard-free so a new msg constructor is a compile error here. *)
let got_stats_of : Sd.msg -> (SR.stats_resp, Tea_rpc.error) result option = function
  | Sd.Got_stats r -> Some r
  | Sd.Set_title (_ : string) -> None
  | Sd.Set_body (_ : string) -> None
  | Sd.Like -> None
  | Sd.Unlike -> None
  | Sd.Add_tag (_ : string) -> None
  | Sd.Remove_tag (_ : string) -> None
  | Sd.Sync_doc (_ : Sd.model) -> None
  | Sd.Request_stats -> None

let stats_req_val = { SR.title = "hello"; body = "a b c d" }
let resp_val = { SR.title_len = 5; word_count = 4 }
let ok_frame = Ok (R.encode_resp SR.Doc_stats resp_val)

(* [reply] into the real app msg so the whole client path is exercised. *)
let stats_cmd = R.call SR.Doc_stats stats_req_val ~reply:(fun r -> Sd.Got_stats r)

let () =
  let lowered = Tea_client.cmd_to_vdom stats_cmd in
  check "cmd_to_vdom lowers R.call to Tea_client.Http with the derived path and encoded body"
    (http_of lowered
    |> Option.fold ~none:false ~some:(fun (path, wire_body, _expect) ->
           String.equal path "/rpc/doc_stats"
           && String.equal wire_body (R.encode_req SR.Doc_stats stats_req_val)));
  check "the lowered Http expect maps an Ok canned resp to Got_stats (Ok stats)"
    (http_of lowered
    |> Option.fold ~none:false ~some:(fun (_path, _body, expect) ->
           got_stats_of (expect ok_frame)
           |> Option.fold ~none:false ~some:(fun r ->
                  Result.fold r ~error:(fun (_ : Tea_rpc.error) -> false)
                    ~ok:(fun s -> stats_resp_eq s resp_val))));
  check "the lowered Http expect routes an Http_status 500 into the app's error msg (no drop)"
    (http_of lowered
    |> Option.fold ~none:false ~some:(fun (_path, _body, expect) ->
           Prim.Status.of_int 500
           |> Option.fold ~none:false ~some:(fun s ->
                  got_stats_of (expect (Error (Cmd.Http_status s)))
                  |> Option.fold ~none:false ~some:(fun r ->
                         Result.fold r ~ok:(fun (_ : SR.stats_resp) -> false)
                           ~error:(function
                             | Tea_rpc.Transport (Cmd.Http_status s') ->
                               Prim.Status.to_int s' = 500
                             | Tea_rpc.Transport Cmd.Network_error -> false
                             | Tea_rpc.Transport Cmd.No_transport -> false
                             | Tea_rpc.Decode _ -> false)))))

(* --- Cmd.map post-composition over Http (both frames) + Batch distribution - *)

type wrap = Wrap of Sd.msg

let () =
  let g m = Wrap m in
  let src_expect =
    http_of (Tea_client.cmd_to_vdom stats_cmd) |> Option.map (fun (_, _, e) -> e)
  in
  let mapped_expect =
    http_of (Tea_client.cmd_to_vdom (Cmd.map g stats_cmd)) |> Option.map (fun (_, _, e) -> e)
  in
  let err_frame s = Error (Cmd.Http_status s) in
  check "Cmd.map post-composes the Http expect (g o expect) on an Ok frame"
    (Option.fold src_expect ~none:false ~some:(fun e ->
         Option.fold mapped_expect ~none:false ~some:(fun e' ->
             e' ok_frame = g (e ok_frame))));
  check "Cmd.map post-composes the Http expect (g o expect) on an Error frame"
    (Prim.Status.of_int 500
    |> Option.fold ~none:false ~some:(fun s ->
           Option.fold src_expect ~none:false ~some:(fun e ->
               Option.fold mapped_expect ~none:false ~some:(fun e' ->
                   e' (err_frame s) = g (e (err_frame s))))))

let () =
  let g m = Wrap m in
  let src_expect =
    http_of (Tea_client.cmd_to_vdom stats_cmd) |> Option.map (fun (_, _, e) -> e)
  in
  let mapped_batch =
    Tea_client.cmd_to_vdom (Cmd.map g (Cmd.batch [ stats_cmd; Cmd.emit Sd.Request_stats ]))
  in
  check "Cmd.map distributes through Batch [http; emit], mapping both children"
    (batch_parts mapped_batch
    |> Option.fold ~none:false ~some:(function
         | [ http_child; echo_child ] ->
           (http_of http_child
           |> Option.fold ~none:false ~some:(fun (path, _body, e') ->
                  String.equal path "/rpc/doc_stats"
                  && Option.fold src_expect ~none:false ~some:(fun e ->
                         e' ok_frame = g (e ok_frame))))
           && is_echo_of (Wrap Sd.Request_stats) echo_child
         | [] -> false
         | [ _ ] -> false
         | _ :: _ :: _ :: _ -> false))

(* --- The server-side Loop resolves Http to No_transport (identity Io) ------ *)

(* No identity-Io Loop precedent existed in the suite; this is the first. A
   minimal APP whose update stores the raw reply (so the No_transport error is
   observable, not folded away like the real app's [Got_stats]). *)
module Identity = struct
  type 'a t = 'a

  let return x = x
  let bind x f = f x
  let all xs = xs
end

module Probe = struct
  open Tea_core
  module Rp = Tea_rpc.Make (Shared_doc_rpc)

  type reply = (SR.stats_resp, Tea_rpc.error) result
  type model = reply option

  type msg =
    | Fire  (** one call whose reply is stored *)
    | Fire_loop  (** a call whose reply re-fires the call — the fuel probe *)
    | Store of reply

  (* [reply] has no faithful wire form (Tea_rpc.error wraps the abstract
     Status); project through the success option purely to satisfy [APP]. The
     Loop never serialises. *)
  let reply_t : reply Repr.t =
    Repr.map
      (Repr.option SR.stats_resp_t)
      (Option.fold ~none:(Error (Tea_rpc.Decode "no wire form")) ~some:(fun s -> Ok s))
      (Result.fold ~ok:Option.some ~error:(fun (_ : Tea_rpc.error) -> None))

  let model_t = Repr.option reply_t

  let msg_t =
    Repr.(
      variant "msg" (fun fire fire_loop store ->
        function
        | Fire -> fire
        | Fire_loop -> fire_loop
        | Store r -> store r)
      |~ case0 "Fire" Fire
      |~ case0 "Fire_loop" Fire_loop
      |~ case1 "Store" reply_t (fun r -> Store r)
      |> sealv)

  let call ~reply =
    Rp.call SR.Doc_stats { SR.title = "t"; body = "b" } ~reply

  let init = (None, Cmd.none)

  let update msg (model : model) =
    match msg with
    | Fire -> (model, call ~reply:(fun r -> Store r))
    | Fire_loop -> (model, call ~reply:(fun (_ : reply) -> Fire_loop))
    | Store r -> (Some r, Cmd.none)

  let view (_ : model) = Html.text "probe"
  let subscriptions (_ : model) = Sub.none
  let merge = Merge.(to_spec (atomic ~eq:Repr.(unstage (equal model_t))))
  let title = Prim.Title.v "probe"
  let url_of_model (_ : model) = None
  let msg_of_url (_ : Prim.Url.t) = None
end

module _ : Tea_core.App.APP = Probe
module L = Tea_core.Loop.Loop (Identity) (Probe)

let probe_fx =
  { L.sleep = (fun (_ : Prim.Delay.t) -> ())
  ; navigate = (fun (_ : Prim.Url.t) -> ())
  }

let () =
  check "identity-Io Loop resolves an R.call to Error (Transport No_transport)"
    (L.step ~fx:probe_fx ~fuel:Prim.Fuel.default Probe.Fire None
    |> Result.fold
         ~error:(fun L.Fuel_exhausted -> false)
         ~ok:(fun (m : Probe.model) ->
           Option.fold m ~none:false ~some:(fun r ->
               Result.fold r ~ok:(fun (_ : SR.stats_resp) -> false)
                 ~error:(function
                   | Tea_rpc.Transport Cmd.No_transport -> true
                   | Tea_rpc.Transport (Cmd.Http_status (_ : Prim.Status.t)) -> false
                   | Tea_rpc.Transport Cmd.Network_error -> false
                   | Tea_rpc.Decode _ -> false))));
  check "a reply that re-fires the call burns fuel like Emit (fuel 2 -> Fuel_exhausted)"
    (Prim.Fuel.of_int 2
    |> Option.fold ~none:false ~some:(fun fuel2 ->
           L.step ~fx:probe_fx ~fuel:fuel2 Probe.Fire_loop None
           |> Result.fold ~ok:(fun (_ : Probe.model) -> false)
                ~error:(fun L.Fuel_exhausted -> true)))

let () = print_endline "client_test: all checks passed"
