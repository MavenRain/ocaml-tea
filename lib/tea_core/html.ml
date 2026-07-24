type 'msg attr =
  | Attr of Prim.Attr_name.t * Prim.Attr_value.t
  | On_click of 'msg
  | On_input of (Prim.Input_text.t -> 'msg)

type 'msg t =
  | Text of Prim.Text.t
  | Element of Prim.Tag.t * 'msg attr list * 'msg t list

let text s = Text (Prim.Text.v s)

(* An element carries at most one click handler (the first listed): DOM
   [onclick] semantics, and the invariant the server's form-post SSR relies on
   (one Msg per click site, identically in both tiers). *)
let first_click_only attrs =
  let keep (seen, acc) a =
    match a with
    | On_click _ -> if seen then (seen, acc) else (true, a :: acc)
    | Attr _ -> (seen, a :: acc)
    | On_input _ -> (seen, a :: acc)
  in
  attrs |> List.fold_left keep (false, []) |> snd |> List.rev

let elt name ?(attrs = []) children =
  Element (Prim.Tag.v name, first_click_only attrs, children)
let div ?attrs children = elt "div" ?attrs children
let span ?attrs children = elt "span" ?attrs children
let p ?attrs children = elt "p" ?attrs children
let h1 ?attrs children = elt "h1" ?attrs children
let ul ?attrs children = elt "ul" ?attrs children
let li ?attrs children = elt "li" ?attrs children
let button ?attrs children = elt "button" ?attrs children
let input ?attrs () = elt "input" ?attrs []

(* Framework-internal helpers use trusted, known-safe attribute names. *)
let known name value = Attr (Prim.Attr_name.v name, Prim.Attr_value.v value)
let class_ v = known "class" v
let id_ v = known "id" v
let type_ v = known "type" v
let value_ v = known "value" v
let placeholder v = known "placeholder" v
let name_ v = known "name" v
let action_ v = known "action" v
let method_ v = known "method" v

(* Untrusted attribute names must pass the [on*] filter. *)
let attr name value =
  match Prim.Attr_name.of_string name with
  | Ok n -> Some (Attr (n, Prim.Attr_value.v value))
  | Error Prim.Attr_name.Empty -> None
  | Error Prim.Attr_name.Event_handler -> None

let on_click msg = On_click msg
let on_input f = On_input (fun it -> f (Prim.Input_text.to_string it))

let map_attr f = function
  | Attr (n, v) -> Attr (n, v)
  | On_click m -> On_click (f m)
  | On_input g -> On_input (fun it -> f (g it))

let rec map f = function
  | Text s -> Text s
  | Element (tag, attrs, children) ->
    Element (tag, List.map (map_attr f) attrs, List.map (map f) children)
