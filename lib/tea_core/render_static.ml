let escape s =
  s
  |> String.to_seq
  |> Seq.map (function
       | '<' -> "&lt;"
       | '>' -> "&gt;"
       | '&' -> "&amp;"
       | '"' -> "&quot;"
       | '\'' -> "&#39;"
       | c -> String.make 1 c)
  |> List.of_seq
  |> String.concat ""

let attr = function
  | Html.Attr (n, v) ->
    Printf.sprintf " %s=\"%s\"" (Prim.Attr_name.to_string n) (escape (Prim.Attr_value.to_string v))
  | Html.On_click _ -> ""
  | Html.On_input _ -> ""

let rec node = function
  | Html.Text t -> escape (Prim.Text.to_string t)
  | Html.Element (tag, attrs, children) ->
    let name = Prim.Tag.to_string tag in
    let a = attrs |> List.map attr |> String.concat "" in
    let inner = children |> List.map node |> String.concat "" in
    Printf.sprintf "<%s%s>%s</%s>" name a inner name

let to_string html = node html
