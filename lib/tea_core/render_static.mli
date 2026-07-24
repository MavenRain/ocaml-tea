(** Server-side rendering: an {!Html.t} to an HTML string with text and
    attribute values HTML-escaped. Event handlers ([on_click]/[on_input]) have
    no static representation and are dropped; the client runtime re-attaches
    behaviour after hydration. *)

val to_string : 'msg Html.t -> string
