(** A pure, backend-agnostic view type.

    Owning our own [Html.t] (rather than exposing vdom directly) buys two things:
    the native server can render the same view to an escaped HTML string
    ({!Render_static}) without linking the vdom package, and every dangerous
    construction site (attribute names, later hrefs) becomes a safety hook.
    The type is [private]: build only through the smart constructors. *)

type 'msg attr = private
  | Attr of Prim.Attr_name.t * Prim.Attr_value.t
  | On_click of 'msg
  | On_input of (Prim.Input_text.t -> 'msg)

type 'msg t = private
  | Text of Prim.Text.t  (** escaped at render time *)
  | Element of Prim.Tag.t * 'msg attr list * 'msg t list

(** {2 Nodes} *)

val text : string -> 'msg t
val elt : string -> ?attrs:'msg attr list -> 'msg t list -> 'msg t
val div : ?attrs:'msg attr list -> 'msg t list -> 'msg t
val span : ?attrs:'msg attr list -> 'msg t list -> 'msg t
val p : ?attrs:'msg attr list -> 'msg t list -> 'msg t
val h1 : ?attrs:'msg attr list -> 'msg t list -> 'msg t
val ul : ?attrs:'msg attr list -> 'msg t list -> 'msg t
val li : ?attrs:'msg attr list -> 'msg t list -> 'msg t
val button : ?attrs:'msg attr list -> 'msg t list -> 'msg t
val input : ?attrs:'msg attr list -> unit -> 'msg t

(** {2 Attributes} *)

val class_ : string -> 'msg attr
val id_ : string -> 'msg attr
val type_ : string -> 'msg attr
val value_ : string -> 'msg attr
val placeholder : string -> 'msg attr

(** Untrusted attribute name: [None] if the name is empty or an [on*] handler. *)
val attr : string -> string -> 'msg attr option

val on_click : 'msg -> 'msg attr
val on_input : (string -> 'msg) -> 'msg attr

(** {2 Functor} *)

val map : ('a -> 'b) -> 'a t -> 'b t
