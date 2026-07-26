(** One Repr-JSON codec for everything on the wire: Irmin Contents, ws model
    frames, form msgs, and RPC payloads (DESIGN §8: one codec, no second
    definition to drift). *)

type err = Decode_failed of string

val to_json : 'a Repr.t -> 'a -> string

val of_json : 'a Repr.t -> string -> ('a, err) result

module Make (A : App.APP) : sig
  type nonrec err = err = Decode_failed of string
  (** Re-export equation: existing [Codec.Decode_failed] matches in
      [tea_server] / [tea_client_run] compile unchanged. *)

  val model_to_json : A.model -> string
  val model_of_json : string -> (A.model, err) result
  val msg_to_json : A.msg -> string
  val msg_of_json : string -> (A.msg, err) result

  val msg_to_label : A.msg -> string
  (** A human-readable label for a message; used as the Irmin commit message
      so the commit log doubles as an event log. *)
end
