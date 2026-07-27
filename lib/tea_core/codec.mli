(** One Repr-JSON codec for everything on the wire: Irmin Contents, ws model
    frames, form msgs, and RPC payloads (DESIGN §8: one codec, no second
    definition to drift). *)

type err = Decode_failed of string

val to_json : 'a Repr.t -> 'a -> string

val of_json : 'a Repr.t -> string -> ('a, err) result
(** Total: every input string yields a value or a [Decode_failed], including
    the ones [Repr] itself answers with an exception (a JSON object naming a
    variant case the witness does not have). Callers are entitled to read the
    result type as the whole story — the decode paths all sit behind untrusted
    input. *)

module Make (A : App.APP) : sig
  type nonrec err = err = Decode_failed of string
  (** Re-export equation: existing [Codec.Decode_failed] matches in
      [tea_server] / [tea_client_run] compile unchanged. *)

  val model_to_json : A.model -> string
  val model_of_json : string -> (A.model, err) result
  val msg_to_json : A.msg -> string
  val msg_of_json : string -> (A.msg, err) result

  val down_t : A.model Wire.down Repr.t
  (** The live socket's down-frame witness (roadmap step 9, D14): derived from
      [A.model_t] and {!Crdt.Replica}'s own witness rather than written out, so
      the [Hello] announcement cannot drift from the [Head] pushes. *)

  val down_to_json : A.model Wire.down -> string

  val down_of_json : string -> (A.model Wire.down, err) result
  (** {!model_of_json} is deliberately no longer what the client calls on a
      socket frame: a bare model cannot say whether it arrived with an identity
      attached. It stays for the paths that genuinely carry a model alone. *)

  val msg_to_label : A.msg -> string
  (** A human-readable label for a message; used as the Irmin commit message
      so the commit log doubles as an event log. *)
end
