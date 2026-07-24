(** Derived wire codecs, replacing lean-tea's hand-written
    [encodeModel]/[decodeModel]/[decodeMsg]. Everything is generated from the
    [APP]'s [Repr.t] values, so server and client share one definition and
    schema drift is impossible. *)

module Make (A : App.APP) = struct
  type err = Decode_failed of string

  let model_to_json (m : A.model) : string = Repr.to_json_string A.model_t m

  let model_of_json (s : string) : (A.model, err) result =
    match Repr.of_json_string A.model_t s with
    | Ok m -> Ok m
    | Error (`Msg e) -> Error (Decode_failed e)

  let msg_of_json (s : string) : (A.msg, err) result =
    match Repr.of_json_string A.msg_t s with
    | Ok m -> Ok m
    | Error (`Msg e) -> Error (Decode_failed e)

  (** A human-readable label for a message; used as the Irmin commit message so
      the commit log doubles as an event log. *)
  let msg_to_label (m : A.msg) : string = Repr.to_string A.msg_t m
end
