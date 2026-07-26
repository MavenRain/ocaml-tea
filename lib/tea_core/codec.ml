(** Derived wire codecs, replacing lean-tea's hand-written
    [encodeModel]/[decodeModel]/[decodeMsg]. Everything is generated from
    [Repr.t] values, so server and client share one definition and schema
    drift is impossible. *)

type err = Decode_failed of string

let to_json (t : 'a Repr.t) (v : 'a) : string = Repr.to_json_string t v

let of_json (t : 'a Repr.t) (s : string) : ('a, err) result =
  Repr.of_json_string t s
  |> Result.map_error (fun (`Msg e) -> Decode_failed e)

module Make (A : App.APP) = struct
  type nonrec err = err = Decode_failed of string

  let model_to_json (m : A.model) : string = to_json A.model_t m
  let model_of_json (s : string) : (A.model, err) result = of_json A.model_t s
  let msg_to_json (m : A.msg) : string = to_json A.msg_t m
  let msg_of_json (s : string) : (A.msg, err) result = of_json A.msg_t s

  (** A human-readable label for a message; used as the Irmin commit message so
      the commit log doubles as an event log. *)
  let msg_to_label (m : A.msg) : string = Repr.to_string A.msg_t m
end
