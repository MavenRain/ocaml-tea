(** Derived wire codecs, replacing lean-tea's hand-written
    [encodeModel]/[decodeModel]/[decodeMsg]. Everything is generated from
    [Repr.t] values, so server and client share one definition and schema
    drift is impossible. *)

type err = Decode_failed of string

let to_json (t : 'a Repr.t) (v : 'a) : string = Repr.to_json_string t v

(* [Repr.of_json_string] returns a result for a malformed document, but it
   RAISES [Invalid_argument "index out of bounds"] when a well-formed object
   names a variant case the witness does not have ([{"Bogus":1}] against any
   app's [msg_t]). Every decode here is fed untrusted input — ws frames, form
   posts, RPC bodies — and every caller reads the result type as a promise of
   totality, so the promise is enforced rather than hoped for: without this the
   live-session pump would die on a crafted frame instead of closing the
   socket, and a form post would answer 500 where it means 400. *)
let of_json (t : 'a Repr.t) (s : string) : ('a, err) result =
  match Repr.of_json_string t s with
  | decoded -> Result.map_error (fun (`Msg e) -> Decode_failed e) decoded
  | exception (e : exn) -> Error (Decode_failed (Printexc.to_string e))

module Make (A : App.APP) = struct
  type nonrec err = err = Decode_failed of string

  let model_to_json (m : A.model) : string = to_json A.model_t m
  let model_of_json (s : string) : (A.model, err) result = of_json A.model_t s
  let msg_to_json (m : A.msg) : string = to_json A.msg_t m
  let msg_of_json (s : string) : (A.msg, err) result = of_json A.msg_t s

  (* The down-frame witness is derived from [A.model_t] and the replica's own
     witness, exactly like every other codec here — so the [Hello] announcement
     cannot drift from the [Head] pushes, nor from what the client decodes. *)
  let down_t : A.model Wire.down Repr.t =
    Repr.(
      variant "down" (fun hello head -> function
        | Wire.Hello (r, m) -> hello (r, m)
        | Wire.Head m -> head m)
      |~ case1 "hello" (pair Crdt.Replica.t A.model_t) (fun (r, m) -> Wire.Hello (r, m))
      |~ case1 "head" A.model_t (fun m -> Wire.Head m)
      |> sealv)

  let down_to_json (d : A.model Wire.down) : string = to_json down_t d
  let down_of_json (s : string) : (A.model Wire.down, err) result = of_json down_t s

  (** A human-readable label for a message; used as the Irmin commit message so
      the commit log doubles as an event log. *)
  let msg_to_label (m : A.msg) : string = Repr.to_string A.msg_t m
end
