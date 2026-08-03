module Name = struct
  type t = string

  type err =
    | Empty
    | Invalid_char of char

  (* [a-z0-9_-]: lowercase so a name is byte-for-byte its own wire form, and
     every byte is closed under Rpc_path admission, which is what makes
     [Make.path_of]'s literal mint total. *)
  let is_name_char c =
    (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || Char.equal c '_'
    || Char.equal c '-'

  let of_string s =
    if String.length s = 0 then Error Empty
    else
      String.to_seq s
      |> Seq.find (fun c -> not (is_name_char c))
      |> Option.fold ~none:(Ok s) ~some:(fun c -> Error (Invalid_char c))

  let v s = s
  let to_string t = t
  let equal = String.equal
end

let prefix = "/rpc/"

(* One definition of the delivery header's name, linked verbatim by both tiers
   (T3). A custom header rather than a body field: it carries transport framing
   that the handler must never see, and it keeps a keyed cross-site POST
   non-simple on its own, so the CSRF preflight argument does not rest on the
   content-type gate alone. *)
let key_header = "x-tea-key"

module Key = struct
  type t =
    { tab : Tea_core.Prim.Tab_id.t
    ; seq : Tea_core.Prim.Msg_seq.t
    }

  type err =
    | Bad_shape
    | Bad_tab of Tea_core.Prim.Tab_id.err
    | Bad_seq

  let v ~(tab : Tea_core.Prim.Tab_id.t) ~(seq : Tea_core.Prim.Msg_seq.t) : t =
    { tab; seq }

  let tab (t : t) : Tea_core.Prim.Tab_id.t = t.tab
  let seq (t : t) : Tea_core.Prim.Msg_seq.t = t.seq

  let to_string (t : t) : string =
    Tea_core.Prim.Tab_id.to_string t.tab
    ^ ":"
    ^ Int.to_string (Tea_core.Prim.Msg_seq.to_int t.seq)

  (* A canonical parse: the accepted string is exactly the canonical print of
     the value it returns, so every alias [int_of_string_opt] alone would
     admit - ["0x7"], ["7_0"], ["+7"], and the zero-padded ["007"] - is
     impossible by construction rather than by enumeration. The guard reads a
     DENSE sequence, so an alias is not cosmetic: it lets the same position
     arrive under two keys, which is exactly the thing the sequence exists to
     make impossible. *)
  let seq_of_string (s : string) : Tea_core.Prim.Msg_seq.t option =
    Option.bind (int_of_string_opt s) (fun (n : int) ->
        if String.equal (Int.to_string n) s then Tea_core.Prim.Msg_seq.of_int n
        else None)

  (* [split_on_char] rather than an index-and-[String.sub] pair: it is total,
     and a half that itself contains a colon lands in the three-or-more arm as
     [Bad_shape] instead of being silently truncated to a prefix. *)
  let of_string (s : string) : (t, err) result =
    match String.split_on_char ':' s with
    | [] | [ (_ : string) ] -> Error Bad_shape
    | (_ : string) :: (_ : string) :: (_ : string) :: (_ : string list) ->
      Error Bad_shape
    | [ tab; seq ] ->
      Result.bind
        (Tea_core.Prim.Tab_id.of_string tab
        |> Result.map_error (fun (e : Tea_core.Prim.Tab_id.err) -> Bad_tab e))
        (fun (tab : Tea_core.Prim.Tab_id.t) ->
          seq_of_string seq
          |> Option.fold ~none:(Error Bad_seq) ~some:(fun seq -> Ok (v ~tab ~seq)))
end

type 'resp keyed_resp =
  | Reply of 'resp
  | Replayed

(* A Repr variant, so the envelope is structured JSON with one derived codec
   both tiers link — never a string-in-string wrapper, and never a status code
   standing in for an application-visible outcome. *)
let keyed_resp_t (resp_t : 'resp Repr.t) : 'resp keyed_resp Repr.t =
  Repr.(
    variant "keyed_resp" (fun reply replayed -> function
      | Reply r -> reply r
      | Replayed -> replayed)
    |~ case1 "reply" resp_t (fun r -> Reply r)
    |~ case0 "replayed" Replayed
    |> sealv)

type error =
  | Transport of Tea_core.Cmd.http_failure
  | Decode of string
  | Applied_reply_lost

type endpoint_kind =
  | Read_only
  | Mutating

module type API = sig
  type ('req, 'resp) t

  val name : ('req, 'resp) t -> Name.t
  val req_t : ('req, 'resp) t -> 'req Repr.t
  val resp_t : ('req, 'resp) t -> 'resp Repr.t
  val kind : ('req, 'resp) t -> endpoint_kind

  type any = Any : ('req, 'resp) t -> any

  val all : any list
end

module Make (A : API) = struct
  let path_of (e : ('req, 'resp) A.t) : Tea_core.Prim.Rpc_path.t =
    Tea_core.Prim.Rpc_path.v (prefix ^ Name.to_string (A.name e))

  let of_name (n : Name.t) : A.any option =
    List.find_opt (fun (A.Any e) -> Name.equal (A.name e) n) A.all

  let encode_req (e : ('req, 'resp) A.t) (req : 'req) : string =
    Tea_core.Codec.to_json (A.req_t e) req

  let decode_req (e : ('req, 'resp) A.t) (s : string) :
      ('req, Tea_core.Codec.err) result =
    Tea_core.Codec.of_json (A.req_t e) s

  let encode_resp (e : ('req, 'resp) A.t) (resp : 'resp) : string =
    Tea_core.Codec.to_json (A.resp_t e) resp

  let decode_resp (e : ('req, 'resp) A.t) (s : string) :
      ('resp, Tea_core.Codec.err) result =
    Tea_core.Codec.of_json (A.resp_t e) s

  (* The transport outcome lifted into {!error}; shared by both decode arms so
     a network failure reads identically whichever channel the call rode. *)
  let of_transport (wire : (string, Tea_core.Cmd.http_failure) result) :
      (string, error) result =
    Result.map_error (fun f -> Transport f) wire

  let lift_decode (r : ('a, Tea_core.Codec.err) result) : ('a, error) result =
    Result.map_error (fun (Tea_core.Codec.Decode_failed m) -> Decode m) r

  (* [Read_only]: today's contract, a bare ['resp] on 200. *)
  let decode_bare (e : ('req, 'resp) A.t)
      (wire : (string, Tea_core.Cmd.http_failure) result) :
      ('resp, error) result =
    Result.bind (of_transport wire) (fun raw -> lift_decode (decode_resp e raw))

  (* [Mutating]: the {!keyed_resp} envelope on 200, ALWAYS, keyed request or
     not. [Replayed] becomes {!Applied_reply_lost} rather than a [Transport]
     failure, because the two say opposite things about the effect: transport
     failure means the effect's fate is unknown, this means the effect
     certainly happened and only its reply bytes are gone. *)
  let decode_keyed (e : ('req, 'resp) A.t)
      (wire : (string, Tea_core.Cmd.http_failure) result) :
      ('resp, error) result =
    Result.bind (of_transport wire) (fun raw ->
        Result.bind
          (lift_decode (Tea_core.Codec.of_json (keyed_resp_t (A.resp_t e)) raw))
          (fun (k : 'resp keyed_resp) ->
            match k with
            | Reply r -> Ok r
            | Replayed -> Error Applied_reply_lost))

  let call (e : ('req, 'resp) A.t) (req : 'req)
      ~(reply : ('resp, error) result -> 'msg) : 'msg Tea_core.Cmd.t =
    let path = path_of e and body = encode_req e req in
    (* The endpoint's own total [kind] witness picks the channel, so an app
       never chooses a delivery contract and never mints a key: classify an
       endpoint [Mutating] and its calls become exactly-once by construction.
       The match is wildcard-free, so a third kind is a compile error here. *)
    match (A.kind e : endpoint_kind) with
    | Read_only ->
      Tea_core.Cmd.http ~path ~body ~expect:(fun wire -> reply (decode_bare e wire))
    | Mutating ->
      Tea_core.Cmd.http_keyed ~path ~body ~expect:(fun wire ->
          reply (decode_keyed e wire))
end
