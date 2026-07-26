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

type error =
  | Transport of Tea_core.Cmd.http_failure
  | Decode of string

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

  let call (e : ('req, 'resp) A.t) (req : 'req)
      ~(reply : ('resp, error) result -> 'msg) : 'msg Tea_core.Cmd.t =
    let expect (wire : (string, Tea_core.Cmd.http_failure) result) : 'msg =
      let typed =
        Result.bind
          (Result.map_error (fun f -> Transport f) wire)
          (fun raw ->
            decode_resp e raw
            |> Result.map_error (fun (Tea_core.Codec.Decode_failed m) ->
                   Decode m))
      in
      reply typed
    in
    Tea_core.Cmd.http ~path:(path_of e) ~body:(encode_req e req) ~expect
end
