(** The typed RPC contract for the Shared document (DESIGN §8). One GADT, one
    set of Repr witnesses; the server routes and the client [call] builder are
    both folds over the same values, so a name, path, or codec cannot drift
    per tier. *)

type stats_req =
  { title : string
  ; body : string
  }

type stats_resp =
  { title_len : int
  ; word_count : int
  }

let stats_req_t =
  Repr.(
    record "stats_req" (fun title body -> { title; body })
    |+ field "title" string (fun r -> r.title)
    |+ field "body" string (fun r -> r.body)
    |> sealr)

let stats_resp_t =
  Repr.(
    record "stats_resp" (fun title_len word_count -> { title_len; word_count })
    |+ field "title_len" int (fun r -> r.title_len)
    |+ field "word_count" int (fun r -> r.word_count)
    |> sealr)

(* The pure semantics of [Doc_stats], defined once and linked by both the
   server handler and the tests, so what the server computes and what the
   suite asserts cannot drift. [word_count] is the number of maximal runs of
   non-whitespace, so a tab or newline separates words exactly as a space
   does (splitting on [' '] alone would miscount ["a\tb"] as one word). *)
let stats_of (req : stats_req) : stats_resp =
  let is_space c =
    Char.equal c ' ' || Char.equal c '\t' || Char.equal c '\n' || Char.equal c '\r'
  in
  let (word_count, (_ : bool)) =
    String.fold_left
      (fun ((count, in_word) : int * bool) c ->
        if is_space c then (count, false)
        else if in_word then (count, true)
        else (count + 1, true))
      (0, false) req.body
  in
  { title_len = String.length req.title; word_count }

type ('req, 'resp) t =
  | History_count : (unit, int) t
  | Doc_stats : (stats_req, stats_resp) t
  | Append_tag : (string, int) t

(* Compile-time-literal-only mints (the house [Tag.v] doctrine): the names
   are program constants, never request or model data. Both witnesses below
   are total and wildcard-free — a new constructor without a name/codec is a
   compile error here, before either tier builds. *)
let name : type req resp. (req, resp) t -> Tea_rpc.Name.t = function
  | History_count -> Tea_rpc.Name.v "history_count"
  | Doc_stats -> Tea_rpc.Name.v "doc_stats"
  | Append_tag -> Tea_rpc.Name.v "append_tag"

let req_t : type req resp. (req, resp) t -> req Repr.t = function
  | History_count -> Repr.unit
  | Doc_stats -> stats_req_t
  | Append_tag -> Repr.string

let resp_t : type req resp. (req, resp) t -> resp Repr.t = function
  | History_count -> Repr.int
  | Doc_stats -> stats_resp_t
  | Append_tag -> Repr.int

(* The anti-CSRF classification the server gates on (D12). [Append_tag] drives
   a Msg through the store, so it is [Mutating]; the two query endpoints answer
   from state they never touch. Wildcard-free on purpose: defaulting a future
   endpoint to [Read_only] is exactly the mistake this witness exists to make
   impossible. *)
let kind : type req resp. (req, resp) t -> Tea_rpc.endpoint_kind = function
  | History_count -> Tea_rpc.Read_only
  | Doc_stats -> Tea_rpc.Read_only
  | Append_tag -> Tea_rpc.Mutating

type any = Any : ('req, 'resp) t -> any

let all = [ Any History_count; Any Doc_stats; Any Append_tag ]
