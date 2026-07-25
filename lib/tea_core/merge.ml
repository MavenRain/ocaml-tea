(* A merger is exactly the payload of [Merge_spec.Three_way]: the ancestor is
   optional because two branches can diverge with no common history (Irmin hands
   us [None] then). Keeping it as that function type means [to_spec] is a pure
   re-wrapping and the combinators are testable without an Irmin repo. *)
type 'a t = ancestor:'a option -> ours:'a -> theirs:'a -> ('a, string) result

let run (m : 'a t) = m
let to_spec (m : 'a t) : 'a Merge_spec.t = Merge_spec.Three_way m
let conflict (msg : string) : 'a t = fun ~ancestor:_ ~ours:_ ~theirs:_ -> Error msg

let atomic ~(eq : 'a -> 'a -> bool) : 'a t =
 fun ~ancestor ~ours ~theirs ->
  if eq ours theirs then Ok ours
  else
    match ancestor with
    | Some a when eq ours a -> Ok theirs
    | Some a when eq theirs a -> Ok ours
    | Some _ | None -> Error "concurrent divergent edits"

let counter : int t =
 fun ~ancestor ~ours ~theirs ->
  let base =
    match ancestor with
    | None -> 0
    | Some a -> a
  in
  Ok (ours + theirs - base)

let set (type a) ~(cmp : a -> a -> int) : a list t =
 fun ~ancestor ~ours ~theirs ->
  let norm = List.sort_uniq cmp in
  let mem x = List.exists (fun y -> cmp x y = 0) in
  let diff xs ys = List.filter (fun x -> not (mem x ys)) xs in
  let union xs ys = norm (List.rev_append xs ys) in
  let anc =
    match ancestor with
    | None -> []
    | Some a -> norm a
  in
  let ours = norm ours and theirs = norm theirs in
  let added = union (diff ours anc) (diff theirs anc) in
  let removed = union (diff anc ours) (diff anc theirs) in
  Ok (diff (union anc added) removed)

let map (f : 'a -> 'a) (m : 'a t) : 'a t =
 fun ~ancestor ~ours ~theirs -> Result.map f (m ~ancestor ~ours ~theirs)

type 'r field =
  | Field : string * ('r -> 'a) * ('a -> 'r -> 'r) * 'a t -> 'r field

let field ~label ~get ~set m = Field (label, get, set, m)

let record (fields : 'r field list) : 'r t =
 fun ~ancestor ~ours ~theirs ->
  List.fold_left
    (fun acc (Field (label, get, put, m)) ->
      match acc with
      | Error _ as e -> e
      | Ok r -> (
        match m ~ancestor:(Option.map get ancestor) ~ours:(get ours) ~theirs:(get theirs) with
        | Ok v -> Ok (put v r)
        | Error msg -> Error (label ^ ": " ^ msg)))
    (Ok ours)
    fields
