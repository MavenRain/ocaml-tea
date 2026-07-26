(* State-based CRDTs: see crdt.mli. Every [join] is a least upper bound over a
   canonical (sorted / deduplicated) representation, so idempotence,
   commutativity and associativity are structural — the SEC guarantee. No
   wildcard arm, no exception in normal control flow, Option/Result via
   combinators. *)

module Replica = struct
  type t = Prim.Session_id.t

  let v (s : Prim.Session_id.t) : t = s
  let compare = Prim.Session_id.compare
  let equal (a : t) (b : t) : bool = compare a b = 0

  (* A direct string witness (via [Prim.Session_id.t]), NOT [Repr.map]: a
     map-wrapped string misframes under irmin-pack's contents-length header. *)
  let t : t Repr.t = Prim.Session_id.t
end

module Dot = struct
  type t =
    { stamp : int64
    ; replica : Replica.t
    }

  let v (s : Clock.stamp) (replica : Replica.t) : t = { stamp = (s :> int64); replica }

  (* Lexicographic on (stamp, replica). Total: distinct events differ in the
     stamp (one replica's clock is strictly monotone) or in the replica. *)
  let compare (a : t) (b : t) : int =
    let c = Int64.compare a.stamp b.stamp in
    if c <> 0 then c else Replica.compare a.replica b.replica

  let equal (a : t) (b : t) : bool = compare a b = 0

  let t : t Repr.t =
    Repr.(
      record "dot" (fun stamp replica -> { stamp; replica })
      |+ field "stamp" int64 (fun d -> d.stamp)
      |+ field "replica" Replica.t (fun d -> d.replica)
      |> sealr)
end

module Ctx = struct
  type t =
    { clock : Clock.t
    ; replica : Replica.t
    }

  let v ~(clock : Clock.t) ~(replica : Replica.t) : t = { clock; replica }
  let dot (t : t) : Dot.t = Dot.v (Clock.next t.clock) t.replica
  let replica (t : t) : Replica.t = t.replica
end

(* Encode a structured state as an opaque, self-delimiting string blob.

   irmin-pack's contents-length framing ([contents_length_header = `Varint])
   round-trips a scalar leaf (an int, a string) but misframes a structured
   dynamic content (a [list] of [pair]s) on reopen — the decoder reads a garbage
   length and blits past the buffer. Wrapping the state's binary encoding as a
   single [Repr.string] makes every CRDT field a scalar leaf, which pack stores
   and reloads faithfully. The [of_bin_string] round-trip is total for any bytes
   we ourselves wrote; the [fallback] is the never-taken corrupt-bytes arm (no
   exception in normal control flow). *)
let blob (type a) (inner : a Repr.t) (fallback : a) : a Repr.t =
  let enc = Repr.unstage (Repr.to_bin_string inner) in
  let dec = Repr.unstage (Repr.of_bin_string inner) in
  Repr.map Repr.string
    (fun (s : string) -> Result.fold (dec s) ~ok:Fun.id ~error:(fun (_ : [ `Msg of string ]) -> fallback))
    enc

module Pn_counter = struct
  (* Sorted-by-replica assoc list replica -> (increments, decrements). Sorted +
     one entry per replica is the canonical form every operation preserves. The
     halves are plain [int] (a like/vote counter never approaches [max_int]);
     [Repr.int] is a self-delimiting varint, which irmin-pack's contents framing
     round-trips, unlike a fixed-width [int64] in this position. *)
  type state = (Replica.t * (int * int)) list

  let bottom : state = []
  let t : state Repr.t = blob Repr.(list (pair Replica.t (pair int int))) bottom

  let value (s : state) : int =
    List.fold_left (fun acc ((_ : Replica.t), (i, d)) -> acc + (i - d)) 0 s

  let equal : state -> state -> bool = Repr.(unstage (equal t))

  (* Adjust one replica's (inc, dec) pair, keeping the list sorted and single-
     entry-per-replica. A missing replica is inserted at its sorted position. *)
  let adjust (r : Replica.t) (f : int * int -> int * int) (s : state) : state =
    let rec go = function
      | [] -> [ (r, f (0, 0)) ]
      | (r', p) :: rest ->
        let c = Replica.compare r r' in
        if c = 0 then (r', f p) :: rest
        else if c < 0 then (r, f (0, 0)) :: (r', p) :: rest
        else (r', p) :: go rest
    in
    go s

  let inc (r : Replica.t) (s : state) : state = adjust r (fun (i, d) -> (i + 1, d)) s
  let dec (r : Replica.t) (s : state) : state = adjust r (fun (i, d) -> (i, d + 1)) s

  let reset (r : Replica.t) (s : state) : state =
    let v = value s in
    adjust r (fun (i, d) -> (i, d + v)) s

  (* Pointwise max over the union of replicas: idempotent (max x x = x),
     commutative and associative (max is), so re-merging never double-counts. *)
  let rec join (a : state) (b : state) : state =
    match (a, b) with
    | [], bs -> bs
    | as_, [] -> as_
    | (ra, (ia, da)) :: at, (rb, (ib, db)) :: bt ->
      let c = Replica.compare ra rb in
      if c = 0 then (ra, (max ia ib, max da db)) :: join at bt
      else if c < 0 then (ra, (ia, da)) :: join at ((rb, (ib, db)) :: bt)
      else (rb, (ib, db)) :: join ((ra, (ia, da)) :: at) bt
end

module type ELT = sig
  type t

  val compare : t -> t -> int
  val t : t Repr.t
end

module type VAL = sig
  type t

  val t : t Repr.t
end

module Or_set (E : ELT) = struct
  type state =
    { adds : (Dot.t * E.t) list
    ; tombs : Dot.t list
    }

  let bottom : state = { adds = []; tombs = [] }

  let t : state Repr.t =
    blob
      Repr.(
        record "or_set" (fun adds tombs -> { adds; tombs })
        |+ field "adds" (list (pair Dot.t E.t)) (fun s -> s.adds)
        |+ field "tombs" (list Dot.t) (fun s -> s.tombs)
        |> sealr)
      bottom
  let equal : state -> state -> bool = Repr.(unstage (equal t))

  let dot_mem (d : Dot.t) (ds : Dot.t list) : bool = List.exists (fun d' -> Dot.equal d d') ds

  (* Sorted-by-Dot dedup insert: minted dots are unique, so [=] only fires on a
     re-observed dot (join), where keeping the existing entry is idempotent. *)
  let insert_dot (d : Dot.t) (ds : Dot.t list) : Dot.t list =
    let rec go = function
      | [] -> [ d ]
      | d' :: rest ->
        let c = Dot.compare d d' in
        if c = 0 then d' :: rest
        else if c < 0 then d :: d' :: rest
        else d' :: go rest
    in
    go ds

  let insert_add ((d, e) : Dot.t * E.t) (adds : (Dot.t * E.t) list) : (Dot.t * E.t) list =
    let rec go = function
      | [] -> [ (d, e) ]
      | (d', e') :: rest ->
        let c = Dot.compare d d' in
        if c = 0 then (d', e') :: rest
        else if c < 0 then (d, e) :: (d', e') :: rest
        else (d', e') :: go rest
    in
    go adds

  let add (d : Dot.t) (e : E.t) (s : state) : state = { s with adds = insert_add (d, e) s.adds }

  (* Observed-remove: tombstone exactly the dots of [e] this replica has in its
     add-set. A concurrent add on another replica carries a dot this set never
     observed, so it is not tombstoned — add-wins. *)
  let remove (e : E.t) (s : state) : state =
    let dots_of_e =
      List.filter_map (fun (d, x) -> if E.compare x e = 0 then Some d else None) s.adds
    in
    { s with tombs = List.fold_left (fun acc d -> insert_dot d acc) s.tombs dots_of_e }

  let live (s : state) : (Dot.t * E.t) list =
    List.filter (fun (d, (_ : E.t)) -> not (dot_mem d s.tombs)) s.adds

  let value (s : state) : E.t list = List.sort_uniq E.compare (List.map snd (live s))
  let mem (e : E.t) (s : state) : bool = List.exists (fun ((_ : Dot.t), x) -> E.compare x e = 0) (live s)

  (* Union of the add-set and the tombstone-set. Union is idempotent,
     commutative and associative, and canonical sorted-dedup insertion makes the
     result order-independent — so the two sides converge structurally. *)
  let join (a : state) (b : state) : state =
    { adds = List.fold_left (fun acc ae -> insert_add ae acc) a.adds b.adds
    ; tombs = List.fold_left (fun acc d -> insert_dot d acc) a.tombs b.tombs
    }
end

module Lww (V : VAL) = struct
  type state =
    { dot : Dot.t option
    ; value : V.t
    }

  (* The dot rides as an opaque blob (a [list]-free scalar leaf for pack); the
     value stays a direct [V.t] leaf. Intended for scalar values (an LWW title /
     body is a string). *)
  let dot_blob : Dot.t option Repr.t = blob Repr.(option Dot.t) None

  let t : state Repr.t =
    Repr.(
      record "lww" (fun dot value -> { dot; value })
      |+ field "dot" dot_blob (fun s -> s.dot)
      |+ field "value" V.t (fun s -> s.value)
      |> sealr)

  let bottom (v : V.t) : state = { dot = None; value = v }
  let value (s : state) : V.t = s.value
  let equal : state -> state -> bool = Repr.(unstage (equal t))
  let set (dot : Dot.t) (v : V.t) (_ : state) : state = { dot = Some dot; value = v }

  (* [a] dominates [b] when its dot is the greater ([None] least). At a genuine
     tie (equal dots) the states are equal — a dot is unique to one write — so
     which side is kept is immaterial to the value. *)
  let dominates (a : Dot.t option) (b : Dot.t option) : bool =
    match (a, b) with
    | None, None -> true
    | Some (_ : Dot.t), None -> true
    | None, Some (_ : Dot.t) -> false
    | Some da, Some db -> Dot.compare da db >= 0

  let join (x : state) (y : state) : state = if dominates x.dot y.dot then x else y
end

type 'r field = F : ('r -> 'a) * ('a -> 'r -> 'r) * ('a -> 'a -> 'a) -> 'r field

let field ~get ~set ~join = F (get, set, join)

let record (fields : 'r field list) (a : 'r) (b : 'r) : 'r =
  List.fold_left (fun acc (F (get, set, join)) -> set (join (get a) (get b)) acc) a fields
