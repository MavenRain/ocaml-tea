module Msg_seq = Tea_core.Prim.Msg_seq
module Tab_map = Map.Make (Tea_core.Prim.Tab_id)
module Rep_map = Map.Make (Tea_core.Crdt.Replica)

module Bound = struct
  type t = int

  let of_int n = if n <= 0 then None else Some n
  let to_int t = t
end

let default_sessions : Bound.t = 4096
let default_tabs : Bound.t = 8

(* [tick] is a monotone counter this module keeps itself: it makes the eviction
   order total and tie-free without a clock parameter, without a [Unix]
   dependency and without coupling dedup to wall time. A replica's own tick is
   refreshed whenever any of its tabs is touched, so the outer eviction sees the
   same recency the inner one does. *)
type entry =
  { high : Msg_seq.t
  ; tick : int64
  }

type per_replica =
  { tabs : entry Tab_map.t
  ; tick : int64
  }

type t =
  { session_bound : Bound.t
  ; tab_bound : Bound.t
  ; tick : int64
  ; live : per_replica Rep_map.t
  }

let v ~(sessions : Bound.t) ~(tabs : Bound.t) : t =
  { session_bound = sessions; tab_bound = tabs; tick = 0L; live = Rep_map.empty }

type verdict =
  | Fresh of Msg_seq.t
  | Duplicate of Msg_seq.t
  | Gapped

(* The least-recently-touched key, as a fold rather than [min_binding] (which
   raises on an empty map) — a fold is what keeps this total. Written twice
   rather than through one polymorphic helper: the two maps have different key
   types and different entry shapes, and the shared version needed a locally
   abstract type on both, which is more machinery than the duplication. *)
let keep_older (candidate : 'k * int64) (acc : ('k * int64) option) : ('k * int64) option =
  Option.fold ~none:(Some candidate)
    ~some:(fun ((_ : 'k), best) ->
      if Int64.compare (snd candidate) best < 0 then Some candidate else acc)
    acc

let oldest_tab (tabs : entry Tab_map.t) : Tea_core.Prim.Tab_id.t option =
  Tab_map.fold (fun k (e : entry) acc -> keep_older (k, e.tick) acc) tabs None
  |> Option.map fst

let oldest_replica (live : per_replica Rep_map.t) : Tea_core.Crdt.Replica.t option =
  Rep_map.fold (fun k (p : per_replica) acc -> keep_older (k, p.tick) acc) live None
  |> Option.map fst

(* Eviction runs only when an insert would exceed a bound, and only for a key
   that is not already present — refreshing an existing entry can never grow the
   map. Dropping an entry can only ever cause a duplicate later, never a loss,
   because an absent entry accepts any sequence number. *)
let admit_tab (bound : Bound.t) (key : Tea_core.Prim.Tab_id.t) (tabs : entry Tab_map.t) :
    entry Tab_map.t =
  if Tab_map.mem key tabs || Tab_map.cardinal tabs < Bound.to_int bound then tabs
  else
    oldest_tab tabs
    |> Option.fold ~none:tabs ~some:(fun victim -> Tab_map.remove victim tabs)

let admit_replica (bound : Bound.t) (key : Tea_core.Crdt.Replica.t)
    (live : per_replica Rep_map.t) : per_replica Rep_map.t =
  if Rep_map.mem key live || Rep_map.cardinal live < Bound.to_int bound then live
  else
    oldest_replica live
    |> Option.fold ~none:live ~some:(fun victim -> Rep_map.remove victim live)

let record (t : t) ~(replica : Tea_core.Crdt.Replica.t) ~(tab : Tea_core.Prim.Tab_id.t)
    ~(high : Msg_seq.t) : t =
  let tick = Int64.add t.tick 1L in
  let live = admit_replica t.session_bound replica t.live in
  let existing =
    Rep_map.find_opt replica live
    |> Option.value ~default:{ tabs = Tab_map.empty; tick }
  in
  let tabs = admit_tab t.tab_bound tab existing.tabs in
  { t with
    tick
  ; live = Rep_map.add replica { tabs = Tab_map.add tab { high; tick } tabs; tick } live
  }

let take (t : t) ~(replica : Tea_core.Crdt.Replica.t) ~(tab : Tea_core.Prim.Tab_id.t)
    ~(seq : Msg_seq.t) : t * verdict =
  Rep_map.find_opt replica t.live
  |> Option.map (fun (p : per_replica) -> p.tabs)
  |> Fun.flip Option.bind (Tab_map.find_opt tab)
  |> Option.fold
       (* No entry: this tab has never been heard from, or its entry was
          evicted. Accept, which is the direction that can only duplicate. *)
       ~none:(record t ~replica ~tab ~high:seq, Fresh seq)
       ~some:(fun (e : entry) ->
         if Msg_seq.compare seq e.high <= 0 then
           (* Already consumed. Re-record at the same high water so the retrying
              tab's recency is refreshed: the entry that is actively in use is
              the one eviction must not take. *)
           (record t ~replica ~tab ~high:e.high, Duplicate e.high)
         else if
           Msg_seq.next e.high
           |> Option.fold ~none:false ~some:(fun nx -> Int.equal (Msg_seq.compare seq nx) 0)
         then (record t ~replica ~tab ~high:seq, Fresh seq)
         else (t, Gapped))

let forget (t : t) ~(replica : Tea_core.Crdt.Replica.t) : t =
  { t with live = Rep_map.remove replica t.live }

let high_water (t : t) ~(replica : Tea_core.Crdt.Replica.t)
    ~(tab : Tea_core.Prim.Tab_id.t) : Msg_seq.t option =
  Rep_map.find_opt replica t.live
  |> Option.map (fun (p : per_replica) -> p.tabs)
  |> Fun.flip Option.bind (Tab_map.find_opt tab)
  |> Option.map (fun (e : entry) -> e.high)

let sessions (t : t) : int = Rep_map.cardinal t.live

let tabs (t : t) ~(replica : Tea_core.Crdt.Replica.t) : int =
  Rep_map.find_opt replica t.live
  |> Option.fold ~none:0 ~some:(fun (p : per_replica) -> Tab_map.cardinal p.tabs)

module Cell = struct
  type cell = t ref

  let v ~(sessions : Bound.t) ~(tabs : Bound.t) : cell = ref (v ~sessions ~tabs)

  let take (c : cell) ~(replica : Tea_core.Crdt.Replica.t)
      ~(tab : Tea_core.Prim.Tab_id.t) ~(seq : Msg_seq.t) : verdict =
    let t, verdict = take !c ~replica ~tab ~seq in
    c := t;
    verdict

  let forget (c : cell) ~(replica : Tea_core.Crdt.Replica.t) : unit =
    c := forget !c ~replica

  let snapshot (c : cell) : t = !c
end
