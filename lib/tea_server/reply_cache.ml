module Msg_seq = Tea_core.Prim.Msg_seq
module Tab_map = Map.Make (Tea_core.Prim.Tab_id)

type found =
  | Busy
  | Original of string
  | Gone

(* A [Pending] window is aged by its OWN tab's polling, never by other tabs'
   traffic: [polls] counts the [Busy] answers this window has already given,
   and each one spends a unit of [pending_grace]. A client polling its own 503
   therefore drains the window itself and eventually reads [Gone] - the
   liveness hole the grace exists to close (G3's rider: a never-settling
   handler must not pin its tab at 503 forever) - while unrelated tabs' load
   can never age a live window into a spurious [Replayed] (D20.2). A poll
   refreshes the slot's recency tick but never restarts the window: the budget
   only ever falls, until [settle] or a new [mark_pending] replaces the
   entry. *)
type entry =
  | Pending of
      { seq : Msg_seq.t
      ; polls : int64
      }
  | Settled of
      { seq : Msg_seq.t
      ; endpoint : string
      ; body : string
      }

type slot =
  { entry : entry
  ; tick : int64
  }

type t =
  { entry_bound : Replay_guard.Bound.t
  ; max_bytes : int
  ; body_cap : int
  ; pending_grace : int64
  ; tick : int64
  ; bytes : int
  ; live : slot Tab_map.t
  }

let default_max_bytes = 16 * 1024 * 1024
let default_body_cap = 64 * 1024
let default_pending_grace = 64

let v ?(entries = Replay_guard.default_rpc_tabs) ?(max_bytes = default_max_bytes)
    ?(body_cap = default_body_cap) ?(pending_grace = default_pending_grace) () : t =
  { entry_bound = entries
  ; max_bytes
  ; body_cap
  ; pending_grace = Int64.of_int pending_grace
  ; tick = 0L
  ; bytes = 0
  ; live = Tab_map.empty
  }

(* Only a settled body weighs anything: a [Pending] entry holds a seq and a
   tick. It still occupies an [entries] slot, because the 503 it answers is a
   promise the cache is making about a key. *)
let weight (e : entry) : int =
  match e with
  | Pending { seq = (_ : Msg_seq.t); polls = (_ : int64) } -> 0
  | Settled { seq = (_ : Msg_seq.t); endpoint = (_ : string); body } -> String.length body

(* Removal is the one place bytes are given back, so every eviction path and
   every replacement path goes through it rather than subtracting by hand. *)
let drop (t : t) (tab : Tea_core.Prim.Tab_id.t) : t =
  Tab_map.find_opt tab t.live
  |> Option.fold ~none:t ~some:(fun (s : slot) ->
         { t with bytes = t.bytes - weight s.entry; live = Tab_map.remove tab t.live })

(* The least recently touched tab, as a fold rather than [min_binding], which
   raises on an empty map: the fold is what keeps this total. *)
let oldest (live : slot Tab_map.t) : Tea_core.Prim.Tab_id.t option =
  Tab_map.fold
    (fun (k : Tea_core.Prim.Tab_id.t) (s : slot) acc ->
      Option.fold ~none:(Some (k, s.tick))
        ~some:(fun ((_ : Tea_core.Prim.Tab_id.t), (best : int64)) ->
          if Int64.compare s.tick best < 0 then Some (k, s.tick) else acc)
        acc)
    live None
  |> Option.map fst

(* Eviction runs only when an insert would exceed the entry bound, and only for
   a tab that is not already present: replacing an existing entry can never
   grow the map. Dropping an entry can only ever cost a client its cached
   bytes, which reads [Gone] and degrades to [Replayed]; it can never cause a
   second apply, because the floor that forbids that lives in the guard, not
   here. *)
let admit (t : t) (tab : Tea_core.Prim.Tab_id.t) : t =
  if
    Tab_map.mem tab t.live
    || Tab_map.cardinal t.live < Replay_guard.Bound.to_int t.entry_bound
  then t
  else oldest t.live |> Option.fold ~none:t ~some:(fun victim -> drop t victim)

(* Terminates because each step removes one entry and an empty map has no
   oldest: a [max_bytes] below a single body's size empties the cache rather
   than looping, which is the visible direction (every retry reads [Gone]). *)
let rec trim (t : t) : t =
  if t.bytes <= t.max_bytes then t
  else oldest t.live |> Option.fold ~none:t ~some:(fun victim -> trim (drop t victim))

let mark_pending (t : t) ~(tab : Tea_core.Prim.Tab_id.t) ~(seq : Msg_seq.t) : t =
  let tick = Int64.add t.tick 1L in
  (* [admit] before [drop]: if the tab is present, admit is a no-op and the
     drop frees its bytes; if it is absent and the map is full, admit takes the
     LRU victim and the drop is a no-op. Either order of presence lands on one
     free slot without a special case. *)
  let seated = drop (admit { t with tick } tab) tab in
  { seated with
    live = Tab_map.add tab { entry = Pending { seq; polls = 0L }; tick } seated.live
  }

(* The seq an entry answers for, either arm: what [settle]'s newest-wins
   comparison reads. *)
let entry_seq (e : entry) : Msg_seq.t =
  match e with
  | Pending { seq; polls = (_ : int64) } -> seq
  | Settled { seq; endpoint = (_ : string); body = (_ : string) } -> seq

let settle (t : t) ~(tab : Tea_core.Prim.Tab_id.t) ~(seq : Msg_seq.t)
    ~(endpoint : string) ~(body : string) : t =
  let install () : t =
    let tick = Int64.add t.tick 1L in
    let cleared = drop { t with tick } tab in
    (* An over-cap body leaves the tab with NO entry rather than with its stale
       [Pending] one: a retry then reads [Gone] and is told [Replayed], where a
       surviving [Pending] would answer 503 for an effect that has already
       finished. *)
    if String.length body > t.body_cap then cleared
    else
      let seated = admit cleared tab in
      trim
        { seated with
          bytes = seated.bytes + String.length body
        ; live =
            Tab_map.add tab { entry = Settled { seq; endpoint; body }; tick } seated.live
        }
  in
  (* Newest wins, enforced rather than assumed: a settle carrying an OLDER seq
     than the tab's live entry is discarded whole - state untouched, tick
     included - because only a non-conforming pipelining caller can produce
     one, and letting it land would destroy the newer window, whose duplicate
     must keep reading [Busy] while the newer effect is still in flight, never
     [Gone] and a 200 [Replayed]. *)
  Tab_map.find_opt tab t.live
  |> Option.fold ~none:install ~some:(fun (s : slot) () ->
         if Msg_seq.compare (entry_seq s.entry) seq > 0 then t else install ())
  |> fun step -> step ()

let touch (t : t) (tab : Tea_core.Prim.Tab_id.t) (s : slot) : t =
  { t with live = Tab_map.add tab { s with tick = t.tick } t.live }

let find (t : t) ~(tab : Tea_core.Prim.Tab_id.t) ~(seq : Msg_seq.t)
    ~(endpoint : string) : t * found =
  let ticked = { t with tick = Int64.add t.tick 1L } in
  Tab_map.find_opt tab ticked.live
  |> Option.fold ~none:(ticked, Gone) ~some:(fun (s : slot) ->
         match s.entry with
         | Pending { seq = open_seq; polls } ->
           if
             Int.equal (Msg_seq.compare seq open_seq) 0
             && Int64.compare polls ticked.pending_grace < 0
           then
             ( touch ticked tab
                 { s with entry = Pending { seq = open_seq; polls = Int64.add polls 1L } }
             , Busy )
           else (ticked, Gone)
         | Settled { seq = stored_seq; endpoint = stored_endpoint; body } ->
           if
             Int.equal (Msg_seq.compare seq stored_seq) 0
             && String.equal endpoint stored_endpoint
           then (touch ticked tab s, Original body)
           else (ticked, Gone))

let size (t : t) : int = Tab_map.cardinal t.live
let bytes (t : t) : int = t.bytes

module Cell = struct
  type cell = t ref

  let v ?entries ?max_bytes ?body_cap ?pending_grace () : cell =
    ref (v ?entries ?max_bytes ?body_cap ?pending_grace ())

  let mark_pending (c : cell) ~(tab : Tea_core.Prim.Tab_id.t) ~(seq : Msg_seq.t) : unit
      =
    c := mark_pending !c ~tab ~seq

  let settle (c : cell) ~(tab : Tea_core.Prim.Tab_id.t) ~(seq : Msg_seq.t)
      ~(endpoint : string) ~(body : string) : unit =
    c := settle !c ~tab ~seq ~endpoint ~body

  let find (c : cell) ~(tab : Tea_core.Prim.Tab_id.t) ~(seq : Msg_seq.t)
      ~(endpoint : string) : found =
    let t, answer = find !c ~tab ~seq ~endpoint in
    c := t;
    answer

  let snapshot (c : cell) : t = !c
end
