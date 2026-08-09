module Rep_map = Map.Make (Tea_core.Crdt.Replica)
module Tab_map = Map.Make (Tea_core.Prim.Tab_id)
module Seq_map = Map.Make (Tea_core.Prim.Msg_seq)

type outcome =
  | Landed
  | Released

(* One row per in-flight [Fresh] attempt. The promise is shared by every
   waiter on the key, and [Lwt.wait] keeps it non-cancelable (the
   [died]/[mark_died] reasoning in [tea_server.ml]): a parked socket dying
   cannot reject the settlement its sibling waiters share. *)
type row =
  { promise : outcome Lwt.t
  ; wake : outcome Lwt.u
  }

type handle = row

type t = { mutable live : row Seq_map.t Tab_map.t Rep_map.t }

let create () : t = { live = Rep_map.empty }

let tabs_of (t : t) ~(replica : Tea_core.Crdt.Replica.t) :
    row Seq_map.t Tab_map.t =
  Option.value ~default:Tab_map.empty (Rep_map.find_opt replica t.live)

let seqs_of (tabs : row Seq_map.t Tab_map.t) ~(tab : Tea_core.Prim.Tab_id.t) :
    row Seq_map.t =
  Option.value ~default:Seq_map.empty (Tab_map.find_opt tab tabs)

let register (t : t) ~(replica : Tea_core.Crdt.Replica.t)
    ~(tab : Tea_core.Prim.Tab_id.t) ~(seq : Tea_core.Prim.Msg_seq.t) : handle =
  let promise, wake = Lwt.wait () in
  let row = { promise; wake } in
  let tabs = tabs_of t ~replica in
  let seqs = seqs_of tabs ~tab in
  let superseded = Seq_map.find_opt seq seqs in
  (* Install before waking. A wake at callback depth zero runs its waiters
     inline, and a woken continuation may re-enter this registry; the map it
     observes must already hold the new row, and no write staged from a
     pre-wake snapshot may follow the wake. *)
  t.live <-
    Rep_map.add replica
      (Tab_map.add tab (Seq_map.add seq row seqs) tabs)
      t.live;
  (* A standing row here means the replay guard re-opened a consumed seq
     under a new attempt - the tab-LRU eviction path, whose licensed failure
     direction is a duplicate, never a loss. [Released] sends the old row's
     waiters to the no-ack path: their clients retransmit, and the
     retransmission parks on THIS attempt's row and takes THIS attempt's
     verdict. *)
  Option.iter
    (fun (old : row) -> Lwt.wakeup_later old.wake Released)
    superseded;
  row

let find (t : t) ~(replica : Tea_core.Crdt.Replica.t)
    ~(tab : Tea_core.Prim.Tab_id.t) ~(seq : Tea_core.Prim.Msg_seq.t) :
    outcome Lwt.t option =
  Option.map
    (fun (r : row) -> r.promise)
    (Option.bind
       (Option.bind (Rep_map.find_opt replica t.live) (Tab_map.find_opt tab))
       (Seq_map.find_opt seq))

(* Removal prunes empty sub-maps on the way out, so a settled key leaves no
   empty [Tab_map]/[Seq_map] husk behind: the table's residents are exactly
   the open rows. The [handle] gate keeps a superseded attempt's late settle
   away from the row that replaced it, and the wake comes strictly LAST -
   the same depth-zero reentrancy discipline as [register]: a woken waiter
   that re-enters the registry must never have its write clobbered by a
   stale pre-wake snapshot. *)
let settle (t : t) ~(replica : Tea_core.Crdt.Replica.t)
    ~(tab : Tea_core.Prim.Tab_id.t) ~(seq : Tea_core.Prim.Msg_seq.t)
    ~(handle : handle) ~(outcome : outcome) : unit =
  let tabs = tabs_of t ~replica in
  let seqs = seqs_of tabs ~tab in
  Option.fold ~none:()
    ~some:(fun (r : row) ->
      if r == handle then (
        let seqs = Seq_map.remove seq seqs in
        let tabs =
          if Seq_map.is_empty seqs then Tab_map.remove tab tabs
          else Tab_map.add tab seqs tabs
        in
        t.live <-
          (if Tab_map.is_empty tabs then Rep_map.remove replica t.live
           else Rep_map.add replica tabs t.live);
        Lwt.wakeup_later r.wake outcome))
    (Seq_map.find_opt seq seqs)

let parked_count (t : t) ~(replica : Tea_core.Crdt.Replica.t)
    ~(tab : Tea_core.Prim.Tab_id.t) : int =
  Seq_map.cardinal (seqs_of (tabs_of t ~replica) ~tab)
