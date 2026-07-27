module Msg_seq = Tea_core.Prim.Msg_seq
module Tab_map = Map.Make (Tea_core.Prim.Tab_id)
module Rep_map = Map.Make (Tea_core.Crdt.Replica)

module Floors = struct
  type t = Msg_seq.t Tab_map.t Rep_map.t

  let empty : t = Rep_map.empty

  let apply (t : t) (e : Guard_sink.event) : t =
    match e with
    | Guard_sink.Advance { replica; tab; seq } ->
      let tabs =
        Rep_map.find_opt replica t |> Option.value ~default:Tab_map.empty
      in
      let floor =
        Tab_map.find_opt tab tabs
        |> Option.fold ~none:seq ~some:(fun (cur : Msg_seq.t) ->
               if Tea_core.Prim.Msg_seq.compare seq cur > 0 then seq else cur)
      in
      Rep_map.add replica (Tab_map.add tab floor tabs) t
    | Guard_sink.Forget { replica } -> Rep_map.remove replica t

  let of_events (events : Guard_sink.event list) : t =
    List.fold_left apply empty events

  let find ~(replica : Tea_core.Crdt.Replica.t) ~(tab : Tea_core.Prim.Tab_id.t)
      (t : t) : Msg_seq.t option =
    Rep_map.find_opt replica t |> Fun.flip Option.bind (Tab_map.find_opt tab)

  let cardinal (t : t) : int =
    Rep_map.fold
      (fun (_ : Tea_core.Crdt.Replica.t) (tabs : Msg_seq.t Tab_map.t) (acc : int) ->
        acc + Tab_map.cardinal tabs)
      t 0
end

type t =
  { cell : Replay_guard.Cell.cell
  ; mutable floors : Floors.t
  ; sink : Guard_sink.t
  }

let v ~(sessions : Replay_guard.Bound.t) ~(tabs : Replay_guard.Bound.t)
    ~(sink : Guard_sink.t) ~(floors : Floors.t) : t =
  { cell = Replay_guard.Cell.v ~sessions ~tabs; floors; sink }

let take (t : t) ~(replica : Tea_core.Crdt.Replica.t)
    ~(tab : Tea_core.Prim.Tab_id.t) ~(seq : Msg_seq.t) : Replay_guard.verdict =
  (* A Cell miss — never heard from, or evicted — asks the mirror before the
     verdict, so the durable floor is what an absent entry degrades to instead
     of "accept anything". Closures on both branches, applied once (the
     [Option.fold] eagerness discipline). *)
  let seed_from_mirror () : unit =
    Option.iter
      (fun (floor : Msg_seq.t) ->
        Replay_guard.Cell.seed t.cell ~replica ~tab ~high:floor)
      (Floors.find ~replica ~tab t.floors)
  in
  Replay_guard.high_water (Replay_guard.Cell.snapshot t.cell) ~replica ~tab
  |> Option.fold ~none:seed_from_mirror ~some:(fun (_ : Msg_seq.t) () -> ())
  |> fun sync -> sync ();
  Replay_guard.Cell.take t.cell ~replica ~tab ~seq

let persist (t : t) ~(replica : Tea_core.Crdt.Replica.t)
    ~(tab : Tea_core.Prim.Tab_id.t) ~(seq : Msg_seq.t) :
    (unit, Guard_sink.err) result Lwt.t =
  let e = Guard_sink.Advance { replica; tab; seq } in
  (* Mirror first: if the sink yields and the Cell evicts this entry before
     the append lands, the next frame's miss still re-seeds at the true
     water. *)
  t.floors <- Floors.apply t.floors e;
  t.sink.Guard_sink.append e

let forget (t : t) ~(replica : Tea_core.Crdt.Replica.t) :
    (unit, Guard_sink.err) result Lwt.t =
  let open Lwt.Syntax in
  let* appended = t.sink.Guard_sink.append (Guard_sink.Forget { replica }) in
  t.floors <- Floors.apply t.floors (Guard_sink.Forget { replica });
  Replay_guard.Cell.forget t.cell ~replica;
  Lwt.return appended

let snapshot (t : t) : Replay_guard.t = Replay_guard.Cell.snapshot t.cell
let floors (t : t) : Floors.t = t.floors
