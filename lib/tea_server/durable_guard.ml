module Msg_seq = Tea_core.Prim.Msg_seq
module Store_water = Tea_core.Prim.Store_water
module Tab_map = Map.Make (Tea_core.Prim.Tab_id)
module Rep_map = Map.Make (Tea_core.Crdt.Replica)

module Floors = struct
  type t = (Msg_seq.t * Store_water.t) Tab_map.t Rep_map.t

  let empty : t = Rep_map.empty

  let apply (t : t) (e : Guard_sink.event) : t =
    match e with
    | Guard_sink.Advance { replica; tab; seq; water } ->
      let tabs =
        Rep_map.find_opt replica t |> Option.value ~default:Tab_map.empty
      in
      (* The seq only ever rises, and the WITNESS never drowns: a record at
         or above the floor adopts the higher seq but only ever RAISES the
         water. A take can mint [bottom] at a higher seq — a no-op update
         commits nothing (irmin skips both the write and the info mint) and
         a fuel-exhausted take commits nothing by design — yet the state
         those takes rest on is whatever the strongest witness said.
         Adopting the incoming water verbatim would let one witness-less
         take de-witness the whole tab: kept-on-trust at the next boot
         where the elder stamp would have read [dropped_behind] — the
         silent-loss direction R20 exists to forbid. *)
      let floor =
        Tab_map.find_opt tab tabs
        |> Option.fold ~none:(seq, water)
             ~some:(fun ((cur, cur_water) : Msg_seq.t * Store_water.t) ->
               if Tea_core.Prim.Msg_seq.compare seq cur >= 0 then
                 ( seq
                 , if Store_water.compare water cur_water >= 0 then water
                   else cur_water )
               else (cur, cur_water))
      in
      Rep_map.add replica (Tab_map.add tab floor tabs) t
    | Guard_sink.Forget { replica } -> Rep_map.remove replica t

  let of_events (events : Guard_sink.event list) : t =
    List.fold_left apply empty events

  let find_stamped ~(replica : Tea_core.Crdt.Replica.t)
      ~(tab : Tea_core.Prim.Tab_id.t) (t : t) :
      (Msg_seq.t * Store_water.t) option =
    Rep_map.find_opt replica t |> Fun.flip Option.bind (Tab_map.find_opt tab)

  let find ~(replica : Tea_core.Crdt.Replica.t) ~(tab : Tea_core.Prim.Tab_id.t)
      (t : t) : Msg_seq.t option =
    find_stamped ~replica ~tab t |> Option.map fst

  let cardinal (t : t) : int =
    Rep_map.fold
      (fun (_ : Tea_core.Crdt.Replica.t)
           (tabs : (Msg_seq.t * Store_water.t) Tab_map.t) (acc : int) ->
        acc + Tab_map.cardinal tabs)
      t 0

  type verdict =
    { kept : int
    ; dropped_behind : int
    ; dropped_no_branch : int
    ; unwitnessed : int
    }

  (* Classification order is the R20 argument in miniature. A bottom floor
     claims nothing, so there is nothing the store could fail — adopted on
     trust wherever it is found, and counted, because "adopted on trust" is
     exactly the unprotected case the boot report must name. Only a floor
     with a REAL claim can be dropped, and only a READABLE head strictly
     below that claim is evidence of a rollback; a missing branch, or one
     whose head reads bottom, is dropped without crying rollback — usually
     routine housekeeping (collected, reaped, or never written), though a
     root restored from before the branch existed reads exactly the same,
     so the operator line names both and clears neither. *)
  let filter ~(head : Tea_core.Crdt.Replica.t -> Store_water.t option) (t : t) :
      t * verdict =
    Rep_map.fold
      (fun (replica : Tea_core.Crdt.Replica.t)
           (tabs : (Msg_seq.t * Store_water.t) Tab_map.t)
           ((acc, verdict) : t * verdict) ->
        let head_water = head replica in
        let kept_tabs, verdict =
          Tab_map.fold
            (fun (tab : Tea_core.Prim.Tab_id.t)
                 ((seq, water) : Msg_seq.t * Store_water.t)
                 ((kept_tabs, verdict) :
                   (Msg_seq.t * Store_water.t) Tab_map.t * verdict) ->
              if Store_water.equal water Store_water.bottom then
                ( Tab_map.add tab (seq, water) kept_tabs
                , { verdict with
                    kept = verdict.kept + 1
                  ; unwitnessed = verdict.unwitnessed + 1
                  } )
              else
                Option.fold head_water
                  ~none:
                    ( kept_tabs
                    , { verdict with
                        dropped_no_branch = verdict.dropped_no_branch + 1
                      } )
                  ~some:(fun (head : Store_water.t) ->
                    if Store_water.covers ~head ~floor:water then
                      ( Tab_map.add tab (seq, water) kept_tabs
                      , { verdict with kept = verdict.kept + 1 } )
                    else if Store_water.equal head Store_water.bottom then
                      ( kept_tabs
                      , { verdict with
                          dropped_no_branch = verdict.dropped_no_branch + 1
                        } )
                    else
                      ( kept_tabs
                      , { verdict with
                          dropped_behind = verdict.dropped_behind + 1
                        } )))
            tabs (Tab_map.empty, verdict)
        in
        ( (if Tab_map.is_empty kept_tabs then acc
           else Rep_map.add replica kept_tabs acc)
        , verdict ))
      t
      ( empty
      , { kept = 0; dropped_behind = 0; dropped_no_branch = 0; unwitnessed = 0 } )
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
    ~(tab : Tea_core.Prim.Tab_id.t) ~(seq : Msg_seq.t)
    ~(water : Store_water.t) : (unit, Guard_sink.err) result Lwt.t =
  let e = Guard_sink.Advance { replica; tab; seq; water } in
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
