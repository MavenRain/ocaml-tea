module Msg_seq = Tea_core.Prim.Msg_seq
module Store_water = Tea_core.Prim.Store_water
module Tab_map = Map.Make (Tea_core.Prim.Tab_id)
module Rep_map = Map.Make (Tea_core.Crdt.Replica)

module Floors = struct
  (* Each floor carries a monotone recency tick beside the seq and the witness
     it records — the [Replay_guard] discipline reused verbatim, so the
     eviction order is total and tie-free without a clock, without a [Unix]
     dependency, and without coupling dedup to wall time. The tick is this
     process's own bookkeeping and is never journaled: what is durable is the
     floor, and recency is a property of the mirror that holds it. *)
  type entry =
    { seq : Msg_seq.t
    ; water : Store_water.t
    ; tick : int64
    }

  (* [count] is maintained incrementally rather than folded on demand: under a
     cap it is read on every [persist], and folding two maps per persisted
     frame to learn a number the inserts already know is work for nothing. *)
  type t =
    { live : entry Tab_map.t Rep_map.t
    ; tick : int64
    ; count : int
    }

  let empty : t = { live = Rep_map.empty; tick = 0L; count = 0 }

  let apply (t : t) (e : Guard_sink.event) : t =
    match e with
    | Guard_sink.Advance { replica; tab; seq; water } ->
      let tick = Int64.add t.tick 1L in
      let tabs =
        Rep_map.find_opt replica t.live |> Option.value ~default:Tab_map.empty
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
         silent-loss direction R20 exists to forbid.

         A record for a key refreshes that key's recency whichever way the
         merge falls, including the lower-seq case that changes nothing else:
         the journal just named this key, and the entry eviction must not
         take is the one still in use ([Replay_guard.take]'s Duplicate arm
         makes the same call for the same reason). *)
      let floor =
        Tab_map.find_opt tab tabs
        |> Option.fold ~none:{ seq; water; tick }
             ~some:(fun (cur : entry) ->
               if Msg_seq.compare seq cur.seq >= 0 then
                 { seq
                 ; water =
                     (if Store_water.compare water cur.water >= 0 then water
                      else cur.water)
                 ; tick
                 }
               else { cur with tick })
      in
      { live = Rep_map.add replica (Tab_map.add tab floor tabs) t.live
      ; tick
      ; count = (if Tab_map.mem tab tabs then t.count else t.count + 1)
      }
    | Guard_sink.Forget { replica } ->
      Rep_map.find_opt replica t.live
      |> Option.fold ~none:t ~some:(fun (tabs : entry Tab_map.t) ->
             { t with
               live = Rep_map.remove replica t.live
             ; count = t.count - Tab_map.cardinal tabs
             })

  let of_events (events : Guard_sink.event list) : t =
    List.fold_left apply empty events

  let find_stamped ~(replica : Tea_core.Crdt.Replica.t)
      ~(tab : Tea_core.Prim.Tab_id.t) (t : t) :
      (Msg_seq.t * Store_water.t) option =
    Rep_map.find_opt replica t.live
    |> Fun.flip Option.bind (Tab_map.find_opt tab)
    |> Option.map (fun (e : entry) -> (e.seq, e.water))

  let find ~(replica : Tea_core.Crdt.Replica.t) ~(tab : Tea_core.Prim.Tab_id.t)
      (t : t) : Msg_seq.t option =
    find_stamped ~replica ~tab t |> Option.map fst

  let cardinal (t : t) : int = t.count

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
     so the operator line names both and clears neither.

     Surviving entries keep their ticks: the filter is a boot-time judgement
     about durability, never a statement about which floor is live. *)
  let filter ~(head : Tea_core.Crdt.Replica.t -> Store_water.t option) (t : t) :
      t * verdict =
    let live, verdict =
      Rep_map.fold
        (fun (replica : Tea_core.Crdt.Replica.t) (tabs : entry Tab_map.t)
             ((acc, verdict) : entry Tab_map.t Rep_map.t * verdict) ->
          let head_water = head replica in
          let kept_tabs, verdict =
            Tab_map.fold
              (fun (tab : Tea_core.Prim.Tab_id.t) (e : entry)
                   ((kept_tabs, verdict) : entry Tab_map.t * verdict) ->
                if Store_water.equal e.water Store_water.bottom then
                  ( Tab_map.add tab e kept_tabs
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
                      if Store_water.covers ~head ~floor:e.water then
                        ( Tab_map.add tab e kept_tabs
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
        t.live
        ( Rep_map.empty
        , { kept = 0; dropped_behind = 0; dropped_no_branch = 0; unwitnessed = 0 }
        )
    in
    ({ t with live; count = verdict.kept }, verdict)

  (* Everything below is internal to [Durable_guard]: the mirror's cap is the
     guard's business, not its callers'. *)

  (* The least-recently-touched floor across both levels, as a fold rather
     than [min_binding], which raises on an empty map: a fold is what keeps
     this total. *)
  let oldest (t : t) :
      (Tea_core.Crdt.Replica.t * Tea_core.Prim.Tab_id.t) option =
    Rep_map.fold
      (fun (replica : Tea_core.Crdt.Replica.t) (tabs : entry Tab_map.t)
           (acc : ((Tea_core.Crdt.Replica.t * Tea_core.Prim.Tab_id.t) * int64)
                  option) ->
        Tab_map.fold
          (fun (tab : Tea_core.Prim.Tab_id.t) (e : entry)
               (acc : ((Tea_core.Crdt.Replica.t * Tea_core.Prim.Tab_id.t) * int64)
                      option) ->
            Option.fold acc
              ~none:(Some ((replica, tab), e.tick))
              ~some:(fun
                  ((_, best) :
                    (Tea_core.Crdt.Replica.t * Tea_core.Prim.Tab_id.t) * int64)
                ->
                if Int64.compare e.tick best < 0 then
                  Some ((replica, tab), e.tick)
                else acc))
          tabs acc)
      t.live None
    |> Option.map fst

  let remove ~(replica : Tea_core.Crdt.Replica.t)
      ~(tab : Tea_core.Prim.Tab_id.t) (t : t) : t =
    Rep_map.find_opt replica t.live
    |> Option.fold ~none:t ~some:(fun (tabs : entry Tab_map.t) ->
           if Tab_map.mem tab tabs then
             let kept = Tab_map.remove tab tabs in
             { t with
               live =
                 (if Tab_map.is_empty kept then Rep_map.remove replica t.live
                  else Rep_map.add replica kept t.live)
             ; count = t.count - 1
             }
           else t)

  (* Evict least-recently-touched floors until the mirror is within [bound].
     Eviction REMOVES a floor, which is the accept direction: an absent floor
     admits any sequence number and a kept floor is never lowered, so the
     floors-never-lower law is untouched. Total and tail-recursive: each step
     strictly decreases [count], and [oldest] answers [None] only for an empty
     mirror, whose count is zero and therefore already within every bound. *)
  let rec cap ~(bound : Replay_guard.Bound.t) (t : t) : t =
    if t.count <= Replay_guard.Bound.to_int bound then t
    else
      oldest t
      |> Option.fold ~none:(fun () -> t)
           ~some:(fun
               ((replica, tab) :
                 Tea_core.Crdt.Replica.t * Tea_core.Prim.Tab_id.t)
               () -> cap ~bound (remove ~replica ~tab t))
      |> fun step -> step ()

  (* Refresh a floor's recency without touching the floor itself: the mirror
     was asked about this key and answered, so it is live and eviction must
     not take it next. Absent keys are a no-op — there is nothing to keep. *)
  let touch ~(replica : Tea_core.Crdt.Replica.t)
      ~(tab : Tea_core.Prim.Tab_id.t) (t : t) : t =
    Rep_map.find_opt replica t.live
    |> Option.fold ~none:(fun () -> t)
         ~some:(fun (tabs : entry Tab_map.t) () ->
           Tab_map.find_opt tab tabs
           |> Option.fold ~none:(fun () -> t)
                ~some:(fun (e : entry) () ->
                  let tick = Int64.add t.tick 1L in
                  { t with
                    tick
                  ; live =
                      Rep_map.add replica
                        (Tab_map.add tab { e with tick } tabs)
                        t.live
                  })
           |> fun step -> step ())
    |> fun step -> step ()
end

type t =
  { cell : Replay_guard.Cell.cell
  ; mutable floors : Floors.t
  ; sink : Guard_sink.t
  ; mirror : Replay_guard.Bound.t
  ; fence : unit -> unit Lwt.t
  }

(* The fallback is unreachable: both bounds are strictly positive by
   construction, so the max of their product and a non-negative cap is
   strictly positive. It exists because [Bound.of_int] is total and this
   function must be too — no exception, no assertion. *)
let default_mirror ~(sessions : Replay_guard.Bound.t)
    ~(tabs : Replay_guard.Bound.t) ?(journal_cap : int = 0) () :
    Replay_guard.Bound.t =
  Int.max
    (4 * Replay_guard.Bound.to_int sessions * Replay_guard.Bound.to_int tabs)
    journal_cap
  |> Replay_guard.Bound.of_int
  |> Option.value ~default:sessions

let v ~(sessions : Replay_guard.Bound.t) ~(tabs : Replay_guard.Bound.t)
    ?(mirror : Replay_guard.Bound.t option)
    ?(fence : unit -> unit Lwt.t = fun () -> Lwt.return_unit)
    ~(sink : Guard_sink.t) ~(floors : Floors.t) () : t =
  let mirror =
    Option.value mirror ~default:(default_mirror ~sessions ~tabs ())
  in
  (* Floors read back at boot are capped on the way in, so the bound is an
     invariant of the value rather than a promise about its callers. At the
     documented composition the cap is at least the journal's own, so this
     drops nothing; a caller that under-provisions it loses floors in the
     accept direction and learns so from [mirror_bound]. *)
  { cell = Replay_guard.Cell.v ~sessions ~tabs
  ; floors = Floors.cap ~bound:mirror floors
  ; sink
  ; mirror
  ; fence
  }

let mirror_bound (t : t) : Replay_guard.Bound.t = t.mirror

let take (t : t) ~(replica : Tea_core.Crdt.Replica.t)
    ~(tab : Tea_core.Prim.Tab_id.t) ~(seq : Msg_seq.t) : Replay_guard.verdict =
  (* A Cell miss — never heard from, or evicted — asks the mirror before the
     verdict, so the durable floor is what an absent entry degrades to instead
     of "accept anything". Closures on both branches, applied once (the
     [Option.fold] eagerness discipline). *)
  let seed_from_mirror () : unit =
    Option.iter
      (fun (floor : Msg_seq.t) ->
        Replay_guard.Cell.seed t.cell ~replica ~tab ~high:floor;
        (* The mirror was consulted and answered, so this key is live and
           eviction must not take it next. A Cell HIT never reaches here and
           deliberately leaves mirror recency alone: the mirror was not asked,
           and a key the Cell is already serving does not need the mirror to
           hold its place. *)
        t.floors <- Floors.touch ~replica ~tab t.floors)
      (Floors.find ~replica ~tab t.floors)
  in
  Replay_guard.high_water (Replay_guard.Cell.snapshot t.cell) ~replica ~tab
  |> Option.fold ~none:seed_from_mirror ~some:(fun (_ : Msg_seq.t) () -> ())
  |> fun sync -> sync ();
  Replay_guard.Cell.take t.cell ~replica ~tab ~seq

(* Cell-only, deliberately: release exists precisely because the failure
   arrived BEFORE [persist], so the mirror and the journal have nothing to
   unwind - a premise [persist] now enforces rather than hopes for, by
   catching a (contract-violating) rejecting sink so no rejection born
   after its mirror advance can reach a release barrier. The
   conditionality lives in [Replay_guard.release]. *)
let release (t : t) ~(replica : Tea_core.Crdt.Replica.t)
    ~(tab : Tea_core.Prim.Tab_id.t) ~(seq : Msg_seq.t) : unit =
  Replay_guard.Cell.release t.cell ~replica ~tab ~seq

let persist (t : t) ~(replica : Tea_core.Crdt.Replica.t)
    ~(tab : Tea_core.Prim.Tab_id.t) ~(seq : Msg_seq.t)
    ~(water : Store_water.t) : (unit, Guard_sink.err) result Lwt.t =
  let e = Guard_sink.Advance { replica; tab; seq; water } in
  (* Mirror first: if the sink yields and the Cell evicts this entry before
     the append lands, the next frame's miss still re-seeds at the true
     water. The raise enforces the cap in the same step, so the mirror is
     bounded at its only growth point. *)
  t.floors <- Floors.cap ~bound:t.mirror (Floors.apply t.floors e);
  (* The sink contract is total-return: [append] resolves [Error], never
     rejects. [Guard_sink.t] is a public record, so a caller-supplied sink
     can break that anyway - and a rejection escaping [persist] would be
     born AFTER the mirror line above advanced, cross the WS release
     barrier, and roll the Cell back behind the mirror: a desync that
     re-opens a seq the mirror already floors and, on the fuel arm, breaks
     once-ever. Caught here, at the one seam both tiers share, a rejecting
     sink degrades to the same audible [Error] the honest sinks already
     return. *)
  (* The commit fence (roadmap step 20, R11) runs strictly between the
     mirror advance above and the sink append, unconditionally — a fence
     that inspected the water would be a policy fork at the one seam both
     tiers share, and a bottom-water floor's wasted call is cheap
     (irmin-pack short-circuits an empty buffer). It lives INSIDE this
     catching seam for the same reason the append does: a rejecting fence
     would otherwise be born after the mirror advance, cross the WS release
     barrier, and desync the Cell — caught here it degrades to the same
     audible [Error], with the floor never appended (the duplicate
     direction, never loss). *)
  Lwt.catch
    (fun () ->
      let open Lwt.Syntax in
      let* () = t.fence () in
      t.sink.Guard_sink.append e)
    (fun (exn : exn) ->
      Lwt.return (Error (Guard_sink.Io (Printexc.to_string exn))))

let forget (t : t) ~(replica : Tea_core.Crdt.Replica.t) :
    (unit, Guard_sink.err) result Lwt.t =
  let open Lwt.Syntax in
  let* appended = t.sink.Guard_sink.append (Guard_sink.Forget { replica }) in
  t.floors <- Floors.apply t.floors (Guard_sink.Forget { replica });
  Replay_guard.Cell.forget t.cell ~replica;
  Lwt.return appended

let snapshot (t : t) : Replay_guard.t = Replay_guard.Cell.snapshot t.cell
let floors (t : t) : Floors.t = t.floors
