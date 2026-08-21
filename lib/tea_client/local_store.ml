module Prim = Tea_core.Prim
module Msg_seq = Prim.Msg_seq
module Replica = Tea_core.Crdt.Replica
module Codec = Tea_core.Codec

type idb_error =
  | Unsupported
  | Blocked
  | Version_error
  | Quota_exceeded
  | Not_found
  | Other of string

let classify (name : string) : idb_error =
  match () with
  | () when String.equal name "QuotaExceededError" -> Quota_exceeded
  | () when String.equal name "VersionError" -> Version_error
  | () when String.equal name "NotFoundError" -> Not_found
  | () -> Other name

let db_name ~(title : string) : string = "tea-local:" ^ title
let lock_name ~(title : string) : string = "tea-local-writer:" ^ title
let store_name : string = "records"
let db_version : int = 1

module type BACKEND = sig
  type db

  val open_db :
    name:string ->
    version:int ->
    on_versionchange:(unit -> unit) ->
    on_blocked:(unit -> unit) ->
    ok:(db -> unit) ->
    err:(idb_error -> unit) ->
    unit

  val read_all :
    db -> store:string -> ok:(string list -> unit) -> err:(idb_error -> unit) -> unit

  val replace_all :
    db ->
    store:string ->
    key:string ->
    value:string ->
    ok:(unit -> unit) ->
    err:(idb_error -> unit) ->
    unit

  val clear_all :
    db -> store:string -> ok:(unit -> unit) -> err:(idb_error -> unit) -> unit
end

module Make (A : Tea_core.App.APP) = struct
  type record =
    { replica : Replica.t
    ; tab : Prim.Tab_id.t
    ; next : Msg_seq.t
    ; queue : (Msg_seq.t * A.msg) list
    ; clock_floor : int64
    ; model : A.model
    ; written_at_ms : float
    }

  let record_t : record Repr.t =
    let open Repr in
    record "tea_local_record"
      (fun replica tab next queue clock_floor model written_at_ms ->
        { replica; tab; next; queue; clock_floor; model; written_at_ms })
    |+ field "replica" Replica.t (fun r -> r.replica)
    |+ field "tab" Prim.Tab_id.t (fun r -> r.tab)
    |+ field "next" Msg_seq.t (fun r -> r.next)
    |+ field "queue" (list (pair Msg_seq.t A.msg_t)) (fun r -> r.queue)
    |+ field "clock_floor" int64 (fun r -> r.clock_floor)
    |+ field "model" A.model_t (fun r -> r.model)
    |+ field "written_at_ms" float (fun r -> r.written_at_ms)
    |> sealr

  let to_json (r : record) : string = Codec.to_json record_t r
  let of_json (s : string) : (record, Codec.err) result = Codec.of_json record_t s

  let decode_all (rows : string list) : record list =
    List.filter_map (fun (s : string) -> Result.to_option (of_json s)) rows

  let choose (rows : record list) : record option =
    List.fold_left
      (fun (best : record option) (r : record) ->
        Option.fold ~none:(Some r)
          ~some:(fun (b : record) ->
            if Float.compare r.written_at_ms b.written_at_ms > 0 then Some r
            else Some b)
          best)
      None rows

  let key_of (r : record) : string = Codec.to_json Replica.t r.replica

  let checkpoint_record ~(replica : Replica.t) ~(delivery : A.msg Delivery.t)
      ~(clock_floor : int64) ~(model : A.model) ~(now_ms : float) : record =
    { replica
    ; tab = Delivery.tab delivery
    ; next = Delivery.next_seq delivery
    ; queue = Delivery.unacked delivery
    ; clock_floor
    ; model
    ; written_at_ms = now_ms
    }

  (* [Buffering] holds pre-resolution payloads newest-first (cons is O(1), and
     {!resolve} pays the one reversal). [unconfirmed] carries the adopted
     replica and the adopted queue's highest seq so a mismatched first Hello
     can drop exactly the adopted prefix and nothing born since. *)
  type gate =
    | Buffering of A.msg list
    | Live of
        { delivery : A.msg Delivery.t
        ; unconfirmed : (Replica.t * Msg_seq.t option) option
        }

  let buffering : gate = Buffering []

  type outcome =
    | No_record
    | Adopt of record

  type resolved =
    { gate : gate
    ; paint : A.model option
    ; seed : int64 option
    }

  (* The adopted queue's highest seq: the last entry of an oldest-first list,
     by a total fold rather than an index. *)
  let high_seq (queue : (Msg_seq.t * A.msg) list) : Msg_seq.t option =
    List.fold_left
      (fun (_ : Msg_seq.t option) ((n : Msg_seq.t), (_ : A.msg)) -> Some n)
      None queue

  let feed (buffered : A.msg list) (d : A.msg Delivery.t) : A.msg Delivery.t =
    List.fold_left
      (fun (d : A.msg Delivery.t) (m : A.msg) ->
        Delivery.record m d
        |> Option.fold ~none:d
             ~some:(fun ((d' : A.msg Delivery.t), (_ : Msg_seq.t * A.msg)) -> d'))
      d (List.rev buffered)

  let resolve ~(mint : unit -> Prim.Tab_id.t)
      ~(confirmed : Replica.t option) (outcome : outcome) (gate : gate) :
      resolved =
    match gate with
    | Live
        { delivery = (_ : A.msg Delivery.t)
        ; unconfirmed = (_ : (Replica.t * Msg_seq.t option) option)
        } ->
      { gate; paint = None; seed = None }
    | Buffering buffered ->
      let fresh () : A.msg Delivery.t * record option =
        (Delivery.v ~tab:(mint ()), None)
      in
      let adopted_pair : A.msg Delivery.t * record option =
        (match outcome with
         | No_record -> fresh
         | Adopt r ->
           Delivery.of_persisted ~tab:r.tab ~next:r.next ~queue:r.queue
           |> Option.fold ~none:fresh
                ~some:(fun (d : A.msg Delivery.t) () -> (d, Some r)))
          ()
      in
      let base, adopted = adopted_pair in
      let delivery = feed buffered base in
      (* The floor rides EVERY validated adoption, not just the painting
         one: a same-replica adoption that skipped it would regress the
         durable floor and let two page lives mint colliding dots. *)
      let delivery, unconfirmed, paint, seed =
        adopted
        |> Option.fold
             ~none:(delivery, None, None, None)
             ~some:(fun (r : record) ->
               confirmed
               |> Option.fold
                    ~none:
                      ( delivery
                      , Some (r.replica, high_seq r.queue)
                      , Some r.model
                      , Some r.clock_floor )
                    ~some:(fun (c : Replica.t) ->
                      if Replica.equal c r.replica then
                        (delivery, None, None, Some r.clock_floor)
                      else
                        ( high_seq r.queue
                          |> Option.fold ~none:delivery
                               ~some:(fun (h : Msg_seq.t) ->
                                 Delivery.ack h delivery)
                        , None
                        , None
                        , Some r.clock_floor )))
      in
      { gate = Live { delivery; unconfirmed }; paint; seed }

  type confirm_effect =
    | Idle
    | Flush
    | Prune_and_flush

  let confirm ~(announced : Replica.t) (gate : gate) : gate * confirm_effect =
    match gate with
    | Buffering (_ : A.msg list) -> (gate, Idle)
    | Live { delivery; unconfirmed } ->
      unconfirmed
      |> Option.fold ~none:(gate, Idle)
           ~some:(fun ((rep : Replica.t), (high : Msg_seq.t option)) ->
             if Replica.equal announced rep then
               (Live { delivery; unconfirmed = None }, Flush)
             else
               ( Live
                   { delivery =
                       high
                       |> Option.fold ~none:delivery
                            ~some:(fun (h : Msg_seq.t) ->
                              Delivery.ack h delivery)
                   ; unconfirmed = None
                   }
               , Prune_and_flush ))

  let record_msg (m : A.msg) (gate : gate) :
      gate * (Msg_seq.t * A.msg) option =
    match gate with
    | Buffering buffered -> (Buffering (m :: buffered), None)
    | Live { delivery; unconfirmed } ->
      Delivery.record m delivery
      |> Option.fold ~none:(gate, None)
           ~some:(fun ((d : A.msg Delivery.t), (entry : Msg_seq.t * A.msg)) ->
             ( Live { delivery = d; unconfirmed }
             , unconfirmed
               |> Option.fold ~none:(Some entry)
                    ~some:(fun (_ : Replica.t * Msg_seq.t option) -> None) ))

  let ack (seq : Msg_seq.t) (gate : gate) : gate =
    match gate with
    | Buffering (_ : A.msg list) -> gate
    | Live { delivery; unconfirmed } ->
      Live { delivery = Delivery.ack seq delivery; unconfirmed }

  let delivery (gate : gate) : A.msg Delivery.t option =
    match gate with
    | Buffering (_ : A.msg list) -> None
    | Live { delivery; unconfirmed = (_ : (Replica.t * Msg_seq.t option) option) }
      ->
      Some delivery

  let replay (gate : gate) :
      (Prim.Tab_id.t * (Msg_seq.t * A.msg) list) option =
    match gate with
    | Buffering (_ : A.msg list) -> None
    | Live { delivery; unconfirmed } ->
      unconfirmed
      |> Option.fold
           ~none:(Some (Delivery.tab delivery, Delivery.unacked delivery))
           ~some:(fun (_ : Replica.t * Msg_seq.t option) -> None)

  let flushable (gate : gate) : bool = Option.is_some (replay gate)

  let unconfirmed_replica (gate : gate) : Replica.t option =
    match gate with
    | Buffering (_ : A.msg list) -> None
    | Live { delivery = (_ : A.msg Delivery.t); unconfirmed } ->
      Option.map
        (fun ((rep : Replica.t), (_ : Msg_seq.t option)) -> rep)
        unconfirmed

  module Flow (B : BACKEND) = struct
    type conn =
      { db : B.db
      ; degraded : bool ref
      }

    let boot ~(title : string) ~(k : conn option * record list -> unit) : unit =
      (* Every arm below funnels through [once]: success callbacks arrive
         from a later task and error arms may answer inline, nothing promises
         [on_blocked] and [ok] are exclusive, and firing [k] twice would
         double-resolve the boot gate. A blocked open degrades terminally
         (D25's v1 ruling); the connection that may still open afterwards
         idles unused, and the versionchange arm closes it if a newer schema
         version ever appears. *)
      let fired = ref false in
      let once (v : conn option * record list) : unit =
        if !fired then () else (
          fired := true;
          k v)
      in
      let degraded = ref false in
      B.open_db ~name:(db_name ~title) ~version:db_version
        ~on_versionchange:(fun () -> degraded := true)
        ~on_blocked:(fun () -> once (None, []))
        ~ok:(fun (db : B.db) ->
          B.read_all db ~store:store_name
            ~ok:(fun (rows : string list) ->
              once (Some { db; degraded }, decode_all rows))
            ~err:(fun (_ : idb_error) -> once (None, [])))
        ~err:(fun (_ : idb_error) -> once (None, []))

    let invalidate (c : conn option) ~(k : (unit, idb_error) result -> unit)
        : unit =
      (c
       |> Option.fold
            ~none:(fun () -> k (Ok ()))
            ~some:(fun (conn : conn) () ->
              if !(conn.degraded) then k (Ok ())
              else (
                (* Degrade FIRST: even if the clear fails, no later
                   checkpoint may resurrect a record that lies about
                   [next]. *)
                conn.degraded := true;
                B.clear_all conn.db ~store:store_name
                  ~ok:(fun () -> k (Ok ()))
                  ~err:(fun (e : idb_error) -> k (Error e)))))
        ()

    let checkpoint (c : conn option) (r : record)
        ~(k : (unit, idb_error) result -> unit) : unit =
      (c
       |> Option.fold
            ~none:(fun () -> k (Ok ()))
            ~some:(fun (conn : conn) () ->
              if !(conn.degraded) then k (Ok ())
              else
                B.replace_all conn.db ~store:store_name ~key:(key_of r)
                  ~value:(to_json r)
                  ~ok:(fun () -> k (Ok ()))
                  ~err:(fun (e : idb_error) -> k (Error e))))
        ()
  end
end
