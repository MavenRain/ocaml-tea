module LS = Tea_client.Local_store

type txn = { birth : int }

type t =
  { mutable store : (string * string) list
  ; mutable queue : (unit -> unit) list (* oldest first *)
  ; mutable injected : LS.idb_error option
  ; mutable turn : int
  ; mutable on_versionchange : (unit -> unit) option
  }

(* [BACKEND.open_db] mints the handle rather than receiving one, so the fake
   routes it to the most recently created instance. Native checks run
   sequentially, one fresh fake per rig. *)
let current : t option ref = ref None

let create () : t =
  let t =
    { store = []
    ; queue = []
    ; injected = None
    ; turn = 0
    ; on_versionchange = None
    }
  in
  current := Some t;
  t

let inject (t : t) (e : LS.idb_error) : unit = t.injected <- Some e

let take (t : t) : LS.idb_error option =
  let i = t.injected in
  t.injected <- None;
  i

let push (t : t) (f : unit -> unit) : unit = t.queue <- t.queue @ [ f ]

let tick (t : t) : bool =
  (* The turn advances on every call, delivered or not: a transaction opened
     before any tick is dead after it, exactly as a real transaction is dead
     after its opening task returns. *)
  t.turn <- t.turn + 1;
  match t.queue with
  | [] -> false
  | f :: rest ->
    t.queue <- rest;
    f ();
    true

let rec drain (t : t) : unit = if tick t then drain t else ()
let rows (t : t) : (string * string) list = t.store

let version_change (t : t) : unit =
  t.on_versionchange |> Option.fold ~none:() ~some:(fun (f : unit -> unit) -> push t f)

type db = t

let txn_open (t : t) : txn = { birth = t.turn }

let txn_request (t : t) (tx : txn) ~(op : [ `Clear | `Put of string * string ]) :
    (unit, LS.idb_error) result =
  if tx.birth <> t.turn then Error (LS.Other "TransactionInactiveError")
  else (
    (match op with
     | `Clear -> t.store <- []
     | `Put (k, v) ->
       t.store <-
         (k, v)
         :: List.filter
              (fun ((k', (_ : string)) : string * string) ->
                not (String.equal k' k))
              t.store);
    Ok ())

let open_db ~name:(_ : string) ~version:(_ : int)
    ~(on_versionchange : unit -> unit) ~(on_blocked : unit -> unit)
    ~(ok : db -> unit) ~(err : LS.idb_error -> unit) : unit =
  !current
  |> Option.fold ~none:()
       ~some:(fun (t : t) ->
         t.on_versionchange <- Some on_versionchange;
         (take t
          |> Option.fold
               ~none:(fun () -> push t (fun () -> ok t))
               ~some:(fun (e : LS.idb_error) () ->
                 match e with
                 | LS.Blocked ->
                   (* Blocked then success: the double-fire the once-guard
                      must absorb. *)
                   push t (fun () -> on_blocked ());
                   push t (fun () -> ok t)
                 | LS.Unsupported | LS.Version_error | LS.Quota_exceeded
                 | LS.Not_found | LS.Other (_ : string) ->
                   push t (fun () -> err e)))
           ())

let read_all (t : db) ~store:(_ : string) ~(ok : string list -> unit)
    ~(err : LS.idb_error -> unit) : unit =
  (take t
   |> Option.fold
        ~none:(fun () ->
          let values = List.map (fun ((_, v) : string * string) -> v) t.store in
          push t (fun () -> ok values))
        ~some:(fun (e : LS.idb_error) () -> push t (fun () -> err e)))
    ()

let clear_all (t : db) ~store:(_ : string) ~(ok : unit -> unit)
    ~(err : LS.idb_error -> unit) : unit =
  (take t
   |> Option.fold
        ~none:(fun () ->
          let tx = txn_open t in
          txn_request t tx ~op:`Clear
          |> Result.fold
               ~ok:(fun () -> push t (fun () -> ok ()))
               ~error:(fun (e : LS.idb_error) -> push t (fun () -> err e)))
        ~some:(fun (e : LS.idb_error) () -> push t (fun () -> err e)))
    ()

let replace_all (t : db) ~store:(_ : string) ~(key : string) ~(value : string)
    ~(ok : unit -> unit) ~(err : LS.idb_error -> unit) : unit =
  (take t
   |> Option.fold
        ~none:(fun () ->
          (* One transaction, both requests in the opening turn: the same
             discipline the jsoo shell promises. An injected failure above
             never reaches here, so a failed checkpoint leaves the rows
             untouched, exactly as a real aborted transaction does. *)
          let tx = txn_open t in
          Result.bind (txn_request t tx ~op:`Clear) (fun () ->
              txn_request t tx ~op:(`Put (key, value)))
          |> Result.fold
               ~ok:(fun () -> push t (fun () -> ok ()))
               ~error:(fun (e : LS.idb_error) -> push t (fun () -> err e)))
        ~some:(fun (e : LS.idb_error) () -> push t (fun () -> err e)))
    ()
