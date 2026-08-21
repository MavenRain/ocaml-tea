module LS = Tea_client.Local_store

type db = Ojs.t

let indexed_db () : Ojs.t = Ojs.get_prop_ascii Ojs.global "indexedDB"

(* Reading [window.indexedDB] is itself a throw site (SecurityError on an
   opaque or storage-blocked origin - exactly the environments this probe
   exists for), so the probe absorbs its own throw into [false] rather than
   letting it cross the FFI and wedge the boot gate in Buffering forever. *)
let supported () : bool =
  match
    Option.is_some (Ojs.option_of_js (fun (x : Ojs.t) -> x) (indexed_db ()))
  with
  | v -> v
  | exception (_ : exn) -> false

(* A later-task trampoline for the arms that would otherwise answer
   synchronously. Duplicated in tab_lock.ml on purpose: two six-line shells
   beat a third module on the audited lib/ surface (the R7 file-count pin). *)
let defer (f : unit -> unit) : unit =
  let (_ : Ojs.t) =
    Ojs.call Ojs.global "setTimeout"
      [| Ojs.fun_to_js 1 (fun (_ : Ojs.t) -> f ()); Ojs.int_to_js 0 |]
  in
  ()

(* [request.error.name] with every null guarded: a missing DOMException still
   classifies, to [Other]. The sentinel is distinct from the [Other]
   constructor's own name so a log line never reads as a real DOMException. *)
let error_name (holder : Ojs.t) : string =
  Ojs.get_prop_ascii holder "error"
  |> Ojs.option_of_js (fun (e : Ojs.t) -> e)
  |> Option.fold ~none:"NoDOMException"
       ~some:(fun (e : Ojs.t) ->
         Ojs.get_prop_ascii e "name"
         |> Ojs.option_of_js Ojs.string_of_js
         |> Option.value ~default:"NoDOMException")

(* Absorb a synchronous IndexedDB throw (a closing connection's [transaction]
   call, for one) into the error arm: no exception crosses the FFI. Same
   absorption shape as [Codec.of_json]. The [err] it fires is same-task; the
   protocol's callers are indifferent, and the real browser throws there
   synchronously too. *)
let guarded ~(err : LS.idb_error -> unit) (f : unit -> unit) : unit =
  match f () with
  | () -> ()
  | exception (_ : exn) -> err (LS.Other "SyncThrow")

let open_db ~(name : string) ~(version : int)
    ~(on_versionchange : unit -> unit) ~(on_blocked : unit -> unit)
    ~(ok : db -> unit) ~(err : LS.idb_error -> unit) : unit =
  if not (supported ()) then defer (fun () -> err LS.Unsupported)
  else
    guarded ~err (fun () ->
        let request =
          Ojs.call (indexed_db ()) "open"
            [| Ojs.string_to_js name; Ojs.int_to_js version |]
        in
        Ojs.set_prop_ascii request "onupgradeneeded"
          (Ojs.fun_to_js 1 (fun (_ : Ojs.t) ->
               let db = Ojs.get_prop_ascii request "result" in
               let names = Ojs.get_prop_ascii db "objectStoreNames" in
               let has =
                 Ojs.bool_of_js
                   (Ojs.call names "contains"
                      [| Ojs.string_to_js LS.store_name |])
               in
               if has then ()
               else
                 let (_ : Ojs.t) =
                   (* Out-of-line keys: [replace_all] puts (value, key). *)
                   Ojs.call db "createObjectStore"
                     [| Ojs.string_to_js LS.store_name |]
                 in
                 ()));
        Ojs.set_prop_ascii request "onblocked"
          (Ojs.fun_to_js 1 (fun (_ : Ojs.t) -> on_blocked ()));
        Ojs.set_prop_ascii request "onerror"
          (Ojs.fun_to_js 1 (fun (_ : Ojs.t) ->
               err (LS.classify (error_name request))));
        Ojs.set_prop_ascii request "onsuccess"
          (Ojs.fun_to_js 1 (fun (_ : Ojs.t) ->
               let db = Ojs.get_prop_ascii request "result" in
               (* The shell closes on versionchange BEFORE telling the
                  protocol: an old page must never hold a future-version
                  database open (D25's obligation to the future). *)
               Ojs.set_prop_ascii db "onversionchange"
                 (Ojs.fun_to_js 1 (fun (_ : Ojs.t) ->
                      let (_ : Ojs.t) = Ojs.call db "close" [||] in
                      on_versionchange ()));
               ok db)))

let read_all (db : db) ~(store : string) ~(ok : string list -> unit)
    ~(err : LS.idb_error -> unit) : unit =
  guarded ~err (fun () ->
      let txn =
        Ojs.call db "transaction"
          [| Ojs.string_to_js store; Ojs.string_to_js "readonly" |]
      in
      let os = Ojs.call txn "objectStore" [| Ojs.string_to_js store |] in
      let request = Ojs.call os "getAll" [||] in
      Ojs.set_prop_ascii request "onerror"
        (Ojs.fun_to_js 1 (fun (_ : Ojs.t) ->
             err (LS.classify (error_name request))));
      Ojs.set_prop_ascii request "onsuccess"
        (Ojs.fun_to_js 1 (fun (_ : Ojs.t) ->
             ok
               (Ojs.list_of_js Ojs.string_of_js
                  (Ojs.get_prop_ascii request "result")))))

let clear_all (db : db) ~(store : string) ~(ok : unit -> unit)
    ~(err : LS.idb_error -> unit) : unit =
  guarded ~err (fun () ->
      let txn =
        Ojs.call db "transaction"
          [| Ojs.string_to_js store; Ojs.string_to_js "readwrite" |]
      in
      let os = Ojs.call txn "objectStore" [| Ojs.string_to_js store |] in
      let (_ : Ojs.t) = Ojs.call os "clear" [||] in
      Ojs.set_prop_ascii txn "oncomplete"
        (Ojs.fun_to_js 1 (fun (_ : Ojs.t) -> ok ()));
      Ojs.set_prop_ascii txn "onabort"
        (Ojs.fun_to_js 1 (fun (_ : Ojs.t) ->
             err (LS.classify (error_name txn)))))

let replace_all (db : db) ~(store : string) ~(key : string) ~(value : string)
    ~(ok : unit -> unit) ~(err : LS.idb_error -> unit) : unit =
  guarded ~err (fun () ->
      let txn =
        Ojs.call db "transaction"
          [| Ojs.string_to_js store; Ojs.string_to_js "readwrite" |]
      in
      let os = Ojs.call txn "objectStore" [| Ojs.string_to_js store |] in
      (* Clear then put, back to back in this one turn, on this ONE
         transaction: IndexedDB auto-commits when the turn ends, so the pair
         is atomic and a crash can never split the stores' truth (D25). *)
      let (_ : Ojs.t) = Ojs.call os "clear" [||] in
      let (_ : Ojs.t) =
        Ojs.call os "put" [| Ojs.string_to_js value; Ojs.string_to_js key |]
      in
      (* Completion reads the transaction, not the requests: [oncomplete] is
         the commit, and a failed request (quota, most likely) aborts the
         whole transaction, so [onabort] is the single failure funnel. *)
      Ojs.set_prop_ascii txn "oncomplete"
        (Ojs.fun_to_js 1 (fun (_ : Ojs.t) -> ok ()));
      Ojs.set_prop_ascii txn "onabort"
        (Ojs.fun_to_js 1 (fun (_ : Ojs.t) ->
             err (LS.classify (error_name txn)))))
