(* The probe absorbs its own throw (a restricted origin can refuse the
   property read) into [None]: a page that cannot even ask for the lock runs
   memory-only, it does not wedge. *)
let locks () : Ojs.t option =
  match
    Ojs.get_prop_ascii Ojs.global "navigator"
    |> Ojs.option_of_js (fun (n : Ojs.t) -> n)
    |> Option.map (fun (n : Ojs.t) ->
           Ojs.option_of_js
             (fun (l : Ojs.t) -> l)
             (Ojs.get_prop_ascii n "locks"))
    |> Option.join
  with
  | v -> v
  | exception (_ : exn) -> None

let supported () : bool = Option.is_some (locks ())

(* Duplicated in idb.ml on purpose: two six-line shells beat a third module
   on the audited lib/ surface (the R7 file-count pin). *)
let defer (f : unit -> unit) : unit =
  let (_ : Ojs.t) =
    Ojs.call Ojs.global "setTimeout"
      [| Ojs.fun_to_js 1 (fun (_ : Ojs.t) -> f ()); Ojs.int_to_js 0 |]
  in
  ()

let acquire ~(name : string) ~(granted : bool -> unit) : unit =
  (locks ()
   |> Option.fold
        ~none:(fun () -> defer (fun () -> granted false))
        ~some:(fun (l : Ojs.t) () ->
          (* Exactly-once across the three answer paths: the grant callback
             (held or not), the request promise's rejection, and a
             synchronous refusal. A held request never settles, so the
             rejection arm can only fire when the callback never ran. *)
          let fired = ref false in
          let fire (b : bool) : unit =
            if !fired then () else (
              fired := true;
              granted b)
          in
          let opts =
            Ojs.obj
              [| ("mode", Ojs.string_to_js "exclusive")
               ; ("ifAvailable", Ojs.bool_to_js true)
              |]
          in
          let cb =
            Ojs.fun_to_js 1 (fun (lock : Ojs.t) ->
                let held =
                  Option.is_some
                    (Ojs.option_of_js (fun (x : Ojs.t) -> x) lock)
                in
                fire held;
                if held then
                  (* The held promise never resolves: the lock lives exactly
                     as long as the page (F12). *)
                  Ojs.new_obj
                    (Ojs.get_prop_ascii Ojs.global "Promise")
                    [| Ojs.fun_to_js 2
                         (fun (_ : Ojs.t) (_ : Ojs.t) -> Ojs.unit_to_js ())
                    |]
                else Ojs.null)
          in
          (* A rejecting request (a document no longer fully active; a
             context that exposes navigator.locks but refuses the call) and
             a synchronous refusal both route to the not-granted arm: the
             caller's gate always resolves. *)
          match
            Ojs.call
              (Ojs.call l "request" [| Ojs.string_to_js name; opts; cb |])
              "catch"
              [| Ojs.fun_to_js 1 (fun (_ : Ojs.t) ->
                     fire false;
                     Ojs.null)
              |]
          with
          | (_ : Ojs.t) -> ()
          | exception (_ : exn) -> defer (fun () -> fire false)))
    ()
