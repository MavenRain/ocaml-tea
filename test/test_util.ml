(** Helpers shared across the test executables (the [test/dune] stanza links
    every module in this directory into each test). *)

(** Bounded wait for an asynchronous delivery: poll [pred] every 10ms for up
    to ~2s. Polling avoids the lost-wakeup race of a condition variable
    signalled before its waiter registers. *)
let await pred =
  let open Lwt.Syntax in
  let rec go n =
    if pred () then Lwt.return true
    else if n = 0 then Lwt.return false
    else
      let* () = Lwt_unix.sleep 0.01 in
      go (n - 1)
  in
  go 200
