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

(** One step of the shared 64-bit LCG (Knuth's MMIX constants) used by the
    seeded property loops: mutate [state] and return the new raw value.
    Callers extract whatever bits they need. *)
let lcg_next (state : int64 ref) : int64 =
  state := Int64.add (Int64.mul !state 6364136223846793005L) 1442695040888963407L;
  !state
