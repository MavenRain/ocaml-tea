(** The R16 drift alarm (roadmap step 24): a compile-time pin that Dream
    still exports the session-payload family under the exact names the sink
    gate's member layer scans for. The five typed bindings below are the
    whole content - value references only, no server runs, nothing is
    called. If a future Dream renames or retypes any of these, this file
    stops compiling, which is the loud alarm that keeps the member scan from
    going vacuous (hunting names that no longer exist while an app calls the
    new ones freely). In the other direction the sink gate pins THIS file's
    bytes: each banned name must occur here exactly once, so a deleted
    binding reads zero and fails the gate rather than rotting silently.

    This file is the single member-allowlisted file in
    [test/sink_gate_test.ml] - the one sanctioned spelling of each banned
    name - and it must never grow a real payload call. Monotone state
    belongs on the session's Irmin branch (register R16). *)

let failures : int ref = ref 0

let check (name : string) (cond : bool) : unit =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    failures := !failures + 1)

(* One binding per banned member, each name spelled exactly once. *)
let (_ : Dream.request -> string -> string option) = Dream.session_field
let (_ : Dream.request -> string -> string -> unit Dream.promise) = Dream.set_session_field
let (_ : Dream.request -> string -> unit Dream.promise) = Dream.drop_session_field
let (_ : Dream.request -> (string * string) list) = Dream.all_session_fields
let (_ : Dream.request -> float) = Dream.session_expires_at

let () =
  (* The condition is discharged by the compiler: reaching this line means
     the five typed bindings above elaborated against the installed Dream.
     The runtime line exists so the suite records the pin. *)
  check
    "probe: the five R16 payload accessors exist in Dream at their pinned types (compile pin)"
    true;
  if !failures = 0 then (
    Printf.printf "\nThe R16 payload family still exists upstream; the gate scans live names.\n%!";
    exit 0)
  else (
    Printf.printf "\n%d probe failure(s).\n%!" !failures;
    exit 1)
