(** Native proof of the reconnect/rebase halves of roadmap step 8 (D8, D9).

    Both live in [Tea_client] as pure state machines over abstract socket and
    timer types, which is the whole point: the browser supplies
    [Js_browser.WebSocket.t] and [Window.timeout_id], and this file supplies
    ordinary records, so the retry ladder, the stale-socket guard, the outbox
    ordering and the rebase policy are all decided off the browser.

    The socket stand-in is a record and never an [int]: the guard these tests
    exist to pin is PHYSICAL equality, and two [int]s that are equal are also
    physically equal, so an int would make the test pass no matter which
    equality the implementation used. *)

module Rc = Tea_client.Reconnect
module Outbox = Tea_client.Rebase.Outbox
module Merge_spec = Tea_core.Merge_spec

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

(* Distinct-by-construction stand-ins: [mk_sock 1 == mk_sock 1] is false even
   though the two are structurally equal.

   The fields are [mutable] for exactly that reason, and never written. An
   immutable record with constant fields is a compile-time constant that OCaml
   statically allocates and SHARES, so [mk_sock 2 == mk_sock 2] came out true
   and the stale-socket checks below passed vacuously. A mutable block is
   always allocated fresh. *)
type sock = { mutable sock_id : int }
type timer = { mutable timer_id : int }

let mk_sock id = { sock_id = id }
let mk_timer id = { timer_id = id }

(* --- the retry ladder ---------------------------------------------------- *)

let () =
  print_endline "\n--- D8: Backoff ladder ---";
  let b0 = Rc.Backoff.initial in
  let b1 = Rc.Backoff.bump b0 in
  let b2 = Rc.Backoff.bump b1 in
  check "initial delay is the 500ms base" (Rc.Backoff.to_ms b0 = 500);
  check "one bump doubles to 1s" (Rc.Backoff.to_ms b1 = 1000);
  check "two bumps double again to 2s" (Rc.Backoff.to_ms b2 = 2000);
  (* Walk the ladder to the cap rather than asserting a hand-computed number
     of steps: the property is saturation, not the step count. *)
  let capped =
    List.fold_left (fun b (_ : int) -> Rc.Backoff.bump b) b0 (List.init 20 Fun.id)
  in
  check "the ladder saturates at the 30s cap" (Rc.Backoff.to_ms capped = 30_000);
  check "bumping a capped ladder is idempotent"
    (Rc.Backoff.to_ms (Rc.Backoff.bump capped) = 30_000);
  check "no rung ever exceeds the cap"
    (List.for_all
       (fun n ->
         let b = List.fold_left (fun b (_ : int) -> Rc.Backoff.bump b) b0 (List.init n Fun.id) in
         Rc.Backoff.to_ms b <= Rc.Backoff.cap_ms)
       (List.init 20 Fun.id))

(* --- the four states ----------------------------------------------------- *)

let () =
  print_endline "\n--- D8: state observations ---";
  let s = mk_sock 1 in
  let t = mk_timer 1 in
  let down : (sock, timer) Rc.t = Rc.down in
  let opening = Rc.opening ~sock:s ~next:Rc.Backoff.initial in
  let up = Rc.on_up ~sock:s opening in
  let waiting = Rc.waiting ~timer:t ~next:Rc.Backoff.initial in
  check "Down is not active" (not (Rc.active down));
  check "Opening is active" (Rc.active opening);
  check "Up is active" (Rc.active up);
  check "Waiting is active even with no socket at all" (Rc.active waiting);
  check "only Up is sendable" (Option.is_some (Rc.sendable up));
  check "Opening is NOT sendable (a send on CONNECTING raises)"
    (Option.is_none (Rc.sendable opening));
  check "Waiting is not sendable" (Option.is_none (Rc.sendable waiting));
  check "Down is not sendable" (Option.is_none (Rc.sendable down));
  check "the sendable socket is the one that was confirmed"
    (Option.fold ~none:false ~some:(fun w -> w == s) (Rc.sendable up))

(* --- the stale-socket guard ---------------------------------------------- *)

let () =
  print_endline "\n--- D8: stale sockets are inert ---";
  let s1 = mk_sock 1 in
  let s2 = mk_sock 2 in
  let opening = Rc.opening ~sock:s1 ~next:Rc.Backoff.initial in
  check "on_up on a socket the machine does not hold leaves Opening"
    (Option.is_none (Rc.sendable (Rc.on_up ~sock:s2 opening)));
  check "on_up on the held socket confirms it"
    (Option.is_some (Rc.sendable (Rc.on_up ~sock:s1 opening)));
  let stale_close (c : (sock, timer) Rc.t) =
    match Rc.on_close ~sock:s2 c with
    | Rc.Stale -> true
    | Rc.Reopen_after { delay_ms = (_ : int); next = (_ : Rc.Backoff.t) } -> false
  in
  check "a superseded socket's close on Opening is Stale" (stale_close opening);
  check "a superseded socket's close on Up is Stale"
    (stale_close (Rc.on_up ~sock:s1 opening));
  (* The equal-but-not-identical case: this is what fails if the guard is ever
     softened from [==] to a structural test. *)
  check "a DIFFERENT socket with the same contents is still Stale"
    (stale_close (Rc.opening ~sock:(mk_sock 2) ~next:Rc.Backoff.initial));
  check "a close while Down is Stale (an intentional stop never reconnects)"
    (stale_close (Rc.down : (sock, timer) Rc.t));
  check "a close while already Waiting is Stale (no second timer)"
    (stale_close (Rc.waiting ~timer:(mk_timer 1) ~next:Rc.Backoff.initial))

(* --- the escalation ladder as the runtime drives it ---------------------- *)

(* One reconnect attempt: classify the close, and report the delay the runtime
   would arm plus the state it would park in. Mirrors [on_socket_close] +
   [open_socket] in [Tea_client_run] with the effects removed. *)
let attempt_after_close ~(sock : sock) (c : (sock, timer) Rc.t) ~(next_sock : sock) :
    (int * (sock, timer) Rc.t) option =
  match Rc.on_close ~sock c with
  | Rc.Stale -> None
  | Rc.Reopen_after { delay_ms; next } ->
    (* the runtime parks in Waiting, then the timer fires into Opening *)
    let (_ : (sock, timer) Rc.t) = Rc.waiting ~timer:(mk_timer delay_ms) ~next in
    Some (delay_ms, Rc.opening ~sock:next_sock ~next)

let () =
  print_endline "\n--- D8: escalation across repeated failures ---";
  (* Every socket is bound once and reused: the close of attempt N must name
     the very object attempt N opened, or the guard classifies it Stale and the
     ladder is never exercised at all. *)
  let s1 = mk_sock 1 in
  let s2 = mk_sock 2 in
  let s3 = mk_sock 3 in
  let s4 = mk_sock 4 in
  let s5 = mk_sock 5 in
  let up = Rc.on_up ~sock:s1 (Rc.opening ~sock:s1 ~next:Rc.Backoff.initial) in
  (* A link that was up drops: the first retry is fast, because reaching Up is
     evidence the server is reachable. *)
  let d1, opening2 =
    attempt_after_close ~sock:s1 up ~next_sock:s2 |> Option.value ~default:(-1, Rc.down)
  in
  check "a dropped live link retries at the 500ms base" (d1 = 500);
  (* That attempt never confirms and dies too: the ladder must have advanced,
     which is the whole reason [Opening] carries it. *)
  let d2, opening3 =
    attempt_after_close ~sock:s2 opening2 ~next_sock:s3
    |> Option.value ~default:(-1, Rc.down)
  in
  check "a failed reconnect escalates to 1s" (d2 = 1000);
  let d3, opening4 =
    attempt_after_close ~sock:s3 opening3 ~next_sock:s4
    |> Option.value ~default:(-1, Rc.down)
  in
  check "a second failed reconnect escalates to 2s" (d3 = 2000);
  (* And a success resets it: the next outage starts at the base again. *)
  let recovered = Rc.on_up ~sock:s4 opening4 in
  check "reaching Up resets the ladder to the base"
    (attempt_after_close ~sock:s4 recovered ~next_sock:s5
    |> Option.fold ~none:false ~some:(fun ((d : int), (_ : (sock, timer) Rc.t)) -> d = 500))

(* --- teardown ------------------------------------------------------------ *)

let () =
  print_endline "\n--- D8: teardown releases exactly one resource ---";
  let s = mk_sock 1 in
  let t = mk_timer 7 in
  let is_close_of w c =
    match Rc.stop c with
    | Rc.Close_socket ws -> ws == w
    | Rc.Nothing -> false
    | Rc.Cancel_timer (_ : timer) -> false
  in
  let is_cancel_of tm c =
    match Rc.stop c with
    | Rc.Cancel_timer id -> id == tm
    | Rc.Nothing -> false
    | Rc.Close_socket (_ : sock) -> false
  in
  check "stopping Down releases nothing"
    (match Rc.stop (Rc.down : (sock, timer) Rc.t) with
    | Rc.Nothing -> true
    | Rc.Close_socket (_ : sock) -> false
    | Rc.Cancel_timer (_ : timer) -> false);
  check "stopping Opening closes the unconfirmed socket"
    (is_close_of s (Rc.opening ~sock:s ~next:Rc.Backoff.initial));
  check "stopping Up closes the live socket"
    (is_close_of s (Rc.on_up ~sock:s (Rc.opening ~sock:s ~next:Rc.Backoff.initial)));
  check "stopping Waiting cancels the armed timer"
    (is_cancel_of t (Rc.waiting ~timer:t ~next:Rc.Backoff.initial))

(* --- D9: the outbox ------------------------------------------------------ *)

let () =
  print_endline "\n--- D9: outbox ordering and apply-once ---";
  let o = Outbox.empty in
  check "a fresh outbox is empty" (Outbox.is_empty o && Outbox.pending o = 0);
  let o = Outbox.buffer "a" o in
  let o = Outbox.buffer "b" o in
  let o = Outbox.buffer "c" o in
  check "three buffered edits are pending" (Outbox.pending o = 3);
  check "a non-empty outbox is not empty" (not (Outbox.is_empty o));
  let msgs, drained = Outbox.drain o in
  check "drain replays in the order the edits were made (FIFO, not stack)"
    (msgs = [ "a"; "b"; "c" ]);
  check "drain empties the outbox: a replayed edit is never sent twice"
    (Outbox.is_empty drained);
  let again, (_ : string Outbox.t) = Outbox.drain drained in
  check "draining twice yields nothing the second time" (again = []);
  (* Partial flush: the link dies after "a" goes out. The survivors must keep
     their relative order, which is what [Tea_client_run.flush_outbox] relies
     on when [send_or_buffer] re-buffers the tail. *)
  let sent = ref [] in
  let leftover =
    List.fold_left
      (fun acc m ->
        if List.length !sent < 1 then (
          sent := m :: !sent;
          acc)
        else Outbox.buffer m acc)
      Outbox.empty msgs
  in
  let replayed, (_ : string Outbox.t) = Outbox.drain leftover in
  check "a flush interrupted mid-way re-buffers the tail in order"
    (!sent = [ "a" ] && replayed = [ "b"; "c" ])

(* --- D9: rebase under each merge policy ---------------------------------- *)

(* A two-field model whose join keeps BOTH sides, so a clobber is visible: the
   local-only field survives a pushed head iff the head was rebased. *)
type doc =
  { local_edit : string
  ; server_edit : string
  }

let () =
  print_endline "\n--- D9: a pushed head is rebased, not applied raw ---";
  let join a b =
    { local_edit = (if String.equal a.local_edit "" then b.local_edit else a.local_edit)
    ; server_edit = (if String.equal a.server_edit "" then b.server_edit else a.server_edit)
    }
  in
  let local = { local_edit = "mine"; server_edit = "" } in
  let incoming = { local_edit = ""; server_edit = "theirs" } in
  let joined = Tea_client.Rebase.reconcile (Merge_spec.crdt_join join) ~local ~incoming in
  check "Crdt_join keeps the un-acked local edit" (String.equal joined.local_edit "mine");
  check "Crdt_join takes the server's edit too"
    (String.equal joined.server_edit "theirs");
  (* Ancestor-freedom is not decoration: a Three_way policy must be handed
     [None], because the tab does not keep the head it last synced from. *)
  let seen_ancestor = ref (Some ()) in
  let three_way =
    Merge_spec.Three_way
      (fun ~ancestor ~ours ~theirs ->
        seen_ancestor := Option.map (fun (_ : doc) -> ()) ancestor;
        Ok { local_edit = ours.local_edit; server_edit = theirs.server_edit })
  in
  let merged = Tea_client.Rebase.reconcile three_way ~local ~incoming in
  check "Three_way is handed no ancestor" (Option.is_none !seen_ancestor);
  check "Three_way's reconciled value is used"
    (String.equal merged.local_edit "mine" && String.equal merged.server_edit "theirs");
  let conflicting = Merge_spec.Three_way (fun ~ancestor:_ ~ours:_ ~theirs:_ -> Error "no") in
  check "a Three_way conflict yields to the server head (R6)"
    (Tea_client.Rebase.reconcile conflicting ~local ~incoming = incoming);
  check "Last_write_wins yields to the server head outright"
    (Tea_client.Rebase.reconcile Merge_spec.Last_write_wins ~local ~incoming = incoming);
  (* Idempotence is what makes replay safe: the same head folded twice is the
     same state, so a msg replayed from the outbox cannot double-count. *)
  let twice = Tea_client.Rebase.reconcile (Merge_spec.crdt_join join) ~local:joined ~incoming in
  check "rebasing the same head twice changes nothing (replay is safe)"
    (twice = joined)

let () =
  print_endline
    "\nD8/D9 hold: the ladder escalates and resets, stale sockets are inert, \n\
     buffered edits replay in order exactly once, and a pushed head is joined \n\
     with local state rather than overwriting it."
