(** The monotonic commit clock in isolation (roadmap step 6, DESIGN §5): a
    frozen wall source mints strictly increasing stamps, a backwards wall
    source cannot lower them, and seeding only ever raises the floor. *)

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

let () =
  let module Clock = Tea_persist.Clock in
  (* Frozen wall clock: successive stamps are wall, wall+1, wall+2. *)
  let frozen = Clock.create ~now:(fun () -> 100L) in
  let s1 = (Clock.next frozen :> int64) in
  let s2 = (Clock.next frozen :> int64) in
  let s3 = (Clock.next frozen :> int64) in
  check "frozen now mints 100, 101, 102" (s1 = 100L && s2 = 101L && s3 = 102L);
  (* A wall clock that jumps backwards cannot drag stamps back. *)
  let readings = ref [ 101L; 50L ] in
  let backwards =
    Clock.create
      ~now:
        (fun () ->
          match !readings with
          | [] -> 50L
          | r :: rest ->
            readings := rest;
            r)
  in
  let b1 = (Clock.next backwards :> int64) in
  let b2 = (Clock.next backwards :> int64) in
  check "backwards now still mints strictly increasing (101 then 102)"
    (b1 = 101L && b2 = 102L);
  (* Seeding below the floor is a no-op; above it, it raises the floor. *)
  let seeded = Clock.create ~now:(fun () -> 100L) in
  let (_ : Clock.stamp) = Clock.next seeded in
  let (_ : Clock.stamp) = Clock.next seeded in
  let (_ : Clock.stamp) = Clock.next seeded in
  Clock.seed seeded 50L;
  let after_low = (Clock.next seeded :> int64) in
  check "seed below the floor is a no-op (next = 103)" (after_low = 103L);
  Clock.seed seeded 500L;
  let after_high = (Clock.next seeded :> int64) in
  check "seed above the floor raises it (next = 501)" (after_high = 501L);
  (* Seeded property loop: an LCG drives 1000 wall readings, including
     backward jumps; every stamp is strictly above its predecessor and at
     least that call's wall reading. *)
  let lcg_state = ref 42L in
  let lcg () =
    (* Fold into [0, 1023] so collisions and backward jumps are common. *)
    Int64.logand (Int64.shift_right_logical (Test_util.lcg_next lcg_state) 33) 1023L
  in
  let last_wall = ref 0L in
  let prop = Clock.create ~now:(fun () -> !last_wall) in
  let monotone, _ =
    List.fold_left
      (fun (ok, prev) (_ : int) ->
        last_wall := lcg ();
        let s = (Clock.next prop :> int64) in
        (ok && Int64.compare s prev > 0 && Int64.compare s !last_wall >= 0, s))
      (true, Int64.min_int)
      (List.init 1000 Fun.id)
  in
  check "1000 LCG-driven stamps: strictly increasing and >= that call's now" monotone;
  Printf.printf "\nThe monotonic commit clock holds (step 6, DESIGN §5).\n%!"
