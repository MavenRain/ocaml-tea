(** The boot lines the identity family prints (roadmap step 18, D23, review).

    A guard verdict an operator cannot act on is worth less than no line at
    all, and a verdict that promises the WRONG thing is worse than both. Two
    explanations in the step-18 cut said something that was not true of the
    boot printing them:

    - {!Tea_server_pack.explain_outcome}'s [Adopted_unbound] line ended "the
      journal is bound to this store now, so the next boot IS protected". True
      under a {!Tea_core.Prim.Store_identity.Bound} caller, which restamps
      before {!Tea_server_pack.Guard_file.open_} returns. FALSE under
      {!Tea_core.Prim.Store_identity.Unresolved}: there is no token to stamp
      the journal WITH, nothing is written, and the journal is still unbound
      at exit.
    - {!Tea_persist_pack.Store_pack.explain_identity}'s three unresolved
      origins said "every guard journal's floors are cleared this boot". Only
      journals that CARRY a binding are held and cleared; journals without one
      keep their floors and stay unbound.

    Both are pure functions of their inputs, so both are asserted here as
    values rather than grepped out of a captured stderr, and each arm of each
    sum is spelled so a new constructor is a compile error in this file. *)

module Id = Tea_core.Prim.Store_identity
module Guard_file = Tea_server_pack.Guard_file
module Store = Tea_persist_pack.Store_pack.Make (Counter_app.App)

(* --- Harness --------------------------------------------------------------- *)

let failures : int ref = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    incr failures)

(** [needle] occurs somewhere in [haystack]. Decided by walking suffixes as
    sequences: no offset is computed, nothing is read out by index, and the
    strings under test are one line each. *)
let contains ~(needle : string) (haystack : string) : bool =
  let rec prefix_at (hay : char Seq.t) (need : char Seq.t) : bool =
    Option.fold (Seq.uncons need) ~none:true
      ~some:(fun ((c : char), (need' : char Seq.t)) ->
        Option.fold (Seq.uncons hay) ~none:false
          ~some:(fun ((d : char), (hay' : char Seq.t)) ->
            Char.equal c d && prefix_at hay' need'))
  in
  let rec occurs (hay : char Seq.t) : bool =
    prefix_at hay (String.to_seq needle)
    || Option.fold (Seq.uncons hay) ~none:false
         ~some:(fun ((_ : char), (hay' : char Seq.t)) -> occurs hay')
  in
  occurs (String.to_seq haystack)

(** The explanation, or the empty string when the outcome earns no line: every
    check below is over a string, so an unexpected [None] fails under its own
    check's name rather than by matching. *)
let line ~(binding : Id.binding) (outcome : Guard_file.identity_outcome) : string =
  Tea_server_pack.explain_outcome ~binding ~channel:"websocket" outcome
  |> Option.value ~default:""

let id : Id.t = Id.of_draws (fun () -> 0x3d)
let bound : Id.binding = Id.Bound id
let unresolved : Id.binding = Id.Unresolved

(* --- E1: the contains helper is not vacuous -------------------------------- *)

let () =
  check "E1 contains finds a needle at the start, in the middle and at the end"
    (contains ~needle:"ab" "abcd"
    && contains ~needle:"bc" "abcd"
    && contains ~needle:"cd" "abcd"
    && contains ~needle:"abcd" "abcd"
    && contains ~needle:"" "abcd")

let () =
  check "E1 contains refuses an absent needle and one longer than the haystack"
    ((not (contains ~needle:"ac" "abcd")) && not (contains ~needle:"abcde" "abcd"))

(* --- E2: Adopted_unbound tracks the BINDING, which is the whole fix --------- *)

let () =
  let l = line ~binding:bound (Guard_file.Adopted_unbound 3) in
  check
    "E2 under a Bound caller Adopted_unbound still promises the next boot is \
     protected, because this open restamped the journal"
    (contains ~needle:"the next boot IS protected" l
    && contains ~needle:"adopted 3 delivery floor(s)" l)

let () =
  let l = line ~binding:unresolved (Guard_file.Adopted_unbound 3) in
  check
    "E2 under an Unresolved caller the SAME outcome says the journal REMAINS \
     UNBOUND and the next boot is NOT protected"
    (contains ~needle:"UNBOUND" l
    && contains ~needle:"NOT protected" l
    && contains ~needle:"adopted 3 delivery floor(s)" l)

let () =
  check
    "E2 the Unresolved adoption never claims protection, under any spelling of \
     the promise"
    (let l = line ~binding:unresolved (Guard_file.Adopted_unbound 3) in
     (not (contains ~needle:"IS protected" l))
     && not (contains ~needle:"bound to this store now" l))

(* --- E3: the silent arms stay silent --------------------------------------- *)

let () =
  let quiet (binding : Id.binding) (o : Guard_file.identity_outcome) : bool =
    Option.is_none
      (Tea_server_pack.explain_outcome ~binding ~channel:"rpc" o)
  in
  check
    "E3 Matched, Freshly_bound and an empty adoption print NOTHING, under \
     either binding"
    (quiet bound Guard_file.Matched
    && quiet unresolved Guard_file.Matched
    && quiet bound Guard_file.Freshly_bound
    && quiet unresolved Guard_file.Freshly_bound
    && quiet bound (Guard_file.Adopted_unbound 0)
    && quiet unresolved (Guard_file.Adopted_unbound 0))

(* --- E4: the loud arms name the channel, the count and the consequence ------ *)

let () =
  let l = line ~binding:bound (Guard_file.Rebound 7) in
  check
    "E4 Rebound names the channel, the count it dropped and the re-binding it \
     performed"
    (contains ~needle:"websocket channel" l
    && contains ~needle:"dropped ALL 7 delivery floor(s)" l
    && contains ~needle:"re-bound to this store now" l)

let () =
  let l = line ~binding:unresolved (Guard_file.Unresolved_cleared 4) in
  check
    "E4 Unresolved_cleared says the binding is left intact and that NOTHING is \
     re-stamped or appended (the strict hold covers the appends too)"
    (contains ~needle:"dropped 4 delivery floor(s)" l
    && contains ~needle:"LEFT INTACT" l
    && contains ~needle:"nothing is re-stamped or appended" l)

let () =
  check "E4 every line printed ends in exactly one newline"
    (List.for_all
       (fun (l : string) ->
         contains ~needle:"\n" l
         && Option.fold
              (Seq.uncons (String.to_seq l |> Seq.drop (String.length l - 1)))
              ~none:false
              ~some:(fun ((c : char), (_ : char Seq.t)) -> Char.equal c '\n'))
       [ line ~binding:bound (Guard_file.Adopted_unbound 3)
       ; line ~binding:unresolved (Guard_file.Adopted_unbound 3)
       ; line ~binding:bound (Guard_file.Rebound 7)
       ; line ~binding:unresolved (Guard_file.Unresolved_cleared 4)
       ])

(* --- E5: explain_identity's unresolved origins state the CONDITIONAL truth -- *)

let () =
  let origins : Store.identity_origin list =
    [ Store.Absent_unmintable "no entropy"; Store.Unreadable "EIO"; Store.Malformed ]
  in
  check
    "E5 each unresolved origin says journals WITH a binding are held and \
     cleared, and journals without one keep their floors but stay UNBOUND"
    (List.for_all
       (fun (o : Store.identity_origin) ->
         let s = Store.explain_identity o in
         contains ~needle:"carry a store-identity binding are held" s
         && contains ~needle:"keep their floors but stay UNBOUND" s)
       origins)

let () =
  check
    "E5 no unresolved origin claims EVERY journal's floors are cleared, which \
     was the false half"
    (List.for_all
       (fun (o : Store.identity_origin) ->
         not (contains ~needle:"every guard journal's floors are cleared" (Store.explain_identity o)))
       [ Store.Absent_unmintable "no entropy"; Store.Unreadable "EIO"; Store.Malformed ])

let () =
  check
    "E5 the resolved origins are untouched: each still names tea.identity and \
     what this boot did to it"
    (contains ~needle:"read the store identity token" (Store.explain_identity Store.Read)
    && contains ~needle:"minted a store identity token" (Store.explain_identity Store.Minted)
    && contains ~needle:"adopted the" (Store.explain_identity Store.Adopted))

(* --- Verdict --------------------------------------------------------------- *)

let () =
  if Int.equal !failures 0 then
    Printf.printf
      "\n\
       The identity boot lines hold: the adoption line tracks its BINDING (E2), \
       the silent arms stay silent (E3), the loud arms name channel, count and \
       consequence (E4), and the unresolved origins state the conditional truth \
       (E5), D23.\n\
       %!"
  else (
    Printf.printf "\n%d of the explanation properties FAILED.\n%!" !failures;
    exit 1)
