(** The store identity token, newtype half (roadmap step 18, D23, R20a).

    {!Tea_core.Prim.Store_identity} is the identity half of the pair whose
    order half is {!Tea_core.Prim.Store_water}: a water answers "did these
    bytes go backwards", this answers "are these the same store's bytes at
    all". S1/S2/S6 drive the pure newtype through its public seams, so every
    check there is arithmetic over strings and needs no root, no clock and no
    filesystem. S3/S4/S5/S7 then drive
    {!Tea_persist_pack.Store_pack.resolve_identity} over real pack roots, which
    is where the token meets a directory. What the token {i does} to a journal
    belongs to the guard tests.

    Two shapes are catastrophic and both are checked directly. A mint that
    repeats makes two stores read as one, so distinctness is asserted over a
    seeded population rather than over a pair. And a token admitted in two
    spellings makes one store read as two, which drops every floor it owns, so
    the case rule is a refusal (S6) and not a normalisation.

    Every observation is total: a [result] is rendered to a string rather than
    unwrapped, so an unexpected [Ok] fails under its own check's name instead
    of taking the process down where the sweep cannot attribute it. *)

module Id = Tea_core.Prim.Store_identity

module Ids = Set.Make (struct
  type t = Id.t

  let compare = Id.compare
end)

(* --- Harness --------------------------------------------------------------- *)

(** One assertion: print TAP-ish, exit 1 on the first failure. *)
let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

(** Every error shape, spelled exhaustively so a new constructor is a compile
    error here and not a silently unasserted case. *)
let err_str (e : Id.err) : string =
  match e with
  | Id.Wrong_length -> "Wrong_length"
  | Id.Invalid_char (c : char) -> Printf.sprintf "Invalid_char %c" c

let outcome (r : (Id.t, Id.err) result) : string =
  Result.fold ~ok:(fun (t : Id.t) -> "Ok " ^ Id.to_string t) ~error:err_str r

let minted (o : Id.t option) : string =
  Option.fold ~none:"None" ~some:(fun (t : Id.t) -> "Some " ^ Id.to_string t) o

(** Consecutive pairs, carried through a fold rather than read out by index:
    the population below is large and every pair in it is distinct, which is
    exactly the input the symmetry and [compare]/[equal] agreement checks
    want. *)
let consecutive_pairs (xs : Id.t list) : (Id.t * Id.t) list =
  List.fold_left
    (fun ((prev : Id.t option), (acc : (Id.t * Id.t) list)) (x : Id.t) ->
      (Some x, Option.fold ~none:acc ~some:(fun (p : Id.t) -> (p, x) :: acc) prev))
    (None, []) xs
  |> snd

(* --- Fixtures -------------------------------------------------------------- *)

(* Written out rather than cut from one another with [String.sub], so the
   lengths are visible bytes; each check asserts the length it relies on, so a
   typo here fails loudly instead of testing the wrong law. *)
let valid_32 = "0123456789abcdef0123456789abcdef"
let short_31 = "0123456789abcdef0123456789abcde"
let long_33 = "0123456789abcdef0123456789abcdef0"
let bad_at_7 = "0123456g89abcdef0123456789abcdef"
let upper_hex = "0123456789ABCDEF0123456789abcdef"

(* --- S1. Totality of the newtype ------------------------------------------- *)

let () =
  check "S1 of_string admits exactly 32 lowercase hex characters"
    (Int.equal (String.length valid_32) 32
    && String.equal (outcome (Id.of_string valid_32)) ("Ok " ^ valid_32))

let () =
  check "S1 of_string refuses 31 and 33 characters with Wrong_length"
    (Int.equal (String.length short_31) 31
    && Int.equal (String.length long_33) 33
    && String.equal (outcome (Id.of_string short_31)) "Wrong_length"
    && String.equal (outcome (Id.of_string long_33)) "Wrong_length")

let () =
  (* The offending byte is NAMED, not merely refused: a length-only rejection
     would pass a check that asserted "is an error" and tell an operator
     nothing about which byte was wrong. *)
  check "S1 of_string names the offending byte ('g' at index 7 is Invalid_char 'g')"
    (Int.equal (String.length bad_at_7) 32
    && String.equal (outcome (Id.of_string bad_at_7)) "Invalid_char g")

let () =
  (* 0, 16, 32, ... 240: one byte per position, so a mint that dropped,
     duplicated or reordered a byte changes the expected spelling. The exact
     string also pins the encoder's case, which S6 then refuses on the way
     back in. *)
  let bytes = List.init 16 (fun (i : int) -> i * 16) in
  check "S1 of_bytes mints from exactly 16 in-range bytes, lowercase and two digits each"
    (String.equal (minted (Id.of_bytes bytes)) "Some 00102030405060708090a0b0c0d0e0f0")

let () =
  let fifteen = List.init 15 (fun (i : int) -> i) in
  let over = List.init 16 (fun (i : int) -> if Int.equal i 3 then 256 else i) in
  let under = List.init 16 (fun (i : int) -> if Int.equal i 3 then -1 else i) in
  check "S1 of_bytes declines 15 bytes, and any list carrying 256 or -1 (declined, never raised)"
    (String.equal (minted (Id.of_bytes fifteen)) "None"
    && String.equal (minted (Id.of_bytes over)) "None"
    && String.equal (minted (Id.of_bytes under)) "None")

(* --- S2. The mint, over a seeded population -------------------------------- *)

(* A pinned seed: the population is large enough that a real collision would be
   astronomical, so any red here is a broken mint and reproduces exactly. *)
let mint_count = 20000

let mints : Id.t list =
  let state = Random.State.make [| 0x5eed; 18; 23 |] in
  let draw () : int = Random.State.bits state in
  List.init mint_count (fun (_ : int) -> Id.of_draws draw)

let pairs : (Id.t * Id.t) list = consecutive_pairs mints

let () =
  check "S2 every seeded mint is 32 characters and is re-admitted by of_string"
    (Int.equal (List.length mints) mint_count
    && List.for_all (fun (id : Id.t) -> Int.equal (String.length (Id.to_string id)) 32) mints
    && List.for_all
         (fun (id : Id.t) ->
           Id.of_string (Id.to_string id)
           |> Result.fold ~ok:(Id.equal id) ~error:(fun (_ : Id.err) -> false))
         mints)

let () =
  (* Cardinality, not an O(n^2) scan: two stores that minted one token read as
     one store, and every floor of the loser is wiped. *)
  check "S2 20000 seeded mints are pairwise distinct (Set cardinal = 20000)"
    (Int.equal (Ids.cardinal (Ids.of_list mints)) mint_count)

let () =
  check "S2 equal is reflexive on every mint and symmetric on every consecutive pair"
    (Int.equal (List.length pairs) (mint_count - 1)
    && List.for_all (fun (id : Id.t) -> Id.equal id id) mints
    && List.for_all
         (fun (((a : Id.t), (b : Id.t)) : Id.t * Id.t) ->
           Bool.equal (Id.equal a b) (Id.equal b a))
         pairs)

let () =
  (* Both directions. The negative one comes from the distinct population; the
     positive one needs two INDEPENDENT constructions of one token, because a
     [compare] that only ever saw physically identical values would agree with
     a broken [equal]. *)
  let twins_agree =
    Id.of_string valid_32
    |> Result.map (fun (a : Id.t) ->
           Id.of_string valid_32
           |> Result.fold
                ~ok:(fun (b : Id.t) -> Id.equal a b && Int.equal (Id.compare a b) 0)
                ~error:(fun (_ : Id.err) -> false))
    |> Result.value ~default:false
  in
  check "S2 compare x y = 0 exactly when equal x y (distinct mints, and two builds of one token)"
    (twins_agree
    && List.for_all (fun (id : Id.t) -> Int.equal (Id.compare id id) 0) mints
    && List.for_all
         (fun (((a : Id.t), (b : Id.t)) : Id.t * Id.t) ->
           Bool.equal (Int.equal (Id.compare a b) 0) (Id.equal a b))
         pairs)

(* --- S6. Case discipline --------------------------------------------------- *)

let () =
  check
    "S6 of_string REFUSES uppercase hex with Invalid_char 'A': two spellings of one value compare \
     unequal and would manufacture a Rebound"
    (Int.equal (String.length upper_hex) 32
    && String.equal (outcome (Id.of_string upper_hex)) "Invalid_char A")

(* --- The resolver, over real pack roots ------------------------------------ *)

module Store = Tea_persist_pack.Store_pack.Make (Counter_app.App)
module Root = Tea_persist_pack.Store_pack.Root

(** The stdlib boundary, in one place: a syscall that refuses becomes a value,
    so a broken fixture fails under its own check's name instead of taking the
    process down where the sweep cannot attribute it. *)
let attempt (f : unit -> 'a) : 'a option =
  try Some (f ()) with (_ : exn) -> None

(** Every origin, spelled exhaustively so a new constructor is a compile error
    here rather than a case nothing asserts. *)
let origin_str (o : Store.identity_origin) : string =
  match o with
  | Store.Read -> "Read"
  | Store.Minted -> "Minted"
  | Store.Adopted -> "Adopted"
  | Store.Absent_unmintable (_ : string) -> "Absent_unmintable"
  | Store.Unreadable (_ : string) -> "Unreadable"
  | Store.Malformed -> "Malformed"

let bound (b : Id.binding) : Id.t option =
  match b with
  | Id.Bound (id : Id.t) -> Some id
  | Id.Unresolved -> None

(** Two bindings name one store. [Unresolved] agrees with nothing, itself
    included: it is the absence of an identity, not an identity value. *)
let bindings_agree (a : Id.binding) (b : Id.binding) : bool =
  Option.fold (bound a) ~none:false ~some:(fun (x : Id.t) ->
      Option.fold (bound b) ~none:false ~some:(Id.equal x))

let read_bytes (path : string) : string option =
  attempt (fun () -> In_channel.with_open_bin path In_channel.input_all)

let write_bytes (path : string) (contents : string) : bool =
  attempt (fun () ->
      Out_channel.with_open_bin path (fun (oc : Out_channel.t) ->
          Out_channel.output_string oc contents))
  |> Option.is_some

let entries_of (path : string) : string list option =
  attempt (fun () -> Sys.readdir path)
  |> Option.map (fun (names : string array) ->
         List.sort String.compare (Array.to_list names))

(** Byte-for-byte recursive copy: the [cp -r] that creates R20 in the first
    place, run in-process so the check owns its own failure rather than a
    shell's exit code. *)
let rec copy_dir (src : string) (dst : string) : bool =
  Option.is_some (attempt (fun () -> Unix.mkdir dst 0o755))
  && Option.fold (entries_of src) ~none:false ~some:(fun (entries : string list) ->
         List.for_all
           (fun (entry : string) ->
             let s = Filename.concat src entry and d = Filename.concat dst entry in
             attempt (fun () -> Unix.lstat s)
             |> Option.fold ~none:false ~some:(fun (st : Unix.stats) ->
                    match st.Unix.st_kind with
                    | Unix.S_DIR -> copy_dir s d
                    | Unix.S_REG ->
                      Option.fold (read_bytes s) ~none:false ~some:(write_bytes d)
                    | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO | Unix.S_SOCK ->
                      false))
           entries)

let is_regular_at (path : string) : bool =
  attempt (fun () -> Unix.lstat path)
  |> Option.fold ~none:false ~some:(fun (st : Unix.stats) ->
         match st.Unix.st_kind with
         | Unix.S_REG -> true
         | Unix.S_DIR | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO | Unix.S_SOCK ->
           false)

let perm_at (path : string) : int option =
  attempt (fun () -> Unix.lstat path)
  |> Option.map (fun (st : Unix.stats) -> st.Unix.st_perm)

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     (* A root is opened and CLOSED before any resolve: the token is minted into
        a directory the backend has already laid its control file into, which is
        the ordering [resolve_identity] documents. Minting into a directory
        nothing else created would leave a root the next boot refuses. *)
     let make_root (path : string) : bool Lwt.t =
       let* opened = Store.open_root (Root.v path) in
       Result.fold opened
         ~ok:(fun (s : Store.t) -> Lwt.map (fun () -> true) (Store.close s))
         ~error:(fun (_ : Store.open_error) -> Lwt.return false)
     in

     (* S3: mint, then read the same token back. *)
     let parent = Filename.temp_dir "ocaml-tea-storeid" "" in
     let root_path = Filename.concat parent "store" in
     let* built = make_root root_path in
     check "S3 the scratch pack root opens and closes" built;
     let root = Root.v root_path in
     let token = Store.identity_path root in
     let first, first_origin = Store.resolve_identity root in
     check "S3 the first resolve MINTS a token into a root that carried none"
       (String.equal (origin_str first_origin) "Minted" && Option.is_some (bound first));
     let second, second_origin = Store.resolve_identity root in
     check "S3 the second resolve READS the same token back, it does not mint a second one"
       (String.equal (origin_str second_origin) "Read" && bindings_agree first second);
     check "S3 tea.identity is a regular file at 0644 (no false confidentiality promise)"
       (is_regular_at token
       && Option.fold (perm_at token) ~none:false ~some:(fun (p : int) ->
              Int.equal p 0o644));
     check "S3 the file holds exactly the token's own 32 characters"
       (Option.fold (bound first) ~none:false ~some:(fun (id : Id.t) ->
            Option.fold (read_bytes token) ~none:false ~some:(fun (raw : string) ->
                String.equal raw (Id.to_string id)
                || String.equal raw (Id.to_string id ^ "\n"))));

     (* S4: a token that does not read is NEVER healed. Overwriting it would
        permanently unbind every journal that already names it, so the refusal
        arm leaves the bytes exactly as found. *)
     let bad_parent = Filename.temp_dir "ocaml-tea-storeid-bad" "" in
     let bad_path = Filename.concat bad_parent "store" in
     let* bad_built = make_root bad_path in
     check "S4 the scratch pack root opens and closes" bad_built;
     let bad_root = Root.v bad_path in
     let bad_token = Store.identity_path bad_root in
     check "S4 a malformed token is planted" (write_bytes bad_token "zzzz");
     let bad_binding, bad_origin = Store.resolve_identity bad_root in
     check "S4 a malformed token resolves Unresolved/Malformed, never a fresh mint"
       (String.equal (origin_str bad_origin) "Malformed" && Option.is_none (bound bad_binding));
     check "S4 the planted bytes are still exactly as found (an existing token is never rewritten)"
       (Option.fold (read_bytes bad_token) ~none:false ~some:(fun (raw : string) ->
            String.equal raw "zzzz"));

     (* S5: the token travels with the store's own bytes. This is the whole
        reason it lives INSIDE the root: a sibling file would be R20 recursed
        one level, carried by no [cp -r] at all. *)
     let copy_path = Filename.concat parent "copy" in
     check "S5 the root copies recursively" (copy_dir root_path copy_path);
     let copied, copied_origin = Store.resolve_identity (Root.v copy_path) in
     check "S5 a cp -r copy carries the SAME token and READS it, it does not mint its own"
       (String.equal (origin_str copied_origin) "Read" && bindings_agree first copied);

     (* The companion negative, so S5 cannot pass because everything is equal. *)
     let other_parent = Filename.temp_dir "ocaml-tea-storeid-other" "" in
     let other_path = Filename.concat other_parent "store" in
     let* other_built = make_root other_path in
     check "S5 the second scratch pack root opens and closes" other_built;
     let other, other_origin = Store.resolve_identity (Root.v other_path) in
     check "S5 an INDEPENDENT root mints a DIFFERENT token (the copy check is not vacuous)"
       (String.equal (origin_str other_origin) "Minted"
       && Option.is_some (bound other)
       && not (bindings_agree first other));

     (* S7: degrade, never refuse. An unreadable token must not be able to stop
        a boot, and the process still being here to print is the proof. *)
     let odd_parent = Filename.temp_dir "ocaml-tea-storeid-dir" "" in
     let odd_path = Filename.concat odd_parent "store" in
     let* odd_built = make_root odd_path in
     check "S7 the scratch pack root opens and closes" odd_built;
     let odd_root = Root.v odd_path in
     let odd_token = Store.identity_path odd_root in
     check "S7 a directory is planted where the token belongs"
       (Option.is_some (attempt (fun () -> Unix.mkdir odd_token 0o755)));
     let before = entries_of odd_path in
     let odd_binding, odd_origin = Store.resolve_identity odd_root in
     check "S7 an unmintable, unreadable token degrades to Unresolved/Unreadable rather than \
            refusing the boot"
       (String.equal (origin_str odd_origin) "Unreadable" && Option.is_none (bound odd_binding));
     let after = entries_of odd_path in
     check "S7 the planted directory is still there and nothing was created beside it"
       (Option.fold before ~none:false ~some:(fun (b : string list) ->
            Option.fold after ~none:false ~some:(fun (a : string list) ->
                List.equal String.equal b a
                && List.exists (String.equal (Filename.basename odd_token)) a)));
     Lwt.return_unit)

let () =
  Printf.printf
    "\n\
     The store identity holds: totality (S1), a distinct seeded mint (S2), the case refusal (S6), \
     and over real roots mint-then-read (S3), never rewritten (S4), carried by a copy (S5), and \
     degrade-never-refuse (S7), D23.\n\
     %!"
