(** Pins for the guard journal frame codec and sinks (roadmap step 11, D16).

    The replay guard's durable floor rides this codec, and the whole design
    leans on one promise: every degradation falls toward duplicating, never
    losing. A torn tail, a flipped byte, a tag from a future version must each
    come back as a classified verdict that discards only the suffix. A codec
    that raises turns a crash-in-flight journal into a boot failure (which
    loses every floor, the one direction the design forbids), and a
    misclassified frame hands the guard a floor the wire never minted. These
    tests therefore pin what the signature cannot: the CRC constant (against
    an independent second implementation, so drift on either side is loud),
    the exact verdict for each corruption shape, byte-exact framing, and
    totality of [of_bytes] on arbitrary bytes. *)

module Sink = Tea_server.Guard_sink
module Codec = Sink.Codec
module Msg_seq = Tea_core.Prim.Msg_seq
module Tab_id = Tea_core.Prim.Tab_id
module Replica = Tea_core.Crdt.Replica

let check (name : string) (cond : bool) : unit =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

(** A replica from a branch-name-literal session id, the trusted test mint. *)
let replica (name : string) : Replica.t =
  Replica.v (Tea_core.Prim.Session_id.v name)

(* [Option.fold]'s [~none:] is EAGER, so a failure branch written as a value
   runs on every call and this file would exit before its first check. Both
   branches here are closures and the application is the only thing that
   chooses, which is what makes a loud refusal safe. *)
let must (what : string) (o : 'a option) : 'a =
  Option.fold
    ~none:(fun () ->
      Printf.printf "FAIL - test setup: %s\n%!" what;
      exit 1)
    ~some:(fun x () -> x)
    o ()

(** A tab id from a 16-byte seed, through the same [of_bytes] mint the browser
    runtime uses, so a change to the mint's arity or range breaks this too. *)
let tab (n : int) : Tab_id.t =
  must "tab id mint refused a valid seed"
    (Tab_id.of_bytes (List.init 16 (fun i -> (n + i) land 0xff)))

let seq (n : int) : Msg_seq.t =
  must (Printf.sprintf "Msg_seq.of_int refused %d" n) (Msg_seq.of_int n)

(** Structural event equality, every constructor pair spelled out. *)
let event_equal (a : Sink.event) (b : Sink.event) : bool =
  match (a, b) with
  | ( Sink.Advance { replica = ra; tab = ta; seq = sa },
      Sink.Advance { replica = rb; tab = tb; seq = sb } ) ->
    Replica.equal ra rb
    && Int.equal (Tab_id.compare ta tb) 0
    && Int.equal (Msg_seq.to_int sa) (Msg_seq.to_int sb)
  | Sink.Forget { replica = ra }, Sink.Forget { replica = rb } ->
    Replica.equal ra rb
  | Sink.Advance { replica = _; tab = _; seq = _ }, Sink.Forget { replica = _ }
    ->
    false
  | Sink.Forget { replica = _ }, Sink.Advance { replica = _; tab = _; seq = _ }
    ->
    false

(** [Ok] with exactly this event and this next offset. *)
let decodes_to (s : string) ~(pos : int) ~(expect : Sink.event) ~(next : int) :
    bool =
  Result.fold
    (Codec.of_bytes s ~pos)
    ~ok:(fun ((e, n) : Sink.event * int) ->
      event_equal e expect && Int.equal n next)
    ~error:(fun (_ : Codec.decode_err) -> false)

(** [Error] satisfying [want]; an [Ok] never does. *)
let errs (r : (Sink.event * int, Codec.decode_err) result)
    ~(want : Codec.decode_err -> bool) : bool =
  Result.fold r ~ok:(fun (_ : Sink.event * int) -> false) ~error:want

let is_torn (r : (Sink.event * int, Codec.decode_err) result) : bool =
  errs r ~want:(fun (e : Codec.decode_err) ->
      match e with
      | Codec.Torn -> true
      | Codec.Bad_crc -> false
      | Codec.Bad_tag (_ : int) -> false
      | Codec.Bad_field (_ : string) -> false)

let is_bad_crc (r : (Sink.event * int, Codec.decode_err) result) : bool =
  errs r ~want:(fun (e : Codec.decode_err) ->
      match e with
      | Codec.Bad_crc -> true
      | Codec.Torn -> false
      | Codec.Bad_tag (_ : int) -> false
      | Codec.Bad_field (_ : string) -> false)

let is_bad_tag (r : (Sink.event * int, Codec.decode_err) result) ~(code : int)
    : bool =
  errs r ~want:(fun (e : Codec.decode_err) ->
      match e with
      | Codec.Bad_tag n -> Int.equal n code
      | Codec.Torn -> false
      | Codec.Bad_crc -> false
      | Codec.Bad_field (_ : string) -> false)

let is_bad_field (r : (Sink.event * int, Codec.decode_err) result)
    ~(field : string) : bool =
  errs r ~want:(fun (e : Codec.decode_err) ->
      match e with
      | Codec.Bad_field f -> String.equal f field
      | Codec.Torn -> false
      | Codec.Bad_crc -> false
      | Codec.Bad_tag (_ : int) -> false)

(** A bounded non-negative draw off the shared seeded LCG. *)
let draw (state : int64 ref) ~(bound : int) : int =
  Int64.to_int
    (Int64.rem
       (Int64.logand (Test_util.lcg_next state) Int64.max_int)
       (Int64.of_int bound))

(* --- 1. The CRC constant, pinned twice ------------------------------------

   The codec does not export its crc32, deliberately, so the constant is
   pinned indirectly: a SECOND, independently written implementation of
   CRC-32 (IEEE 802.3, reflected, polynomial 0xedb88320) lives here, checked
   once against the published standard vector and once against the trailing
   bytes [to_bytes] actually writes. The two implementations can only drift
   together by writing the same bug twice; a change to either side fails a
   check below. *)

let local_crc32 (s : string) : int32 =
  let step (c : int32) : int32 =
    let shifted = Int32.shift_right_logical c 1 in
    if Int32.equal (Int32.logand c 1l) 1l then Int32.logxor shifted 0xedb88320l
    else shifted
  in
  let over_byte (acc : int32) (ch : char) : int32 =
    List.fold_left
      (fun (c : int32) (_ : int) -> step c)
      (Int32.logxor acc (Int32.of_int (Char.code ch)))
      (List.init 8 Fun.id)
  in
  Int32.lognot (String.fold_left over_byte 0xffffffffl s)

(** Frame a hand-built body exactly as the codec does, but with the local CRC,
    so a crafted body reaches the tag and field checks rather than dying at
    [Bad_crc]. *)
let frame_of_body (b : string) : string =
  let head = Bytes.create 4 in
  Bytes.set_int32_be head 0 (Int32.of_int (String.length b));
  let tail = Bytes.create 4 in
  Bytes.set_int32_be tail 0 (local_crc32 b);
  Bytes.to_string head ^ b ^ Bytes.to_string tail

let () =
  check "local crc32 matches the standard vector crc32(123456789) = 0xCBF43926"
    (Int32.equal (local_crc32 "123456789") 0xCBF43926l);
  let frame =
    Codec.to_bytes
      (Sink.Advance { replica = replica "crc-pin"; tab = tab 1; seq = seq 7 })
  in
  let total = String.length frame in
  let body = String.sub frame 4 (total - 8) in
  check "the length header counts exactly tag plus payload"
    (Int.equal (Int32.to_int (String.get_int32_be frame 0)) (total - 8));
  check "the trailing crc equals an independent crc32 of the body span"
    (Int32.equal (String.get_int32_be frame (total - 4)) (local_crc32 body))

(* --- 2. Round-trips -------------------------------------------------------- *)

let () =
  let adv =
    Sink.Advance { replica = replica "round"; tab = tab 2; seq = seq 41 }
  in
  let fadv = Codec.to_bytes adv in
  check "Advance round-trips and next lands on the frame end"
    (decodes_to fadv ~pos:0 ~expect:adv ~next:(String.length fadv));
  let fgt = Sink.Forget { replica = replica "round-forget" } in
  let ffgt = Codec.to_bytes fgt in
  check "Forget round-trips and next lands on the frame end"
    (decodes_to ffgt ~pos:0 ~expect:fgt ~next:(String.length ffgt))

(* --- 3. A multi-frame stream, seeded property ----------------------------- *)

(** One LCG-driven event, a 3:1 Advance:Forget mix over a small replica pool
    and wide tab/seq ranges. *)
let gen_event (state : int64 ref) : Sink.event =
  let r = replica (Printf.sprintf "gen-%d" (draw state ~bound:8)) in
  if Int.equal (draw state ~bound:4) 0 then Sink.Forget { replica = r }
  else
    Sink.Advance
      { replica = r
      ; tab = tab (draw state ~bound:1000)
      ; seq = seq (1 + draw state ~bound:1_000_000)
      }

(** Decode a whole stream by threading [next]; [None] on any error. *)
let rec decode_all (s : string) (pos : int) (acc : Sink.event list) :
    Sink.event list option =
  if Int.equal pos (String.length s) then Some (List.rev acc)
  else
    Result.fold
      (Codec.of_bytes s ~pos)
      ~ok:(fun ((e, next) : Sink.event * int) -> decode_all s next (e :: acc))
      ~error:(fun (_ : Codec.decode_err) -> None)

let () =
  let state = ref 0x7ea5eedL in
  let events = List.init 200 (fun (_ : int) -> gen_event state) in
  let stream = String.concat "" (List.map Codec.to_bytes events) in
  check "200 lcg-generated frames stream back in order"
    (Option.fold ~none:false
       ~some:(fun (decoded : Sink.event list) ->
         Int.equal (List.length decoded) 200
         && List.for_all2 event_equal events decoded)
       (decode_all stream 0 []))

(* --- 4. Torn tails ---------------------------------------------------------

   The crash-in-flight shape: a write that stopped mid-frame. Every prefix of
   the final frame must classify as [Torn] at that frame's position while the
   completed frames before it stay decodable, because "keep the prefix, drop
   the tail" is exactly the duplicate-direction recovery the boot path
   performs. *)

let () =
  let e1 = Sink.Advance { replica = replica "torn-a"; tab = tab 11; seq = seq 3 } in
  let e2 = Sink.Forget { replica = replica "torn-b" } in
  let e3 = Sink.Advance { replica = replica "torn-c"; tab = tab 12; seq = seq 9 } in
  let f1 = Codec.to_bytes e1 in
  let f2 = Codec.to_bytes e2 in
  let f3 = Codec.to_bytes e3 in
  let p2 = String.length f1 in
  let p3 = p2 + String.length f2 in
  let stream = f1 ^ f2 ^ f3 in
  let torn_at (cut : int) : bool =
    let prefix = String.sub stream 0 cut in
    decodes_to prefix ~pos:0 ~expect:e1 ~next:p2
    && decodes_to prefix ~pos:p2 ~expect:e2 ~next:p3
    && is_torn (Codec.of_bytes prefix ~pos:p3)
  in
  check "every truncation of the final frame is Torn there, earlier frames intact"
    (List.for_all torn_at
       (List.init (String.length f3) (fun (i : int) -> p3 + i)));
  (* A header that lies long: the frame claims more bytes than remain. *)
  let honest = Codec.to_bytes (Sink.Forget { replica = replica "overlong" }) in
  let lying = Bytes.of_string honest in
  Bytes.set_int32_be lying 0 (Int32.of_int (String.length honest - 8 + 100));
  check "a length header claiming more bytes than remain is Torn"
    (is_torn (Codec.of_bytes (Bytes.to_string lying) ~pos:0));
  (* A non-positive length has no body span to CRC, so it must classify as
     the same stop-here verdict, never raise. *)
  let with_len (n : int) : string =
    let head = Bytes.create 4 in
    Bytes.set_int32_be head 0 (Int32.of_int n);
    Bytes.to_string head ^ String.make 8 'x'
  in
  check "a zero length header classifies as Torn, not a crash"
    (is_torn (Codec.of_bytes (with_len 0) ~pos:0));
  check "a negative length header classifies as Torn, not a crash"
    (is_torn (Codec.of_bytes (with_len (-5)) ~pos:0))

(* --- 5. A complete frame whose bytes lie ----------------------------------- *)

let () =
  let frame =
    Codec.to_bytes
      (Sink.Advance { replica = replica "flip"; tab = tab 21; seq = seq 5 })
  in
  (* Index 6 sits inside the payload (body starts at 4, tag at 4). *)
  let flipped =
    String.init (String.length frame) (fun (j : int) ->
        if Int.equal j 6 then Char.chr (Char.code frame.[j] lxor 0xff)
        else frame.[j])
  in
  check "one flipped payload byte in a complete frame is Bad_crc"
    (is_bad_crc (Codec.of_bytes flipped ~pos:0))

(* --- 6. A tag this codec never wrote ---------------------------------------

   Framed with a CORRECT crc over the crafted body, so the verdict tested is
   the tag check itself, not a CRC shadow of it. *)

let () =
  check "tag 0 under a valid crc is Bad_tag 0"
    (is_bad_tag (Codec.of_bytes (frame_of_body "\000") ~pos:0) ~code:0);
  check "tag 9 under a valid crc is Bad_tag 9"
    (is_bad_tag (Codec.of_bytes (frame_of_body "\009") ~pos:0) ~code:9)

(* --- 7. A well-framed body the wire mints refuse ---------------------------

   The decode path re-validates the client-chosen fields through the same
   mints the pump uses, so a corrupt-but-CRC-valid journal cannot hand the
   guard an inhabitant the wire could not. We craft Advance bodies with the
   same Repr triple the codec uses (a drift in that witness breaks the
   round-trip pins above, so borrowing it here is safe) and plant one field
   each mint must refuse: a non-positive seq, a non-hex tab. *)

let advance_body_t : (Replica.t * string * int) Repr.t =
  Repr.(triple Replica.t string int)

let encode_advance_raw = Repr.unstage (Repr.to_bin_string advance_body_t)

let () =
  let r = replica "crafted" in
  let bad_seq =
    frame_of_body
      ("\001" ^ encode_advance_raw (r, Tab_id.to_string (tab 3), -1))
  in
  check "a crafted Advance with seq -1 is Bad_field seq"
    (is_bad_field (Codec.of_bytes bad_seq ~pos:0) ~field:"seq");
  let bad_tab =
    frame_of_body ("\001" ^ encode_advance_raw (r, "not-32-hex-chars", 5))
  in
  check "a crafted Advance with a non-hex tab is Bad_field tab"
    (is_bad_field (Codec.of_bytes bad_tab ~pos:0) ~field:"tab")

(* --- 8. The sinks ---------------------------------------------------------- *)

(** Run one append to completion; the sinks under test are pure closures, so
    [Lwt_main.run] returns immediately. *)
let run_append (s : Sink.t) (e : Sink.event) : (unit, Sink.err) result =
  Lwt_main.run (s.Sink.append e)

let append_ok (s : Sink.t) (e : Sink.event) : bool =
  Result.fold (run_append s e)
    ~ok:(fun () -> true)
    ~error:(fun (_ : Sink.err) -> false)

let () =
  let a = Sink.Advance { replica = replica "sink"; tab = tab 31; seq = seq 1 } in
  let b = Sink.Forget { replica = replica "sink" } in
  let c = Sink.Advance { replica = replica "sink"; tab = tab 32; seq = seq 2 } in
  check "the null sink accepts every event" (append_ok Sink.null a && append_ok Sink.null b);
  let sink, read = Sink.memory () in
  check "memory records appends oldest-first"
    (append_ok sink a && append_ok sink b
    &&
    let got = read () in
    Int.equal (List.length got) 2 && List.for_all2 event_equal [ a; b ] got);
  check "the reader reflects a later append in order"
    (append_ok sink c
    &&
    let got = read () in
    Int.equal (List.length got) 3 && List.for_all2 event_equal [ a; b; c ] got)

(* --- 9. Totality on garbage ------------------------------------------------

   The boot path feeds [of_bytes] whatever the disk holds. Any raise here is
   the boot-failure shape (loses every floor), so a raise is converted into a
   loud FAIL rather than a crash the harness might misreport. *)

let () =
  let state = ref 0xdeadbeefL in
  let survives (_ : int) : bool =
    let len = draw state ~bound:64 in
    let s = String.init len (fun (_ : int) -> Char.chr (draw state ~bound:256)) in
    try
      Result.fold
        (Codec.of_bytes s ~pos:0)
        ~ok:(fun (_ : Sink.event * int) -> true)
        ~error:(fun (_ : Codec.decode_err) -> true)
    with e ->
      Printf.printf "FAIL - of_bytes raised %s on %d garbage bytes\n%!"
        (Printexc.to_string e) len;
      exit 1
  in
  check "of_bytes is total on 200 lcg garbage strings"
    (List.for_all survives (List.init 200 Fun.id))
