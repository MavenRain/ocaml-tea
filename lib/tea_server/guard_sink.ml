module Prim = Tea_core.Prim

type event =
  | Advance of
      { replica : Tea_core.Crdt.Replica.t
      ; tab : Prim.Tab_id.t
      ; seq : Prim.Msg_seq.t
      ; water : Prim.Store_water.t
      }
  | Forget of { replica : Tea_core.Crdt.Replica.t }

type err =
  | Sink_closed
  | Io of string

type t = { append : event -> (unit, err) result Lwt.t }

let null : t = { append = (fun (_ : event) -> Lwt.return (Ok ())) }

let memory () : t * (unit -> event list) =
  let recorded : event list ref = ref [] in
  ( { append =
        (fun (e : event) ->
          recorded := e :: !recorded;
          Lwt.return (Ok ()))
    }
  , fun () -> List.rev !recorded )

module Codec = struct
  type decode_err =
    | Torn
    | Bad_crc
    | Bad_tag of int
    | Bad_field of string

  (* CRC-32 (IEEE 802.3, reflected, poly 0xedb88320), bit-serial over Int32 —
     a dozen lines beat a new dependency for frames this small, and the
     standard test vector is pinned in guard_sink_test so the constant cannot
     drift silently. Recursion depth is a fixed 8, one call per payload
     byte. *)
  let poly = 0xedb88320l

  let rec crc_shift (n : int) (c : int32) : int32 =
    if n = 0 then c
    else
      let shifted = Int32.shift_right_logical c 1 in
      crc_shift (n - 1)
        (if Int32.equal (Int32.logand c 1l) 1l then Int32.logxor shifted poly
         else shifted)

  let crc32 (s : string) : int32 =
    String.fold_left
      (fun (acc : int32) (ch : char) ->
        crc_shift 8 (Int32.logxor acc (Int32.of_int (Char.code ch))))
      0xffffffffl s
    |> Int32.lognot

  (* The payloads ride the same Repr binary codec the store and the wire
     already use; the two client-chosen fields are re-validated through their
     wire mints on decode, exactly as the pump does, so a corrupt-but-CRC-valid
     journal can never hand the guard an inhabitant the wire could not. *)
  let advance_body_t :
      (Tea_core.Crdt.Replica.t * string * int * Prim.Store_water.t) Repr.t =
    Repr.(quad Tea_core.Crdt.Replica.t string int Prim.Store_water.t)

  (* The pre-step-13 Advance payload: decoded forever, written never, so an
     upgraded server keeps every floor its journal already holds — at
     [Store_water.bottom], the honest "no witness" claim. *)
  let legacy_advance_body_t : (Tea_core.Crdt.Replica.t * string * int) Repr.t =
    Repr.(triple Tea_core.Crdt.Replica.t string int)

  let forget_body_t : Tea_core.Crdt.Replica.t Repr.t = Tea_core.Crdt.Replica.t

  (* repr 0.8 stages its binary codecs; unstage each exactly once. *)
  let encode_advance = Repr.unstage (Repr.to_bin_string advance_body_t)
  let decode_advance_bin = Repr.unstage (Repr.of_bin_string advance_body_t)

  let decode_advance_legacy_bin =
    Repr.unstage (Repr.of_bin_string legacy_advance_body_t)

  let encode_forget = Repr.unstage (Repr.to_bin_string forget_body_t)
  let decode_forget_bin = Repr.unstage (Repr.of_bin_string forget_body_t)
  let tag_advance_legacy = '\001'
  let tag_forget = '\002'

  (* A step-12 binary meeting tag 3 reads [Bad_tag] and keeps only the frames
     before it: floors fall, replays read Fresh — the duplicate side, the one
     downgrade direction the design permits. *)
  let tag_advance = '\003'

  (* The only place the framing constants live: every frame kind, this
     codec's events and tag 4's store-identity header alike, gets its length
     and its CRC from here. *)
  let frame ~(tag : char) ~(payload : string) : string =
    let b = String.make 1 tag ^ payload in
    let head = Bytes.create 4 in
    Bytes.set_int32_be head 0 (Int32.of_int (String.length b));
    let tail = Bytes.create 4 in
    Bytes.set_int32_be tail 0 (crc32 b);
    Bytes.to_string head ^ b ^ Bytes.to_string tail

  let to_bytes (e : event) : string =
    match e with
    | Advance { replica; tab; seq; water } ->
      frame ~tag:tag_advance
        ~payload:
          (encode_advance
             (replica, Prim.Tab_id.to_string tab, Prim.Msg_seq.to_int seq, water))
    | Forget { replica } ->
      frame ~tag:tag_forget ~payload:(encode_forget replica)

  (* One validation spelling for both Advance arms, current and legacy: the
     two client-chosen fields go back through their wire mints. The water
     needs no mint — every int64 is a lattice point, and [bottom] claims
     nothing. *)
  let advance_validated ~(replica : Tea_core.Crdt.Replica.t) ~(tab : string)
      ~(seq : int) ~(water : Prim.Store_water.t) ~(next : int) :
      (event * int, decode_err) result =
    Result.fold (Prim.Tab_id.of_string tab)
      ~error:(fun (_ : Prim.Tab_id.err) -> Error (Bad_field "tab"))
      ~ok:(fun (tab : Prim.Tab_id.t) ->
        Prim.Msg_seq.of_int seq
        |> Option.fold
             ~none:(Error (Bad_field "seq"))
             ~some:(fun (seq : Prim.Msg_seq.t) ->
               Ok (Advance { replica; tab; seq; water }, next)))

  let decode_advance (payload : string) ~(next : int) :
      (event * int, decode_err) result =
    Result.fold
      (decode_advance_bin payload)
      ~error:(fun (`Msg reason) -> Error (Bad_field reason))
      ~ok:(fun
          ((replica, tab, seq, water) :
            Tea_core.Crdt.Replica.t * string * int * Prim.Store_water.t)
        -> advance_validated ~replica ~tab ~seq ~water ~next)

  let decode_advance_legacy (payload : string) ~(next : int) :
      (event * int, decode_err) result =
    Result.fold
      (decode_advance_legacy_bin payload)
      ~error:(fun (`Msg reason) -> Error (Bad_field reason))
      ~ok:(fun ((replica, tab, seq) : Tea_core.Crdt.Replica.t * string * int) ->
        advance_validated ~replica ~tab ~seq ~water:Prim.Store_water.bottom
          ~next)

  let decode_forget (payload : string) ~(next : int) :
      (event * int, decode_err) result =
    Result.fold
      (decode_forget_bin payload)
      ~error:(fun (`Msg reason) -> Error (Bad_field reason))
      ~ok:(fun (replica : Tea_core.Crdt.Replica.t) ->
        Ok (Forget { replica }, next))

  (* Framing only, no tag knowledge: an unrecognised tag comes back as data,
     which is what lets a caller own a frame kind this codec never writes. *)
  let unframe (s : string) ~(pos : int) :
      (char * string * int, decode_err) result =
    let total = String.length s in
    if pos < 0 || pos + 4 > total then Error Torn
    else
      let len = Int32.to_int (String.get_int32_be s pos) in
      (* A non-positive length is indistinguishable from a truncated write:
         there is no body span to check a CRC over, so it is the same "stop
         here, keep the prefix" verdict as running out of bytes. *)
      if len < 1 || pos + 4 + len + 4 > total then Error Torn
      else
        let b = String.sub s (pos + 4) len in
        let stored_crc = String.get_int32_be s (pos + 4 + len) in
        let computed_crc = crc32 b in
        if not (Int32.equal stored_crc computed_crc) then Error Bad_crc
        else Ok (b.[0], String.sub b 1 (len - 1), pos + 4 + len + 4)

  let of_bytes (s : string) ~(pos : int) : (event * int, decode_err) result =
    Result.fold (unframe s ~pos)
      ~error:(fun (e : decode_err) -> Error e)
      ~ok:(fun ((tag, payload, next) : char * string * int) ->
        if Char.equal tag tag_advance then decode_advance payload ~next
        else if Char.equal tag tag_advance_legacy then
          decode_advance_legacy payload ~next
        else if Char.equal tag tag_forget then decode_forget payload ~next
        else Error (Bad_tag (Char.code tag)))
end
