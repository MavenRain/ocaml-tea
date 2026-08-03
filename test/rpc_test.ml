(** The typed-RPC contract, checked without a socket (roadmap step 7, DESIGN
    §8). Everything here is pure: [Tea_rpc.Make] derives the paths, codecs, and
    the client [call] builder from the ONE {!Shared_doc_rpc} definition, so a
    name, wire path, or codec that drifts per tier is a failing check — or does
    not compile at all.

    Discipline: NEVER apply the polymorphic [(=)] to a {!Tea_core.Cmd.t} that
    may contain [Http] — its [expect] field is a closure, and structural
    comparison over a function raises at runtime. The call laws below observe a
    command only by matching its (private, but matchable) constructor to pull
    [expect] out and {e evaluate} it on canned frames. *)

module S = Shared_doc_rpc
module R = Tea_rpc.Make (S)
module Prim = Tea_core.Prim
module Cmd = Tea_core.Cmd
module Codec = Tea_core.Codec

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else (
    Printf.printf "FAIL - %s\n%!" name;
    exit 1)

(* Repr-derived structural equality, the codec-honest comparison for payloads
   (never [(=)] where a closure could hide). *)
let stats_req_eq = Repr.(unstage (equal S.stats_req_t))
let stats_resp_eq = Repr.(unstage (equal S.stats_resp_t))

(* [name_err]/[rpc_path_err] classify a parse into its error variant (or [None]
   on Ok). Written with [Result.fold] so no two-arm Result match appears. *)
let name_err s =
  Tea_rpc.Name.of_string s
  |> Result.fold ~ok:(fun (_ : Tea_rpc.Name.t) -> None) ~error:Option.some

let rpc_path_err s =
  Prim.Rpc_path.of_string s
  |> Result.fold ~ok:(fun (_ : Prim.Rpc_path.t) -> None) ~error:Option.some

let is_success i =
  Prim.Status.of_int i |> Option.fold ~none:false ~some:Prim.Status.is_success

(* The derived wire paths, as plain strings, in [all] order. *)
let paths =
  List.map (fun (S.Any e) -> Prim.Rpc_path.to_string (R.path_of e)) S.all

(* --- Name.of_string admission table --------------------------------------- *)

let () =
  check "Name.of_string admits a lowercase literal"
    (Tea_rpc.Name.of_string "ping" |> Result.map Tea_rpc.Name.to_string = Ok "ping");
  check "Name.of_string admits underscores and digit-with-dash"
    (Result.is_ok (Tea_rpc.Name.of_string "save_doc")
    && Result.is_ok (Tea_rpc.Name.of_string "a-1"));
  check "Name.of_string rejects the empty string as Empty"
    (name_err "" = Some Tea_rpc.Name.Empty);
  check "Name.of_string rejects '/' as Invalid_char '/'"
    (name_err "a/b" = Some (Tea_rpc.Name.Invalid_char '/'));
  (* NB: the spec listed [Invalid_char 'A'] for "Ping", but the first
     out-of-charset byte of "Ping" is 'P'; the strict parse reports that. *)
  check "Name.of_string rejects an uppercase leading byte as Invalid_char 'P'"
    (name_err "Ping" = Some (Tea_rpc.Name.Invalid_char 'P'))

(* --- deployed literals are strict-parse fixed points (deviation #1) -------- *)

let () =
  check "every deployed name literal passes the strict parse and round-trips"
    (List.for_all
       (fun (S.Any e) ->
         let n = S.name e in
         Tea_rpc.Name.of_string (Tea_rpc.Name.to_string n)
         |> Result.fold ~error:(fun (_ : Tea_rpc.Name.err) -> false)
              ~ok:(fun n' -> Tea_rpc.Name.equal n' n))
       S.all)

(* --- literal wire-path pins (catch a globally-consistent name swap) -------- *)

let () =
  check "History_count pins to /rpc/history_count"
    (Prim.Rpc_path.to_string (R.path_of S.History_count) = "/rpc/history_count");
  check "Doc_stats pins to /rpc/doc_stats"
    (Prim.Rpc_path.to_string (R.path_of S.Doc_stats) = "/rpc/doc_stats");
  check "Append_tag pins to /rpc/append_tag"
    (Prim.Rpc_path.to_string (R.path_of S.Append_tag) = "/rpc/append_tag")

(* --- distinctness, length equation, reserved-path disjointness ------------- *)

let () =
  let dedup = List.sort_uniq String.compare paths in
  check "derived paths are pairwise distinct" (List.length paths = List.length dedup);
  check "all enumerates exactly three endpoints" (List.length S.all = 3);
  check "Tea_rpc.prefix is /rpc/" (String.equal Tea_rpc.prefix "/rpc/");
  let reserved = [ "/"; "/msg"; "/undo"; Tea_core.Wire.ws_path; "/app" ] in
  check "no derived path collides with a reserved flat path"
    (List.for_all (fun p -> not (List.mem p reserved)) paths);
  check "every derived path is rooted under the rpc prefix"
    (List.for_all
       (fun p ->
         String.length p >= String.length Tea_rpc.prefix
         && String.equal (String.sub p 0 (String.length Tea_rpc.prefix)) Tea_rpc.prefix)
       paths)

(* --- of_name round-trip and the unknown-name miss ------------------------- *)

let () =
  check "of_name recovers every endpoint by its name"
    (List.for_all
       (fun (S.Any e) ->
         R.of_name (S.name e)
         |> Option.fold ~none:false ~some:(fun (S.Any e') ->
                Tea_rpc.Name.equal (S.name e') (S.name e)))
       S.all);
  check "of_name on an undeployed name is None"
    (Option.is_none (R.of_name (Tea_rpc.Name.v "no_such_endpoint")))

(* --- the cover witness: a wildcard-free total match is the maintenance
   tripwire — a new constructor without an arm here fails to compile. Paired
   with the length equation above. *)
let _cover : type a b. (a, b) S.t -> unit = function
  | S.History_count -> ()
  | S.Doc_stats -> ()
  | S.Append_tag -> ()

let () = check "cover witness compiles and all still has length 3" (List.length S.all = 3)

(* --- the anti-CSRF classification (D12): the table the server gates on ------
   Pinned per constructor, not derived, so a silent reclassification of a
   store-writing endpoint to [Read_only] - which would drop its origin gate -
   fails here rather than in production. *)

let () =
  check "Append_tag is classified Mutating" (S.kind S.Append_tag = Tea_rpc.Mutating);
  check "the two query endpoints are classified Read_only"
    (S.kind S.History_count = Tea_rpc.Read_only && S.kind S.Doc_stats = Tea_rpc.Read_only);
  check "exactly one deployed endpoint is Mutating"
    (List.length
       (List.filter (fun (S.Any e) -> S.kind e = Tea_rpc.Mutating) S.all)
    = 1)

(* --- Doc_stats pure semantics (the one definition server + tests share) ---- *)

let () =
  let stats = S.stats_of { S.title = "hello"; body = "a b c d" } in
  check "stats_of counts space-separated words and title length"
    (stats.S.title_len = 5 && stats.S.word_count = 4);
  (* Tabs and newlines separate words exactly as spaces do, and runs of mixed
     whitespace collapse — the guard against [split_on_char ' '] miscounting
     ["a\tb\nc"] as one word. *)
  check "stats_of treats tab/newline/CR and multi-space runs as one separator"
    ((S.stats_of { S.title = ""; body = "a\tb\nc\r d  e" }).S.word_count = 5);
  check "stats_of counts no words in whitespace-only or empty bodies"
    ((S.stats_of { S.title = "t"; body = " \t\n" }).S.word_count = 0
    && (S.stats_of { S.title = "t"; body = "" }).S.word_count = 0)

(* --- codec round-trips: unit/int edges, canned records, seeded property ---- *)

let () =
  check "History_count req codec round-trips unit"
    (R.encode_req S.History_count () |> R.decode_req S.History_count = Ok ());
  let int_roundtrip v =
    R.encode_resp S.History_count v |> R.decode_resp S.History_count = Ok v
  in
  (* [Repr.int]'s JSON codec is IEEE-754 double backed, so it round-trips
     faithfully only up to the safe-integer boundary 2^53 - 1 — beyond that
     (e.g. OCaml's [max_int]) the JSON number loses precision. [History_count]
     returns a non-negative commit count, so its faithful range is the honest
     edge to pin (deviation from the spec's literal [max_int]). *)
  check "History_count resp codec round-trips 0, 1, and the JSON int-safe max"
    (int_roundtrip 0 && int_roundtrip 1 && int_roundtrip 9007199254740991);
  let req = { S.title = "hello"; body = "a b c d" } in
  check "Doc_stats req codec round-trips a canned value"
    (R.encode_req S.Doc_stats req |> R.decode_req S.Doc_stats
     |> Result.fold ~error:(fun (_ : Codec.err) -> false) ~ok:(fun r -> stats_req_eq r req));
  let resp = { S.title_len = 5; word_count = 4 } in
  check "Doc_stats resp codec round-trips a canned value"
    (R.encode_resp S.Doc_stats resp |> R.decode_resp S.Doc_stats
     |> Result.fold ~error:(fun (_ : Codec.err) -> false) ~ok:(fun r -> stats_resp_eq r resp));
  (* [Append_tag]'s payloads are bare [string]/[int], the shapes most likely to
     be confused for JSON-quoted vs raw; pin both directions on values with
     escaping and an edge count. *)
  check "Append_tag req codec round-trips plain, quoted, and empty tag strings"
    (List.for_all
       (fun t -> R.encode_req S.Append_tag t |> R.decode_req S.Append_tag = Ok t)
       [ "urgent"; ""; "a \"quoted\" tag"; "tab\there" ]);
  check "Append_tag resp codec round-trips a tag count"
    (R.encode_resp S.Append_tag 0 |> R.decode_resp S.Append_tag = Ok 0
    && R.encode_resp S.Append_tag 7 |> R.decode_resp S.Append_tag = Ok 7)

(* One printable-ASCII string (space..'~', so quotes and backslashes appear)
   whose length and content are driven by the LCG — stresses the JSON string
   escaping the codec must reverse. *)
let string_of_raw raw =
  let len = Int64.to_int (Int64.logand raw 0xfL) in
  String.init len (fun i ->
      let shift = i mod 8 * 8 in
      let byte = Int64.to_int (Int64.logand (Int64.shift_right_logical raw shift) 0xffL) in
      Char.chr (32 + (byte mod 95)))

let () =
  let seed = ref 0x5eedL in
  let loop_ok =
    List.fold_left
      (fun acc (_ : int) ->
        let a = Test_util.lcg_next seed in
        let b = Test_util.lcg_next seed in
        let req = { S.title = string_of_raw a; body = string_of_raw b } in
        let round = R.encode_req S.Doc_stats req |> R.decode_req S.Doc_stats in
        acc
        && Result.fold round
             ~error:(fun (_ : Codec.err) -> false)
             ~ok:(fun r -> stats_req_eq r req))
      true
      (List.init 200 Fun.id)
  in
  check "200 seeded stats_req values round-trip through the Doc_stats codec" loop_ok

(* --- decode-error shape: a non-empty reason string ------------------------- *)

let nonempty_reason r =
  r |> Result.fold ~ok:(fun _ -> false) ~error:(fun (Codec.Decode_failed m) -> String.length m > 0)

let () =
  check "of_json on non-JSON reports Decode_failed with a non-empty reason"
    (nonempty_reason (Codec.of_json Repr.int "not json"));
  check "decode_req on a truncated object reports a non-empty reason"
    (nonempty_reason (R.decode_req S.Doc_stats "{"));
  check "decode_req on the wrong JSON shape reports a non-empty reason"
    (nonempty_reason (R.decode_req S.Doc_stats "[]"))

(* --- Prim.Rpc_path.of_string exact-error table ---------------------------- *)

let () =
  check "Rpc_path.of_string rejects empty as Empty"
    (rpc_path_err "" = Some Prim.Rpc_path.Empty);
  check "Rpc_path.of_string rejects an unrooted path as Not_rooted"
    (rpc_path_err "api" = Some Prim.Rpc_path.Not_rooted);
  check "Rpc_path.of_string admits a rooted multi-segment path"
    (Prim.Rpc_path.of_string "/api/save" |> Result.map Prim.Rpc_path.to_string
     = Ok "/api/save");
  check "Rpc_path.of_string rejects '?' as Invalid_char '?'"
    (rpc_path_err "/a?b" = Some (Prim.Rpc_path.Invalid_char '?'));
  check "Rpc_path.of_string rejects a CR byte as Invalid_char '\\r'"
    (rpc_path_err "/a\rb" = Some (Prim.Rpc_path.Invalid_char '\r'))

(* --- Prim.Status boundary table ------------------------------------------- *)

let () =
  check "Status.of_int rejects 0, 99, and 600"
    (Option.is_none (Prim.Status.of_int 0)
    && Option.is_none (Prim.Status.of_int 99)
    && Option.is_none (Prim.Status.of_int 600));
  check "Status.of_int admits 100, 200, 299, 300, and 599"
    (Option.is_some (Prim.Status.of_int 100)
    && Option.is_some (Prim.Status.of_int 200)
    && Option.is_some (Prim.Status.of_int 299)
    && Option.is_some (Prim.Status.of_int 300)
    && Option.is_some (Prim.Status.of_int 599));
  check "Status.is_success is true only across 200..299"
    (is_success 200 && is_success 250 && is_success 299
    && (not (is_success 199))
    && not (is_success 300))

(* --- call expect-closure laws (pure, no transport) ------------------------- *)

(* Pull [expect] out of a command by matching the (private, matchable) [Http]
   constructor; every other constructor is enumerated so a new command verb is
   a compile error here, never a silent [None]. *)
let expect_of : type m.
    m Cmd.t -> ((string, Cmd.http_failure) result -> m) option = function
  | Cmd.Http { expect; delivery = (_ : Cmd.Http_delivery.t); path = (_ : Prim.Rpc_path.t); body = (_ : string) } -> Some expect
  | Cmd.None_ -> None
  | Cmd.Batch (_ : m Cmd.t list) -> None
  | Cmd.Emit (_ : m) -> None
  | Cmd.After ((_ : Prim.Delay.t), (_ : m)) -> None
  | Cmd.Navigate (_ : Prim.Url.t) -> None

let () =
  (* [reply = Fun.id] makes the reply msg the raw [(resp, error) result], so
     the laws read off the payload directly. *)
  let cmd =
    R.call S.Doc_stats { S.title = "hello"; body = "a b c d" } ~reply:Fun.id
  in
  let v = { S.title_len = 5; word_count = 4 } in
  let law expect =
    (* Ok wire that decodes -> Ok payload *)
    let ok_decoded =
      expect (Ok (R.encode_resp S.Doc_stats v))
      |> Result.fold ~error:(fun (_ : Tea_rpc.error) -> false) ~ok:(fun r -> stats_resp_eq r v)
    in
    (* Ok wire that does NOT decode -> Error (Decode _) *)
    let ok_garbage =
      expect (Ok "garbage")
      |> Result.fold ~ok:(fun (_ : S.stats_resp) -> false)
           ~error:(function
             | Tea_rpc.Decode _ -> true
             | Tea_rpc.Transport (_ : Cmd.http_failure) -> false
             | Tea_rpc.Applied_reply_lost -> false)
    in
    (* Error (Http_status s) -> Error (Transport (Http_status s)) *)
    let status_lifts =
      Prim.Status.of_int 503
      |> Option.fold ~none:false ~some:(fun s ->
             expect (Error (Cmd.Http_status s))
             |> Result.fold ~ok:(fun (_ : S.stats_resp) -> false)
                  ~error:(function
                    | Tea_rpc.Transport (Cmd.Http_status s') ->
                      Prim.Status.to_int s' = Prim.Status.to_int s
                    | Tea_rpc.Transport Cmd.Network_error -> false
                    | Tea_rpc.Transport Cmd.No_transport -> false
                    | Tea_rpc.Decode _ -> false
                    | Tea_rpc.Applied_reply_lost -> false))
    in
    (* Error Network_error -> Error (Transport Network_error) *)
    let network_lifts =
      expect (Error Cmd.Network_error)
      |> Result.fold ~ok:(fun (_ : S.stats_resp) -> false)
           ~error:(function
             | Tea_rpc.Transport Cmd.Network_error -> true
             | Tea_rpc.Transport (Cmd.Http_status (_ : Prim.Status.t)) -> false
             | Tea_rpc.Transport Cmd.No_transport -> false
             | Tea_rpc.Decode _ -> false
             | Tea_rpc.Applied_reply_lost -> false)
    in
    ok_decoded && ok_garbage && status_lifts && network_lifts
  in
  check "call's expect closure obeys the four transport/decode laws"
    (expect_of cmd |> Option.fold ~none:false ~some:law)

(* --- the delivery key's wire grammar (roadmap step 15, D20) --------------- *)

let key_err s =
  Tea_rpc.Key.of_string s
  |> Result.fold ~ok:(fun (_ : Tea_rpc.Key.t) -> None) ~error:Option.some

(* Spelled out rather than sliced: [String.sub] would be a partial accessor in
   a file whose whole subject is total parsing. *)
let hex32 = "0123456789abcdef0123456789abcdef"
let hex31 = "0123456789abcdef0123456789abcde"
let hex32_upper_head = "A123456789abcdef0123456789abcdef"

let a_tab =
  Prim.Tab_id.of_string hex32
  |> Result.fold ~error:(fun (_ : Prim.Tab_id.err) -> None) ~ok:Option.some

let () =
  (* The one surviving header-literal pin. Both tiers read this single
     definition, so a one-byte drift is not a typo: it is a deploy where every
     request arrives keyless and the de-duplication silently does nothing. *)
  check "the key header literal is x-tea-key" (String.equal Tea_rpc.key_header "x-tea-key");
  check "Key round-trips through to_string / of_string"
    (a_tab
    |> Option.fold ~none:false ~some:(fun tab ->
           let k = Tea_rpc.Key.v ~tab ~seq:Prim.Msg_seq.one in
           String.equal (Tea_rpc.Key.to_string k) (hex32 ^ ":1")
           && Tea_rpc.Key.of_string (Tea_rpc.Key.to_string k)
              |> Result.fold
                   ~error:(fun (_ : Tea_rpc.Key.err) -> false)
                   ~ok:(fun k' ->
                     Prim.Tab_id.compare (Tea_rpc.Key.tab k') tab = 0
                     && Prim.Msg_seq.compare (Tea_rpc.Key.seq k') Prim.Msg_seq.one = 0)));
  check "Key.of_string rejects a missing and a doubled separator as Bad_shape"
    (key_err hex32 = Some Tea_rpc.Key.Bad_shape
    && key_err (hex32 ^ ":1:2") = Some Tea_rpc.Key.Bad_shape);
  check "Key.of_string rejects a 31-hex tab as Bad_tab Wrong_length"
    (key_err (hex31 ^ ":1") = Some (Tea_rpc.Key.Bad_tab Prim.Tab_id.Wrong_length));
  check "Key.of_string rejects an uppercase tab byte as Bad_tab (Invalid_char 'A')"
    (key_err (hex32_upper_head ^ ":1")
    = Some (Tea_rpc.Key.Bad_tab (Prim.Tab_id.Invalid_char 'A')));
  (* [int_of_string_opt] alone would ADMIT the sign, the hex, the underscore
     AND the zero-padded spelling, giving one position in the stream several
     accepted spellings - which is precisely what a dense-sequence guard
     cannot tolerate. The parse accepts exactly the canonical print of the
     value it returns, so the aliases are closed by construction rather than
     by enumeration (F4 of the step-15 adversarial review). *)
  check
    "Key.of_string rejects 0, a sign, padding, hex, an underscore, and leading zeros as Bad_seq"
    (List.for_all
       (fun s -> key_err (hex32 ^ ":" ^ s) = Some Tea_rpc.Key.Bad_seq)
       [ "0"; "-1"; "+1"; " 1"; "1 "; "0x10"; "1_0"; ""; "one"; "007"; "01"; "00" ])

(* --- the keyed_resp envelope, and the deploy-skew wall -------------------- *)

let keyed_int_t = Tea_rpc.keyed_resp_t Repr.int

let () =
  let enveloped = Codec.to_json keyed_int_t (Tea_rpc.Reply 7) in
  let replayed = Codec.to_json keyed_int_t Tea_rpc.Replayed in
  let bare = Codec.to_json Repr.int 7 in
  check "keyed_resp round-trips both arms"
    (Codec.of_json keyed_int_t enveloped
     |> Result.fold
          ~error:(fun (_ : Codec.err) -> false)
          ~ok:(function
            | Tea_rpc.Reply n -> n = 7
            | Tea_rpc.Replayed -> false)
    && Codec.of_json keyed_int_t replayed
       |> Result.fold
            ~error:(fun (_ : Codec.err) -> false)
            ~ok:(function
              | Tea_rpc.Reply (_ : int) -> false
              | Tea_rpc.Replayed -> true));
  (* A mixed deploy must fail LOUDLY rather than silently read a wrong value,
     so neither codec may accept the other's bytes. Pinned in both directions:
     a stale pre-envelope bundle gets a typed Decode, and a pre-envelope server
     answering a current bundle gets one too. *)
  check "a bare body does not decode as keyed_resp"
    (Result.is_error (Codec.of_json keyed_int_t bare));
  check "an enveloped body does not decode as the bare resp"
    (Result.is_error (Codec.of_json Repr.int enveloped));
  check "Replayed's wire form is not the bare resp either"
    (Result.is_error (Codec.of_json Repr.int replayed))

(* --- call reads the channel off the endpoint's kind ----------------------- *)

(* The delivery classification of a built command, pulled out the same way
   [expect_of] pulls the closure; every other verb enumerated so a new command
   constructor is a compile error rather than a silent [None]. *)
let delivery_of : type m. m Cmd.t -> Cmd.Http_delivery.t option = function
  | Cmd.Http
      { delivery
      ; path = (_ : Prim.Rpc_path.t)
      ; body = (_ : string)
      ; expect = (_ : (string, Cmd.http_failure) result -> m)
      } ->
    Some delivery
  | Cmd.None_ -> None
  | Cmd.Batch (_ : m Cmd.t list) -> None
  | Cmd.Emit (_ : m) -> None
  | Cmd.After ((_ : Prim.Delay.t), (_ : m)) -> None
  | Cmd.Navigate (_ : Prim.Url.t) -> None

let is_bare (d : Cmd.Http_delivery.t) : bool =
  match d with
  | Cmd.Http_delivery.Bare -> true
  | Cmd.Http_delivery.Keyed -> false

let keyed_tag_t = Tea_rpc.keyed_resp_t (S.resp_t S.Append_tag)

let () =
  let read_only_cmd = R.call S.Doc_stats { S.title = "t"; body = "b" } ~reply:Fun.id in
  let mutating_cmd = R.call S.Append_tag "urgent" ~reply:Fun.id in
  (* An app never picks a delivery contract and never mints a key: the
     endpoint's own [kind] witness picks the channel, which is what makes
     "classify it Mutating" the whole of the app-facing story. *)
  check "call puts a Read_only endpoint on the Bare channel"
    (delivery_of read_only_cmd |> Option.fold ~none:false ~some:is_bare);
  check "call puts a Mutating endpoint on the Keyed channel"
    (delivery_of mutating_cmd
    |> Option.fold ~none:false ~some:(fun d -> not (is_bare d)));
  (* The decode follows the channel it chose. *)
  check "the Mutating expect decodes Reply into Ok"
    (expect_of mutating_cmd
    |> Option.fold ~none:false ~some:(fun expect ->
           expect (Ok (Codec.to_json keyed_tag_t (Tea_rpc.Reply 3))) = Ok 3));
  (* The one outcome an app must act on rides the 200 channel as a typed arm,
     never a status: effect certain, value lost, and distinguishable from a
     transport failure, where the effect's fate is unknown. *)
  check "the Mutating expect turns Replayed into Applied_reply_lost"
    (expect_of mutating_cmd
    |> Option.fold ~none:false ~some:(fun expect ->
           expect (Ok (Codec.to_json keyed_tag_t Tea_rpc.Replayed))
           = Error Tea_rpc.Applied_reply_lost));
  check "the Mutating expect refuses a bare body as a typed Decode"
    (expect_of mutating_cmd
    |> Option.fold ~none:false ~some:(fun expect ->
           expect (Ok (R.encode_resp S.Append_tag 3))
           |> Result.fold
                ~ok:(fun (_ : int) -> false)
                ~error:(function
                  | Tea_rpc.Decode _ -> true
                  | Tea_rpc.Transport (_ : Cmd.http_failure) -> false
                  | Tea_rpc.Applied_reply_lost -> false)));
  check "the Read_only expect still reads a bare body"
    (expect_of read_only_cmd
    |> Option.fold ~none:false ~some:(fun expect ->
           expect (Ok (R.encode_resp S.Doc_stats { S.title_len = 1; word_count = 1 }))
           |> Result.fold
                ~error:(fun (_ : Tea_rpc.error) -> false)
                ~ok:(fun (s : S.stats_resp) -> s.S.title_len = 1)))

let () = print_endline "rpc_test: all checks passed"
