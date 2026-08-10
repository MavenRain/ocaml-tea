(* Shared byte predicates. Kept as boolean comparisons rather than char pattern
   matches, so no [_ ->] wildcard arm is needed over the 256-value char space. *)
let is_control_byte c = Char.code c < 0x20 || Char.code c = 0x7f
let is_lower c = c >= 'a' && c <= 'z'
let is_upper c = c >= 'A' && c <= 'Z'
let is_digit c = c >= '0' && c <= '9'

let first_where (pred : char -> bool) (s : string) : char option =
  String.to_seq s |> Seq.find_map (fun c -> if pred c then Some c else None)

module Title = struct
  type t = string

  let v s = s
  let to_string t = t
end

module Url = struct
  type t = string

  type err =
    | Empty
    | Not_relative
    | Backslash
    | Control_char of char

  (* Run AFTER the relative-only checks. A '\' anywhere is rejected because
     browsers normalise it to '/', so [/\evil.com] would read as the
     protocol-relative [//evil.com] once the [s.[1] <> '/'] guard has passed. A
     control byte anywhere is rejected because a [t] feeds the [Location]
     response header, where a CR or LF is response splitting. *)
  let of_string s =
    let n = String.length s in
    if n = 0 then Error Empty
    else if s.[0] <> '/' then Error Not_relative
    else if n >= 2 && s.[1] = '/' then Error Not_relative (* // = protocol-relative escape *)
    else if String.contains s '\\' then Error Backslash
    else first_where is_control_byte s |> Option.fold ~none:(Ok s) ~some:(fun c -> Error (Control_char c))

  let to_string t = t
end

module Tag = struct
  type t = string

  type err =
    | Empty
    | Invalid_char of char

  (* [a-z][a-z0-9-]*: HTML elements and custom elements. The tag position has
     no escaping at render, so this charset is the entire guard. *)
  let is_tail_char c = is_lower c || is_digit c || Char.equal c '-'

  let of_string s =
    if String.length s = 0 then Error Empty
    else if not (is_lower s.[0]) then Error (Invalid_char s.[0])
    else
      first_where (fun c -> not (is_tail_char c)) s
      |> Option.fold ~none:(Ok s) ~some:(fun c -> Error (Invalid_char c))

  let v s = s
  let to_string t = t
end

module Attr_name = struct
  type t = string

  type err =
    | Empty
    | Event_handler
    | Invalid_char of char

  let is_event_handler s =
    String.length s >= 2
    && Char.lowercase_ascii s.[0] = 'o'
    && Char.lowercase_ascii s.[1] = 'n'

  (* Allowlist [A-Za-z0-9_.:-] (covers class, data-*, aria-*, xlink:href,
     viewBox). Names are emitted UNescaped at render, so one accepted name like
     [x onmouseover=alert(1) y] would smuggle a live handler; the charset is
     the guard. *)
  let is_name_char c = is_lower c || is_upper c || is_digit c || String.contains "_.:-" c

  let of_string s =
    if String.length s = 0 then Error Empty
    else if is_event_handler s then Error Event_handler
    else
      first_where (fun c -> not (is_name_char c)) s
      |> Option.fold ~none:(Ok s) ~some:(fun c -> Error (Invalid_char c))

  let v s = s
  let to_string t = t
end

module Attr_value = struct
  type t = string

  let v s = s
  let to_string t = t
end

module Text = struct
  type t = string

  let v s = s
  let to_string t = t
end

module Input_text = struct
  type t = string

  let v s = s
  let to_string t = t
end

module Delay = struct
  type t = int

  let of_ms n = if n < 0 then None else Some n
  let to_ms t = t
end

module Interval = struct
  type t = int

  let of_ms n = if n <= 0 then None else Some n
  let to_ms t = t
end

module Fuel = struct
  type t = int

  let default = 1000
  let of_int n = if n < 0 then None else Some n
  let burn t = if t <= 0 then None else Some (t - 1)
  let to_int t = t
end

module Session_id = struct
  type t = string

  let of_string s =
    if String.length s = 0 then None
    else if String.contains s '/' then None
    else Some s

  let v s = s
  let to_string t = t
  let compare = String.compare
  let t = Repr.string
end

module Retention = struct
  type t = int

  let of_int n = if n > 0 then Some n else None
  let default = 8
  let to_int t = t
end

module Ttl = struct
  type t = float

  let of_seconds s = if s > 0. then Some s else None
  let to_seconds t = t
end

module Branch_name = struct
  type t = string

  let main = "main"
  let of_session sid = "session-" ^ Session_id.to_string sid

  (* Charset [A-Za-z0-9._-]+ with no leading '-' or '.', so a branch name is a
     single ref path component that cannot begin an option-looking or
     hidden-file-looking token. [main] and [session-<hex>] both pass. *)
  let is_branch_char c = is_lower c || is_upper c || is_digit c || String.contains "._-" c

  let of_string s =
    if String.length s = 0 then None
    else if Char.equal s.[0] '-' || Char.equal s.[0] '.' then None
    else first_where (fun c -> not (is_branch_char c)) s |> Option.fold ~none:(Some s) ~some:(fun _ -> None)

  let to_string t = t
end

module Commit_ref = struct
  type t = string

  let of_hash h = h
  let to_string t = t
end

module Rpc_path = struct
  type t = string

  type err =
    | Empty
    | Not_rooted
    | Invalid_char of char

  (* [A-Za-z0-9/_.-], rooted: the path is spliced into the client's
     [XHR.open_] and matched byte-for-byte by the server router, so the
     charset closes URL-metachar smuggling (? # \) and control-byte injection
     at admission. *)
  let is_path_char c =
    is_lower c || is_upper c || is_digit c || Char.equal c '/'
    || Char.equal c '_' || Char.equal c '.' || Char.equal c '-'

  let of_string s =
    if String.length s = 0 then Error Empty
    else if not (Char.equal s.[0] '/') then Error Not_rooted
    else
      first_where (fun c -> not (is_path_char c)) s
      |> Option.fold ~none:(Ok s) ~some:(fun c -> Error (Invalid_char c))

  let v s = s
  let to_string t = t
end

module Status = struct
  type t = int

  let of_int n = if n >= 100 && n <= 599 then Some n else None
  let is_success t = t >= 200 && t <= 299
  let to_int t = t
end

module Tab_id = struct
  type t = string

  type err =
    | Wrong_length
    | Invalid_char of char

  (* 16 bytes as lowercase hex. The length is fixed on the decode path because
     this value is client-chosen and becomes a key in a bounded server-side
     table: a count bound only bounds memory if each key's size is bounded. *)
  let hex_length = 32
  let byte_count = 16
  let is_hex c = is_digit c || (c >= 'a' && c <= 'f')

  let of_string s =
    if Int.equal (String.length s) hex_length then
      first_where (fun c -> not (is_hex c)) s
      |> Option.fold ~none:(Ok s) ~some:(fun c -> Error (Invalid_char c))
    else Error Wrong_length

  let of_bytes bs =
    if
      Int.equal (List.length bs) byte_count
      && List.for_all (fun b -> b >= 0 && b <= 255) bs
    then Some (String.concat "" (List.map (fun b -> Printf.sprintf "%02x" b) bs))
    else None

  (* Total by construction: the count is fixed here and every draw is masked
     into range, so there is no refusal for a caller to handle with a constant
     id — which would be the tab-collision bug wearing a fallback's clothes. *)
  let of_draws draw =
    String.concat ""
      (List.init byte_count (fun (_ : int) -> Printf.sprintf "%02x" (draw () land 0xff)))

  let to_string t = t
  let compare = String.compare
  let t = Repr.string
end

module Msg_seq = struct
  type t = int

  let one = 1
  let next t = if t >= max_int then None else Some (t + 1)
  let of_int n = if n < 1 then None else Some n
  let to_int t = t
  let compare = Int.compare
  let t = Repr.int
end

module Store_water = struct
  type t = int64

  (* Below every date [Clock] can mint, so [bottom] is a true lattice bottom
     rather than a sentinel a real date could collide with. *)
  let bottom = Int64.min_int
  let of_date (date : int64) : t = date
  let covers ~(head : t) ~(floor : t) : bool = Int64.compare head floor >= 0
  let compare = Int64.compare
  let equal = Int64.equal
  let to_int64 (t : t) : int64 = t
  let t = Repr.int64
end

module Store_identity = struct
  type t = string

  type err =
    | Wrong_length
    | Invalid_char of char

  (* The [Tab_id] shape, one layer up: 16 bytes as lowercase hex, and no
     trusted [v] mint, so every inhabitant arrives validated. Uppercase is a
     refusal rather than a normalisation, because a second spelling of one
     store's token reads as a foreign store. *)
  let hex_length = 32
  let byte_count = 16
  let is_hex c = is_digit c || (c >= 'a' && c <= 'f')

  let of_string s =
    if Int.equal (String.length s) hex_length then
      first_where (fun c -> not (is_hex c)) s
      |> Option.fold ~none:(Ok s) ~some:(fun c -> Error (Invalid_char c))
    else Error Wrong_length

  let of_bytes bs =
    if
      Int.equal (List.length bs) byte_count
      && List.for_all (fun b -> b >= 0 && b <= 255) bs
    then Some (String.concat "" (List.map (fun b -> Printf.sprintf "%02x" b) bs))
    else None

  (* Total by construction, the [Tab_id.of_draws] reason: a refusal here would
     invite a constant token, which is the store-collision bug wearing a
     fallback's clothes. *)
  let of_draws draw =
    String.concat ""
      (List.init byte_count (fun (_ : int) -> Printf.sprintf "%02x" (draw () land 0xff)))

  let to_string t = t
  let equal = String.equal
  let compare = String.compare

  type binding =
    | Bound of t
    | Unresolved
end

module Store_epoch = struct
  type t = int64

  (* The [Clock] shape, one layer up: a monotone counter that SATURATES at
     the top rather than wrap, because a wrap would re-mint an old stamp and
     an old stamp is exactly what this counter exists to expose. *)
  let bottom = 0L
  let succ t = if Int64.equal t Int64.max_int then t else Int64.add t 1L
  let compare = Int64.compare
  let equal = Int64.equal
  let hex_length = 16

  (* Fixed-width lowercase hex, the [Store_identity] encoding one value
     down: no padding ambiguity, no second spelling of one stamp. The domain
     is [bottom, max_int] on BOTH sides: [to_string] only ever prints values
     the mint can reach, and [of_string] refuses the top half of the 16-hex
     space, because [succ] saturates only at [max_int] itself - a value
     above it would WRAP back through zero, and a wrap re-mints old stamps,
     which is exactly what this counter exists to expose. The mint never
     emits one, so refusing costs nothing ([Store_identity.of_string]'s
     uppercase rule, one failure class over). Reads forward: length, then
     charset, then parse, then the domain gate. *)
  let to_string t = Printf.sprintf "%016Lx" t

  let of_string s =
    if
      Int.equal (String.length s) hex_length
      && Option.is_none (first_where (fun c -> not (Store_identity.is_hex c)) s)
    then
      Int64.of_string_opt ("0x" ^ s)
      |> Option.fold ~none:(Error `Malformed) ~some:(fun (t : int64) ->
             if Int64.compare t 0L >= 0 then Ok t else Error `Malformed)
    else Error `Malformed

  type binding =
    | Bound of t
    | Unresolved
end
