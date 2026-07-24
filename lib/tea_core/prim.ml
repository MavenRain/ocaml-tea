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

  let of_string s =
    let n = String.length s in
    if n = 0 then Error Empty
    else if s.[0] <> '/' then Error Not_relative
    else if n >= 2 && s.[1] = '/' then Error Not_relative (* // = protocol-relative escape *)
    else Ok s

  let to_string t = t
end

module Tag = struct
  type t = string

  let v s = s
  let to_string t = t
end

module Attr_name = struct
  type t = string

  type err =
    | Empty
    | Event_handler

  let is_event_handler s =
    String.length s >= 2
    && Char.lowercase_ascii s.[0] = 'o'
    && Char.lowercase_ascii s.[1] = 'n'

  let of_string s =
    if String.length s = 0 then Error Empty
    else if is_event_handler s then Error Event_handler
    else Ok s

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

  let to_string t = t
end

module Branch_name = struct
  type t = string

  let main = "main"
  let of_session sid = "session-" ^ Session_id.to_string sid

  let of_string s =
    if String.length s = 0 then None
    else if String.contains s ' ' then None
    else Some s

  let to_string t = t
end

module Commit_ref = struct
  type t = string

  let of_hash h = h
  let to_string t = t
end
