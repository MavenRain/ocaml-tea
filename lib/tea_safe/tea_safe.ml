(* Shared byte predicates. Kept as boolean comparisons rather than char
   pattern matches so no [_ ->] wildcard arm is needed on the 256-value char
   space. *)
let is_control_byte c = Char.code c < 0x20 || Char.code c = 0x7f

let first_where (pred : char -> bool) (s : string) : char option =
  String.to_seq s |> Seq.find_map (fun c -> if pred c then Some c else None)

module Proof = struct
  (* Phantom abbreviation: the parameter never appears on the right, so a token
     carries no data and, sealed abstract in the .mli with no repr in scope, has
     no serialization witness. *)
  type 'p t = unit

  module Mint () = struct
    type cap = unit

    let grant () : cap t = ()
  end
end

module Origin_gate = struct
  (* The sole capability family for the same-origin decision; generative, so no
     other application of Proof.Mint can forge a compatible token. *)
  module Cap = Proof.Mint ()

  type same_origin = Cap.cap

  type denial =
    | Origin_missing
    | Host_missing
    | Both_missing
    | Origin_mismatch

  (* Exact full-string match against both scheme forms, never a prefix test. *)
  let matches host origin =
    String.equal origin ("http://" ^ host) || String.equal origin ("https://" ^ host)

  let check ~origin ~host =
    match origin, host with
    | Some o, Some h -> if matches h o then Ok (Cap.grant ()) else Error Origin_mismatch
    | None, Some _ -> Error Origin_missing
    | Some _, None -> Error Host_missing
    | None, None -> Error Both_missing
end

module Safe_key = struct
  module Step = struct
    type t = string

    type err =
      | Empty
      | Dot
      | Dot_dot
      | Separator
      | Control_char of char

    (* Classify the first offending byte: a separator escapes the namespace, a
       control byte smuggles structure into the printed path. *)
    let first_bad s =
      String.to_seq s
      |> Seq.find_map (fun c ->
             if Char.equal c '/' then Some Separator
             else if is_control_byte c then Some (Control_char c)
             else None)

    let of_string s =
      if String.length s = 0 then Error Empty
      else if String.equal s "." then Error Dot
      else if String.equal s ".." then Error Dot_dot
      else first_bad s |> Option.fold ~none:(Ok s) ~some:Result.error

    let v s = s
    let to_string t = t
  end

  (* Nonempty by construction: [root] seeds the head, [( / )] only extends. *)
  type t = string list

  let root step = [ Step.to_string step ]
  let ( / ) t step = t @ [ Step.to_string step ]
  let to_steps t = t
end

module Safe_path = struct
  type t = string list

  type err =
    | Empty
    | Absolute
    | Backslash
    | Dot_segment
    | Dot_dot_segment
    | Empty_segment
    | Control_char of char

  let segment_err seg =
    if String.equal seg "" then Some Empty_segment
    else if String.equal seg "." then Some Dot_segment
    else if String.equal seg ".." then Some Dot_dot_segment
    else None

  let of_string s =
    if String.length s = 0 then Error Empty
    else if Char.equal s.[0] '/' then Error Absolute
    else if String.contains s '\\' then Error Backslash
    else
      first_where is_control_byte s
      |> Option.fold
           ~some:(fun c -> Error (Control_char c))
           ~none:
             (let segs = String.split_on_char '/' s in
              List.find_map segment_err segs |> Option.fold ~none:(Ok segs) ~some:Result.error)

  let segments t = t
  let to_string t = String.concat "/" t
end

module Safe_cmd = struct
  let has_nul s = String.contains s '\000'

  module Program = struct
    type t = string

    type err =
      | Empty
      | Nul

    let of_string s =
      if String.length s = 0 then Error Empty else if has_nul s then Error Nul else Ok s

    let v s = s
  end

  module Arg = struct
    type t = string

    type err = Nul

    let of_string s = if has_nul s then Error Nul else Ok s
  end

  (* A program paired with its argv; no representation carries a single shell
     line, so injection by concatenation cannot be written. *)
  type t = string * string list

  let v (prog : Program.t) (args : Arg.t list) : t = prog, args
  let program (prog, _) = prog
  let args (_, argv) = argv
end

module Header = struct
  let is_alpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
  let is_digit c = c >= '0' && c <= '9'

  module Name = struct
    type t = string

    type err =
      | Empty
      | Invalid_char of char

    (* RFC 7230 token characters. *)
    let is_token_char c = is_alpha c || is_digit c || String.contains "!#$%&'*+-.^_`|~" c

    let of_string s =
      if String.length s = 0 then Error Empty
      else
        first_where (fun c -> not (is_token_char c)) s
        |> Option.fold ~none:(Ok s) ~some:(fun c -> Error (Invalid_char c))

    let v s = s
    let to_string t = t
  end

  module Value = struct
    type t = string

    type err = Control_char of char

    (* A header value cannot fold onto a new line: reject every control byte
       except HTAB, which is legal linear whitespace inside a value. *)
    let is_bad c = is_control_byte c && not (Char.equal c '\t')

    let of_string s =
      first_where is_bad s |> Option.fold ~none:(Ok s) ~some:(fun c -> Error (Control_char c))

    let to_string t = t
  end

  type t = string * string

  let v (n : Name.t) (value : Value.t) : t = Name.to_string n, Value.to_string value
  let name (n, _) = n
  let value (_, v) = v
end

module Redirect = struct
  module Allowlist = struct
    type t = string list

    let v origins = origins
  end

  type t = string

  type err =
    | Empty
    | Control_char of char
    | Not_allowlisted

  (* Anchored: equal to a listed origin, or under it past a literal '/'. The
     '/' anchor is what defeats the suffix and userinfo tricks. *)
  let under origin candidate =
    String.equal candidate origin || String.starts_with ~prefix:(origin ^ "/") candidate

  let external_of_string (allow : Allowlist.t) s =
    if String.length s = 0 then Error Empty
    else
      first_where is_control_byte s
      |> Option.fold
           ~some:(fun c -> Error (Control_char c))
           ~none:(if List.exists (fun origin -> under origin s) allow then Ok s else Error Not_allowlisted)

  let to_string t = t
end

module Security_headers = struct
  module Frame = struct
    type t =
      | Deny
      | Same_origin
  end

  module Source = struct
    type t =
      | None_
      | Self
  end

  type t =
    { frame : Frame.t
    ; default_src : Source.t
    ; script_src : Source.t
    ; style_src : Source.t
    ; img_src : Source.t
    ; connect_src : Source.t
    ; form_action : Source.t
    }

  let v ~frame ~default_src ~script_src ~style_src ~img_src ~connect_src ~form_action =
    { frame; default_src; script_src; style_src; img_src; connect_src; form_action }

  let strict =
    v ~frame:Frame.Deny ~default_src:Source.Self ~script_src:Source.Self ~style_src:Source.Self
      ~img_src:Source.Self ~connect_src:Source.Self ~form_action:Source.Self

  let source_str = function
    | Source.None_ -> "'none'"
    | Source.Self -> "'self'"

  let frame_ancestors_str = function
    | Frame.Deny -> "'none'"
    | Frame.Same_origin -> "'self'"

  let x_frame_options_str = function
    | Frame.Deny -> "DENY"
    | Frame.Same_origin -> "SAMEORIGIN"

  let csp t =
    String.concat "; "
      [ "default-src " ^ source_str t.default_src
      ; "script-src " ^ source_str t.script_src
      ; "style-src " ^ source_str t.style_src
      ; "img-src " ^ source_str t.img_src
      ; "connect-src " ^ source_str t.connect_src
      ; "form-action " ^ source_str t.form_action
      ; "object-src 'none'"
      ; "base-uri 'none'"
      ; "frame-ancestors " ^ frame_ancestors_str t.frame
      ]

  (* Values here are built from the finite sums above, so they are control-byte
     free by construction; within this sealed unit [Header.Value.t] is a plain
     string, so [Header.v] accepts them directly. The tests re-pass every
     emitted value through [Header.Value.of_string] to prove it. *)
  let to_headers t =
    [ Header.v (Header.Name.v "Content-Security-Policy") (csp t)
    ; Header.v (Header.Name.v "X-Frame-Options") (x_frame_options_str t.frame)
    ; Header.v (Header.Name.v "X-Content-Type-Options") "nosniff"
    ]
end
