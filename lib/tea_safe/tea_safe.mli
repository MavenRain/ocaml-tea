(** Security primitives as enforced module boundaries (roadmap step 5, risk
    R7). Each entry in the DESIGN section 9 mapping table becomes an abstract
    type whose only inhabitants are values that already passed the check, so an
    illegal state (an [on*] attribute name, a [..] path segment, a [CR]/[LF] in
    a response header, a forged capability) has no representation rather than a
    runtime guard that a caller can forget.

    This library links {b nothing}, not even [repr]. That is load bearing for
    R7: with no [Repr.t] in scope, a serialization witness for {!Proof.t} cannot
    be written inside the library, and none can be derived outside it because
    the type is abstract with no constructor, accessor, equality, or printer. A
    capability token therefore cannot enter a model, a store, or a wire frame
    through the typed surface. The zero-dependency rule also keeps the unit
    compiling identically for native and js_of_ocaml, alongside {!Tea_core}.

    The XSS and open-redirect primitives whose sinks live inside {!Tea_core}
    (attribute names, tag names, the redirect [Url]) are hardened in place in
    [Tea_core.Prim]; they are not relocated here, because [Tea_core] depends
    only on [repr] and must stay that way. What lives here is the vocabulary the
    server and store tiers speak: capability proofs, store keys, response
    headers, content-security policy, filesystem paths, and process argv. *)

(** {1 Capabilities (R7)} *)

module Proof : sig
  (** An unforgeable witness that a named check succeeded. A ['p t] proves that
      the check owning the phantom tag ['p] ran and passed; a sink demands it as
      an argument, so a call path that skipped the check is a type error rather
      than a forgotten [if]. This replaces the bare-bool-and-branch gate, whose
      failure mode is a future call site that simply does not consult the bool.

      Never serializable: this library links no [repr], and [t] exposes no
      constructor, accessor, equality, or printer, so a token cannot be placed
      in a [Repr.t] and thus cannot reach a model, the store, or a wire frame.
      Honest limits: [Marshal] can serialize any runtime value (the guarantee
      covers the typed surface, not the runtime's escape hatches), and the type
      system cannot stop a token captured in a closure that outlives its
      request; request scoping is a contract each mint site states. *)
  type 'p t

  (** Mint a capability family. Each {i application} of this generative functor
      yields a fresh, incompatible {!cap} and the sole means of producing a
      [cap t]. Apply it once, at module level, inside the module that owns the
      check, keep the resulting module private, and export only [cap]-typed
      results of your own checking function, so every [cap t] in the program is
      downstream of that check. Generativity (the [()] argument) is what stops a
      second application from minting a compatible token. *)
  module Mint () : sig
    type cap

    val grant : unit -> cap t
  end
end

(** {1 Same-origin WebSocket gate (Proof's live instance)} *)

module Origin_gate : sig
  (** The browser-origin gate for the WebSocket handshake, and the live wiring
      of {!Proof} (risk R7). Browsers do not enforce same-origin on WebSocket
      connects but always send [Origin] on the handshake, so exact-matching it
      against [http(s)://<Host>] is the whole cross-site-hijacking guard;
      non-browser clients must supply [Origin]. Every header-presence case is
      enumerated and the default is deny. The one and only producer of a
      [same_origin Proof.t] in the program is {!check}, so a WebSocket accepted
      without that proof cannot be expressed. *)
  type same_origin

  type denial =
    | Origin_missing
    | Host_missing
    | Both_missing
    | Origin_mismatch

  (** [check ~origin ~host] mints proof when [origin] equals [http://host] or
      [https://host] exactly (never a prefix or suffix match, which is the
      [trusted.example.com.evil.io] trap). Both headers absent, either absent,
      or a mismatch each yield a distinct {!denial}. *)
  val check : origin:string option -> host:string option -> (same_origin Proof.t, denial) result
end

(** {1 Store keys (SafeQuery and SafeKey: the no-SQL analogue)} *)

module Safe_key : sig
  (** Irmin tree paths (DESIGN section 9 rows 4a and 4b: SafeQuery and SafeKey;
      Irmin has no SQL, so the tree path is the injectable coordinate). A step
      that typechecks cannot escape its namespace: ['/'] (the printed-path
      separator), [NUL] and every control byte, [""], ["."] and [".."] are
      rejected at the boundary, and a key is nonempty by construction ({!root}
      takes its head as an argument, so the empty key, which would address the
      tree root, has no representation). Honest limit: this guards the path
      grammar, not authorization; branch isolation is [Session_id] and
      [Branch_name]'s job. *)
  module Step : sig
    type t

    type err =
      | Empty
      | Dot
      | Dot_dot
      | Separator
      | Control_char of char

    (** Untrusted key steps. The exploded-tree future (per-field paths derived
        from application data) enters here and nowhere else. *)
    val of_string : string -> (t, err) result

    (** Framework-internal trusted mint for compile-time literals ([v "model"]);
        never request- or model-derived data (audit: [rg 'Step.v']). *)
    val v : string -> t

    val to_string : t -> string
  end

  type t

  (** A single-step key. Its argument makes the empty key unrepresentable. *)
  val root : Step.t -> t

  (** Extend a key by one step, deeper in the tree. *)
  val ( / ) : t -> Step.t -> t

  (** The one bridge to Irmin's raw string-list path; total by construction,
      since every element came through {!Step.of_string} or a {!Step.v}
      literal. Once a path is a {!t}, a future dynamic path must produce
      {!Step.t} values, so the compiler, not a reviewer, catches the first
      exploded-tree author who would splice a raw string. *)
  val to_steps : t -> string list
end

(** {1 Filesystem paths (SafePath)} *)

module Safe_path : sig
  (** A relative filesystem path that provably cannot climb (SafePath row).
      Splitting on ['/'], every segment is nonempty and none is ["."] or [".."];
      a leading ['/'], backslash separators, [NUL], and control bytes are
      rejected. Scope is POSIX-relative strings only. Honest limits: symlinks
      inside the root are an OS concern the type cannot see, and validation
      applies to the {i decoded} string, so callers must percent-decode before
      {!of_string} (Dream does), or ["..%2f"] reads as an inert literal here and
      a traversal at the sink. Boundary-only this milestone: the only filesystem
      access in the framework is [Dream.static], which self-guards; this is the
      type any future framework file API must consume. *)
  type t

  type err =
    | Empty
    | Absolute
    | Backslash
    | Dot_segment
    | Dot_dot_segment
    | Empty_segment
    | Control_char of char

  val of_string : string -> (t, err) result

  val segments : t -> string list

  (** ['/']-joined, never leading ['/'], never containing a [".."] segment, so
      it always names a location at or below the root it is resolved against. *)
  val to_string : t -> string
end

(** {1 Process invocations (SafeCmd)} *)

module Safe_cmd : sig
  (** Process invocations as exec-shaped argv, never a shell line (SafeCmd row).
      There is deliberately no [to_shell_string] and no [of_shell_string]: the
      only readback is exec-array shaped, so injection by concatenation is
      unrepresentable rather than escaped. [NUL] is rejected because [execve]
      arguments are [NUL]-terminated, so a [NUL] would silently truncate and
      rewrite the command; every other byte, including the shell metacharacters
      [';'], ['|'], ['$'], and quotes, is inert data here. No process is spawned
      in this module (that would drag [Unix] in and break js purity); a future
      server-side effect consumes {!program} and {!args}. *)
  module Program : sig
    type t

    type err =
      | Empty
      | Nul

    val of_string : string -> (t, err) result

    (** Trusted operator literal ([v "convert"]). *)
    val v : string -> t
  end

  module Arg : sig
    type t

    type err = Nul

    (** Untrusted argument bytes; every byte except [NUL] is admitted. *)
    val of_string : string -> (t, err) result
  end

  type t

  val v : Program.t -> Arg.t list -> t

  val program : t -> string

  val args : t -> string list
end

(** {1 Response headers (header injection)} *)

module Header : sig
  (** Response headers that cannot split (header-injection row). {!Value} has no
      trusted bypass on purpose: a value is where injection lives, so every
      value passes the control-byte check. *)
  module Name : sig
    type t

    type err =
      | Empty
      | Invalid_char of char

    (** RFC 7230 token characters only; a name admitting [':'] or [CR]/[LF]
        would be a whole forged header. *)
    val of_string : string -> (t, err) result

    (** Trusted compile-time literal ([v "X-Frame-Options"]). *)
    val v : string -> t

    val to_string : t -> string
  end

  module Value : sig
    type t

    (** [CR], [LF], [NUL], every byte below [0x20] except [HTAB], and [0x7f]: a
        value that cannot contain a line break cannot split a response. *)
    type err = Control_char of char

    val of_string : string -> (t, err) result

    val to_string : t -> string
  end

  type t

  val v : Name.t -> Value.t -> t

  val name : t -> string

  val value : t -> string
end

(** {1 Open-redirect allowlist} *)

module Redirect : sig
  (** Off-site redirect targets (open-redirect row, completing the partial
      mitigation). In-app targets stay [Tea_core.Prim.Url] (relative-only, the
      live guard at the redirect sink); this type is the only representation of
      an off-site target and cannot name a host outside the operator's
      allowlist. Matching is anchored origin comparison, not URL parsing: a
      candidate is accepted iff it equals a listed origin exactly or begins with
      [origin ^ "/"], which kills the suffix trick
      ([https://docs.example.com.evil.com]) and the userinfo trick
      ([https://docs.example.com@evil.com]) for free, since ['.'] and ['@'] both
      fail the anchored ['/'] comparison. Boundary-only this milestone: no
      external-redirect feature exists to wire yet. *)
  module Allowlist : sig
    type t

    (** Trusted operator literals: full origins ([scheme://host[:port]], no
        trailing slash, no path), for example ["https://docs.example.com"]. *)
    val v : string list -> t
  end

  type t

  type err =
    | Empty
    | Control_char of char
    | Not_allowlisted

  val external_of_string : Allowlist.t -> string -> (t, err) result

  val to_string : t -> string
end

(** {1 Content-Security-Policy and clickjacking} *)

module Security_headers : sig
  (** Clickjacking and CSP row. Policies are finite sums; ['unsafe-inline'],
      ['unsafe-eval'], wildcard sources, and allow-all framing have no
      constructors, so a server cannot be configured into them. Loosening a
      policy means adding a constructor here, a reviewed type change, never a
      string. {!to_headers} serializes the sums to fixed compiled-in strings, so
      its output is control-byte-free by construction (and re-passes
      {!Header.Value}'s invariant, checked in tests). The legacy
      [X-Frame-Options] header is derived from the same {!Frame.t} as
      [frame-ancestors] by exhaustive match, so the two cannot drift. Honest
      limit: pre-CSP3 engines did not match a same-origin [ws://] under ['self'];
      no wildcard escape hatch is provided to compensate. *)
  module Frame : sig
    type t =
      | Deny
      | Same_origin
  end

  module Source : sig
    type t =
      | None_
      | Self
  end

  type t

  (** [frame:Deny] with every source ['self']: covers the SSR form page, the
      [/app] bundle, and the same-origin WebSocket, plus the always-on
      hardening {!to_headers} appends (object-src ['none'], base-uri ['none'],
      [X-Content-Type-Options: nosniff]). The middleware default. *)
  val strict : t

  val v
    :  frame:Frame.t
    -> default_src:Source.t
    -> script_src:Source.t
    -> style_src:Source.t
    -> img_src:Source.t
    -> connect_src:Source.t
    -> form_action:Source.t
    -> t

  val to_headers : t -> Header.t list
end
