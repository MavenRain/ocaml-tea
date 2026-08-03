(** The durable pack-backed Dream tier (roadmap step 8, D2): the mem tier's
    handler bodies over {!Tea_persist_pack.Store_pack}, plus an Origin-gated
    [POST /admin/checkpoint]. Kept in its own library so irmin-pack stays out
    of [tea_server]'s (and the js_of_ocaml client's) dependency closure. *)

module Root = Tea_persist_pack.Store_pack.Root
module Guard_file = Guard_file

(** The exactly-once RPC channel's server state (roadmap step 15, D20),
    re-exported so an app naming the [?rpc_once] builder's argument does not
    have to reach past this library into {!Tea_server}. *)
module Rpc_once = Tea_server.Rpc_once

type guards =
  { ws : Tea_server.Durable_guard.t
  ; ws_journal : Guard_file.t option
  ; rpc : Tea_server.Durable_guard.t
  ; rpc_journal : Guard_file.t option
  }
(** Both delivery channels' guards (roadmap step 15, D20.3). A journal that
    failed to open is [None] beside a null-sink guard, never an absent
    channel: that channel serves at-least-once rather than not at all, so a
    caller must close whichever journals are [Some] and must not read [None]
    as "nothing to do". *)

val open_guards :
  guard_dir:string ->
  head_water:(Tea_core.Crdt.Replica.t -> Tea_core.Prim.Store_water.t option) ->
  guards
(** Open the websocket channel's journal at [<guard_dir>/journal] and the keyed
    RPC channel's at [<guard_dir>/rpc/journal], filtering both against
    [head_water] (R20).

    What {!serve_pack} calls, exposed because the pair is a unit and the
    relation between its halves is the part that can silently break. The two
    channels must land in DIFFERENT directories, or one channel's floors
    adjudicate the other's replays; the websocket journal must be opened FIRST,
    because [Guard_file.open_] creates its own directory but never the parent,
    so an rpc-first boot dies ENOENT on a fresh root; and both must be filtered
    against the SAME [head_water] snapshot, taken once between the store open
    and this call, or the two boot filters can straddle a write and judge their
    floors against different heads. None of that is observable from inside a
    blocking [serve_pack], which is why it is a named function with a return
    type rather than two call sites.

    [head_water] comes from {!Guard_file.head_water_of_list} over one
    [Store_core.CORE.branch_waters] read. Caller closes every [Some] journal at
    teardown, after the repo. *)

module Make_pack (A : Tea_core.App.APP) : sig
  module Store : module type of Tea_persist_pack.Store_pack.Make (A)

  val handle_checkpoint :
    ?retention:Store.Retention.t -> Store.t -> Dream.request -> Dream.response Lwt.t
  (** [POST /admin/checkpoint] handler: same-origin only (a
      {!Tea_safe.Origin_gate.denial} answers 403), otherwise squash [main],
      link the checkpoint onto the retention spine, and GC to the anchor. *)

  val handler_pack :
    ?client_dir:string ->
    ?rpc:Dream.route list ->
    ?coalesce:A.msg Tea_core.Coalesce_spec.t ->
    ?retention:Store.Retention.t ->
    ?guard:Tea_server.Durable_guard.t ->
    ?sessions:Tea_server.Session_secret.t ->
    Store.t ->
    Dream.handler
  (** The mem tier's request pipeline with [POST /admin/checkpoint] folded into
      the RPC route list. Exposed so tests can drive it with [Dream.test].
      [?guard] is the journal-backed replay guard {!serve_pack} builds
      (roadmap step 11, D16); the default is the mem tier's null-sink guard.
      [?sessions] chooses where a browser's identity is kept (roadmap step 12,
      D17); the default is the mem tier's per-process
      {!Tea_server.Session_secret.memory}; {!serve_pack} is the entry point
      that defaults to a durable identity, this handler on its own does not.

      A future reap invocation from this tier {b must} pass
      [~forget:(fun sid -> Durable_guard.forget guard
      ~replica:(Tea_core.Crdt.Replica.v sid) |> Lwt.map ignore)] — see
      [Tea_persist.Store_core.CORE.reap]'s precondition. *)

  val serve_pack :
    ?interface:string ->
    ?port:int ->
    ?client_dir:string ->
    ?rpc:Dream.route list ->
    ?rpc_once:(Store.t -> Rpc_once.t -> Dream.route list) ->
    ?coalesce:A.msg Tea_core.Coalesce_spec.t ->
    ?retention:Store.Retention.t ->
    ?lower_root:string ->
    ?sessions:Tea_server.Session_secret.t ->
    root:Root.t ->
    unit ->
    unit
  (** Blocking entry point: open (or initialise) the pack store at [root] and
      the guard journals at [<root>.guard/journal] and [<root>.guard/rpc/journal]
      (a {i sibling} of the pack root, so irmin-pack GC and migration never meet
      a foreign file; a failed journal open is one stderr line and a null-sink
      guard, never an abort), serve until SIGINT/SIGTERM, then close the
      journals and the repo — that teardown order and the graceful-signal
      boundary are what make the delivery guarantee "exactly-once across an
      {i orderly} restart, at-least-once otherwise" (D16).

      [?rpc_once] mounts the keyed RPC channel (roadmap step 15, D20). It is a
      route {i builder} rather than a route list because both values it is
      applied to are made by this function and cannot be composed in advance:
      the channel wraps a guard over a journal opened and closed here, and the
      store is the pack root this function opened, which no app could reopen
      beside a live server. An app writes
      [~rpc_once:(fun repo o -> Rpc.routes_once o (my_handler repo))] and gets
      the durable channel, or omits it and gets today's unkeyed [Mutating]
      semantics. The channel's floors ride their own journal at
      [<root>.guard/rpc], with their own bounds, cap and boot filter (D20.3), so
      a step-14 root opens here with that journal simply absent and a step-14
      binary reading a step-15 root never looks inside the subdirectory. The
      returned routes are appended to [?rpc].

      When [?sessions] is omitted, identity is made as durable as the model
      (roadmap step 12, D17): the secret is resolved via
      {!Tea_server.Session_secret.resolve} with [<root>.secret] as the fallback
      file (another durability sibling of the pack root), so the session id,
      and with it the branch name and replica id, survives a restart and the
      D16 journal is actually consulted. Resolution failure is one stderr line
      and a per-process {!Tea_server.Session_secret.memory} back end: the
      degradation direction is fresh-identity, never an abort. The resolved
      back end is announced on stderr either way.

      Both siblings are named off [root] with any trailing separator
      normalised away, so [TEA_ROOT=/data/store/] cannot put them {i inside}
      the directory irmin-pack owns.

      This function does {b not} always return: two conditions refuse to serve
      and [exit 1] without binding a port, because returning [unit] would end
      the binary at status 0 and no supervisor could tell a refusal from a
      clean shutdown.
      - The root is unusable (a file, an absent parent, an existing directory
        that is not a pack store). One stderr line naming the root and the
        remedy, via {!Tea_persist_pack.Store_pack.explain}. Nothing is created
        beside a root that was refused.
      - The root does not exist but [<root>.guard] does, i.e. the pack store
        was wiped or restored out of step with its journal. Serving would let
        surviving floors judge returning clients' replays [Duplicate] against
        branches that no longer exist, which is silent loss (DESIGN R20).

      [Dream.cookie_sessions] marks the session cookie [Secure] only when
      Dream itself terminates TLS. Behind a TLS-terminating proxy Dream sees
      plain http, so a durable session cookie is emitted WITHOUT [Secure] and
      without the [__Host-] prefix, and any plaintext request to the same
      host discloses it - which now matters more than it did, because an
      intercepted cookie no longer dies at the next restart. Terminate TLS in
      Dream, or make the proxy refuse http: the framework cannot tell the two
      deployments apart from inside the handler. *)
end
