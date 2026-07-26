# ocaml-tea - an OCaml + Irmin full-stack TEA framework

*Working name.* A port of the ideas in [lean-tea](https://github.com/Verilean/lean-tea)
(a pure-Lean 4 full-stack framework built on **The Elm Architecture**) to **OCaml**,
replacing lean-tea's vendored **SQLite** with **[Irmin](https://irmin.org)** - a
Git-like versioned, branchable, mergeable store.

The swap is not cosmetic. Irmin changes the persistence *model*, and OCaml's
toolchain removes an entire tier lean-tea had to hand-write.

---

## 1. Status (what is real vs. designed)

| Layer | State |
|---|---|
| `lib/tea_core` - APP signature, `Cmd`/`Sub`, `Html` + SSR, `Merge_spec`, `Merge` combinators, `Codec`, `Loop`, `Wire` | **built, green** (`repr` only) |
| `examples/shared_doc` - collaborative doc app (real `Three_way` merge) + server/client | **built, green** |
| `test/merge_test`, `test/collab_test` - merge laws + two-session reconcile | **passes** (confirmed by mutation) |
| `examples/counter` - shared Counter app | **built, green** |
| `lib/tea_persist` - Irmin versioned model store | **built, green; test passes** |
| `test/persist_test` - apply/history/undo end-to-end | **passes** |
| `lib/tea_server` - Dream wiring (SSR + form-post + `/ws` live view) | **built, green** (`live_session` pump over a testable transport seam) |
| `test/server_test` - Dream tier end-to-end (sessions, CSRF, undo, live pump) | **passes** (confirmed by mutation) |
| `examples/counter/server` - native Counter server binary (serves `/app` bundle) | **built** |
| `lib/tea_client` - pure `Html.t → vdom` / `Cmd.t → Vdom.Cmd.t` translations + `Subs` planning | **built, green** (`tea_core` + `vdom.base` only, links natively) |
| `lib/tea_client_run` - jsoo runtime: mount, `After`/`Navigate`, popstate, `Sub` interpreter (`Every` ⇒ `setInterval`, `Store_watch` ⇒ WebSocket) | **built** |
| `test/client_test` - translation fidelity via native decoder evaluation | **passes** (confirmed by mutation) |
| `examples/counter/client` - Counter in the browser (js_of_ocaml) | **built** (`main.bc.js` + `index.html`) |
| `lib/tea_rpc` - GADT endpoint contract + `Tea_server.Rpc` dispatch + `Cmd.Http`/XHR wire | **built, green** (roadmap step 7, §8) |
| `lib/tea_safe` - the 9 security primitives as `.mli` boundaries | **built, green** (roadmap step 5, R7) |
| `lib/tea_persist/clock` + `store_core` - monotonic commit clock, backend-generic store, coalescing, checkpoint | **built, green** (roadmap step 6) |
| `lib/tea_persist_pack` - irmin-pack backend + GC behind a retained checkpoint | **built, green; `test/pack_test` passes** |
| `test/clock_test`, `test/dedup_test`, `test/coalesce_test`, `test/pack_test` - step-6 suite | **passes** (confirmed by mutation) |
| `lib/tea_client/reconnect` + `rebase` + `local_channel` - pure link state machine, outbox/rebase, client-local channel | **built, green** (roadmap step 8, D8/D9/D10-client) |
| `lib/tea_core/local` - the `LOCAL` companion contract + the empty companion | **built, green** (roadmap step 8, D10) |
| `test/client_reconnect_test`, `test/client_channel_test` - step-8 P6 suite | **passes** (confirmed by mutation) |
| `test/browser/smoke.mjs` - Playwright end-to-end smoke over the real compiled server binaries | **passes, 8 ok / 1 xfail** (confirmed by mutation; roadmap step 8, D13) |

Toolchain: OCaml 5.3.0, dune 3.24, a dedicated opam switch (`irmin-tea` locally) with
`irmin 3.11`, `dream 1.0.0~alpha8`, `repr 0.8`, `vdom 0.3`, `js_of_ocaml 6.4`.

> **Caveat on the risk register (§10):** the design workflow's adversarial
> *verify* pass did not run (weekly usage limit). The risks below are
> finder-reported and **not independently verified** - treat them as leads.

---

## 2. lean-tea → ocaml-tea mapping

| lean-tea | ocaml-tea | note |
|---|---|---|
| `WebApp {init,update,view,encode/decodeModel,decodeMsg}` | `module type APP` (`app.ml`) | `Repr.t` reprs replace the three hand-written codecs |
| Pure `update`, no effects | `update : msg -> model -> model * msg Cmd.t` | we **add** Elm-style managed effects (as data) |
| Model shipped in the `X-Model` header | Model lives in Irmin, keyed by session branch | **T1** - see §5 |
| SQLite `Entity`/`Repo`, typed SQL | Irmin tree + `Merge_spec` | no query language; indexes are tree paths (§5) |
| Hand-written LeanJs compiler | `js_of_ocaml` + `ocaml-vdom` | **T3** - the whole tier disappears |
| Servant-style `Endpoint` record + generated JS | GADT `('req,'resp) api` | **stronger** no-drift: both tiers type-check one definition |
| 9 private-constructor security primitives | OCaml abstract/private/phantom types | **T4** - §9 |
| Pure-Lean HTTP/1.1 + RFC6455 WS + OAuth | Dream | §6 |

## 3. The four theses

- **T1 - Irmin replaces SQLite *and* the `X-Model` header.** The server model is
  a per-session Irmin branch; each `update` is one commit; history gives
  undo/redo/time-travel/audit for free. **Proven** (`test/persist_test`).
- **T2 - branch-per-session + 3-way merge = built-in collaboration.** Concurrent
  sessions reconcile via Irmin's tree merge, dispatching to the app's
  `Merge_spec`. **Proven end-to-end** (`test/collab_test`): two sessions fork a
  shared doc, edit concurrently, and `Store.merge_into` sums a counter, unions a
  tag set, takes a one-sided title edit, and *surfaces* a divergent free-text
  edit as a labelled conflict. Merge quality is supplied by the `Merge`
  combinator library (§5), not guessed. Honest limit: reconciliation is only as
  good as the combinators an app composes; this is *not* a CRDT.
- **T3 - one Model/Msg/update source, compiled to native + JS.** The shared core
  depends only on `repr` (= `Irmin.Type`), so one wire codec serves both tiers
  with zero schema drift. **Core proven** (builds); client tier pending.
- **T4 - security primitives are module boundaries.** `private`/abstract/phantom
  types make illegal states uncompilable. **Realized** as `lib/tea_safe` (roadmap
  step 5, R7): the nine primitives are `.mli` boundaries, the `Proof` capability
  is a phantom token in a `repr`-free library (unserializable by construction),
  and the live sinks are wired so a WebSocket accepted without the same-origin
  check, or a raw store path, does not typecheck. Every boundary value is a
  validating newtype, `on*` and off-charset attribute names are rejected, and the
  new rejections are confirmed by mutation.

## 4. Module layout

```
lib/tea_core/     (deps: repr)          - pure, IO-agnostic, compiles to native AND js
  prim.mli/ml       newtypes: Title, Url, Tag, Attr_name(!on*), Fuel, Session_id, Branch_name, ...
  cmd.mli/ml        private effect-as-data variant + smart ctors + map
  sub.mli/ml        private subscription variant (timers; Store_watch = §7)
  html.mli/ml       private view type; safety hooks in constructors
  render_static     Html.t -> escaped HTML string (server SSR), exhaustive fold
  merge_spec        pure 3-way merge policy (LWW | Three_way f)
  app.ml            module type APP (the WebApp mirror)
  codec.ml          Codec(APP): Repr JSON + commit-message labels
  loop.ml           IO signature + Loop(IO)(APP): fuel-bounded Cmd interpreter
lib/tea_persist/  (deps: tea_core irmin irmin.mem irmin.unix lwt)
  store.ml          Store(APP): session branches, apply=commit, history, undo, merge_into
lib/tea_server/   (deps: tea_core tea_persist dream lwt lwt.unix)
  tea_server.ml     Make(APP): Dream session -> hex Session_id -> branch; Loop over Lwt;
                    formify: On_click sites -> CSRF-protected <form> posts of the Repr-JSON Msg
examples/counter/ Counter_app.App (shared) + server/main.ml (Dream) + [pending] client/
test/             persist_test, server_test (Dream.test, no socket)
```

Design conventions honored throughout: exhaustive matches (no `_ ->` on finite
sums), combinators over loops, newtypes over primitives, safety encoded in `.mli`
boundaries, `result`/`option` over exceptions in the public surface.

## 5. Persistence (the crux)

**Layout.** Today the model is a single Contents blob at `["model"]` on a branch
named `session-<id>` (`main` for the shared/default branch). The commit *message*
is the serialized `Msg` (`Codec.msg_to_label`), so `git log` on the store reads as
the event log. `apply` = load → `update` → `set_exn` (one commit).

**Undo / time-travel.** `undo` walks `S.Commit.parents` and `S.Head.set`s to the
parent - no bespoke undo stack. `history` returns the first-parent chain.

**Merge (T2).** `Merge_spec` lifts into `Contents.merge`:
`Last_write_wins → Irmin.Merge.(option (default model_t))`;
`Three_way f → Irmin.Merge.(option (v model_t merge_f))` where `merge_f` forces
the ancestor promise and maps conflicts to `` `Conflict ``. Irmin only calls it
when both branches touched the same path. An app supplies `f` by composing the
`Merge` combinator library (`atomic`/`counter`/`set`/`record`, with `conflict`
as the never-guess default); `examples/shared_doc` is the worked example, and
`Store.fork` gives a collaborator a session branch that shares a single, clean
common ancestor with the source.

> **Subtlety (surfaced by `test/collab_test`, and R1-adjacent).** Irmin commits
> are content-addressed, and `Info` stamps them at one-second resolution. Two
> branches that make the *identical* edit (same resulting tree, same parent,
> same second) therefore collapse to *one* commit, which can move a later merge
> base off the true root - e.g. two counters that both `+1` as their first
> concurrent act dedup, and the merge counts that shared step once, not twice.
> This is consistent (the shared commit genuinely *is* shared) but surprising.
> **Fixed in roadmap step 6:** every store handle mints commit dates from a
> monotonic clock (`Tea_persist.Clock`, `max(wall, last+1)`, seeded from the
> branch heads on open), so intended-distinct edits stay distinct;
> `test/dedup_test` pins the regression, including the merge counting both
> sibling deltas.
>
> *Known edge (latent-LOW, documented not fixed).* The reopen seed draws from
> `S.Repo.heads` only, so a commit orphaned by `undo` (still resident, dated
> above every head) is not counted. A reopen within that commit's wall-clock
> second could re-mint its date — but this fools no live path: a redo landing
> back on the exact undone commit is the correct outcome, and within one
> session all sessions share the single clock. A full all-commits seed scan
> would defeat the R1 scale goal, so this waits for a feature that needs
> date-uniqueness across all live objects rather than across heads.

**Refinements:**
- *Exploded tree* (not yet built) - one path per model field / entity / index
  row, so merges are per-field and "queries" become index-tree lookups
  (lean-tea's `Entity`/`Repo` patterns → maintained secondary-index subtrees,
  since Irmin has no SQL).
- *Backends* - **shipped** (step 6): the store body is backend-generic
  (`Store_core.Make (A) (S)`); `Tea_persist.Store` is the `irmin.mem` shim,
  `Tea_persist_pack.Store_pack` the durable `irmin-pack` one (scale + GC,
  always `Indexing_strategy.minimal` so delete-GC stays allowed). `irmin-git`
  remains a functor argument away.

  > **The pack contents-framing contract.** A pack entry is
  > `[hash][kind][the contents codec's own bytes]`; nothing in irmin-pack writes
  > a length of its own, so with `contents_length_header = `Varint` the reader
  > recovers the entry length by decoding a varint at the head of *our* bytes.
  > A `Repr.record`'s leading varint frames only its **first field**, so any
  > multi-field model was handed a truncated buffer on reopen and died in
  > `decode_bin` (`Invalid_argument "index out of bounds"`); a single-field model
  > (the Counter) satisfies the contract by accident, which is why every
  > Counter-driven pack test passed while the D1 CvRDT doc could not survive one
  > close/reopen. `Store_pack.framed` wraps the whole model as one
  > `Repr.string`, restoring the invariant for any app model — so framing is the
  > storage layer's job, and CRDT states stay structural `Repr` values (a
  > readable JSON wire form). Pinned by `test/pack_crdt_test.ml`, whole-model and
  > exploded. Setting the header to `None` is not the alternative: with minimal
  > indexing the length would then be unrecoverable (reads fail as
  > `dangling hash`).
- *History-growth control* - **shipped** (step 6, R1): `Coalesce_spec`-driven
  commit coalescing (amend-with-ownership-guard, no timers — see §6 for the
  per-socket wiring), `checkpoint` squash-to-root, and `Store_pack.gc
  ~retain:checkpoint`. The retention knob is the checkpoint argument itself.

## 6. Server (Dream) - designed

`Tea_server.Make (A : APP)` produces a Dream app. Per request: resolve the
Dream session → Irmin branch → `Store.load` → `A.update` (via `Loop` with an `fx`
record whose handlers close over the branch) → commit → `Render_static.to_string
(A.view model')`. Live view: `S.watch` on the session branch fanned out over
`Dream.websocket`. Dream supplies HTTP/1.1(+TLS), RFC6455 WS, sessions, CSRF, and
typed forms - deleting lean-tea's hand-written HTTP/WS/OAuth stack. The `X-Model`
header is gone: state is the branch head.

**Status: built** for the SSR + form-post path (`Tea_server.Make`). The server
rewrites every `On_click` site of the shared view into a same-origin `<form>`
carrying the Repr-JSON Msg and Dream's CSRF token, so any APP is fully usable
with zero client JS (progressive enhancement: the client tier later re-attaches
live handlers to the same view). A `Navigate` Cmd becomes the post-update
redirect target; undo is served at `/undo`.

**Live view: built** (roadmap step 3). `Wire.ws_path` (`/ws`) accepts Msg
frames up, drives them through the same `step` (Loop + commit), and answers
every commit on the session branch - from this socket, a form post, or another
tab - with a full Repr-JSON model frame, produced by `Store.watch` (Irmin
`S.watch` anchored at the current head, reading the model *at the notified
commit*). All down-frames funnel through one `Lwt_stream` writer, so sends
never interleave; the pump is written against a `live_transport` record
(send/receive functions), the same seam discipline as `Loop`'s IO, so
`server_test` drives it with in-memory queues and no handshake. Protocol
errors are fail-stop: an undecodable frame or exhausted fuel closes the
socket. Because `Dream.origin_referrer_check` exempts GET and a WS handshake
is a GET, `/ws` carries its own same-origin gate (`Origin` must name `Host`;
CSWSH defense). A `Navigate` effect has no WS surface (dropped server-side;
the client's optimistic run of the same Msg performs it). Known residuals:
frames for commits landing during watch registration can transiently reorder
around the initial announcement (converges - later commits fire later
callbacks); an optional `?client_dir` serves the compiled jsoo bundle at
`/app` so SSR page, live client, and socket share one origin and one Dream
session. Two documented invariants: an element keeps at most one
`On_click` (first wins, enforced by `Html.elt`, so both tiers agree), and click
handlers belong on leaf controls (an `On_click` ancestor of another `On_click`
would render as nested forms, which is invalid HTML; id-based form association
arrives with the client tier).

## 7. Client (vdom + js_of_ocaml) - designed

`Tea_client.Make (A : APP)` links the *same* library as the server and renders with
`ocaml-vdom` (a faithful Elm-architecture impl): `Vdom.app ~init ~update ~view`,
mounted via `Vdom_blit`. `Html.t → 'msg Vdom.vdom` and `Cmd.t → 'msg Vdom.Cmd.t`
are total translations. Transport is a Dream WebSocket carrying Repr-JSON `Msg`
frames up and committed-head announcements down; the client runs `update`
optimistically and rebases pending msgs on each server head (browser mirror of
Irmin branch semantics). **Lwt is banned client-side** - vdom's callback-based Cmd
handlers sidestep the known `js_of_ocaml-lwt` rough edges.

**Status: built** for the local MVP (roadmap step 2). The tier is split along
the native/JS boundary: `tea_client` holds the total translations plus `Make`
and depends only on `tea_core` + `vdom.base`, so `test/client_test` proves
translation fidelity *natively* - `On_click` deliberately translates to a
`Decoder.const`-shaped handler, evaluable off the browser, so the exact Msg is
recovered in tests without a DOM. `tea_client_run` (byte/js-only) interprets
the two command extensions (`After` ⇒ `setTimeout`, `Navigate` ⇒
`history.pushState`), sets the title, mounts onto `document.body`, and
dispatches `msg_of_url` at load and on `popstate`. One deliberate asymmetry:
`value` crosses as the DOM *property* (a controlled input must track the
model on redraw); every other attribute crosses verbatim, as `Render_static`
prints it. `After` timers are fire-and-forget (one mount per page life, no
dispose path; a future dispose must track and clear them). Lwt-free holds:
neither client library links `lwt`.

**Subscriptions: built** (roadmap step 3). The pure half lives in
`Tea_client.Subs` (natively tested): flatten the `Sub` tree into specs, key
them by runtime resource (`Key_every ms` / `Key_store` - callbacks are
deliberately not part of the key), and `plan` the wanted-vs-active diff. The
effectful half lives in `Start`: after mount and after every `update` the
subscriptions of the new model are re-planned - `Every` runs on
`setInterval`, `Store_watch` opens the `Wire.ws_path` WebSocket. Handlers are
looked up from the *current* model's subscriptions at fire time
(`Vdom_blit.get`), so resources never hold stale callbacks. When the socket
is open, every locally-born msg is mirrored up it (the optimistic path:
local `update` applies immediately, the server commits the same Msg, and the
committed head returns as a `Store_watch` frame); remote-born msgs are fenced
by an `applying_remote` flag - sound because `Vdom_blit.process` runs
`update` synchronously - so a pushed head can never echo back up the socket.
Timer- and `After`-born msgs are mirrored like any other, so a chatty
`Sub.every` app commits on every tick (R1 - coalescing is roadmap step 6).

**Reconnect, rebase and the local channel: built** (roadmap step 8,
D8/D9/D10-client). The three standing residuals of the paragraph above are
closed, each by a pure module in `tea_client` that the js_of_ocaml runtime
merely performs:

- **D8, `Reconnect`.** `socket : WS.t option` became a four-state machine
  `Down | Opening | Up | Waiting`, over *abstract* socket and timer types so
  it links natively. A close reopens on an exponential ladder (500ms,
  doubling, capped at 30s) carried in the `Opening`/`Waiting` states - it has
  to ride `Opening` too, or the escalation is lost at exactly the
  `Waiting -> Opening` step and a server that refuses every connection is
  retried at 500ms forever. Reaching `Up` resets the ladder, because that is
  the evidence the server is reachable. Every close is matched by **physical**
  equality against the socket the machine holds: a superseded socket's close
  event arrives *after* its replacement is already opening, and acting on it
  would tear the healthy socket down and arm a second timer. `Waiting` counts
  as active for `Subs.plan`, or a merely-pending reconnect would be torn down
  and rebuilt on the next update.
- **D9, `Rebase`.** A msg born while the link is not `Up` now waits in an
  outbox and is replayed in order on reconnect, instead of applying locally
  and being logged as lost. `Opening` deliberately does not count as sendable:
  a `send` on a CONNECTING socket raises. Replay is send-once by construction
  (a buffered msg is one the server never received) and idempotent anyway
  under D1's joins. On the way in, a pushed head is `reconcile`d against the
  model this tab holds, under the app's own `Merge_spec.t`, before the
  `Store_watch` subscription ever sees it - the head does not contain the
  outbox's edits, so handing it over raw was a clobber.
- **D10 (client half), `Tea_core.Local` + `Local_channel`.** Per-client state
  (an RPC reply, a UI toggle) gets a home that is not the replicated model. A
  companion answers `Some` to claim a msg - local half only, and **not**
  mirrored up the socket - or `None` to decline it to `App.update`. The
  dispatch runs through `Result.fold` rather than `Option.fold` precisely
  because the latter's `~none:` is a *value*: evaluating the declined branch
  anyway would mint a CRDT dot for an edit the app never made, which is
  invisible in the returned state and is therefore pinned by its own mutation.
  `Start (A) = Start_local (A) (Local_none (A))`, so there is one mount path
  rather than two that can drift.

The server head remains the authority (R6) wherever the app's merge policy has
no way to keep both sides: `reconcile` under `Last_write_wins` is the incoming
head unchanged, and a `Three_way` conflict yields to it. Under `Crdt_join`
nothing is lost.

- **D13, `test/browser/smoke.mjs`.** The browser smoke test (roadmap step 8,
  P7), driving the real compiled server binaries with a real Chromium. It is
  the only test that observes the tiers being *connected* rather than merely
  correct: the jsoo bundle mounting, the socket dialling, a click reaching the
  store, a commit streaming back into a second tab, an XHR decoding a typed
  reply, and a browser attaching the `Origin` header D12 demands. Deliberately
  outside `dune runtest` (node + a ~150MB Chromium + two ports would make the
  OCaml suite fail on any machine with only the OCaml toolchain); run it with
  `test/browser/run.sh`. Its assertion primitive samples the observable
  *before* the action and refuses to pass if the wanted condition already held,
  because a browser test is the easiest kind to write vacuously; each check is
  confirmed by `test/browser/mutate.py`.

> **D14 - the acting tab double-counts its own PN-counter dot. Found by D13 on
> its first run; open.** A locally-born msg is applied twice under two
> *different* CRDT replica ids: once optimistically on the client, whose `ctx`
> mints under the constant id `"client"` (`tea_client.ml`), and once on the
> server, which applies the very same forwarded msg under the session-branch
> id. `Rebase.reconcile` then *joins* the two states, and a `Pn_counter` join
> sums across replica slots. Characterized exactly: displayed = server truth +
> that tab's own local dots, so an acting tab reads 2x an observer tab (1/2,
> 2/4, 3/6 confirmed), and a reload - which discards the local dots - snaps it
> back to the truth.
>
> The root cause is a category error, not a merge bug: **the client is not a
> replica, it is a predictor of the server's replica.** Its wall source is
> already `0` precisely so it can never win an LWW tie-break (§7, R6), which is
> the same admission made halfway. `Lww` and `Or_set` survive the double-apply
> (higher stamp wins; the set dedups by element), so `Pn_counter` - the one
> CRDT whose op is not idempotent under re-application - is where it surfaces.
> Note this also falsifies the safety claim in `rebase.mli`'s outbox doc, that
> a re-delivered edit "joins idempotently rather than counting twice": state
> join is idempotent, but *replaying an op* at a second replica is not. Today
> that claim is carried by send-once actually holding, not by the CRDTs.
>
> Three candidate fixes, in the order they were considered:
> 1. **Share the replica id.** The server tells the client the session-branch
>    replica id, and the client mints under it - so the optimistic state is a
>    prediction of the *same* slot and `join` becomes a per-slot `max`: no
>    double count, and an unconfirmed local edit still survives (the client's
>    slot is simply ahead). Principled, and the only option that keeps both
>    properties. Costs a wire addition and a boot-order decision (what id
>    edits made before the announcement use), which is why it is a phase and
>    not a patch.
> 2. **Drop local dots on an incoming head** (`reconcile` = `incoming` under
>    `Crdt_join`). One line, but it re-opens exactly the clobber D9 exists to
>    close: an in-flight edit flickers away until its own echo returns.
> 3. **Don't apply forwarded msgs optimistically.** Correct, and it throws away
>    the optimistic UI.

## 8. Shared RPC contract - built (roadmap step 7; hardened step 8, D11/D12)

lean-tea kept its tiers aligned by convention: route strings and encode/decode
pairs matched only because a human kept them matching. ocaml-tea makes each
alignment a compile error. An endpoint is a constructor of a two-parameter
GADT `('req,'resp) t` carrying its own `Repr` codecs (`req_t`/`resp_t` are
total, wildcard-free matches), and both tiers link the one compilation unit
that defines it (a `Tea_rpc.API`; here
`examples/shared_doc/shared_doc_rpc.ml`). Therefore: (1) adding an endpoint
without a server handler, a client Cmd translation, or a codec projection is a
non-exhaustive-match error, not a 404 discovered at runtime; (2) calling an
endpoint with the wrong request type, or reading its response at the wrong
type, fails to unify with the constructor's indices at both tiers; (3) the
tiers cannot disagree on the wire codec because each side projects it from the
same constructor of the same module - there is no second definition to drift.
The single remaining stringly seam is the wire name, confined to one `name`
witness: its totality is compiler-enforced, `of_name` is derived from `all`,
the server's routes are generated by folding over `all` (no third copy of a
name exists anywhere), and the literal wire paths are pinned by test.

**Wire format.** Raw Repr-JSON request body POSTed to `/rpc/<name>`
(`Content-Type: application/json`), raw Repr-JSON response on 200, no
envelope. The HTTP status is exclusively the transport-error channel (404
route-miss, 415 content-type gate, 413 size cap at 64 KiB post-read, 400
decode refusal); app-level fallibility is declared as `'resp = ('ok,'err)
result` inside the GADT and rides the 200 channel through `resp_t`.

**Plumbing.** `Tea_core.Cmd` gained the documented `Http` constructor
(`{path : Prim.Rpc_path.t; body; expect : (string, http_failure) result ->
'msg}`); `Tea_rpc.Make(A).call` is the typed client builder over it. The
Io-generic server `Loop` resolves every `Http` as `expect (Error
No_transport)` fed back through `update`, fuel-bounded - fail closed INTO the
app; the documented migration path is swapping that arm for an `fx.http`
field (compile-forced, surface-preserving) if server-side dispatch is ever
wanted. Apps must not synchronously retry `Transport No_transport` in
`update` (fuel exhaustion is the only exit). The browser runtime interprets
the lowered `Tea_client.Http` extension over `Js_browser.XHR`; `Prim.Status`
classifies the outcome (XHR status 0 = `Network_error`).

**Security posture.** The content-type gate makes a cross-site RPC POST
non-simple (and no CORS headers are served); strict CSP + nosniff ride every
rpc response via `secure_headers`, including router 404s.

**Anti-CSRF gate (D12, step 8).** The hard gate is now *shipped*, so
store-mutating endpoints are admitted. `Tea_rpc` carries
`endpoint_kind = Read_only | Mutating` and `API` a total, wildcard-free
`kind` witness beside the codec witnesses, so no endpoint can reach the router
unclassified. `Rpc.route` matches it: a `Mutating` endpoint dispatches only
behind the `same_origin` proof `Origin_gate.check` mints - the `accept_ws`
discipline, one function that *demands* the proof - and answers 403 on every
`denial` arm. The gate runs **first**, ahead of the content-type and body
checks, so a forged cross-site POST is refused without its body being read
(observable: cross-origin + a refused content type is 403, where the same
request same-origin is 415). `shared_doc` ships the real thing:
`Append_tag : (string, int) t`, `Mutating`, which drives one `Add_tag` Msg
through `Server.step` on the canonical branch, so an accepted RPC mutation is an
ordinary labelled commit through the same `App.update` every other path runs -
not a second write path with its own semantics. Two limits, because the obvious
readings of that sentence are both wrong: it does **not** reach live peers (a
live session watches its *per-cookie* session branch, and the canonical branch
is not that branch - cross-branch propagation is the still-deferred R3 auto-merge
story), and `step` is load-step-commit with **no** compare-and-set, which R10
below covers.
`/msg` form traffic keeps using Dream's own token. Residual, by construction:
`Read_only` is a *declaration*, not an enforcement - a `Read_only` handler that
writes the store re-opens the hole, so classification is a review obligation.
The negative case is not browser-testable (Chromium sets `Origin` itself and
will not forge one), so `test/csrf_test` carries it over `Dream.test`, pairing
every refusal with a read of the branch: refused means *nothing was written*.

**Streaming body cap (D11, step 8).** The 64 KiB cap is enforced *while* the
body streams (`Body.read_capped` over `Dream.body_stream`, aborting the instant
the accumulated length exceeds the cap), so it now bounds peak memory per
request - cap plus one transport chunk - and not merely decode work. Within the
cap the result is byte-identical to `Dream.body`, which is what keeps the
earlier assertions true. The chunk source is a seam (the `live_transport`
discipline): `test/rpc_stream_test` drives it with a *counting* source and
asserts on bytes **pulled**, because a status code cannot distinguish "refused"
from "refused without reading it all" - the post-read cap this replaced passes
every status check.

(Server-read-only is distinct from what a *client* does with the reply: the
`shared_doc` example folds its `Doc_stats` reply into a `model_t` field, so
because that model is replicated the reply becomes a shared, on-demand badge
committed and broadcast like any edit. A genuinely per-client reply would need
a client-local state channel this single-replicated-model framework does not
yet have - **deferred**.)

**Totality ledger.** Compile-checked: the four witnesses (`name`, `req_t`,
`resp_t`, `kind`), the rank-2 server handler, `cmd_to_vdom`, the `Loop` arm,
and cross-tier payload/codec agreement (all derived from the constructor).
Test-checked: `all` completeness (cover witness + length equation + server
reachability sweep), name uniqueness and literal-parse validity, literal
wire-path pins, per-endpoint codec roundtrips, and the per-constructor `kind`
table (`test/rpc_test`, plus the `server_test` / `client_test` transport checks
and the `rpc_stream_test` / `csrf_test` gate checks). The example's server tier
is a library (`shared_doc_serve`) precisely so the suite drives the handler the
binary serves - a store assertion against a second hand-written copy of the
handler would prove nothing.

## 9. Security primitives (T4) - mapping

Each lean-tea private-constructor primitive → an OCaml `.mli` boundary:

| Primitive | OCaml technique | status |
|---|---|---|
| `Proof c` capability | phantom-typed abstract token from a generative mint, in a library that links no `repr` so it is unserializable by construction | **`Tea_safe.Proof`** (live: `Origin_gate`) |
| SafeAttr / XSS | `Attr_name.of_string` rejects `on*` and now allowlists the name charset (names render unescaped); `Tag.of_string` guards the tag position; values escaped at render | **`Prim`/`Html`, hardened** |
| SafePath (traversal) | abstract type whose segments provably carry no `..`/`.`/NUL/backslash, none absolute | **`Tea_safe.Safe_path`** (boundary) |
| **SafeQuery → SafeKey** | Irmin has no SQL: abstract `Step` rejecting separator smuggling / namespace escape, nonempty key by construction | **`Tea_safe.Safe_key`** (wired into `Store`) |
| SafeCmd | program + argv newtypes, never a shell string (no `to_shell_string` exists) | **`Tea_safe.Safe_cmd`** (boundary) |
| Header injection | typed response-header builders, CRLF unrepresentable in a value | **`Tea_safe.Header`** |
| Open redirect | `Url.of_string` = relative-only and now CRLF/backslash-free (Location-clean) + `Redirect` anchored allowlist | **`Prim` hardened + `Tea_safe.Redirect`** |
| Clickjacking / CSP | finite sum policy builders (unsafe option is *not a constructor*), strict CSP on every response | **`Tea_safe.Security_headers`** (middleware) |

## 10. Risk register (finder-reported, **unverified** - verify pass did not run)

- **R1 (high) - commit-per-Msg volume / unbounded history.** Chatty UIs make
  millions of commits. **Mitigation shipped** (roadmap step 6):
  `Tea_core.Coalesce_spec` + the store's amend-based coalescer folds a chatty
  run into one commit (ownership-guarded, test-and-set, no debounce loss
  window); `checkpoint` squashes a session branch to a single root;
  `Tea_persist_pack.gc ~retain:checkpoint` discards everything older than and
  unreachable from the retained checkpoint. Residual footgun: GC under live
  pre-checkpoint sessions degrades their history walks to truncation/`None`
  (documented precondition, not enforced — enforcement belongs to the R3
  reaper story).
- **R2 (high) - 3-way merge of arbitrary models is unsound by default.** A naive
  last-writer-wins silently drops concurrent edits. **Mitigation shipped:**
  `lib/tea_core/merge` - `conflict` (never guess) and `atomic` (conflict when
  both sides move apart) as the safe defaults, plus structural `counter`, `set`,
  and `record` combinators that reconcile without guessing; the merge laws
  (reflexivity, identity, commutativity) are checked in `test/merge_test`. A
  QCheck framework is not in the switch, so those laws run over a seeded
  generator loop instead (same idea, zero new deps).
- **R3 (med) - branch/Lwt lifecycle leaks.** Abandoned session branches pin
  history. Mitigation: a reaper keyed to Dream session expiry; order undo writes
  so a crash strands a harmless redo pointer, never a lost head.
- **R4 (med) - Irmin watch latency on the git FS backend.** Live view may lag.
  Mitigation: `irmin-pack` or an in-process pub/sub over `S.watch`.
- **R5 (med) - Repr must behave identically under js_of_ocaml.** Shared-codec
  correctness (T3) depends on it. Mitigation: a native-vs-jsoo round-trip
  conformance test in CI.
- **R6 (med) - optimistic client replay can diverge from server merge.**
  **Mitigation shipped** (roadmap step 8, D9): an incoming head is no longer
  applied raw. `Tea_client.Rebase.reconcile` folds it into the model the tab is
  holding under the app's own `Merge_spec.t`, and edits made while the link was
  down wait in an outbox and replay in order once it is back. Under `Crdt_join`
  the fold is lossless *and* idempotent, so replaying a local edit onto a newer
  head converges however the two interleave. The residual is now honest and
  policy-shaped rather than runtime-shaped: under `Last_write_wins` there is no
  operation that could keep both sides, so the server head still wins outright,
  and a `Three_way` conflict yields to it rather than stranding the tab on a
  divergent local state.
- **R7 (med) - security boundary bypass via direct Dream/Irmin/Unix calls, and
  Proof tokens must never be serialized.** **Mitigation shipped** (`lib/tea_safe`,
  roadmap step 5): the nine primitives are enforced `.mli` boundaries. `Proof`
  cannot be serialized because `tea_safe` links no `repr`, so no `Repr.t` witness
  for the token is compilable in-library or derivable outside it. The live sinks
  are wired: `Origin_gate` mints the capability the WebSocket `accept_ws` demands
  (a socket accepted without the same-origin check is now uncompilable), `Store`
  speaks only `Safe_key`, and a `Security_headers` middleware puts a strict CSP on
  every response. The remaining direct-sink discipline (never call raw
  Dream/Irmin/Unix past a boundary) stays a review convention. **Extended to the
  RPC tier** (roadmap step 8, D12): `Rpc.route`'s mutating dispatch demands the
  same `Origin_gate` proof, so that path cannot be taken without one. Unlike
  `accept_ws` this is *not* an isolated sink, and the difference is worth being
  exact about: `accept_ws` is the module's only mention of `Dream.websocket`,
  whereas `Rpc.route`'s `dispatch` must stay in scope for the `Read_only` arm, so
  rewriting the `Mutating` arm to call it directly would compile. The gate is
  enforced by the total `kind` match plus `test/csrf_test`, and keeping the two
  arms distinct is a review obligation - see R8.
- **R10 (med) - `step` is a read-modify-write with no compare-and-set, and D12
  put different clients on one branch.** `Store.commit` is a plain `set` (last
  write wins), so `load` -> `Loop.step` -> `commit` can interleave: two writers
  each read the pre-edit model and the second erases the first. This was
  harmless while every write path was *per-session* (the only racer was the same
  user); the `Mutating` RPC endpoint is the first path where different clients
  read-modify-write the **same** branch, which is what promotes this from
  theoretical to a real exposure. Note `append_commit`'s test-and-set does not
  fix it either: the retry re-commits the *same stale model*, so it preserves
  history, not content. Mitigation shipped: `shared_doc_serve`'s handler holds
  an `Lwt_mutex` across the whole read-modify-write, correct within one process.
  **Not test-confirmed, and deliberately labelled as such** - under `Irmin_mem`
  with an `Add_tag` that emits `Cmd.none` there is no Lwt yield between load and
  commit, so deleting the lock leaves `test/csrf_test` green (verified by
  mutation). The real fix belongs in `store_core`: a compare-and-set retry
  around the whole step, which would also cover the pack backend and any Msg
  with a Cmd tail. Until then, an app mutating a shared branch from concurrent
  requests must serialize it itself.
- **R8 (low) - a `Read_only` RPC endpoint whose handler writes the store.** The
  `Tea_rpc.endpoint_kind` witness is what the server gates on, and it is a
  *declaration*: the type system forces every endpoint to carry one (no
  wildcard, so a new constructor cannot default to ungated) but cannot check
  that a `Read_only` handler is in fact read-only. Misclassifying a
  store-writing endpoint restores the CSRF exposure the gate removes.
  Mitigation: the per-constructor `kind` table pinned in `test/rpc_test` (so a
  silent reclassification fails a check rather than production), and
  classification as a review obligation on every new endpoint.
- **R9 (low) - what the D11 cap does and does not bound.** It bounds *this
  loop*: the buffer never holds more than 64 KiB (the chunk that would cross the
  cap is refused rather than added), and bytes pulled never exceed the cap plus
  that one chunk, whatever `Content-Length` claims. It is **not** a process
  memory budget, and three gaps are worth naming rather than rounding away.
  (i) `Buffer` doubles as it grows and reallocates by copying, so live bytes
  transiently exceed the length it is holding; `Buffer.contents` then copies the
  admitted body once more, so an admitted 64 KiB request touches ~2x64 KiB.
  (ii) A refused body is left undrained. That is deliberate - it is why the
  refusal is cheap - but what the transport does with a request nobody drained
  is Dream's business, and an undrained keep-alive connection may be *held* to a
  timeout rather than closed, so each refusal can cost a parked connection.
  (iii) How many such requests are in flight at once is Dream's connection
  limit, not this cap's job. `test/rpc_stream_test` pins bytes *pulled* as a
  number, so a rewrite that drains the stream before deciding (the pre-D11
  behaviour, which keeps every status code correct) fails a check - but note
  that check covers (i) not at all.

## 11. Roadmap

1. **Server MVP** - `tea_server` + Counter served over Dream (SSR + form-post
   update path), no WS yet. **Done** (`tea_server.ml`, `server_test`).
2. **Client MVP** - `tea_client` + `Html.t → vdom`, Counter in the browser off
   the same `Counter_app`. **Done** (`tea_client`, `tea_client_run`,
   `client_test`, `examples/counter/client`).
3. **Live view** - `Sub.Store_watch` ⇒ `S.watch` ⇒ WebSocket ⇒
   `Vdom_blit.process`. **Done** (`Wire`, `Store.watch`, `live_session` +
   same-origin gate, `Tea_client.Subs`, the `Start` sub interpreter, counter
   `Sync`); the rebase buffer and auto-reconnect it deferred landed in step 8
   (D8/D9, §7).
4. **Collaboration demo** - a two-session shared doc proving T2 with a real
   `Three_way` merge + the combinator library (R2). **Done** (`lib/tea_core/merge`,
   `Store.fork`, `examples/shared_doc`, `test/merge_test`, `test/collab_test`).
5. **`tea_safe`** - the 9 security primitives as enforced `.mli` boundaries (R7).
   **Done** (`lib/tea_safe`: `Proof` capability tokens + its `Origin_gate` live
   instance, `Safe_key`, `Safe_path`, `Safe_cmd`, `Header`, `Redirect`,
   `Security_headers`; in-place `Prim` hardening of `Attr_name`/`Tag`/`Url`/
   `Branch_name` closing the survey's attribute-name-injection, raw-tag, and
   Location-CRLF gaps; wired into `Store` and `Tea_server`; `test/safe_test`
   plus extended `server_test`, every new check confirmed by mutation).
6. **History hygiene** - coalescing + `irmin-pack` + GC retention (R1).
   **Done** (`Tea_persist.Clock` monotonic commit dates fixing the §5 dedup
   collapse; backend-generic `Store_core` with `checkpoint` squash and the
   ownership-guarded coalescer driven by `Tea_core.Coalesce_spec`;
   `Tea_persist_pack.Store_pack` with `gc ~retain:checkpoint`; `?coalesce`
   per-socket policy on `Tea_server.serve`; `clock_test`/`dedup_test`/
   `coalesce_test`/`pack_test`, every new check confirmed by mutation).
7. **RPC GADT** + a typed endpoint example. **Done** (`lib/tea_rpc`: `Name`,
   `API`, `Make` with derived paths/codecs and the typed `call` builder;
   `Tea_core.Cmd.Http` + `http_failure` and the `Loop` `No_transport` arm;
   `Tea_client.Http` lowering + the `tea_client_run` XHR interpreter;
   `Tea_server.Rpc` dispatch with the content-type gate, size cap, and
   transport-only statuses, threaded via `?rpc`; `Prim.Rpc_path` /
   `Prim.Status`; generic `Codec.to_json`/`of_json` + the new `codec.mli`;
   `examples/shared_doc` `History_count`/`Doc_stats`; `test/rpc_test` plus
   `server_test`/`client_test` additions, every new check confirmed by
   mutation).
8. **Deferral backlog** - the twelve deferrals recorded by steps 1-7, worked in
   phases. **P1-P5 done** (D1/D6/D10-shared CvRDT model + exploded tree;
   D7/D2-D5 clock hoist, pack-backed serving, session reaper, retention ring,
   archive GC; D11/D12 streaming body cap + Origin-gated mutating endpoints).
   **P6 done** - the client tier's three residuals: `Tea_client.Reconnect`
   (D8, four-state link + 500ms→30s backoff ladder + physical-equality stale
   guard), `Tea_client.Rebase` (D9, FIFO outbox replayed on reconnect + an
   incoming head reconciled under the app's own `Merge_spec.t`),
   `Tea_core.Local` + `Tea_client.Local_channel` (D10 client half, the
   per-client companion whose claimed msgs never cross the socket) with
   `Tea_client_run.Start_local` and `Start = Start_local (A) (Local_none (A))`;
   `shared_doc`'s stats readout moved into a `Local` companion;
   `test/client_reconnect_test` + `test/client_channel_test`, every new check
   confirmed by mutation. **P7 done** - D13, the browser smoke test
   (`test/browser/smoke.mjs` + `run.sh` + `mutate.py`): two scenarios over the
   real compiled binaries and a real Chromium (counter live view across two
   tabs on one session cookie; shared_doc `Doc_stats` over the jsoo XHR path
   and a browser-issued same-origin `Append_tag` through the D12 gate), 8 ok /
   1 xfail, all four mutations caught. Step 8 is **complete** as scoped.

   P7 closes the oldest residual in this file, and it paid for itself on the
   first run by finding **D14** (§7): the acting tab double-counts its own
   PN-counter dot, because the client and the server apply one msg under two
   different replica ids. Every in-process test passes, because no in-process
   test can see both applications at once. That is the next phase, and the
   smoke test pins the current behaviour as an `xfail` so the fix cannot land
   without coming back through it.
