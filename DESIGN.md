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
| `test/browser/smoke.mjs` - Playwright end-to-end smoke over the real compiled server binaries | **passes, 9 ok / 0 xfail** (confirmed by mutation; roadmap step 8, D13 - its D14 pin went stale when step 9 fixed the bug, and is now an ordinary assertion) |
| `Wire.down` + `Tea_client.Identity` - the server announces its session replica, the tab mints under it | **built, green** (roadmap step 9, D14) |
| `test/predictor_test` - one intent, one replica slot, across both tiers in one process | **passes** (confirmed by mutation) |
| `Wire.up` + `Tea_client.Delivery` + `Tea_server.Replay_guard` - sequenced up-frames, an unacknowledged queue, and per-(replica, tab) de-duplication above `update` | **built, green** (roadmap step 10, D15) |
| `test/replay_test`, `test/delivery_test`, `test/exactly_once_test` - the guard, the queue, and both tiers with the socket killed mid-flight | **passes** (confirmed by mutation) |
| `Tea_server.Guard_sink` + `Durable_guard` + `Tea_server_pack.Guard_file` - durable floor mirror behind the in-memory guard, CRC-framed append-only journal on the pack tier | **built, green** (roadmap step 11, D16) |
| `test/guard_sink_test`, `test/durable_guard_test`, `test/guard_file_test` + extended `exactly_once_test`/`replay_test`/`reaper_test` - codec totality, restart re-seed, torn-tail fold, tombstone-before-removal | **passes** |
| `Tea_server.Session_secret` + `?sessions` threaded through `handler`/`handler_pack`/`serve_pack` (deliberately NOT the mem tier's `serve`, so identity durability cannot outrun model durability) + `Store_pack.open_root` - durable session identity on the pack tier (`TEA_SECRET`/`TEA_SECRET_FILE`/`<root>.secret` resolver), per-process `memory` everywhere else | **built, green** (roadmap step 12, D17) |
| `test/session_secret_test`, `test/session_identity_test`, `test/pack_root_test` + browser B4/B5/B6/B7 - secret resolution, sealed composition, cookie adoption across restart, different-secret converse, typed pack-root failures (refused audibly AND non-zero), orphaned guard journal over a wiped root | **passes** (confirmed by mutation) |
| `Tea_core.Prim.Store_water` + the water-stamped `Advance` frame (tag `'\003'`) + `Guard_file.open_ ~head_water` boot filter with the four-way `verdict` - every durable floor carries the head water of its own session branch, and a restored/rolled-back pack root drops exactly the floors it no longer covers | **built, green** (roadmap step 13, D18) |
| `test/guard_water_test` (W1-W6 real waters on a pack store incl. a real dir-copy snapshot/restore, G1-G8 hand-built floors) + C1-C3 in `guard_sink_test` + browser B8 three-lives rollback scenario | **passes** (confirmed by mutation) |
| `Store_core.based`/`load_based`/`based_model`/`committed`/`commit_based` + `commit_coalesced` and `append_commit` re-pinned to the witness + `step_with ?interpose` taking the token - the TEA step is a compare-and-set whose denial reconciles through the app's own `Merge_spec.t` instead of refusing or re-running, and `shared_doc_serve`'s `Lwt_mutex` is retired | **built, green** (roadmap step 14, D19) |
| `test/contention_test` C1-C15, S1-S2 over four local apps (`Or_set`, a merging `Three_way`, a refusing `Three_way`, `Last_write_wins`) - the program-order interleave, distinct dots, parentage, the winning round's water, round count, the true ancestor across two rounds, both non-CRDT arms, a reap under the witness, pack close/reopen for both write arms, the coalesced append and the interrupted amend, undo as an R10b characterization, and the server seam through `?interpose` | **passes** (confirmed by mutation, except C11/C12 which are pins: see roadmap 14) |
| `Cmd.Http_delivery` + `Cmd.http_keyed` + `Tea_rpc.Key`/`keyed_resp`/`Applied_reply_lost` + `Tea_server.Reply_cache`/`Rpc_once`/`routes_once ?on_taken` + `Tea_client.Rpc_delivery` + a second `Guard_file` journal at `<root>.guard/rpc` behind `Tea_server_pack.open_guards` + `Durable_guard ?mirror` - the RPC tier routed through the D15-D18 guard family as a second channel (server-derived floor tabs, a bounded newest-reply cache, the `Mutating` 200 enveloped), with the floors mirror bounded as the ride-along | **built, green** (roadmap step 15, D20) |
| `test/rpc_once_test` (incl. T1's ordering arm: the keyed 200 waits for the floor's append), `test/rpc_pack_once_test` (the native B10: two lives over one pack root), `test/rpc_window_test` (the `?on_taken` two-writer checks), `test/reply_cache_test`, `test/rpc_delivery_test`, `test/rpc_journal_test`, `test/pack_guards_test` + browser B9/B10 lost-response scenarios | **passes** (confirmed by mutation: 19 ids, full dual-suite sweep) |
| `live_session` teardown restructured (`handle_frame` never a cancellation target, sender death a `Lwt.wait`-based `died` signal raced through `Lwt.choose`) + ONE `Lwt.protected` span over `step_ws` and both `persist_taken` arms - cancellation atomicity for the take-to-ack span | **built, green** (roadmap step 16, D21) |
| `test/cancel_test` s1-s7 - a deterministic mid-span sender death, the session promise waiting on the in-flight span, a `handle_frame` rejection releasing the taken seq for a Fresh replay that applies exactly once, an external cancel leaving a completed orphan with no ack minted, a replay raced against the still-parked orphan reading Duplicate, the fuel arm's once-ever bottom floor under cancellation, and a cancelled wrapper whose orphan then rejects still releasing for a Fresh replay | **passes** (confirmed by mutation: 7 s16 ids) |
| `Tea_core.Prim.Store_identity` + `Store_pack.resolve_identity` minting `<root>/tea.identity` + the tag-`'\004'` identity header on both guard journals + `Guard_file.open_ ~identity` with the five-way `identity_outcome` + `Guard_sink.Codec.frame`/`unframe` - a create-once store lineage token cross-binding the pack root to its guard journals, so a journal restored beside a DIFFERENT store drops its floors as visible duplicates instead of losing the replays silently | **built, green** (roadmap step 18, D23) |
| `test/store_identity_test`, `test/guard_identity_test` (I1-I13: both channels, the bottom-water stranger, the legacy adopt, the compaction round-trip, the Unresolved hold, its recovery and its held-file byte identity, the foreign header, the corrupt head) + `test/identity_explain_test` boot-log truthfulness + `pack_guards_test` header independence + `archive_gc_test` post-GC survival + `rpc_pack_once_test`'s end-to-end mismatch control | **passes** (confirmed by mutation: 12 s18 ids 12/12 killed + 5 review-fix ids) |

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
  a `send` on a CONNECTING socket raises. Replay was send-once by construction
  (a buffered msg is one the server never received) — **that reasoning was
  sound and still left a hole, which is D15 below: it classified only the msgs
  born while the link was down, and said nothing about a msg already handed to
  a socket that then died.** On the way in, a pushed head is `reconcile`d against the
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

> **D14 - the acting tab double-counted its own PN-counter dot. Found by D13
> on its first run; FIXED in step 9 (fix 1 below).** A locally-born msg was applied twice under two
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
>
> **Shipped (step 9): fix 1.** `Wire.down` makes the down-channel a two-case
> sum - `Hello (replica, head)` once per socket, `Head model` for every commit
> after it - derived through the same `Repr` witness as everything else, so the
> announcement cannot drift from the pushes. `live_session` takes the replica
> from the session's *own* `ctx_of_session`, not a second derivation of the
> branch name, so an announcement can never disagree with what the server
> applies under. `Tea_client.Identity` holds the tab's replica in one
> page-global cell that `Rebase.absorb` rebinds on every `Hello`, reconnects
> included. One intent is now one replica slot, and `join` reconciles the
> client's prediction with the server's apply by per-slot `max` instead of
> summing them. Confirmed end-to-end: the D13 acting-tab pin went stale and now
> asserts A settles at 1, same as the observer tab.
>
> **The boot-order decision** (the cost fix 1 was priced at): a tab that acts
> *before* its `Hello` arrives has no id to predict into, so it mints under a
> provisional replica and that one edit keeps a slot of its own. `absorb`
> deliberately resyncs to the `Hello`'s head rather than folding the local
> model in, which sheds the stale prediction - but it cannot *un-mint* the dot,
> because the head is delivered through the app's own store-watch handler,
> which joins. No CRDT here can offer an un-mint at all: `Lww` has no previous
> value to revert to. So the window is narrowed to "edits made between mount
> and the first frame" and pinned, in `test/predictor_test`, as behaviour that
> is recorded and not blessed. The socket opens at mount, so on a page that
> reaches its server the window is one round trip; a page that never reaches
> one keeps the provisional id forever, which is sound precisely because it
> also never forwards a msg for a server to apply a second time.
>
> **Known bound, newly load-bearing:** client and server now mint dots under
> one replica id from *two* clocks, so `Dot` uniqueness is no longer carried by
> construction. It holds on magnitudes: the client's wall source is `0`
> (deliberately, R6 - it must lose every LWW tie to the server), so its stamps
> are small counts while the server's are wall-seconds. A collision needs ~2^31
> edits in one page life. Documented on `Crdt.Dot`; if the client ever gets a
> real clock, this needs a per-tier tag in the dot instead.
>
> **Found while fixing it:** `Codec.of_json` was not total. `Repr.of_json_string`
> answers a malformed document with `Error` but *raises*
> `Invalid_argument "index out of bounds"` when a well-formed object names a
> variant case the witness does not have - so `{"Bogus":1}` on the live socket
> killed the pump instead of closing it, and the same frame through the form
> post answered 500 where it meant 400. Reachable by anyone who can open the
> socket, on a surface every caller reads as total because it returns a result.
> Now caught at the one seam, with checks on both the frame witness and the msg
> witness.

> **D15 - a msg handed to a dying socket was lost, silently. FIXED in step 10.**
> `send_or_buffer` wrote `WS.send ws (Codec.msg_to_json msg)` and forgot the
> message. The outbox held only the msgs *born* while the link was down, so a
> msg already handed to a socket that then died was in no queue at all: nothing
> replayed it, and the next `Hello` made `absorb` resync to the server head,
> discarding the prediction. No error, no log, no trace - the edit simply never
> happened. D9's send-once reasoning was sound about the msgs it classified and
> silent about this one, which is why no in-process test saw it: none of them
> could kill a socket mid-flight.
>
> The fix is at-least-once delivery plus de-duplication, because retrying alone
> would recreate D14's double count from the other direction - state *join* is
> idempotent, replaying an *op* is not, and no CRDT here detects a re-applied
> op (a re-executed msg re-mints its dot from the server's own clock).
>
> - **`Wire.up`** gives every up-frame a delivery header: `Apply {tab; seq; msg}`.
>   The fields stay primitives, not newtypes, because the client chooses them:
>   they become `Prim.Tab_id.t` / `Prim.Msg_seq.t` only after the server's own
>   validators accept them. A `Repr` witness for an abstract type is total on
>   decode and would admit inhabitants the newtype's constructor rejects.
> - **`Prim.Tab_id`** is the identity the scheme needed, and the reason it is a
>   *new* one is the trap: a replica id is minted from the session, and a session
>   is a cookie, so **two tabs on one cookie share one replica id** (the D13
>   smoke test drives exactly that). A guard keyed on the session alone answers
>   "already seen" to the second tab's first edit and drops it forever - the very
>   bug this closes, reintroduced by its own fix. A socket id will not do either:
>   it dies precisely when de-duplication matters. So the key is per page load,
>   minted client-side, and it is a de-duplication key, **not a credential**.
> - **`Tea_client.Delivery` replaces `Rebase.Outbox`**, and the replacement is the
>   point: membership is now "recorded when made, dropped when acknowledged"
>   rather than "born while the link was down", so in-flight and offline-born
>   edits stop being different cases. Keeping both queues would have been worse
>   than keeping neither - two replay triggers at two different times means an
>   offline-born edit reaches the server *ahead* of an older already-sent one,
>   which under `Last_write_wins` flips the winner.
> - **`Tea_server.Replay_guard`** is one integer per `(replica, tab)`, consulted
>   *above* `A.update`. One integer suffices because a WebSocket delivers in
>   order, so a dead socket truncates a *suffix*: what the server has consumed
>   for a tab is always a prefix, and a prefix is one number.
> - **The ack is minted by the pump, never by the watch.** The watch fires for
>   every writer on the branch - a form post, an undo, the other tab - so a
>   `Head` cannot stand in for an acknowledgement; and a msg whose update is a
>   no-op produces no commit at all, so a watch-derived ack would never arrive
>   and the client would retry that msg forever.
> - **Consume before apply** is structural, not a convention: `Cell.take` is
>   synchronous with no Lwt yield point before the step, so two live sockets for
>   one tab cannot both see a seq as fresh, and a msg whose update exhausts fuel
>   is attempted exactly once instead of killing the session on every reconnect.
>
> **The guarantee, stated exactly: exactly-once effect within one server process
> lifetime and within the guard's bounds; at-least-once outside them.** (Step
> 10's statement, and still the whole story on the mem tier; step 11, D16 below,
> extends the lifetime bound across *orderly* restarts and names the one case
> that stays out of scope.) The
> bounds are honest, and every one of them degrades toward *duplicating* rather
> than losing, because the guard never invents a high water it did not observe:
> an absent entry accepts anything. A server restart on the mem tier re-applies
> whatever is
> replayed (in memory, deliberately not announced on the wire: on learning an
> epoch changed, a client's only sound action is still to replay, so the frame
> would change no behaviour). Eviction at the bounds does the same. The one path
> to the *loss* side within the guard's own state is a stale entry outliving its
> branch, which is why
> `Replay_guard.forget` exists and why its precondition is written on it: any
> future reaper loop must call it for every collected session. (The durable
> layer adds a second loss path, stated in D16.)
>
> **Left open, deliberately:** the client's unacknowledged queue is unbounded,
> exactly as the outbox was - and that is what lets the server refuse a gap
> outright, since an honest client never skips a seq because it never discards
> one. The RPC tier was not covered: `/rpc/*` is a non-idempotent POST with the
> same problem and a different retry origin (the browser's, not this
> framework's) - **closed in step 15 (D20 below)**, which routes `/rpc/*`
> through this same guard family as a second channel rather than building a
> sibling stack. A frame whose *envelope* cannot be decoded has no seq to consume
> and still closes the session, which needs the two tiers' `up_t` witnesses to
> disagree - a deploy skew, the state T3 calls disallowed.
>
> **What step 10 does *not* change:** the D14 pre-announcement window, exactly as
> pinned. A replay is a re-send, never a second local apply, so it mints no new
> client dot; de-duplication suppresses the second server apply, so it mints no
> new server dot. `test/predictor_test`'s pin stands, and `exactly_once_test`
> carries a sibling pin so a future fix reddens both rather than one.

> **D16 - the high water made durable. Landed in step 11.** Step 10's guarantee
> stopped at the process boundary: the high water lived in memory, so a restart
> forgot it, and a client replaying an unacknowledged msg across that restart
> was applied a second time. Step 11 puts a durable floor behind the guard
> without moving the verdict off its synchronous path:
>
> - **`Tea_server.Guard_sink`** is the seam: an `Advance`/`Forget` event, a
>   `null` sink (the mem tier, step-10 semantics byte for byte), a `memory`
>   sink (the test seam for simulated restarts), and a pure, total,
>   CRC-32-framed codec, so a torn tail or a flipped byte is a classified
>   verdict rather than an exception.
> - **`Tea_server.Durable_guard`** is two layers: the bounded in-memory
>   `Replay_guard.Cell` in front, a durable `Floors` mirror behind. A Cell miss
>   is seeded from the mirror *before* the verdict, `take` stays synchronous
>   (consume-before-apply is still structural), and `persist` is the only
>   asynchronous write.
> - **`Tea_server_pack.Guard_file`** is the pack tier's journal: append-only at
>   `<root>.guard/journal`, fold-until-broken on read, per-record flush, fsync
>   only on close, a cap with drop-oldest, compaction at 4x cap and on any
>   open where the boot filter dropped floors (the drop is durable: a refused
>   record left behind would silently un-drop once the head rises past it).
> - The pump persists **after the apply attempt and before the ack**, on both
>   the success and the fuel-exhausted arms, because the water means "taken",
>   not "applied" (a no-op msg mints no commit, so the record cannot ride the
>   store).
> - `reap ?forget` writes the tombstone **before** the branch removal, so a
>   crash between the two steps lands on the duplicate side.
>
> **The guarantee, restated in three cases** (amended in step 13, D18, whose
> floors carry a store witness; this block supersedes the step-11 wording):
>
> - **Exactly-once effect across an orderly restart - now including a restart
>   onto a pack root that was restored, rolled back, or hot-copied.** A
>   durable floor is honoured only while its own session branch still carries
>   a commit at least as new as the one standing when the floor was taken
>   (`Store_water.covers ~head ~floor`, equality load-bearing: strict would
>   drop every floor on every clean restart). A branch that went backwards,
>   or vanished, cannot suppress a replay. Within the guard's bounds, as
>   before.
> - **At-least-once - a visible, convergent double count - on every
>   degradation, and the set is now larger.** A torn journal tail, a cap
>   eviction, a failed append, and a missing journal, as before; and now
>   also a floor whose branch head no longer covers it (rollback, hot copy,
>   reap, a checkpoint GC that made the head unreadable, an `undo` that
>   moved the head back), a pre-step-13 journal (every legacy record reads
>   `Store_water.bottom` and is adopted on trust for its own lifetime), and
>   a step-12 binary reading a step-13 journal (it stops at `Bad_tag 3` on
>   the first record and discards the whole file). Each of these loses only
>   floors, and an absent floor accepts anything.
> - **Still on the loss side, narrowed but not closed.** `kill -9` is now
>   largely paid off on the pack tier. The design rests on one inequality: a
>   floor must not be more durable than the commit whose effect it records,
>   and a hard kill still violates it mid-life (the journal reaches the page
>   cache per record, irmin-pack buffers commits in user space). But a floor
>   whose commit died in that buffer no longer has a covering branch head at
>   the next boot, so it is dropped and its message replays - the exact case
>   step 11 declared out of scope (R11). The window survives only for tabs
>   that never minted a witness - every record reads `bottom` (no-op or
>   fuel-exhausted takes before any real commit, or a legacy journal); a
>   witness-less take after a real one cannot drown the elder stamp, because
>   the floor fold never lowers a water - and for divergence that
>   begins *after* the boot check, which runs once at `Guard_file.open_` and
>   says nothing about a second writer over the same root (R18) or a swap
>   under a live process. The identity half is R20a (§10): a journal
>   restored beside a *different* store whose same-named branch carries a
>   newer head date is not detected - order, not identity, is what a water
>   can answer. And `<root>.secret` remains bound to neither sibling: an
>   absent or rotated secret still silently orphans every branch and every
>   floor.
>
> **Residuals, stated rather than hidden:**
>
> - `Durable_guard`'s in-process `Floors` mirror is **unbounded**. The
>   asymmetry is exact: the `Cell` in front of it is bounded precisely because
>   an attacker mints session cookies freely, and `Guard_file`'s cap bounds the
>   journal's copy of the floors, but the mirror between them has neither
>   defense, so a hostile cookie-minting client grows it without limit (§10,
>   R12).
> - A server that wires `reap` while a durable guard is live **must** pass
>   `Durable_guard.forget`, or a floor outlives its branch and every replay
>   onto the recreated branch reads `Duplicate` against a stale high water:
>   total silent loss onto an empty model. Step 11 closes the ordering half by
>   construction (tombstone first, removal second); the wiring half stays the
>   caller's obligation, exactly as `Replay_guard.forget`'s precondition
>   states. Step 13 narrows the unwired case to a window: at the next boot
>   the filter drops every *witnessed* floor whose branch is gone
>   (`dropped_no_branch`), so the exposure is the mid-life stretch between
>   the reap and the following restart - plus `bottom`-water floors, which
>   no head can drop.
> - The two-clocks-behind-one-replica-id note (step 9) and the D14
>   pre-announcement window are unchanged by this step.

> **D17 - durable session identity (roadmap step 12).** Step 11's journalled
> floor worked and nothing consulted it. `Tea_server.Handlers.handler` ran the
> router under `Dream.memory_sessions`, and `hex (Dream.session_id request)`
> derives BOTH the Irmin branch name AND the CRDT replica id, so a restart
> minted a fresh session id and a reconnecting tab landed on a NEW branch (empty
> model) under a NEW replica, where the journalled floor is looked up under a
> key that can never match. Durable floors need a durable identity, and the
> browser harness pinned exactly that: after SIGTERM plus restart the server
> held 1, not 2.
>
> The fix is one string made to survive an `execve`. `Tea_server.Session_secret`
> owns the session back end as a sealed sum: `memory` is `Dream.memory_sessions`
> verbatim (the default of every handler, and the correct pairing for a volatile
> store, because identity durability must never exceed MODEL durability - a
> durable id over a dead store is the reads-1 loss arm), and `durable` is
> `Dream.cookie_sessions` under `Dream.set_secret`. The module hands out ONE
> composite middleware and never the two halves, because the nesting is
> load-bearing and silently catastrophic when inverted: `set_secret` installs
> the key by mutating the request on the way IN, so a session back end wrapped
> around it loads with Dream's process-global fallback and stores with the
> configured secret, minting a fresh session on every request. `hex` is kept
> exactly as it is: removing it would rename every branch (`session-<hex>` to
> `session-<raw>`) and lose every session on upgrade, `Branch_name.of_session`
> does not validate, and `hex`'s output is a subset of both the `Session_id` and
> `Branch_name` alphabets, so the derivation cannot produce an inadmissible ref
> name.
>
> `serve_pack` is the one place the default flips, because it is the one tier
> whose model is itself durable. The secret is resolved synchronously from
> `TEA_SECRET`, else `TEA_SECRET_FILE`, else `<root>.secret` - a SIBLING of the
> pack root, the same discipline and the same argument as `<root>.guard` -
> minted on first boot with `O_EXCL` mode `0600` and fsynced, so two boots
> racing on one root cannot clobber each other (the loser's `EEXIST` re-reads
> the winner's bytes). Every failure is a typed value the wiring site folds
> over, one stderr line, and a fall back to per-process identity: never an
> abort, never silence. Note the degradation direction differs from the guard
> journal's - a lost journal DUPLICATES, a lost secret ORPHANS every existing
> session branch, and with no reaper wired those branches are never reclaimed.
>
> What the browser harness now measures: B4 asserts the restarted server holds 2
> and, on the same request, that life 2 ADOPTS the presented cookie (200 with no
> `Set-Cookie` reissue - Dream emits the cookie only when the session is dirty,
> and a cookie it could not decrypt yields a fresh dirty pre-session, so absence
> is positive proof of adoption), that life 2 is a new pid, and that
> `<root>.secret` is mode 0600. B5 is the converse that makes B4 attributable:
> two lives with DIFFERENT secrets do not share the session. `mut-b4-fresh-root`
> and `mut-journal-unwired`, both blocked since step 11 because the identity gap
> made them unobservable, are now unblocked and distinguishable - fresh root
> reads 1, unwired journal reads 3.
>
> Left open here and recorded in section 10: the durable cookie is an
> unrevocable bearer credential with a sliding lifetime (R13); the secret is the
> system's only credential and the id space it protects is enumerable from disk
> (R14); `Guard_file`'s cap eviction now evicts LIVE clients (R15, amending
> R12); the cookie session payload is rollback-able the day anything is put in
> it (R16); Dream's own cookie loader has two residual raises past a successful
> decrypt (R17); behind a TLS-terminating proxy the durable cookie ships
> without `Secure` and without the `__Host-` prefix, and the exposure is no
> longer bounded by process lifetime (R19); and the three durability siblings
> (`<root>`, `<root>.guard`, `<root>.secret`) can be restored out of step,
> where losing the pack store alone turns the surviving floors into silent loss
> (R20 - the outright-wipe case is refused at boot; the rollback case is
> closed per-floor by step 13's water filter, D18 below, leaving the
> different-store residue R20a).

> **D18 - the store water: a monotone, derived, per-floor witness (roadmap
> step 13).** Step 12's boot refusal covers only the outright wipe; a pack
> root ROLLED BACK under a surviving guard journal still made every stale
> floor read `Duplicate` against an effect the store no longer holds - R20's
> silent-loss residue, reached with no reaper involved. Step 13 binds each
> floor to the store state it de-duplicates against: `Prim.Store_water` is
> the session branch's head `Info` date (a step-6 `Clock` stamp, strictly
> increasing per branch), captured from the very commit the take minted -
> `commit`/`commit_coalesced`/`append_commit` now *return* the water, so the
> persist hot path never reads a head - and written into the `Advance`
> record (tag `'\003'`; legacy tag `'\001'` decodes at `bottom` forever,
> written never). `Guard_file.open_` compares every floor against one
> `Store.branch_waters` read and drops what the live head no longer
> `covers`, counted in a four-way `verdict` and reported in at most three
> conditional operator lines. Four adjudications carry the design, recorded
> so they are not re-litigated:
>
> - **Derived, never written.** A witness that must be written can itself be
>   restored out of step with the thing it witnesses - R20 recursed, growing
>   the pairwise restore surface from C(3,2)=3 to C(4,2)=6. A witness that
>   *is* a function of the store's own bytes cannot disagree with them:
>   there is no fourth durability sibling, no reserved ref, no journal
>   header, and nothing foreign written inside the pack root.
> - **Per-key, not a global scalar.** Sufficient three times over: a global
>   mismatch would empty up to every floor for sessions that were never
>   rolled back (blast radius); a global max is structurally blind to a
>   SELECTIVE restore of one branch while others advanced (coverage); and
>   per-key pays off R11 for free - a commit lost in irmin-pack's user-space
>   buffer leaves exactly its own floor above exactly its own head, so
>   exactly that message replays as a visible duplicate, which no global
>   epoch can do without emptying everything.
> - **A field on the record, not a journal header.** The certificate rides
>   the `Advance` frame itself, so a floor without one is unrepresentable. A
>   header would be erased by the first compaction (`compact` rewrites
>   purely from the kept floors); a sibling witness file is a fourth path to
>   restore out of step; a free-standing water event re-opens the question
>   of which floors a stamp covers. One parser, one length field, one CRC.
> - **Drop and serve, never refuse.** An uncovered floor is dropped and its
>   replay re-admitted as a visible, convergent duplicate - the accept
>   direction the whole guard family degrades toward - where refusing to
>   boot would turn a routine checkpoint GC or reap into an outage. Only
>   `dropped_behind` (a readable head strictly below the floor) speaks of
>   rollback; a missing or unreadable head is `dropped_no_branch`, routine
>   after GC; adopted `bottom` floors are `unwitnessed`, the
>   trust-on-first-use upgrade window, and the first post-upgrade boot is
>   classified on the LOSS side of the guarantee and says so on stderr.
>
> One refutation recorded because the false argument would outlive the
> session that heard it: a global water is NOT attacker-poisonable. The
> claim was that N commits inside one second advance the clock N seconds
> past wall time, permanently blinding a global check. But `Clock.next`
> advances strictly off `last`, and reopening reseeds from every head, so
> every commit after a poison to P mints P+1, P+2, ...: any later stamp
> sits strictly ABOVE the water of any snapshot containing the poison, and
> the rollback is still caught. Per-key stands on the three grounds above,
> not on the poisoning claim. One false positive accepted and named rather
> than special-cased: `undo` moves a branch head backwards, so a session
> that used `undo` has its floors dropped at the next restart - only
> unacknowledged messages replay, so the cost rounds to zero, and the
> undone effect really is gone, which makes the re-admission defensible as
> a true positive.

> **D19 - the witnessed step: compare-and-set with reconciliation, not retry
> (roadmap step 14).** R10 said the framework's step was a read-modify-write
> with no compare-and-set. D19 closes it by moving the whole read-modify-write
> onto a compare-and-set keyed to a **load-time witness**, and by resolving
> contention with the app's own `Merge_spec.t` rather than by re-running
> anything.
>
> `Store_core` gains an abstract, session-bound token `based`, minted only by
> `load_based`, carrying the head commit the model was read *through* and the
> model itself. `commit_based` owns a loop: mint a commit parented on the
> witnessed head, move the branch by `S.Head.test_and_set`, and on denial
> re-observe, reconcile the once-stepped model against the head that actually
> landed, and re-attempt against that new witness.
>
> Three consequences carry the design.
>
> - **The witness is minted at load and never re-read at commit.** That is
>   precisely what the old `append_commit` got wrong: it re-read the head and
>   tested against *that*, so the test almost always passed while the stale
>   model still landed on top of the racer. R10 named this in so many words -
>   the retry preserved history, not content. The window R10 describes opens at
>   the `load`, not at the commit, so a witness taken any later closes nothing;
>   a `~test` parameter on `commit` would have been self-satisfying. The token
>   is abstract for the same reason, and session-bound for a second: a witness
>   minted on branch A presented to branch B would typecheck under a labelled
>   `~base:`, and under a total commit that mistake is not even an error - the
>   test fails, the loop reloads B, and a foreign model is joined into it.
> - **Nothing is re-run.** `Loop.step` executes exactly once per taken message,
>   so no `fx.sleep` fires twice, no `fx.navigate` is recorded twice, and no
>   CRDT dot is minted twice. Re-running the pure `A.update` against `theirs`
>   in the conflict arm was tempting and is rejected: it silently drops any
>   part of `ours` that came from the settled `Cmd` tail, and the store cannot
>   tell whether such a part exists, so it trades a declared, labelled,
>   history-preserving loss for an undetectable one. The commit *date*, by
>   contrast, IS re-minted per round: freezing it would let a retried commit
>   land with a date older than the racing parent it retried over, which is the
>   monotonicity failure D18's water rests on.
> - **The commit is total**, so the pump's `Fresh` arm is byte-identical to
>   before and the inversion the D16/D18 family exists to prevent - a durable
>   floor raised for an apply that was refused - is unrepresentable rather than
>   handled. See §10 R10 for why a `Contended` refusal with a retry budget is
>   unsafe upstream of any floor, and R10d for the fate this does not close
>   (**closed in step 16**, D21).
>
> One door serves both the whole-blob and the exploded arms on the based path
> (`S.Commit.v` + `S.Head.test_and_set`, layering onto `S.Commit.tree` of the
> witnessed commit), because `writes` already reduces the whole-blob case to
> the single historical `model_path_raw` write and `scatter` consumes either
> list identically. Plain `commit` is untouched in both arms, which is the
> strongest available form of "the whole-blob / exploded non-unification is
> preserved": that comment's subject is the historical `set_exn` transaction
> and the witness path's `set_tree_exn`, and neither changes a byte. The based
> path never touches `S.tree`; that is an argument rather than a proof, which
> is why C11 and C12 round-trip a real pack store before the step counts as
> green.
>
> The water a floor may claim is read off the commit the CAS minted, never off
> a head after the fact: a head read after a successful commit can belong to a
> later writer, and a floor stamped with it is a forged witness.

> **D20 - rpc-exactly-once: the RPC tier as a second channel through the
> D15-D18 guard family (roadmap step 15).** The WS tier already owned a
> complete exactly-once stack: a dense per-tab seq space (D15), a synchronous
> consume-before-apply verdict, a durable floor journal with a boot-time
> store witness (D16, D18), and one law for every degradation - toward the
> visible duplicate, never silent loss. Step 15 does not build an RPC sibling
> of that stack; it routes the RPC tier *through* it: one verdict type, one
> boot filter, one `Floors` shape, one sink type, and the pump discipline
> transcribed onto the RPC dispatch path (origin gate first, then the key
> parse, then the verdict, then the witnessed step, then the floor's persist,
> and only then the 200). The load-bearing insight: **an RPC floor is keyed
> by the branch the effect lands on, not by the client that asked for it.**
> The key is `(floor_replica, floor_tab)` with `floor_replica` the canonical
> replica minted by the same path `branch_waters` and `ctx_of_session` share,
> so the D18 boot filter works unchanged - `branch_waters` already lists
> `main`, the floor's water is the canonical commit's own mint, and `covers`
> asks its question about one branch. WS floors are keyed by session replicas
> whose branch names are `"session-" ^ hex` by construction, so the two
> channels' key spaces are disjoint by grammar even before the two-journal
> separation makes the partition structural.
>
> Four sub-decisions carry the design:
>
> - **D20.1 - the floor tab is server-derived, never client-asserted.** The
>   wire key stays `x-tea-key: <client tab hex> ":" <seq>`, but the guard,
>   the reply cache, the journal record and the boot filter are keyed on
>   `floor_tab`, the digest of
>   `"tea-rpc-floor\000" ^ hex(session_id) ^ "\000" ^ client_tab` - the
>   session id enters HEX-ENCODED, so both halves are hex and neither can
>   contain the NUL separator: no two distinct (session, tab) pairs slide
>   into one digest input. The output is exactly 32 lowercase hex
>   characters, exactly `Tab_id`'s grammar, so the parse is total in fact. A forger who knows a victim's client tab but not its
>   cookie lands in its own namespace (T13). The unkeyed-`Digest` honesty
>   is R23 (§10).
> - **D20.2 - the concurrent-duplicate window is a Pending marker plus a
>   framework 503, never a promise join and never a spurious `Replayed`.**
>   The `Reply_cache` slot is a sum. The Fresh arm marks Pending
>   synchronously in the continuation that received the verdict - no yield,
>   the D15 consume-before-apply law untouched - and settles it with the
>   reply bytes after the persist attempt. A duplicate finding Pending is
>   answered 503, consumed by the client runtime's existing 5xx retry arm
>   and never surfaced to `expect`; the retry then reads Settled and gets
>   the original bytes. `pending_grace` is a per-window POLL BUDGET, not an
>   age: only the window's own tab's Busy answers spend it, so foreign
>   traffic can never age a live window into a spurious `Replayed`, while a
>   wedged apply's own polling still drains the window to `Gone` rather
>   than holding its client hostage. `settle` compares seqs and discards a
>   stale settle whole, so newest-wins is enforced rather than assumed. And
>   a rejecting apply is caught by a barrier around the keyed handler: the
>   failure arm releases the take (`Durable_guard.release`, conditional on
>   the high water still being exactly the taken seq), leaves the Pending
>   marker standing for racing duplicates, and answers 500 for the client's
>   5xx retry arm to re-send - toward the visible duplicate, never silent
>   loss (R27). Step 16 mirrors this barrier in the WS pump's `Fresh` arm
>   (D21), with one placement the keyed tier does not need: the WS catch
>   sits INSIDE the `Lwt.protected` body, so it only ever sees
>   body-originated exceptions and can never release while the protected
>   orphan may yet persist the floor.
> - **D20.3 - two journals, no `Floors.split`.** The RPC channel opens its
>   own `Guard_file` at `<root>.guard/rpc`: caps and compaction are per
>   channel, and each boot filter runs per journal against one shared
>   `branch_waters` read. `Tea_server_pack.open_guards` is the exported
>   seam - a `guards` record over both channels' guards and journals - and
>   the ws journal opens first because `Guard_file` creates its own
>   directory, not its parent: `<root>.guard/rpc` is only creatable after
>   `<root>.guard` exists.
> - **D20.4 - bounds are derived from the tables they front, not written as
>   constants.** `Durable_guard.v ?mirror` bounds the floors mirror (the
>   R12 ride-along), defaulting to `default_mirror = max (4 * sessions *
>   tabs) journal_cap`; `Reply_cache.v` takes `~entries ~max_bytes
>   ~body_cap ~pending_grace` with stated defaults and a byte budget.
>   Mirror eviction is LRU over a monotone recency tick and REMOVES a
>   floor, never lowers one - absence is the accept side - and it is the
>   stack's only self-healing eviction: the journal record survives its own
>   cap, so the evicted floor returns at the next boot. Within one process
>   lifetime, dedup for an evicted key is suspended until restart; across
>   restarts it resumes if the journal record survived. (T11, T16, T17;
>   §10 R12 closes on this.)
>
> **The guarantee, in the D16 three-case voice.** For a `Mutating` RPC that
> carries a delivery key, whose retries present the same session cookie, and
> whose encoded reply fits `body_cap`, on the pack tier with the RPC journal
> open and durable session identity (D17):
>
> - **Exactly-once effect across browser retries and an orderly restart**
>   (SIGINT/SIGTERM: store closed, then journal): the endpoint's effect is
>   applied exactly once however many times the key is delivered, and the
>   reply is answered byte-identically to every retry while the process that
>   computed it lives, degrading to the typed `Replayed` marker across a
>   restart or a reply-cache eviction. Never a silently re-applied effect;
>   never recomputed at replay time: the bytes answered to a retry are the
>   bytes computed by the one delivery that was taken.
> - **At-least-once - a visible convergent double apply - when the floor is
>   lost:** a torn RPC-journal tail, an RPC-journal cap eviction, a
>   mirror-bound eviction, or a failed append. Each loses only a floor, and
>   an absent floor accepts anything.
> - **`kill -9` out of scope on the loss side, exactly as D16/D18 state
>   it:** the journal reaches the page cache per record while irmin-pack
>   buffers commits in user space, so a floor can outlive the
>   canonical-branch commit it witnesses; at the next boot the filter drops
>   every witnessed such floor and the retry re-applies as a visible
>   duplicate. What remains exposed is bottom-water floors (fuel-exhausted
>   or no-op takes) and divergence after the boot snapshot, bounded by the
>   pack's auto-flush lag.
>
> The preconditions are part of the sentence, not footnotes: the key must be
> present, the cookie stable across retries, and the reply within the cap,
> for the first case to hold. A keyless request keeps today's semantics -
> at-most-once effect per HTTP exchange, retry safety the caller's problem -
> and a cookieless caller (curl, server-to-server) is keyless in effect,
> since its Dream session, hence its floor namespace, is fresh per request:
> the accept direction. On the mem tier the whole sentence collapses to
> D16's honest first case, and with `Session_secret.memory` the namespace
> derivation is not restart-stable, consistent with that tier claiming
> nothing across restarts.
>
> **The 200 is strictly ordered after the floor's append**, exactly as the
> pump's ack is: T1's ordering arm holds the sink's append open behind a
> gated promise and pins that the keyed 200 is still unresolved while the
> append is held, resolves once released, and followed exactly one append.
> An acknowledgement that outruns its floor is the inversion the whole
> D15-D18 family exists to forbid, on the second channel as on the first.
>
> The step-14 example-tier residual - no two-writer check against the real
> `Mutating` handler, no interpose seam - closes here: `routes_once
> ?on_taken` is the interpose-class hook (one-shot, re-pointed to a no-op
> before it runs), and T7/T14 land a concurrent duplicate inside the
> take-to-settle window against the real handler, while T2b pins reply
> honesty under D19 reconciliation and browser B9 retries a genuinely lost
> 200 through a response-eating route.
>
> **The reply-cache seam, conditionally:** the consumed marker (the floor)
> and the reply bytes live in two stores with two lifetimes - the journal
> survives restart, the cache does not, which is why `Replayed` exists at
> all. IF a durable reply tier is ever added, the consumed marker and the
> reply bytes must land as one record, with their own store-water witness
> and boot filter (R20 recursed). Recorded as a conditional obligation of
> that future step, never as a binding invariant of the current split
> architecture.

> **D21 - cancellation atomicity: the take-to-ack span outlives its socket
> (roadmap step 16, R10d).** The pump's span - admit, `step_ws`,
> `persist_taken`, ack - consumes the sequence number synchronously at its
> head, and the guard outlives the socket. A cancellation landing anywhere
> inside that span therefore left the seq consumed, the effect unapplied and
> no floor persisted, so the reconnect's replay read `Duplicate` and was
> acked without applying: window W1, the silent loss the whole D15-D18
> family exists to forbid. And the old teardown manufactured exactly that
> cancellation itself: `live_session` ended in `Lwt.finalize (fun () ->
> Lwt.pick [ sender; pump ]) unwatch`, so a send failure cancelled the pump
> mid-span.
>
> Two cancellation sources, two mechanisms. The **internal** one is closed
> by construction: the per-frame body now lives in `handle_frame`, never an
> element of any `Lwt.pick`/`Lwt.choose` list and never a `Lwt.cancel`
> target. The sender's promise is bound as `drained`, never raced, and
> joined by teardown; its death
> is a `Lwt.wait`-based one-shot `died` signal, fired in the sender's own
> catch arm - at most once, because `iter_s`'s promise rejects at most once
> - and a `wait` promise is not cancelable, so no cancellation search can
> reject it. The pump waits for next-frame-versus-died through `Lwt.choose`,
> which never cancels the losing branch: a hung `receive_frame` cannot block
> death detection, and death detection cannot cancel a receive. A
> `Lwt.state` pre-check on `died` makes the between-frames observation
> deterministic: `choose` picks uniformly at random among candidates that
> are ALREADY resolved, so a death that fired during the previous span,
> raced against a pipelining client's already-available frame, would
> otherwise be a coin flip. With the pre-check, `choose` only ever starts
> on two pending promises, and a frame that loses to `died` there is
> abandoned unread - the client retransmits on reconnect and the guard
> adjudicates the replay as usual. The `died` arm of that choose is minted
> once, above the recursion: a per-iteration `Lwt.map` would register one
> more permanent waiter on the pending `died` every frame - a leak that
> grows for the life of a healthy session - while `choose`'s own waiters
> are removable and cleaned up as each round settles. The
> wait-for-the-in-flight-frame property costs nothing: the recursive bind
> chain already forwards it, so the session's promise cannot settle while a
> `handle_frame` is still running - pinned rather than assumed (s2).
>
> The **external** source - a caller cancelling the promise `live_session`
> returned - is stopped by ONE `Lwt.protected` body covering `step_ws` and
> BOTH `persist_taken` arms, the success floor and the fuel-exhaustion
> bottom floor. The backwards cancellation search stops at the wrapper: the
> wrapper rejects `Canceled`, the body runs to natural completion as an
> orphan, and no ack is ever minted for it, so a torn window lands at worst
> in W2's already-licensed visible-duplicate shape. `protected` and not
> `no_cancel`, deliberately: under `no_cancel` the outer bind survives and
> the continuation RUNS, so the pump would recurse past the death of its own
> socket; under `protected` the outer chain rejects immediately and the
> recursion is unreachable. Two Lwt truths bound the claim rather than
> break it: a cancel landing in the callback-deferral window after the
> body already resolved is a no-op (cancellation is advisory; the session
> lives on as if the cancel came a frame later), and the catch's default
> filter passes runtime exceptions (`Out_of_memory`, `Stack_overflow`)
> through without the release. The fuel arm's once-ever contract now holds
> under cancellation too: the orphan finishes `Loop.step`, observes
> `Fuel_exhausted`, and persists the bottom floor.
>
> The third closure (rounds 2-3) is the REJECTION door. `protected` only
> converts a cancellation into a completing orphan; a rejection of the
> span's own body - the app's `update`, the commit door's I/O - propagated
> with the seq already consumed by the synchronous take and no floor
> persisted, so the reconnect replay read `Duplicate` and was acked without
> applying: W1 through another door, and the very hazard the keyed HTTP
> tier already closed with the R27 barrier. An `Lwt.catch` INSIDE the
> protected body mirrors R27: a rejection runs `Durable_guard.release`
> (conditional on the high water still being exactly the taken seq) and
> re-fails, so the replay reads `Fresh` and re-applies - if the body
> half-landed before rejecting, that is a visible convergent duplicate, the
> licensed direction. The discrimination the barrier needs - release only
> when the body is dead, never while a live orphan may yet persist the
> floor - is POSITIONAL, not a match on the exception value: `protected`
> never rejects its body, so the wrapper's cancel never reaches the catch,
> and every exception the catch does see is body-originated, making the
> release unconditionally its compensation. Round 2 seated the catch
> OUTSIDE `protected` and matched on `Canceled`, and that shape had two
> holes: a body that rejected AFTER the wrapper was cancelled re-failed
> into an already-settled promise, so the rejection was dropped before any
> release ran (cancel-then-orphan-rejects: W1 again), and a body-internal
> `Canceled` - a third party cancelling something inside the span, with no
> orphan behind it - was misread as orphan-alive and never released.
> Inside the body both close: the orphan's own catch releases before the
> re-fail is dropped, and a body-internal `Canceled` is a dead body like
> any other. R27's HTTP path has no orphan, so it needs no such
> positioning.
>
> The sink seam is totalized in the same spirit: `Durable_guard.persist`
> advances the floors mirror BEFORE its append, so a caller-supplied sink
> that REJECTED the append promise (the contract says resolve `Error`)
> would carry a rejection born after the mirror advance across the release
> barrier and roll the Cell back behind the mirror - re-opening a seq the
> mirror already floors and, on the fuel arm, breaking once-ever.
> `persist` catches the rejection at the one seam both tiers share and
> degrades it to the same audible `Error` an honest sink returns
> (durable_guard_test §13).
>
> Teardown is unconditional on WHY the pump settled, and its order is
> load-bearing: `unwatch` first, so no NEW watch callback can be dispatched
> after the stream closes. An already-in-flight delivery is not joined: one
> that read the registration before `unwatch` removed it can still push
> against the closed stream, where the push rejects and irmin's own protect
> logs it - the frame had nowhere to go either way, so the guarantee is "no
> frame is silently minted after teardown", never "no callback runs". Then
> `push None`, and the cleanup BINDS the sender's drain promise: closing
> the stream is what lets the backgrounded `iter_s` finish the queue and
> terminate, and awaiting it is what puts the last ack of a promptly-closed
> session on the wire before the session promise settles and the socket can
> be closed over it - a sender that already died resolved its own promise
> in its catch arm, so the bind never parks on a dead peer. An inner
> `finalize` keeps the close unconditional even when `unwatch` itself
> rejects. The span's guarantee keeps the three-case family form:
> exactly-once (applied, floored, acked), a licensed visible convergent
> duplicate, or the fuel arm's once-ever attempted-with-no-claim floor -
> never a silently consumed-but-unapplied sequence number.
>
> The wording is deliberately narrower than "structurally unreachable": W1
> is closed for the internal race by construction, for any other
> cancellation source by the protected barrier, and for a rejection of the
> span's own body by the release barrier. Whether Dream's own
> WS-disconnect path cancels the promise `live_session` returns was
> source-inspected by the step-17 panel and is **provisionally ruled
> out, not empirically confirmed** - on the
> same footing as the HTTP tier, where the R27 barrier already converts a
> cancellation into a release.
>
> Two more residuals were DECLARED here; step 17 closes the second (D22). **The wedged-peer
> drain**: teardown joins `drained` unconditionally, so a peer that ACKs
> TCP but never reads - a held-open zero window - parks `send_frame`
> forever and the session promise with it; cancellation is not prompt.
> Bounded in practice by the transport erroring the write (the sender's
> catch resolves `drained` on any rejection, and a crashed peer hits the
> TCP retransmission timeout), and left with the socket layer on purpose:
> a grace-bounded pick over the drain would reintroduce exactly the
> send-cancellation s16-a exists to forbid and cut the final ack of
> slow-but-honest peers. **A Duplicate ack riding an attempt that later
> fails**: a `Duplicate` ack issued WHILE an attempt is in flight asserts
> "consumed" on the strength of that attempt. The triple coincidence -
> wrapper cancelled at seq n, the client reconnects and replays n while
> the orphan is still parked (it reads `Duplicate` and is acked without
> applying), and the orphan then REJECTS - releases n only after the
> client has dropped it from its outbox on that ack, so the effect never
> lands and no retransmission is coming. The late release itself is safe
> (conditional on the water still being exactly n); the window is CLOSED
> in step 17: Duplicate acks now PARK on the in-flight attempt (D22).
>
> `test/cancel_test` pins all of it (s1-s7, 41 checks). The deterministic W1
> reproduction arms the transport's kill gate BEFORE the session opens, so
> the sender parks mid-send on the session's own Hello frame; `?interpose`
> then wakes the gate from inside the take-to-ack window, the parked send
> rejects, and the sender dies exactly where W1 lived - the checks read that
> the edit still landed exactly once, the floor persisted before the session
> settled, and the reconnect replay was acked with the edit already applied.
> Seven mutations (s16-a..e: the original pick race restored, `protected`
> dropped to `Fun.id`, `protected` weakened to `no_cancel`, the `died`
> signal never fired, the fuel arm's persist dropped; s16-f: the rejection
> barrier's release dropped, killed by s3's reconnect ladder reading a
> Duplicate-acked-without-apply; and round 3's s16-g: the round-2
> `Canceled` arm reintroduced inside the inner catch, killed by s7's
> rejected-orphan ladder - the released seq must replay as Fresh and apply
> exactly once, where the mutant leaves it consumed and the replay is
> Duplicate-acked with nothing applied; s6's parked-orphan ladder guards
> the converse direction, a release firing while the orphan still owns the
> seq), every one red. Two seams are excluded by
> declaration rather than given a fake killer: dropping teardown's
> `push None` merely parks the ended session's sender forever on a
> never-closed stream, and dropping `unwatch` is swallowed at the same
> seam, because the closed stream rejects the orphan callback's push and
> irmin logs it - each a defect with no observable at the public seam,
> named in cancel_test s3 so its check is not mistaken for coverage.

> **D22 - duplicate-ack parking: a replay never acks over an in-flight
> attempt (roadmap step 17; closes D21's declared F5' window).** The
> `Ack_park` registry holds one open row per (replica, tab, seq) -
> nested total maps, each row a settlement promise minted by
> `Lwt.wait`. The Fresh arm registers the row synchronously at the
> verdict, in the same pre-yield head that consumes the seq, so no
> replay can observe a consumed seq without a row. A second `register`
> over a standing row is reachable - the replay guard's tab-LRU
> eviction can re-open a consumed seq while its first attempt is still
> in flight (an eviction is licensed to duplicate, never to lose) - and
> SUPERSEDES it: the new row is installed first, the old row's waiters
> then wake `Released`, drop their ack, and their clients'
> retransmissions park on the new attempt's row. `settle` is
> handle-gated - `register` returns the row it opened, and a settle
> reaches only that row - so a superseded attempt's late settle never
> cross-wires its verdict onto the successor's waiters. Both mutators
> write the map strictly before they wake: `wakeup_later` runs its
> waiters inline at callback depth zero, a woken continuation may
> re-enter the registry, and a write staged from a pre-wake snapshot
> would clobber what it did. The span settles the
> row at its three exits, and only there: `Landed` strictly AFTER
> `persist_taken` resolves - step_ws succeeding locally is not enough
> (mutant s17-g) - `Released` at the fuel arm's bottom, and `Released`
> in the catch after the release barrier reopens the seq. The Duplicate
> arm asks `find` for an open row under the acked number and either
> acks immediately (no row: the attempt already settled) or parks on
> the row's settlement through `Lwt.protected`: `Landed` fires the ack
> on the duplicate's own socket; `Released` sends NOTHING - the client
> still holds the message, its retransmission reads `Fresh` and is the
> retry the release licensed. The park blocks the pump exactly as the
> Fresh span blocks it, is raced against the sender's death exactly as
> the pump's own frame-wait is (through a once-minted death arm behind
> the same deterministic state pre-check: a settlement that never
> arrives cannot hold `finalize`'s teardown hostage), and a stream end
> is observed between frames - the parked session's teardown is bounded
> by the attempt and by the socket's own death, whichever comes first.
>
> Acks are CUMULATIVE: `Duplicate` carries the high water, not the
> replayed seq, so a below-water replay parks on the WATER's open row -
> the ack it withholds is the water's own claim. `Lwt.wait` (not
> `Lwt.task`) keeps the shared settlement uncancelable through any
> single waiter: a cancelled parked socket dies alone and its sibling
> still resolves (s13.2). The step-17 sweep pinned the wiring with 20
> mutants, 19 killed each on its predicted checks - four pin the
> adversarial-pass fixes: supersession's `Released` wake, the handle
> gate, write-before-wake, and the park's death arm; s17-e
> (`Lwt.wait` -> `Lwt.task`) is the declared survivor - masked by the
> `Lwt.protected` wrap, and proven load-bearing by the joint s17-ef
> mutant, which drops the wrap too and dies on s13.2. The RPC tier
> needs no parking: the reply cache serializes the window, and T22
> pins the duplicate-inside-a-failing-window case (503, no commit,
> Fresh retry, exactly once). The spec's s17-i (clear the
> `Reply_cache` `Pending` marker in the RPC exn arm) is adjudicated
> INERT: the common-path release unconditionally re-opens the seq, so
> the standing marker has no reachable observable; the distinguishing
> interleave belongs to the pending_grace family the step scoped out.
>
> One residual is DECLARED in the window's place (R10f): cross-socket
> out-of-order landing under cumulative acks. Socket A holds seq 1 in
> a gated persist; socket B lands seq 2 and raises the water; a replay
> now acks water 2 immediately - the water's own row is closed - while
> seq 1 is still unlanded, and the water-keyed `find` never consults
> older open rows. Pinned by `test/cancel_test` s6 (re-cut with a
> positive parked witness) and s8-s14 (s14: a dead sender releases a
> parked socket while the settlement stays open), `test/ack_park_test`
> (pure registry: rows, prune, multi-waiter wake, supersession, the
> stale-handle no-op, depth-zero reentrancy), and `rpc_window_test`
> T22; suite baseline 1069 native checks across 44 executables.

> **D23 - store-identity binding: a guard journal knows which store earned
> its floors (roadmap step 18; closes R20a for independent stores, declares
> R20b in its place).** The shape: a create-once 128-bit token, 32
> lowercase hex characters (`Prim.Store_identity`), minted once into
> `<root>/tea.identity` by `Store_pack.resolve_identity` and stamped as
> each guard journal's own first frame, tag `'\004'`. Inside the root and
> not a fourth `<root>.identity` sibling by `prim.mli:322-326`'s own rule:
> a witness that must be written is a fourth thing that can be restored
> out of step, R20 recursed one level, and only a file inside the tree
> travels with the `cp -r`/tar/rsync/snapshot that creates R20 in the
> first place. That placement is now evidence rather than the bet
> `tea_server_pack.ml:283-287` declined in step 12:
> `Layout.Classification.Upper.v` is a closed match on the `store.*`
> scheme whose fallthrough is `` `Unknown``, `file_manager.ml`'s post-GC
> cleanup never removes an `` `Unknown`` entry, and `open_rw`/`open_ro` do
> not scan the directory at all, so the `tea.` prefix defends against the
> SCHEME, not against today's filename list. The journal side is an
> in-band header and not a fifth file because the claim and the floors it
> authorises are the same bytes and the same `compact` tmp-then-rename
> generation: no single deletion, no partial write and no failed stamp can
> leave floors standing without their binding, which a
> `<root>.guard/identity` file could. It also makes the two channels
> identical by construction - each journal reads and writes only its own
> header, so the websocket stamp is physically incapable of satisfying the
> rpc check, and the rpc floors are the dangerous ones.
> `Guard_file.open_ ~identity` reports one of five outcomes, and the two
> absences are asymmetric - the structural twin of `Store_water.bottom`:
> an absent journal claim is no claim, adopted on trust and counted
> (`Adopted_unbound`), because treating it as a mismatch would wipe every
> floor on every upgrade; a decoded DIFFERENT claim is `Rebound`, floors
> cleared outright and the journal re-bound in the same open (step 13's
> own immediate-compaction argument, one layer up); a store whose own
> token cannot be established is `Unresolved_cleared`, floors cleared but
> the journal HELD byte-for-byte - no compaction, no re-stamp, no append
> persisted - so a boot that can read the token again keeps what it
> earned. The drop-everything arm is reserved for a DECIDED difference,
> and the review round below sharpened where that line runs: a header
> frame that unframes cleanly but whose payload is not a valid token
> cannot be this store's token, so it is a decided mismatch and lands
> `Rebound`; head bytes that fail unframing on a NON-empty journal are
> undecidable and land `Adopted_unbound 0`, audible; `Freshly_bound` is
> reserved for the exactly zero-byte file, the only silent arm. The
> one-line implementation that would be a NEW silent loss and is therefore
> forbidden: a mismatch must clear the decoded events BEFORE
> `Floors.of_events`, never `filter ~head:(fun _ -> None)`, because that
> filter keeps a `Store_water.bottom` floor unconditionally before it
> consults any head (`durable_guard.ml:125-130`), so a stranger's legacy,
> no-op and fuel-exhausted records would survive and go on judging replays
> Duplicate; pinned by s18-a, the most important mutant of the sweep.
> Declared non-goals, each with its reason: no binding artifact and no MAC
> for `<root>.secret` - a wrong secret already fails AEAD authentication
> and yields a FRESH session, strictly safer than a duplicate, and the
> secret is a restorable sibling whose rotation would become a
> mass-duplicate event; no strict refuse-boot flag - its failure mode is a
> refuse-loop on the routine restore, clearable only by deleting the
> artifact the operator is trying to protect, against
> `tea_server_pack.ml:286-287`'s standing line; no lock, and no part of
> this argument leans on one (R18). The sweep, from the measurement: 12
> mutants, s18-a through s18-l, 12 of 12 killed on their predicted checks,
> zero survivors, every kill a genuine test failure on a clean build - no
> build-error and no timeout pseudo-kills. One prediction missed sideways:
> s18-f's expected I7 collateral never fired, because `link`'s EEXIST arm
> makes an unconditional mint converge to adoption; S3's origin assertion
> killed it outright, so no gap. The adversarial review round (seven shard
> finders plus one adversarial correctness pass, every finding
> independently re-verified against the live code) confirmed five
> findings, all fixed and each pinned by its own kill-checked test: the
> strict hold held only the open path - an append past `4 * cap` could
> compact a HELD journal and restamp this store's floors under the
> stranger's token, silent loss on the return restore - fixed by a `held`
> field that disarms every write path, pinned by I11's byte-for-byte
> journal identity across thirty appends; a CRC-valid header whose payload
> decodes to no token discarded its frame boundary and erased intact
> floors as a silent `Freshly_bound` - fixed by the `Foreign_header` state
> keeping the consumed offset, pinned by I12; a corrupt first frame on a
> non-empty journal bucketed as "empty" - fixed as above, pinned by I13;
> boot-log lines that promised a binding an Unresolved boot never
> performed - fixed by the pure `explain_outcome` and the conditional
> `explain_identity` wording, pinned by `identity_explain_test`; and an
> adopt-race readback failure mislabelled `Absent_unmintable` where the
> honest origin is `Unreadable` - fixed by forwarding the readback's own
> origin, the arm a two-process race window, declared untested. Suite
> after the round: 1178 native checks across 47 executables.

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
envelope - for requests and `Read_only` responses. Since step 15 (D20) a
`Mutating` response is *always* the two-arm `keyed_resp` Repr variant
(`Reply of 'resp | Replayed`), keyed or not: static-by-kind rather than
dynamic-by-key, because `expect` is fixed at `call` time and cannot see
headers, so a header-dependent response shape would be undecodable by
construction. That is a breaking change to the public 200-channel contract
for ALL callers, permanently and intentionally - curl and server-to-server
consumers now decode `keyed_resp`, not bare `'resp`, a named non-browser
break and not merely a stale-browser skew window - paid once, at the
compile-driven boundary both tiers link. The HTTP status is exclusively the
transport-error channel (404 route-miss, 415 content-type gate, 413 size cap
at 64 KiB post-read, 400 decode refusal; step 15 adds two 400 reasons - a
malformed delivery key, a delivery gap - and one framework 503 for the
Pending window, each adjudicated transport, not app outcome, so the law
stands); app-level fallibility is declared as `'resp = ('ok,'err)
result` inside the GADT and rides the 200 channel through `resp_t`. D20's
"never recomputed at replay time" clause makes that declaration
load-bearing: a fuel-exhausted reply is a state-shaped value computed once
at take time, so the applied-versus-attempted distinction belongs in a
result-shaped `'resp` - the fallible-endpoint residual, still open (§7 D20).

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
story), and it is the first path that puts *different* clients on one branch,
which is what made R10 a real exposure rather than a theoretical one. Since step
14 (D19) `Server.step` reads through a witness token and commits by
compare-and-set, so two concurrent `Append_tag` calls reconcile instead of
erasing each other, and the app-level `Lwt_mutex` that used to stand in for that
is gone.
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

**Delivery keys (D20, step 15).** A `Mutating` request may carry
`x-tea-key: <32 lowercase hex tab> ":" <positive decimal seq>`
(`Tea_rpc.Key`; the header literal is pinned by T20; the seq parse accepts
exactly the canonical print of the value it returns, so aliased spellings
of one position - `"007"`, `"+7"`, `" 7"` - are closed by construction). Absent, the request
keeps today's semantics: at-most-once effect per HTTP exchange, no dedup,
the legacy/curl arm. Present but malformed, it is a 400 with nothing
consumed (T9). Present and well-formed, the call is de-duplicated through
the same guard family as the WS tier - the machinery, the floor-tab
derivation and the guarantee sentence are §7 D20. The session cookie is
thereby a *semantic input* to a keyed call: the dedup namespace derives
from it, which is what makes a forged key land in the forger's own
namespace (D20.1) - and what the native tests must thread `Set-Cookie` for
explicitly. Legacy story, in two facts: the journal codec is untouched -
RPC floors are ordinary tag-`'\003'` `Advance` records, a step-14 root
opens under step 15 with the RPC journal simply absent (created on the
first keyed take), a step-15 root opens under a step-14 binary that never
looks at `.guard/rpc`, and nothing migrates (T10 pins both directions); and
a cached pre-15 client bundle decoding a `Mutating` response fails as a
loud typed `Decode` error, not a silent misread, because both tiers link
one `Api` definition (T15).

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
  holding under the app's own `Merge_spec.t`, and unacknowledged edits wait in
  `Tea_client.Delivery` and replay in order once it is back — which since step 10
  (D15) means *every* edit the server has not confirmed, not just the ones born
  while the link was down. Under `Crdt_join`
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
- **R10 (med) - `step` was a read-modify-write with no compare-and-set, and D12
  put different clients on one branch. CLOSED in step 14 (D19).** `Store.commit`
  was a plain `set` (last write wins), so `load` -> `Loop.step` -> `commit`
  could interleave: two writers each read the pre-edit model and the second
  erased the first. This was harmless while every write path was *per-session*
  (the only racer was the same user); the `Mutating` RPC endpoint was the first
  path where different clients read-modify-write the **same** branch, which is
  what promoted it from theoretical to a real exposure. `append_commit`'s
  test-and-set did not fix it either: the retry re-committed the *same stale
  model*, so it preserved history, not content.

  D19 moves the whole read-modify-write onto a compare-and-set keyed to a
  **load-time witness** (`Store_core.load_based` / `commit_based`) and resolves
  contention with the app's own `Merge_spec.t` rather than by re-running
  anything, so no effect fires twice and no CRDT dot is minted twice. The
  `Lwt_mutex` mitigation in `shared_doc_serve` is retired; what replaces it is
  not another check at the example tier but a type: `step_with` takes a token,
  so nothing routed through the seam can reach the last-write-wins door. The
  property is pinned at the framework tier by `test/contention_test` C1-C15,
  S1-S2, and the interleave is deterministic because it is **program order**,
  not timing - which is exactly why the old mutex was mutation-invisible (under
  `Irmin_mem` with an `Add_tag` that emits `Cmd.none` there is no Lwt yield
  between load and commit, so deleting the lock left `test/csrf_test` green,
  verified by mutation). Residuals below carry what D19 does *not* close.
- **R10b (low) - unguarded head moves erase acked commits.** `undo`, `redo` and
  `fork` still move the head with an unconditional `S.Head.set`, so a commit the
  pump has already floored and acked can be erased by a racing undo. The fix is
  mechanically small and semantically large (it forces a decision about what a
  racing undo *means*: refuse, merge, or win positionally) and under `Crdt_join`
  a later reconcile can rejoin undone content anyway, so it deserves its own
  record rather than a line in D19. `fork` is the mildest of the three: it
  writes only inside a branch it has just observed to be absent. Pinned as
  behaviour by C15, a labelled characterization check, so a future fix turns it
  red on purpose rather than by surprise.
- **R10c (low) - the non-CRDT arms keep content only for the writer that was
  already acked.** For a `Three_way` app whose merge returns `Error`, and for a
  `Last_write_wins` app, D19 keeps `ours` and demotes `theirs` to history.
  `ours` has to win: the message being committed is one the pump has already
  taken and is about to ack, and the D16 contract is that an acked effect exists
  in the store. R10 is therefore closed *for content* only under `Crdt_join` and
  a successful `Three_way`. It is still strictly better than before, where the
  loser was erased *and* unreachable: the loser is now the committed parent, and
  the conflict reason is durable in the commit message rather than on a stderr
  line that dies with the process. Pinned by C8 and C9.
- **R10d (CLOSED in step 16, D21) - a cancelled pump promise abandons a taken
  sequence number.** `live_session` ended in `Lwt.pick [...; pump () ]`, so a
  dying socket cancelled the pump and anything it awaited, while the `Cell` had
  already consumed the sequence number into a guard that outlives the socket:
  the reconnect's replay read `Duplicate` and was acked without applying -
  window W1, silent loss, live at every await inside the step. Step 16 closes
  W1 at every door, and the wording is deliberately narrower than "structurally
  unreachable". The internal sender-vs-pump race is closed **by construction**:
  the per-frame body now lives in `handle_frame`, never an element of any
  `Lwt.pick`/`Lwt.choose` list, and sender death is a `Lwt.wait`-based `died`
  signal raced against the next frame through `Lwt.choose`, which never cancels
  the losing branch. Any other cancellation source reaching the take-to-ack
  span is stopped **by the `Lwt.protected` barrier**: the span runs to natural
  completion as an orphan, no ack is minted for it, and a torn window lands in
  W2's already-licensed visible-duplicate shape - never in W1. And a rejection
  of the span's own body is compensated **by the R27-mirror release barrier**,
  seated INSIDE the protected body (round 3): `protected` never rejects its
  body, so every exception the catch sees is body-originated and the release
  is unconditionally correct - the still-running orphan is protected by
  position (the wrapper's cancel never reaches the catch), not by a match on
  `Canceled`. What remains
  open: whether Dream's own WS-disconnect path cancels the promise
  `live_session` returns - source-inspected by the step-17 panel,
  provisionally ruled out, not empirically confirmed - on the same footing
  as the HTTP-tier residual, where the R27 barrier already converts a
  cancellation into a release; plus the wedged-peer drain (§7 D21). The
  Duplicate-ack-riding-a-failing-attempt window is CLOSED in step 17 by
  per-key parking (§7 D22), and R10f is declared in its place.
  Pinned by `test/cancel_test` s1-s14, seven s16 mutations, and the
  step-17 roster (§7 D21/D22).
- **R10e (low) - per-socket liveness under sustained fan-in.** A denied round
  certifies that the *system* progressed, not that this socket will. The
  reconcile loop is unbounded on purpose (every loss-free exhaustion arm is
  either the loop continued or a plain set, and a plain set is R10), so the
  instruments are `committed.rounds`, asserted by C5, and one diagnostic line at
  eight rounds. A lost round also leaves an unreferenced commit and tree, and
  this repo has no GC wired; `commit_coalesced` already orphans commits, and D19
  makes the rate proportional to contention.
- **R10f (low, declared in step 17, D22) - cross-socket out-of-order landing
  under cumulative acks.** `Duplicate` carries the high water, so a replay
  arriving after a younger socket lands seq n+1 is acked with water n+1 even
  while an older socket still holds seq n un-landed in a gated persist: the
  water-keyed `find` sees the water's row settled and never consults older
  open rows. The ack claims the water, and the D16 contract reads per-seq.
  Closing it needs the Duplicate arm to park on the OLDEST open row at or
  below the water - a scan the current per-key `find` deliberately does not
  do. Declared, not closed; the single-socket window D22 closes is the one
  step 17 targeted.
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
- **R11 (med) - a hard kill can silently lose the unacknowledged in-flight
  tail.** Roadmap step 11 (D16, §7) makes the replay-guard floor durable, and
  the design rests on one inequality: a floor must not be more durable than
  the commit whose effect it records. An orderly SIGINT/SIGTERM teardown
  preserves it (store closed before journal); `kill -9` violates it, because
  the journal reaches the page cache per record while irmin-pack buffers
  commits in user space. A surviving floor over a dead commit makes the replay
  read `Duplicate` against an effect that is gone. **Narrowed by step 13
  (D18):** such a floor carries a water its restored branch head no longer
  covers, so the next boot drops it and the message replays as a visible
  duplicate. What remains on the loss side is tabs that never minted a
  witness (every record reads `bottom`: no-op or fuel-exhausted takes
  before any real commit, legacy records; the fold never lets a later
  bottom drown an elder stamp) and divergence after the boot check; the
  loss stays bounded by the
  pack's auto-flush lag. Every other durable-layer failure (torn tail, cap
  eviction, failed append) falls on the duplicate side.
- **R12 (low) - `Durable_guard`'s in-process floor mirror is unbounded.
  CLOSED in step 15 (D20.4, the ride-along).** The `Replay_guard.Cell` in
  front of it is bounded precisely because an attacker mints session cookies
  freely, and `Guard_file`'s cap bounds the journal's copy of the floors; the
  mirror between them had neither defense, so a hostile cookie-minting client
  grew it without limit. `Durable_guard.v ?mirror` now caps it with LRU
  eviction over a monotone recency tick (touched on persist and on a
  Cell-miss seed hit, deliberately not on a Cell hit), and the default is a
  documented function of the bounds in force - `max (4 * sessions * tabs)
  journal_cap` - never a constant that can drift from the tables it fronts.
  Eviction carries the same direction argument the Cell already carries: it
  REMOVES a floor, never lowers one, so absence lands on the
  accept/duplicate side; and it is the stack's only self-healing eviction,
  because the journal record survives its own cap and the evicted floor
  returns at the next boot. Pinned by T11 (LRU order, recency, the boot
  heal), T16 (eviction, replay and reopen cohere to the true floor) and T17
  (the composed mirror satisfies the wiring rule, and `default_mirror`
  computes the stated formula).
- **R13 (med) - a durable session cookie is an unrevocable bearer credential
  with a sliding lifetime.** Step 12 replaces `Dream.memory_sessions` with
  `cookie_sessions` + `set_secret` on the pack tier, which removes an ACCIDENTAL
  safety property: a restart used to invalidate every session. It does not any
  more. The whole session travels in an AEAD cookie, Dream refreshes it (id
  preserved) once it is half-expired, so a stolen cookie exercised at least once
  per half-lifetime never expires, and `cookie_sessions` has NO server-side
  revocation - `Dream.invalidate_session` re-issues client-side only, and a copy
  of the old cookie stays valid until its own `expires_at`. The levers, all
  coarse, are: rotate the secret (invalidates every session at once), shorten
  the lifetime (threaded as `?lifetime` on `Session_secret.durable` and
  defaulted to 7 days, half of Dream's default), or an app-level deny-list keyed
  on `Dream.session_id`. Accepted rather than fixed because ocaml-tea has no
  user identity to revoke TO: the session IS the principal and the branch it
  names IS its data. Any app adding authentication must add revocation and must
  call `Dream.invalidate_session` at the auth boundary - there is no
  session-fixation defence today, and the pre-auth branch's fate is that app's
  question to answer.
- **R14 (med) - the session secret is the system's only credential, and the id
  space it protects is public.** Anything that reads the secret can mint a
  cookie for ANY session id, and therefore read and write any session branch
  (nothing downstream of `Dream.session_id` authorizes: the id IS the
  capability), and can mint CSRF tokens under the same key. It needs no search:
  branch names under `$TEA_ROOT` are literally `session-<hex>` and the
  world-readable `$TEA_ROOT.guard/journal` records the replica string in every
  `Advance`/`Forget`, so a local read enumerates every live id. Step 15's
  ledger correction: the RPC journal additionally records `(main, floor_tab)`
  entries, and the floor tab is a DERIVATION - a session-salted digest of the
  client tab (D20.1) - not a client input, so the id disclosure this entry
  documents confers no floor authority: forging a victim's floor key needs
  the victim's cookie, the WS tier's own precondition. (That sentence
  replaces the design draft's refuted "leaks no new authority" audit claim
  with the reason it is now true; R23 records the unkeyed-`Digest` honesty
  beside it.) The session id's 144 bits defend against guessing only. Consequences, all implemented: the
  secret file is `0600`, created `O_EXCL`, refused if it is not a regular file,
  if any group or other permission bit is set, or if it exceeds 4096 bytes, and
  it is NEVER rewritten on a validation failure (an unreadable secret may be a
  transient mount problem; overwriting it would permanently orphan every
  existing branch). Honest limits: `lstat`-then-`open` is a TOCTOU because
  OCaml's `Unix` has no `O_NOFOLLOW`, but winning that race requires write
  access to the DIRECTORY holding `<root>.secret`, and anyone with that access
  can already unlink the file and drop in a secret of their own choosing, which
  the `0600` and regular-file checks would then happily accept. The real
  security boundary is therefore ownership of the secret's directory, not the
  file checks; the checks defend only a directory that is already trustworthy.
  The race is strictly weaker than the precondition it lives under, which is
  why it is documented rather than closed with an inode recheck. A process
  running as root can still be pointed at a file an unprivileged user owns (no
  `st_uid` check); and an
  environment variable is a weaker home than a file (`/proc/<pid>/environ`,
  `ps e`, container inspection, crash dumps, inheritance by every child), which
  is why `TEA_SECRET` is an override and `<root>.secret` is the default.
- **R15 (low) - amends R12.** Step 12 leaves the in-process floor mirror's
  growth RATE unchanged (it is driven by accepted messages, not by minted
  cookies), but it changes what cap eviction COSTS. `Guard_file`'s drop-oldest
  victims used to be ids no client could ever return to, because a restart
  rotated identity; they are now LIVE sessions, so a flood of distinct
  `(replica, tab)` pairs is a way to force an honest client's replay back onto
  the duplicate side across a restart. Still the accept/duplicate direction,
  never loss, but now reachable on purpose. In the other direction step 12
  removes an unbounded table this register never named on the pack tier:
  `memory_sessions` retains a hashtable entry per minted session and sweeps it
  only on explicit invalidation or on presenting an already-expired id.
  Amended by step 15 (D20.3): the RPC journal's own drop-oldest cap pushes
  RPC clients to the duplicate side across a restart, the same direction, on
  its own file - `<root>.guard/rpc` - rather than the shared WS journal. The
  in-process-mirror clause this entry amended is closed with R12; the
  journal-cap direction is what R15 keeps naming.
- **R16 (low today, high on first use) - the cookie session payload is
  rollback-able.** `cookie_sessions` stores the whole session client-side; AEAD
  proves the server issued SOME version, never that it issued the LATEST, and
  there is no server-side copy to compare against. ocaml-tea reads only
  `Dream.session_id`, and the id is identical across every version a client may
  replay, so nothing is exposed today - that safety is an accident of non-use
  (`session_field`, `set_session_field`, and `session_expires_at` have zero
  callers in this repo). The day an app calls `Dream.set_session_field`,
  anything whose monotonicity matters must NOT go there: roles and entitlements
  (rollback restores a revoked privilege), consumed one-time tokens (rollback is
  replay), counters and quotas (rollback is a reset), consent flags. Monotone
  state belongs on the session's Irmin branch, which is server-held and
  versioned by construction - that is the branch's whole purpose.
- **R17 (low) - Dream's cookie session loader has two raises we deliberately do
  not catch.** `Cookie.load` calls `Yojson.Basic.from_string` unguarded and
  `failwith "Bad payload"` on a non-string payload value (Dream's own source
  marks both TODO). Both are reachable ONLY after a successful authenticated
  decrypt, i.e. only if a future Dream changes the payload schema under our own
  secret; a tampered or foreign cookie is rejected by AEAD first and degrades
  silently to a fresh pre-session. We do not wrap the session middleware in
  `Lwt.catch`, because the wrapper would necessarily also enclose the
  application handler and would replace Dream's own logging error handler with a
  bare 500 - a diagnostics regression paid for a schema-change hazard. Accepted;
  the mitigation is pinning the Dream version.
- **R18 (low) - exactly one process may own a pack root.** irmin-pack 3.11 takes
  no inter-process lock on the root and `Guard_file` takes none on the journal,
  so a second writer corrupts the store and journal compaction (a rename over
  the path) silently discards the other process's appended floors. This was
  previously self-limiting: a second process minted different session ids, so no
  client crossed over. A shared secret makes both processes mutually
  intelligible to the same client, which is exactly what lets a load balancer,
  or a systemd restart overlapping a still-draining process, route one client
  into both. A load-balanced pair over one root is a data-loss configuration,
  not a scaling one. A lock file is out of scope for this step. Step 18's
  store-identity token does not change this and must not be read as
  touching it: two processes over one root resolve the SAME
  `<root>/tea.identity`, so both match, both serve, and the token sees
  neither the second writer nor a root swapped under a running server - it
  is read once, at boot. No part of the step-18 design leans on a lock,
  and `test/pack_root_test.ml`'s stale comment claiming one was fixed in
  that step.
- **R19 (med) - a durable session cookie behind a TLS-terminating proxy is
  emitted without `Secure` and without the `__Host-` prefix.**
  `Dream.cookie_sessions` marks the cookie `Secure` only when Dream itself
  terminates TLS. Behind a reverse proxy that terminates TLS - a very common
  deployment - Dream sees plain http, so the cookie goes out unmarked, and any
  plaintext request to the same host discloses it. Step 12 raises the stakes
  rather than creating the gap: an intercepted cookie used to die at the next
  restart, and now it does not, so the exposure window is no longer bounded by
  process lifetime. The framework cannot tell the two deployments apart from
  inside the handler. The mitigations are deployment-side: terminate TLS in
  Dream, or make the proxy refuse http. Documented at the `serve_pack` call
  site in `lib/tea_server_pack/tea_server_pack.mli`; the register entry exists
  so the deployment posture is a named decision rather than a doc-comment
  aside.
- **R20 (med) - the three durability siblings can be restored out of step, and
  step 12 makes that silent loss.** `<root>`, `<root>.guard` and
  `<root>.secret` are three separate paths with no cross-binding, so a backup,
  a restore, or a manual wipe can easily keep some and drop others. The
  dangerous direction is losing the pack store while keeping the other two:
  returning tabs present cookies that `<root>.secret` still decrypts, so
  `Dream.session_id` yields the same id, hence the same branch name and the
  same CRDT replica id, hence the same guard key. The branch is gone, so the
  model materialises at `bottom`, but `<root>.guard` still holds that session's
  high-water floor, so the next replayed message is judged `Duplicate` and
  dropped onto an empty model. That is the loss side of the guard, reached with
  no reaper involved, and it inverts the standing "degradation is duplicate,
  never loss" direction. Before step 12 the same wipe was harmless because
  identity was per-process: a returning tab got a fresh id, a fresh floor key,
  and `Fresh`. Implemented now: `serve_pack` refuses to boot, loudly and with a
  non-zero exit, when the pack root does not exist but `<root>.guard` does,
  which covers the outright-wipe case. The rollback direction - an OLDER pack
  snapshot restored under a NEWER journal, where the root exists and the boot
  refusal cannot fire - is closed per-floor in step 13 (D18): every `Advance`
  record carries the head water its own session branch stood at when the
  floor was taken (`Prim.Store_water`, the branch head's `Info` date,
  strictly increasing per branch by the step-6 clock), and `Guard_file.open_`
  drops every floor the live head no longer covers, so the replay is
  re-admitted as a visible duplicate. A create-time identity token could not
  have closed this - it is constant over the store's whole life, so an older
  snapshot of the SAME store carries it and it matches; only a monotone,
  per-floor witness can see a branch that went backwards. **R20a, the
  identity residue - CLOSED for independent stores in step 18 (D23),
  narrowed to R20b.** A journal restored beside a DIFFERENT store whose
  same-named branch happens to carry a newer head date passed the check,
  because order, not identity, is what a water can answer. The blast
  radius was larger than this entry first stated and it was concentrated
  on the RPC channel, not the websocket one: ws floors are keyed by
  session replica, so an unrelated store has no `session-<hex>` branch and
  the floors already fell out as `dropped_no_branch`; but every RPC floor
  is keyed on `main` (`floor_replica:Store.main_replica`), every pack
  store has a `main`, and `Store_water` is an approximate wall-clock date,
  so an RPC journal restored beside ANY store written to more recently
  kept ALL its floors and judged every replayed keyed call Duplicate -
  silent loss with no lineage coincidence and no 144-bit collision
  required. Step 18 closes that: `<root>/tea.identity` is minted once
  inside the pack root, each journal carries it as its first frame, and a
  cleanly decoded mismatch drops every floor in that journal and re-binds
  it, so the replays land Fresh as visible duplicates. What a create-time
  token still cannot see is a divergent COPY of one lineage - the same
  argument this entry already makes against the rollback case applies
  here too: a clone, a restored-then-advanced backup, or a staging copy
  carries the token unchanged, so the original's journal beside the copy
  still reads as bound and the loss stands. That is **R20b**. Two smaller
  exposures remain named rather than closed: a first boot after the
  step-18 upgrade adopts a headerless journal's floors on trust (counted
  `Adopted_unbound`, one warning line, and the journal is bound in that
  same boot so the window is exactly one boot), and a downgrade to a
  pre-step-18 binary meets tag `4` at byte 0, reads `Bad_tag`, and keeps
  zero frames - the duplicate side, the same sanctioned downgrade
  direction step 13's tag `3` already established, but wider.
- **R20b (med) - a create-time token cannot distinguish a store from its
  own copy.** A `cp -r`, a filesystem snapshot, a restored-then-advanced
  backup, and a staging clone all carry `<root>/tea.identity` unchanged,
  so a journal from the original placed beside the copy decodes a
  MATCHING header and keeps every floor - and because the copy has
  diverged, those floors can stand over effects the copy does not hold.
  This is the same structural fact D23's own placement argument depends
  on (the token travels with the bytes, which is what makes a partial
  restore detectable) read in the other direction, and it is not a defect
  in the token so much as the boundary of what a value constant over a
  store's life can answer. Also unchanged: an operator who deliberately
  copies the token between roots, or hand-mixes `store.*` from one root
  with `tea.identity` from another, gets a matching read - the token is
  exactly as reliable as the root being restored as a UNIT, and it is
  deliberately not MAC'd against `<root>.secret`, which is itself a
  restorable sibling whose rotation would then clear every floor. The
  successor is a boot EPOCH: a counter bumped in the root at each open
  and echoed into each journal, which MOVES on divergence and would see
  the copy. Not shipped in step 18 - it is a larger change and its own
  failure mode (a journal whose stamp lagged a boot false-positives into
  a floor wipe) is accept-side but noisy, so it is named here rather than
  half-built.
- **R21 (low) - delivery dedup is not intent dedup.** Repeated user intents
  (a double-click) are distinct calls, distinct seqs, and apply twice,
  visibly. App-level intent coalescing is out of scope because it would
  reintroduce the app-visible key surface D20 closed, or fight the dense
  one-in-flight numbering.
- **R22 (low) - no framework path back to a reply lost to restart or
  eviction.** On `Applied_reply_lost` the app re-reads through a `Read_only`
  sibling of its own choosing, and that read races later writers, so it is
  not "the reply your call earned". An opt-in declared read-only sibling is
  a coherent future step, not designed here.
- **R23 (low) - the floor-tab derivation uses an unkeyed stdlib `Digest`
  (MD5; the `session_secret` staging precedent).** Only preimage and
  second-preimage resistance are relied on, and the attacker cannot choose
  the session half of the input, so collision weakness does not apply;
  upgrade to a secret-keyed derivation if the session secret is ever plumbed
  to the RPC route. R14's step-15 ledger correction points here.
- **R24 (low, latent) - `Cmd.http_keyed` is public**, so an app can enable
  the runtime's automatic retry against a POST path with no server-side
  guard, turning at-most-once into at-least-once against an unguarded
  endpoint. The doc comment carries the precondition; revisit if apps use
  it.
- **R25 (low, latent) - the new `Cmd.Http` `delivery` field is a breaking
  change** for any future third-party interpreter contract; this step is
  where that debt was taken knowingly.
- **R26 (low) - multi-canonical-branch RPC apps need a `floor_replica` per
  route group.** The journal, the boot filter and `Floors` already handle
  arbitrary replicas, so the extension is wiring, not redesign.
- **R27 (CLOSED in step 15) - a keyed handler that REJECTS after its take.**
  Found by the step's adversarial review pass: the Fresh arm consumed the
  seq before the apply, so a handler rejection left the take standing and
  every retry read Duplicate against a reply that was never settled - a
  wedged key, silent loss. Closed in the same change by the
  barrier-plus-release pair: `Lwt.try_bind` around the keyed apply, and a
  `release` un-take (`Replay_guard.release` through
  `Durable_guard.release`, Cell-only because the failure arrives strictly
  before the persist) conditional on the high water still being exactly
  the taken seq. The Pending marker is left standing - dropping it would
  hand a racing duplicate `Gone`, the exact spurious-`Replayed` lie D20.2
  forbids - and the response is 500 for the client's 5xx retry arm. A
  half-applied handler that rejects re-applies on the retry: the visible
  convergent duplicate, the family's licensed degradation direction,
  never the silent drop (T21).

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
   different replica ids. Every in-process test passed, because no in-process
   test ran both applications at once. The smoke test pinned that behaviour as
   an `xfail` so the fix could not land without coming back through it.

9. **D14 - the client as a predictor, not a replica.** **Done.** The server
   announces the replica id it applies a session under (`Wire.Hello`, the
   down-channel's new first frame), the tab adopts it (`Tea_client.Identity`),
   and `Rebase.absorb` - one total function - decides what every down-frame
   does: adopt-and-resync for a `Hello`, rebase for a `Head`. One user intent
   is one replica slot on both tiers, so `join` reconciles by `max` instead of
   summing. `test/predictor_test` is the test the previous eight steps could
   not have written: it runs the client's optimistic apply *and* a real
   `Store` session's apply of the same intent, then folds the pushed head back
   through the app's own subscription handler - the composition only a browser
   could reach before. The D13 pin went stale, as designed, and is now an
   ordinary assertion: the acting tab settles at 1. Six mutations, six reds.
   Fixed in passing, because the phase's own wire check tripped over it:
   `Codec.of_json` raised instead of returning `Error` on a crafted variant
   case (§7).

   Left open, deliberately: the pre-announcement window (an edit made before
   the first frame keeps a provisional slot - it needs an un-mint the CRDTs
   cannot offer, and it is pinned rather than hidden); delivery dedup, which
   still rests on send-once and would need the seq-number acks §7 records as
   deferred (**closed in step 10**); and `Dot` uniqueness across the two tiers,
   which is now a magnitude argument rather than a construction (§7).

10. **D15 - delivery that survives the socket.** **Done.** A msg handed to a
    dying socket used to vanish silently: the outbox held only the msgs *born*
    while the link was down, so nothing replayed it and the next `Hello` resync
    discarded the prediction. Closing that needs a retry, and a retry needs
    de-duplication or it recreates D14's double count - so the two land
    together. `Wire.up` carries a `(tab, seq)` delivery header; `Prim.Tab_id`
    is the identity a session id cannot supply, because one cookie is one
    replica shared by every tab on it; `Tea_client.Delivery` replaces
    `Rebase.Outbox` with one queue whose members leave only on an
    acknowledgement; `Tea_server.Replay_guard` keeps one integer per
    `(replica, tab)` above `A.update`, which is enough because in-order
    delivery makes the consumed set a prefix. The ack is minted by the pump,
    never by the store watch - the watch fires for every writer on the branch,
    and a no-op msg produces no commit to hang an ack on.

    `test/exactly_once_test` is the file the previous nine steps could not
    write: it runs a real store session, a real `live_session`, and the real
    client queue over a transport it **kills and re-opens in the middle**, then
    asserts which of the two asymmetric failures happened - a silent lost edit
    or a visible double count. Fifteen mutations, fifteen reds.

    Left open, deliberately: the guarantee is *exactly-once effect within one
    server process lifetime and within the guard's bounds; at-least-once
    outside them*, and every bound degrades toward duplicating rather than
    losing (§7). The client's unacknowledged queue stays unbounded, which is
    what makes refusing a gap free. The RPC tier is not covered (**closed in
    step 15, D20**). The D14
    pre-announcement window is unchanged, and now pinned in two files.
    The process-lifetime bound itself is what step 11 (D16) narrows.

11. **D16 - the high water made durable.** **Done.** Step 10's guarantee
    stopped at the process boundary; this step carries the floor across it
    without moving the verdict off its synchronous path. `Tea_server.Guard_sink`
    is the seam (an `Advance`/`Forget` event, `null` and `memory` sinks, a
    pure total CRC-32-framed codec); `Tea_server.Durable_guard` layers the
    bounded `Replay_guard.Cell` over a durable `Floors` mirror, seeding a Cell
    miss from the mirror before the verdict, with `take` still synchronous and
    `persist` the only asynchronous write; `Tea_server_pack.Guard_file` is the
    pack tier's append-only journal at `<root>.guard/journal`
    (fold-until-broken read, per-record flush, fsync only on close, cap with
    drop-oldest,
    compaction at 4x cap). The pump persists after the apply attempt and
    before the ack on both the success and the fuel-exhausted arms, because
    the water means "taken", not "applied"; `reap ?forget` writes the
    tombstone before the branch removal. `guard_sink_test`,
    `durable_guard_test`, `guard_file_test`, plus extended
    `exactly_once_test`/`replay_test`/`reaper_test`.

    The guarantee is now three cases, not a sentence: exactly-once effect
    across an *orderly* restart (SIGINT/SIGTERM teardown, store closed before
    journal); at-least-once - a visible, convergent double count - on a torn
    journal tail, a cap eviction, or a failed append, since each loses only a
    floor and an absent floor accepts anything; and `kill -9` explicitly out
    of scope on the *loss* side, because the journal reaches the page cache
    per record while irmin-pack buffers commits in user space, so a floor can
    outlive its commit and the unacknowledged in-flight tail is silently
    lost, bounded by the pack's auto-flush lag (§7 D16, R11).

    Left open, deliberately: the in-process `Floors` mirror is unbounded
    where the Cell and the journal are both capped (R12); wiring `reap` past
    a live durable guard without `Durable_guard.forget` is still the loss
    path `Replay_guard.forget`'s precondition names; the step-9
    two-clocks-behind-one-replica-id note and the D14 pre-announcement window
    are untouched.

12. **Durable session identity** - `Tea_server.Session_secret` (the secret
    newtype with no elimination form, the sealed `memory`/`durable` back-end
    sum, the sealed `set_secret`-outside-`cookie_sessions` composition, and a
    total synchronous resolver over `TEA_SECRET` / `TEA_SECRET_FILE` /
    `<root>.secret` with `O_EXCL` 0600 minting); `?sessions` threaded through
    `handler`, `serve`, `handler_pack`, and `serve_pack`, defaulting to today's
    `memory_sessions` everywhere except `serve_pack`, the one tier whose model
    is itself durable. **Done** (`session_secret_test`, `session_identity_test`,
    browser B4 turned from a pin into a check at 2, new B5 secret converse,
    `mut-b4-fresh-root` and `mut-journal-unwired` unblocked, every new check
    confirmed by mutation). Optional companion landed with it:
    `Store_pack.open_root`, so an unusable `TEA_ROOT` is an audible typed
    failure rather than an uncaught `Pack_error` (`pack_root_test`, browser B6).

13. **The store water** - bind the guard journal to the pack store so an
    out-of-step restore is never *silent* loss (R20, D18). **Done**
    (`Prim.Store_water`, the sealed head-`Info`-date newtype with
    `covers ~head ~floor`; `Store_core.commit`/`commit_coalesced`/
    `append_commit` return the water they minted so the persist hot path
    never reads a head, `head_water`/`branch_waters` read it back fenced;
    the tag-`'\003'` water-stamped `Advance` frame, tag `'\001'` decoding
    at `bottom` forever; the max-witness floor fold, under which a
    witness-less take (no-op, fuel exhaustion) raises the seq but never
    drowns the tab's strongest water; the `Guard_file.open_ ~head_water`
    boot filter with the four-way `Guard_file.verdict` and three
    conditional operator lines, compacting the journal in the same open
    whenever the filter fired, so a drop is durable and cannot un-drop
    when the branch head later rises past the stale water;
    browser B8, three lives over a real `cp` snapshot/restore, where a
    served count of 2 is the guarantee, 1 the R20 silent loss, 3 the
    rollback not taking and 4 neither; `guard_water_test` W1-W6 real
    waters / G1-G8 hand-built floors, C1-C3 in `guard_sink_test`, 12 new
    mutations across both suites).

    Left open, deliberately: R20a - order, not identity, is what a water
    can answer, so a journal restored beside a DIFFERENT store whose
    same-named branch carries a newer head date passes, and the identity
    token stays the named future fix (§10 R20); `<root>.secret` is still
    bound to neither sibling; the check is boot-time only, silent on a
    second writer or a swap under a live process (R18); tabs that never
    minted a witness (every record `bottom`: legacy journals, no-op or
    fuel-exhausted takes before any real commit) stay trust-on-first-use
    forever, bounded by the `unwitnessed` counter's visibility, though a
    bottom record after a real witness no longer de-witnesses the tab;
    and a session that used `undo` has its floors dropped at
    the next restart, a named false positive on the duplicate side.

14. **The witnessed step** - make the TEA step a compare-and-set transaction so
    two writers on one branch cannot erase each other (R10, D19). **Done**
    (`Store_core.based`, the abstract session-bound token minted only by
    `load_based`, which reads the head once and gathers the model *through*
    that commit rather than off the branch, both reads fenced; `based_model`,
    so the model is read off the token and the safe wiring is the SHORT one;
    `committed = { water; model; rounds }`, a record because the committed
    model differs from the caller's input under contention and a tuple would
    let a caller pick up its own stale value by position; `commit_based`, one
    tail-recursive `attempt` carrying its own witness AND the ancestor that
    witness stands for, minting per round through `S.Commit.v` +
    `S.Head.test_and_set` - `append_commit`'s production recipe, one door for
    both the whole-blob and exploded arms, with plain `commit` untouched in
    both; the pure `resolution` type, exhaustive over `Merge_spec.t`'s three
    constructors, whose conflict arm carries its reason OUT so the single
    commit site writes it into the label instead of onto a stderr line that
    dies with the process; the absent-head arm, because a denied test-and-set
    does not imply a competitor committed - the reaper removes whole branches,
    and `gather` reports absence as the app's INITIAL model, which a three-way
    policy would read as "theirs deleted everything"; `commit_coalesced` with
    the amend decision AND its test-and-set both pinned to the witness rather
    than to a head re-read taken after the caller's load; `append_commit`
    reworked onto the token, its three-attempt retry and its plain-`commit`
    fallback deleted, the first because retrying without recomputing is what
    R10 indicts and the second because an unconditional last write wins is R10
    itself; `step_with` re-signed to take the token, plus `?interpose`, a hook
    fired between the witnessed read and the commit so a test can land a
    competing writer in the one window that matters without a sleep or a
    scheduler assumption - deliberately NOT an injectable commit function,
    which would let a closure ignore the witness and reach the plain door in a
    directory that has no `.mli` to forbid it; `shared_doc_serve`'s
    `mutate_lock` retired; `test/contention_test` C1-C15 and S1-S2, 17 checks
    over four local apps - an `Or_set` app because a counter CANNOT express two
    survivors here (the replica is minted from the branch name and R10's
    premise is one branch, so both writers apply under one replica and
    `Pn_counter.join` is a per-replica max), a `Three_way` app that merges, one
    that always refuses, and a `Last_write_wins` app; 13 new mutations, 11
    killing and 2 declared equivalents; the mutation driver's native runner
    gains `NATIVE_TIMEOUT_S` and a `TimeoutExpired` non-catch verdict, because
    an unbounded reconcile loop's regression is a HANG, and a sweep that hangs
    reports nothing at all, which is strictly worse than a MISS).

    Nothing is re-run: a denied round re-runs neither `Loop.step` nor
    `A.update`, it reconciles the model already computed against the model at
    the head that landed. Re-running would double a real `Lwt_unix.sleep` and
    mint a second CRDT dot for one user operation, which a join then keeps -
    one click, two set elements. And the commit is **total**, which is a
    composition requirement rather than a convenience: a refusal would have to
    travel to the pump, whose `Cell` has already consumed the sequence number
    before the apply and whose guard outlives the socket, so a refused message
    replays as `Duplicate` and is acked without ever being applied. Un-taking
    would mean lowering a floor whose stated law is that it never lowers. The
    D16/D18 family's standing direction - degrade toward a visible duplicate,
    never toward silent loss - therefore forbids the obvious `Contended` retry
    budget outright.

    Left open, deliberately: R10b, R10c, R10d and R10e (§10; R10d since
    **closed in step 16**, D21) - unguarded
    `undo`/`redo`/`fork` head moves, the non-CRDT arms keeping content only for
    the already-acked writer, the pre-existing cancellation loss, and
    per-socket liveness plus the unreferenced commits a lost round leaves in a
    repo with no GC wired; the example tier was NOT driven by a two-writer
    check, because the `Mutating` RPC handler exposed no interpose seam and,
    under `Irmin_mem` with `Cmd.none`, two RPC calls simply serialize, so what
    replaced the retired mutex was a type-level guarantee at the seam and not
    a check at the example (**closed in step 15**: `routes_once ?on_taken` is
    that seam, and T7/T14 land a concurrent duplicate against the real
    handler while browser B9 retries through it); and C11/C12, the pack
    close/reopen round trips that
    were this step's U1 decision point, are **pins rather than
    mutation-confirmed** - both went green, so the `S.Commit.v` door is sound
    on pack and Irmin's contents-level `S.test_and_set` fallback stays
    unopened, but no mutation in the table kills them. Two mutations are
    declared equivalents and say why: the round's base tree is unobservable end
    to end (`scatter` rewrites every model path, and nothing in the suite
    writes a path the model does not own), and an amend decision taken from a
    live head read instead of the witness falls into an append that reconciles
    to an identical landed state, so catching it would need a check on the
    orphaned amend commit or on the clock tick it burns.

15. **rpc-exactly-once (+ the mirror bound)** - route the RPC tier through
    the D15-D18 guard family as a second channel, and bound the floors
    mirror as the ride-along (R12/R15, D20). **Done** (`Cmd.Http_delivery` +
    the `delivery` field on `Cmd.Http` + `Cmd.http_keyed`, threaded
    wildcard-free through both interpreters; `Tea_rpc.Key` - `x-tea-key`,
    `<32 hex> ":" <decimal>`, a total `of_string` over `split_on_char`
    whose seq arm accepts exactly the canonical print -
    plus `keyed_resp = Reply of 'resp | Replayed` with its Repr codec and
    `Applied_reply_lost`, `call` keying `Mutating` endpoints and unwrapping
    the envelope while `Read_only` stays raw; `Tea_server.Reply_cache`, the
    newest reply per floor tab - `Busy`/`Original`/`Gone`, four bounds
    (`entries`, `max_bytes`, `body_cap`, and `pending_grace`, the
    per-window poll budget of D20.2), a pure core whose
    `find` returns `t * found` because a pure value cannot record its own
    recency touch, under an imperative `Cell` with the plain arity;
    `Rpc_once` - guard, replies, `floor_replica`, kept outside the
    `Handlers` functor because none of its fields mentions `Api`, re-spelt
    `Rpc.once` where an app writes it - and `routes_once ?on_taken`, the
    keyed dispatch: origin gate, key parse, the server-derived floor tab
    (D20.1), then the pump discipline transcribed - a gap is 400, a
    duplicate reads the cache (503 while Pending, the original bytes once
    Settled, `Replayed` when Gone), and Fresh marks Pending synchronously,
    applies through the witnessed step, persists the floor at the canonical
    commit's own water, settles, and only then answers 200;
    `Tea_client.Rpc_delivery` and the runtime's keyed XHR arm - one live
    request per head seq, the 5xx/network retry with backoff, `rotate` on a
    4xx refusal; `Tea_server_pack.open_guards` and the second `Guard_file`
    journal at `<root>.guard/rpc` (one `branch_waters` read feeding both
    boot filters; the ws journal opens first, because `Guard_file` creates
    its own directory and not its parent); `Durable_guard ?mirror` +
    `default_mirror` + `mirror_bound`; `serve`/`serve_pack` re-signed to
    take `?rpc_once:(Store.t -> Rpc_once.t -> Dream.route list)` - a
    builder over the store, because an app's keyed handler is a function of
    the store and those entry points open it themselves; `shared_doc` grown
    a real `Publish_tag` mutation with a `publish_line` verdict readout and
    a durable tier (`Make_rpc` functorized over the store, `main.ml`'s
    `TEA_ROOT` arm - retiring its own "a durable shared_doc would need the
    pack tier first" deferral); seven new test files carrying T1-T21:
    `rpc_once_test` with T1's ordering arm (the keyed 200 still unresolved
    while the floor's append is held open behind a gated sink - the check
    the whole pre-existing suite could not red), `rpc_pack_once_test` (T5,
    T6 and the native B10: two lives over one pack root sharing
    `<root>.secret`, so life two decrypts life one's cookie),
    `rpc_window_test` (T2b/T7/T14 through `?on_taken`, applying
    `Dream.handler` directly because a nested `Lwt_main.run` dies under
    `Dream.test`, plus T21's rejecting-handler arm: an armed failure
    answers 500, commits nothing, and the retry lands the effect exactly
    once - R27), `reply_cache_test`, `rpc_delivery_test`,
    `rpc_journal_test`, `pack_guards_test`; browser B9 and B10 over a
    genuinely lost 200 (Playwright `route.fetch()` then `route.abort()`, no
    proxy process), B9 discriminating on commit COUNT because the tag set
    is an `Or_set` and duplicate suppression is unobservable in it; 25
    mutation ids in the dual-suite driver, every one confirmed RED against
    green baselines of 986 native checks across 42 executables and 89
    browser checks).

    Left open, deliberately: R21-R26 and the R15 amendment (§10); the
    client `Rpc_delivery` queue is unbounded, exactly as `Delivery` is and
    for the same reason - a dropped entry is the silent loss this step
    exists to remove, and `pending` is the honesty valve; one-in-flight
    serializes a page's `Mutating` calls (per tab, mutating-only), and a
    duplicate during a long Fresh window waits through the client's
    backoff, bounded by the `pending_grace` poll budget draining to
    `Replayed`; the
    single-branch contract leans on prose plus T5 where the WS tier leans
    on construction; the fallible-endpoint residual of §8 stays open - a
    fuel-exhausted reply is a state-shaped value computed once at take
    time, and the applied-versus-attempted distinction belongs in a
    `('ok, 'err) result`-shaped `'resp`; the mem tier keeps only the
    in-process sentence; and keyless or cookieless callers keep today's
    at-most-once semantics by stated precondition.

16. **Cancellation atomicity** - close R10d's W1 window, so a dying socket
    can no longer abandon a taken sequence number (R10d, D21). **Done**
    (`live_session`'s teardown restructured: the per-frame body extracted
    into `handle_frame`, never an element of any `Lwt.pick`/`Lwt.choose`
    list and never a `Lwt.cancel` target; the sender's promise bound as
    `drained` - never raced, joined by teardown - and its death turned
    into a `Lwt.wait`-based one-shot
    `died` signal, raced against the next frame through `Lwt.choose`, which
    never cancels the losing branch, so a hung `receive_frame` cannot block
    death detection and death detection cannot cancel a receive; ONE
    `Lwt.protected` body over `step_ws` and BOTH `persist_taken` arms, so
    an external cancellation rejects only the wrapper while the span
    completes as an orphan with no ack minted - a torn window lands in W2's
    licensed visible-duplicate shape, never in W1 - and `protected` rather
    than `no_cancel`, because `no_cancel` would let the pump recurse past
    the death of its own socket; the fuel arm's once-ever bottom floor
    preserved under cancellation; an `Lwt.catch` rejection barrier
    mirroring the HTTP tier's R27, seated INSIDE the protected body
    (round 3) - `protected` never rejects its body, so every exception
    the catch sees is body-originated and it releases the taken seq
    unconditionally before re-failing, the replay reading `Fresh` and
    re-applying, while the wrapper's cancel never reaches the catch at
    all; the `died` choose arm minted once above the recursion, so a
    healthy session leaks no per-frame waiter; the pump
    pre-checks `Lwt.state died`, so the between-frames death observation
    is deterministic rather than a coin flip against a pipelining
    client's already-resolved frame; teardown unconditional and ordered,
    `unwatch` then `push None` under an inner `finalize`, with the
    sender's drain promise BOUND so the final ack is on the wire before
    the session promise settles;
    `test/cancel_test` s1-s7, 41 checks, with the deterministic W1
    reproduction arming the transport's kill gate before the session opens
    so the sender parks mid-send on the Hello frame and `?interpose` wakes
    the gate inside the take-to-ack window; seven mutations s16-a..g,
    every one red against the 41-check baseline - the original pick race
    restored, `protected` dropped to `Fun.id`, `protected` weakened to
    `no_cancel`, the `died` signal never fired, the fuel arm's persist
    dropped, the rejection barrier's release dropped (s16-f: s3's
    reconnect ladder reads the Duplicate-acked-without-apply loss), the
    round-2 `Canceled` arm reintroduced inside the inner catch (s16-g:
    s7's rejected-orphan ladder, the released seq replaying as Fresh
    and applying exactly once, with s6's parked-orphan ladder guarding
    the converse release-too-early direction); suite baselines 1030
    native checks across 43 executables and 89 browser checks).

    Left open, deliberately: whether Dream's own WS-disconnect path cancels
    the promise `live_session` returns was source-inspected by the step-17
    panel and is provisionally ruled out (not empirically confirmed), on the same
    footing as the HTTP tier, where the R27 barrier already converts a
    cancellation into a release; a peer that ACKs TCP but never reads
    parks the joined drain forever, so cancellation of such a session is
    not prompt (transport liveness stays the socket layer's job - a
    grace-bounded pick over the drain would reintroduce exactly the
    send-cancellation s16-a forbids); a `Duplicate` ack issued while an
    attempt is in flight rides on that attempt's success, so an attempt
    that fails after such an ack is a recorded loss window whose closure
    needs per-key in-flight parking of duplicate acks (closed in step 17);
    teardown's `push None` and the `unwatch`
    line have no killing mutation by declaration - dropping `push None`
    parks the ended session's sender forever on a
    never-closed stream, and a dropped `unwatch` is swallowed by the
    closed stream rejecting the orphan callback's push, each a defect
    with no observable at the public seam; and the closure wording stays
    narrower than "structurally unreachable" on purpose - W1 is closed
    for the internal sender-vs-pump race by construction, for any other
    cancellation source by the protected barrier, and for a rejection of
    the span's own body by the release barrier, nothing more.

17. **Duplicate-ack parking** - close D21's declared
    Duplicate-ack-riding-a-failing-attempt window, so a replayed seq is
    never acked on the strength of an attempt that can still fail (D22;
    R10f declared in its place). **Done** (`Ack_park` registry: one row
    per (replica, tab, seq), minted by `Lwt.wait`; the Fresh arm
    registers synchronously at the verdict, the span settles at its three
    exits - `Landed` only after `persist_taken` resolves, `Released` at
    the fuel bottom and in the catch after the release barrier; the
    Duplicate arm parks through `Lwt.protected` on the open row under the
    acked WATER - acks are cumulative, so a below-water replay parks on
    the water's attempt - acking on `Landed`, silent on `Released`, the
    client's retransmission licensing the Fresh retry; `Lwt.wait` keeps
    the shared settlement uncancelable through any single waiter, so a
    cancelled parked socket dies alone (s13.2), and the park is raced
    against the sender's death, so a settlement that never arrives
    cannot wedge the teardown past a dead sender (s14). A second
    register over a standing row - the replay guard's tab-LRU eviction
    can re-open a consumed seq mid-flight - supersedes it (old waiters
    wake Released), the handle-gated settle keeps a superseded
    attempt's late verdict off the successor's waiters, and both
    mutators write the map before they wake (depth-zero reentrancy).
    The RPC tier needs no
    parking - the reply cache serializes the window - and T22 pins the
    duplicate-inside-a-failing-window case. The spec's s17-i
    (`Reply_cache` `Pending` clearing) adjudicated INERT: the common-path
    release unconditionally re-opens the seq. Pinned by cancel_test's s6
    re-cut + s8-s14, pure ack_park_test (12 checks), T22, and a
    20-mutant sweep: 19 killed on their predicted checks, s17-e
    (`Lwt.wait` -> `Lwt.task`) the declared survivor, masked by
    `Lwt.protected` and proven load-bearing by the joint s17-ef mutant;
    suite baseline 1069 native checks across 44 executables.)
18. **Store-identity token** - close DESIGN R20a for independent stores,
    so a guard journal restored beside a DIFFERENT pack store drops its
    floors instead of judging replays Duplicate against effects that
    store never produced (D23; R20b declared in its place). **Done**
    (`Tea_core.Prim.Store_identity`, a 16-byte / 32-lowercase-hex
    de-duplication key over stores, minted once by
    `Store_pack.resolve_identity` into `<root>/tea.identity` with the
    `O_EXCL`-temp + fsync + `Unix.link` publish `Session_secret` already
    uses, never rewritten on any failure and never re-minted; inside the
    root because a fourth sibling is R20 recursed one level, and under
    the `tea.` prefix because irmin-pack's `store.*` classifier leaves
    `` `Unknown`` entries alone through open, GC cleanup and migration.
    Each journal carries the binding as its own first frame, tag
    `'\004'`, peeled before `decode_prefix` so no `Guard_sink.event`, no
    `Floors` fold and no cap eviction ever sees it, and written only
    inside `compact`'s existing tmp-then-rename cutover so the binding
    and the floors are always one generation.
    `Guard_file.open_ ~identity` takes one binding read once and handed
    to both channels, exactly as `~head_water` is, and reports one of
    five exhaustive outcomes: `Matched`; `Freshly_bound`, reserved for
    the exactly zero-byte journal; `Adopted_unbound n` for a headerless
    journal, including one whose head bytes cannot be unframed,
    trust-on-first-use and counted, bound in that same open; `Rebound n`
    for a decoded different token, or a header whose payload can be no
    store's token, floors cleared BEFORE `Floors.of_events` - never by
    starving the water filter, which keeps `bottom` floors
    unconditionally - and the journal re-bound immediately on step 13's
    own argument; and `Unresolved_cleared n` when the root's own token
    cannot be read or minted, which clears the floors but HOLDS the
    journal byte-for-byte - no compaction, no re-stamp, no append
    persisted - so a later boot that can read the token keeps what it
    earned. Degradation is duplicate in every arm and there is no
    refusal anywhere on the path, by construction of the types rather
    than by convention. Declared non-goals: no binding for
    `<root>.secret`, no MAC against it, no strict fail-closed flag.
    Pinned by `test/store_identity_test`, `test/guard_identity_test`
    I1-I13, `test/identity_explain_test`'s boot-log truthfulness
    checks, the two-channel checks in `pack_guards_test`, a post-GC
    survival check in `archive_gc_test`, an end-to-end `Rpc_once`
    positive control in `rpc_pack_once_test`, a 12-mutant sweep - 12/12
    killed on their predicted checks, zero survivors, every kill a test
    failure on a clean build - and five kill-checked review-fix mutants
    from the adversarial round; suite baseline 1178 native checks
    across 47 executables.)
