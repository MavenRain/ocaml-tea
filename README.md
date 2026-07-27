# ocaml-tea

A full-stack OCaml web framework on **The Elm Architecture**, backed by
**[Irmin](https://irmin.org)** (a Git-like versioned store) instead of SQL.
An OCaml + Irmin reimagining of [lean-tea](https://github.com/Verilean/lean-tea).

The idea in one line: **your app's `Model` lives in Irmin, and every `update` is a
commit** - so undo/redo, time-travel, an audit log, and collaborative merge come
from the store, not from framework code.

See [DESIGN.md](./DESIGN.md) for the architecture, the lean-tea mapping, and the
roadmap.

## What builds today

- `lib/tea_core` - the pure TEA core (`APP` signature, effects-as-data `Cmd`/`Sub`,
  a `private` `Html` type with server-side rendering, `Merge_spec`, the `Merge`
  three-way combinator library, `Codec`, and a fuel-bounded `Loop`). Depends only
  on `repr`, so it compiles to native *and* JS.
- `lib/tea_persist` - the Irmin-backed versioned model store: session branches,
  `apply` = one commit, `history`, `undo`, `merge_into`, and `fork` (branch a
  session off another's head to share a clean merge ancestor).
- `lib/tea_server` - the Dream tier: per-session Irmin branches behind Dream
  sessions, one commit per posted Msg, and SSR that rewrites every `On_click`
  site of the shared view into a CSRF-protected form post (the app works with
  zero client JS). Undo is served at `/undo`.
- `lib/tea_client` + `lib/tea_client_run` - the js_of_ocaml + ocaml-vdom client
  tier: the *same* `APP` in the browser, with a WebSocket live view driven by
  `Sub.Store_watch`.
- `examples/counter` - the shared Counter app, plus `server/` (a native Dream
  binary: `PORT=8080 dune exec examples/counter/server/main.exe`) and a browser
  `client/`.
- `examples/shared_doc` - a collaboratively-edited document whose fields merge
  structurally (a summed like counter, a unioned tag set, and free-text that
  *conflicts* rather than clobbering): the worked example of the `Merge`
  combinators and thesis T2.
- `test/persist_test`, `test/server_test`, `test/client_test` - the store, the
  Dream tier (sessions, branch isolation, CSRF, undo, live pump), and the client
  translations, end-to-end.
- `test/merge_test`, `test/collab_test` - the merge-combinator laws, and two
  sessions reconciling a shared document (T2 proven end-to-end).

The core, the Dream server, the js_of_ocaml client, the WebSocket live view, and
the collaborative three-way merge all build and are green; `tea_safe` (security
primitives) and history hygiene are the next milestones in DESIGN.md.

## Build & test

This project uses a dedicated opam switch so it won't disturb your other switches:

```sh
eval "$(opam env --switch=irmin-tea)"   # local dev switch; name is incidental, never committed
dune build
dune exec ./test/persist_test.exe
```

Expected:

```
ok   - three increments -> count = 3
ok   - history has one commit per update (3)
ok   - undo walks to previous commit -> count = 2
ok   - reload after undo -> count = 2

All persistence invariants hold (T1 proven end-to-end).
```

### The browser smoke test

`dune runtest` is hermetic: it links the tiers and drives them in-process, which
proves they are correct but not that they are *connected*. The end-to-end gate
runs the real compiled server binaries against a real Chromium, and lives
outside dune so the OCaml suite still passes on a machine without a browser
toolchain:

```sh
(cd test/browser && npm install)   # once: playwright + its Chromium
test/browser/run.sh                # dune build, then the smoke test
python3 test/browser/mutate.py     # confirm each check by mutation
```

It opens two tabs on one session and asserts a click in one reaches the other
purely over the live-view WebSocket, then round-trips a typed RPC through the
compiled jsoo XHR path and a browser-issued same-origin mutating POST.

It has already earned its keep: its first run found D14 (`DESIGN.md` §7), where
the acting tab double-counted its own PN-counter dot because the client and the
server applied one msg under two different replica ids. It held that as an
`xfail` pin until step 9 shared the replica id between the tiers, at which point
the pin went stale and became the ordinary assertion it is today.

If you're setting up from scratch on a new machine, create a switch (e.g. `opam switch create ocaml-tea 5.3.0`); it needs the system
libraries `pkgconf`, `libev`, `libffi` (`brew install pkgconf libev libffi`),
then `opam install irmin irmin-git dream js_of_ocaml vdom`.
