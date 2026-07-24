# ocaml-tea

A full-stack OCaml web framework on **The Elm Architecture**, backed by
**[Irmin](https://irmin.org)** (a Git-like versioned store) instead of SQL.
An OCaml + Irmin reimagining of [lean-tea](https://github.com/Verilean/lean-tea).

The idea in one line: **your app's `Model` lives in Irmin, and every `update` is a
commit** — so undo/redo, time-travel, an audit log, and collaborative merge come
from the store, not from framework code.

See [DESIGN.md](./DESIGN.md) for the architecture, the lean-tea mapping, and the
roadmap.

## What builds today

- `lib/tea_core` — the pure TEA core (`APP` signature, effects-as-data `Cmd`/`Sub`,
  a `private` `Html` type with server-side rendering, `Merge_spec`, `Codec`, and a
  fuel-bounded `Loop`). Depends only on `repr`, so it compiles to native *and* JS.
- `lib/tea_persist` — the Irmin-backed versioned model store: session branches,
  `apply` = one commit, `history`, `undo`, and `merge_into`.
- `examples/counter` — the shared Counter app.
- `test/persist_test` — proves the store end-to-end.

Server (Dream) and client (js_of_ocaml + ocaml-vdom) tiers are designed in
DESIGN.md and are the next milestones.

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

If you're setting up from scratch on a new machine, create a switch (e.g. `opam switch create ocaml-tea 5.3.0`); it needs the system
libraries `pkgconf`, `libev`, `libffi` (`brew install pkgconf libev libffi`),
then `opam install irmin irmin-git dream js_of_ocaml vdom`.
