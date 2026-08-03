#!/bin/sh
# The browser smoke test (roadmap step 8, D13): build the tiers, then drive the
# real compiled server binaries with a real Chromium. See smoke.mjs.
#
# Deliberately NOT a `dune runtest` rule. It needs node, a Playwright-managed
# Chromium (~150MB, out of the opam switch entirely) and two listening ports,
# so wiring it into the default test alias would make `dune runtest` fail on
# any machine that has the OCaml toolchain but not the browser one. The OCaml
# suite stays hermetic; this is the opt-in end-to-end gate.
#
# First run needs the harness deps:  (cd test/browser && npm install)
set -eu

here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)

if [ ! -d "$here/node_modules/playwright" ]; then
  echo "test/browser: playwright is not installed; run (cd test/browser && npm install)" >&2
  exit 1
fi

echo "== dune build =="
# `@default` recursively, from the repo. Both halves are load-bearing, and both
# fail the same silent way. A bare `dune build` is the NON-recursive default
# alias: it builds each client's compiled bundle but not the index.html beside
# it, because the page is a source file that only reaches _build through the
# per-directory default alias each client dune declares. And dune resolves that
# alias relative to the CURRENT DIRECTORY, so invoking this script from outside
# the repo builds an empty set and exits 0. Either way the build reports
# success, the server answers 404 under /app, and the failure surfaces three
# layers away as every scenario below timing out on a selector that never
# appears.
cd "$repo"
opam exec --switch=irmin-tea -- dune build --root "$repo" @default

echo "== browser smoke =="
cd "$here" && exec node smoke.mjs
