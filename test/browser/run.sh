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
opam exec --switch=irmin-tea -- dune build --root "$repo"

echo "== browser smoke =="
cd "$here" && exec node smoke.mjs
