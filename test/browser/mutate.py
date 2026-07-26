#!/usr/bin/env python3
"""Mutation driver for the browser smoke test (roadmap step 8, D13).

A browser test is the easiest kind of test to write vacuously: a selector that
never matches, a poll that gives up quietly, an assert on something the server
never touched. So each check in smoke.mjs is confirmed the same way every other
test in this repo is - by applying a mutation it MUST catch, watching it go
red, and restoring.

Each mutation names the checks it is expected to break; anything else going red
(or the target staying green) is reported as a MISS and fails the driver.

  python3 test/browser/mutate.py          # run every mutation
  python3 test/browser/mutate.py M2       # run one

Restores the tree with `git checkout --` after each mutation, so an interrupted
run leaves at most one mutation live - check `git status` if it dies.
"""

import re
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]

MUTATIONS = [
    {
        "id": "M1",
        "why": "the live view is real: without a store subscription no frame reaches tab B",
        "file": "examples/counter/counter_app.ml",
        "old": "let subscriptions _model = Sub.store_watch (fun m -> Sync m)",
        "new": "let subscriptions _model = Sub.none",
        # Killing the subscription also kills the echo the acting tab needs to
        # reach 2, so the D14 pin correctly stops holding as well.
        "expect_red": ["purely via the WS live frame", "KNOWN BUG D14"],
    },
    {
        "id": "M2",
        "why": "the rendered stats line is the RPC reply, not a fixed string",
        "file": "examples/shared_doc/shared_doc_rpc.ml",
        # Mutating `title_len` rather than `word_count`: the latter is a bound
        # tuple element, so replacing its use trips warning-as-error and the
        # mutation would die at COMPILE time, proving typing instead of
        # observation.
        "old": "{ title_len = String.length req.title; word_count }",
        "new": "{ title_len = 0; word_count }",
        "expect_red": ["round-trips Doc_stats over XHR"],
    },
    {
        "id": "M3",
        "why": "the mutating endpoint's 200 means a COMMIT, not just a reply",
        "file": "examples/shared_doc/server/shared_doc_serve.ml",
        "old": "Lwt.bind (Server.step s (Shared_doc_app.App.Add_tag req))",
        "new": "Lwt.bind (Server.step s (Shared_doc_app.App.Sync_doc (fst Shared_doc_app.App.init)))",
        "expect_red": ["raises the stored count"],
    },
    {
        "id": "M4",
        "why": "the mount gate fires: a renamed class must break the run, not skip it",
        "file": "examples/counter/counter_app.ml",
        "old": 'class_ "count"',
        "new": 'class_ "count-renamed"',
        "expect_red": ["counter: scenario ran to completion"],
    },
]


def run(cmd, **kw):
    return subprocess.run(cmd, cwd=REPO, capture_output=True, text=True, **kw)


def smoke():
    """Run the harness; return (stdout, red_labels)."""
    out = run(["node", "test/browser/smoke.mjs"]).stdout
    red = [ln for ln in out.splitlines() if ln.startswith(("FAIL", "STALE"))]
    return out, red


def apply_mutation(m):
    path = REPO / m["file"]
    src = path.read_text()
    if src.count(m["old"]) != 1:
        return f"anchor {m['old']!r} occurs {src.count(m['old'])} times in {m['file']}, expected 1"
    path.write_text(src.replace(m["old"], m["new"]))
    return None


def main():
    wanted = sys.argv[1:]
    chosen = [m for m in MUTATIONS if not wanted or m["id"] in wanted]

    base_out, base_red = smoke()
    if base_red:
        print("BASELINE IS NOT GREEN - fix that before mutating:\n" + "\n".join(base_red))
        return 1
    print(f"baseline green ({base_out.strip().splitlines()[-1]})\n")

    verdicts = []
    for m in chosen:
        err = apply_mutation(m)
        try:
            if err:
                verdicts.append((m["id"], False, err))
                continue
            build = run(["opam", "exec", "--switch=irmin-tea", "--", "dune", "build"])
            if build.returncode != 0:
                # A mutation that dies at COMPILE time proves typing, not
                # observation: the check was never given the chance to fail.
                verdicts.append((m["id"], False, "build failed (mutation is not observable): " + build.stderr.strip()[:200]))
                continue
            _, red = smoke()
            hit = [w for w in m["expect_red"] if any(w in ln for ln in red)]
            stray = [ln for ln in red if not any(w in ln for w in m["expect_red"])]
            ok = len(hit) == len(m["expect_red"]) and not stray
            detail = f"red={len(red)}"
            if len(hit) != len(m["expect_red"]):
                detail += f"; MISSED {[w for w in m['expect_red'] if w not in hit]}"
            if stray:
                detail += f"; STRAY {[s[:60] for s in stray]}"
            verdicts.append((m["id"], ok, detail))
        finally:
            run(["git", "checkout", "--", m["file"]])

    run(["opam", "exec", "--switch=irmin-tea", "--", "dune", "build"])
    print()
    for mid, ok, detail in verdicts:
        m = next(x for x in MUTATIONS if x["id"] == mid)
        print(f"{'RED ' if ok else 'MISS'} {mid}: {m['why']}  [{detail}]")
    bad = [v for v in verdicts if not v[1]]
    print(f"\n{len(verdicts) - len(bad)}/{len(verdicts)} mutations caught")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
