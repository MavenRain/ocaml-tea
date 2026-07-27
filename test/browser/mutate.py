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
        # Only tab B's check reddens. This entry used to also name the D14 PIN's
        # label, which was correct while D14 was a bug: killing the subscription
        # killed the echo the acting tab needed to reach 2, so the pin stopped
        # holding. Step 9 fixed D14 and rewrote that pin as an ordinary
        # transition on A settling at 1 — and A reaches 1 by its own OPTIMISTIC
        # apply, which needs no frame at all, so this mutation leaves it green.
        # The stale entry named a label no run can print, and since a missed
        # expectation is a failure, M1 could not pass. Confirmed statically:
        # "KNOWN BUG" survives in this harness only inside smoke.mjs's header
        # comment, never as a check label.
        # Step 11 added B1-B4, which ALL depend on the live view, so this
        # mutation now reddens the earliest live-frame check in each of them
        # too. Every one must be named: an unnamed red is reported as a STRAY,
        # and a stray is a failure just like a missed expectation.
        "expect_red": [
            "purely via the WS live frame",
            "the click's up-frame is captured in flight",
            "tab A's first edit reaches tab B",
            "the click lands while its Ack is dropped",
            "first click commits and streams to the observer",
        ],
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
        # The count element is the mount gate for EVERY counter-based scenario,
        # so renaming its class strands B1-B4 at their own mount waits as well.
        "expect_red": [
            "counter: scenario ran to completion",
            "replay B1: scenario ran to completion",
            "replay B2: scenario ran to completion",
            "replay B3: scenario ran to completion",
            "durable B4: scenario ran to completion",
        ],
    },
    # --- step 11 (D16): the delivery scenarios B1-B4 -------------------------
    #
    # The B scenarios abort at their FIRST failing check (smoke.mjs's `step`
    # early return), so each expectation below names the EARLIEST check its
    # mutation breaks - naming a later one would report a MISS for driver
    # reasons, not for vacuity.
    {
        "id": "mut-unacked-empty",
        "why": "the replay is real: a client whose unacked queue reads empty re-sends nothing",
        "file": "lib/tea_client/delivery.ml",
        # `unacked` feeds flush_outbox, the ONLY replay path. Filter-to-empty
        # rather than a bare [] so `t` stays used and the mutation cannot die
        # at compile time (warnings-as-errors would prove typing, not
        # observation - the M2 lesson).
        "old": "let unacked (t : 'msg t) : (Msg_seq.t * 'msg) list = List.rev t.queue",
        "new": "let unacked (t : 'msg t) : (Msg_seq.t * 'msg) list = List.filter (fun ((_ : Msg_seq.t), (_ : 'msg)) -> false) t.queue",
        # B1: the dropped edit is never replayed, the observer stays 0.
        # B3: no second apply ever crosses the wire.
        # B4: nothing is replayed after the restart, so no post-restart Ack.
        # B2 and the step-8 scenarios never break a socket, so they stay green.
        "expect_red": [
            "replays the edit onto the store exactly once",
            "a second apply crosses the wire",
            "a post-restart Ack arrives",
        ],
    },
    {
        "id": "mut-tab-collapse",
        "why": "the guard key is (replica, tab): collapsed to the replica alone, tab B's first edit reads as tab A's replay",
        "file": "lib/tea_server/replay_guard.ml",
        # A comparator that calls every tab equal collapses the per-replica tab
        # map to one entry, which is exactly the two-tabs bug D15's key closes.
        # Only B2 has two tabs SENDING on one session; every other scenario has
        # a single acting tab, so only B2 may redden.
        "old": "module Tab_map = Map.Make (Tea_core.Prim.Tab_id)",
        "new": "module Tab_map = Map.Make (struct type t = Tea_core.Prim.Tab_id.t let compare (_ : t) (_ : t) = 0 end)",
        "expect_red": ["guard key keeps both tabs"],
    },
    {
        "id": "mut-b4-fresh-root",
        "why": "B4's restart really reopens the SAME root: a fresh one loses the store and the count comes back 1, not 2",
        "file": "examples/counter/server/main.ml",
        # Each server life gets a random sibling suffix, so the second life
        # opens an empty store and an empty journal: the replay re-applies onto
        # 0 and both tabs read 1 - the lost-store arm of the B4 verdict. This
        # is the mutation that forced B4's two-click design: with one click,
        # 0 + a replayed 1 is indistinguishable from the real guarantee.
        "old": "Pack_server.serve_pack ~port ?client_dir ~root:(Tea_server_pack.Root.v root) ())",
        "new": 'Pack_server.serve_pack ~port ?client_dir ~root:(Tea_server_pack.Root.v (Random.self_init (); root ^ "-" ^ string_of_int (Random.int 1000000))) ())',
        # BLOCKED on the session-identity gap that B4 now pins. A restart
        # already lands on a fresh branch under a fresh replica (Dream's
        # memory-backed sessions lose the session id), so the honest world and
        # this mutated one BOTH leave the restarted server holding 1: the
        # mutation is currently unobservable, and running it would report a
        # MISS that says nothing about vacuity. Unblocks the day session
        # identity survives a restart, which is also the day B4's pin reddens.
        "blocked": "session identity is not durable; see B4's PIN in smoke.mjs",
        "expect_red": ["the RESTARTED SERVER itself holds 2"],
    },
    {
        "id": "mut-journal-unwired",
        "why": "B4's floor really comes from the file journal: on the null sink the restart forgets it and the replay double-applies to 3",
        "file": "lib/tea_server_pack/tea_server_pack.ml",
        # `ignore` keeps sink/floors used so the mutation compiles (M2's
        # lesson). The pack tier degrades to step-10 in-memory semantics: life
        # 1 still de-duplicates, but life 2 reads Fresh for the replayed seq 2
        # and re-applies it. B1-B3 run the mem tier - serve_pack is never on
        # their path - so they MUST stay green under this mutation.
        "old": "~tabs:Replay_guard.default_tabs ~sink ~floors",
        "new": "~tabs:Replay_guard.default_tabs ~sink:(ignore sink; Guard_sink.null) ~floors:(ignore floors; Durable_guard.Floors.empty)",
        # BLOCKED on the same session-identity gap as mut-b4-fresh-root, and
        # for a sharper reason: a restart already lands on a fresh replica, so
        # the journalled floor is never consulted no matter what sink wrote it.
        # Unwiring the journal is therefore unobservable end to end (measured:
        # zero reds). It DID score a red in the first sweep, but only against
        # B4's old client-rendered assertion, which the join makes unreliable -
        # that red was not evidence either. The journal's effect is covered in
        # process instead, where durable_guard_test/exactly_once_test pin it and
        # the OCaml sweep confirmed it. Unblocks when B4's pin reddens.
        "blocked": "session identity is not durable, so no restart consults the journal; see B4's PIN",
        "expect_red": ["the RESTARTED SERVER itself holds 2"],
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
    # A blocked mutation is one nothing can currently observe (see its own
    # note). Naming it explicitly on the command line still runs it; a bare
    # sweep skips it and SAYS SO, because a silently dropped mutation reads as
    # coverage that was never there.
    chosen = [
        m
        for m in MUTATIONS
        if (m["id"] in wanted) or (not wanted and not m.get("blocked"))
    ]
    skipped = [m for m in MUTATIONS if m not in chosen]
    for m in skipped:
        print(f"SKIP {m['id']}: blocked - {m['blocked']}")

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
