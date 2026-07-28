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

Restores each mutated file from an in-memory copy of its pre-mutation bytes,
NEVER `git checkout --`: on a working tree carrying uncommitted work, checkout
restores the HEAD version and silently destroys the unstaged changes (measured,
not theoretical - it reverted an uncommitted tea_server_pack.ml mid-sweep). An
interrupted run still leaves at most one mutation live - check `git status`
if it dies.
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
            "life 1 commits one click under its own secret",
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
        # so renaming its class strands B1-B5 at their own mount waits as well.
        "expect_red": [
            "counter: scenario ran to completion",
            "replay B1: scenario ran to completion",
            "replay B2: scenario ran to completion",
            "replay B3: scenario ran to completion",
            "durable B4: scenario ran to completion",
            "secret B5: scenario ran to completion",
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
        # UNBLOCKED by step 12 (D17): session identity is durable, so the
        # restarted server consults the journal and this mutation is finally
        # observable. It is now COMPOUND: the fresh root moves <root>.secret
        # too (serve_pack resolves the secret beside the root), so life 2 also
        # mints a fresh secret and refuses life 1's cookie - its red means
        # "store OR identity lost", and mut-journal-unwired is the
        # discriminating half. Every red must be named or the driver calls it
        # STRAY: the secret is no longer at <root>.secret (the harness stats
        # the unsuffixed path), the SSR read with the pinned life-1 cookie is
        # refused (Set-Cookie reissue), the verdict lands on the lost-identity
        # arm (0), and B6's existing empty dir gains a random-suffixed leaf
        # whose parent exists, so the preflight rightly accepts it and B6's
        # refusal never prints - which costs B6 its exit-status check too,
        # since an accepted root SERVES rather than exiting.
        # B7 is collateral for the same reason: its TEA_ROOT gains a suffix,
        # so the surviving <root>.guard is no longer the mutated root's
        # sibling, the orphan pair is not recognised, and the server serves.
        # Its "not a crash" check is the one B7 check that stays green (a
        # server that runs prints no uncaught exception).
        "expect_red": [
            "the durable secret is on disk at <root>.secret",
            "life 2 ADOPTS the presented session cookie",
            "the RESTARTED SERVER itself holds 2",
            "preflight B6: an unusable TEA_ROOT is refused",
            "preflight B6: the refusal exits NON-ZERO",
            "orphan B7: a surviving <root>.guard beside a missing pack root is refused",
            "orphan B7: the refusal exits NON-ZERO",
            "orphan B7: the server never became usable",
        ],
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
        # UNBLOCKED by step 12 (D17). This is the DISCRIMINATING half of the
        # B4 verdict: the store and the identity both survive (same root, same
        # <root>.secret, cookie adopted with no reissue), only the floor is
        # lost, so the replayed seq 2 reads Fresh and double-applies - the
        # restarted server reads 3, the lost-floor arm. Exactly one check
        # reddens; adoption and the on-disk secret stay green, which is what
        # separates this red from mut-b4-fresh-root's compound one.
        "expect_red": ["the RESTARTED SERVER itself holds 2"],
    },
    # --- step 12 (D17): durable session identity ----------------------------
    {
        "id": "mut-secret-per-boot",
        "why": "B4's identity really rides ONE on-disk secret: a per-boot secret path is the D16 bug wearing a hat",
        "file": "lib/tea_server_pack/tea_server_pack.ml",
        # Each life resolves (and mints) its own <root>.secret-<pid>, so life 2
        # cannot decrypt life 1's cookie and the restart loses identity while
        # the store and the journal both survive - exactly the pre-step-12
        # world. The harness stats the unsuffixed <root>.secret, so that check
        # reddens too, and every red must be named or it counts as STRAY.
        # The path is built by the [sibling] helper now (it keeps .secret and
        # .guard OUTSIDE a root spelled with a trailing separator), so the
        # anchor follows it there. The mutation is unchanged in intent: suffix
        # the resolved file with the pid so every life mints its own.
        "old": 'Session_secret.resolve ~file:(sibling root ".secret") ()',
        "new": 'Session_secret.resolve ~file:(sibling root ".secret" ^ "-" ^ string_of_int (Unix.getpid ())) ()',
        "expect_red": [
            "the durable secret is on disk at <root>.secret",
            "life 2 ADOPTS the presented session cookie",
            "the RESTARTED SERVER itself holds 2",
        ],
    },
    {
        "id": "mut-secret-env-ignored",
        "why": "B5's anti-vacuity partner: with TEA_SECRET ignored both lives fall to one file and identity wrongly survives a secret change",
        "file": "lib/tea_server/session_secret.ml",
        # B5 sets DIFFERENT explicit TEA_SECRET values per life; ignoring the
        # variable drops both lives onto the same <root>.secret file, so life 2
        # adopts the cookie it was meant to refuse: no Set-Cookie reissue and
        # the server-rendered count survives as 1 instead of resetting to 0.
        # B4 never sets TEA_SECRET (the harness strips it), so B4 stays green -
        # which is the point: this red belongs to B5 alone.
        # The source reads the variable through its own [env_opt] alias
        # (which maps "" to None), not Sys.getenv_opt directly.
        "old": "env_opt env_var",
        "new": "(ignore env_var; None)",
        "expect_red": [
            "life 2 with a different TEA_SECRET reissues a Set-Cookie",
            "two lives with different TEA_SECRET do NOT share the session",
        ],
    },
    # --- the refusal scenarios B6 and B7 ------------------------------------
    #
    # Both refusals are two claims, not one: the audible stderr line AND the
    # non-zero status. The line alone is what an operator reads; the status is
    # what a supervisor reads, and it is the half a `unit`-returning arm
    # silently drops. So each gets a mutation that leaves one half intact and
    # kills the other, which is what keeps the two checks from covering for
    # each other.
    {
        "id": "mut-preflight-exit-zero",
        "why": "B6's status check is real: a preflight that prints and returns unit ends the binary at 0, and no supervisor can tell that from a clean shutdown",
        "file": "lib/tea_server_pack/tea_server_pack.ml",
        # There are TWO `exit 1)` in this file now (the orphan refusal above it
        # and this one), so the anchor carries the preceding Printf argument
        # line to stay unique - apply_mutation refuses anything that matches
        # more than once, but a silently-relocated mutation would be worse.
        # The eprintf is deliberately KEPT: the stderr check must stay green,
        # so the only red is the status one.
        "old": "             (Root.to_string root) (Store.explain e);\n           exit 1)",
        "new": "             (Root.to_string root) (Store.explain e);\n           ())",
        "expect_red": ["preflight B6: the refusal exits NON-ZERO"],
    },
    {
        "id": "mut-orphan-check-disabled",
        "why": "B7 is real: with the orphan check disabled the server happily serves a surviving journal over a wiped store, which is the SILENT LOSS path (a replay judged Duplicate against an empty model)",
        "file": "lib/tea_server_pack/tea_server_pack.ml",
        # `false &&` rather than deleting the branch: guard_dir and root stay
        # used, so the mutation cannot die at compile time and prove typing
        # instead of observation (the M2 lesson).
        "old": "if (not (Sys.file_exists (Root.to_string root))) && Sys.file_exists guard_dir then (",
        "new": "if false && (not (Sys.file_exists (Root.to_string root))) && Sys.file_exists guard_dir then (",
        # With the check disabled the root is merely missing under an existing
        # parent, which the preflight rightly ACCEPTS (it creates the store),
        # so the server serves: no orphan line, no exit at all. Three of B7's
        # four checks redden. The fourth ("not an uncaught exception") stays
        # green by construction - a server that runs prints no Fatal error -
        # which is exactly why it is a separate check rather than a conjunct.
        "expect_red": [
            "orphan B7: a surviving <root>.guard beside a missing pack root is refused",
            "orphan B7: the refusal exits NON-ZERO",
            "orphan B7: the server never became usable",
        ],
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
    """Apply and return (original_text, error): the caller restores from the
    returned text, never from git."""
    path = REPO / m["file"]
    src = path.read_text()
    if src.count(m["old"]) != 1:
        return None, f"anchor {m['old']!r} occurs {src.count(m['old'])} times in {m['file']}, expected 1"
    path.write_text(src.replace(m["old"], m["new"]))
    return src, None


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
        original, err = apply_mutation(m)
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
            if original is not None:
                (REPO / m["file"]).write_text(original)

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
