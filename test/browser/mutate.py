#!/usr/bin/env python3
"""Mutation driver for the smoke test and the OCaml suite (roadmap step 8, D13;
two-suite since step 13, R20).

A browser test is the easiest kind of test to write vacuously: a selector that
never matches, a poll that gives up quietly, an assert on something the server
never touched. So each check in smoke.mjs is confirmed the same way every other
test in this repo is - by applying a mutation it MUST catch, watching it go
red, and restoring.

Each mutation names the checks it is expected to break; anything else going red
(or the target staying green) is reported as a MISS and fails the driver.

  python3 test/browser/mutate.py                    # run every mutation
  python3 test/browser/mutate.py M2                 # run one
  python3 test/browser/mutate.py mut-covers-strict  # ...by id

`expect_red` is a dict of SUITE to labels: `{"native": [...], "browser": [...]}`.
A suite runs for a mutation only if its list is non-empty, so a browser-only
entry never pays for the OCaml suite and vice versa - and a step-13 entry can
name a check in each tier, which is how the two halves of one guarantee (the
codec's witness and the server's use of it) are told apart. Both suites are
FAIL-FAST, so every expectation must name the EARLIEST check its mutation
breaks: the OCaml `check` exits 1 on the first failure of a test file, and
smoke.mjs's `step` early-returns out of a scenario. Naming a later check
reports a MISS for driver reasons rather than for vacuity.

A declared EQUIVALENT names its suites in `run` instead, with both expect_red
lists empty: it must build, execute, and leave everything green. Declaring one
beats inventing an expectation for it, because a missed expectation is a driver
failure.

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
        "expect_red": {"native": [], "browser": [
            "purely via the WS live frame",
            "the click's up-frame is captured in flight",
            "tab A's first edit reaches tab B",
            "the click lands while its Ack is dropped",
            "first click commits and streams to the observer",
            "life 1 commits one click under its own secret",
            "rollback B8: life 1 commits one click",
        ]},
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
        "expect_red": {"native": [], "browser": ["round-trips Doc_stats over XHR"]},
    },
    {
        "id": "M3",
        "why": "the mutating endpoint's 200 means a COMMIT, not just a reply",
        "file": "examples/shared_doc/server/shared_doc_serve.ml",
        "old": "Lwt.bind (Server.step s (Shared_doc_app.App.Add_tag req))",
        "new": "Lwt.bind (Server.step s (Shared_doc_app.App.Sync_doc (fst Shared_doc_app.App.init)))",
        "expect_red": {"native": [], "browser": ["raises the stored count"]},
    },
    {
        "id": "M4",
        "why": "the mount gate fires: a renamed class must break the run, not skip it",
        "file": "examples/counter/counter_app.ml",
        "old": 'class_ "count"',
        "new": 'class_ "count-renamed"',
        # The count element is the mount gate for EVERY counter-based scenario,
        # so renaming its class strands B1-B5 at their own mount waits as well.
        "expect_red": {"native": [], "browser": [
            "counter: scenario ran to completion",
            "replay B1: scenario ran to completion",
            "replay B2: scenario ran to completion",
            "replay B3: scenario ran to completion",
            "durable B4: scenario ran to completion",
            "secret B5: scenario ran to completion",
            "rollback B8: scenario ran to completion",
        ]},
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
        # B8: same, after the rollback - and only there, because B8 gates its
        # live clicks on the tab having re-dialled (tap.opens), so no click of
        # its own travels the replay path.
        # B2 and the step-8 scenarios never break a socket, so they stay green.
        "expect_red": {"native": [], "browser": [
            "replays the edit onto the store exactly once",
            "a second apply crosses the wire",
            "a post-restart Ack arrives",
            "a post-rollback Ack arrives",
        ]},
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
        "expect_red": {"native": [], "browser": ["guard key keeps both tabs"]},
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
        "expect_red": {"native": [], "browser": [
            "the durable secret is on disk at <root>.secret",
            "life 2 ADOPTS the presented session cookie",
            "the RESTARTED SERVER itself holds 2",
            "preflight B6: an unusable TEA_ROOT is refused",
            "preflight B6: the refusal exits NON-ZERO",
            "orphan B7: a surviving <root>.guard beside a missing pack root is refused",
            "orphan B7: the refusal exits NON-ZERO",
            "orphan B7: the server never became usable",
            # B8 never reaches a life-2 check (measured 2026-07-28): the
            # server opens the SUFFIXED root, so the harness's life-1 epilogue
            # `cp` of the unsuffixed TEA_ROOT dies with ENOENT and the
            # scenario lands in its catch-all. Everything after life 1 is
            # unreachable, hence unnamed.
            "rollback B8: scenario ran to completion",
        ]},
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
        #
        # Step 13: B8 loses its LINE but keeps its count. With no floors at
        # all, life 3 has nothing to drop, so the rollback goes unannounced -
        # while the replay it would have swallowed is admitted for the wrong
        # reason and still lands on 2. That is precisely why the line and the
        # count are separate checks.
        "expect_red": {"native": [], "browser": [
            "the RESTARTED SERVER itself holds 2",
            "rollback B8: life 3 boots over the rolled-back root and SAYS SO",
        ]},
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
        "expect_red": {"native": [], "browser": [
            "the durable secret is on disk at <root>.secret",
            "life 2 ADOPTS the presented session cookie",
            "the RESTARTED SERVER itself holds 2",
            # B8's life-2 count check SURVIVES this mutation (measured
            # 2026-07-28): the observer still renders 2 even though identity
            # is lost - a client-side join, the same weakness that made B4
            # assert via SSR. The first B8 red is the sibling-survival stat of
            # the unsuffixed <root>.secret, right after the rollback restore;
            # fail-fast makes everything after it unreachable, hence unnamed.
            "rollback B8: the guard journal and the durable secret survived the rollback",
        ]},
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
        "expect_red": {"native": [], "browser": [
            "life 2 with a different TEA_SECRET reissues a Set-Cookie",
            "two lives with different TEA_SECRET do NOT share the session",
        ]},
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
        "expect_red": {"native": [], "browser": ["preflight B6: the refusal exits NON-ZERO"]},
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
        "expect_red": {"native": [], "browser": [
            "orphan B7: a surviving <root>.guard beside a missing pack root is refused",
            "orphan B7: the refusal exits NON-ZERO",
            "orphan B7: the server never became usable",
        ]},
    },
    # --- step 13 (R20): the store water that binds the journal to the store --
    #
    # These are the first entries that name NATIVE checks, and the native
    # suite is fail-fast per executable (each `check` exits 1 on the first
    # failure), so a native expectation names the EARLIEST check the mutation
    # breaks in each test file - never the whole set it would break if the run
    # continued. The comment on each entry records the rest.
    {
        "id": "mut-water-global",
        "why": "the witness is PER BRANCH: a store-wide maximum lets a busy session's water vouch for an idle one's floors",
        "file": "lib/tea_persist/store_core.ml",
        # Reads the newest head in the whole repo instead of this session's
        # own. Every other water check still passes under it - one session's
        # store-wide max IS its own head - which is exactly what makes W3 the
        # only thing pinning per-key granularity.
        "old": """  let head_water (s : session) : Tea_core.Prim.Store_water.t Lwt.t =
    let* head =
      Lwt.catch (fun () -> S.Head.find s.branch) (fun (_ : exn) -> Lwt.return None)
    in""",
        "new": """  let head_water (s : session) : Tea_core.Prim.Store_water.t Lwt.t =
    let* head =
      Lwt.catch
        (fun () ->
          let* heads = S.Repo.heads s.repo in
          Lwt.return
            (List.fold_left
               (fun (acc : S.commit option) (c : S.commit) ->
                 Option.fold acc ~none:(Some c) ~some:(fun (b : S.commit) ->
                     if
                       Int64.compare
                         (S.Info.date (S.Commit.info c))
                         (S.Info.date (S.Commit.info b))
                       > 0
                     then Some c
                     else Some b))
               None heads))
        (fun (_ : exn) -> Lwt.return None)
    in""",
        # The browser tier cannot see this: serve_pack reads branch_waters at
        # boot, and the pump stamps floors with the water its own commit
        # returned, so head_water is on no serving path.
        "expect_red": {"native": ["W3 waters are per-branch"], "browser": []},
    },
    {
        "id": "mut-water-bottom",
        "why": "an absent head means 'no claim', but a PRESENT head must yield its real date: collapsed to bottom, every witness vanishes and the branch heads stop vouching for anything",
        "file": "lib/tea_persist/store_core.ml",
        # Both readers (head_water and branch_waters) share this one helper, so
        # the collapse reaches the boot filter too. Native: W2 first, then W3,
        # W5, W6, G1 and G7's Duplicate half. Browser: every real floor is now
        # judged against a bottom head, which classifies as no_branch - so B4's
        # restart double-applies, and B8 drops its floor for the WRONG reason,
        # keeping its count green while losing its line.
        "old": """    Option.fold head
      ~none:Tea_core.Prim.Store_water.bottom""",
        "new": """    Option.fold (ignore head; None)
      ~none:Tea_core.Prim.Store_water.bottom""",
        "expect_red": {
            "native": ["W2 each commit lifts"],
            "browser": [
                "the RESTARTED SERVER itself holds 2",
                "rollback B8: life 3 boots over the rolled-back root and SAYS SO",
            ],
        },
    },
    {
        "id": "mut-covers-strict",
        "why": "the equality in `covers` is load-bearing: after an orderly restart a floor's water EQUALS its branch head, so a strict comparison silently turns durable de-duplication off on every clean restart",
        "file": "lib/tea_core/prim.ml",
        "old": "let covers ~(head : t) ~(floor : t) : bool = Int64.compare head floor >= 0",
        "new": "let covers ~(head : t) ~(floor : t) : bool = Int64.compare head floor > 0",
        # The highest-probability regression in the whole step, and step 11's
        # own browser scenario is its executioner: B4's life 2 drops the floor
        # it should have honoured and the replay double-applies to 3. B8 keeps
        # its count (its floor deserved to drop) but loses its silence: life 2
        # now cries rollback over an intact root, which is what that check is
        # for. Native: G1's kept arm, whose ra head EQUALS its floor.
        "expect_red": {
            "native": ["G1 floors are adopted under covering waters"],
            "browser": [
                "the RESTARTED SERVER itself holds 2",
                "rollback B8: life 2 boots over an intact root and says nothing about a rollback",
            ],
        },
    },
    {
        "id": "mut-covers-inverted",
        "why": "the loud sanity mutation: with the comparison inverted a floor is honoured exactly when it should be dropped",
        "file": "lib/tea_core/prim.ml",
        "old": "let covers ~(head : t) ~(floor : t) : bool = Int64.compare head floor >= 0",
        "new": "let covers ~(head : t) ~(floor : t) : bool = Int64.compare floor head >= 0",
        # If this one does not redden everywhere it is aimed, the sweep itself
        # is broken. It is the true R20 arm: B8's rolled-back floor survives,
        # swallows the replay as a Duplicate, and the count lands on 1 - the
        # silent loss the whole step exists to close - with no line printed.
        # B4 stays green (an orderly restart's floor equals its head, and
        # equality passes in both directions), which is the pair that tells the
        # two covers mutations apart. Native: G1 first (rb's floor sits STRICTLY
        # below its head), then G2, G4, G7.
        "expect_red": {
            "native": ["G1 floors are adopted under covering waters"],
            "browser": [
                "rollback B8: life 3 boots over the rolled-back root and SAYS SO",
                "the ROLLED-BACK SERVER itself holds 2",
            ],
        },
    },
    {
        "id": "mut-compaction-drops-water",
        "why": "compaction must re-emit each floor at its STORED water: rewritten at bottom, every surviving floor is covered by everything and the boot filter turns itself off with no symptom",
        "file": "lib/tea_server_pack/guard_file.ml",
        # The silent-disable trap: `compact` rebuilds the journal purely from
        # `events_of_kept`, so this costs nothing until the journal first
        # exceeds its cap - at which point the witness is gone for good.
        "old": "-> Guard_sink.Advance { replica; tab; seq; water }))",
        "new": "-> Guard_sink.Advance { replica; tab; seq; water = (ignore water; Tea_core.Prim.Store_water.bottom) }))",
        # No browser scenario writes enough records to trip compaction, which
        # is precisely why the native G5 check exists.
        "expect_red": {"native": ["G5 compaction re-emitted each floor's water"], "browser": []},
    },
    {
        "id": "mut-legacy-water-top",
        "why": "a pre-step-13 record carries NO witness, and bottom is the honest spelling of that: decoded at the top of the lattice it would instead vouch for every floor it meets",
        "file": "lib/tea_server/guard_sink.ml",
        # Turns every in-place upgrade into the opposite of a fleet-wide wipe:
        # the old floors would be honoured against any store, restored or not.
        # (The wipe direction - max_int as a FLOOR - is what the bottom reading
        # avoids; either way the point is that the legacy arm must not invent a
        # witness it never had.) Two executables redden: the codec's own C2 and
        # the boot filter's G3.
        "old": """        advance_validated ~replica ~tab ~seq ~water:Prim.Store_water.bottom
          ~next)""",
        "new": """        advance_validated ~replica ~tab ~seq
          ~water:(Prim.Store_water.of_date Int64.max_int) ~next)""",
        # A step-13 binary never WRITES a tag-1 record, so no browser life can
        # produce one: this is a native-only claim by construction.
        "expect_red": {
            "native": [
                "a valid legacy tag 1 triple decodes at water bottom",
                "G3 a legacy floor is adopted at a real head",
            ],
            "browser": [],
        },
    },
    {
        "id": "mut-filter-not-applied",
        "why": "the verdict is computed but the drop is not applied - the server says the right thing and does the wrong one, the most dangerous plausible slip in this change",
        "file": "lib/tea_server_pack/guard_file.ml",
        # The verdict (and therefore every operator line) is left completely
        # intact, so B8's line stays GREEN and only its count moves. That is
        # the whole reason the line and the behaviour are two checks rather
        # than one conjunct; mut-rollback-line-silent is the other half.
        "old": """          let admitted, verdict =
            Durable_guard.Floors.filter ~head:head_water floors0
          in""",
        "new": """          let admitted, verdict =
            let (a : Durable_guard.Floors.t), (v : verdict) =
              Durable_guard.Floors.filter ~head:head_water floors0
            in
            ((ignore a; floors0), v)
          in""",
        "expect_red": {
            "native": ["G2 the SAME journal bytes drop the rolled-back floor"],
            "browser": ["the ROLLED-BACK SERVER itself holds 2"],
        },
    },
    {
        "id": "mut-rollback-line-silent",
        "why": "the operator line is the OTHER half of the guarantee: a drop nobody is told about leaves an operator restoring backups blind",
        "file": "lib/tea_server_pack/tea_server_pack.ml",
        # Rewrites the sentence's opening while keeping its single %d and its
        # trailing newline-flush, so the drop still happens and only the
        # telemetry moves. Exactly one check may redden.
        "old": '"tea_server_pack: dropped %d delivery floor(s) standing ABOVE their branch heads: the pack root is OLDER than the guard journal',
        "new": '"tea_server_pack: (suppressed) %d delivery floor(s) dropped',
        "expect_red": {
            "native": [],
            "browser": ["rollback B8: life 3 boots over the rolled-back root and SAYS SO"],
        },
    },
    {
        "id": "mut-persist-water-bottom",
        "why": "the floor's witness must be the water of the very commit it de-duplicates: stamped bottom, every floor is trusted forever and R20 is back",
        "file": "lib/tea_server/tea_server.ml",
        # THE DEDICATED EXECUTIONER OF THE BROWSER TIER. No native check runs
        # the real pump against a real pack root, so if B8's checks are ever
        # deleted or weakened this mutation goes unkilled and the end-to-end
        # wiring is uncovered. Bottom floors are always adopted (an absence
        # never manufactures a drop), so life 3 honours the rolled-back floor,
        # swallows the replay, and lands on 1 - the R20 silent loss - without
        # printing the rollback line, because nothing was dropped.
        "old": "persist_taken ~water:o.water n in",
        "new": "persist_taken ~water:(ignore o.water; Prim.Store_water.bottom) n in",
        "expect_red": {
            "native": [],
            "browser": [
                "rollback B8: life 3 boots over the rolled-back root and SAYS SO",
                "the ROLLED-BACK SERVER itself holds 2",
            ],
        },
    },
    {
        "id": "mut-branch-waters-empty",
        "why": "the boot lookup must actually name the branches: an empty list makes every floor unwitnessed-by-absence and drops the lot at every boot",
        "file": "lib/tea_persist/store_core.ml",
        # `S.Branch.list` is still called and its result still bound, so the
        # mutation compiles and proves observation rather than typing (the M2
        # lesson). Every real floor now meets a head of None, which classifies
        # as no_branch: B4's restart double-applies to 3, and B8 drops its
        # floor for the wrong reason - count green, line red.
        "old": """    let* names = S.Branch.list t.repo in
    Lwt_list.map_s
      (fun (name : S.branch) ->
        let* head =
          Lwt.catch
            (fun () -> S.Branch.find t.repo name)
            (fun (_ : exn) -> Lwt.return None)
        in
        Lwt.return (replica_of_name name, water_of_commit_opt head))
      names""",
        "new": """    let* names = S.Branch.list t.repo in
    let (_ : S.branch list) = names in
    Lwt.return []""",
        "expect_red": {
            "native": ["W4 branch_waters names the session"],
            "browser": [
                "the RESTARTED SERVER itself holds 2",
                "rollback B8: life 3 boots over the rolled-back root and SAYS SO",
            ],
        },
    },
    # --- declared equivalents ------------------------------------------------
    #
    # A mutation nothing can observe is not a gap, it is a fact about the
    # design - but only if it is DECLARED and run. `run` names the suites that
    # must execute and stay green; inventing an expectation for one of these
    # would score a MISS and read as a broken sweep.
    {
        "id": "mut-touch-not-restricted",
        "why": "the cap ranks only ADMITTED keys: an unrestricted touch table lets a dropped floor's recency count against a survivor",
        "file": "lib/tea_server_pack/guard_file.ml",
        # Declared EQUIVALENT until the first full sweep (2026-07-28) proved
        # otherwise: G8's companion arm pins exactly this - restrict-touch is
        # what keeps the cap ranking over admitted keys only, so with it gone
        # the eviction decision shifts and the surviving key changes.
        # `events_of_kept` still filter_maps the stray touch key away in the
        # FILE; the observable is the cap decision, not the journal bytes.
        "old": """          let touch1 =
            Key_map.filter
              (fun ((replica, tab) : Key.t) (_ : int) ->
                Durable_guard.Floors.find_stamped ~replica ~tab admitted
                |> Option.is_some)
              touch0
          in""",
        "new": """          let touch1 = touch0 in""",
        "expect_red": {"native": ["G8 companion: the water drop is counted"], "browser": []},
    },
    {
        "id": "EQUIVALENT-water-int64-compare",
        "why": "Store_water.compare IS Int64.compare on the representation: a check that could kill this would be pinning a representation rather than a behaviour",
        "file": "lib/tea_core/prim.ml",
        # Kept as a declared equivalent rather than dropped, because the day
        # the newtype gains a real ordering (a tier tag, a lexicographic pair)
        # this entry is where the sweep will notice it stopped being one.
        "old": """  let covers ~(head : t) ~(floor : t) : bool = Int64.compare head floor >= 0
  let compare = Int64.compare""",
        "new": """  let covers ~(head : t) ~(floor : t) : bool = Int64.compare head floor >= 0
  let compare (a : t) (b : t) : int = Int64.compare a b""",
        "expect_red": {"native": [], "browser": []},
        "run": ["native"],
    },
]


def run(cmd, **kw):
    return subprocess.run(cmd, cwd=REPO, capture_output=True, text=True, **kw)


def build():
    return run(["opam", "exec", "--switch=irmin-tea", "--", "dune", "build"])


def browser():
    """Run the browser harness; return (summary, red_labels)."""
    out = run(["node", "test/browser/smoke.mjs"]).stdout
    red = [ln for ln in out.splitlines() if ln.startswith(("FAIL", "STALE"))]
    summary = out.strip().splitlines()[-1] if out.strip() else "(no output)"
    return summary, red


# The built test executables. `dune build` puts every `(tests)` name here, and
# dune runs them with this as the working directory, so running them from here
# is byte-for-byte what `dune runtest` does.
NATIVE_DIR = REPO / "_build" / "default" / "test"


def native():
    """Run the OCaml suite; return (summary, red_labels).

    Executes each built test executable DIRECTLY rather than going through
    `dune build @runtest`, for two measured reasons. This dune has no
    `--keep-going` (it is not a `dune build` option here), so the alias stops
    at the first failing action and a mutation that reddens two test files
    reports only one of them. And dune caches a passing test: a run whose
    inputs did not change prints nothing at all, which is indistinguishable
    from a run that passed. Executing the exes is deterministic, complete, and
    needs no cache reasoning.

    Each executable is itself fail-fast (`check` exits 1 on the first failure),
    so at most one FAIL line comes back per file - hence the doctrine that a
    native expectation names the EARLIEST check a mutation breaks.
    """
    exes = sorted(NATIVE_DIR.glob("*.exe"))
    if not exes:
        return "(no test executables built)", ["FAIL - the native suite was never built"]
    red, ok = [], 0
    for exe in exes:
        r = subprocess.run([str(exe)], cwd=NATIVE_DIR, capture_output=True, text=True)
        lines = r.stdout.splitlines()
        ok += sum(1 for ln in lines if ln.startswith("ok"))
        failures = [ln for ln in lines if ln.startswith("FAIL")]
        red += [f"{exe.stem}: {ln}" for ln in failures]
        # A test that dies without printing FAIL (an uncaught exception, a
        # signal) is red too, and silently dropping it would read as coverage.
        if r.returncode != 0 and not failures:
            red.append(f"FAIL - {exe.stem} exited {r.returncode} printing no FAIL line")
    return f"{ok} ok across {len(exes)} test executables", red


SUITES = ("native", "browser")
RUNNERS = {"native": native, "browser": browser}


def suites_for(m):
    """A suite runs for a mutation iff it is expected to redden there, or the
    mutation is a declared equivalent that names it in `run`."""
    named = m.get("run", [])
    return [s for s in SUITES if m["expect_red"].get(s) or s in named]


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
    # Only BLOCKED entries are worth announcing: on a named run the rest of
    # the table is unselected, not skipped (and has no "blocked" note).
    skipped = [m for m in MUTATIONS if m.get("blocked") and m not in chosen]
    for m in skipped:
        print(f"SKIP {m['id']}: blocked - {m['blocked']}")

    # Only the suites some chosen mutation actually needs are baselined: a
    # browser-only selection must not pay for the OCaml suite, and vice versa.
    needed = [s for s in SUITES if any(s in suites_for(m) for m in chosen)]
    for s in needed:
        base_summary, base_red = RUNNERS[s]()
        if base_red:
            print(f"{s.upper()} BASELINE IS NOT GREEN - fix that before mutating:\n" + "\n".join(base_red))
            return 1
        print(f"baseline green [{s}] ({base_summary})")
    print()

    verdicts = []
    for m in chosen:
        original, err = apply_mutation(m)
        try:
            if err:
                verdicts.append((m["id"], False, err))
                continue
            built = build()
            if built.returncode != 0:
                # A mutation that dies at COMPILE time proves typing, not
                # observation: the check was never given the chance to fail.
                verdicts.append((m["id"], False, "build failed (mutation is not observable): " + built.stderr.strip()[:200]))
                continue
            detail, ok = "", True
            for s in suites_for(m):
                wanted = m["expect_red"].get(s, [])
                _, red = RUNNERS[s]()
                hit = [w for w in wanted if any(w in ln for ln in red)]
                stray = [ln for ln in red if not any(w in ln for w in wanted)]
                ok = ok and len(hit) == len(wanted) and not stray
                detail += f"{s} red={len(red)}"
                if len(hit) != len(wanted):
                    detail += f"; MISSED {[w for w in wanted if w not in hit]}"
                if stray:
                    detail += f"; STRAY {[x[:60] for x in stray]}"
                detail += "  "
            verdicts.append((m["id"], ok, detail.strip()))
        finally:
            if original is not None:
                (REPO / m["file"]).write_text(original)

    build()
    print()
    for mid, ok, detail in verdicts:
        m = next(x for x in MUTATIONS if x["id"] == mid)
        print(f"{'RED ' if ok else 'MISS'} {mid}: {m['why']}  [{detail}]")
    bad = [v for v in verdicts if not v[1]]
    print(f"\n{len(verdicts) - len(bad)}/{len(verdicts)} mutations caught")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
