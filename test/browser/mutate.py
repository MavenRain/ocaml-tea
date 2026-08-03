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
    # --- Roadmap step 14, D19: the witnessed write path under contention -----
    # Native-only, all of them. The browser tier drives one writer per
    # scenario and this whole family lives in the window between one writer's
    # read and its commit, so smoke.mjs has nothing to say about any of it.
    # Each entry is an inline argument swap or an addition, never a deletion
    # that orphans a binding: warnings 26/27/32 are errors in this build, and
    # a mutation that dies at compile time proves typing rather than
    # observation. contention_test is fail-fast like every other file here, so
    # each label below is the check that ACTUALLY went red when the mutation
    # was hand-driven, not the deepest check it would reach.
    {
        "id": "mut-cas-tests-a-fresh-head",
        "why": "the test-and-set must name the head this writer READ: tested against a freshly read one instead, every witnessed commit is last-write-wins again and the racer's content is gone",
        "file": "lib/tea_persist/store_core.ml",
        # The pre-D19 disease one layer down. The witness still supplies the
        # parents and the base tree, so the commit is well formed and only the
        # CAS is wrong: no hang, no extra round, the loser's content simply
        # vanishes while its commit stays in history. R10 exactly.
        "old": "      let* moved = S.Head.test_and_set s.branch ~test:witness ~set:(Some c) in",
        "new": """      let* head_live = S.Head.find s.branch in
      let* moved = S.Head.test_and_set s.branch ~test:head_live ~set:(Some c) in""",
        "expect_red": {
            "native": ["both writers' content survives an interleaved commit"],
            "browser": [],
        },
    },
    {
        "id": "mut-resolve-theirs-is-ours",
        "why": "`theirs` is the model read through the commit that WON the race: handed ours instead, the loop merges a writer with itself and the round it just lost leaves nothing in content",
        "file": "lib/tea_persist/store_core.ml",
        # The join still runs and still returns, so the shape of the fix is
        # untouched and only its inputs are wrong - the failure mode a merge
        # helper refactor actually produces.
        "old": "            let r = resolve ~ancestor:(Some ancestor) ~ours ~theirs in",
        "new": "            let r = resolve ~ancestor:(Some ancestor) ~ours ~theirs:ours in",
        "expect_red": {
            "native": ["both writers' content survives an interleaved commit"],
            "browser": [],
        },
    },
    {
        "id": "mut-resolve-ours-is-theirs",
        "why": "`ours` is the model the pump has already acked: handed theirs instead, the reconcile keeps the racer and drops the write the client was told existed, which is the D16 silent loss",
        "file": "lib/tea_persist/store_core.ml",
        # The mirror of mut-resolve-theirs-is-ours, and the one that matters
        # more: this direction loses an ACKED effect rather than an unacked
        # one. `ours` stays used by the tree, the None arm and the outcome, so
        # nothing is orphaned.
        "old": "            let r = resolve ~ancestor:(Some ancestor) ~ours ~theirs in",
        "new": "            let r = resolve ~ancestor:(Some ancestor) ~ours:theirs ~theirs in",
        "expect_red": {
            "native": ["both writers' content survives an interleaved commit"],
            "browser": [],
        },
    },
    {
        "id": "mut-ancestor-frozen",
        "why": "the ancestor is a function of the ROUND: frozen at the writer's own read, round two compares an ours that already absorbed round one against a base predating it and reads a re-add as a delete",
        "file": "lib/tea_persist/store_core.ml",
        # Only a three-way app can see this, and only from the second round on,
        # which is why C7 lands its third writer from inside the merge itself.
        # A CRDT join ignores the ancestor entirely, so C1 stays green and the
        # earliest red is C7.
        "old": "            let r = resolve ~ancestor:(Some ancestor) ~ours ~theirs in",
        "new": "            let r = resolve ~ancestor:(Some b.model) ~ours ~theirs in",
        "expect_red": {"native": ["two contention rounds use the true ancestor"], "browser": []},
    },
    {
        "id": "mut-step-rereads-after-interpose",
        "why": "the server seam must commit against the token its own read minted: re-minted after the step, the witness names a head this step never read and a mid-step writer is erased by the Cmd tail",
        "file": "lib/tea_server/tea_server.ml",
        # The seam-level twin of mut-cas-tests-a-fresh-head: the store keeps
        # its whole reconcile loop and is simply lied to about what was read.
        # `based` stays used by `St.based_model`, so nothing is orphaned.
        "old": """        let* () = interpose () in
        let* (landed : St.committed) = commit based ~msg model' in""",
        "new": """        let* () = interpose () in
        let* reread = St.load_based s in
        let* (landed : St.committed) = commit reread ~msg model' in""",
        "expect_red": {
            "native": ["a mid-step writer is not erased by a Cmd-tail step"],
            "browser": [],
        },
    },
    {
        "id": "mut-water-of-the-witness",
        "why": "the water an outcome carries is the WINNING round's mint: reporting the witness's water instead stamps the delivery floor with a commit this writer did not land, which is the forged witness D18 exists to prevent",
        "file": "lib/tea_persist/store_core.ml",
        # Not literally the denied round's own water - reaching that needs a
        # ref threaded around the loop, which is two edits and the driver
        # applies one anchor - but the same class: any water other than the
        # mint of the round that moved the head. The uncontended case has no
        # witness and is left alone, so C6 stays green and C4, where the
        # ordering between two writers' waters is asserted, is the earliest
        # red.
        "old": """      if moved then
        Lwt.return
          ( c
          , ({ water = Tea_core.Prim.Store_water.of_date (S.Info.date (S.Commit.info c))""",
        "new": """      if moved then
        Lwt.return
          ( c
          , ({ water =
                 Tea_core.Prim.Store_water.of_date
                   (S.Info.date (S.Commit.info (Option.value witness ~default:c)))""",
        "expect_red": {"native": ["the winning round's water is the newest mint"], "browser": []},
    },
    {
        "id": "mut-coalesced-append-rereads",
        "why": "the coalesced append must reconcile against the witness it was HANDED: re-reading the head first restores the three-attempt retry's disease, and a writer landing between the caller's read and the append is clobbered",
        "file": "lib/tea_persist/store_core.ml",
        # Scoped to the coalescer, so the plain based path is untouched and
        # C1-C12 stay green: this is the entry that says the append inherited
        # the witness rather than merely inheriting the loop.
        "old": "    let* (c, landed) = commit_witnessed b ~label:(Codec.msg_to_label msg) model in",
        "new": """    let* reread = load_based b.session in
    let* (c, landed) = commit_witnessed reread ~label:(Codec.msg_to_label msg) model in""",
        "expect_red": {
            "native": ["a coalesced append reconciles instead of clobbering"],
            "browser": [],
        },
    },
    {
        "id": "EQUIVALENT-amend-consults-live-head",
        "why": "the amend decision is taken against the witness rather than a live head read, and nothing here can currently tell: a denied amend falls into an append that reconciles to the same landed state",
        "file": "lib/tea_persist/store_core.ml",
        # Hand-driven: the whole suite stays green (795 ok, 0 red).
        #
        # What IS observable is the same either way. With the witness, the
        # amend is attempted, its test-and-set is denied by the foreign head,
        # the run is sealed and `append_commit` reconciles; with a live head
        # read the amend is never attempted and the same `append_commit` runs
        # with the same witness. Both land three commits, both keep the
        # foreign writer as the parent, both fold the run's likes to 2, and
        # both leave the run pointing at the commit that actually landed.
        #
        # What is NOT observable is the difference between them: the witness
        # path mints an amend commit that loses its CAS and is left
        # unreferenced, and it burns one clock tick doing so. Catching this
        # needs a check that looks at one of those two - the orphaned commit
        # (count the objects the repo holds, or assert the amend was tried at
        # all) or the tick (assert the landed water is the branch's second
        # mint and not its third). The regression it stands for is real and
        # sits outside what a single-threaded test can schedule: a head that
        # moves BACK onto this coalescer's own minted commit between the
        # caller's read and the decision would be amended against a commit
        # this writer never saw.
        "old": """      let amend =
        (* Pure decision: amendable iff the head is the commit we minted and
           the policy folds the run's Msg with the incoming one.

           Taken against the WITNESS since D19, not against a fresh head read.
           The old read happened after the caller's load, so a writer landing
           in that gap flipped the run to the append path while the model in
           hand was already stale: the amend test was right and its timing was
           wrong. Asking the witness makes the decision and the test-and-set
           agree about which commit this writer actually saw. *)
        Option.bind b.head (fun (h : S.commit) ->""",
        "new": """      let* live = S.Head.find s.branch in
      let amend =
        Option.bind live (fun (h : S.commit) ->""",
        "expect_red": {"native": [], "browser": []},
        "run": ["native"],
    },
    {
        "id": "mut-absent-head-merges-init",
        "why": "an absent head is a reap, not a competitor: resolved against the app's INITIAL model, a three-way policy reads the empty branch as 'theirs deleted everything' and wipes the writer's own field on the way back in",
        "file": "lib/tea_persist/store_core.ml",
        # The arm exists precisely so [gather]'s "absent reads as init" never
        # reaches the resolver. Re-attempting as a fresh root is the only
        # loss-free answer, and this mutation takes the other one.
        "old": "            attempt None ancestor ours label (rounds + 1))",
        "new": """            let r = resolve ~ancestor:(Some ancestor) ~ours ~theirs:(fst A.init) in
            attempt None ancestor (resolved_model r) (resolved_label label r) (rounds + 1))""",
        "expect_red": {
            "native": ["a reap between the read and the commit neither wipes nor resurrects blindly"],
            "browser": [],
        },
    },
    {
        "id": "EQUIVALENT-round-base-tree-from-branch",
        "why": "the round's base tree comes from the witness, and no check can currently tell: [scatter] rewrites every model path, so the base carries only what the model does not write, and nothing in the suite puts anything there",
        "file": "lib/tea_persist/store_core.ml",
        # Hand-driven twice. The edit below (the live branch tree instead of
        # the witness's) leaves the suite green, and so does the strictly
        # stronger `S.Tree.empty ()`, which discards the base entirely - so
        # this is not a near miss, it is the base tree being unobservable end
        # to end.
        #
        # The pack pair was the hoped-for red: store_core.ml:517-524 records
        # that a root tree rebuilt from [S.tree] and re-saved does NOT survive
        # a pack close and reopen, and C11/C12 exist to decide whether the
        # based path may use that door. It may not, and it does not: this
        # mutation still lands through [S.Commit.v], which survives the round
        # trip whatever the base tree was read from. Catching it needs a check
        # that writes a path the model does not own - a foreign key, or a D6
        # field the app has since dropped - and asserts it is still there
        # after a reconciled round.
        #
        # The discarded call keeps `base_tree` used on purpose: warning 32 is
        # an error in this build and an orphaned binding would be reported as
        # "not observable" rather than as the equivalent it is.
        "old": "      let* tree = scatter s.exploded (base_tree witness) ours in",
        "new": """      let (_ : S.tree) = base_tree witness in
      let* live_base = S.tree s.branch in
      let* tree = scatter s.exploded live_base ours in""",
        "expect_red": {"native": [], "browser": []},
        "run": ["native"],
    },
    {
        "id": "mut-conflict-keeps-theirs",
        "why": "a refused merge must keep OURS: the Msg being committed is one the pump has already taken and is about to ack, so keeping theirs acknowledges an effect the store does not hold",
        "file": "lib/tea_persist/store_core.ml",
        # The Three_way error arm only. `ours` stays used by the join and by
        # the last-write-wins arm, so nothing is orphaned, and the label still
        # carries the reason - which is what tells this apart from
        # mut-conflict-reason-dropped even though both redden C8.
        "old": "        ~error:(fun (reason : string) -> Conflicted { model = ours; reason })",
        "new": "        ~error:(fun (reason : string) -> Conflicted { model = theirs; reason })",
        "expect_red": {
            "native": ["a declared conflict keeps ours, keeps theirs as parent, and says why"],
            "browser": [],
        },
    },
    {
        "id": "mut-lww-declares-theirs",
        "why": "the last-write-wins arm is the R10c pin: declaring theirs makes 'last write' name the write that lost, and the acked model is gone from content while D19 still claims the arm is honest",
        "file": "lib/tea_persist/store_core.ml",
        "old": "    | Tea_core.Merge_spec.Last_write_wins -> Declared ours",
        "new": "    | Tea_core.Merge_spec.Last_write_wins -> Declared theirs",
        "expect_red": {
            "native": ["last-write-wins keeps ours with theirs as parent"],
            "browser": [],
        },
    },
    {
        "id": "mut-conflict-reason-dropped",
        "why": "the app's REASON is the audit trail: dropped from the label, a declared loss leaves the branch saying only that something was committed, and the log stops being the place the loss can be found",
        "file": "lib/tea_persist/store_core.ml",
        # A typed wildcard rather than a dropped field, so the reason stops
        # travelling without warning 27 turning the mutation into a build
        # failure. The model still lands, which is the half C8 keeps green
        # here and mut-conflict-keeps-theirs reddens.
        "old": '    | Conflicted { model = (_ : A.model); reason } -> label ^ " [conflict: " ^ reason ^ "]"',
        "new": "    | Conflicted { model = (_ : A.model); reason = (_ : string) } -> label",
        "expect_red": {
            "native": ["a declared conflict keeps ours, keeps theirs as parent, and says why"],
            "browser": [],
        },
    },
    # ---- roadmap step 15 (D20, rpc exactly-once): the spec 5.4 table as s15-*.
    # The ids are namespaced because this file's own M1-M4 predate the step-15
    # numbering. Every recipe below was applied and watched by hand during I2-I6
    # (2026-07-30/31, logs in ~/Documents/ocaml-tea-step15-harness/) EXCEPT
    # s15-M5, whose first confirmation is this driver. The hand runs mostly
    # exercised only each mutant's killer suite; where a collateral red in a
    # sibling suite is certain by construction it is named too, and the first
    # full driver sweep is the arbiter for the remainder.
    # ---- step-15 adversarial-review fixes (F1-F4): the recipes that confirm
    # the review-pass tests, numbered on from s15-M17. First confirmed by the
    # 2026-08-02 fix pass (rerun-fix.log in the step-15 harness dir).
    {
        "id": "s15-M18",
        "why": "the Pending grace drains under the tab's own polling, never silently holds",
        "file": "lib/tea_server/reply_cache.ml",
        "old": "&& Int64.compare polls ticked.pending_grace < 0",
        "new": "&& 0 = 0",
        "expect_red": {"native": [
            "the window drains under its own polling: past the budget it reads Gone",
        ], "browser": []},
    },
    {
        "id": "s15-M19",
        "why": "a fresh window answers Busy from its first poll: the budget starts full",
        "file": "lib/tea_server/reply_cache.ml",
        "old": "Pending { seq; polls = 0L }",
        "new": "Pending { seq; polls = 1000000L }",
        "expect_red": {"native": [
            "a Pending entry at the same seq reads Busy inside the grace",
            "a duplicate inside the Pending window is refused 503, not answered 200",
            "the 503 carries no keyed reply body at all",
        ], "browser": []},
    },
    {
        "id": "s15-M20",
        "why": "settle compares seqs: a stale settle must not destroy a newer window",
        "file": "lib/tea_server/reply_cache.ml",
        # "> 0" to "> 99" rather than deleting the guard, so [entry_seq] stays
        # referenced and the kill is behavioral, not a warning-32 build death.
        "old": "if Msg_seq.compare (entry_seq s.entry) seq > 0 then t else install ()",
        "new": "if Msg_seq.compare (entry_seq s.entry) seq > 99 then t else install ()",
        "expect_red": {"native": [
            "a stale settle is discarded: the newer Pending still reads Busy",
        ], "browser": []},
    },
    {
        "id": "s15-M21",
        "why": "the barrier's failure arm releases the take, or the retry is refused forever",
        "file": "lib/tea_server/tea_server.ml",
        "old": "              Durable_guard.release o.guard ~replica:o.floor_replica ~tab:floor ~seq;",
        "new": "              ();",
        "expect_red": {"native": [
            "the retry of a released delivery reads Fresh and answers its own reply",
            "the released seq's effect landed exactly once, on the retry",
        ], "browser": []},
    },
    {
        "id": "s15-M22",
        "why": "the seq parse accepts exactly the canonical print, no aliased spellings",
        "file": "lib/tea_rpc/tea_rpc.ml",
        "old": r'''        if String.equal (Int.to_string n) s then Tea_core.Prim.Msg_seq.of_int n
        else None''',
        "new": "        Tea_core.Prim.Msg_seq.of_int n",
        "expect_red": {"native": [
            "Key.of_string rejects 0, a sign, padding, hex, an underscore, and leading zeros as Bad_seq",
        ], "browser": []},
    },
    {
        "id": "s15-M23",
        "why": "release un-takes exactly the failed seq, conditionally on the high water",
        "file": "lib/tea_server/replay_guard.ml",
        # Inverted rather than deleted, so [restore] stays referenced and the
        # kill is behavioral, not a warning-26 build death.
        "old": "if Int.equal (Msg_seq.compare high seq) 0 then restore () else t",
        "new": "if Int.equal (Msg_seq.compare high seq) 0 then t else restore ()",
        "expect_red": {"native": [
            "a released first-seq take reads Fresh again, not Duplicate",
            "the retry of a released delivery reads Fresh and answers its own reply",
            "the released seq's effect landed exactly once, on the retry",
        ], "browser": []},
    },
    {
        "id": "s15-M1",
        "why": "the guard's verdict precedes the effect: a duplicate inside the window must not commit twice",
        "file": "lib/tea_server/tea_server.ml",
        # The confirmed shape: hoist the handler call ABOVE the take and delete
        # the Fresh arm's own call. The hoisted binding stays used (the Fresh
        # arm reads resp/water), so nothing is orphaned into a warning-27 build
        # kill. A duplicate and a gap now apply before being refused.
        "old": r'''        match Durable_guard.take o.guard ~replica:o.floor_replica ~tab:floor ~seq with
        | Replay_guard.Gapped ->
          (* Nothing consumed, nothing applied, no state touched. The seq space
             is dense per tab (the client queue is one-in-flight over dense
             numbering), so an honest client never gaps. The WS tier's
             silent-ignore arm is unavailable because HTTP must answer
             something, and answering on the 200 channel would type a protocol
             violation into every endpoint's contract. *)
          Dream.respond ~status:`Bad_Request ~headers:text_plain "rpc delivery gap"
        | Replay_guard.Duplicate (_ : Tea_core.Prim.Msg_seq.t) ->
          (* Never invoke the handler, never persist: the effect already
             happened, and this arm exists only to answer for it. *)
          (match Reply_cache.Cell.find o.replies ~tab:floor ~seq ~endpoint with
          | Reply_cache.Busy ->
            (* The taken delivery is still inside its window. 503 means "the
               effect's fate is unknown at this instant, ask again", a
               transport-channel meaning the client runtime's 5xx retry arm
               consumes; it never reaches the app's [expect]. *)
            Dream.respond ~status:`Service_Unavailable ~headers:text_plain
              "rpc delivery in flight"
          | Reply_cache.Original bytes ->
            (* Byte-identical to what the one taken delivery computed: replayed
               verbatim, never recomputed at replay time. *)
            ok_json bytes
          | Reply_cache.Gone ->
            (* Effect certain, value lost. The typed arm the whole envelope
               exists for; the client surfaces it as [Applied_reply_lost]. *)
            ok_json replayed_body)
        | Replay_guard.Fresh (_ : Tea_core.Prim.Msg_seq.t) ->
          (* [mark_pending] in the SAME continuation as the verdict, with no
             Lwt bind between them: a yield here would let a concurrent
             duplicate read "no entry" and be told [Replayed] for an effect
             still running, which is the one answer that would be a lie. *)
          Reply_cache.Cell.mark_pending o.replies ~tab:floor ~seq;
          Lwt.try_bind
            (fun () ->
              let* () = on_taken () in
              h.handle_keyed ep req)''',
        "new": r'''        let* resp, water = h.handle_keyed ep req in
        match Durable_guard.take o.guard ~replica:o.floor_replica ~tab:floor ~seq with
        | Replay_guard.Gapped ->
          (* Nothing consumed, nothing applied, no state touched. The seq space
             is dense per tab (the client queue is one-in-flight over dense
             numbering), so an honest client never gaps. The WS tier's
             silent-ignore arm is unavailable because HTTP must answer
             something, and answering on the 200 channel would type a protocol
             violation into every endpoint's contract. *)
          Dream.respond ~status:`Bad_Request ~headers:text_plain "rpc delivery gap"
        | Replay_guard.Duplicate (_ : Tea_core.Prim.Msg_seq.t) ->
          (* Never invoke the handler, never persist: the effect already
             happened, and this arm exists only to answer for it. *)
          (match Reply_cache.Cell.find o.replies ~tab:floor ~seq ~endpoint with
          | Reply_cache.Busy ->
            (* The taken delivery is still inside its window. 503 means "the
               effect's fate is unknown at this instant, ask again", a
               transport-channel meaning the client runtime's 5xx retry arm
               consumes; it never reaches the app's [expect]. *)
            Dream.respond ~status:`Service_Unavailable ~headers:text_plain
              "rpc delivery in flight"
          | Reply_cache.Original bytes ->
            (* Byte-identical to what the one taken delivery computed: replayed
               verbatim, never recomputed at replay time. *)
            ok_json bytes
          | Reply_cache.Gone ->
            (* Effect certain, value lost. The typed arm the whole envelope
               exists for; the client surfaces it as [Applied_reply_lost]. *)
            ok_json replayed_body)
        | Replay_guard.Fresh (_ : Tea_core.Prim.Msg_seq.t) ->
          (* [mark_pending] in the SAME continuation as the verdict, with no
             Lwt bind between them: a yield here would let a concurrent
             duplicate read "no entry" and be told [Replayed] for an effect
             still running, which is the one answer that would be a lie. *)
          Reply_cache.Cell.mark_pending o.replies ~tab:floor ~seq;
          Lwt.try_bind
            (fun () ->
              let* () = on_taken () in
              Lwt.return (resp, water))''',
        "expect_red": {"native": [
            "a duplicate delivered INSIDE the window lands NO second commit",
            "the whole window landed exactly one commit",
            "the taken delivery's reply counts the branch it actually committed",
            "a gapped delivery applies nothing",
            "a gapped delivery consumes nothing else",
            "and it commits (so the replay above was de-duplication, not a dead channel)",
            "and it applied nothing: the commit count is unmoved",
            "and that refusal applied nothing",
            "a second delivery of the re-applied key does not apply AGAIN",
            "the rejected delivery committed nothing",
            "the released seq's effect landed exactly once, on the retry",
        ], "browser": []},
    },
    {
        "id": "s15-M2",
        "why": "a taken delivery's floor reaches the journal, not just the in-process mirror",
        "file": "lib/tea_server/tea_server.ml",
        # The water binding is consumed by a typed wildcard so the deletion is
        # not a warning-27 build kill. The kill lands UPSTREAM of the spec's
        # designated T3 arm: rpc_pack_once_test's restart section reds first
        # and its T5 die exits the file before T3's arm runs. The two
        # ordering-arm labels are the same absence seen from rpc_once_test's
        # gated sink: nothing ever appends, so the 200 resolves with the gate
        # still closed and the append count stays zero.
        "old": r'''              let* persisted =
                Durable_guard.persist o.guard ~replica:o.floor_replica ~tab:floor ~seq
                  ~water
              in''',
        "new": r'''              let* persisted =
                let (_ : Tea_core.Prim.Store_water.t) = water in
                Lwt.return (Ok ())
              in''',
        "expect_red": {"native": [
            "the restart adopts the keyed floor from the journal",
            "life 1's keyed delivery wrote a floor record",
            "the keyed 200 is still unresolved while the floor's append is held open",
            "and the acknowledgement followed exactly one append",
        ], "browser": []},
    },
    {
        "id": "s15-M3",
        "why": "the persisted floor carries the commit's own water, not a bottom claim",
        "file": "lib/tea_server/tea_server.ml",
        # The handler's water is renamed _water in the same span, or the
        # mutation dies as warning 27 at compile time (measured; a build-error
        # kill is not a kill). Bottom floors are always adopted unwitnessed,
        # so adoption itself stays green and only the witness checks red.
        "old": r'''            (fun (resp, water) ->
              let body = enveloped resp in
              let* persisted =
                Durable_guard.persist o.guard ~replica:o.floor_replica ~tab:floor ~seq
                  ~water
              in''',
        "new": r'''            (fun (resp, _water) ->
              let body = enveloped resp in
              let* persisted =
                Durable_guard.persist o.guard ~replica:o.floor_replica ~tab:floor ~seq
                  ~water:Tea_core.Prim.Store_water.bottom
              in''',
        "expect_red": {"native": [
            "with its store witness matched against this boot's branch head",
            "the floor's witness is not the bottom claim of nothing",
            "and the witness IS the canonical commit's own water, not a second clock",
            "and drops ONLY that one: the witnessed floor is still adopted",
        ], "browser": []},
    },
    {
        "id": "s15-M4",
        "why": "the reply cache answers a seq only its own bytes, never the newest body",
        "file": "lib/tea_server/reply_cache.ml",
        # || true rather than a deletion: dropping the conjunct orphans the
        # pattern binding into a warning-27 build kill.
        "old": r'''             Int.equal (Msg_seq.compare seq stored_seq) 0
             && String.equal endpoint stored_endpoint''',
        "new": r'''             (true || Int.equal (Msg_seq.compare seq stored_seq) 0)
             && String.equal endpoint stored_endpoint''',
        "expect_red": {"native": [
            "a duplicate of an older seq reads Gone, never the newest bytes",
            "the victim's consumed seq answers Replayed, never a newer seq's bytes",
        ], "browser": []},
    },
    {
        "id": "s15-M5",
        "why": "the key grammar is exact: a 31-hex tab is refused, not padded into a namespace",
        "file": "lib/tea_rpc/tea_rpc.ml",
        # Padding rather than skipping the validation: acceptance has to
        # produce a well-typed Key, and a 31-hex tab that round-trips into
        # SOMEONE'S floor is exactly the forgery the grammar row pins.
        "old": r'''    | [ tab; seq ] ->
      Result.bind
        (Tea_core.Prim.Tab_id.of_string tab''',
        "new": r'''    | [ tab; seq ] ->
      let tab = if String.length tab = 31 then tab ^ "0" else tab in
      Result.bind
        (Tea_core.Prim.Tab_id.of_string tab''',
        "expect_red": {"native": [
            "Key.of_string rejects a 31-hex tab as Bad_tab Wrong_length",
        ], "browser": []},
    },
    {
        "id": "s15-M6",
        "why": "a gap is refused on the 400 channel, never answered as a keyed reply",
        "file": "lib/tea_server/tea_server.ml",
        "old": r'''          Dream.respond ~status:`Bad_Request ~headers:text_plain "rpc delivery gap"''',
        "new": r'''          ok_json replayed_body''',
        "expect_red": {"native": [
            "a gapped delivery is refused 400",
        ], "browser": []},
    },
    {
        "id": "s15-M7",
        "why": "a retry re-sends the RECORDED seq: the queue head never renumbers",
        "file": "lib/tea_client/rpc_delivery.ml",
        # Spec 5.4 designated a browser kill (B9: a fresh-seq retry as a second
        # take), but the full driver sweep measured B9 green under this mutant;
        # the native head-numbering pin is the one real kill.
        "old": r'''  match t.queue with
  | [] -> None
  | frame :: (_ : (Msg_seq.t * 'msg entry) list) -> Some frame''',
        "new": r'''  match t.queue with
  | [] -> None
  | ((_ : Msg_seq.t), e) :: (_ : (Msg_seq.t * 'msg entry) list) -> Some (t.next, e)''',
        "expect_red": {"native": [
            "the first recorded call is numbered one and is the head",
        ], "browser": []},
    },
    {
        "id": "s15-M8",
        "why": "the rpc journal lives under .guard/rpc: the two channels never share a ledger",
        "file": "lib/tea_server_pack/tea_server_pack.ml",
        # Both positive controls ("the reopened ... channel adopts the floor it
        # wrote") stayed green under the hand run: the reds below are absences
        # stated beside presences, so none can be explained by an empty ledger.
        "old": r'''      ~dir:(Filename.concat guard_dir "rpc")''',
        "new": r'''      ~dir:guard_dir''',
        "expect_red": {"native": [
            "the rpc journal is a file UNDER .guard/rpc, so one restore keeps both",
            "the rpc channel did not inherit the websocket channel's floor",
            "the websocket channel did not inherit the rpc channel's floor",
            "each ledger holds exactly the one floor its own channel wrote",
        ], "browser": []},
    },
    {
        "id": "s15-M9",
        "why": "mirror eviction takes the least-recently-TOUCHED floor, not the freshest",
        "file": "lib/tea_server/durable_guard.ml",
        "old": r'''                if Int64.compare e.tick best < 0 then''',
        "new": r'''                if Int64.compare e.tick best > 0 then''',
        "expect_red": {"native": [
            "THE POINT: the victim is the least-recently-TOUCHED floor",
        ], "browser": []},
    },
    {
        "id": "s15-M10",
        "why": "a Read_only endpoint rides the Bare channel: classification is kind-driven",
        "file": "lib/tea_rpc/tea_rpc.ml",
        "old": r'''    | Read_only ->
      Tea_core.Cmd.http ~path ~body ~expect:(fun wire -> reply (decode_bare e wire))''',
        "new": r'''    | Read_only ->
      Tea_core.Cmd.http_keyed ~path ~body ~expect:(fun wire -> reply (decode_bare e wire))''',
        "expect_red": {"native": [
            "call puts a Read_only endpoint on the Bare channel",
            "a Read_only call lowers as the bare delivery contract",
        ], "browser": []},
    },
    {
        "id": "s15-M11",
        "why": "the keyed 200 is written strictly after the persist attempt completes",
        "file": "lib/tea_server/tea_server.ml",
        # The mutant that survived the WHOLE pre-existing suite: an un-awaited
        # journal append still completes on the next scheduler pump, so every
        # after-the-fact read agrees with the pristine pipeline. Only the
        # ordering arm's gated sink can see it, by holding the response as a
        # raw promise and asserting it is still asleep.
        "old": r'''              let* persisted =
                Durable_guard.persist o.guard ~replica:o.floor_replica ~tab:floor ~seq
                  ~water
              in
              let () =
                Result.fold persisted
                  ~ok:(fun () -> ())
                  ~error:(fun (e : Guard_sink.err) ->
                    (* One stderr line and in-process semantics, never a failed
                       response: the effect landed, and refusing to answer for it
                       would lose the reply on top of the floor. The pump
                       precedent. *)
                    let reason =
                      match e with
                      | Guard_sink.Sink_closed -> "sink closed"
                      | Guard_sink.Io reason -> reason
                    in
                    Printf.eprintf
                      "tea_server: rpc delivery floor NOT recorded (%s); this delivery keeps in-process semantics and may re-apply as a visible duplicate after a restart\n%!"
                      reason)
              in
              Reply_cache.Cell.settle o.replies ~tab:floor ~seq ~endpoint ~body;
              (* The 200 IS this channel's acknowledgement, and it is written
                 strictly after the persist attempt completes, on BOTH persist
                 outcomes. Answering first would acknowledge a delivery whose floor
                 might never reach the journal. *)
              ok_json body''',
        "new": r'''              let (_ : (unit, Guard_sink.err) result Lwt.t) =
                Durable_guard.persist o.guard ~replica:o.floor_replica ~tab:floor ~seq
                  ~water
              in
              Reply_cache.Cell.settle o.replies ~tab:floor ~seq ~endpoint ~body;
              ok_json body''',
        "expect_red": {"native": [
            "the keyed 200 is still unresolved while the floor's append is held open",
        ], "browser": []},
    },
    {
        "id": "s15-M12",
        "why": "the origin gate guards the keyed path too, not only the legacy routes",
        "file": "lib/tea_server/tea_server.ml",
        # Anchored on dispatch_read_only: the legacy [routes] carries a
        # byte-identical gate block whose Read_only arm reads
        # `dispatch request` instead. Forging the origin from Host (rather
        # than inverting the gate) keeps every same-origin arm green, so the
        # one red is the gate's absence and nothing else.
        "old": r'''          | Read_only -> dispatch_read_only request
          | Mutating ->
            Tea_safe.Origin_gate.check
              ~origin:(Dream.header request "Origin")''',
        "new": r'''          | Read_only -> dispatch_read_only request
          | Mutating ->
            Tea_safe.Origin_gate.check
              ~origin:
                (Dream.header request "Host"
                |> Option.map (fun (h : string) -> "http://" ^ h))''',
        "expect_red": {"native": [
            "a cross-origin mutating POST is refused 403",
        ], "browser": []},
    },
    {
        "id": "s15-M13",
        "why": "the floor namespace is session-scoped: a forged tab cannot reach the victim's floor",
        "file": "lib/tea_server/tea_server.ml",
        # An if-true swap so session_id_hex stays used; deleting it outright is
        # a warning kill on the parameter's one consumer.
        "old": r'''    "tea-rpc-floor\000" ^ session_id_hex ^ "\000"''',
        "new": r'''    "tea-rpc-floor\000" ^ (if true then "" else session_id_hex) ^ "\000"''',
        "expect_red": {"native": [
            "a forged client tab under another cookie is Fresh in its OWN namespace",
            "a forged client tab under another cookie applies under its own floor",
        ], "browser": []},
    },
    {
        "id": "s15-M14",
        "why": "a Pending hit answers 503, never Replayed: the effect's fate is not yet known",
        "file": "lib/tea_server/tea_server.ml",
        "old": r'''            Dream.respond ~status:`Service_Unavailable ~headers:text_plain
              "rpc delivery in flight"''',
        "new": r'''            ok_json replayed_body''',
        "expect_red": {"native": [
            "a duplicate inside the Pending window is refused 503, not answered 200",
            "the 503 carries no keyed reply body at all",
        ], "browser": []},
    },
    {
        "id": "s15-M15",
        "why": "the reply cache never forges across endpoints sharing a floor tab",
        "file": "lib/tea_server/reply_cache.ml",
        "old": r'''             Int.equal (Msg_seq.compare seq stored_seq) 0
             && String.equal endpoint stored_endpoint''',
        "new": r'''             Int.equal (Msg_seq.compare seq stored_seq) 0
             && (true || String.equal endpoint stored_endpoint)''',
        "expect_red": {"native": [
            "the reply cache never forges: an endpoint mismatch reads Gone",
        ], "browser": []},
    },
    {
        "id": "s15-M16",
        "why": "the key header literal is pinned: client and server cannot drift apart silently",
        "file": "lib/tea_rpc/tea_rpc.ml",
        # Both tiers read Tea_rpc.key_header, so a consistent drift breaks no
        # traffic anywhere: the single literal pin is the ONLY thing that can
        # red, which is exactly the T20/G9 argument for keeping it.
        "old": r'''let key_header = "x-tea-key"''',
        "new": r'''let key_header = "x-tea-kev"''',
        "expect_red": {"native": [
            "the key header literal is x-tea-key",
        ], "browser": []},
    },
    {
        "id": "s15-M17",
        "why": "the mirror default is derived (4x Cell, max journal cap), not a constant",
        "file": "lib/tea_server/durable_guard.ml",
        # The product is still computed and discarded so sessions/tabs/
        # journal_cap all stay consumed. Instance-vs-derivation compares move
        # together and stay green (the I4c note); what reds is every check
        # that pins the derivation against an independently stated cap.
        "old": r'''  Int.max
    (4 * Replay_guard.Bound.to_int sessions * Replay_guard.Bound.to_int tabs)
    journal_cap
  |> Replay_guard.Bound.of_int
  |> Option.value ~default:sessions''',
        "new": r'''  let (_ : int) =
    (4 * Replay_guard.Bound.to_int sessions * Replay_guard.Bound.to_int tabs)
    + journal_cap
  in
  Replay_guard.Bound.of_int 8 |> Option.value ~default:sessions''',
        "expect_red": {"native": [
            "the rpc mirror out-remembers its Cell by the factor of four",
            "the rpc mirror holds every floor its journal can replay at boot",
            "the websocket mirror holds every floor its journal can replay at boot",
            "the default is four times the Cell it seeds",
            "the rpc channel's mirror clears its own journal cap",
            "the websocket channel's mirror clears its own, much larger population",
            "so the two channels were not handed one shared bound",
        ], "browser": []},
    },
    {
        "id": "s15-M-B9",
        "why": "a cached reply is replayed byte-identical, not degraded to the reply-lost marker",
        "file": "lib/tea_server/tea_server.ml",
        # _bytes because a plain unused binding is a warning-27 build kill.
        # The B9 retry and same-key checks stayed green under the hand run,
        # which is the evidence the seven browser checks are independent.
        "old": r'''          | Reply_cache.Original bytes ->
            (* Byte-identical to what the one taken delivery computed: replayed
               verbatim, never recomputed at replay time. *)
            ok_json bytes''',
        "new": r'''          | Reply_cache.Original _bytes ->
            (* Byte-identical to what the one taken delivery computed: replayed
               verbatim, never recomputed at replay time. *)
            ok_json replayed_body''',
        "expect_red": {"native": [
            "a retry answers the ORIGINAL bytes, not a recompute",
            "the retry after the window answers the ORIGINAL bytes",
            "the retry replays the taken delivery's OWN bytes, not a recompute",
        ], "browser": [
            "B9: the reply is dropped, the client retries, and the app renders the cached count",
            "B9: the server counts exactly one tag on the canonical document",
            "B9: the retry was answered with the byte-identical cached reply",
            "B9: the app saw a real reply, not the reply-lost marker (the cache held)",
        ]},
    },
    {
        "id": "s15-M-B10",
        "why": "a Cell miss consults the durable mirror: the journal floor survives a restart",
        "file": "lib/tea_server/durable_guard.ml",
        # Browser-only on purpose, matching the hand confirmation: B9 stays
        # GREEN under this mutant (its retry hits a warm in-process Cell and
        # never consults the mirror), which is the measured proof the two
        # scenarios are not redundant. The _floor rename is load-bearing:
        # warning 27 is an error here, and keeping the touch keeps
        # t/replica/tab used. The durable B4 line is the websocket channel
        # seen through the same consult path.
        "old": r'''      (fun (floor : Msg_seq.t) ->
        Replay_guard.Cell.seed t.cell ~replica ~tab ~high:floor;''',
        "new": r'''      (fun (_floor : Msg_seq.t) ->''',
        "expect_red": {"native": [], "browser": [
            "durable B4: the RESTARTED SERVER itself holds 2",
            "B10: the retry crosses the restart and the app surfaces the reply-lost marker",
            "B10: life 2 refused the duplicate from the journal alone",
        ]},
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

# Per-executable wall clock. Generous against the slowest honest file (the pack
# tiers do real IO) and still far under a human's patience, because what it is
# really guarding is a mutant that hangs rather than fails: see the
# TimeoutExpired arm in `native`.
NATIVE_TIMEOUT_S = 180


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
        try:
            r = subprocess.run(
                [str(exe)], cwd=NATIVE_DIR, capture_output=True, text=True,
                timeout=NATIVE_TIMEOUT_S,
            )
        except subprocess.TimeoutExpired:
            # Roadmap step 14 (D19) put an UNBOUNDED reconcile loop in the
            # commit path: a mutation that stops a round from re-basing does
            # not fail, it spins, and a sweep that hangs reports nothing at
            # all - strictly worse than a MISS. A timeout is therefore its own
            # verdict rather than a catch: it lands in `red` under a label no
            # expectation names, so it scores as a STRAY and fails the sweep
            # loudly instead of being read as coverage.
            red.append(f"FAIL - {exe.stem} did not finish within {NATIVE_TIMEOUT_S}s (hung)")
            continue
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
