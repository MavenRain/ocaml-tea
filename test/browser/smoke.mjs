/* The browser smoke test (roadmap step 8, D13): the one assertion the OCaml
   suite structurally cannot make.
 *
 * Every other test in `test/` links the libraries directly and drives them
 * in-process. That proves the tiers are correct; it does not prove they are
 * CONNECTED. The jsoo bundle actually mounting, the WebSocket actually
 * dialling, a click actually reaching the store, a commit actually streaming
 * back to a second tab, an XHR actually decoding a typed RPC reply, a browser
 * actually attaching the Origin header the D12 gate demands - none of that is
 * observed by any in-process test. This harness drives the real compiled
 * server binaries with a real Chromium and asserts the transitions end to end.
 *
 * Run:  test/browser/run.sh          (builds first, then runs this)
 * Or:   node test/browser/smoke.mjs  (after `dune build`)
 *
 * House test style, same as the OCaml suite: a flat list of labelled checks,
 * one per line, non-zero exit if any fails. No test framework.
 *
 * ANTI-VACUITY IS THE POINT. A browser test that asserts a condition which was
 * already true before the action passes forever - including against a server
 * that does nothing at all. So the assertion primitive here is `transition`:
 * it samples the observable BEFORE the action, refuses to pass if the wanted
 * condition already held (reported as VACUOUS, a failure), and only then polls
 * for the change. Plain `check` is reserved for preconditions and for facts
 * with no before-state, such as an HTTP status.
 *
 * `pin` is the third class: a characterization pin on a KNOWN BUG. It passes
 * (printed `xfail`) while the bug is present and FAILS once the behaviour
 * changes, so a fix cannot land without this file being revisited. A pin never
 * blesses the behaviour it records.
 *
 * It has already paid for itself once: this file's first run pinned D14 (the
 * acting tab double-counting its own PN-counter dot), and step 9's fix turned
 * that pin STALE - which is exactly how the fix came back through here, where
 * the D14 line is now an ordinary `transition` on the acting tab. No pins are
 * outstanding today; the primitive stays for the next one.
 *
 * Step 11 (D16) added the delivery scenarios: B1-B3 drive the step-10 replay
 * guard on the mem tier through `page.routeWebSocket` (a dropped up-frame, a
 * two-tab seq collision, a dropped Ack), and B4 restarts the pack-backed
 * server binary over one TEA_ROOT to assert exactly-once ACROSS A PROCESS -
 * the one claim every in-process test is structurally blind to.
 *
 * Step 12 (D17) turned B4's identity pin into the positive check (serve_pack
 * now defaults to a durable identity minted at <root>.secret) and added B5,
 * the different-secret converse: Dream's fallback secret is process-global,
 * so only a run that CHANGES the secret and watches identity reset can
 * attribute B4's green to the configured secret rather than to the tester's
 * environment.
 *
 * B6 and B7 are the REFUSAL scenarios, and they drive the server binary raw
 * with no browser at all, because the entire claim is that the process exits
 * before it ever serves. B6 points TEA_ROOT at an unusable root (an existing
 * empty directory, the likeliest operator mistake). B7 points it at a root
 * that does NOT exist while its <root>.guard sibling survives, the shape a
 * partial wipe or a partial restore leaves behind. Both must produce one
 * audible stderr line AND a non-zero exit: an uncaught Pack_error is the old
 * bug, and a clean-looking exit 0 is unreadable to a supervisor (systemd
 * Restart=on-failure, k8s restartPolicy: OnFailure, a CI gate). B7 is the
 * silent-LOSS direction specifically: with the floors alive and the store
 * gone, a returning tab's replay is judged Duplicate and dropped onto a model
 * that materialised empty, so serving at all is worse than refusing.
 *
 * Step 13 (R20) added B8, the other out-of-step restore: the journal and the
 * store both survive, but the store is OLDER. Three lives and a real on-disk
 * snapshot/restore between two of them, because no in-process test can roll a
 * pack root back under a running system - and because the whole claim is that
 * the boot filter turns what used to be a silent lost edit into one visible
 * duplicate plus one line an operator can read.
 */

import { chromium } from 'playwright'
import { spawn } from 'node:child_process'
import { existsSync, statSync } from 'node:fs'
import { cp, mkdir, mkdtemp, readdir, rm, stat } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import { dirname, join, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), '..', '..')
const PORT_COUNTER = Number(process.env.SMOKE_PORT_COUNTER ?? 8137)
const PORT_SHARED_DOC = Number(process.env.SMOKE_PORT_SHARED_DOC ?? 8138)
const PORT_REPLAY = Number(process.env.SMOKE_PORT_REPLAY ?? 8139)
const PORT_DURABLE = Number(process.env.SMOKE_PORT_DURABLE ?? 8140)
const PORT_SECRET = Number(process.env.SMOKE_PORT_SECRET ?? 8141)
/* B7's server must never listen. It gets its own port anyway, so that when a
   mutation DOES let it serve, it collides with nothing and its red is about
   the missing refusal alone. */
const PORT_ORPHAN = Number(process.env.SMOKE_PORT_ORPHAN ?? 8142)
/* B8 runs three lives in sequence on one port; nothing overlaps, because each
   life is stopped and awaited before the next is spawned. */
const PORT_ROLLBACK = Number(process.env.SMOKE_PORT_ROLLBACK ?? 8143)
const PORT_LOST_REPLY = Number(process.env.SMOKE_PORT_LOST_REPLY ?? 8144)
const PORT_LOST_REPLY_RESTART = Number(process.env.SMOKE_PORT_LOST_REPLY_RESTART ?? 8145)
const PORT_LOCAL = Number(process.env.SMOKE_PORT_LOCAL ?? 8146)
const HEADED = process.env.SMOKE_HEADED === '1'
const WAIT_MS = Number(process.env.SMOKE_TIMEOUT_MS ?? 15000)

const sleep = (ms) => new Promise((r) => setTimeout(r, ms))
const show = (v) => JSON.stringify(v)

/* ------------------------------------------------------------------ checks */

const check = (label, ok, detail = '') => ({ label, ok, detail, kind: 'check' })

/* The anti-vacuity assertion. `read` samples the observable, `want` is the
   predicate on it. `pre` may be supplied when the before-state has to be
   sampled earlier than the action - page B's count must be read before page A
   is clicked, or the WS frame could land in between and hide the transition.

   Polling rather than page.waitForFunction: it reads DOM and non-DOM
   observables through one primitive, and it keeps the vacuity guard in exactly
   one place. There is no fixed sleep anywhere - the loop returns the moment
   the condition holds, and fails fast at the deadline. */
async function transition(label, read, want, opts = {}) {
  const { act = async () => {}, timeout = WAIT_MS, kind = 'check' } = opts
  const pre = 'pre' in opts ? opts.pre : await read()
  if (want(pre))
    return { label, ok: false, kind, detail: `VACUOUS: the wanted condition already held before the action (pre=${show(pre)})` }
  await act()
  const deadline = Date.now() + timeout
  const poll = async () => {
    const now = await read()
    if (want(now)) return { label, ok: true, kind, detail: `${show(pre)} -> ${show(now)}` }
    if (Date.now() >= deadline)
      return { label, ok: false, kind, detail: `timed out after ${timeout}ms: pre=${show(pre)} last=${show(now)}` }
    await sleep(50)
    return poll()
  }
  return poll()
}

/* A characterization pin on a known bug: `transition` with the buggy outcome as
   the wanted condition. Present => `xfail` (not a failure). Absent => a real
   failure, because either the bug is fixed (update this file) or it changed
   shape (worse: it needs re-diagnosing). */
const pin = (label, read, want, opts = {}) => transition(label, read, want, { ...opts, kind: 'pin' })

/* The static sibling of [pin], for a known-bad value that is ALREADY settled
   rather than reached by an action. [pin] cannot express this: it is built on
   [transition], whose vacuity guard correctly rejects a condition that already
   held before the action. Same pin semantics otherwise: it asserts today's
   broken value, counts as an xfail while the gap is open, and turns into a
   loud FAIL the day the gap closes and the value changes. */
const pinValue = (label, ok, detail = '') => ({ label, ok, detail, kind: 'pin' })

/* ------------------------------------------------------------------ server */

/* Spawn a real compiled example server on one origin, so the static bundle,
   the WebSocket and the /rpc routes share a scheme/host/port - which is what
   the same-origin gate and the shared Dream session cookie both require.

   Split into spawnServer/stop (roadmap step 11, D16) because the durable
   scenario needs what no `withServer` body can express: stop the server with
   a CHOSEN signal (SIGTERM for the graceful path whose teardown closes the
   pack store first and the guard journal second; SIGKILL if a scenario ever
   needs the ungraceful one) and then start a second life on the same
   TEA_ROOT. Environment hygiene is strict: TEA_ROOT is stripped from the
   inherited environment and reinstated only through opts.env, so a variable
   leaked by the calling shell cannot silently turn a mem-tier scenario into a
   pack-tier one - the mem scenarios run with it genuinely UNSET, not "".
   TEA_SECRET and TEA_SECRET_FILE are stripped for the same reason (step 12,
   D17): the durable identity B4 asserts must come from the <root>.secret file
   the server minted, never from the tester's shell - with either leaked, B4
   can go green through the environment. */
async function spawnServer({ name, exe, clientDir, port, env = {} }) {
  const exePath = resolve(REPO, exe)
  if (!existsSync(exePath)) throw new Error(`${name}: ${exe} is missing - run \`dune build\` first`)
  const ambient = { ...process.env }
  delete ambient.TEA_ROOT
  delete ambient.TEA_SECRET
  delete ambient.TEA_SECRET_FILE
  const proc = spawn(exePath, [], {
    cwd: REPO,
    env: { ...ambient, PORT: String(port), CLIENT_DIR: resolve(REPO, clientDir), ...env },
    stdio: ['ignore', 'pipe', 'pipe'],
  })
  const log = []
  proc.stdout.on('data', (d) => log.push(String(d)))
  proc.stderr.on('data', (d) => log.push(String(d)))
  const base = `http://localhost:${port}`
  const tail = () => log.join('').split('\n').slice(-20).join('\n')
  /* The WHOLE log, for assertions about what a life said WHEN IT BOOTED.
     `tail` keeps the last 20 lines, which is right for a check made while the
     server is still starting and wrong for one made after traffic: Dream logs
     every request, so a boot-time operator line scrolls out of a tail. */
  const full = () => log.join('')
  const exited = new Promise((r) => proc.on('exit', (code, signal) => r({ code, signal })))
  const deadline = Date.now() + WAIT_MS
  const ready = async () => {
    if (proc.exitCode !== null) throw new Error(`${name} exited with ${proc.exitCode} before serving:\n${tail()}`)
    const ok = await fetch(base + '/')
      .then((r) => r.ok)
      .catch(() => false)
    if (ok) return
    if (Date.now() >= deadline) throw new Error(`${name} never answered on ${base} within ${WAIT_MS}ms:\n${tail()}`)
    await sleep(100)
    return ready()
  }
  await ready()
  return { name, proc, base, tail, full, exited }
}

/* Deliver `signal` and wait for the exit, escalating to SIGKILL at the
   deadline so a wedged teardown cannot hang the harness. Returns the
   {code, signal} the process exited with - the durable scenario asserts on
   it, because exit 0 is the evidence the signal handler ran the documented
   store-then-journal teardown rather than the process being torn down. */
async function stop(handle, signal = 'SIGTERM') {
  if (handle.proc.exitCode === null && handle.proc.signalCode === null) handle.proc.kill(signal)
  const outcome = await Promise.race([handle.exited, sleep(WAIT_MS).then(() => null)])
  if (outcome !== null) return outcome
  handle.proc.kill('SIGKILL')
  return handle.exited
}

async function withServer(opts, body) {
  const srv = await spawnServer(opts)
  try {
    return await body({ base: srv.base })
  } finally {
    await stop(srv, 'SIGTERM')
  }
}

/* ------------------------------------------------- scenario 1: the counter */

/* One browser context = one Dream session cookie = one session branch, so both
   tabs watch the same branch. Page A's click is the only action taken; page B
   is a pure observer, and its transition is therefore attributable to the
   live-view WebSocket alone. */
async function counterScenario(browser, base) {
  const ctx = await browser.newContext()
  const errors = []
  const open = async () => {
    const page = await ctx.newPage()
    page.on('pageerror', (e) => errors.push(String(e)))
    await page.goto(`${base}/app/index.html`)
    /* The mount gate: fails on a 404 bundle or a boot-time exception, before
       any assertion below could pass for the wrong reason. */
    await page.waitForSelector('.counter .count', { timeout: WAIT_MS })
    return page
  }
  /* A first, then B: A's response installs the session cookie that B inherits.
     Opening them concurrently would race two sessions onto two branches, and
     the live-view assertion below would be testing nothing. */
  const a = await open()
  const b = await open()

  const countOf = (page) => page.$eval('.counter .count', (el) => el.textContent)
  const preA = await countOf(a)
  const preB = await countOf(b)
  const plus = a.locator('.counter button').filter({ hasText: /^\+$/ })

  const mounted = check('counter: both tabs mount the jsoo bundle and render the count', preA === '0' && preB === '0', `A=${preA} B=${preB}`)

  /* The end-to-end live view, and the headline assertion of this file: tab B
     takes no local action at all, so its count can only move by way of tab A's
     click -> WS frame up -> App.update -> store commit -> store watch -> WS
     frame down -> tab B's Sync. Every tier is on that path. */
  const liveView = await transition(
    'counter: tab B moves 0 -> 1 with no local action, purely via the WS live frame',
    () => countOf(b),
    (v) => v === '1',
    { pre: preB, act: () => plus.click() }
  )

  /* D14, found by this harness on its first run and FIXED in step 9. The
     acting tab used to settle at 2: it minted its optimistic PN-counter dot
     under the constant replica id "client" while the server applied the very
     same forwarded Msg under the session-branch id, so one user intent
     occupied two replica slots and the join summed them. The server now
     announces its replica id on the socket (Wire.Hello) and the tab mints
     under it, making the two applies one slot that `join` reconciles by max.

     This is the assertion the whole phase exists to move, and it is checked on
     the ACTING tab - the one every in-process test was blind to, because no
     in-process test ran both applications of one intent. */
  const d14 = await transition(
    'counter: D14 - the acting tab counts its own click ONCE (A settles at 1, same as B)',
    () => countOf(a),
    (v) => v === '1',
    { pre: preA }
  )

  await ctx.close()
  return [mounted, liveView, d14, check('counter: no uncaught browser exception', errors.length === 0, errors.join(' | '))]
}

/* --------------------------------------------- scenario 2: the shared doc */

/* Exercises the typed RPC transport in the two directions the OCaml tests
   cannot reach: through the compiled jsoo XHR path (Doc_stats), and through
   the browser's own fetch with a browser-set Origin header (Append_tag, the
   D12 gate's pass side). The gate's REFUSE side is not reachable from here -
   Chromium will not let a page forge a cross-origin Origin - and is covered
   by test/csrf_test.ml with a forged header. */
async function sharedDocScenario(browser, base) {
  const ctx = await browser.newContext()
  const errors = []
  const page = await ctx.newPage()
  page.on('pageerror', (e) => errors.push(String(e)))
  await page.goto(`${base}/app/index.html`)
  await page.waitForSelector('.doc .stats-line', { timeout: WAIT_MS })

  const statsLine = () => page.$eval('.doc .stats-line', (el) => el.textContent)
  const preStats = await statsLine()
  /* Both numbers in the expected reply are chosen HERE ('smoke title' is 11
     chars, 'hello brave world' is 3 words), not read off the app's defaults,
     so the rendered line pins the reply against this tab's actual live model.
     A stubbed, empty or stale reply cannot produce it. */
  await page.fill('.doc input.title', 'smoke title')
  await page.fill('.doc input.body', 'hello brave world')

  const rpcStats = await transition(
    'shared_doc: the Stats button round-trips Doc_stats over XHR and renders the typed reply',
    statsLine,
    (v) => /^11 chars in the title, 3 words in the body$/.test(v),
    { pre: preStats, act: () => page.locator('.doc .stats button').click() }
  )

  /* A same-origin mutating POST issued by the page itself: Chromium attaches a
     truthful Origin header, so this is the D12 gate's pass side end to end. */
  const appendTag = (tag) =>
    page.evaluate(async (t) => {
      const r = await fetch('/rpc/append_tag', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(t),
      })
      return { status: r.status, body: await r.text() }
    }, tag)

  /* Step 15's flag day (D20) changed this endpoint's 200 shape: a [Mutating]
     reply is now ENVELOPED - {"reply": n} for an answer the one taken delivery
     computed, "replayed" for a duplicate whose bytes are gone. The envelope is
     on the wire whether or not the caller sent x-tea-key, and this POST
     deliberately sends none, so it is unwrapped here unconditionally rather
     than probed for. Reading `reply` by name (not JSON.parse alone) is what
     keeps the check below honest: a bare integer, or a "replayed" that means
     the effect is unconfirmed, both decode to null and fail rather than
     passing as a count. */
  const tagCount = (r) => {
    if (r.status !== 200) return null
    const v = JSON.parse(r.body)
    return v !== null && typeof v === 'object' && 'reply' in v ? v.reply : null
  }
  const first = await appendTag('smoke-one')
  const n1 = tagCount(first)
  const gate = check('shared_doc: same-origin POST /rpc/append_tag passes the D12 gate with 200', first.status === 200, show(first))
  const integer = check('shared_doc: the reply body decodes as an integer tag count', Number.isInteger(n1), `body=${first.body}`)
  /* That the write really landed in the store: a second DISTINCT tag must
     raise the count. A handler that answered 200 without committing, or an
     Or_set that swallowed the add, fails here and only here. */
  const committed = await transition(
    'shared_doc: a second distinct tag raises the stored count (the mutation really committed)',
    async () => tagCount(await appendTag('smoke-two')),
    (n) => n === n1 + 1,
    { pre: n1 }
  )

  await ctx.close()
  return [rpcStats, gate, integer, committed, check('shared_doc: no uncaught browser exception', errors.length === 0, errors.join(' | '))]
}

/* ------------------------------- scenarios 3-6: delivery (steps 10-11, D16) */

/* The counter example again, but driven through `page.routeWebSocket` so the
   harness can hold individual wire frames in its hand: drop an up-frame, drop
   an Ack, watch a replay cross. The route survives the page's reconnects (a
   re-dial matching the pattern lands in the same handler), so a tap's
   counters span socket lives - which is exactly the span the delivery
   guarantee is about.

   These scenarios abort at their FIRST failing check (the `step` early
   return): a mutation's earliest broken check is then the only red line it
   prints, which is what test/browser/mutate.py names in expect_red. */

/* Repr-JSON puts every wire frame on the socket as a one-key object -
   {"apply": ...} up, {"hello" | "head" | "ack": ...} down - so the tag is
   read by parsing, never by substring: a model payload can then never be
   misread as a frame kind. */
const frameTag = (message) => {
  try {
    const v = JSON.parse(String(message))
    return v && typeof v === 'object' ? Object.keys(v)[0] : null
  } catch {
    return null
  }
}

/* One tap = the observables and knobs for one routed page. `applies` counts
   client->server apply frames as they ARRIVE at the route, dropped ones
   included (an attempt is an attempt); `acks` counts server->client Acks
   actually FORWARDED, because a dropped Ack never reached the client and must
   not count as one. `socket` is the newest client-side route, so a scenario
   can break the live link and let the reconnect ladder rebuild it. */
function wsTap() {
  /* [droppedAcks] counts acks the tap swallowed. It matters because an ack the
     CLIENT never sees is still proof the SERVER got that far, and the pump
     persists the taken floor BEFORE it acks, so "the ack was emitted" is the
     only sound signal that the durable floor is written. Without it a test can
     only wait on a rendered count, which the store watch can satisfy while the
     journal append is still in flight. */
  /* [opens] counts sockets the route has seen, which is how a scenario waits
     for a RECONNECT rather than guessing. A click made while the tab is still
     re-dialling is queued and flushed by the replay path instead of being sent
     live, so without this gate a scenario about live delivery can silently
     become a scenario about replay (and a mutation that kills replay reddens a
     check that was never about it). */
  const tap = { applies: 0, acks: 0, droppedAcks: 0, dropUps: 0, opens: 0, dropAcks: false, socket: null }
  tap.handler = (ws) => {
    tap.socket = ws
    tap.opens += 1
    const server = ws.connectToServer()
    ws.onMessage((message) => {
      if (frameTag(message) === 'apply') {
        tap.applies += 1
        if (tap.dropUps > 0) {
          tap.dropUps -= 1
          return
        }
      }
      server.send(message)
    })
    server.onMessage((message) => {
      if (frameTag(message) === 'ack') {
        if (tap.dropAcks) {
          tap.droppedAcks += 1
          return
        }
        tap.acks += 1
      }
      ws.send(message)
    })
  }
  return tap
}

const countOn = (page) => page.$eval('.counter .count', (el) => el.textContent)
const plusOn = (page) => page.locator('.counter button').filter({ hasText: /^\+$/ })

/* Same open discipline as the counter scenario: acting page FIRST (its
   response installs the session cookie), observer second, and the route must
   be registered before goto or the first socket escapes it. */
const openCounter = async (ctx, base, errors, { tap } = {}) => {
  const page = await ctx.newPage()
  page.on('pageerror', (e) => errors.push(String(e)))
  if (tap) await page.routeWebSocket(/\/ws$/, tap.handler)
  await page.goto(`${base}/app/index.html`)
  await page.waitForSelector('.counter .count', { timeout: WAIT_MS })
  return page
}

/* B1 (D15): an up-frame dropped IN FLIGHT. The click is recorded in the tab's
   delivery queue and sent; the route swallows the frame, so the server never
   hears it. Breaking the socket forces the reconnect ladder, and the flush of
   the unacked queue on reconnect is the only path by which the edit can still
   land. Exactly-once is read off a pure observer tab, whose count can only
   move server-side, and the follow-up click is the second probe: had the
   replay double-applied, its wanted transition would already hold and report
   VACUOUS. */
async function dropUpScenario(browser, base) {
  const ctx = await browser.newContext()
  const errors = []
  const tap = wsTap()
  const out = []
  const step = (r) => {
    out.push(r)
    return r.ok
  }
  try {
    const a = await openCounter(ctx, base, errors, { tap })
    const o = await openCounter(ctx, base, errors)
    const preA = await countOn(a)
    const preO = await countOn(o)
    if (!step(check('replay B1: acting and observer tabs mount at 0', preA === '0' && preO === '0', `A=${preA} O=${preO}`))) return out

    tap.dropUps = 1
    if (!step(await transition(
      "replay B1: the click's up-frame is captured in flight and dropped",
      () => tap.applies,
      (n) => n >= 1,
      { act: () => plusOn(a).click() }
    ))) return out
    if (!step(check('replay B1: the dropped edit never reached the store (observer still 0)', (await countOn(o)) === '0'))) return out

    if (!step(await transition(
      'replay B1: breaking and reopening the socket replays the edit onto the store exactly once (observer 0 -> 1)',
      () => countOn(o),
      (v) => v === '1',
      { act: () => tap.socket.close() }
    ))) return out

    if (!step(await transition(
      'replay B1: a follow-up click moves the observer 1 -> 2 (no hidden double apply)',
      () => countOn(o),
      (v) => v === '2',
      { act: () => plusOn(a).click() }
    ))) return out

    step(check('replay B1: no uncaught browser exception', errors.length === 0, errors.join(' | ')))
    return out
  } finally {
    await ctx.close()
  }
}

/* B2 (D15): two tabs, one session cookie, one replica - the (replica, tab)
   guard key's whole reason to exist. Both tabs number their messages from
   seq 1; a guard keyed on the replica alone would read the second tab's first
   edit as the first tab's replay and swallow it. Each landing is asserted on
   the OTHER tab, so it can only have travelled through the store. */
async function twoTabScenario(browser, base) {
  const ctx = await browser.newContext()
  const errors = []
  const out = []
  const step = (r) => {
    out.push(r)
    return r.ok
  }
  try {
    const a = await openCounter(ctx, base, errors)
    const b = await openCounter(ctx, base, errors)
    const preA = await countOn(a)
    const preB = await countOn(b)
    if (!step(check('replay B2: two tabs on one session cookie both mount at 0', preA === '0' && preB === '0', `A=${preA} B=${preB}`))) return out
    if (!step(await transition(
      "replay B2: tab A's first edit reaches tab B (0 -> 1)",
      () => countOn(b),
      (v) => v === '1',
      { act: () => plusOn(a).click() }
    ))) return out
    if (!step(await transition(
      "replay B2: tab B's first edit also lands - the (replica, tab) guard key keeps both tabs at seq 1 (A sees 2)",
      () => countOn(a),
      (v) => v === '2',
      { act: () => plusOn(b).click() }
    ))) return out
    step(check('replay B2: no uncaught browser exception', errors.length === 0, errors.join(' | ')))
    return out
  } finally {
    await ctx.close()
  }
}

/* B3 (D15): the down-Ack is dropped, so the server has consumed seq 1 while
   the client still holds it as unacked. The client's replay - the flush a
   later server frame prompts, and again the flush the forced reconnect
   prompts - re-sends a message the server has already applied; the guard must
   answer Duplicate, acknowledge, and leave the count alone. The replay itself
   is asserted on the wire (a second apply crossing the route), because
   "nothing double counted" without "the replay actually happened" would be
   vacuous. */
async function dropAckScenario(browser, base) {
  const ctx = await browser.newContext()
  const errors = []
  const tap = wsTap()
  const out = []
  const step = (r) => {
    out.push(r)
    return r.ok
  }
  try {
    const a = await openCounter(ctx, base, errors, { tap })
    const o = await openCounter(ctx, base, errors)
    const preA = await countOn(a)
    const preO = await countOn(o)
    if (!step(check('replay B3: acting and observer tabs mount at 0', preA === '0' && preO === '0', `A=${preA} O=${preO}`))) return out

    tap.dropAcks = true
    const appliesBefore = tap.applies
    if (!step(await transition(
      'replay B3: the click lands while its Ack is dropped (observer 0 -> 1)',
      () => countOn(o),
      (v) => v === '1',
      { pre: preO, act: () => plusOn(a).click() }
    ))) return out

    /* Dropping stops in the act, so the replay's Ack can finally drain the
       queue; the break forces a reconnect flush even if no server frame has
       prompted one yet. */
    if (!step(await transition(
      'replay B3: the client replays the already-consumed message (a second apply crosses the wire)',
      () => tap.applies,
      (n) => n >= appliesBefore + 2,
      {
        pre: appliesBefore,
        act: () => {
          tap.dropAcks = false
          return tap.socket.close()
        },
      }
    ))) return out

    if (!step(await transition(
      'replay B3: the replay is not double-applied - a follow-up click moves the observer 1 -> 2',
      () => countOn(o),
      (v) => v === '2',
      { act: () => plusOn(a).click() }
    ))) return out

    step(check('replay B3: no uncaught browser exception', errors.length === 0, errors.join(' | ')))
    return out
  } finally {
    await ctx.close()
  }
}

/* B4, THE HEADLINE (step 11, D16): exactly-once across a real process
   restart, which nothing in-process can prove. Life 1 commits click 1
   (acknowledged, so it leaves the queue) and click 2 (its Ack dropped, so the
   client still holds it), then dies by SIGTERM - the graceful path whose
   teardown closes the pack store first and the guard journal second. Life 2
   opens the same TEA_ROOT; the page's reconnect ladder re-dials, the client
   replays its unacked seq 2, and the journal-seeded floor must answer
   Duplicate: both tabs still read 2.

   TWO clicks before the kill, not one, and that is load-bearing: with a
   single unacked click, "reads 1 after the restart" is satisfied both by the
   real guarantee and by a server that lost the store and merely re-applied
   the replay (0 + 1 also reads 1) - the assertion could never distinguish
   them, and mutate.py's fresh-root mutation could never go red. With 1
   committed + 1 replayed-and-suppressed, the verdict 2 separates all three
   worlds: 1 is the lost-store arm, 3 is the lost-floor (double apply) arm.

   This is a positive test, not a pin: the durable sink exists now. */
async function durableRestartScenario(browser) {
  /* The pack root must be a path irmin-pack creates and owns: the PARENT has
     to exist and the leaf must not. Handing it the mkdtemp directory itself
     dies with `Pack_error: "Invalid_layout"` before the server ever listens
     (and a missing parent dies with No_such_file_or_directory), so the temp
     directory is the parent and the store sits one component below it,
     exactly as the OCaml pack tests do it. Both lives share this path: that
     is what makes the restart a restart rather than a fresh store. */
  const parent = await mkdtemp(join(tmpdir(), 'tea-smoke-b4-'))
  const root = join(parent, 'store')
  const server = () =>
    spawnServer({
      name: 'durable counter server',
      exe: '_build/default/examples/counter/server/main.exe',
      clientDir: '_build/default/examples/counter/client',
      port: PORT_DURABLE,
      env: { TEA_ROOT: root },
    })
  let srv = null
  let ctx = null
  const errors = []
  const tap = wsTap()
  const out = []
  const step = (r) => {
    out.push(r)
    return r.ok
  }
  try {
    srv = await server()
    ctx = await browser.newContext()
    const a = await openCounter(ctx, srv.base, errors, { tap })
    const o = await openCounter(ctx, srv.base, errors)
    const preA = await countOn(a)
    const preO = await countOn(o)
    if (!step(check('durable B4: pack-backed tabs mount at 0', preA === '0' && preO === '0', `A=${preA} O=${preO}`))) return out

    const acksBefore = tap.acks
    if (!step(await transition(
      'durable B4: first click commits and streams to the observer (0 -> 1)',
      () => countOn(o),
      (v) => v === '1',
      { pre: preO, act: () => plusOn(a).click() }
    ))) return out
    if (!step(await transition(
      "durable B4: the first click's Ack reaches the client (only the second click will ride the restart)",
      () => tap.acks,
      (n) => n > acksBefore,
      { pre: acksBefore }
    ))) return out

    tap.dropAcks = true
    if (!step(await transition(
      'durable B4: second click commits while its Ack is dropped (observer 1 -> 2)',
      () => countOn(o),
      (v) => v === '2',
      { act: () => plusOn(a).click() }
    ))) return out

    /* Wait for the server to have EMITTED that ack (the tap swallows it, so
       the client still replays). The pump persists the taken floor before it
       acks, so this is the signal that the floor is written; the observer's
       count is NOT, because the store watch can render the commit while the
       journal append is still in flight. Stopping on the count alone made this
       scenario flaky: a SIGTERM landing in that window closes the journal
       under the pending append, the floor is lost, and the replay double
       applies to 3. That degrades in the designed direction (a visible,
       convergent duplicate rather than a loss), but it is a race, and a test
       that races is not evidence. */
    if (!step(await transition(
      'durable B4: the server emitted the second click\'s Ack before the stop, so its floor is persisted (persist precedes ack)',
      () => tap.droppedAcks,
      (n) => n > 0,
      { pre: 0 }
    ))) return out

    /* Life 1's identity, captured BEFORE the stop: the cookie is what life 2
       must decrypt, and the pid is what proves life 2 is a different process.
       Without the cookie in hand, the adoption check below could pass against
       a context that never held a session at all. */
    const cookieBefore = (await ctx.cookies(srv.base)).find((c) => c.name === 'dream.session')?.value ?? ''
    const pidBefore = srv.proc.pid
    if (!step(check('durable B4: life 1 issued a session cookie to capture', cookieBefore !== '', 'no dream.session cookie in the context'))) return out
    const outcome = await stop(srv, 'SIGTERM')
    /* Only AFTER the process is dead may the tap stop dropping acks. The
       client retransmits its unacked seq 2 every few tens of milliseconds,
       and the still-alive server re-acks every retransmit; a single re-ack
       FORWARDED in the gap between un-dropping and the SIGTERM dequeues
       seq 2 and silently destroys the scenario's premise - the restart must
       carry an unacked message. (Measured, not theoretical: one awaited
       ctx.cookies() call in that gap was enough to lose the race
       deterministically.) Between the death and life 2 no server exists, so
       nothing can be wrongly dropped here; the un-drop is what lets the
       POST-restart ack through to the counter below. */
    tap.dropAcks = false
    if (!step(check(
      'durable B4: SIGTERM stops the pack server gracefully (exit 0 runs the store-then-journal teardown)',
      outcome.code === 0,
      `exit=${show(outcome)}`
    ))) return out

    const acksAtRestart = tap.acks
    srv = await server()
    step(check('durable B4: life 2 is a NEW process (the restart really happened)', srv.proc.pid !== pidBefore, `pid ${pidBefore} -> ${srv.proc.pid}`))
    step(check(
      'durable B4: life 2 did NOT fall back to an ephemeral secret',
      !srv.tail().includes('session secret unavailable'),
      srv.tail()
    ))
    const secretMode = (() => {
      try {
        return statSync(`${root}.secret`).mode & 0o777
      } catch {
        return null
      }
    })()
    step(check(
      'durable B4: the durable secret is on disk at <root>.secret, mode 0600',
      secretMode === 0o600,
      `mode=${secretMode === null ? 'MISSING' : secretMode.toString(8)}`
    ))
    if (!step(await transition(
      'durable B4: after the restart the client replays its unacked message and the server answers (a post-restart Ack arrives)',
      () => tap.acks,
      (n) => n > acksAtRestart,
      { pre: acksAtRestart, timeout: WAIT_MS * 2 }
    ))) return out

    /* Ask the SERVER what it holds, FIRST, because it is the only reading
       here that separates the worlds: 2 is the guarantee, 0 or 1 is the
       lost-identity or lost-store arm, 3 is the double-apply arm.

       A client-rendered count cannot make that distinction. A tab's model
       already holds 2, and the reconnect Hello is JOINED into it rather than
       adopted over it (the CvRDT merge), so a server that came back empty
       never pulls a tab back down. mut-b4-fresh-root proved exactly that by
       scoring ZERO reds against the client-count assertion.

       So: the SSR route, which runs no client logic at all, requested with
       the CAPTURED life-1 cookie pinned in the header. Pinned, not a page in
       the context, because the read must race nothing: the open tabs'
       reconnect handshakes hit life 2 first, and a server that refuses the
       old cookie reissues a fresh one on the upgrade response, silently
       swapping the context's cookie between the capture and this read. With
       the header pinned, both checks below are deterministic statements
       about life 1's identity, whatever the tabs got up to in between.

       headersArray, NOT headers(): headers() joins duplicate Set-Cookie
       headers into one string, and a joined absence test could miss a
       reissue hiding behind another cookie. Dream emits Set-Cookie only when
       the session is dirty, and a cookie it could not decrypt yields a fresh
       dirty pre-session - so the ABSENCE of a dream.session Set-Cookie here
       is positive proof the presented cookie was decrypted and adopted. */
    const resp = await ctx.request.get(`${srv.base}/`, {
      headers: { cookie: `dream.session=${cookieBefore}` },
    })
    const reissued = (await resp.headersArray())
      .filter((h) => h.name.toLowerCase() === 'set-cookie' && h.value.startsWith('dream.session='))
    step(check(
      'durable B4: life 2 ADOPTS the presented session cookie (200 with no Set-Cookie reissue), so the identity itself survived',
      resp.ok() && reissued.length === 0,
      `status=${resp.status()} reissued=${reissued.length}`
    ))
    const served = (await resp.text()).match(/class="count"[^>]*>([^<]*)</)?.[1] ?? 'UNPARSED'

    /* Step 12 (D17) closed B4's KNOWN GAP, and this check sits where the pin
       used to. The journal (D16) was never the missing piece: what a restart
       lost was the session id, which derives BOTH the branch name AND the
       replica id, so life 2 used to land on a fresh branch under a fresh
       replica and the journalled floor was looked up under a key that could
       never match. serve_pack now defaults to cookie sessions under a secret
       resolved from <root>.secret, so life 2 decrypts life 1's cookie, lands
       on the SAME branch under the SAME replica, and the journal-seeded floor
       answers Duplicate.

       The verdict 2 separates three worlds: 1 is the lost-store OR
       lost-identity arm (the replay applies onto an empty model), 3 is the
       lost-floor arm (the replay double-applies). The adoption check above is
       the identity witness that attributes a 2 here to the survived session
       rather than to any accident of client state. */
    step(check(
      'durable B4: the RESTARTED SERVER itself holds 2 (one committed click plus one replayed-and-suppressed click)',
      served === '2',
      `server-rendered count=${served} (0 = the identity was lost, 1 = the store was lost, 3 = the floor was lost)`
    ))

    step(check('durable B4: no uncaught browser exception', errors.length === 0, errors.join(' | ')))
    return out
  } finally {
    if (ctx) await ctx.close().catch(() => {})
    if (srv) await stop(srv, 'SIGTERM').catch(() => {})
    /* The journal is a SIBLING of the pack root (serve_pack keeps it out of
       irmin-pack's directory on purpose); both live under the temp parent, so
       removing the parent takes the store and the journal together. */
    await rm(parent, { recursive: true, force: true })
  }
}

/* B5, the different-secret converse (step 12, D17). B4 alone cannot attribute
   its evidence: an in-process probe found that Dream's fallback secret is
   lazily minted and PROCESS-GLOBAL, so any mechanism that happens to carry
   identity across lives - the tester's shell exporting TEA_SECRET, a shared
   fallback, a cached cookie - satisfies "B4 reads 2" just as well as the
   configured secret does. B5 is the converse that pins the attribution: one
   TEA_ROOT, two lives, DIFFERENT explicit secrets. If the configured secret is
   what carries identity, life 2 must REFUSE life 1's cookie (a Set-Cookie
   reissue) and the server-rendered count must reset to exactly 0 - the store
   still holds life 1's commit, but under a branch this fresh session cannot
   name. A count of exactly 0 and a 200 are both required: "not 2" would also
   be satisfied by a crashed life 2. */
async function secretConverseScenario(browser) {
  const parent = await mkdtemp(join(tmpdir(), 'tea-smoke-b5-'))
  const root = join(parent, 'store')
  /* Valid per Secret.of_string (32-512 chars of [A-Za-z0-9_-]), differing in
     more than one character so no single bit flip could confuse them. */
  const secretOne = 'b5_life_one_secret_0123456789_0123456789_A'
  const secretTwo = 'b5_life_two_secret_9876543210_9876543210_B'
  const server = (secret) =>
    spawnServer({
      name: 'secret-converse counter server',
      exe: '_build/default/examples/counter/server/main.exe',
      clientDir: '_build/default/examples/counter/client',
      port: PORT_SECRET,
      env: { TEA_ROOT: root, TEA_SECRET: secret },
    })
  let srv = null
  let ctx = null
  const errors = []
  const out = []
  const step = (r) => {
    out.push(r)
    return r.ok
  }
  try {
    srv = await server(secretOne)
    ctx = await browser.newContext()
    /* Actor and observer, as in B4: the ACTING tab renders its click
       optimistically, so only the observer's move proves the commit reached
       the store - and a committed count of 1 under secret one is the
       anti-vacuity floor for "resets to 0" below. */
    const a = await openCounter(ctx, srv.base, errors)
    const o = await openCounter(ctx, srv.base, errors)
    const preA = await countOn(a)
    const preO = await countOn(o)
    if (!step(check('secret B5: pack-backed tabs mount at 0', preA === '0' && preO === '0', `A=${preA} O=${preO}`))) return out
    if (!step(await transition(
      'secret B5: life 1 commits one click under its own secret (observer 0 -> 1)',
      () => countOn(o),
      (v) => v === '1',
      { pre: preO, act: () => plusOn(a).click() }
    ))) return out
    await a.close()
    await o.close()

    const outcome = await stop(srv, 'SIGTERM')
    if (!step(check(
      'secret B5: SIGTERM stops life 1 gracefully (exit 0 flushes the committed click to disk)',
      outcome.code === 0,
      `exit=${show(outcome)}`
    ))) return out

    srv = await server(secretTwo)
    const ssr = await ctx.newPage()
    const resp = await ssr.goto(`${srv.base}/`)
    const reissued = (await resp.headersArray())
      .filter((h) => h.name.toLowerCase() === 'set-cookie' && h.value.startsWith('dream.session='))
    step(check(
      'secret B5: life 2 with a different TEA_SECRET reissues a Set-Cookie, so the presented cookie was refused',
      reissued.length > 0,
      `status=${resp.status()} reissued=${reissued.length}`
    ))
    const served = await countOn(ssr)
    await ssr.close()
    step(check(
      'secret B5: two lives with different TEA_SECRET do NOT share the session (the server-rendered count resets to 0)',
      resp.ok() && served === '0',
      `status=${resp.status()} server-rendered count=${served} (1 would mean identity survived a secret change)`
    ))

    step(check('secret B5: no uncaught browser exception', errors.length === 0, errors.join(' | ')))
    return out
  } finally {
    if (ctx) await ctx.close().catch(() => {})
    if (srv) await stop(srv, 'SIGTERM').catch(() => {})
    await rm(parent, { recursive: true, force: true })
  }
}

/* B6, the pack-root preflight (step 12, separable): an unusable TEA_ROOT must
   be refused with one audible stderr line, never an uncaught
   Pack_error "Invalid_layout". Raw spawn, NOT spawnServer: the entire point
   is that the process exits before serving, which spawnServer's ready loop
   treats as a failure. The refused dir is exactly the shape mkdtemp hands
   out - an existing empty directory - which is also the likeliest operator
   mistake (pointing TEA_ROOT at a directory they just created). */
async function packRootPreflightScenario() {
  const out = []
  const step = (r) => {
    out.push(r)
    return r.ok
  }
  const dir = await mkdtemp(join(tmpdir(), 'tea-smoke-b6-'))
  try {
    const exePath = resolve(REPO, '_build/default/examples/counter/server/main.exe')
    const ambient = { ...process.env }
    delete ambient.TEA_ROOT
    delete ambient.TEA_SECRET
    delete ambient.TEA_SECRET_FILE
    const proc = spawn(exePath, [], {
      cwd: REPO,
      env: { ...ambient, TEA_ROOT: dir },
      stdio: ['ignore', 'pipe', 'pipe'],
    })
    let log = ''
    proc.stdout.on('data', (d) => { log += String(d) })
    proc.stderr.on('data', (d) => { log += String(d) })
    const exit = await Promise.race([
      new Promise((r) => proc.on('exit', (code) => r({ exited: true, code }))),
      sleep(WAIT_MS).then(() => ({ exited: false, code: null })),
    ])
    if (!exit.exited) proc.kill('SIGKILL')
    step(check(
      'preflight B6: an unusable TEA_ROOT is refused with an audible line, not an uncaught Pack_error',
      exit.exited && log.includes('pack root unusable') && !log.includes('Fatal error: exception'),
      `exited=${exit.exited} code=${exit.code} log=${log.split('\n').slice(0, 3).join(' | ')}`
    ))
    /* The STATUS, not just the line. This refusal used to print its stderr
       and return unit, which ended the binary at 0, and a supervisor (systemd
       Restart=on-failure, k8s restartPolicy: OnFailure, a CI gate) cannot
       tell a root the server never opened from a clean shutdown. Integer code
       required: a process we had to SIGKILL exits with code null, and null
       must not read as non-zero. */
    step(check(
      'preflight B6: the refusal exits NON-ZERO, so a supervisor restarts it instead of reading a clean shutdown',
      exit.exited && Number.isInteger(exit.code) && exit.code !== 0,
      `exited=${exit.exited} code=${show(exit.code)}`
    ))
    /* The refusal happens BEFORE secret resolution and the journal open, so
       a refused root must gain neither durability sibling - this is the
       ordering claim serve_pack's error arm makes, observed from outside. */
    step(check(
      'preflight B6: the refused root gained no .secret or .guard sibling',
      !existsSync(`${dir}.secret`) && !existsSync(`${dir}.guard`),
      `${existsSync(`${dir}.secret`) ? 'found .secret ' : ''}${existsSync(`${dir}.guard`) ? 'found .guard' : ''}`
    ))
    return out
  } finally {
    await rm(dir, { recursive: true, force: true })
  }
}

/* B7, the orphaned guard journal. The three durability siblings (<root>,
   <root>.guard, <root>.secret) are three separate paths with no
   cross-binding, so a wipe or a restore can keep some and drop others. One
   direction is silent LOSS rather than the duplicate this system always
   degrades towards: with the pack root gone but the journal surviving, a
   returning tab's cookie still decrypts to its old session id, hence its old
   branch name and its old replica id, hence its old guard key. The branch is
   gone so the model materialises empty, but the surviving floor judges the
   replayed message Duplicate and drops it onto that empty model - a loss no
   client can see and no later commit repairs. serve_pack must refuse the pair
   before the preflight ever runs.

   Raw spawn like B6, for the same reason: the process is supposed to die
   before it serves, which spawnServer's ready loop would report as a harness
   failure rather than as the assertion it is. The setup is the wipe shape
   exactly - a root INSIDE a fresh temp dir that does not exist, beside a
   <root>.guard directory that does. */
async function orphanedJournalScenario() {
  const out = []
  const step = (r) => {
    out.push(r)
    return r.ok
  }
  const parent = await mkdtemp(join(tmpdir(), 'tea-smoke-b7-'))
  const root = join(parent, 'store')
  try {
    /* The journal without its store. `parent` exists, so the root is missing
       for the only reason under test - it was wiped - and NOT because its
       parent is missing, which the preflight would refuse for its own
       unrelated reason and hide the orphan check behind. */
    await mkdir(`${root}.guard`)
    const exePath = resolve(REPO, '_build/default/examples/counter/server/main.exe')
    const ambient = { ...process.env }
    delete ambient.TEA_ROOT
    delete ambient.TEA_SECRET
    delete ambient.TEA_SECRET_FILE
    const proc = spawn(exePath, [], {
      cwd: REPO,
      env: { ...ambient, PORT: String(PORT_ORPHAN), TEA_ROOT: root },
      stdio: ['ignore', 'pipe', 'pipe'],
    })
    let log = ''
    proc.stdout.on('data', (d) => { log += String(d) })
    proc.stderr.on('data', (d) => { log += String(d) })
    const exit = await Promise.race([
      new Promise((r) => proc.on('exit', (code) => r({ exited: true, code }))),
      sleep(WAIT_MS).then(() => ({ exited: false, code: null })),
    ])
    if (!exit.exited) proc.kill('SIGKILL')
    const head = log.split('\n').slice(0, 3).join(' | ')
    step(check(
      'orphan B7: a surviving <root>.guard beside a missing pack root is refused with an audible line',
      log.includes('guard journal orphaned'),
      head
    ))
    step(check(
      'orphan B7: the refusal exits NON-ZERO, so a supervisor restarts it instead of reading a clean shutdown',
      exit.exited && Number.isInteger(exit.code) && exit.code !== 0,
      `exited=${exit.exited} code=${show(exit.code)}`
    ))
    /* A refusal, not a crash: an uncaught exception would exit non-zero and
       print a line too, so without this the two checks above are equally
       satisfied by the failure mode the refusal exists to replace. */
    step(check(
      'orphan B7: the refusal is a diagnosis, not an uncaught exception',
      !log.includes('Fatal error: exception'),
      head
    ))
    /* Same "did not listen" evidence B6 uses: the process ended within the
       deadline instead of running on as a server. Serving the orphaned pair
       at all is the silent-loss outcome, so this is the check that says the
       loss window never opened. */
    step(check(
      'orphan B7: the server never became usable - it exited instead of serving over the orphaned journal',
      exit.exited,
      `exited=${exit.exited} code=${show(exit.code)}`
    ))
    return out
  } finally {
    await rm(parent, { recursive: true, force: true })
  }
}

/* The exact operator sentence serve_pack prints when the boot filter drops
   floors standing ABOVE their branch heads. Kept as a constant because two
   checks read it in opposite directions: life 2 must NOT say it (the line is
   conditional, not decorative) and life 3 must. */
const ROLLBACK_LINE = 'the pack root is OLDER than the guard journal'

/* Recursive on-disk byte count. The rollback's INDEPENDENT witness: the pack
   is append-only, so life 2's two extra commits make its root strictly larger
   than life 1's snapshot, and a restored root that does not shrink back to the
   snapshot's size did not take. Without this, "the count came back 2" would be
   the only evidence the restore happened at all, and a copy that silently did
   nothing would read as the guarantee it is supposed to test. */
const dirSize = async (dir) => {
  const entries = await readdir(dir, { recursive: true, withFileTypes: true })
  const sizes = await Promise.all(
    entries.filter((e) => e.isFile()).map((e) => stat(join(e.parentPath, e.name)).then((s) => s.size))
  )
  return sizes.reduce((a, b) => a + b, 0)
}

/* B8, THE ROLLBACK (step 13, R20): the store and the guard journal are two
   separate paths, so a restore can put them out of step - and until step 13
   that was SILENT LOSS. An operator restores yesterday's pack root beside
   today's journal; a returning tab replays the message the journal already
   has a floor for; the guard answers Duplicate and drops it onto a model that
   no longer contains it. Nothing errors, nothing logs, and the edit is gone.
 *
 * The fix binds them by ORDER rather than identity: every floor now carries
 * the water (the commit date) of the very commit it de-duplicates, and a floor
 * standing above its branch's head at boot is dropped rather than honoured. A
 * dropped floor costs one visible duplicate; an honoured stale floor costs a
 * silent edit, and this system always degrades towards the duplicate.
 *
 * Three lives, because two cannot express it: life 1 commits ONE acknowledged
 * click and is snapshotted on disk; life 2 commits two more, the second with
 * its Ack dropped so the client still holds it; then life 1's root is restored
 * BESIDE life 2's journal and life 3 boots over the pair.
 *
 * The single served count separates four worlds, which is why life 2 clicks
 * twice: 2 is the guarantee (life 1's click plus the re-admitted replay); 1 is
 * the R20 silent loss (the surviving floor swallowed the replay); 3 means the
 * rollback silently did not take, the likeliest harness bug; 4 means the
 * rollback did not take AND the floor was lost. With a single life-2 click the
 * guarantee and the failed rollback would both render 2 and the scenario would
 * be unfalsifiable - the same trap B4 documents above.
 *
 * The audible line and the behaviour are asserted SEPARATELY, per the B6/B7
 * doctrine: an operator who restored the wrong root must be told, and a server
 * that says the right thing while doing the wrong one is the most dangerous
 * slip in this change. Each half has a mutation that kills it alone. */
async function rollbackScenario(browser) {
  const parent = await mkdtemp(join(tmpdir(), 'tea-smoke-b8-'))
  const root = join(parent, 'store')
  /* The snapshot sits UNDER the parent beside the root, never inside it: the
     journal and the secret are the root's siblings (<root>.guard,
     <root>.secret), so a copy named `snapshot` cannot be mistaken for either
     by the server or by the wipe in `finally`. */
  const snapshot = join(parent, 'snapshot')
  const server = () =>
    spawnServer({
      name: 'rollback counter server',
      exe: '_build/default/examples/counter/server/main.exe',
      clientDir: '_build/default/examples/counter/client',
      port: PORT_ROLLBACK,
      env: { TEA_ROOT: root },
    })
  let srv = null
  let ctx = null
  const errors = []
  const tap = wsTap()
  const out = []
  const step = (r) => {
    out.push(r)
    return r.ok
  }
  try {
    /* ---------------------------------- life 1: one acknowledged click ---- */
    srv = await server()
    ctx = await browser.newContext()
    const a = await openCounter(ctx, srv.base, errors, { tap })
    const o = await openCounter(ctx, srv.base, errors)
    const preA = await countOn(a)
    const preO = await countOn(o)
    if (!step(check('rollback B8: pack-backed tabs mount at 0', preA === '0' && preO === '0', `A=${preA} O=${preO}`))) return out

    const acksLife1 = tap.acks
    if (!step(await transition(
      'rollback B8: life 1 commits one click (observer 0 -> 1)',
      () => countOn(o),
      (v) => v === '1',
      { pre: preO, act: () => plusOn(a).click() }
    ))) return out
    /* Acknowledged, so it leaves the client's queue: the ONLY message that may
       ride the rollback is life 2's last one. A second unacked message would
       land a second re-admitted replay and the served count would no longer
       separate the four worlds. */
    if (!step(await transition(
      "rollback B8: life 1's click is acknowledged, so it leaves the queue and cannot be replayed later",
      () => tap.acks,
      (n) => n > acksLife1,
      { pre: acksLife1 }
    ))) return out

    /* Life 1's cookie, captured before the stop, exactly as B4 does: the SSR
       read at the end must present an identity minted BEFORE the rollback, or
       a green count could mean the tabs got a fresh session rather than that
       the witness worked. */
    const cookieBefore = (await ctx.cookies(srv.base)).find((c) => c.name === 'dream.session')?.value ?? ''
    if (!step(check(
      'rollback B8: life 1 issued a session cookie to capture',
      cookieBefore !== '',
      cookieBefore === '' ? 'no dream.session cookie in the context' : `${cookieBefore.length} cookie bytes held`
    ))) return out

    const outcome1 = await stop(srv, 'SIGTERM')
    srv = null
    if (!step(check(
      'rollback B8: SIGTERM stops life 1 gracefully, so the snapshot is taken over a quiesced pack root',
      outcome1.code === 0,
      `exit=${show(outcome1)}`
    ))) return out

    /* The snapshot is taken with no process holding the root - the same shape
       an operator's backup takes, and the shape guard_water_test's W6 already
       certified in-process (a dir copy of a closed pack root reopens as a
       valid OLDER store). */
    await cp(root, snapshot, { recursive: true })
    const snapSize = await dirSize(snapshot)
    if (!step(check('rollback B8: the snapshot copied a non-empty pack root', snapSize > 0, `snapshot bytes=${snapSize}`))) return out

    /* ------------------- life 2: one more acknowledged, one unacknowledged - */
    const opensLife1 = tap.opens
    srv = await server()
    const pidLife2 = srv.proc.pid
    /* Wait for the acting tab to be BACK on the wire before clicking. A click
       made mid-reconnect would be queued and flushed by the replay path, which
       would quietly turn the two checks below into a second replay test and
       leave the live-delivery claim untested. */
    if (!step(await transition(
      'rollback B8: the acting tab re-dials life 2 (the next click is delivered live, not replayed)',
      () => tap.opens,
      (n) => n > opensLife1,
      { pre: opensLife1, timeout: WAIT_MS * 2 }
    ))) return out
    const acksLife2 = tap.acks
    if (!step(await transition(
      'rollback B8: life 2 reopens the same root and commits a second click (observer 1 -> 2)',
      () => countOn(o),
      (v) => v === '2',
      { act: () => plusOn(a).click(), timeout: WAIT_MS * 2 }
    ))) return out
    if (!step(await transition(
      "rollback B8: life 2's first click is acknowledged too (only the LAST click rides the rollback)",
      () => tap.acks,
      (n) => n > acksLife2,
      { pre: acksLife2, timeout: WAIT_MS * 2 }
    ))) return out

    tap.dropAcks = true
    const droppedBefore = tap.droppedAcks
    if (!step(await transition(
      'rollback B8: life 2 commits a third click while its Ack is dropped (observer 2 -> 3)',
      () => countOn(o),
      (v) => v === '3',
      { act: () => plusOn(a).click() }
    ))) return out
    /* The ack the tap swallowed is the signal that the floor is on disk at
       that commit's water: the pump persists the taken floor BEFORE it acks,
       while the observer's count only says the commit rendered. Stopping on
       the count alone is the race B4 measured. */
    if (!step(await transition(
      "rollback B8: the server emitted the third click's Ack before the stop, so its floor is persisted at that commit's water",
      () => tap.droppedAcks,
      (n) => n > droppedBefore,
      { pre: droppedBefore }
    ))) return out

    /* Life 2 booted over a root its journal exactly matches, so the boot
       filter must be SILENT. Without this, an implementation that printed the
       rollback line on every boot would satisfy life 3's line check for free -
       the line would be decoration rather than a diagnosis. */
    step(check(
      'rollback B8: life 2 boots over an intact root and says nothing about a rollback (the line is conditional, not decorative)',
      !srv.full().includes(ROLLBACK_LINE),
      srv.full().split('\n').filter((l) => l.startsWith('tea_server_pack:')).join(' | ')
    ))

    const outcome2 = await stop(srv, 'SIGTERM')
    srv = null
    /* Un-drop only after the death, per B4's measured race: a live server
       re-acks every retransmit, and one forwarded re-ack would dequeue the
       message the rollback is supposed to carry. */
    tap.dropAcks = false
    if (!step(check(
      'rollback B8: SIGTERM stops life 2 gracefully (exit 0 runs the store-then-journal teardown)',
      outcome2.code === 0,
      `exit=${show(outcome2)}`
    ))) return out

    /* ------------------------ the rollback: life 1's root, life 2's journal */
    const liveSize = await dirSize(root)
    await rm(root, { recursive: true, force: true })
    await cp(snapshot, root, { recursive: true })
    const rolledSize = await dirSize(root)
    if (!step(check(
      "rollback B8: the pack root really rolled back - its bytes are life 1's snapshot again, strictly fewer than life 2 left",
      rolledSize === snapSize && rolledSize < liveSize,
      `life2=${liveSize} snapshot=${snapSize} restored=${rolledSize}`
    ))) return out
    if (!step(check(
      'rollback B8: the guard journal and the durable secret survived the rollback (only the store went back)',
      existsSync(`${root}.guard`) && existsSync(`${root}.secret`),
      `${existsSync(`${root}.guard`) ? '' : 'missing .guard '}${existsSync(`${root}.secret`) ? '' : 'missing .secret'}`
    ))) return out

    /* ------------------------------------ life 3: over the rolled-back pair */
    const acksLife3 = tap.acks
    /* Spawned through an explicit catch rather than plain `await`, so a server
       that REFUSES to serve reddens the disposition check below by name
       instead of aborting the scenario as a harness failure. Drop-and-serve is
       an adjudication (a rollback costs floors, not the site), and an
       adjudication has to be pinned rather than assumed. */
    const life3 = await server().then((s) => ({ srv: s, err: null })).catch((e) => ({ srv: null, err: e }))
    srv = life3.srv
    if (!step(check(
      'rollback B8: life 3 SERVES rather than refusing (a rollback drops floors, it does not take the site down)',
      life3.srv !== null,
      life3.err ? String(life3.err.message ?? life3.err) : 'the ready loop returned'
    ))) return out
    step(check(
      'rollback B8: life 3 is a NEW process over the rolled-back root',
      srv.proc.pid !== pidLife2,
      `pid ${pidLife2} -> ${srv.proc.pid}`
    ))

    /* Polled rather than read once: the operator line is printed before the
       server listens, but the pipe's data event is delivered asynchronously,
       so a bare read can lose a race the server has already won. `pre: false`
       is the literal before-state - life 3 had emitted no log at all when it
       was spawned - so the vacuity guard stays meaningful. */
    step(await transition(
      'rollback B8: life 3 boots over the rolled-back root and SAYS SO (the line names the root as older than the journal)',
      () => srv.full().includes(ROLLBACK_LINE),
      (v) => v === true,
      { pre: false }
    ))
    step(check(
      'rollback B8: the drop is a diagnosis, not a crash (life 3 raised nothing)',
      !srv.full().includes('Fatal error: exception'),
      srv.full().split('\n').slice(-3).join(' | ')
    ))

    /* The replay must actually happen, or every count below is vacuous. It
       arrives either way: the guard acknowledges a Duplicate too (an unacked
       duplicate is a replay loop that never ends), so this check says "the
       message was re-sent and answered", never "it was applied". */
    if (!step(await transition(
      'rollback B8: after the rollback the client replays its unacked message and the server answers (a post-rollback Ack arrives)',
      () => tap.acks,
      (n) => n > acksLife3,
      { pre: acksLife3, timeout: WAIT_MS * 2 }
    ))) return out

    /* The SSR route with life 1's cookie pinned in the header, never a
       rendered tab: a tab's model already holds 3 and the reconnect Hello is
       JOINED into it, so no server state can pull it back down. B4's
       mut-b4-fresh-root scored ZERO reds against a client-count assertion. */
    const resp = await ctx.request.get(`${srv.base}/`, {
      headers: { cookie: `dream.session=${cookieBefore}` },
    })
    const reissued = (await resp.headersArray())
      .filter((h) => h.name.toLowerCase() === 'set-cookie' && h.value.startsWith('dream.session='))
    /* Not optional. A rotated or absent <root>.secret rotates every replica id
       and parks the surviving floors under keys no client can present, which
       would neutralise R20 by accident: the count would come back 2 because
       the floor was UNREACHABLE, not because the witness dropped it. */
    step(check(
      'rollback B8: life 3 ADOPTS the presented session cookie, so the dropped floor was reachable and its being unused is the fix rather than an accident',
      resp.ok() && reissued.length === 0,
      `status=${resp.status()} reissued=${reissued.length}`
    ))
    const served = (await resp.text()).match(/class="count"[^>]*>([^<]*)</)?.[1] ?? 'UNPARSED'
    step(check(
      "rollback B8: the ROLLED-BACK SERVER itself holds 2 (life 1's surviving click plus the re-admitted replay)",
      served === '2',
      `server-rendered count=${served} (1 = the surviving floor swallowed the replay, the R20 silent loss; 3 = the rollback did not take; 4 = the rollback did not take AND the floor was lost)`
    ))

    step(check('rollback B8: no uncaught browser exception', errors.length === 0, errors.join(' | ')))
    return out
  } finally {
    if (ctx) await ctx.close().catch(() => {})
    if (srv) await stop(srv, 'SIGTERM').catch(() => {})
    await rm(parent, { recursive: true, force: true })
  }
}

/* -------------------------------------------------------------------- main */

/* --------------------------- scenario 9: the keyed RPC tier (step 15, D20) */

/* The request header that carries <tab>:<seq>. Read from the intercepted
   request rather than assumed: if the client ever stopped sending it the
   endpoint would silently fall back to the unkeyed path, every check below
   would still pass on a plain round trip, and only this assertion notices. */
const KEY_HEADER = 'x-tea-key'

/* B9, lost response. The proxy lets the POST reach the server, reads the real
   200 upstream, then aborts the PAGE's request: the effect lands, the reply
   never arrives. The client's keyed queue must retry THE SAME KEY, and the
   server must answer that retry out of its reply cache with the bytes the one
   taken delivery computed - so the app surfaces a real count, not a transport
   error and not "tag applied, reply lost" (that marker is B10's, and here it
   would mean the cache missed).

   Only the FIRST response is eaten. A standing drop would starve the retry
   ladder forever, and the scenario could not tell a working retry from a hung
   one - the timeout would look identical either way.

   On what this can and cannot witness: the document's tags are a set and the
   button always publishes the same string, so the rendered count is 1 whether
   the retry was refused or applied a second time. The count is therefore NOT
   the exactly-once witness here. What carries the claim at this tier is that
   both requests bore one key and that the second answer is BYTE-IDENTICAL to
   the first - the reply cache returning what the taken delivery computed
   rather than recomputing it. A double apply is caught where it is visible:
   natively by the commit count (rpc_once_test, rpc_pack_once_test), and in
   the browser by B10, whose marker cannot appear without a surviving floor. */
async function lostReplyScenario(browser, base) {
  const ctx = await browser.newContext()
  const errors = []
  const page = await ctx.newPage()
  page.on('pageerror', (e) => errors.push(String(e)))

  const keys = []
  const bodies = []
  let eaten = 0
  await page.route('**/rpc/append_tag', async (route) => {
    keys.push(route.request().headers()[KEY_HEADER] ?? '')
    /* fetch() runs the request upstream for real, so the server applies it and
       caches the reply; abort() is what the page sees. The order is the whole
       point: aborting first would drop the EFFECT too, and this would be a
       test of a plain retry rather than of a lost response. */
    const upstream = await route.fetch()
    const body = await upstream.text()
    bodies.push(body)
    if (eaten === 0) {
      eaten += 1
      await route.abort('connectionfailed')
      return
    }
    /* Fulfil from the upstream response so its headers (the Dream session
       cookie among them) survive; only the body is restated. */
    await route.fulfill({ response: upstream, body })
  })

  await page.goto(`${base}/app/index.html`)
  await page.waitForSelector('.doc .publish-line', { timeout: WAIT_MS })
  const publishLine = () => page.$eval('.doc .publish-line', (el) => el.textContent)

  const surfaced = await transition(
    'B9: the reply is dropped, the client retries, and the app renders the cached count',
    publishLine,
    (v) => /^1 tags on the document$/.test(v),
    { act: () => page.locator('.doc .publish button').click() }
  )

  /* "Exactly one tag in the canonical document", asserted where a browser can
     actually observe it: the server computes this count from the canonical doc
     AFTER applying, and ships it in the envelope.

     Deliberately NOT read off `.doc ul.tags`. The keyed handler commits to the
     canonical session, while this tab's live view watches its OWN session
     branch, so the tag it just published is not in this tab's model and no
     amount of waiting will put it there - an earlier draft of this check spun
     for the full timeout on an empty list. The branch itself is asserted
     natively, where the store is in hand (rpc_once_test, rpc_pack_once_test);
     the count is the browser's share of that claim. */
  const oneTag = check(
    'B9: the server counts exactly one tag on the canonical document',
    bodies.length >= 2 && JSON.parse(bodies[1]).reply === 1,
    show(bodies[1] ?? null)
  )

  const retried = check(
    'B9: the dropped response provoked a retry (more than one POST reached the server)',
    keys.length >= 2,
    `posts=${keys.length}`
  )
  const oneKey = check(
    'B9: every attempt carried one and the same non-empty x-tea-key',
    keys.length >= 2 && keys[0] !== '' && keys.every((k) => k === keys[0]),
    show(keys)
  )
  /* The reply cache's own claim, checked on the wire: byte-identical, not
     merely equivalent. A recomputed answer could coincide in value; it is this
     equality that says the second POST never re-entered the handler. */
  const cached = check(
    'B9: the retry was answered with the byte-identical cached reply',
    bodies.length >= 2 && bodies[1] === bodies[0],
    show(bodies.slice(0, 2))
  )
  const notMarker = check(
    'B9: the app saw a real reply, not the reply-lost marker (the cache held)',
    bodies.length >= 2 && !/replayed/.test(bodies[1]),
    show(bodies[1] ?? null)
  )

  await ctx.close()
  return [
    surfaced,
    oneTag,
    retried,
    oneKey,
    cached,
    notMarker,
    check('B9: no uncaught browser exception', errors.length === 0, errors.join(' | ')),
  ]
}

/* B10, lost response plus restart (step 15, D20), the durable statement B9
   cannot make. B9's retry is refused by a reply cache that lives in the same
   process as the effect, so all it can witness is that one process did not
   apply twice. Here the process itself goes away between the effect and the
   retry: the only thing left to refuse the duplicate is the delivery floor on
   disk. What the browser sees is the typed consequence - the client surfaces
   [Applied_reply_lost], "tag applied, reply lost", which is the whole reason
   the reply envelope has a second case at all.

   The restart is performed INSIDE the route handler, after the real 200 has
   been read upstream and before the page is told anything. That ordering is
   what makes this scenario deterministic rather than a race. Aborting first and
   then restarting would leave the client free to retry against a server that is
   still alive - the backoff ladder starts at 500ms and a graceful SIGTERM plus
   a fresh boot takes longer than that - and a retry answered from life 1's warm
   cache is B9 again, wearing B10's name. Holding the page's request until life
   2 is listening means the first thing the client can possibly do after the
   failure is talk to life 2.

   The count is again not the exactly-once witness (tags are a set, so a double
   apply of one string is invisible in the document); here it is the wire that
   carries the claim, and it carries a stronger one than B9's byte-identity: an
   answer of "replayed" can only be produced by a guard that already knows this
   key was delivered, and nothing in the new process knows that except the
   journal beside the pack root. The probe below is the complement - it rules
   out the degenerate reading where the effect never survived at all. */
async function lostReplyRestartScenario(browser) {
  /* Parent exists, leaf does not: the same pack-root shape B4 explains. */
  const parent = await mkdtemp(join(tmpdir(), 'tea-smoke-b10-'))
  const root = join(parent, 'store')
  const server = () =>
    spawnServer({
      name: 'lost-reply-restart shared_doc server',
      exe: '_build/default/examples/shared_doc/server/main.exe',
      clientDir: '_build/default/examples/shared_doc/client',
      port: PORT_LOST_REPLY_RESTART,
      env: { TEA_ROOT: root },
    })
  let srv = null
  let ctx = null
  const errors = []
  const out = []
  const step = (r) => {
    out.push(r)
    return r.ok
  }
  try {
    srv = await server()
    const base = srv.base
    const pidBefore = srv.proc.pid
    let pidAfter = null
    ctx = await browser.newContext()
    const page = await ctx.newPage()
    page.on('pageerror', (e) => errors.push(String(e)))

    const keys = []
    const bodies = []
    let eaten = 0
    await page.route('**/rpc/append_tag', async (route) => {
      keys.push(route.request().headers()[KEY_HEADER] ?? '')
      const upstream = await route.fetch()
      const body = await upstream.text()
      bodies.push(body)
      if (eaten === 0) {
        eaten += 1
        /* Between the answer and the abort: life 1 dies having applied the tag
           and cached the reply it will never deliver, and life 2 comes up over
           the same root with an empty cache and the floor it inherits. */
        await stop(srv, 'SIGTERM')
        srv = await server()
        pidAfter = srv.proc.pid
        await route.abort('connectionfailed')
        return
      }
      await route.fulfill({ response: upstream, body })
    })

    await page.goto(`${base}/app/index.html`)
    await page.waitForSelector('.doc .publish-line', { timeout: WAIT_MS })
    const publishLine = () => page.$eval('.doc .publish-line', (el) => el.textContent)

    /* Not gated: every check after this one reads state already recorded by the
       proxy, and if the marker never arrives those recordings are the diagnosis
       (which POST was answered how, and whether a second one happened at all). */
    step(await transition(
      'B10: the retry crosses the restart and the app surfaces the reply-lost marker',
      publishLine,
      (v) => /^tag applied, reply lost$/.test(v),
      { act: () => page.locator('.doc .publish button').click() }
    ))

    step(check(
      'B10: the restart really happened (life 2 is a new process on the same root)',
      pidAfter !== null && pidAfter !== pidBefore,
      `pid ${pidBefore} -> ${pidAfter === null ? 'NEVER RESPAWNED' : pidAfter}`
    ))
    /* Life 1 answered for real before it died. Without this the scenario would
       also pass if the first POST had been refused outright, which is a lost
       REQUEST, not a lost response, and proves nothing about the floor. */
    step(check(
      'B10: life 1 applied the tag and computed a real reply before the drop',
      bodies.length >= 1 && JSON.parse(bodies[0])?.reply === 1,
      show(bodies[0] ?? null)
    ))
    step(check(
      'B10: the dropped response provoked a retry that reached life 2',
      keys.length >= 2,
      `posts=${keys.length}`
    ))
    step(check(
      'B10: the retry carried the same non-empty x-tea-key across the boot',
      keys.length >= 2 && keys[0] !== '' && keys.every((k) => k === keys[0]),
      show(keys)
    ))
    /* The durable claim, on the wire. Parsed rather than string-matched: the
       envelope's other case is an object, so decoding to exactly the string
       "replayed" says this was the reply-lost arm and not a reply that happens
       to mention the word. */
    step(check(
      'B10: life 2 refused the duplicate from the journal alone (the wire says "replayed")',
      bodies.length >= 2 && JSON.parse(bodies[1]) === 'replayed',
      show(bodies[1] ?? null)
    ))

    /* The complement: "replayed" would also be the honest answer if the tag had
       never survived the restart and the floor were all that was left, so the
       document is asked directly. A DISTINCT tag is used deliberately - probing
       with the published string would append into a set that already holds it
       and answer 1 whether or not the original survived, which is a check that
       cannot fail. Unkeyed on purpose: this probe is a reader, and giving it a
       key would enrol it in the very guard it is meant to observe. */
    const probe = await page.evaluate(async () => {
      const r = await fetch('/rpc/append_tag', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify('probe'),
      })
      return { status: r.status, body: await r.text() }
    })
    step(check(
      'B10: the canonical document holds exactly one published tag after the boot',
      probe.status === 200 && JSON.parse(probe.body)?.reply === 2,
      `${show(probe)} (a count of 1 = the published tag did not survive the restart)`
    ))

    step(check('B10: no uncaught browser exception', errors.length === 0, errors.join(' | ')))
    return out
  } finally {
    if (ctx) await ctx.close().catch(() => {})
    if (srv) await stop(srv, 'SIGTERM').catch(() => {})
    await rm(parent, { recursive: true, force: true })
  }
}

/* ------------------- scenarios B11-B13: the browser-local tier (step 25) */

/* Step 25's three browser-only claims. Everything else about the local tier
   is proven natively against the honest fake (test/local_store_test.ml);
   these three pin exactly what the fake structurally cannot: a REAL reload
   against a real IndexedDB, a REAL Web Locks election between two live
   pages, and a REAL boot with the API deleted. */

/* The app's own store, read from inside the page: the rows are the
   observable the native Flow tests can only fake. */
const readLocalRows = (page) =>
  page.evaluate(
    () =>
      new Promise((resolve) => {
        const req = indexedDB.open('tea-local:Counter', 1)
        req.onerror = () => resolve(null)
        req.onsuccess = () => {
          const db = req.result
          let tx
          try {
            tx = db.transaction('records', 'readonly')
          } catch {
            db.close()
            resolve(null)
            return
          }
          const all = tx.objectStore('records').getAll()
          all.onsuccess = () => {
            db.close()
            resolve(all.result)
          }
          all.onerror = () => {
            db.close()
            resolve(null)
          }
        }
      })
  )

const parsedRows = async (page) => ((await readLocalRows(page)) ?? []).map((s) => JSON.parse(s))

/* Server truth through the context's OWN cookie jar: the SSR page renders the
   session's count, and ctx.request shares the browser context's session, so
   this is the server-observed count of the same branch the tabs live on. A
   bare node fetch would mint a fresh session and always read 0. */
const serverCount = async (ctx, base) => {
  const html = await (await ctx.request.get(base + '/')).text()
  const m = html.match(/class="count"[^>]*>(-?\d+)</)
  return m ? m[1] : null
}

/* Setup-only poll (not an assertion): the scenarios below sequence page LIVES,
   and a life must not start until the previous one's effects are on disk. */
const until = async (label, read, want) => {
  const deadline = Date.now() + WAIT_MS
  const poll = async () => {
    const v = await read()
    if (want(v)) return v
    if (Date.now() >= deadline)
      throw new Error(`${label}: gave up after ${WAIT_MS}ms (last=${show(v)})`)
    await sleep(50)
    return poll()
  }
  return poll()
}

/* A closed page's writer lock releases asynchronously; the next life must not
   contest the election until it is actually free. The watcher page is the SSR
   page: same origin (locks are origin-scoped), no client bundle, no lock. */
const untilWriterFree = async (ctx, base) => {
  const w = await ctx.newPage()
  await w.goto(`${base}/`)
  await until(
    'writer lock free',
    () =>
      w.evaluate(async () =>
        ((await navigator.locks.query()).held ?? []).filter(
          (l) => l.name === 'tea-local-writer:Counter'
        ).length
      ),
    (n) => n === 0
  )
  await w.close()
}

/* B11: three page lives over one session. Life 1 is live and leaves an acked
   checkpoint. Life 2 boots with its socket black-holed: it must paint from
   the snapshot, stay marked provisional (no Hello can arrive), and checkpoint
   an offline click into the queue. Life 3 is live again: the first Hello
   confirms the adopted replica, clears the marker, and releases the queued
   edit - asserted on the SERVER, because a dropped edit is silent
   client-side (the roadmap acceptance, D13-class connected-proof). */
async function persistReloadScenario(browser, base) {
  const ctx = await browser.newContext()
  const out = []
  const countSel = '.counter .count'
  const countOf = (page) => page.$eval(countSel, (el) => el.textContent)
  const provisional = (page) => page.$('[data-tea-provisional]').then((el) => el !== null)
  const plusOf = (page) => page.locator('.counter button').filter({ hasText: /^\+$/ })

  const a = await ctx.newPage()
  await a.goto(`${base}/app/index.html`)
  await a.waitForSelector(countSel, { timeout: WAIT_MS })
  await until('life 1: Hello adopted', () => provisional(a), (p) => p === false)
  await plusOf(a).click()
  await until(
    'life 1: acked checkpoint written',
    () => parsedRows(a),
    /* Repr omits an empty list field on encode: no `queue` key IS the
       acked-empty queue. */
    (rows) => rows.length === 1 && Number(rows[0]?.next) === 2 && (rows[0]?.queue ?? []).length === 0
  )
  await a.close()
  await untilWriterFree(ctx, base)

  const b = await ctx.newPage()
  await b.routeWebSocket(/\/ws$/, () => {})
  await b.goto(`${base}/app/index.html`)
  await b.waitForSelector(countSel, { timeout: WAIT_MS })
  out.push(
    await transition(
      'B11: paint-from-snapshot-offline - a reload with the socket dead paints the persisted count',
      () => countOf(b),
      (v) => v === '1',
      /* App.init's 0 is the true before-state of every fresh document; the
         paint races this read, so it is supplied rather than sampled. */
      { pre: '0' }
    )
  )
  out.push(
    check(
      'B11: provisional-until-hello - the offline paint carries data-tea-provisional',
      await provisional(b)
    )
  )
  await plusOf(b).click()
  await until(
    'life 2: offline edit checkpointed',
    () => parsedRows(b),
    (rows) => rows.length === 1 && Number(rows[0]?.next) === 3 && (rows[0]?.queue ?? []).length === 1
  )
  await b.close()
  await untilWriterFree(ctx, base)

  const c = await ctx.newPage()
  out.push(
    await transition(
      'B11: queued-edit-reaches-server-after-reload - the offline click lands after the next boot',
      () => serverCount(ctx, base),
      (v) => v === '2',
      {
        act: async () => {
          await c.goto(`${base}/app/index.html`)
          await c.waitForSelector(countSel, { timeout: WAIT_MS })
        },
      }
    )
  )
  out.push(
    await transition(
      'B11: provisional-cleared-on-hello - the marker leaves when the first Hello lands',
      () => provisional(c),
      (v) => v === false,
      /* Set synchronously at mount, before any async work could observe it. */
      { pre: true }
    )
  )
  await ctx.close()
  return out
}

/* B12: two live tabs, one writer. The election is read straight off the Web
   Locks API; liveness of the LOSER is asserted on the server, because a
   memory-only tab that silently dropped edits would look identical in its
   own DOM (lens-4). */
async function twoTabWriterScenario(browser, base) {
  const ctx = await browser.newContext()
  const out = []
  const countSel = '.counter .count'
  const open = async () => {
    const p = await ctx.newPage()
    await p.goto(`${base}/app/index.html`)
    await p.waitForSelector(countSel, { timeout: WAIT_MS })
    return p
  }
  const a = await open()
  const b = await open()
  const election = await b.evaluate(async () => {
    const held = (await navigator.locks.query()).held ?? []
    const writers = held.filter((l) => l.name === 'tea-local-writer:Counter')
    const acquirable = await navigator.locks.request(
      'tea-local-writer:Counter',
      { mode: 'exclusive', ifAvailable: true },
      (lock) => lock !== null
    )
    return { writers: writers.length, acquirable }
  })
  out.push(
    check(
      'B12: second-tab-boots-memory-only - exactly one page holds the writer lock and it is not free',
      election.writers === 1 && election.acquirable === false,
      show(election)
    )
  )
  const plusOf = (p) => p.locator('.counter button').filter({ hasText: /^\+$/ })
  out.push(
    await transition(
      'B12: both-tabs-edits-reach-server-state - one click in each tab drives the server 0 -> 2',
      () => serverCount(ctx, base),
      (v) => v === '2',
      {
        act: async () => {
          await plusOf(a).click()
          await plusOf(b).click()
        },
      }
    )
  )
  out.push(
    await transition(
      'B12: no-silent-drop-server-observed - the memory-only tab keeps landing edits (2 -> 4)',
      () => serverCount(ctx, base),
      (v) => v === '4',
      {
        act: async () => {
          await plusOf(b).click()
          await plusOf(b).click()
        },
      }
    )
  )
  await ctx.close()
  return out
}

/* B13: the API-absent degrade. IndexedDB is deleted before any page script
   runs; the page must mount, stay fully live, and say nothing scary. */
async function idbAbsentScenario(browser, base) {
  const ctx = await browser.newContext()
  await ctx.addInitScript(() => {
    Object.defineProperty(window, 'indexedDB', { value: undefined, configurable: true })
  })
  const out = []
  const errors = []
  const page = await ctx.newPage()
  page.on('pageerror', (e) => errors.push('pageerror: ' + String(e)))
  page.on('console', (m) => {
    if (m.type() === 'error') errors.push('console: ' + m.text())
  })
  await page.goto(`${base}/app/index.html`)
  const mounted = await page
    .waitForSelector('.counter .count', { timeout: WAIT_MS })
    .then(() => true)
    .catch(() => false)
  out.push(check('B13: boots-without-indexeddb - the bundle mounts with the API deleted', mounted))
  out.push(
    await transition(
      'B13: live-edit-flows-without-store - a click still reaches the server, 0 -> 1',
      () => serverCount(ctx, base),
      (v) => v === '1',
      { act: () => page.locator('.counter button').filter({ hasText: /^\+$/ }).click() }
    )
  )
  out.push(
    check(
      'B13: no-uncaught-console-error - the degrade is silent',
      errors.length === 0,
      errors.join(' | ')
    )
  )
  await ctx.close()
  return out
}

const main = async () => {
  const browser = await chromium.launch({ headless: !HEADED })
  /* A scenario that throws (a selector never appearing, a server that never
     came up) becomes one failed check rather than killing the run, so the
     other scenario still reports. */
  const guarded = async (name, run) =>
    run().catch((e) => [check(`${name}: scenario ran to completion`, false, String(e && e.message ? e.message : e))])
  try {
    const counter = await withServer(
      {
        name: 'counter server',
        exe: '_build/default/examples/counter/server/main.exe',
        clientDir: '_build/default/examples/counter/client',
        port: PORT_COUNTER,
      },
      ({ base }) => guarded('counter', () => counterScenario(browser, base))
    )
    const sharedDoc = await withServer(
      {
        name: 'shared_doc server',
        exe: '_build/default/examples/shared_doc/server/main.exe',
        clientDir: '_build/default/examples/shared_doc/client',
        port: PORT_SHARED_DOC,
      },
      ({ base }) => guarded('shared_doc', () => sharedDocScenario(browser, base))
    )
    /* B1-B3 share one mem-tier server: contexts isolate the sessions, so the
       three scenarios land on three branches and three replica ids. TEA_ROOT
       is genuinely unset here (spawnServer strips it), so this server is the
       step-10 in-memory guard byte for byte. */
    const replay = await withServer(
      {
        name: 'replay counter server',
        exe: '_build/default/examples/counter/server/main.exe',
        clientDir: '_build/default/examples/counter/client',
        port: PORT_REPLAY,
      },
      async ({ base }) => [
        ...(await guarded('replay B1', () => dropUpScenario(browser, base))),
        ...(await guarded('replay B2', () => twoTabScenario(browser, base))),
        ...(await guarded('replay B3', () => dropAckScenario(browser, base))),
      ]
    )
    /* B4 owns its server lifecycle (it restarts it mid-scenario), so it is
       not a withServer body. */
    const durable = await guarded('durable B4', () => durableRestartScenario(browser))
    const secret = await guarded('secret B5', () => secretConverseScenario(browser))
    const preflight = await guarded('preflight B6', () => packRootPreflightScenario())
    const orphan = await guarded('orphan B7', () => orphanedJournalScenario())
    /* B8 owns three server lives and a filesystem rollback between two of
       them, so like B4 it is not a withServer body. */
    const rollback = await guarded('rollback B8', () => rollbackScenario(browser))
    /* B9 runs on the mem tier deliberately: the binary's non-TEA_ROOT arm
       still gates /rpc through Rpc_once.once_mem, so the keyed channel is
       live, and the reply cache it exercises is per-process anyway. The
       durable tier is B10's, where the guarantee is about what survives the
       process rather than what the cache remembers inside it. */
    const lostReply = await withServer(
      {
        name: 'lost-reply shared_doc server',
        exe: '_build/default/examples/shared_doc/server/main.exe',
        clientDir: '_build/default/examples/shared_doc/client',
        port: PORT_LOST_REPLY,
      },
      ({ base }) => guarded('lost reply B9', () => lostReplyScenario(browser, base))
    )
    /* B10 restarts its server mid-scenario, so like B4 and B8 it owns its own
       lifecycle rather than running as a withServer body. */
    const lostReplyRestart = await guarded('lost reply restart B10', () => lostReplyRestartScenario(browser))
    /* B11-B13 share one mem-tier server, like B1-B3: contexts isolate the
       sessions, so each scenario owns a branch, a replica and an origin-load
       of browser storage state. */
    const localTier = await withServer(
      {
        name: 'local-tier counter server',
        exe: '_build/default/examples/counter/server/main.exe',
        clientDir: '_build/default/examples/counter/client',
        port: PORT_LOCAL,
      },
      async ({ base }) => [
        ...(await guarded('local tier B11', () => persistReloadScenario(browser, base))),
        ...(await guarded('local tier B12', () => twoTabWriterScenario(browser, base))),
        ...(await guarded('local tier B13', () => idbAbsentScenario(browser, base))),
      ]
    )
    return [
      ...counter,
      ...sharedDoc,
      ...replay,
      ...durable,
      ...secret,
      ...preflight,
      ...orphan,
      ...rollback,
      ...lostReply,
      ...lostReplyRestart,
      ...localTier,
    ]
  } finally {
    await browser.close()
  }
}

const results = await main().catch((e) => [check('browser smoke harness ran', false, String(e && e.stack ? e.stack : e))])

/* A pin that holds is `xfail`; a pin that stops holding is a failure that says
   so in its own label, because the fix has to come back through this file. */
const verdict = (r) => (r.kind === 'pin' ? (r.ok ? 'xfail' : 'STALE') : r.ok ? 'ok   ' : 'FAIL ')
results.forEach((r) => console.log(`${verdict(r)} ${r.label}${r.detail ? `  [${r.detail}]` : ''}`))

const failed = results.filter((r) => !r.ok).length
const pinned = results.filter((r) => r.kind === 'pin' && r.ok).length
console.log(`\n${results.length - failed - pinned} ok / ${pinned} xfail (pinned known bug) / ${failed} FAIL`)
process.exit(failed === 0 ? 0 : 1)
