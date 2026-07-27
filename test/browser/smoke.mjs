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
 */

import { chromium } from 'playwright'
import { spawn } from 'node:child_process'
import { existsSync } from 'node:fs'
import { mkdtemp, rm } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import { dirname, join, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), '..', '..')
const PORT_COUNTER = Number(process.env.SMOKE_PORT_COUNTER ?? 8137)
const PORT_SHARED_DOC = Number(process.env.SMOKE_PORT_SHARED_DOC ?? 8138)
const PORT_REPLAY = Number(process.env.SMOKE_PORT_REPLAY ?? 8139)
const PORT_DURABLE = Number(process.env.SMOKE_PORT_DURABLE ?? 8140)
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
   pack-tier one - the mem scenarios run with it genuinely UNSET, not "". */
async function spawnServer({ name, exe, clientDir, port, env = {} }) {
  const exePath = resolve(REPO, exe)
  if (!existsSync(exePath)) throw new Error(`${name}: ${exe} is missing - run \`dune build\` first`)
  const ambient = { ...process.env }
  delete ambient.TEA_ROOT
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
  return { name, proc, base, tail, exited }
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

  const tagCount = (r) => (r.status === 200 ? JSON.parse(r.body) : null)
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
  const tap = { applies: 0, acks: 0, dropUps: 0, dropAcks: false, socket: null }
  tap.handler = (ws) => {
    tap.socket = ws
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
        if (tap.dropAcks) return
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
  const root = await mkdtemp(join(tmpdir(), 'tea-smoke-b4-'))
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

    tap.dropAcks = false
    const outcome = await stop(srv, 'SIGTERM')
    if (!step(check(
      'durable B4: SIGTERM stops the pack server gracefully (exit 0 runs the store-then-journal teardown)',
      outcome.code === 0,
      `exit=${show(outcome)}`
    ))) return out

    const acksAtRestart = tap.acks
    srv = await server()
    if (!step(await transition(
      'durable B4: after the restart the client replays its unacked message and the server answers (a post-restart Ack arrives)',
      () => tap.acks,
      (n) => n > acksAtRestart,
      { pre: acksAtRestart, timeout: WAIT_MS * 2 }
    ))) return out

    const postA = await countOn(a)
    const postO = await countOn(o)
    if (!step(check(
      'durable B4: exactly-once across the restart - both tabs read 2 (not 1: lost store; not 3: double apply)',
      postA === '2' && postO === '2',
      `A=${postA} O=${postO}`
    ))) return out

    if (!step(await transition(
      'durable B4: a follow-up click moves the count 2 -> 3 (the guard admits fresh edits after the restart)',
      () => countOn(o),
      (v) => v === '3',
      { act: () => plusOn(a).click() }
    ))) return out

    step(check('durable B4: no uncaught browser exception', errors.length === 0, errors.join(' | ')))
    return out
  } finally {
    if (ctx) await ctx.close().catch(() => {})
    if (srv) await stop(srv, 'SIGTERM').catch(() => {})
    /* The journal is a SIBLING of the pack root (serve_pack keeps it out of
       irmin-pack's directory on purpose), so both must go. */
    await rm(root, { recursive: true, force: true })
    await rm(root + '.guard', { recursive: true, force: true })
  }
}

/* -------------------------------------------------------------------- main */

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
    return [...counter, ...sharedDoc, ...replay, ...durable]
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
