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
 */

import { chromium } from 'playwright'
import { spawn } from 'node:child_process'
import { existsSync } from 'node:fs'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), '..', '..')
const PORT_COUNTER = Number(process.env.SMOKE_PORT_COUNTER ?? 8137)
const PORT_SHARED_DOC = Number(process.env.SMOKE_PORT_SHARED_DOC ?? 8138)
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
   the same-origin gate and the shared Dream session cookie both require. */
async function withServer({ name, exe, clientDir, port }, body) {
  const exePath = resolve(REPO, exe)
  if (!existsSync(exePath)) throw new Error(`${name}: ${exe} is missing - run \`dune build\` first`)
  const proc = spawn(exePath, [], {
    cwd: REPO,
    env: { ...process.env, PORT: String(port), CLIENT_DIR: resolve(REPO, clientDir) },
    stdio: ['ignore', 'pipe', 'pipe'],
  })
  const log = []
  proc.stdout.on('data', (d) => log.push(String(d)))
  proc.stderr.on('data', (d) => log.push(String(d)))
  const base = `http://localhost:${port}`
  const tail = () => log.join('').split('\n').slice(-20).join('\n')
  try {
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
    return await body({ base })
  } finally {
    proc.kill('SIGTERM')
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
    return [...counter, ...sharedDoc]
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
