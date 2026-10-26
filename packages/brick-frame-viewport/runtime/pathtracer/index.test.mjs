/**
 * End-to-end test of the orchestrator protocol (index.js) with the real
 * worker.js. Each "Web Worker" is a Node worker_threads thread running
 * worker.js under a minimal `self` shim, exposed with the Web Worker surface
 * the orchestrator uses (`onmessage`, `onerror`, `postMessage`, `terminate`).
 */
import assert from 'node:assert/strict'
import test from 'node:test'
import { Worker } from 'node:worker_threads'
import { renderSnapshot } from './index.js'

const WORKER_HREF = new URL('./worker.js', import.meta.url).href

/**
 * Runs in the thread: installs `self` before worker.js loads, then forwards
 * messages from the parent port into `self.onmessage`. Messages posted before
 * the listener is attached wait in the port's queue.
 */
const SHIM = `
const { parentPort } = require('node:worker_threads')
globalThis.self = {
  onmessage: null,
  postMessage: (msg, transfer) => parentPort.postMessage(msg, transfer),
}
import(${JSON.stringify(WORKER_HREF)}).then(
  () => parentPort.on('message', (data) => self.onmessage({ data })),
  (err) => parentPort.postMessage({ type: 'error', message: String(err && err.stack || err) }),
)
`

/** Web-Worker-like wrapper around a worker_threads thread running worker.js. */
class ThreadWorker {
  constructor() {
    this.thread = new Worker(SHIM, { eval: true })
    this.onmessage = null
    this.onerror = null
    this.terminated = false
    this.thread.on('message', (data) => this.onmessage?.({ data }))
    this.thread.on('error', (err) => this.onerror?.({ message: err.message }))
  }

  postMessage(msg, transfer) {
    this.thread.postMessage(msg, transfer)
  }

  terminate() {
    this.terminated = true
    return this.thread.terminate()
  }
}

// A white-ish square at height 0 and a red one above it, both in view.
const SQUARE = [
  -60, 0, -60, 60, 0, -60, 60, 0, 60, 0.9, 0.9, 0.9, 1, 0,
  -60, 0, -60, 60, 0, 60, -60, 0, 60, 0.9, 0.9, 0.9, 1, 0,
  -20, 30, -20, 20, 30, -20, 20, 30, 20, 0.8, 0.1, 0.1, 1, 0,
  -20, 30, -20, 20, 30, 20, -20, 30, 20, 0.8, 0.1, 0.1, 1, 0,
]
const CAMERA = { position: [0, 80, 300], target: [0, 30, 0], up: [0, 1, 0], fovYDegrees: 45 }
const WIDTH = 24
const HEIGHT = 16

function baseRequest(overrides = {}) {
  return {
    width: WIDTH,
    height: HEIGHT,
    transparent: false,
    camera: CAMERA,
    triangles: Float32Array.from(SQUARE),
    timeBudgetMs: 300,
    ...overrides,
  }
}

/** Test encoder: checks the RGBA buffer size and returns a marker URL. */
function fakeEncode(seen) {
  return async (rgba, width, height) => {
    seen.push({ length: rgba.length, width, height })
    return 'data:image/png;base64,FAKE'
  }
}

function recordingFactory() {
  const workers = []
  const createWorker = () => {
    const w = new ThreadWorker()
    workers.push(w)
    return w
  }
  return { workers, createWorker }
}

test('renderSnapshot resolves with a PNG, reports progress, and terminates its workers', async () => {
  const { workers, createWorker } = recordingFactory()
  const progress = []
  const encoded = []
  const { dataUrl } = await renderSnapshot(baseRequest(), {
    onProgress: (p) => progress.push(p),
    workerCount: 2,
    createWorker,
    encodePng: fakeEncode(encoded),
    minBudgetMs: 0,
  })
  assert.equal(dataUrl, 'data:image/png;base64,FAKE')
  assert.equal(workers.length, 2)
  assert.ok(progress.length >= 1, 'progress should be reported at least once')
  for (const p of progress) {
    assert.ok(p.fraction >= 0 && p.fraction <= 1, `fraction ${p.fraction}`)
    assert.ok(p.samples >= 0)
  }
  assert.deepEqual(encoded, [{ length: WIDTH * HEIGHT * 4, width: WIDTH, height: HEIGHT }])
  assert.ok(workers.every((w) => w.terminated), 'every worker is terminated after resolving')
})

test('aborting through AbortController rejects with AbortError and terminates the workers', async () => {
  const { workers, createWorker } = recordingFactory()
  const controller = new AbortController()
  const pending = renderSnapshot(baseRequest({ timeBudgetMs: 5000 }), {
    signal: controller.signal,
    workerCount: 2,
    createWorker,
    encodePng: fakeEncode([]),
    minBudgetMs: 0,
  })
  setTimeout(() => controller.abort(), 50)
  await assert.rejects(pending, (err) => err.name === 'AbortError')
  assert.equal(workers.length, 2)
  assert.ok(workers.every((w) => w.terminated), 'abort terminates every worker')
})

test('a wrong triangle stride rejects before any worker starts', async () => {
  const { workers, createWorker } = recordingFactory()
  await assert.rejects(
    renderSnapshot(baseRequest({ triangles: new Float32Array(15) }), { workerCount: 2, createWorker, minBudgetMs: 0 }),
    /14 numbers per triangle/,
  )
  assert.equal(workers.length, 0)
})

test('an already-aborted signal rejects with AbortError without starting workers', async () => {
  const { workers, createWorker } = recordingFactory()
  const controller = new AbortController()
  controller.abort()
  await assert.rejects(
    renderSnapshot(baseRequest(), { signal: controller.signal, workerCount: 2, createWorker }),
    (err) => err.name === 'AbortError',
  )
  assert.equal(workers.length, 0)
})
