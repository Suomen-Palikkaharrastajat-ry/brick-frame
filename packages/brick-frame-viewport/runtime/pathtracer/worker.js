/**
 * Module Web Worker for the path tracer. Each worker renders the rows
 * `y ≡ index (mod count)` of the image with one sample per pass.
 *
 * Messages in:
 * - `{ type: 'start', request, triangles, index, count, seed }`: `triangles`
 *   is a Float32Array transferred in; `request` carries width, height,
 *   transparent and camera. Builds the scene, writes the guide buffers, then
 *   renders passes until stopped.
 * - `{ type: 'stop' }`: finish the current pass. Rows that already have a
 *   sample are skipped, so every row ends with at least one sample.
 *
 * Messages out:
 * - `{ type: 'guide', index, albedo, normal }` once, after the scene is built.
 * - `{ type: 'progress', index, samples }` after each pass (samples = fewest
 *   samples in any of this worker's rows).
 * - `{ type: 'done', index, sums, counts }` with the premultiplied RGBA sums
 *   and per-row sample counts (local row order).
 * - `{ type: 'error', message }` on failure.
 *
 * Yielding: rendering yields to the event loop about every YIELD_INTERVAL_MS
 * of work, and always at the end of a pass, so a queued `stop` is seen
 * promptly. Yields use a MessageChannel rather than setTimeout, because
 * browsers clamp nested timeouts to at least 4 ms.
 *
 * `createPathTracerWorker(post)` holds the protocol and is testable without a
 * browser. The module-level wiring at the bottom attaches it to `self`.
 */
import { prepareScene } from './scene.js'
import { createRng } from './rng.js'
import { captureGuide, renderRowSample } from './trace.js'

/** Work time (ms) between yields to the event loop. */
export const YIELD_INTERVAL_MS = 30

/**
 * Returns a `yieldTurn()` that resolves in a later macrotask, via a
 * MessageChannel. Each call posts one message to the channel and resolves
 * when it comes back, so a `stop` message queued meanwhile is handled first.
 */
function createYielder() {
  const channel = new MessageChannel()
  const waiters = []
  channel.port1.onmessage = () => {
    const resolve = waiters.shift()
    if (resolve) resolve()
  }
  return () =>
    new Promise((resolve) => {
      waiters.push(resolve)
      channel.port2.postMessage(null)
    })
}

/**
 * Creates one worker's message handler. `post(message, transfer)` sends a
 * message back to the orchestrator. Returns `{ handle(msg) }` for incoming
 * messages.
 */
export function createPathTracerWorker(post, { yieldTurn = createYielder(), now = () => performance.now() } = {}) {
  let stopRequested = false

  async function run(msg) {
    const { index, count, seed } = msg
    const scene = prepareScene({ ...msg.request, triangles: msg.triangles })
    const width = scene.width
    const rowYs = []
    for (let y = index; y < scene.height; y += count) rowYs.push(y)
    const rows = rowYs.length
    const rowLen = width * 3
    if (rows === 0) {
      post({ type: 'done', index, sums: new Float32Array(0), counts: new Uint32Array(0) }, [])
      return
    }

    const albedo = new Float32Array(rows * rowLen)
    const normal = new Float32Array(rows * rowLen)
    for (let j = 0; j < rows; j++) {
      captureGuide(scene, rowYs[j], albedo.subarray(j * rowLen, (j + 1) * rowLen), normal.subarray(j * rowLen, (j + 1) * rowLen))
    }
    post({ type: 'guide', index, albedo, normal }, [albedo.buffer, normal.buffer])

    const sums = new Float32Array(rows * width * 4)
    const counts = new Uint32Array(rows)
    const rng = createRng(seed)
    const pixel = new Float64Array(4)
    let sliceStart = now()
    for (;;) {
      for (let j = 0; j < rows; j++) {
        if (stopRequested && counts[j] > 0) continue
        renderRowSample(scene, rng, rowYs[j], sums.subarray(j * width * 4, (j + 1) * width * 4), pixel)
        counts[j]++
        if (now() - sliceStart >= YIELD_INTERVAL_MS) {
          await yieldTurn()
          sliceStart = now()
        }
      }
      let fewest = Infinity
      for (let j = 0; j < rows; j++) fewest = Math.min(fewest, counts[j])
      post({ type: 'progress', index, samples: fewest })
      // Always yield at a pass boundary: a fast pass would otherwise never let
      // a queued `stop` message in.
      await yieldTurn()
      sliceStart = now()
      if (stopRequested) break
    }
    post({ type: 'done', index, sums, counts }, [sums.buffer, counts.buffer])
  }

  return {
    handle(msg) {
      if (msg.type === 'stop') {
        stopRequested = true
      } else if (msg.type === 'start') {
        run(msg).catch((err) => {
          post({ type: 'error', message: String(err?.message ?? err) })
        })
      }
    },
  }
}

if (typeof self !== 'undefined' && typeof self.postMessage === 'function') {
  const worker = createPathTracerWorker((msg, transfer = []) => self.postMessage(msg, transfer))
  self.onmessage = (event) => worker.handle(event.data)
}
