/**
 * Offline progressive path tracer for high-quality snapshots.
 *
 * `renderSnapshot(request, { onProgress, signal, workerCount })` splits the
 * image rows between module workers (`worker.js`), renders passes until the
 * time budget runs out, then denoises (`denoise.js`), tone maps
 * (`output.js`) and encodes a PNG data URL. Progress arrives at most 4 times a
 * second as `{ fraction, samples }`, where `fraction` follows elapsed time
 * against the budget and `samples` is the fewest samples in any pixel row.
 *
 * Cancelling through `signal` terminates every worker and rejects with an
 * AbortError. Nothing is posted for a cancelled render.
 */

import { denoise } from './denoise.js'
import { toRgba8 } from './output.js'

const DEFAULT_BUDGET_MS = 30000
const MIN_BUDGET_MS = 5000
const MAX_BUDGET_MS = 120000
const PROGRESS_INTERVAL_MS = 250
const TRIANGLE_STRIDE = 14

/** Time budget in ms, defaulting to 30 s and clamped to [min, 120 s] (min defaults to 5 s). */
export function clampBudget(ms, min = MIN_BUDGET_MS) {
  const value = Number.isFinite(ms) ? ms : DEFAULT_BUDGET_MS
  return Math.min(MAX_BUDGET_MS, Math.max(min, value))
}

/** One worker per spare core, never fewer than one. */
export function workerCountFor(hardwareConcurrency) {
  return Math.max(1, (hardwareConcurrency || 4) - 1)
}

function abortError() {
  const err = new Error('Snapshot render cancelled')
  err.name = 'AbortError'
  return err
}

async function encodePng(rgba, width, height) {
  if (typeof document !== 'undefined') {
    const canvas = document.createElement('canvas')
    canvas.width = width
    canvas.height = height
    const ctx = canvas.getContext('2d')
    if (!ctx) throw new Error('Canvas 2D context unavailable')
    ctx.putImageData(new ImageData(rgba, width, height), 0, 0)
    return canvas.toDataURL('image/png')
  }
  const canvas = new OffscreenCanvas(width, height)
  const ctx = canvas.getContext('2d')
  ctx.putImageData(new ImageData(rgba, width, height), 0, 0)
  const blob = await canvas.convertToBlob({ type: 'image/png' })
  return new Promise((resolve, reject) => {
    const reader = new FileReader()
    reader.onload = () => resolve(reader.result)
    reader.onerror = () => reject(reader.error)
    reader.readAsDataURL(blob)
  })
}

/**
 * Merges the per-worker results into full-image buffers. Row `y` belongs to
 * worker `y % count`, at local row `floor(y / count)`.
 */
function assemble(results, width, height, count) {
  const pixels = width * height
  const sums = new Float32Array(pixels * 4)
  const albedo = new Float32Array(pixels * 3)
  const normal = new Float32Array(pixels * 3)
  for (const result of results) {
    const { index, sums: rowSums, counts, guide } = result
    for (let j = 0; j < counts.length; j++) {
      const y = index + j * count
      const dst = y * width
      sums.set(rowSums.subarray(j * width * 4, (j + 1) * width * 4), dst * 4)
      albedo.set(guide.albedo.subarray(j * width * 3, (j + 1) * width * 3), dst * 3)
      normal.set(guide.normal.subarray(j * width * 3, (j + 1) * width * 3), dst * 3)
    }
  }
  return { sums, albedo, normal }
}

/**
 * Turns accumulated premultiplied sums into unpremultiplied colour and
 * coverage, averaging each row by its own sample count.
 */
function resolveSums(results, width, height, count, sums) {
  const pixels = width * height
  const color = new Float32Array(pixels * 3)
  const alpha = new Float32Array(pixels)
  for (const result of results) {
    for (let j = 0; j < result.counts.length; j++) {
      const n = result.counts[j]
      if (n === 0) continue
      const y = result.index + j * count
      for (let x = 0; x < width; x++) {
        const p = y * width + x
        const a = sums[p * 4 + 3] / n
        alpha[p] = a
        if (a > 0) {
          color[p * 3] = sums[p * 4] / n / a
          color[p * 3 + 1] = sums[p * 4 + 1] / n / a
          color[p * 3 + 2] = sums[p * 4 + 2] / n / a
        }
      }
    }
  }
  return { color, alpha }
}

/** Default worker factory: the module worker next to this file. */
function defaultCreateWorker() {
  return new Worker(new URL('./worker.js', import.meta.url), { type: 'module' })
}

/**
 * Renders the request with path tracing and resolves to `{ dataUrl }` (PNG).
 * `request` is the contract payload (`width`, `height`, `transparent`,
 * `camera`, `triangles`, `timeBudgetMs`); `triangles` may be an array or a
 * typed array of 14 numbers per triangle.
 *
 * Options: `onProgress`, `signal`, `workerCount`, plus two injection points
 * for tests: `createWorker(index)` returns a Web-Worker-like object
 * (`onmessage`, `onerror`, `postMessage`, `terminate`), and `encodePng(rgba,
 * width, height)` returns a PNG data URL. `minBudgetMs` lowers the usual 5 s
 * floor on the time budget.
 */
export async function renderSnapshot(request, options = {}) {
  const { onProgress, signal, createWorker = defaultCreateWorker, encodePng: encode = encodePng } = options
  if (signal?.aborted) throw abortError()

  const source = request.triangles
  if (!source || source.length === 0) throw new Error('No model triangles to render')
  if (source.length % TRIANGLE_STRIDE !== 0) throw new Error('Triangle data is not 14 numbers per triangle')
  const width = Math.round(request.width)
  const height = Math.round(request.height)
  if (!(width > 0 && height > 0)) throw new Error('Render size must be positive')
  const triangles = Float32Array.from(source)
  const budget = clampBudget(request.timeBudgetMs, options.minBudgetMs)
  const count = Math.min(height, options.workerCount ?? workerCountFor(globalThis.navigator?.hardwareConcurrency))
  const shared = {
    width,
    height,
    transparent: Boolean(request.transparent),
    camera: request.camera,
  }

  return new Promise((resolve, reject) => {
    const workers = []
    const results = new Array(count)
    const latest = new Array(count).fill(0)
    let remaining = count
    let settled = false
    let stopTimer = null
    let progressTimer = null
    const guides = new Array(count)
    const startedAt = Date.now()

    function cleanup() {
      settled = true
      clearTimeout(stopTimer)
      clearInterval(progressTimer)
      signal?.removeEventListener('abort', onAbort)
      for (const w of workers) w.terminate()
      workers.length = 0
    }

    function fail(err) {
      if (settled) return
      cleanup()
      reject(err)
    }

    function onAbort() {
      fail(abortError())
    }

    function stopAll() {
      for (const w of workers) w.postMessage({ type: 'stop' })
    }

    function emitProgress() {
      if (settled || !onProgress) return
      const fraction = Math.min(1, (Date.now() - startedAt) / budget)
      onProgress({ fraction, samples: Math.min(...latest) })
    }

    async function finish() {
      const done = results.slice()
      const { sums, albedo, normal } = assemble(
        done.map((r, i) => ({ ...r, guide: guides[i] })),
        width,
        height,
        count,
      )
      const { color, alpha } = resolveSums(done, width, height, count, sums)
      const denoised = denoise(color, alpha, albedo, normal, width, height)
      const rgba = toRgba8(denoised.color, denoised.alpha, width, height)
      const dataUrl = await encode(rgba, width, height)
      if (settled) return
      cleanup()
      resolve({ dataUrl })
    }

    signal?.addEventListener('abort', onAbort, { once: true })

    for (let i = 0; i < count; i++) {
      const worker = createWorker(i)
      workers.push(worker)
      worker.onmessage = (event) => {
        const msg = event.data
        if (settled) return
        if (msg.type === 'guide') {
          guides[msg.index] = { albedo: msg.albedo, normal: msg.normal }
        } else if (msg.type === 'progress') {
          latest[msg.index] = msg.samples
        } else if (msg.type === 'done') {
          results[msg.index] = { index: msg.index, sums: msg.sums, counts: msg.counts }
          remaining--
          if (remaining === 0) finish().catch(fail)
        } else if (msg.type === 'error') {
          fail(new Error(msg.message))
        }
      }
      worker.onerror = (event) => fail(new Error(event.message || 'Path tracer worker failed'))
      const copy = new Float32Array(triangles)
      worker.postMessage(
        { type: 'start', request: shared, triangles: copy, index: i, count, seed: i + 1 },
        [copy.buffer],
      )
    }

    stopTimer = setTimeout(stopAll, budget)
    if (onProgress) progressTimer = setInterval(emitProgress, PROGRESS_INTERVAL_MS)
  })
}
