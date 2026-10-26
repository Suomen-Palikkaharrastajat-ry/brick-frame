/**
 * Snapshot capture and save for the host apps' camera-globe snapshot button.
 *
 * A host calls `initSnapshots(app.ports)` and declares the ports below. Every
 * port is optional; missing ones are skipped.
 *
 * - `captureRender` (Elm → JS): `{ width, height, transparent, filename,
 *   timeBudgetMs, camera, triangles }`. When `triangles` is a non-empty flat
 *   array (14 numbers per triangle, render space, LDU), the frame is rendered
 *   by the offline progressive path tracer in `pathtracer/`. Any render still
 *   in flight is cancelled first. Without `triangles`, the largest visible
 *   WebGL canvas is copied onto an offscreen canvas that fits inside
 *   `width` × `height` with its aspect ratio kept (the fallback). Unless
 *   `transparent`, the page's `--color-surface-canvas` token fills the
 *   background first.
 * - `cancelSnapshot` (Elm → JS): stops the render in flight and terminates
 *   its workers. Nothing is sent for a cancelled render.
 * - `snapshotProgress` (JS → Elm): `{ fraction, samples }`, at most about
 *   four times a second while the path tracer runs.
 * - `snapshotReady` (JS → Elm): `{ dataUrl, filename }`, sent as soon as the
 *   PNG is encoded.
 * - `snapshotFailed` (JS → Elm): a string error message, e.g. no triangles or
 *   a worker error.
 * - `saveSnapshot` (Elm → JS): `{ dataUrl, filename }`, downloaded as a file.
 */
import { renderSnapshot } from './pathtracer/index.js'

const DEFAULT_FILENAME = 'palikkakehys-render.png'

/** The render currently in flight, or null. */
let activeRender = null

/** The visible canvas with the largest on-screen area, or null when none is shown. */
function largestVisibleCanvas(doc) {
  let best = null
  let bestArea = 0
  for (const canvas of doc.querySelectorAll('canvas')) {
    const area = canvas.clientWidth * canvas.clientHeight
    if (area > bestArea) {
      best = canvas
      bestArea = area
    }
  }
  return best
}

/**
 * Size of `source` scaled to fit inside `maxWidth` × `maxHeight`, keeping its
 * aspect ratio. A missing bound leaves that axis to the source size.
 */
export function fittedSize(sourceWidth, sourceHeight, maxWidth, maxHeight) {
  const scale = Math.min(
    maxWidth > 0 ? maxWidth / sourceWidth : Infinity,
    maxHeight > 0 ? maxHeight / sourceHeight : Infinity,
  )
  const finiteScale = Number.isFinite(scale) ? scale : 1
  return {
    width: Math.max(1, Math.round(sourceWidth * finiteScale)),
    height: Math.max(1, Math.round(sourceHeight * finiteScale)),
  }
}

function fillBackground(ctx, width, height) {
  const bg = getComputedStyle(document.documentElement)
    .getPropertyValue('--color-surface-canvas')
    .trim()
  if (!bg) return
  ctx.fillStyle = bg
  ctx.fillRect(0, 0, width, height)
}

/** Fallback: copies the on-screen WebGL canvas. */
function captureCanvasCopy(ports, { width, height, transparent, filename }) {
  const canvas = largestVisibleCanvas(document)
  if (!canvas) return

  const sourceWidth = canvas.width || canvas.clientWidth
  const sourceHeight = canvas.height || canvas.clientHeight
  if (!sourceWidth || !sourceHeight) return

  const size = fittedSize(sourceWidth, sourceHeight, width, height)

  const offscreen = document.createElement('canvas')
  offscreen.width = size.width
  offscreen.height = size.height
  const ctx = offscreen.getContext('2d')
  if (!ctx) return

  if (!transparent) fillBackground(ctx, size.width, size.height)

  ctx.imageSmoothingEnabled = true
  ctx.imageSmoothingQuality = 'high'
  ctx.drawImage(canvas, 0, 0, size.width, size.height)

  ports.snapshotReady?.send({
    dataUrl: offscreen.toDataURL('image/png'),
    filename: filename || DEFAULT_FILENAME,
  })
}

function cancelRender() {
  if (!activeRender) return
  const render = activeRender
  activeRender = null
  render.abort()
}

/** Path-traced render; reports progress, then the PNG or a failure message. */
function startPathTrace(ports, request) {
  cancelRender()
  const controller = new AbortController()
  activeRender = controller
  const filename = request.filename || DEFAULT_FILENAME
  renderSnapshot(request, {
    signal: controller.signal,
    onProgress: (progress) => ports.snapshotProgress?.send(progress),
  })
    .then(({ dataUrl }) => {
      if (activeRender !== controller) return
      activeRender = null
      ports.snapshotReady?.send({ dataUrl, filename })
    })
    .catch((err) => {
      if (controller.signal.aborted || activeRender !== controller) return
      activeRender = null
      ports.snapshotFailed?.send(String(err?.message ?? err))
    })
}

function captureSnapshot(ports, request) {
  if (request.triangles !== undefined && request.triangles !== null) {
    startPathTrace(ports, request)
  } else {
    captureCanvasCopy(ports, request)
  }
}

function downloadSnapshot({ dataUrl, filename }) {
  const link = document.createElement('a')
  link.href = dataUrl
  link.download = filename || DEFAULT_FILENAME
  document.body.appendChild(link)
  link.click()
  link.remove()
}

/** Wire the snapshot ports of a host app. Ports the host does not declare are skipped. */
export function initSnapshots(ports) {
  if (ports?.captureRender) {
    ports.captureRender.subscribe((request) => captureSnapshot(ports, request))
  }
  if (ports?.cancelSnapshot) {
    ports.cancelSnapshot.subscribe(cancelRender)
  }
  if (ports?.saveSnapshot) {
    ports.saveSnapshot.subscribe(downloadSnapshot)
  }
}
