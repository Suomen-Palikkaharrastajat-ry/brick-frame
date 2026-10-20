import './main.css'
import { initBricksRuntime } from '../packages/brick-frame-viewport/runtime/initBricksRuntime.js'
import { Elm } from './src/Main.elm'

// The studio is served below the viewer (`<site>/studio/`) and shares its LDraw
// library, fonts and example models, which live at the site root.
const siteRoot = new URL(import.meta.env.VITE_SITE_ROOT ?? '../', window.location.href).href

const DRAFT_KEY = 'palikkakehys-studio-draft'

function readDraft() {
  try {
    return window.localStorage.getItem(DRAFT_KEY)
  } catch (_) {
    // Storage can be blocked (private windows, embedded frames); a draft is a
    // convenience, not something the studio needs.
    return null
  }
}

// What to open, in order: a model named in the URL (`#src=<url>`; the camera keys
// in the same hash are read by the viewport itself), the draft left by the last
// session, then the example model.
const hashParams = new URLSearchParams(window.location.hash.replace(/^#/, ''))
const requestedModel = hashParams.get('src')
const draft = requestedModel ? null : readDraft()
// An empty draft is a draft: someone who started a new document gets it back.
const model = requestedModel ?? (draft !== null ? '' : `${siteRoot}examples/wheeler.ldr`)

const runtime = initBricksRuntime({
  elm: Elm.Main,
  node: document.getElementById('app'),
  mode: 'simulator',
  ldrawBase: import.meta.env.VITE_LDRAW_BASE ?? `${siteRoot}ldraw`,
  ldrawFallbackBase: import.meta.env.VITE_LDRAW_FALLBACK_BASE ?? '',
  defaultModel: model,
  loadModelInJs: true,
  extraFlags: { draft },
  initialHash: window.location.hash ?? '',
  syncUrlHash: true,
  useWindowResize: true,
  dragDropTarget: window,
  suppressGestureTarget: window,
})

const { ports } = runtime.app

// Save: hand the browser a file to download.
ports.downloadFile.subscribe(({ filename, mime, text }) => {
  let blob
  if (mime.includes('binary') || mime.includes('zip') || mime.includes('octet-stream')) {
    try {
      const binStr = atob(text)
      const len = binStr.length
      const bytes = new Uint8Array(len)
      for (let i = 0; i < len; i++) {
        bytes[i] = binStr.charCodeAt(i)
      }
      blob = new Blob([bytes], { type: mime })
    } catch (_) {
      blob = new Blob([text], { type: mime })
    }
  } else {
    blob = new Blob([text], { type: mime })
  }
  const url = URL.createObjectURL(blob)
  const link = document.createElement('a')
  link.href = url
  link.download = filename
  document.body.appendChild(link)
  link.click()
  link.remove()
  URL.revokeObjectURL(url)
})

// High-resolution screenshot / render capture
if (ports.captureRender) {
  ports.captureRender.subscribe(({ width, height, transparent, filename }) => {
    const canvas = document.querySelector('canvas')
    if (!canvas) return

    const targetWidth = width || canvas.width
    const targetHeight = height || canvas.height

    const offscreen = document.createElement('canvas')
    offscreen.width = targetWidth
    offscreen.height = targetHeight
    const ctx = offscreen.getContext('2d')

    if (!transparent) {
      // Solid neutral dark theme background
      ctx.fillStyle = '#0f172a'
      ctx.fillRect(0, 0, targetWidth, targetHeight)
    }

    ctx.imageSmoothingEnabled = true
    ctx.imageSmoothingQuality = 'high'
    ctx.drawImage(canvas, 0, 0, targetWidth, targetHeight)

    offscreen.toBlob((blob) => {
      if (!blob) return
      const url = URL.createObjectURL(blob)
      const link = document.createElement('a')
      link.href = url
      link.download = filename || 'palikkakehys-render.png'
      document.body.appendChild(link)
      link.click()
      link.remove()
      URL.revokeObjectURL(url)
    }, 'image/png')
  })
}

// Draft: the document is offered on every change, which is far more often than
// it needs storing — write the latest one once the edits pause.
let pendingDraft = null
let draftTimer = null
ports.saveDraft.subscribe((text) => {
  pendingDraft = text
  if (draftTimer !== null) return
  draftTimer = window.setTimeout(() => {
    draftTimer = null
    try {
      window.localStorage.setItem(DRAFT_KEY, pendingDraft)
    } catch (_) {
      // Quota or blocked storage: the draft is best effort.
    }
  }, 500)
})

// Browser shortcuts that clash with the studio (save, duplicate, select all, undo/redo).
// Elm cannot prevent browser default actions from a keydown subscription.
window.addEventListener('keydown', (event) => {
  if ((event.ctrlKey || event.metaKey) && !event.altKey) {
    const key = event.key.toLowerCase()
    if (key === 's') {
      event.preventDefault()
      ports.saveRequested.send(null)
    } else if (key === 'd' || key === 'a' || key === 'z' || key === 'y') {
      event.preventDefault()
    }
  }
})
