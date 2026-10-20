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
  const url = URL.createObjectURL(new Blob([text], { type: mime }))
  const link = document.createElement('a')
  link.href = url
  link.download = filename
  document.body.appendChild(link)
  link.click()
  link.remove()
  URL.revokeObjectURL(url)
})

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
    } else if (key === 'd' || key === 'a' || key === 'z' || key === 'y' || key === 'g') {
      event.preventDefault()
    }
  }
})
