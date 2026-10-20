import './main.css'
import { initBricksRuntime } from '../packages/brick-frame-viewport/runtime/initBricksRuntime.js'
import { Elm } from './src/Main.elm'
import {
  openThumbnailDb,
  getCachedThumbnail,
  storeCachedThumbnail,
  renderIsometricThumbnail,
} from './thumbnail-pipeline.js'

// The studio is served below the viewer (`<site>/studio/`) and shares its LDraw
// library, fonts and example models, which live at the site root.
const siteRoot = new URL(import.meta.env.VITE_SITE_ROOT ?? '../', window.location.href).href

const DRAFT_KEY = 'palikkakehys-studio-draft'
const FAVORITES_KEY = 'palikkakehys-studio-favorites'
const RECENTS_KEY = 'palikkakehys-studio-recents'

function readStorage(key, fallback = null) {
  try {
    const raw = window.localStorage.getItem(key)
    return raw !== null ? JSON.parse(raw) : fallback
  } catch (_) {
    return fallback
  }
}

function writeStorage(key, value) {
  try {
    window.localStorage.setItem(key, JSON.stringify(value))
  } catch (_) {}
}

function readDraft() {
  try {
    return window.localStorage.getItem(DRAFT_KEY)
  } catch (_) {
    return null
  }
}

// What to open, in order: a model named in the URL (`#src=<url>`; the camera keys
// in the same hash are read by the viewport itself), the draft left by the last
// session, then the example model.
const hashParams = new URLSearchParams(window.location.hash.replace(/^#/, ''))
const requestedModel = hashParams.get('src')
const draft = requestedModel ? null : readDraft()
const favorites = readStorage(FAVORITES_KEY, [])
const recentParts = readStorage(RECENTS_KEY, [])
const model = requestedModel ?? (draft !== null ? '' : `${siteRoot}examples/wheeler.ldr`)

const ldrawBase = import.meta.env.VITE_LDRAW_BASE ?? `${siteRoot}ldraw`

const runtime = initBricksRuntime({
  elm: Elm.Main,
  node: document.getElementById('app'),
  mode: 'simulator',
  ldrawBase,
  ldrawFallbackBase: import.meta.env.VITE_LDRAW_FALLBACK_BASE ?? '',
  defaultModel: model,
  loadModelInJs: true,
  extraFlags: { draft, favorites, recentParts },
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

// Draft: write latest edits to localStorage with debounce
let pendingDraft = null
let draftTimer = null
ports.saveDraft.subscribe((text) => {
  pendingDraft = text
  if (draftTimer !== null) return
  draftTimer = window.setTimeout(() => {
    draftTimer = null
    try {
      window.localStorage.setItem(DRAFT_KEY, pendingDraft)
    } catch (_) {}
  }, 500)
})

// Favorites and recents persistence
ports.saveFavorites?.subscribe((favs) => {
  writeStorage(FAVORITES_KEY, favs)
})

ports.saveRecents?.subscribe((recents) => {
  writeStorage(RECENTS_KEY, recents)
})

// Thumbnail rendering pipeline & IndexedDB caching
openThumbnailDb().then((db) => {
  if (!ports.requestThumbnails || !ports.thumbnailLoaded) return

  const queue = []
  let processing = false

  async function processQueue() {
    if (processing || queue.length === 0) return
    processing = true

    while (queue.length > 0) {
      const item = queue.shift()
      const { file, color } = item
      try {
        let url = await getCachedThumbnail(db, file, color)
        if (!url) {
          url = await renderIsometricThumbnail(file, color, ldrawBase)
          if (url) {
            await storeCachedThumbnail(db, file, color, url)
          }
        }
        if (url) {
          ports.thumbnailLoaded.send({ file, color, url })
        }
      } catch (err) {
        console.warn('Thumbnail generation failed for', file, err)
      }
      // Yield briefly between renders to preserve 60fps main thread responsiveness
      await new Promise((r) => setTimeout(r, 8))
    }

    processing = false
  }

  ports.requestThumbnails.subscribe((items) => {
    for (const item of items) {
      if (!queue.some((q) => q.file === item.file && q.color === item.color)) {
        queue.push(item)
      }
    }
    processQueue()
  })
})

// Browser shortcuts that clash with the studio (save, duplicate, select all, undo/redo).
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
