import './main.css'
import { unzipSync } from 'fflate'
import { Elm } from './src/Main.elm'
import GeometryWorker from './geometry-worker.js?worker'

function pathFromBase(relativePath) {
  const rawBase = String(import.meta.env.BASE_URL ?? '/')
  const base = rawBase.endsWith('/') ? rawBase : `${rawBase}/`
  const relative = String(relativePath ?? '').replace(/^\/+/, '')
  return `${base}${relative}`
}

const defaultLdrawBase = pathFromBase('ldraw')
const defaultFallbackBase = ''
const defaultMaxRpm = 50
const defaultModelUrl = pathFromBase('examples/gears.ldr')

const ldrawBase = import.meta.env.VITE_LDRAW_BASE ?? defaultLdrawBase
const ldrawFallbackBase =
  import.meta.env.VITE_LDRAW_FALLBACK_BASE ?? defaultFallbackBase
const maxRpmRaw = Number(import.meta.env.VITE_MAX_RPM ?? defaultMaxRpm)
const maxRpm = Number.isFinite(maxRpmRaw) && maxRpmRaw >= 1 ? maxRpmRaw : defaultMaxRpm

const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: {
    ldrawBase,
    ldrawFallbackBase,
    defaultModelUrl,
    initialHash: window.location.hash ?? '',
    maxRpm,
  },
})

const geometryWorker = new GeometryWorker()
const geometryFlattenCache = new Map()
const GEOMETRY_FLATTEN_CACHE_LIMIT = 8
let pendingFlattenKey = null

function rememberFlattenResult(cacheKey, resultText) {
  geometryFlattenCache.set(cacheKey, resultText)
  if (geometryFlattenCache.size > GEOMETRY_FLATTEN_CACHE_LIMIT) {
    const oldestKey = geometryFlattenCache.keys().next().value
    if (oldestKey !== undefined) {
      geometryFlattenCache.delete(oldestKey)
    }
  }
}

geometryWorker.addEventListener('message', (event) => {
  const text = String(event.data ?? '')
  try {
    const parsed = JSON.parse(text)
    if (parsed.ok) {
      if (typeof pendingFlattenKey === 'string') {
        rememberFlattenResult(pendingFlattenKey, text)
      }
      pendingFlattenKey = null
      app.ports.geometryFlattened.send(text)
    } else {
      pendingFlattenKey = null
      app.ports.geometryFlattenFailed.send(
        parsed.error ?? 'Geometry worker failed',
      )
    }
  } catch {
    pendingFlattenKey = null
    app.ports.geometryFlattenFailed.send('Invalid geometry worker response')
  }
})

app.ports.requestGeometryFlatten.subscribe((payload) => {
  const cacheHit = geometryFlattenCache.get(payload)
  if (cacheHit) {
    app.ports.geometryFlattened.send(cacheHit)
    return
  }

  pendingFlattenKey = payload
  geometryWorker.postMessage(payload)
})

app.ports.setUrlHash.subscribe((hash) => {
  const normalized = String(hash ?? '')
  const withPrefix = normalized.startsWith('#') ? normalized : `#${normalized}`
  if (window.location.hash !== withPrefix) {
    window.history.replaceState(null, '', withPrefix)
  }
})

// ── File upload port ──────────────────────────────────────────────────────────

// Hidden file input — created once, reused for every upload request.
const fileInput = document.createElement('input')
fileInput.type = 'file'
fileInput.accept = '.ldr,.mpd,.dat,.io'
fileInput.style.display = 'none'
document.body.appendChild(fileInput)

const allowedExtensions = new Set(['.ldr', '.mpd', '.dat', '.io'])

function hasAllowedExtension(filename) {
  const lower = filename.toLowerCase()
  return [...allowedExtensions].some((ext) => lower.endsWith(ext))
}

function reportLoadError(message) {
  app.ports.fileLoadError.send(message)
}

function readLdrawFile(file) {
  if (!file) return

  if (!hasAllowedExtension(file.name)) {
    reportLoadError(
      `Unsupported file type "${file.name}". Please use .ldr, .mpd, .dat, or .io.`,
    )
    return
  }

  const lowerName = file.name.toLowerCase()
  if (lowerName.endsWith('.io')) {
    readStudioIoFile(file)
    return
  }

  const reader = new FileReader()
  reader.onload = (e) => {
    app.ports.fileContentReceived.send(e.target.result)
  }
  reader.onerror = () => {
    reportLoadError(`Failed to read "${file.name}".`)
  }
  reader.readAsText(file)
}

function readStudioIoFile(file) {
  const reader = new FileReader()
  reader.onload = (e) => {
    try {
      const arrayBuffer = e.target?.result
      if (!(arrayBuffer instanceof ArrayBuffer)) {
        throw new Error('Invalid .io file payload')
      }
      const text = extractLdrawFromStudioArchive(arrayBuffer)
      app.ports.fileContentReceived.send(text)
    } catch (error) {
      const details = error instanceof Error ? error.message : String(error)
      reportLoadError(`Failed to read "${file.name}" as BrickLink Studio .io: ${details}`)
    }
  }
  reader.onerror = () => {
    reportLoadError(`Failed to read "${file.name}".`)
  }
  reader.readAsArrayBuffer(file)
}

function extractLdrawFromStudioArchive(arrayBuffer) {
  const files = unzipSync(new Uint8Array(arrayBuffer))
  const entries = Object.entries(files)
  if (entries.length === 0) {
    throw new Error('Archive is empty')
  }

  const normalizedEntries = entries.map(([name, bytes]) => ({
    name,
    normalized: String(name).replaceAll('\\', '/').toLowerCase(),
    bytes,
  }))

  const preferred = [
    'modelv2.ldr',
    'model.ldr',
    'model2.ldr',
  ]

  for (const target of preferred) {
    const found = normalizedEntries.find((entry) => entry.normalized.endsWith(target))
    if (found) {
      return decodeLdrawBytes(found.bytes)
    }
  }

  const anyLdr = normalizedEntries.find((entry) => entry.normalized.endsWith('.ldr'))
  if (anyLdr) {
    return decodeLdrawBytes(anyLdr.bytes)
  }

  throw new Error('No .ldr model found in archive')
}

function decodeLdrawBytes(bytes) {
  const decoded = new TextDecoder('utf-8').decode(bytes)
  return decoded.charCodeAt(0) === 0xfeff ? decoded.slice(1) : decoded
}

// Elm → JS: open the file picker
app.ports.requestFileUpload.subscribe(() => {
  fileInput.value = '' // reset so the same file can be re-selected
  fileInput.click()
})

// User picked a file → read it and send back to Elm
fileInput.addEventListener('change', () => {
  const file = fileInput.files[0]
  readLdrawFile(file)
})

// Global drag-and-drop support
function suppressDefaultDragBehavior(event) {
  event.preventDefault()
  event.stopPropagation()
}

window.addEventListener('dragenter', suppressDefaultDragBehavior)
window.addEventListener('dragover', suppressDefaultDragBehavior)

window.addEventListener('drop', (event) => {
  suppressDefaultDragBehavior(event)

  const droppedFile = event.dataTransfer?.files?.[0]
  if (!droppedFile) {
    reportLoadError('Drop a .ldr, .mpd, .dat, or .io file to load a model.')
    return
  }

  readLdrawFile(droppedFile)
})

// iOS Safari pinch-zoom gesture events are separate from Touch/Pointer events.
// Prevent default page zoom so model controls remain responsive.
function suppressGestureZoom(event) {
  event.preventDefault()
}

window.addEventListener('gesturestart', suppressGestureZoom, { passive: false })
window.addEventListener('gesturechange', suppressGestureZoom, { passive: false })
window.addEventListener('gestureend', suppressGestureZoom, { passive: false })
