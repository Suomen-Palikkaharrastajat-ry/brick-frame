import './main.css'
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

geometryWorker.addEventListener('message', (event) => {
  const text = String(event.data ?? '')
  try {
    const parsed = JSON.parse(text)
    if (parsed.ok) {
      app.ports.geometryFlattened.send(text)
    } else {
      app.ports.geometryFlattenFailed.send(
        parsed.error ?? 'Geometry worker failed',
      )
    }
  } catch {
    app.ports.geometryFlattenFailed.send('Invalid geometry worker response')
  }
})

app.ports.requestGeometryFlatten.subscribe((payload) => {
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
fileInput.accept = '.ldr,.mpd,.dat'
fileInput.style.display = 'none'
document.body.appendChild(fileInput)

const allowedExtensions = new Set(['.ldr', '.mpd', '.dat'])

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
      `Unsupported file type "${file.name}". Please use .ldr, .mpd, or .dat.`,
    )
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
    reportLoadError('Drop a .ldr, .mpd, or .dat file to load a model.')
    return
  }

  readLdrawFile(droppedFile)
})
