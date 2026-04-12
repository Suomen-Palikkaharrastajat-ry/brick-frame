import { unzipSync } from 'fflate'
import { Elm } from '../src/Main.elm'
import GeometryWorker from '../geometry-worker.js?worker'

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
const allowedExtensions = new Set(['.ldr', '.mpd', '.dat', '.io'])
const GEOMETRY_FLATTEN_CACHE_LIMIT = 8

function normalizeMode(rawMode) {
  return String(rawMode ?? '').toLowerCase() === 'viewer' ? 'viewer' : 'simulator'
}

function hasAllowedExtension(filename) {
  const lower = String(filename ?? '').toLowerCase()
  return [...allowedExtensions].some((ext) => lower.endsWith(ext))
}

function inferFileExtension(nameOrUrl) {
  const normalized = String(nameOrUrl ?? '').toLowerCase().split('?')[0].split('#')[0]
  return [...allowedExtensions].find((ext) => normalized.endsWith(ext)) ?? null
}

function decodeLdrawBytes(bytes) {
  const decoded = new TextDecoder('utf-8').decode(bytes)
  return decoded.charCodeAt(0) === 0xfeff ? decoded.slice(1) : decoded
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

  const preferred = ['model2.ldr', 'modelv2.ldr', 'model.ldr']

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

function createFileReader({ reportLoadError, pushFileText }) {
  function readStudioIoFile(file) {
    const reader = new FileReader()
    reader.onload = (event) => {
      try {
        const arrayBuffer = event.target?.result
        if (!(arrayBuffer instanceof ArrayBuffer)) {
          throw new Error('Invalid .io file payload')
        }
        const text = extractLdrawFromStudioArchive(arrayBuffer)
        pushFileText(text)
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

  function readLdrawFile(file) {
    if (!file) return

    if (!hasAllowedExtension(file.name)) {
      reportLoadError(
        `Unsupported file type "${file.name}". Please use .ldr, .mpd, .dat, or .io.`,
      )
      return
    }

    if (String(file.name).toLowerCase().endsWith('.io')) {
      readStudioIoFile(file)
      return
    }

    const reader = new FileReader()
    reader.onload = (event) => {
      pushFileText(String(event.target?.result ?? ''))
    }
    reader.onerror = () => {
      reportLoadError(`Failed to read "${file.name}".`)
    }
    reader.readAsText(file)
  }

  async function loadFromUrl(url) {
    const target = String(url ?? '').trim()
    if (!target) {
      reportLoadError('Load URL cannot be empty.')
      return false
    }

    try {
      const extension = inferFileExtension(target)
      const response = await fetch(target)
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}`)
      }

      if (extension === '.io') {
        const buffer = await response.arrayBuffer()
        pushFileText(extractLdrawFromStudioArchive(buffer))
      } else {
        pushFileText(await response.text())
      }
      return true
    } catch (error) {
      const details = error instanceof Error ? error.message : String(error)
      reportLoadError(`Failed to load "${target}". ${details}`)
      return false
    }
  }

  return { readLdrawFile, loadFromUrl }
}

export function initBricksRuntime(options) {
  const {
    node,
    mode = 'simulator',
    ldrawBase = import.meta.env.VITE_LDRAW_BASE ?? defaultLdrawBase,
    ldrawFallbackBase = import.meta.env.VITE_LDRAW_FALLBACK_BASE ?? defaultFallbackBase,
    maxRpm = import.meta.env.VITE_MAX_RPM ?? defaultMaxRpm,
    defaultModel = defaultModelUrl,
    initialHash = typeof window !== 'undefined' ? window.location.hash ?? '' : '',
    syncUrlHash = true,
    runtimeEventHandler = null,
    dragDropTarget = typeof window !== 'undefined' ? window : null,
    suppressGestureTarget = typeof window !== 'undefined' ? window : null,
    documentRef = typeof document !== 'undefined' ? document : null,
  } = options ?? {}

  if (!node) {
    throw new Error('initBricksRuntime requires a mount node')
  }

  const maxRpmRaw = Number(maxRpm)
  const sanitizedMaxRpm = Number.isFinite(maxRpmRaw) && maxRpmRaw >= 1 ? maxRpmRaw : defaultMaxRpm

  const app = Elm.Main.init({
    node,
    flags: {
      ldrawBase,
      ldrawFallbackBase,
      defaultModelUrl: String(defaultModel ?? ''),
      initialHash,
      maxRpm: sanitizedMaxRpm,
      uiMode: normalizeMode(mode),
    },
  })

  const cleanups = []
  const geometryWorker = new GeometryWorker()
  const geometryFlattenCache = new Map()
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

  function addEventListener(target, eventName, handler, eventOptions) {
    if (!target?.addEventListener) {
      return
    }
    target.addEventListener(eventName, handler, eventOptions)
    cleanups.push(() => {
      target.removeEventListener(eventName, handler, eventOptions)
    })
  }

  function sendRuntimeEvent(eventType, detail = {}) {
    if (typeof runtimeEventHandler === 'function') {
      runtimeEventHandler({ type: eventType, ...detail })
    }
  }

  function pushFileText(text) {
    app.ports.fileContentReceived.send(String(text ?? ''))
  }

  function reportLoadError(message) {
    const normalized = String(message ?? 'Unknown load error')
    app.ports.fileLoadError.send(normalized)
    sendRuntimeEvent('model-error', { message: normalized })
  }

  const { readLdrawFile, loadFromUrl } = createFileReader({ reportLoadError, pushFileText })

  function loadFromText(text) {
    pushFileText(String(text ?? ''))
  }

  async function loadFromFile(file) {
    readLdrawFile(file)
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
        app.ports.geometryFlattenFailed.send(parsed.error ?? 'Geometry worker failed')
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

  if (syncUrlHash && typeof window !== 'undefined') {
    app.ports.setUrlHash.subscribe((hash) => {
      const normalized = String(hash ?? '')
      const withPrefix = normalized.startsWith('#') ? normalized : `#${normalized}`
      if (window.location.hash !== withPrefix) {
        window.history.replaceState(null, '', withPrefix)
      }
    })
  }

  if (app.ports.runtimeEvent) {
    app.ports.runtimeEvent.subscribe((payload) => {
      try {
        const parsed = typeof payload === 'string' ? JSON.parse(payload) : payload
        if (parsed && typeof parsed === 'object') {
          sendRuntimeEvent(String(parsed.type ?? 'runtime-event'), parsed)
        }
      } catch {
        sendRuntimeEvent('runtime-event', { payload })
      }
    })
  }

  const ownerDocument = documentRef ?? node.ownerDocument
  if (ownerDocument) {
    const fileInput = ownerDocument.createElement('input')
    fileInput.type = 'file'
    fileInput.accept = '.ldr,.mpd,.dat,.io'
    fileInput.style.display = 'none'
    ownerDocument.body?.appendChild(fileInput)

    addEventListener(fileInput, 'change', () => {
      const file = fileInput.files?.[0]
      readLdrawFile(file)
    })

    cleanups.push(() => {
      fileInput.remove()
    })

    app.ports.requestFileUpload.subscribe(() => {
      fileInput.value = ''
      fileInput.click()
    })
  }

  function suppressDefaultDragBehavior(event) {
    event.preventDefault()
    event.stopPropagation()
  }

  if (dragDropTarget) {
    addEventListener(dragDropTarget, 'dragenter', suppressDefaultDragBehavior)
    addEventListener(dragDropTarget, 'dragover', suppressDefaultDragBehavior)
    addEventListener(dragDropTarget, 'drop', (event) => {
      suppressDefaultDragBehavior(event)
      const droppedFile = event.dataTransfer?.files?.[0]
      if (!droppedFile) {
        reportLoadError('Drop a .ldr, .mpd, .dat, or .io file to load a model.')
        return
      }
      readLdrawFile(droppedFile)
    })
  }

  function suppressGestureZoom(event) {
    event.preventDefault()
  }

  if (suppressGestureTarget) {
    addEventListener(suppressGestureTarget, 'gesturestart', suppressGestureZoom, { passive: false })
    addEventListener(suppressGestureTarget, 'gesturechange', suppressGestureZoom, { passive: false })
    addEventListener(suppressGestureTarget, 'gestureend', suppressGestureZoom, { passive: false })
  }

  function destroy() {
    geometryWorker.terminate()
    while (cleanups.length > 0) {
      const cleanup = cleanups.pop()
      cleanup()
    }
  }

  return {
    app,
    destroy,
    loadFromText,
    loadFromFile,
    loadFromUrl,
  }
}

export function runtimeDefaults() {
  return {
    defaultLdrawBase,
    defaultFallbackBase,
    defaultModelUrl,
    defaultMaxRpm,
  }
}
