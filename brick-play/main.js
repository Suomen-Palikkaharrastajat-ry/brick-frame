import './main.css'
import { initBricksRuntime } from '../packages/brick-frame-viewport/runtime/initBricksRuntime.js'
import { initSnapshots } from '../packages/brick-frame-viewport/runtime/snapshot.js'
import { initThumbnails } from '../packages/brick-frame-viewport/runtime/thumbnail-pipeline.js'
import { Elm } from './src/Main.elm'

const siteRoot = new URL(import.meta.env.VITE_SITE_ROOT ?? '../', window.location.href).href
const ldrawBase = import.meta.env.VITE_LDRAW_BASE ?? `${siteRoot}ldraw`

const runtime = initBricksRuntime({
  elm: Elm.Main,
  node: document.getElementById('app'),
  mode: 'simulator',
  ldrawBase,
  ldrawFallbackBase: import.meta.env.VITE_LDRAW_FALLBACK_BASE ?? '',
  defaultModel: '',
  loadModelInJs: false,
  extraFlags: {},
  initialHash: window.location.hash ?? '',
  syncUrlHash: true,
  useWindowResize: true,
  dragDropTarget: window,
  suppressGestureTarget: window,
})

if (runtime.app?.ports?.copyToClipboard) {
  runtime.app.ports.copyToClipboard.subscribe((text) => {
    const toCopy = text.startsWith('#')
      ? new URL(text, window.location.href).href
      : text
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(toCopy).catch(() => {})
    }
  })
}

initSnapshots(runtime.app?.ports)

// Rendered part images for the dock (the kit, or the Technic catalog), with the parts' .dat titles as names.
initThumbnails(runtime.app?.ports, ldrawBase, { titles: true })
