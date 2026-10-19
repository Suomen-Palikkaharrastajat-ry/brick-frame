import './main.css'
import { initBricksRuntime } from '../packages/brick-frame-viewport/runtime/initBricksRuntime.js'
import { Elm } from './src/Main.elm'

// The studio is served below the viewer (`<site>/studio/`) and shares its LDraw
// library, fonts and example models, which live at the site root.
const siteRoot = new URL(import.meta.env.VITE_SITE_ROOT ?? '../', window.location.href).href

// `#src=<url>` names the model to open; the camera keys in the same hash are
// read by the viewport itself.
const hashParams = new URLSearchParams(window.location.hash.replace(/^#/, ''))
const model = hashParams.get('src') ?? `${siteRoot}examples/wheeler.ldr`

initBricksRuntime({
  elm: Elm.Main,
  node: document.getElementById('app'),
  mode: 'simulator',
  ldrawBase: import.meta.env.VITE_LDRAW_BASE ?? `${siteRoot}ldraw`,
  ldrawFallbackBase: import.meta.env.VITE_LDRAW_FALLBACK_BASE ?? '',
  defaultModel: model,
  loadModelInJs: true,
  initialHash: window.location.hash ?? '',
  syncUrlHash: true,
  useWindowResize: true,
  dragDropTarget: window,
  suppressGestureTarget: window,
})
