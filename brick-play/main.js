import './main.css'
import { initBricksRuntime } from '../packages/brick-frame-viewport/runtime/initBricksRuntime.js'
import { Elm } from './src/Main.elm'

const siteRoot = new URL(import.meta.env.VITE_SITE_ROOT ?? '../', window.location.href).href
const ldrawBase = import.meta.env.VITE_LDRAW_BASE ?? `${siteRoot}ldraw`

initBricksRuntime({
  elm: Elm.Main,
  node: document.getElementById('app'),
  mode: 'simulator',
  ldrawBase,
  ldrawFallbackBase: import.meta.env.VITE_LDRAW_FALLBACK_BASE ?? '',
  defaultModel: '',
  loadModelInJs: false,
  extraFlags: {},
  initialHash: window.location.hash ?? '',
  syncUrlHash: false,
  useWindowResize: true,
  dragDropTarget: window,
  suppressGestureTarget: window,
})
