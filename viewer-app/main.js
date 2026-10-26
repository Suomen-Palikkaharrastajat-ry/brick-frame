import './main.css'
import { initBricksRuntime, runtimeDefaults } from '../packages/brick-frame-viewport/runtime/initBricksRuntime.js'
import { initSnapshots } from '../packages/brick-frame-viewport/runtime/snapshot.js'
import { Elm } from './src/Main.elm'

const defaults = runtimeDefaults()
const hashParams = new URLSearchParams(window.location.hash.replace(/^#/, ''))
const requestedModel = hashParams.get('src')

const runtime = initBricksRuntime({
  elm: Elm.Main,
  studioUrl: import.meta.env.VITE_STUDIO_URL ?? 'studio/',
  node: document.getElementById('app'),
  mode: 'simulator',
  ldrawBase: import.meta.env.VITE_LDRAW_BASE ?? defaults.defaultLdrawBase,
  ldrawFallbackBase: import.meta.env.VITE_LDRAW_FALLBACK_BASE ?? defaults.defaultFallbackBase,
  maxRpm: import.meta.env.VITE_MAX_RPM ?? defaults.defaultMaxRpm,
  defaultModel: requestedModel ?? defaults.defaultModelUrl,
  initialHash: window.location.hash ?? '',
  syncUrlHash: true,
  useWindowResize: true,
  dragDropTarget: window,
  suppressGestureTarget: window,
})

initSnapshots(runtime.app?.ports)
