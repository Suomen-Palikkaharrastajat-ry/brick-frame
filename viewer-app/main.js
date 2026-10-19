import './main.css'
import { initBricksRuntime, runtimeDefaults } from './runtime/initBricksRuntime.js'

const defaults = runtimeDefaults()

initBricksRuntime({
  node: document.getElementById('app'),
  mode: 'simulator',
  ldrawBase: import.meta.env.VITE_LDRAW_BASE ?? defaults.defaultLdrawBase,
  ldrawFallbackBase: import.meta.env.VITE_LDRAW_FALLBACK_BASE ?? defaults.defaultFallbackBase,
  maxRpm: import.meta.env.VITE_MAX_RPM ?? defaults.defaultMaxRpm,
  defaultModel: defaults.defaultModelUrl,
  initialHash: window.location.hash ?? '',
  syncUrlHash: true,
  useWindowResize: true,
  dragDropTarget: window,
  suppressGestureTarget: window,
})
