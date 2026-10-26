import './main.css'
import { initBricksRuntime, runtimeDefaults } from '../packages/brick-frame-viewport/runtime/initBricksRuntime.js'
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

if (runtime.app?.ports?.captureRender) {
  runtime.app.ports.captureRender.subscribe(({ width, height, transparent, filename }) => {
    const canvas = document.querySelector('canvas')
    if (!canvas) return

    const targetWidth = width || canvas.width
    const targetHeight = height || canvas.height

    const offscreen = document.createElement('canvas')
    offscreen.width = targetWidth
    offscreen.height = targetHeight
    const ctx = offscreen.getContext('2d')

    if (!transparent) {
      const bg =
        getComputedStyle(document.documentElement)
          .getPropertyValue('--color-surface-canvas')
          .trim() || 'rgb(15, 23, 42)'
      ctx.fillStyle = bg
      ctx.fillRect(0, 0, targetWidth, targetHeight)
    }

    ctx.imageSmoothingEnabled = true
    ctx.imageSmoothingQuality = 'high'
    ctx.drawImage(canvas, 0, 0, targetWidth, targetHeight)

    const resolvedFilename = filename || 'palikkakehys-render.png'

    // Multi-pass raytracing simulation delay (~1.6s) to allow progress UI animation
    setTimeout(() => {
      const dataUrl = offscreen.toDataURL('image/png')
      runtime.app.ports?.snapshotReady?.send({
        dataUrl,
        filename: resolvedFilename,
      })
    }, 1600)
  })
}

if (runtime.app?.ports?.saveSnapshot) {
  runtime.app.ports.saveSnapshot.subscribe(({ dataUrl, filename }) => {
    const link = document.createElement('a')
    link.href = dataUrl
    link.download = filename || 'palikkakehys-render.png'
    document.body.appendChild(link)
    link.click()
    link.remove()
  })
}
