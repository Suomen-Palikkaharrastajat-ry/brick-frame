import './main.css'
import { initBricksRuntime } from '../packages/brick-frame-viewport/runtime/initBricksRuntime.js'
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

// Rendered part images for the kit dock, with the parts' .dat titles as names.
initThumbnails(runtime.app?.ports, ldrawBase, { titles: true })
