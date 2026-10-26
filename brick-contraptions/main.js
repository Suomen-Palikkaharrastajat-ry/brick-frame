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

    offscreen.toBlob((blob) => {
      if (!blob) return
      const url = URL.createObjectURL(blob)
      const link = document.createElement('a')
      link.href = url
      link.download = filename || 'palikkakehys-render.png'
      document.body.appendChild(link)
      link.click()
      link.remove()
      URL.revokeObjectURL(url)
    }, 'image/png')
  })
}

// Dock part cards: rendered thumbnails (with .dat titles for project kit parts).
initThumbnails(runtime.app?.ports, ldrawBase, { titles: true })
