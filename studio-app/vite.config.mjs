import fs from 'node:fs'
import path from 'node:path'
import { defineConfig, loadEnv } from 'vite'
import tailwindcss from '@tailwindcss/vite'
import elmTailwind from 'elm-tailwind-classes/vite'
import elm from 'vite-plugin-elm'

// In dev the studio runs on its own port, so the viewer's static assets (the
// synced LDraw library, fonts, example models) are served from its public dir.
// In production they sit at the site root next to `studio/`.
const viewerPublic = path.resolve(import.meta.dirname, '../viewer-app/public')
const mimeTypes = {
  '.ldr': 'text/plain',
  '.mpd': 'text/plain',
  '.dat': 'text/plain',
  '.json': 'application/json',
  '.ttf': 'font/ttf',
  '.svg': 'image/svg+xml',
  '.png': 'image/png',
  '.ico': 'image/x-icon',
}

function serveViewerAssets() {
  return {
    name: 'serve-viewer-assets',
    configureServer(server) {
      server.middlewares.use((req, res, next) => {
        const url = new URL(req.url ?? '/', 'http://localhost')
        const file = path.join(viewerPublic, decodeURIComponent(url.pathname))
        if (!file.startsWith(viewerPublic) || !fs.existsSync(file) || !fs.statSync(file).isFile()) {
          next()
          return
        }
        res.setHeader('Content-Type', mimeTypes[path.extname(file)] ?? 'application/octet-stream')
        fs.createReadStream(file).pipe(res)
      })
    },
  }
}

export default defineConfig(({ mode }) => {
  const env = loadEnv(mode, process.cwd(), '')
  const configuredBase = String(env.VITE_PUBLIC_BASE ?? './').trim()
  const siteBase = configuredBase.endsWith('/') ? configuredBase : `${configuredBase}/`
  // Relative bases (the default) already resolve against the page.
  const base = siteBase === './' ? './' : `${siteBase}studio/`

  return {
    // node_modules is a read-only Nix-store symlink; keep Vite's dep cache off it.
    cacheDir: '.vite',
    publicDir: false,
    plugins: [elmTailwind(), elm({ debug: false }), tailwindcss(), serveViewerAssets()],
    build: {
      // The viewer build empties `build/` first; the studio goes in beside it.
      outDir: '../build/studio',
      emptyOutDir: true,
    },
    base,
    server: {
      port: 5174,
      // The runtime and the geometry worker live in packages/brick-frame-viewport.
      fs: { allow: ['..'] },
    },
  }
})
