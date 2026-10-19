import { defineConfig, loadEnv } from 'vite'
import elm from 'vite-plugin-elm'

export default defineConfig(({ mode }) => {
  const env = loadEnv(mode, process.cwd(), '')
  const configuredBase = String(env.VITE_PUBLIC_BASE ?? './').trim()
  const base = configuredBase.endsWith('/') ? configuredBase : `${configuredBase}/`

  return {
    // node_modules is a read-only Nix-store symlink; keep Vite's dep cache off it.
    cacheDir: '.vite',
    plugins: [elm({ debug: false })],
    base,
    server: {
      // The runtime and the geometry worker live in packages/brick-frame-viewport.
      fs: { allow: ['..'] },
    },
    build: {
      outDir: '../build/docs',
      emptyOutDir: false,
      lib: {
        entry: 'web-components/index.js',
        name: 'BricksViewer',
        formats: ['es', 'iife'],
        fileName: (format) => `bricks-viewer.${format === 'es' ? 'esm' : 'iife'}.js`,
      },
      rollupOptions: {
        output: {
          assetFileNames: '[name][extname]',
        },
      },
    },
  }
})
