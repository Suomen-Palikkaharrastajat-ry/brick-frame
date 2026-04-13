import { defineConfig, loadEnv } from 'vite'
import elm from 'vite-plugin-elm'

export default defineConfig(({ mode }) => {
  const env = loadEnv(mode, process.cwd(), '')
  const configuredBase = String(env.VITE_PUBLIC_BASE ?? './').trim()
  const base = configuredBase.endsWith('/') ? configuredBase : `${configuredBase}/`

  return {
    plugins: [elm({ debug: false })],
    base,
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
