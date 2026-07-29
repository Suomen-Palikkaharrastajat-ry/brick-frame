import { defineConfig, loadEnv } from 'vite'
import tailwindcss from '@tailwindcss/vite'
import elmTailwind from 'elm-tailwind-classes/vite'
import elm from 'vite-plugin-elm'

export default defineConfig(({ mode }) => {
  const env = loadEnv(mode, process.cwd(), '')
  const configuredBase = String(env.VITE_PUBLIC_BASE ?? './').trim()
  const base = configuredBase.endsWith('/') ? configuredBase : `${configuredBase}/`

  return {
    // node_modules is a read-only Nix-store symlink; keep Vite's dep cache off it.
    cacheDir: '.vite',
    publicDir: 'public',
    plugins: [
      elmTailwind(),
      elm({ debug: false }),
      tailwindcss(),
    ],
    build: {
      outDir: '../build',
      emptyOutDir: true,
    },
    base,
  }
})
