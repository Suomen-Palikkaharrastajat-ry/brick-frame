import { initBricksRuntime } from '../runtime/initBricksRuntime.js'

const DEFAULT_HEIGHT = '420px'

function createShadowStyles() {
  const style = document.createElement('style')
  style.textContent = `
:host {
  --color-brand: #05131D;
  --color-brand-yellow: #FAC80A;
  --color-brand-red: #C91A09;
  --color-text-primary: #05131D;
  --color-text-on-dark: #FFFFFF;
  --color-text-muted: #6B7280;
  --color-bg-page: #FFFFFF;
  --color-bg-subtle: #F9FAFB;
  --color-border-default: #E5E7EB;
  display: block;
  position: relative;
  width: 100%;
  min-height: ${DEFAULT_HEIGHT};
  font-family: Outfit, system-ui, sans-serif;
}

:host([hidden]) {
  display: none;
}

.mount {
  width: 100%;
  height: 100%;
  min-height: ${DEFAULT_HEIGHT};
}

input[type="range"] {
  accent-color: var(--color-brand-yellow);
}
`
  return style
}

class BaseBricksElement extends HTMLElement {
  static get observedAttributes() {
    return ['src', 'ldraw-base', 'ldraw-fallback-base', 'max-rpm']
  }

  constructor(mode) {
    super()
    this.mode = mode
    this.runtime = null
    this.runtimeReady = false
    this.root = this.attachShadow({ mode: 'open' })
    this.mount = document.createElement('div')
    this.mount.className = 'mount'
    this.root.append(createShadowStyles(), this.mount)
  }

  connectedCallback() {
    this.ensureSizing()

    this.runtime = initBricksRuntime({
      node: this.mount,
      mode: this.mode,
      defaultModel: this.getAttribute('src') ?? '',
      ldrawBase: this.getAttribute('ldraw-base') ?? undefined,
      ldrawFallbackBase: this.getAttribute('ldraw-fallback-base') ?? undefined,
      maxRpm: this.getAttribute('max-rpm') ?? undefined,
      initialHash: '',
      syncUrlHash: false,
      dragDropTarget: this,
      suppressGestureTarget: this,
      runtimeEventHandler: (eventPayload) => this.forwardRuntimeEvent(eventPayload),
      documentRef: this.ownerDocument,
    })
    this.runtimeReady = true
  }

  disconnectedCallback() {
    this.runtimeReady = false
    this.runtime?.destroy()
    this.runtime = null
  }

  attributeChangedCallback(name, oldValue, newValue) {
    if (!this.runtimeReady || oldValue === newValue) {
      return
    }

    if (name === 'src') {
      const src = String(newValue ?? '').trim()
      if (src) {
        this.runtime.loadFromUrl(src)
      }
    }
  }

  ensureSizing() {
    if (!this.style.minHeight) {
      this.style.minHeight = DEFAULT_HEIGHT
    }
  }

  forwardRuntimeEvent(eventPayload) {
    const type = String(eventPayload?.type ?? 'runtime-event')
    this.dispatchEvent(
      new CustomEvent(type, {
        detail: eventPayload,
        bubbles: true,
        composed: true,
      }),
    )
  }

  async loadFromText(text, filename = 'inline.ldr') {
    this.runtime?.loadFromText(String(text ?? ''))
    return { ok: true, filename }
  }

  async loadFromFile(file) {
    if (!this.runtime || !file) {
      return { ok: false }
    }
    await this.runtime.loadFromFile(file)
    return { ok: true }
  }

  async loadFromUrl(url) {
    if (!this.runtime) {
      return { ok: false }
    }
    const ok = await this.runtime.loadFromUrl(url)
    return { ok }
  }
}

export class BricksViewerElement extends BaseBricksElement {
  constructor() {
    super('viewer')
  }
}

export class BricksSimulatorElement extends BaseBricksElement {
  constructor() {
    super('simulator')
  }
}

export function registerBricksElements() {
  if (!customElements.get('bricks-viewer')) {
    customElements.define('bricks-viewer', BricksViewerElement)
  }

  if (!customElements.get('bricks-simulator')) {
    customElements.define('bricks-simulator', BricksSimulatorElement)
  }
}
