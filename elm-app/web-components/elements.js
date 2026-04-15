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
  min-height: var(--bricks-min-height, ${DEFAULT_HEIGHT});
  font-family: Outfit, system-ui, sans-serif;
}

:host([hidden]) {
  display: none;
}

.mount {
  width: 100%;
  height: 100%;
  min-height: var(--bricks-min-height, ${DEFAULT_HEIGHT});
}

input[type="range"] {
  accent-color: var(--color-brand-yellow);
}
`
  return style
}

class BaseBricksElement extends HTMLElement {
  static get observedAttributes() {
    return [
      'src',
      'controls',
      'ldraw-base',
      'ldraw-fallback-base',
      'max-rpm',
      'motor-index',
      'rpm',
      'worker-mode',
      'worker-url',
      'camera-azimuth',
      'camera-elevation',
      'camera-distance',
      'camera-target-x',
      'camera-target-y',
      'camera-target-z',
      'ambient-strength',
      'light-strength',
      'vibrance',
      'edge-width',
    ]
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
    const initialHash = this.cameraHashFromAttributes()

    this.runtime = initBricksRuntime({
      node: this.mount,
      mode: this.mode,
      defaultModel: this.getAttribute('src') ?? undefined,
      controlsEnabled: this.hasAttribute('controls'),
      ldrawBase: this.getAttribute('ldraw-base') ?? undefined,
      ldrawFallbackBase: this.getAttribute('ldraw-fallback-base') ?? undefined,
      maxRpm: this.getAttribute('max-rpm') ?? undefined,
      initialMotorIndex: this.getAttribute('motor-index') ?? undefined,
      initialRpm: this.getAttribute('rpm') ?? undefined,
      workerMode: this.getAttribute('worker-mode') ?? undefined,
      workerUrl: this.getAttribute('worker-url') ?? undefined,
      ambientStrength: this.getAttribute('ambient-strength') ?? undefined,
      lightStrength: this.getAttribute('light-strength') ?? undefined,
      vibrance: this.getAttribute('vibrance') ?? undefined,
      edgeWidth: this.getAttribute('edge-width') ?? undefined,
      initialHash,
      syncUrlHash: false,
      useWindowResize: false,
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
      return
    }

    if (
      name === 'worker-mode'
      || name === 'worker-url'
      || name === 'controls'
      || name === 'motor-index'
      || name === 'rpm'
      || name === 'camera-azimuth'
      || name === 'camera-elevation'
      || name === 'camera-distance'
      || name === 'camera-target-x'
      || name === 'camera-target-y'
      || name === 'camera-target-z'
      || name === 'ambient-strength'
      || name === 'light-strength'
      || name === 'vibrance'
      || name === 'edge-width'
    ) {
      this.runtimeReady = false
      this.runtime?.destroy()
      this.runtime = null
      this.connectedCallback()
    }
  }

  cameraHashFromAttributes() {
    const maybeAzimuthDeg = this.parseOptionalNumericAttribute('camera-azimuth')
    const maybeElevationDeg = this.parseOptionalNumericAttribute('camera-elevation')
    const maybeDistance = this.parseOptionalNumericAttribute('camera-distance')
    const maybeTargetX = this.parseOptionalNumericAttribute('camera-target-x')
    const maybeTargetY = this.parseOptionalNumericAttribute('camera-target-y')
    const maybeTargetZ = this.parseOptionalNumericAttribute('camera-target-z')

    const params = []
    if (Number.isFinite(maybeAzimuthDeg)) {
      params.push(`az=${String((maybeAzimuthDeg * Math.PI) / 180)}`)
    }
    if (Number.isFinite(maybeElevationDeg)) {
      params.push(`el=${String((maybeElevationDeg * Math.PI) / 180)}`)
    }
    if (Number.isFinite(maybeDistance)) {
      params.push(`d=${String(maybeDistance)}`)
    }
    if (Number.isFinite(maybeTargetX)) {
      params.push(`tx=${String(maybeTargetX)}`)
    }
    if (Number.isFinite(maybeTargetY)) {
      params.push(`ty=${String(maybeTargetY)}`)
    }
    if (Number.isFinite(maybeTargetZ)) {
      params.push(`tz=${String(maybeTargetZ)}`)
    }

    return params.join('&')
  }

  parseOptionalNumericAttribute(name) {
    const raw = this.getAttribute(name)
    if (raw == null || String(raw).trim() === '') {
      return null
    }
    const parsed = Number(raw)
    return Number.isFinite(parsed) ? parsed : null
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

export function registerBricksElements() {
  if (!customElements.get('bricks-viewer')) {
    customElements.define('bricks-viewer', BricksViewerElement)
  }
}
