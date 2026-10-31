# Brick Frame (Palikkakehys)

Brick Frame is a browser-based 3D LEGO® model viewer and Web Components library built with Elm + WebGL. It renders LDraw `.ldr` and `.mpd` files, animates gear train physics, and provides embeddable custom elements for displaying 3D models on websites.

## Ecosystem Architecture

The Brick Frame ecosystem consists of:
- **`brick-studio`**: The upstream CAD Studio editor and core Elm package suite (`brick-frame-data`, `brick-frame-simulator`, `brick-frame-viewport`, `brick-frame-simulation`, `brick-frame-ui`, `brick-frame-studio`).
- **`brick-play`**: The interactive touchscreen-friendly building sandbox and Technic simulator.
- **`brick-catalog`**: The zero-404 CDN parts catalog and pre-bundled MPD closure service (`catalog.palikkaharrastajat.fi`).
- **`brick-frame` (this repository)**: The focused 3D model viewer application and custom HTML Web Components bundle.

```
brick-frame/
├── viewer-app/                         # WebGL 3D model viewer application (Vite + Elm)
│   ├── src/                            # Viewer Elm source code
│   ├── web-components/                 # Web Component wrappers (<brick-viewer>)
│   └── public/                         # Public assets and LDraw model files
├── vendor/
│   ├── brick-studio/                   # Submodule providing core Elm packages
│   └── master-builder/                 # Submodule providing UI tokens & design components
├── scripts/                            # LDraw sync and missing-parts scripts
├── docs/                               # Architecture and specification docs
└── devenv.nix                          # Streamlined pure Elm/Node development environment
```

## Quick Start

Using [devenv](https://devenv.sh):

```bash
# Enter devenv shell
devenv shell

# Initialize git submodules and symlinks
make vendor

# Run unit tests
make test

# Start the viewer dev server (http://localhost:5173)
make dev

# Build production viewer and web-components bundles → build/
make build
```

## Web Components Usage

The `<brick-viewer>` custom element allows embedding interactive 3D LDraw models into any HTML page:

```html
<script type="module" src="https://cdn.palikkaharrastajat.fi/brick-viewer.js"></script>

<brick-viewer
  src="https://catalog.palikkaharrastajat.fi/bundles/3001.mpd"
  camera="45,30,150"
  auto-rotate>
</brick-viewer>
```

## Licensing and Trademarks

- Software is open source under the MIT License (see [THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md)).
- LEGO® is a registered trademark of the LEGO Group of companies, which does not sponsor, authorize, or endorse this project.
- LDraw™ is a trademark owned and licensed by the Estate of James Jessiman.
- LDraw Parts Library content is redistributable under Creative Commons Attribution (CC BY 2.0 / CC BY 4.0).
