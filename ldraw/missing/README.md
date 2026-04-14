# Missing LDraw Parts Catalog

This folder defines project-maintained replacement parts that are not available
in the synced official LDraw library.

## Catalog

`catalog.tsv` format:

```text
part|strategy|target|description|source_models
```

- `part`: output filename under `elm-app/public/ldraw/parts/`
- `strategy`:
  - `alias`: generate a wrapper part that references another part
  - `inline`: copy static content from `ldraw/missing/inline/<target>`
- `target`:
  - for `alias`: referenced part filename (for example `98138.dat`)
  - for `inline`: filename relative to `ldraw/missing/inline/`
- `description`: first header line in generated part
- `source_models`: optional free-form note (for traceability)

Comments start with `#`.

## CI behavior

`make ldraw-missing` runs:

1. `scripts/generate-missing-ldraw-parts.sh`
2. `scripts/check-missing-ldraw-parts.sh`

The check fails if any `.ldr`/`.mpd` model reference cannot be resolved from:

- embedded MPD sections (`0 FILE ...`),
- synced official LDraw files, or
- generated replacements from this catalog.
