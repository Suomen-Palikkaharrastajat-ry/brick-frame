#!/usr/bin/env bash
set -euo pipefail

LDRAW_SYNC_URL="${LDRAW_SYNC_URL:-https://library.ldraw.org/library/updates/complete.zip}"
LDRAW_SYNC_DIR="${LDRAW_SYNC_DIR:-elm-app/public/ldraw}"
LDRAW_SYNC_LOCK="${LDRAW_SYNC_LOCK:-ldraw-sync.lock.json}"
LDRAW_NOTICE_FILE="${LDRAW_NOTICE_FILE:-THIRD_PARTY_NOTICES}"

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

archive_path="$tmp_dir/complete.zip"
headers_path="$tmp_dir/headers.txt"
extract_dir="$tmp_dir/extracted"

echo "sync-ldraw: downloading ${LDRAW_SYNC_URL}"
curl -fLsS --retry 3 --retry-delay 2 -D "$headers_path" -o "$archive_path" "$LDRAW_SYNC_URL"

archive_sha256="$(sha256sum "$archive_path" | awk '{ print $1 }')"
last_modified="$(
  awk 'tolower($1)=="last-modified:"{ $1=""; sub(/^ /, "", $0); gsub(/\r/, "", $0); print; exit }' "$headers_path"
)"

mkdir -p "$extract_dir"
unzip -q "$archive_path" -d "$extract_dir"

if [[ -d "$extract_dir/ldraw" ]]; then
  ldraw_source="$extract_dir/ldraw"
elif [[ -d "$extract_dir/complete/ldraw" ]]; then
  ldraw_source="$extract_dir/complete/ldraw"
else
  candidate="$(find "$extract_dir" -type d -name ldraw | head -n 1 || true)"
  if [[ -z "$candidate" ]]; then
    echo "sync-ldraw: failed to locate ldraw directory in archive"
    exit 1
  fi
  ldraw_source="$candidate"
fi

echo "sync-ldraw: syncing into ${LDRAW_SYNC_DIR}"
rm -rf "$LDRAW_SYNC_DIR"
mkdir -p "$LDRAW_SYNC_DIR"
cp -a "$ldraw_source"/. "$LDRAW_SYNC_DIR"/

if [[ ! -f "$LDRAW_SYNC_DIR/CAreadme.txt" ]]; then
  echo "sync-ldraw: CAreadme.txt missing after extraction"
  exit 1
fi

license_lines="$tmp_dir/license-lines.txt"
find "$LDRAW_SYNC_DIR" -type f \( -name '*.dat' -o -name '*.ldr' -o -name '*.mpd' \) \
  -print0 \
  | xargs -0 grep -h '^0 !LICENSE ' > "$license_lines" || true

count_not_redistributable="$(grep -c 'Not redistributable' "$license_lines" || true)"
count_cc_by_4="$(grep -c 'Licensed under CC BY 4.0' "$license_lines" || true)"
count_cc_by_2_and_4="$(grep -c 'Licensed under CC BY 2.0 and CC BY 4.0' "$license_lines" || true)"
count_cc0="$(grep -c 'Marked with CC0 1.0' "$license_lines" || true)"
count_legacy_ccal="$(grep -c 'Redistributable under CCAL version 2.0' "$license_lines" || true)"

if [[ "$count_not_redistributable" -gt 0 ]]; then
  echo "sync-ldraw: found ${count_not_redistributable} non-redistributable files in synced library"
  exit 1
fi

total_files="$(find "$LDRAW_SYNC_DIR" -type f | wc -l | tr -d ' ')"
synced_at_utc="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"

cat > "$LDRAW_SYNC_LOCK" <<EOF
{
  "source_url": "${LDRAW_SYNC_URL}",
  "synced_at_utc": "${synced_at_utc}",
  "archive_sha256": "${archive_sha256}",
  "archive_last_modified": "${last_modified}",
  "destination": "${LDRAW_SYNC_DIR}",
  "total_files": ${total_files},
  "license_summary": {
    "cc_by_4_0": ${count_cc_by_4},
    "cc_by_2_0_and_4_0": ${count_cc_by_2_and_4},
    "cc0_1_0": ${count_cc0},
    "legacy_ccal_2_0": ${count_legacy_ccal},
    "not_redistributable": ${count_not_redistributable}
  }
}
EOF

cat > "$LDRAW_NOTICE_FILE" <<'EOF'
THIRD-PARTY NOTICES
===================

LDraw Official Parts Library
----------------------------
Source: https://library.ldraw.org/

This project uses files from the LDraw parts library. Licensing is declared per
file via `0 !LICENSE ...` headers and accompanying readme/license files included
in the synced library (for example `CAreadme.txt`).

The sync pipeline enforces a defensive policy and fails if any file declares:
`0 !LICENSE Not redistributable : see NonCAreadme.txt`.

This notice is informational only and is not legal advice.
EOF

echo "sync-ldraw: wrote ${LDRAW_SYNC_LOCK}"
echo "sync-ldraw: wrote ${LDRAW_NOTICE_FILE}"
echo "sync-ldraw: done (files=${total_files}, cc-by-4.0=${count_cc_by_4}, cc0=${count_cc0}, legacy-ccal=${count_legacy_ccal})"
