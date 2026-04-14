#!/usr/bin/env bash
set -euo pipefail

CATALOG_PATH="${LDRAW_MISSING_CATALOG:-ldraw/missing/catalog.tsv}"
LDRAW_DIR="${LDRAW_SYNC_DIR:-elm-app/public/ldraw}"
PARTS_DIR="${LDRAW_DIR}/parts"
INLINE_DIR="${LDRAW_MISSING_INLINE_DIR:-ldraw/missing/inline}"
MANIFEST_PATH="${LDRAW_MISSING_MANIFEST:-${LDRAW_DIR}/.generated-missing-parts.list}"

if [[ ! -f "${CATALOG_PATH}" ]]; then
  echo "generate-missing-ldraw-parts: catalog missing at ${CATALOG_PATH}"
  exit 1
fi

if [[ ! -d "${PARTS_DIR}" ]]; then
  echo "generate-missing-ldraw-parts: expected parts dir at ${PARTS_DIR}"
  exit 1
fi

mkdir -p "$(dirname "${MANIFEST_PATH}")"

generated_count=0

# Remove previously generated files so deletions in catalog are reflected.
if [[ -f "${MANIFEST_PATH}" ]]; then
  while IFS= read -r rel_path; do
    [[ -z "${rel_path}" ]] && continue
    rm -f "${LDRAW_DIR}/${rel_path}"
  done < "${MANIFEST_PATH}"
fi

new_manifest="$(mktemp)"

while IFS='|' read -r raw_part raw_strategy raw_target raw_description raw_sources || [[ -n "${raw_part:-}" ]]; do
  part="$(printf '%s' "${raw_part:-}" | xargs)"

  if [[ -z "${part}" || "${part}" =~ ^# ]]; then
    continue
  fi

  strategy="$(printf '%s' "${raw_strategy:-}" | xargs | tr '[:upper:]' '[:lower:]')"
  target="$(printf '%s' "${raw_target:-}" | xargs)"
  description="$(printf '%s' "${raw_description:-}" | xargs)"

  if [[ -z "${strategy}" ]]; then
    echo "generate-missing-ldraw-parts: missing strategy for part '${part}'"
    exit 1
  fi

  if [[ "${part}" == */* ]]; then
    echo "generate-missing-ldraw-parts: part must be a filename, got '${part}'"
    exit 1
  fi

  if [[ -f "${PARTS_DIR}/${part}" ]]; then
    # If the part already exists and declares official LDraw metadata, this
    # catalog entry is stale and should be removed.
    if rg -q '^0 !LDRAW_ORG ' "${PARTS_DIR}/${part}"; then
      echo "generate-missing-ldraw-parts: stale catalog entry '${part}' now exists as an official part"
      exit 1
    fi
  fi

  case "${strategy}" in
    alias)
      if [[ -z "${target}" ]]; then
        echo "generate-missing-ldraw-parts: alias target missing for '${part}'"
        exit 1
      fi

      target_path="${PARTS_DIR}/${target}"
      if [[ ! -f "${target_path}" ]]; then
        echo "generate-missing-ldraw-parts: alias target '${target}' for '${part}' was not found at ${target_path}"
        exit 1
      fi

      {
        printf '0 %s\n' "${description:-Generated replacement part}"
        printf '0 Name: %s\n' "${part}"
        printf '0 Author: CI replacement generator\n'
        printf '0 !LDRAW_ORG Unofficial_Part\n'
        printf '0 !LICENSE Redistributable under CCAL version 2.0 : see CAreadme.txt\n'
        printf '0 // generated from %s\n' "${CATALOG_PATH}"
        printf '\n'
        printf '1 16 0 0 0 1 0 0 0 1 0 0 0 1 %s\n' "${target}"
      } > "${PARTS_DIR}/${part}"
      ;;

    inline)
      if [[ -z "${target}" ]]; then
        echo "generate-missing-ldraw-parts: inline target missing for '${part}'"
        exit 1
      fi

      inline_src="${INLINE_DIR}/${target}"
      if [[ ! -f "${inline_src}" ]]; then
        echo "generate-missing-ldraw-parts: inline source '${inline_src}' not found for '${part}'"
        exit 1
      fi

      cp "${inline_src}" "${PARTS_DIR}/${part}"
      ;;

    *)
      echo "generate-missing-ldraw-parts: unsupported strategy '${strategy}' for '${part}'"
      exit 1
      ;;
  esac

  printf 'parts/%s\n' "${part}" >> "${new_manifest}"
  generated_count=$((generated_count + 1))
done < "${CATALOG_PATH}"

mv "${new_manifest}" "${MANIFEST_PATH}"

echo "generate-missing-ldraw-parts: generated ${generated_count} replacement part(s)"
echo "generate-missing-ldraw-parts: manifest ${MANIFEST_PATH}"
