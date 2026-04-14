#!/usr/bin/env bash
set -euo pipefail

LDRAW_DIR="${LDRAW_SYNC_DIR:-elm-app/public/ldraw}"
PARTS_DIR="${LDRAW_DIR}/parts"
PRIMS_DIR="${LDRAW_DIR}/p"
REPORT_PATH="${LDRAW_MISSING_REPORT:-build/ldraw-missing-report.txt}"

if [[ ! -d "${PARTS_DIR}" || ! -d "${PRIMS_DIR}" ]]; then
  echo "check-missing-ldraw-parts: expected '${PARTS_DIR}' and '${PRIMS_DIR}'"
  exit 1
fi

mkdir -p "$(dirname "${REPORT_PATH}")"

# Keep model scanning scoped to project-owned model sources and exclude synced
# library + generated build/vendor trees.
mapfile -d '' model_files < <(
  find . \
    \( -path './.git' -o -path './node_modules' -o -path './build' -o -path './dist-newstyle' -o -path './vendor' -o -path './elm-app/elm-stuff' -o -path './packages/brick-frame-simulator/elm-stuff' -o -path './elm-app/public/ldraw' \) -prune \
    -o -type f \( -name '*.ldr' -o -name '*.mpd' \) -print0
)

if [[ ${#model_files[@]} -eq 0 ]]; then
  echo "check-missing-ldraw-parts: no model files found"
  : > "${REPORT_PATH}"
  exit 0
fi

missing_tmp="$(mktemp)"
all_refs_tmp="$(mktemp)"

normalize() {
  local value="$1"
  value="${value//\\//}"
  value="$(printf '%s' "${value}" | tr -d '\r' | tr '[:upper:]' '[:lower:]')"
  # shellcheck disable=SC2001
  value="$(printf '%s' "${value}" | sed 's#^\./##; s#^[[:space:]]*##; s#[[:space:]]*$##')"
  printf '%s' "${value}"
}

exists_in_library() {
  local name="$1"

  if [[ "${name}" == parts/* || "${name}" == p/* ]]; then
    [[ -f "${LDRAW_DIR}/${name}" ]]
    return
  fi

  if [[ "${name}" == s/* ]]; then
    [[ -f "${PARTS_DIR}/${name}" ]]
    return
  fi

  if [[ "${name}" == 48/* || "${name}" == 8/* ]]; then
    [[ -f "${PRIMS_DIR}/${name}" ]]
    return
  fi

  if [[ -f "${PARTS_DIR}/${name}" || -f "${PARTS_DIR}/s/${name}" || -f "${PRIMS_DIR}/${name}" ]]; then
    return 0
  fi

  return 1
}

for file in "${model_files[@]}"; do
  # Parse per-file embedded MPD names (0 FILE ...), which satisfy references
  # without requiring library files.
  embedded_tmp="$(mktemp)"
  awk '
    toupper($1) == "0" && toupper($2) == "FILE" {
      out = $3
      for (i = 4; i <= NF; i++) out = out " " $i
      print out
    }
  ' "${file}" > "${embedded_tmp}"

  while IFS= read -r embedded_name || [[ -n "${embedded_name}" ]]; do
    [[ -z "${embedded_name}" ]] && continue
    printf '%s\n' "$(normalize "${embedded_name}")" >> "${embedded_tmp}.norm"
  done < "${embedded_tmp}"

  awk '
    $1 == "1" {
      if (NF >= 15) {
        out = $15
        for (i = 16; i <= NF; i++) out = out " " $i
        print out
      }
    }
    $1 == "11" {
      if (NF >= 18) {
        out = $18
        for (i = 19; i <= NF; i++) out = out " " $i
        print out
      }
    }
  ' "${file}" | while IFS= read -r raw_ref || [[ -n "${raw_ref}" ]]; do
    [[ -z "${raw_ref}" ]] && continue
    ref="$(normalize "${raw_ref}")"
    printf '%s|%s\n' "${file#./}" "${ref}" >> "${all_refs_tmp}"

    if [[ -f "${embedded_tmp}.norm" ]] && rg -Fxq "${ref}" "${embedded_tmp}.norm"; then
      continue
    fi

    if ! exists_in_library "${ref}"; then
      printf '%s|%s\n' "${file#./}" "${ref}" >> "${missing_tmp}"
    fi
  done

  rm -f "${embedded_tmp}" "${embedded_tmp}.norm"
done

sort -u "${all_refs_tmp}" > "${all_refs_tmp}.sorted"

if [[ ! -s "${missing_tmp}" ]]; then
  {
    echo "status: ok"
    echo "checked_model_files: ${#model_files[@]}"
    echo "total_references: $(wc -l < "${all_refs_tmp}.sorted" | tr -d ' ')"
    echo "missing_references: 0"
  } > "${REPORT_PATH}"

  echo "check-missing-ldraw-parts: no unresolved references"
  echo "check-missing-ldraw-parts: report ${REPORT_PATH}"
  rm -f "${missing_tmp}" "${all_refs_tmp}" "${all_refs_tmp}.sorted"
  exit 0
fi

sort -u "${missing_tmp}" > "${missing_tmp}.sorted"

{
  echo "status: failed"
  echo "checked_model_files: ${#model_files[@]}"
  echo "total_references: $(wc -l < "${all_refs_tmp}.sorted" | tr -d ' ')"
  echo "missing_references: $(wc -l < "${missing_tmp}.sorted" | tr -d ' ')"
  echo
  echo "missing:"
  cat "${missing_tmp}.sorted"
} > "${REPORT_PATH}"

echo "check-missing-ldraw-parts: unresolved references found"
cat "${REPORT_PATH}"

echo "check-missing-ldraw-parts: report ${REPORT_PATH}"
rm -f "${missing_tmp}" "${missing_tmp}.sorted" "${all_refs_tmp}" "${all_refs_tmp}.sorted"
exit 1
