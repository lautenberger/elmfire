#!/usr/bin/env bash
set -euo pipefail

CASE_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_BIN="${PYTHON_BIN:-python3}"
export PYTHONNOUSERSITE=1
ELMFIRE_BIN="${ELMFIRE_BIN:-elmfire}"

mkdir -p "$CASE_DIR/outputs" "$CASE_DIR/figures" "$CASE_DIR/logs/matplotlib"
export MPLCONFIGDIR="$CASE_DIR/logs/matplotlib"
"$PYTHON_BIN" "$CASE_DIR/scripts/preprocess.py"
"$PYTHON_BIN" "$CASE_DIR/scripts/spatial_evidence.py"

while IFS= read -r variant; do
  [[ -n "$variant" ]] || continue
  config="$CASE_DIR/variants/$variant/elmfire.data"
  [[ -f "$config" ]] || { echo "[ERROR] missing variant namelist: $config" >&2; exit 2; }
  echo "[INFO] ${CASE_DIR##*/}: $variant"
  (cd "$CASE_DIR" && "$ELMFIRE_BIN" "${config#$CASE_DIR/}") \
    >"$CASE_DIR/logs/$variant.stdout" 2>"$CASE_DIR/logs/$variant.stderr"
done < "$CASE_DIR/variants/variant_ids.txt"

"$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
"$CASE_DIR/compile_case.sh"
