#!/usr/bin/env bash
set -euo pipefail

CASE_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_BIN="${PYTHON_BIN:-python3}"
export PYTHONNOUSERSITE=1
ELMFIRE_BIN="${ELMFIRE_BIN:-elmfire}"

mkdir -p "$CASE_DIR/logs/matplotlib" "$CASE_DIR/outputs" "$CASE_DIR/figures"
export MPLCONFIGDIR="$CASE_DIR/logs/matplotlib"
"$PYTHON_BIN" "$CASE_DIR/scripts/preprocess.py"
"$PYTHON_BIN" "$CASE_DIR/scripts/spatial_evidence.py"

while IFS= read -r variant; do
  echo "[INFO] CASE30_MIT: $variant"
  (
    cd "$CASE_DIR"
    "$ELMFIRE_BIN" "variants/$variant/elmfire.data"
  ) >"$CASE_DIR/logs/$variant.stdout" 2>"$CASE_DIR/logs/$variant.stderr"
done < "$CASE_DIR/scripts/variants.txt"

"$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
"$CASE_DIR/compile_case.sh"
