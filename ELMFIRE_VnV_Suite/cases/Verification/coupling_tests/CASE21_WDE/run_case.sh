#!/usr/bin/env bash
set -euo pipefail

CASE_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)
PYTHON_BIN=${PYTHON_BIN:-python3}
export PYTHONNOUSERSITE=1
ELMFIRE_BIN=${ELMFIRE_BIN:-elmfire}

mkdir -p "$CASE_DIR/logs"
export MPLCONFIGDIR="$CASE_DIR/logs/matplotlib"

"$PYTHON_BIN" "$CASE_DIR/scripts/generate_inputs.py"
"$PYTHON_BIN" "$CASE_DIR/scripts/spatial_evidence.py"

while IFS=$'\t' read -r label config_rel guide_name; do
  [[ -z "$label" || "$label" == \#* ]] && continue
  config_path="$CASE_DIR/$config_rel"
  variant_dir=$(dirname "$config_path")
  mkdir -p "$variant_dir/outputs" "$variant_dir/scratch"
  echo "[INFO] Running $guide_name ($label)"
  (
    cd "$variant_dir"
    "$ELMFIRE_BIN" "$(basename "$config_path")"
  ) >"$CASE_DIR/logs/${label}.stdout" 2>"$CASE_DIR/logs/${label}.stderr"
done < "$CASE_DIR/scripts/variants.tsv"

"$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
"$CASE_DIR/compile_case.sh"
