#!/usr/bin/env bash
set -euo pipefail

CASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_BIN="${PYTHON_BIN:-python3}"
export PYTHONNOUSERSITE=1
ELMFIRE_BIN="${ELMFIRE_BIN:-elmfire}"

mkdir -p "$CASE_DIR/logs" "$CASE_DIR/figures" "$CASE_DIR/outputs"
"$PYTHON_BIN" "$CASE_DIR/scripts/generate_inputs.py"
"$PYTHON_BIN" "$CASE_DIR/scripts/spatial_evidence.py"

for config_path in "$CASE_DIR"/data/*/inputs/elmfire.data; do
  variant_dir="$(cd "$(dirname "$config_path")/.." && pwd)"
  variant_name="$(basename "$variant_dir")"
  mkdir -p "$variant_dir/logs" "$variant_dir/outputs" "$variant_dir/scratch"
  echo "[INFO] Running spatial grid $variant_name"
  (
    cd "$variant_dir"
    "$ELMFIRE_BIN" "inputs/elmfire.data" > "logs/elmfire.stdout" 2> "logs/elmfire.stderr"
  )
done

"$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
"$CASE_DIR/compile_case.sh"
