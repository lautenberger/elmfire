#!/usr/bin/env bash
set -euo pipefail

CASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_BIN="${PYTHON_BIN:-python3}"
export PYTHONNOUSERSITE=1
ELMFIRE_BIN="${ELMFIRE_BIN:-elmfire}"

mkdir -p "$CASE_DIR/data/outputs" "$CASE_DIR/data/scratch" "$CASE_DIR/logs" "$CASE_DIR/figures" "$CASE_DIR/outputs"
"$PYTHON_BIN" "$CASE_DIR/scripts/generate_inputs.py"
"$PYTHON_BIN" "$CASE_DIR/scripts/spatial_evidence.py"

for config_path in "$CASE_DIR"/data/inputs/elmfire_[0-9]*.data; do
  config_name="$(basename "$config_path")"
  time_grid="${config_name#elmfire_}"
  time_grid="${time_grid%.data}"
  mkdir -p "$CASE_DIR/data/outputs/$time_grid" "$CASE_DIR/data/scratch/$time_grid" "$CASE_DIR/logs/$time_grid"
  echo "[INFO] Running temporal grid $time_grid"
  (
    cd "$CASE_DIR"
    "$ELMFIRE_BIN" "data/inputs/$config_name" > "logs/$time_grid/elmfire.stdout" 2> "logs/$time_grid/elmfire.stderr"
  )
done

"$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
"$CASE_DIR/compile_case.sh"
