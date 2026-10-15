#!/usr/bin/env bash
set -euo pipefail

CASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_BIN="${PYTHON_BIN:-python3}"
export PYTHONNOUSERSITE=1
ELMFIRE_BIN="${ELMFIRE_BIN:-elmfire}"

mkdir -p "$CASE_DIR/data/outputs" "$CASE_DIR/data/scratch" "$CASE_DIR/logs" "$CASE_DIR/figures" "$CASE_DIR/outputs"
"$PYTHON_BIN" "$CASE_DIR/scripts/generate_inputs.py"
"$PYTHON_BIN" "$CASE_DIR/scripts/spatial_evidence.py"
(
  cd "$CASE_DIR"
  "$ELMFIRE_BIN" "data/inputs/elmfire.data" > "logs/elmfire.stdout" 2> "logs/elmfire.stderr"
)
"$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
"$CASE_DIR/compile_case.sh"
