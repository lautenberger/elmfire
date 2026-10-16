#!/usr/bin/env bash
# Run one complete single-domain verification workflow.
#
# The preprocessor builds the local inputs and namelist. ELMFIRE then runs from
# the case directory so all relative paths resolve locally. The postprocessor
# calculates the acceptance metrics and figures before LaTeX rebuilds the
# standalone report. Configure ELMFIRE_BIN and PYTHON_BIN in the environment.
set -euo pipefail

CASE_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_BIN="${PYTHON_BIN:-python3}"
export PYTHONNOUSERSITE=1
ELMFIRE_BIN="${ELMFIRE_BIN:-elmfire}"

# Stage 1: create deterministic inputs and refresh the local namelist.
echo "[INFO] Preprocessing $(basename "$CASE_DIR")"
"$PYTHON_BIN" "$CASE_DIR/scripts/preprocess.py"

# Stage 2: remove only generated simulation rasters and scratch files, then run
# ELMFIRE. Input manifests written by preprocessing remain available for audit.
mkdir -p "$CASE_DIR/outputs" "$CASE_DIR/logs/scratch"
find "$CASE_DIR/outputs" -maxdepth 1 -type f \
  \( -name '*.tif' -o -name '*.csv' -o -name '*.bin' \) -delete
find "$CASE_DIR/logs/scratch" -mindepth 1 -delete

echo "[INFO] Running ELMFIRE"
(
  cd "$CASE_DIR"
  "$ELMFIRE_BIN" elmfire.data.in
) >"$CASE_DIR/logs/elmfire.stdout" 2>"$CASE_DIR/logs/elmfire.stderr"

# Stage 3: calculate explicit verification metrics and regenerate figures.
echo "[INFO] Postprocessing $(basename "$CASE_DIR")"
"$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"

# Stage 4: compile the self-contained verification-guide entry.
echo "[INFO] Compiling report/case_report.pdf"
(
  cd "$CASE_DIR/report"
  latexmk -pdf -interaction=nonstopmode -halt-on-error -file-line-error \
    case_report.tex
)

echo "[OK] Verification workflow completed: $(basename "$CASE_DIR")"
