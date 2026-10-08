#!/usr/bin/env bash
# Prepare and evaluate the capability-gated parametric verification design.
#
# This case requires the resolution-independent single-structure adapter. The
# reviewed capability flags in case.json currently mark it unavailable, so
# preprocessing records every point as non-runnable. Postprocessing makes that
# limitation explicit in metrics and figures; no substitute model is run.
set -euo pipefail

CASE_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_BIN="${PYTHON_BIN:-python3}"
export PYTHONNOUSERSITE=1

# Stage 1: generate the complete parameter design and capability manifest.
echo "[INFO] Preprocessing $(basename "$CASE_DIR")"
"$PYTHON_BIN" "$CASE_DIR/scripts/preprocess.py"

# Stage 2: evaluate imported evidence and capability status.
echo "[INFO] Postprocessing $(basename "$CASE_DIR")"
"$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"

# Stage 3: compile the self-contained verification-guide entry.
echo "[INFO] Compiling report/case_report.pdf"
(
  cd "$CASE_DIR/report"
  latexmk -pdf -interaction=nonstopmode -halt-on-error -file-line-error \
    case_report.tex
)

echo "[OK] Capability-gated verification workflow completed: $(basename "$CASE_DIR")"
