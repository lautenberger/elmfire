#!/usr/bin/env bash
set -euo pipefail
CASE_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_BIN="${PYTHON_BIN:-python3}"
"$PYTHON_BIN" "$CASE_DIR/scripts/metrics_to_macro.py"
(cd "$CASE_DIR/report" && latexmk -lualatex -silent case_report.tex)
echo "[OK] Built $CASE_DIR/report/case_report.pdf"
