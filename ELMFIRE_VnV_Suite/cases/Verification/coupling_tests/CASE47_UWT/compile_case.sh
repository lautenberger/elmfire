#!/usr/bin/env bash
set -euo pipefail

CASE_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_BIN="${PYTHON_BIN:-python3}"
"$PYTHON_BIN" "$CASE_DIR/scripts/metrics_to_macro.py"
"$PYTHON_BIN" "$CASE_DIR/scripts/report_readability.py"
(cd "$CASE_DIR/report" && latexmk -lualatex -interaction=nonstopmode -halt-on-error case_report.tex)
echo "[OK] Built $CASE_DIR/report/case_report.pdf"
