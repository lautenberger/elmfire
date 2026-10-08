#!/usr/bin/env bash
set -euo pipefail

CASE_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_BIN="${PYTHON_BIN:-python3}"
rm -f -- "$CASE_DIR/report/case_report.pdf"
"$PYTHON_BIN" "$CASE_DIR/scripts/metrics_to_macro.py"
"$PYTHON_BIN" "$CASE_DIR/scripts/report_readability.py"
(cd "$CASE_DIR/report" && latexmk -lualatex -g -silent case_report.tex)
[[ -s "$CASE_DIR/report/case_report.pdf" ]] || { echo "[ERROR] report PDF was not built" >&2; exit 1; }
echo "[OK] Built $CASE_DIR/report/case_report.pdf"
