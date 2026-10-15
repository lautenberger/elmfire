#!/usr/bin/env bash
set -euo pipefail

CASE_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
python3 "$CASE_DIR/scripts/metrics_to_macro.py"
(cd "$CASE_DIR/report" && latexmk -lualatex -interaction=nonstopmode -halt-on-error case_report.tex)
echo "[OK] Built $CASE_DIR/report/case_report.pdf"
