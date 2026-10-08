#!/usr/bin/env bash
set -euo pipefail

CASE_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)
(
  cd "$CASE_DIR/report"
  latexmk -lualatex -interaction=nonstopmode -halt-on-error -file-line-error case_report.tex
)
echo "[OK] Built $CASE_DIR/report/case_report.pdf"
