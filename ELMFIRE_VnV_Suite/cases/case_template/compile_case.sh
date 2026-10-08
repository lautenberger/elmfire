#!/usr/bin/env bash
set -euo pipefail

# --- Configuration ---
SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)
CASE_DIR="$SCRIPT_DIR"

# --- Generate LaTeX macros from metrics.json ---
METRICS_JSON="$CASE_DIR/outputs/metrics.json"
MACROS_TEX="$CASE_DIR/report/metrics_macros.tex"

echo "[INFO] Creating $MACROS_TEX from $METRICS_JSON"
python3 "$CASE_DIR/scripts/metrics_to_macro.py"

# --- Build case report PDF ---
( cd "$CASE_DIR/report" && latexmk -lualatex -silent case_report.tex )

# --- Done ---
echo "[OK] Built $CASE_DIR/report/case_report.pdf"

