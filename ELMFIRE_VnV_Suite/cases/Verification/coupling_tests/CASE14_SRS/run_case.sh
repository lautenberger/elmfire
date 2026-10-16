#!/usr/bin/env bash
# Run one complete manifest-based verification workflow.
#
# The preprocessor owns all variant construction. This runner converts its
# manifest into a small tab-separated execution plan, runs every supported
# ELMFIRE variant in its own directory, records logs and input fingerprints,
# calculates verification metrics, and rebuilds the standalone PDF report.
# Configure ELMFIRE_BIN and, if needed, PYTHON_BIN before invoking this file.
set -euo pipefail

CASE_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_BIN="${PYTHON_BIN:-python3}"
export PYTHONNOUSERSITE=1
ELMFIRE_BIN="${ELMFIRE_BIN:-elmfire}"

# Stage 1: generate deterministic rasters, namelists, and the variant manifest.
echo "[INFO] Preprocessing $(basename "$CASE_DIR")"
"$PYTHON_BIN" "$CASE_DIR/scripts/preprocess.py"

# Stage 2: translate supported manifest entries into an execution plan.
# Cases use either a JSON array or a dictionary containing ``variants``; this
# normalization keeps the shell loop simple and reports capability-gated skips.
mkdir -p "$CASE_DIR/logs"
RUN_PLAN="$CASE_DIR/logs/variant_run_plan.tsv"
"$PYTHON_BIN" - "$CASE_DIR/variants/manifest.json" >"$RUN_PLAN" <<'PLAN_PY'
import json
import sys
from pathlib import Path

manifest = json.loads(Path(sys.argv[1]).read_text(encoding="utf-8"))
variants = manifest if isinstance(manifest, list) else manifest["variants"]
for item in variants:
    name = item["name"]
    if not item.get("runnable", True):
        reason = item.get("capability_status", "required capability unavailable")
        print(f"[SKIP] {name}: {reason}", file=sys.stderr)
        continue
    working_directory = (
        item.get("working_directory") or item.get("directory") or item.get("path")
    )
    config = Path(item.get("config") or item.get("namelist") or "elmfire.data.in").name
    fingerprint = item.get("input_fingerprint", "")
    print("\t".join((name, working_directory, config, fingerprint)))
PLAN_PY

# Each variant is isolated under variants/<name>. Clear generated products so
# postprocessing cannot mix old and new simulations; retain inputs and configs.
while IFS=$'\t' read -r name working_directory config input_fingerprint; do
  variant_dir="$CASE_DIR/$working_directory"
  mkdir -p "$variant_dir/outputs" "$variant_dir/logs/scratch"
  find "$variant_dir/outputs" -mindepth 1 -delete
  find "$variant_dir/logs/scratch" -mindepth 1 -delete
  rm -f "$variant_dir/logs/completed_input_fingerprint.txt"

  echo "[INFO] Running $name"
  (
    cd "$variant_dir"
    "$ELMFIRE_BIN" "$config"
  ) >"$variant_dir/logs/elmfire.stdout" \
    2>"$variant_dir/logs/elmfire.stderr"

  # Fingerprints let postprocessing reject results from a different input set.
  # Cases without a fingerprint simply omit the completion marker.
  if [[ -n "$input_fingerprint" ]]; then
    printf '%s\n' "$input_fingerprint" \
      >"$variant_dir/logs/completed_input_fingerprint.txt"
  fi
  echo "[OK] $name completed"
done <"$RUN_PLAN"

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
