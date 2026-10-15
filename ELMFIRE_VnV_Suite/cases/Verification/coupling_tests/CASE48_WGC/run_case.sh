#!/usr/bin/env bash
set -euo pipefail

CASE_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_BIN="${PYTHON_BIN:-python3}"
ELMFIRE_BIN="${ELMFIRE_BIN:-elmfire}"
export PYTHONNOUSERSITE=1
export MPLCONFIGDIR="$CASE_DIR/logs/matplotlib"
mkdir -p "$MPLCONFIGDIR"

"$PYTHON_BIN" "$CASE_DIR/scripts/preprocess.py"
while IFS=$'\t' read -r variant input_fingerprint; do
  [[ -n "$variant" ]] || continue
  variant_dir="$CASE_DIR/variants/$variant"
  mkdir -p "$variant_dir/outputs" "$variant_dir/logs/scratch"
  find "$variant_dir/outputs" -mindepth 1 -delete
  find "$variant_dir/logs/scratch" -mindepth 1 -delete
  rm -f "$variant_dir/logs/completed_input_fingerprint.txt" \
    "$variant_dir/logs/completion_marker.json"
  echo "[INFO] CASE48_WGC: $variant"
  "$PYTHON_BIN" "$CASE_DIR/scripts/record_attempt.py" \
    --variant "$variant" --executable "$ELMFIRE_BIN" --status STARTED
  if (
    cd "$variant_dir"
    "$ELMFIRE_BIN" elmfire.data.in
  ) >"$variant_dir/logs/elmfire.stdout" 2>"$variant_dir/logs/elmfire.stderr"; then
    :
  else
    exit_code=$?
    "$PYTHON_BIN" "$CASE_DIR/scripts/record_attempt.py" \
      --variant "$variant" --executable "$ELMFIRE_BIN" --status FAILED \
      --exit-code "$exit_code" --failure-stage ELMFIRE_EXECUTION
    echo "[ERROR] ELMFIRE failed for $variant" >&2
    "$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
    "$CASE_DIR/compile_case.sh"
    exit 1
  fi
  if "$PYTHON_BIN" "$CASE_DIR/scripts/record_completion.py" \
      --variant "$variant" --executable "$ELMFIRE_BIN" \
      --execution-directory "$variant_dir"; then
    "$PYTHON_BIN" "$CASE_DIR/scripts/record_attempt.py" \
      --variant "$variant" --executable "$ELMFIRE_BIN" --status COMPLETED \
      --exit-code 0
  else
    exit_code=$?
    "$PYTHON_BIN" "$CASE_DIR/scripts/record_attempt.py" \
      --variant "$variant" --executable "$ELMFIRE_BIN" --status FAILED \
      --exit-code "$exit_code" --failure-stage COMPLETION_RECEIPT
    echo "[ERROR] completion receipt failed for $variant" >&2
    "$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
    "$CASE_DIR/compile_case.sh"
    exit 1
  fi
done < "$CASE_DIR/variants/run_plan.tsv"

"$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
"$CASE_DIR/compile_case.sh"
"$PYTHON_BIN" -c 'import json,sys; p=json.load(open(sys.argv[1], encoding="utf-8")); m=p.get("metrics"); ok=isinstance(p,dict) and p.get("case_id")=="CASE48_WGC" and p.get("overall_status")=="PASS" and p.get("workflow_status")=="COMPLETE" and p.get("verification_passed") is True and p.get("required_outputs_complete") is True and p.get("required_variant_count")==p.get("completed_variant_count")==4 and isinstance(m,list) and len(m)==9 and all(isinstance(x,dict) and x.get("status")=="PASS" for x in m); sys.exit(0 if ok else 1)' "$CASE_DIR/outputs/metrics.json"
