#!/usr/bin/env bash
set -euo pipefail

CASE_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_BIN="${PYTHON_BIN:-python3}"
ELMFIRE_BIN="${ELMFIRE_BIN:-elmfire}"
export PYTHONNOUSERSITE=1

mkdir -p "$CASE_DIR/outputs" "$CASE_DIR/figures" "$CASE_DIR/logs/matplotlib"
export MPLCONFIGDIR="$CASE_DIR/logs/matplotlib"
"$PYTHON_BIN" "$CASE_DIR/scripts/preprocess.py"
"$PYTHON_BIN" "$CASE_DIR/scripts/spatial_evidence.py"

while IFS= read -r variant; do
  [[ -n "$variant" ]] || continue
  variant_dir="$CASE_DIR/variants/$variant"
  config="$CASE_DIR/variants/$variant/elmfire.data"
  [[ -f "$config" ]] || { echo "[ERROR] missing variant namelist: $config" >&2; exit 2; }
  mkdir -p "$variant_dir/logs"
  rm -f "$variant_dir/logs/completed_input_fingerprint.txt" \
    "$variant_dir/logs/completion_marker.json"
  echo "[INFO] CASE47_UWT: $variant"
  "$PYTHON_BIN" "$CASE_DIR/scripts/record_attempt.py" \
    --variant "$variant" --executable "$ELMFIRE_BIN" --status STARTED
  if (cd "$CASE_DIR" && "$ELMFIRE_BIN" "${config#$CASE_DIR/}") \
      >"$CASE_DIR/logs/$variant.stdout" 2>"$CASE_DIR/logs/$variant.stderr"; then
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
      --execution-directory "$CASE_DIR"; then
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
done < "$CASE_DIR/variants/variant_ids.txt"

"$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
"$CASE_DIR/compile_case.sh"
"$PYTHON_BIN" -c 'import json,sys; p=json.load(open(sys.argv[1], encoding="utf-8")); m=p.get("metrics"); ok=isinstance(p,dict) and p.get("case_id")=="CASE47_UWT" and p.get("overall_status")=="PASS" and p.get("workflow_status")=="COMPLETE" and p.get("verification_passed") is True and p.get("required_outputs_complete") is True and p.get("required_variant_count")==p.get("completed_variant_count")==8 and isinstance(m,list) and m and all(isinstance(x,dict) and x.get("status")=="PASS" for x in m); sys.exit(0 if ok else 1)' "$CASE_DIR/outputs/metrics.json"
