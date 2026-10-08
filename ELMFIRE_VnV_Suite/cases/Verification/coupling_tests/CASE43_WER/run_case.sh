#!/usr/bin/env bash
set -euo pipefail

CASE_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_BIN="${PYTHON_BIN:-python3}"
ELMFIRE_BIN="${ELMFIRE_BIN:-elmfire}"
export PYTHONNOUSERSITE=1
export MPLCONFIGDIR="$CASE_DIR/logs/matplotlib"
mkdir -p "$CASE_DIR/logs" "$MPLCONFIGDIR" "$CASE_DIR/outputs" "$CASE_DIR/figures"

"$PYTHON_BIN" "$CASE_DIR/scripts/preprocess.py"
while IFS= read -r variant; do
  [[ -n "$variant" ]] || continue
  config="$CASE_DIR/variants/$variant/elmfire.data"
  completion_marker="$CASE_DIR/variants/$variant/outputs/completion_marker.json"
  [[ -f "$config" ]] || { echo "[ERROR] missing generated namelist: $config" >&2; exit 2; }
  rm -f -- "$completion_marker"
  "$PYTHON_BIN" "$CASE_DIR/scripts/input_fingerprint.py" \
    --variant "$variant" --write-attempt --status STARTED
  echo "[INFO] CASE43_WER: $variant"
  if ! (cd "$CASE_DIR" && "$ELMFIRE_BIN" "${config#$CASE_DIR/}") \
      >"$CASE_DIR/logs/$variant.stdout" 2>"$CASE_DIR/logs/$variant.stderr"; then
    "$PYTHON_BIN" "$CASE_DIR/scripts/input_fingerprint.py" \
      --variant "$variant" --write-attempt --status FAILED
    echo "[ERROR] ELMFIRE failed for $variant" >&2
    "$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
    "$CASE_DIR/compile_case.sh"
    exit 1
  fi
  if ! "$PYTHON_BIN" "$CASE_DIR/scripts/input_fingerprint.py" \
      --variant "$variant" --write-completion-marker --executable "$ELMFIRE_BIN"; then
    "$PYTHON_BIN" "$CASE_DIR/scripts/input_fingerprint.py" \
      --variant "$variant" --write-attempt --status RECEIPT_FAILED
    echo "[ERROR] could not bind completion evidence for $variant" >&2
    "$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
    "$CASE_DIR/compile_case.sh"
    exit 1
  fi
done < "$CASE_DIR/variants/variant_ids.txt"

"$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
"$CASE_DIR/compile_case.sh"
"$PYTHON_BIN" -c 'import json,sys; p=json.load(open(sys.argv[1], encoding="utf-8")); m=p.get("metrics"); ok=isinstance(p,dict) and p.get("case_id")=="CASE43_WER" and p.get("overall_status")=="PASS" and p.get("workflow_status")=="COMPLETE" and p.get("verification_passed") is True and p.get("required_outputs_complete") is True and p.get("required_variant_count")==p.get("completed_variant_count")==23 and isinstance(m,list) and len(m)==9 and all(isinstance(x,dict) and x.get("status")=="PASS" for x in m); sys.exit(0 if ok else 1)' "$CASE_DIR/outputs/metrics.json"
