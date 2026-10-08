#!/usr/bin/env bash
set -euo pipefail

CASE_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_BIN="${PYTHON_BIN:-python3}"
ELMFIRE_BIN="${ELMFIRE_BIN:-elmfire}"
export PYTHONNOUSERSITE=1
export MPLCONFIGDIR="$CASE_DIR/logs/matplotlib"
mkdir -p "$CASE_DIR/logs" "$MPLCONFIGDIR" "$CASE_DIR/outputs" "$CASE_DIR/figures"

"$PYTHON_BIN" "$CASE_DIR/scripts/preprocess.py"

if [[ "$ELMFIRE_BIN" == */* ]]; then
  if [[ "$ELMFIRE_BIN" == /* ]]; then
    executable_candidate="$ELMFIRE_BIN"
  else
    executable_candidate="$CASE_DIR/$ELMFIRE_BIN"
  fi
else
  executable_candidate="$(command -v "$ELMFIRE_BIN")" || {
    echo "[ERROR] executable not found: $ELMFIRE_BIN" >&2
    exit 127
  }
fi
[[ -x "$executable_candidate" ]] || {
  echo "[ERROR] executable is not runnable: $executable_candidate" >&2
  exit 126
}
ELMFIRE_EXECUTABLE="$(cd -- "$(dirname -- "$executable_candidate")" && pwd -P)/$(basename -- "$executable_candidate")"
while IFS= read -r variant; do
  [[ -n "$variant" ]] || continue
  config="$CASE_DIR/variants/$variant/elmfire.data"
  marker="$CASE_DIR/variants/$variant/outputs/run_complete.json"
  stdout_path="$CASE_DIR/logs/$variant.stdout"
  stderr_path="$CASE_DIR/logs/$variant.stderr"
  [[ -f "$config" ]] || { echo "[ERROR] missing $config" >&2; exit 2; }
  rm -f -- "$marker"
  echo "[INFO] CASE45_HRS: $variant"
  if ! (cd "$CASE_DIR" && "$ELMFIRE_EXECUTABLE" "${config#$CASE_DIR/}") \
      >"$stdout_path" 2>"$stderr_path"; then
    echo "[ERROR] ELMFIRE failed for $variant; no completion marker written" >&2
    "$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
    "$CASE_DIR/compile_case.sh"
    exit 1
  fi
  "$PYTHON_BIN" "$CASE_DIR/scripts/record_success.py" "$variant" \
    --executable "$ELMFIRE_EXECUTABLE" --stdout "$stdout_path" --stderr "$stderr_path"
done < "$CASE_DIR/variants/variant_ids.txt"
"$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
"$CASE_DIR/compile_case.sh"
"$PYTHON_BIN" "$CASE_DIR/scripts/assert_pass.py"
