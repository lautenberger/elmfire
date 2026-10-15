#!/usr/bin/env bash
set -euo pipefail

CASE_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_BIN="${PYTHON_BIN:-python3}"
ELMFIRE_REQUESTED="${ELMFIRE_BIN:-elmfire}"
export PYTHONNOUSERSITE=1
export MPLCONFIGDIR="$CASE_DIR/logs/matplotlib"
mkdir -p "$CASE_DIR/logs" "$MPLCONFIGDIR" "$CASE_DIR/outputs" "$CASE_DIR/figures"

"$PYTHON_BIN" "$CASE_DIR/scripts/preprocess.py"
"$PYTHON_BIN" "$CASE_DIR/scripts/record_attempt.py" runner-start \
  --requested-executable "$ELMFIRE_REQUESTED"

ELMFIRE_RESOLVED="$(command -v -- "$ELMFIRE_REQUESTED" || true)"
if [[ -z "$ELMFIRE_RESOLVED" || ! -x "$ELMFIRE_RESOLVED" ]]; then
  echo "[ERROR] ELMFIRE executable cannot be resolved: $ELMFIRE_REQUESTED" >&2
  "$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
  "$CASE_DIR/compile_case.sh"
  exit 1
fi
ELMFIRE_RESOLVED="$("$PYTHON_BIN" -c 'from pathlib import Path; import sys; print(Path(sys.argv[1]).resolve(strict=True))' "$ELMFIRE_RESOLVED")"

run_failed=0
while IFS= read -r variant; do
  [[ -n "$variant" ]] || continue
  variant_dir="$CASE_DIR/variants/$variant"
  config="$variant_dir/elmfire.data"
  stdout_path="$variant_dir/logs/elmfire.stdout"
  stderr_path="$variant_dir/logs/elmfire.stderr"
  marker="$variant_dir/outputs/run_complete.json"
  [[ -f "$config" ]] || { echo "[ERROR] missing $config" >&2; run_failed=1; break; }
  mkdir -p "$variant_dir/logs"
  rm -f -- "$marker" "$stdout_path" "$stderr_path"
  echo "[INFO] CASE46_WUT: $variant"
  "$PYTHON_BIN" "$CASE_DIR/scripts/record_attempt.py" start \
    --variant "$variant" \
    --executable "$ELMFIRE_RESOLVED" \
    --stdout "$stdout_path" \
    --stderr "$stderr_path"
  set +e
  (cd "$CASE_DIR" && "$ELMFIRE_RESOLVED" "${config#$CASE_DIR/}") \
      >"$stdout_path" 2>"$stderr_path"
  elmfire_exit=$?
  set -e
  "$PYTHON_BIN" "$CASE_DIR/scripts/record_attempt.py" finish \
    --variant "$variant" --exit-code "$elmfire_exit"
  if (( elmfire_exit != 0 )); then
    echo "[ERROR] ELMFIRE failed for $variant; no completion receipt written" >&2
    run_failed=1
    break
  fi
  if ! "$PYTHON_BIN" "$CASE_DIR/scripts/record_success.py" \
      --variant "$variant" \
      --executable "$ELMFIRE_RESOLVED" \
      --execution-directory "$CASE_DIR" \
      --stdout "$stdout_path" \
      --stderr "$stderr_path"; then
    echo "[ERROR] provenance receipt validation failed for $variant" >&2
    run_failed=1
    break
  fi
done < "$CASE_DIR/variants/variant_ids.txt"

"$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
"$CASE_DIR/compile_case.sh"
if (( run_failed != 0 )); then
  exit 1
fi

"$PYTHON_BIN" - "$CASE_DIR/outputs/metrics.json" <<'PY'
import json
import sys
from pathlib import Path

path = Path(sys.argv[1])
payload = json.loads(path.read_text(encoding="utf-8"))
expected_names = [
    "evidence and exact dump ledger",
    "source fireline-intensity setup",
    "terminal PHI/TOA state consistency",
    "intended W-to-U receiver heat exposure",
    "intended W-to-U finite ignition delay",
    "contiguous strict-threshold matrix",
    "isolated distance/threshold matrix",
    "first-update transition timing",
    "zero-source control",
]
metrics = payload.get("metrics")
valid = (
    payload.get("schema_version") == 2
    and payload.get("case_id") == "CASE46_WUT"
    and payload.get("overall_status") == "PASS"
    and payload.get("workflow_status") == "COMPLETE"
    and payload.get("verification_passed") is True
    and payload.get("required_outputs_complete") is True
    and payload.get("required_variant_count") == 7
    and payload.get("completed_variant_count") == 7
    and isinstance(payload.get("executable"), dict)
    and isinstance(metrics, list)
    and [item.get("name") for item in metrics] == expected_names
    # The FLIN shortcut is diagnostic; the primary heat/FTP rows decide PASS.
    and all(metrics[index].get("status") == "PASS" for index in (0, 1, 2, 3, 4, 8))
    and all(item.get("status") in {"PASS", "FAIL", "NOT EVALUABLE"} for item in metrics[5:8])
)
if not valid:
    raise SystemExit("CASE46_WUT strict PASS gate rejected the generated verdict")
PY

echo "[PASS] CASE46_WUT: strict verification gate satisfied"
