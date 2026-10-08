#!/usr/bin/env bash
set -euo pipefail

CASE_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ELMFIRE_BIN=${ELMFIRE_BIN:-elmfire}
ELMFIRE_MPI_RANKS=${ELMFIRE_MPI_RANKS:-50}
PYTHON_BIN=${PYTHON_BIN:-python3}
export PYTHONNOUSERSITE=1
export PROJ_NETWORK=OFF

if ! [[ "$ELMFIRE_MPI_RANKS" =~ ^[1-9][0-9]*$ ]] || [[ "$ELMFIRE_MPI_RANKS" -gt 50 ]]; then
  echo "[ERROR] ELMFIRE_MPI_RANKS must be an integer from 1 through 50 for this 50-member case." >&2
  exit 2
fi

mkdir -p "$CASE_DIR/outputs" "$CASE_DIR/figures" "$CASE_DIR/logs/scratch"
"$PYTHON_BIN" "$CASE_DIR/scripts/preprocess.py"

if [[ "$ELMFIRE_MPI_RANKS" -gt 1 ]]; then
  command -v mpirun >/dev/null || { echo "[ERROR] mpirun is required for $ELMFIRE_MPI_RANKS ranks." >&2; exit 127; }
  launcher=(mpirun -np "$ELMFIRE_MPI_RANKS")
else
  launcher=()
fi

echo "[INFO] Running Camp Fire validation with $ELMFIRE_MPI_RANKS MPI rank(s)."
(cd "$CASE_DIR" && "${launcher[@]}" "$ELMFIRE_BIN" elmfire.data.in) \
  >"$CASE_DIR/logs/elmfire.stdout" 2>"$CASE_DIR/logs/elmfire.stderr"

"$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
bash "$CASE_DIR/compile_case.sh"
echo "[OK] Camp Fire validation pipeline complete."
