#!/usr/bin/env bash

set -u   # no set -e (elmfire exit code is unreliable)

BASE_DIR="$(pwd)"
FAILED=0

command -v elmfire >/dev/null || {
  echo "ERROR: elmfire not found in PATH"
  exit 1
}

mapfile -t RUN_DIRS < <(ls -d [0-9][0-9] 2>/dev/null | sort)

if [ "${#RUN_DIRS[@]}" -eq 0 ]; then
  echo "No run directories found"
  exit 1
fi

for d in "${RUN_DIRS[@]}"; do
  echo "========================================"
  echo "Running ELMFIRE in folder: $d"
  echo "========================================"

  cd "$BASE_DIR/$d" || { echo "Cannot cd into $d"; FAILED=1; continue; }

  DATA_FILE="$d.data"
  if [ ! -f "$DATA_FILE" ]; then
    echo "Missing $DATA_FILE"
    FAILED=1
    continue
  fi

  mkdir -p outputs
  LOGFILE="outputs/elmfire_${d}.log"

  # ---- RUN ELMFIRE ----
  elmfire "$DATA_FILE" > "$LOGFILE" 2>&1

  # ---- SUCCESS CHECK GOES HERE ----
  if grep -q "End of simulation reached successfully" "$LOGFILE"; then
    echo "SUCCESS: $d"
  else
    echo "FAILED: $d (success message not found)"
    FAILED=1
  fi

  echo
done

cd "$BASE_DIR"

if [ $FAILED -ne 0 ]; then
  echo "One or more runs failed."
  exit 1
fi

echo "All simulations completed."
