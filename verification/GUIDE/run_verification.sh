#!/usr/bin/env bash
#
# Convenience wrapper: activate the elmfire conda env, make sure the ELMFIRE
# binary is on PATH, then run the GUIDE verification harness.
#
# Examples:
#   ./run_verification.sh                       # all cases
#   ./run_verification.sh --cases Point Windy   # a subset
#   ./run_verification.sh --ia-ensemble 1000    # shrink the heavy suppression run
#
# NOTE: do NOT use `set -u` here. Sourcing conda's profile script references
# unbound variables internally, which under `set -u` aborts this wrapper
# silently (exit 1, no output) before it ever reaches the run. That is the
# classic "nothing happens" symptom.

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Activate the elmfire conda environment if available.
if [ -f /home/nick/miniconda3/etc/profile.d/conda.sh ]; then
  # shellcheck disable=SC1091
  source /home/nick/miniconda3/etc/profile.d/conda.sh
  conda activate elmfire 2>/dev/null || true
fi

# Locate the ELMFIRE binary (allow override via ELMFIRE_BIN).
if [ -z "${ELMFIRE_BIN:-}" ]; then
  if command -v elmfire >/dev/null 2>&1; then
    ELMFIRE_BIN="$(command -v elmfire)"
  elif [ -x /home/nick/elmfire/elmfire/build/linux/bin/elmfire ]; then
    ELMFIRE_BIN=/home/nick/elmfire/elmfire/build/linux/bin/elmfire
  else
    echo "ERROR: elmfire binary not found. Set ELMFIRE_BIN." >&2
    exit 1
  fi
fi
export ELMFIRE_BIN

echo "Using ELMFIRE_BIN=$ELMFIRE_BIN"
echo "Python: $(command -v python)"
echo "Starting harness (this prints progress per case; runs can take a while)..."
echo

# -u => unbuffered stdout so progress shows live even when piped / in IDE terminals.
PYTHONUNBUFFERED=1 python -u "$HERE/verify_guide.py" "$@"
status=$?
echo
echo "Harness exited with status $status"
exit $status
