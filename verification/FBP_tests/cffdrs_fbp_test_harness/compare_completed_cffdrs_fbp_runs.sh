#!/usr/bin/env bash
set -euo pipefail

# Compare the six already-completed CFFDRS/FBP verification suites.
#
# Expected directory layout (all relative to this script):
#   _generated_prior_legacy_mode2/
#   _generated_prior_legacy_mode3/
#   _generated_new_legacy_mode2/
#   _generated_new_legacy_mode3/
#   _generated_new_dynamic_mode2/
#   _generated_new_dynamic_mode3/
#
# This script DOES NOT rebuild ELMFIRE and DOES NOT rerun any test cases.
# It only maps the completed suite directories to the names expected by
# compare_fbp_models.py and runs the post-processing comparison.

HARNESS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PYTHON="${PYTHON:-python3}"
COMPARE_PY="${COMPARE_PY:-$HARNESS_DIR/compare_fbp_models.py}"

# Actual completed run directories.
PRIOR_LEGACY_MODE2="$HARNESS_DIR/_generated_prior_legacy_mode2"
PRIOR_LEGACY_MODE3="$HARNESS_DIR/_generated_prior_legacy_mode3"
NEW_LEGACY_MODE2="$HARNESS_DIR/_generated_new_legacy_mode2"
NEW_LEGACY_MODE3="$HARNESS_DIR/_generated_new_legacy_mode3"
NEW_DYNAMIC_MODE2="$HARNESS_DIR/_generated_new_dynamic_mode2"
NEW_DYNAMIC_MODE3="$HARNESS_DIR/_generated_new_dynamic_mode3"

# compare_fbp_models.py expects canonical suite names under one work root.
# Use symlinks so the completed test directories are never copied or modified.
WORK_ROOT="${WORK_ROOT:-$HARNESS_DIR/_comparison_workspace}"
OUT_DIR="${OUT_DIR:-$HARNESS_DIR/comparison}"

require_dir() {
    [[ -d "$1" ]] || {
        echo "ERROR: missing completed test directory: $1" >&2
        exit 1
    }
}

require_file() {
    [[ -f "$1" ]] || {
        echo "ERROR: missing file: $1" >&2
        exit 1
    }
}

require_file "$COMPARE_PY"
require_dir "$PRIOR_LEGACY_MODE2"
require_dir "$PRIOR_LEGACY_MODE3"
require_dir "$NEW_LEGACY_MODE2"
require_dir "$NEW_LEGACY_MODE3"
require_dir "$NEW_DYNAMIC_MODE2"
require_dir "$NEW_DYNAMIC_MODE3"

# Sanity-check that each completed suite contains case directories.
for ROOT in \
    "$PRIOR_LEGACY_MODE2" \
    "$PRIOR_LEGACY_MODE3" \
    "$NEW_LEGACY_MODE2" \
    "$NEW_LEGACY_MODE3" \
    "$NEW_DYNAMIC_MODE2" \
    "$NEW_DYNAMIC_MODE3"; do

    NCASES=$(find "$ROOT" -mindepth 1 -maxdepth 1 -type d -regextype posix-extended \
        -regex '.*/[0-9]{2}' | wc -l)

    if [[ "$NCASES" -eq 0 ]]; then
        echo "ERROR: no two-digit case directories found under $ROOT" >&2
        exit 1
    fi

    if [[ ! -f "$ROOT/targets.csv" ]]; then
        echo "WARNING: $ROOT/targets.csv is missing; target-error columns will be unavailable" >&2
    fi

    if [[ ! -f "$ROOT/timings_external.csv" ]]; then
        echo "WARNING: $ROOT/timings_external.csv is missing; runtime columns will be unavailable" >&2
    fi

done

# Recreate only the lightweight symlink workspace. The completed results remain untouched.
rm -rf "$WORK_ROOT"
mkdir -p "$WORK_ROOT" "$OUT_DIR"

ln -s "$PRIOR_LEGACY_MODE2" "$WORK_ROOT/old_legacy_mode2"
ln -s "$PRIOR_LEGACY_MODE3" "$WORK_ROOT/old_legacy_mode3"
ln -s "$NEW_LEGACY_MODE2"   "$WORK_ROOT/new_legacy_mode2"
ln -s "$NEW_LEGACY_MODE3"   "$WORK_ROOT/new_legacy_mode3"
ln -s "$NEW_DYNAMIC_MODE2"  "$WORK_ROOT/new_dynamic_mode2"
ln -s "$NEW_DYNAMIC_MODE3"  "$WORK_ROOT/new_dynamic_mode3"

echo "CFFDRS/FBP completed-run comparison"
echo "===================================="
echo "Prior legacy MODE=2 : $PRIOR_LEGACY_MODE2"
echo "Prior legacy MODE=3 : $PRIOR_LEGACY_MODE3"
echo "New legacy MODE=2   : $NEW_LEGACY_MODE2"
echo "New legacy MODE=3   : $NEW_LEGACY_MODE3"
echo "New dynamic MODE=2  : $NEW_DYNAMIC_MODE2"
echo "New dynamic MODE=3  : $NEW_DYNAMIC_MODE3"
echo "Comparison output   : $OUT_DIR"
echo

"$PYTHON" "$COMPARE_PY" \
    --work-root "$WORK_ROOT" \
    --out-dir "$OUT_DIR"

echo
echo "Comparison complete."
echo "Results written to: $OUT_DIR"
echo
echo "Primary outputs:"
for FILE in \
    model_case_summary.csv \
    pairwise_raster_metrics.csv \
    runtime_and_accuracy_summary.csv \
    comparison_report.md \
    ros_target_comparison_mode2.png \
    ros_target_comparison_mode3.png \
    fi_target_comparison_mode2.png \
    fi_target_comparison_mode3.png \
    runtime_by_case_mode2.png \
    runtime_by_case_mode3.png; do
    if [[ -f "$OUT_DIR/$FILE" ]]; then
        echo "  $OUT_DIR/$FILE"
    fi
done
