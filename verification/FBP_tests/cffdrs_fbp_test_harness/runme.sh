#!/usr/bin/env bash
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PYTHON="${PYTHON:-python3}"
# ELMFIRE_BIN="${ELMFIRE_BIN:-elmfire}"
ELMFIRE_BIN=elmfire_1.1.prior
PROFILE="${PROFILE:-legacy}"          # legacy | dynamic
VERSION="${VERSION:-prior}"          # prior | new
MODE="${MODE:-3}"                    # 2 | 3
DEBUG="${DEBUG:-0}"                  # 1 enables CFFDRS diagnostic rasters
TIMINGS="${TIMINGS:-0}"              # 1 enables ELMFIRE internal timing CSV
N_ITERS="${N_ITERS:-20}"
#SOURCE_ROOT="${SOURCE_ROOT:-$HERE}"
SOURCE_ROOT=~/elmfire/verification/FBP_tests
RUN_ROOT="${RUN_ROOT:-$HERE/_generated_${VERSION}_${PROFILE}_mode${MODE}}"

command -v "$PYTHON" >/dev/null 2>&1 || { echo "ERROR: $PYTHON not found" >&2; exit 1; }
if [[ "$ELMFIRE_BIN" == */* ]]; then
    [[ -x "$ELMFIRE_BIN" ]] || { echo "ERROR: $ELMFIRE_BIN is not executable" >&2; exit 1; }
else
    command -v "$ELMFIRE_BIN" >/dev/null 2>&1 || { echo "ERROR: $ELMFIRE_BIN not found in PATH" >&2; exit 1; }
fi

SETUP_ARGS=(
    --root "$SOURCE_ROOT"
    --output-root "$RUN_ROOT"
    --profile "$PROFILE"
    --mode "$MODE"
    --n-iters "$N_ITERS"
    --clean
)
[[ "$DEBUG" == "1" ]] && SETUP_ARGS+=(--debug)
[[ "$TIMINGS" == "1" ]] && SETUP_ARGS+=(--timings)

"$PYTHON" "$HERE/setup.py" "${SETUP_ARGS[@]}"

printf 'case,wall_s,user_s,sys_s,max_rss_kb,exit_code\n' > "$RUN_ROOT/timings_external.csv"
FAILED=0

for CASE_DIR in "$RUN_ROOT"/[0-9][0-9]; do
    [[ -d "$CASE_DIR" ]] || continue
    CASE="$(basename "$CASE_DIR")"
    DATA_FILE="$CASE.data"
    LOG="$CASE_DIR/outputs/elmfire_${CASE}.log"
    TIMEFILE="$CASE_DIR/outputs/time_${CASE}.csv"

    echo "============================================================"
    echo "Running $CASE  profile=$PROFILE  MODE=$MODE"
    echo "============================================================"

    rm -f "$CASE_DIR/outputs"/*.tif "$CASE_DIR/outputs"/*.csv "$LOG" "$TIMEFILE" 2>/dev/null || true

    set +e
    if [[ -x /usr/bin/time ]]; then
        (
            cd "$CASE_DIR"
            /usr/bin/time -f '%e,%U,%S,%M' -o "$TIMEFILE" "$ELMFIRE_BIN" "$DATA_FILE" > "$LOG" 2>&1
        )
        RC=$?
    else
        START_NS=$(date +%s%N)
        (
            cd "$CASE_DIR"
            "$ELMFIRE_BIN" "$DATA_FILE" > "$LOG" 2>&1
        )
        RC=$?
        END_NS=$(date +%s%N)
        WALL=$(awk -v a="$START_NS" -v b="$END_NS" 'BEGIN {printf "%.6f", (b-a)/1e9}')
        printf '%s,NA,NA,NA\n' "$WALL" > "$TIMEFILE"
    fi
    set -e

    if [[ -s "$TIMEFILE" ]]; then
        IFS=, read -r WALL USER SYS RSS < "$TIMEFILE"
    else
        WALL=NA; USER=NA; SYS=NA; RSS=NA
    fi
    printf '%s,%s,%s,%s,%s,%s\n' "$CASE" "$WALL" "$USER" "$SYS" "$RSS" "$RC" >> "$RUN_ROOT/timings_external.csv"

    if [[ "$RC" -ne 0 ]] || ! grep -q 'End of simulation reached successfully' "$LOG"; then
        echo "FAILED: $CASE. See $LOG" >&2
        FAILED=1
    else
        echo "SUCCESS: $CASE (${WALL}s wall)"
    fi
    echo

done

OUTPUT_KIND=headfire
[[ "$MODE" == "3" ]] && OUTPUT_KIND=levelset
"$PYTHON" "$HERE/compare.py" --root "$RUN_ROOT" --output-kind "$OUTPUT_KIND"

if [[ "$FAILED" -ne 0 ]]; then
    echo "One or more simulations failed." >&2
    exit 1
fi

echo "Run root: $RUN_ROOT"
