#!/usr/bin/env bash
set -euo pipefail

HARNESS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PYTHON="${PYTHON:-python3}"

# Repository/test locations.
REPO_ROOT="${REPO_ROOT:-$(git rev-parse --show-toplevel 2>/dev/null || true)}"
[[ -n "$REPO_ROOT" ]] || { echo "ERROR: set REPO_ROOT to the ELMFIRE repository" >&2; exit 1; }
SOURCE_ROOT="${SOURCE_ROOT:-$REPO_ROOT/verification/FBP_tests}"
WORK_ROOT="${WORK_ROOT:-$SOURCE_ROOT/_model_comparison}"

# Revisions used when executables are not supplied explicitly.
OLD_REF="${OLD_REF:-upstream/main}"
NEW_REF="${NEW_REF:-updated_directory_structure_maria}"
# OLD_ELMFIRE="${OLD_ELMFIRE:-}"
# NEW_ELMFIRE="${NEW_ELMFIRE:-}"
NEW_ELMFIRE="$HOME/elmfire/build/linux/bin/elmfire_1.1.new"
OLD_ELMFIRE="$HOME/elmfire/build/linux/bin/elmfire_1.1.prior"

N_ITERS="${N_ITERS:-20}"
TEST_MODES="${TEST_MODES:-2 3}"
RUN_DEBUG_SUITE="${RUN_DEBUG_SUITE:-1}"
RUN_MPI_SMOKE="${RUN_MPI_SMOKE:-1}"
MPI_PROCS="${MPI_PROCS:-1 2 4}"
MPIEXEC="${MPIEXEC:-mpirun}"
MPIEXEC_ARGS="${MPIEXEC_ARGS:-}"

mkdir -p "$WORK_ROOT"

require_file() {
    [[ -f "$1" ]] || { echo "ERROR: missing $1" >&2; exit 1; }
}
require_file "$SOURCE_ROOT/parameters.csv"
require_file "$SOURCE_ROOT/targets.csv"
require_file "$SOURCE_ROOT/daily_weather.txt"
require_file "$HARNESS_DIR/setup.py"
require_file "$HARNESS_DIR/runme.sh"
require_file "$HARNESS_DIR/compare_fbp_models.py"

build_ref() {
    local label="$1"
    local ref="$2"
    local wt="$WORK_ROOT/worktrees/$label"
    local bin="$WORK_ROOT/bin/$label"

    mkdir -p "$WORK_ROOT/worktrees" "$bin"

    if [[ -e "$wt/.git" || -d "$wt/.git" ]]; then
        git -C "$REPO_ROOT" worktree remove --force "$wt" >/dev/null 2>&1 || true
    fi
    rm -rf "$wt"

    echo "Creating $label worktree from $ref"
    git -C "$REPO_ROOT" rev-parse --verify "$ref^{commit}" >/dev/null || {
        echo "ERROR: git ref '$ref' is not available locally. Fetch it first." >&2
        exit 1
    }
    git -C "$REPO_ROOT" worktree add --detach "$wt" "$ref"

    echo "Building $label ($ref)"
    (
        cd "$wt"
        mkdir -p "$bin"
        ELMFIRE_INSTALL_DIR="$bin" ./build/linux/make_gnu.sh elmfire
    )

    [[ -x "$bin/elmfire" ]] || { echo "ERROR: build did not produce $bin/elmfire" >&2; exit 1; }
    printf '%s\n' "$bin/elmfire"
}

if [[ -z "$OLD_ELMFIRE" ]]; then
    OLD_ELMFIRE="$(build_ref old "$OLD_REF" | tail -n 1)"
fi
if [[ -z "$NEW_ELMFIRE" ]]; then
    NEW_ELMFIRE="$(build_ref new "$NEW_REF" | tail -n 1)"
fi

[[ -x "$OLD_ELMFIRE" ]] || { echo "ERROR: OLD_ELMFIRE not executable: $OLD_ELMFIRE" >&2; exit 1; }
[[ -x "$NEW_ELMFIRE" ]] || { echo "ERROR: NEW_ELMFIRE not executable: $NEW_ELMFIRE" >&2; exit 1; }

cat > "$WORK_ROOT/builds.txt" <<EOF
OLD_REF=$OLD_REF
OLD_ELMFIRE=$OLD_ELMFIRE
NEW_REF=$NEW_REF
NEW_ELMFIRE=$NEW_ELMFIRE
EOF

run_suite() {
    local exe="$1"
    local profile="$2"
    local mode="$3"
    local root="$4"
    local debug="${5:-0}"

    echo
    echo "################################################################"
    echo "SUITE: $(basename "$root")"
    echo "exe=$exe"
    echo "profile=$profile mode=$mode debug=$debug"
    echo "################################################################"

    ELMFIRE_BIN="$exe" \
    PROFILE="$profile" \
    MODE="$mode" \
    DEBUG="$debug" \
    TIMINGS=0 \
    N_ITERS="$N_ITERS" \
    SOURCE_ROOT="$SOURCE_ROOT" \
    RUN_ROOT="$root" \
    PYTHON="$PYTHON" \
        "$HARNESS_DIR/runme.sh"
}

for MODE in $TEST_MODES; do
    run_suite "$OLD_ELMFIRE" legacy  "$MODE" "$WORK_ROOT/old_legacy_mode${MODE}" 0
    run_suite "$NEW_ELMFIRE" legacy  "$MODE" "$WORK_ROOT/new_legacy_mode${MODE}" 0
    run_suite "$NEW_ELMFIRE" dynamic "$MODE" "$WORK_ROOT/new_dynamic_mode${MODE}" 0
done

# Generate detailed new-model CFFDRS diagnostics separately so runtime comparisons
# above are not distorted by the extra diagnostic GeoTIFF writes.
if [[ "$RUN_DEBUG_SUITE" == "1" ]]; then
    run_suite "$NEW_ELMFIRE" dynamic 2 "$WORK_ROOT/new_dynamic_mode2_debug" 1
fi

# Multi-rank correctness smoke test for the new shared-memory / parallel-I/O path.
# This is not a performance benchmark: the 100x100 single-band verification case
# is intentionally small. It checks that np=1/2/4 produce the same Mode-2 fields.
if [[ "$RUN_MPI_SMOKE" == "1" ]]; then
    if ! command -v "$MPIEXEC" >/dev/null 2>&1; then
        echo "WARNING: $MPIEXEC not found; skipping MPI smoke test" >&2
    else
        rm -rf "$WORK_ROOT/mpi_smoke"
        mkdir -p "$WORK_ROOT/mpi_smoke"
        for NP in $MPI_PROCS; do
            ROOT="$WORK_ROOT/mpi_smoke/np${NP}"
            "$PYTHON" "$HARNESS_DIR/setup.py" \
                --root "$SOURCE_ROOT" \
                --output-root "$ROOT" \
                --profile dynamic \
                --mode 2 \
                --n-iters 1 \
                --clean

            printf 'case,wall_s,user_s,sys_s,max_rss_kb,exit_code\n' > "$ROOT/timings_external.csv"
            LOG="$ROOT/01/outputs/elmfire_01.log"
            TIMEFILE="$ROOT/01/outputs/time_01.csv"

            echo "MPI smoke: np=$NP"
            set +e
            (
                cd "$ROOT/01"
                if [[ -x /usr/bin/time ]]; then
                    # shellcheck disable=SC2086
                    /usr/bin/time -f '%e,%U,%S,%M' -o "$TIMEFILE" \
                        "$MPIEXEC" $MPIEXEC_ARGS -np "$NP" "$NEW_ELMFIRE" 01.data > "$LOG" 2>&1
                else
                    # shellcheck disable=SC2086
                    "$MPIEXEC" $MPIEXEC_ARGS -np "$NP" "$NEW_ELMFIRE" 01.data > "$LOG" 2>&1
                fi
            )
            RC=$?
            set -e
            if [[ -s "$TIMEFILE" ]]; then
                IFS=, read -r WALL USER SYS RSS < "$TIMEFILE"
            else
                WALL=NA; USER=NA; SYS=NA; RSS=NA
            fi
            printf '01,%s,%s,%s,%s,%s\n' "$WALL" "$USER" "$SYS" "$RSS" "$RC" >> "$ROOT/timings_external.csv"
            if [[ "$RC" -ne 0 ]] || ! grep -q 'End of simulation reached successfully' "$LOG"; then
                echo "ERROR: MPI smoke np=$NP failed; see $LOG" >&2
                exit 1
            fi
        done
    fi
fi

"$PYTHON" "$HARNESS_DIR/compare_fbp_models.py" --work-root "$WORK_ROOT"

echo
echo "Comparison complete."
echo "Results: $WORK_ROOT/comparison"
