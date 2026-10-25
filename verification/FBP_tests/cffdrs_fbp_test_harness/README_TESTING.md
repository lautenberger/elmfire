# ELMFIRE Canadian CFFDRS/FBP comparison harness

This harness is intended to compare the prior CFFDRS/FBP implementation with the updated implementation without conflating algorithm changes with the new input plumbing.

## Files

- `elmfire_init.f90`: updated current source with mandatory CFFDRS `CBH_FILENAME` validation removed. If no landscape or CBH raster is supplied, it warns and retains the existing fuel-model-table CBH fallback.
- `setup.py`: portable replacement for `verification/FBP_tests/setup.py`.
- `compare.py`: portable replacement for `verification/FBP_tests/compare.py`; explicitly distinguishes Mode 2 head-fire outputs from Mode 3 level-set outputs.
- `runme.sh`: portable single-executable FBP verification runner.
- `run_cffdrs_fbp_comparison.sh`: builds or accepts old/new executables, runs the comparison matrix, runs an MPI correctness smoke test, and invokes post-processing.
- `compare_fbp_models.py`: creates CSV summaries, pairwise raster metrics, diagnostic summaries, MPI reproducibility checks, plots, and `comparison_report.md`.
- `elmfire_legacy_mode2.data`, `elmfire_legacy_mode3.data`: cross-version legacy-input templates.
- `elmfire_latest_dynamic_mode2.data`, `elmfire_latest_dynamic_mode3.data`: latest-only BUI/GCF/m/s input templates.
- `decks/`: rendered `.data` files for all 20 existing FBP verification cases.

## Why the legacy deck explicitly sets `DEAD_MC_IN_PERCENT=.FALSE.`

The prior implementation defaults to percent dead-fuel moisture input, whereas the updated CFFDRS path treats M1 as a fraction. For an apples-to-apples test, `setup.py` writes M1/M10/M100 as fractions and the cross-version legacy deck explicitly sets `DEAD_MC_IN_PERCENT=.FALSE.`. This makes the physical M1 input identical in the old and new runs.

## Comparison matrix

For each of MODE=2 and MODE=3:

1. `old_legacy`: prior executable, legacy BUI/curing route.
2. `new_legacy`: updated executable, same physical legacy BUI/curing route.
3. `new_dynamic`: updated executable, explicit BUI raster + explicit GCF raster + equivalent m/s wind input.

Interpretation:

- `old_legacy` vs `new_legacy` isolates FBP/ELMFIRE implementation changes.
- `new_legacy` vs `new_dynamic` isolates the new BUI/GCF/wind input route.
- Each suite is also compared with the existing 20-case ROS/FI reference targets.
- MODE=2 is the cleaner equation/head-fire test; MODE=3 adds level-set propagation.

A separate `new_dynamic_mode2_debug` suite enables the expanded CFFDRS diagnostic rasters. It is excluded from runtime comparisons because the new model writes substantially more diagnostic output when `DUMP_CFFDRS_DEBUG=.TRUE.`.

## Portable single-version run

Place the harness files in `verification/FBP_tests`, or set `SOURCE_ROOT` to that directory.

```bash
ELMFIRE_BIN=/path/to/elmfire \
PROFILE=legacy \
MODE=3 \
./runme.sh
```

For the new dynamic input route:

```bash
ELMFIRE_BIN=/path/to/new/elmfire \
PROFILE=dynamic \
MODE=2 \
DEBUG=1 \
./runme.sh
```

## Full old/new comparison

From inside the ELMFIRE repository, with this harness in `verification/FBP_tests`:

```bash
./verification/FBP_tests/run_cffdrs_fbp_comparison.sh
```

Default git refs:

```text
OLD_REF=upstream/main
NEW_REF=updated_directory_structure_maria
```

The script creates detached git worktrees and builds both executables if `OLD_ELMFIRE` and `NEW_ELMFIRE` are not supplied. It does not fetch remotes automatically.

To compare already-built binaries instead:

```bash
OLD_ELMFIRE=/path/to/old/elmfire \
NEW_ELMFIRE=/path/to/new/elmfire \
./run_cffdrs_fbp_comparison.sh
```

Useful overrides:

```bash
N_ITERS=20
TEST_MODES="2 3"
RUN_DEBUG_SUITE=1
RUN_MPI_SMOKE=1
MPI_PROCS="1 2 4"
WORK_ROOT=/scratch/$USER/elmfire_fbp_compare
```

## Outputs

The comparison directory contains:

```text
model_case_summary.csv
pairwise_raster_metrics.csv
runtime_and_accuracy_summary.csv
new_dynamic_cffdrs_diagnostics.csv        # when debug suite is run
mpi_reproducibility.csv                   # when MPI smoke test is run
comparison_report.md
ros_target_comparison_mode2.png
fi_target_comparison_mode2.png
runtime_by_case_mode2.png
ros_target_comparison_mode3.png
fi_target_comparison_mode3.png
runtime_by_case_mode3.png
```

The small 100 x 100 MPI smoke test is a correctness/reproducibility check, not a scaling benchmark. Meaningful MPI performance testing should use Dogrib or a production-size multi-band Canada tile, ideally with enough weather bands to cross `WX_BANDS_KEPT_IN_MEM` and exercise rolling weather-window reloads.
