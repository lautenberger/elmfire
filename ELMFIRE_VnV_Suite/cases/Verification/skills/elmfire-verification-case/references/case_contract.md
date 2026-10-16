# Case contract and file layout

Every case includes the identity, runner, scripts, and six report source files
shown below. Add only the optional data and artifact directories meaningful to
the case.

```text
CASE##_PURPOSE/
├── case.yaml
├── case.json                  # when required by case tooling
├── elmfire.data.in
├── run_case.sh
├── compile_case.sh
├── data/
│   ├── inputs/
│   ├── misc/
│   ├── weather/
│   └── observations/
├── scripts/
│   ├── preprocess.py
│   ├── postprocess.py
│   └── metrics_to_macro.py
├── outputs/metrics.json
├── figures/
├── logs/
└── report/
    ├── case_report.tex
    ├── case_metadata.tex
    ├── case_body.tex
    ├── case_macros.tex
    ├── technical_macros.tex
    └── metrics_macros.tex
```

`preprocess.py` may be named `generate_inputs.py` when that is clearer. A capability-only case may omit `elmfire.data.in` or the ELMFIRE stage when the omission is explicit in metadata and the verification contract.

## Metadata

`case.yaml` is the discovery-facing description. Keep paths relative to the case root and include the canonical `case_id`, descriptive title, ELMFIRE configuration, variants, expected figures, and metrics path as applicable. `case.json`, when present, records the same identity and machine-readable verification contract. Conflicting metadata is an error.

For a case that executes ELMFIRE, include the compact `namelist_contract`
defined in `docs/namelist-versioning.md`. It contains only exact,
machine-checkable values essential to the test. The report remains the sole
home for scientific intent and reasoning. Do not add `scientific_intent.yaml`
unless the contract later becomes too large for `case.yaml` or must be reused
independently of case metadata.

## Input generation

Generate deterministic rasters and namelists locally. Explain grid origin, resolution, row direction, geotransform, projection, units, nodata handling, ignition placement, and buffer cells wherever applicable. Record enough metadata to reconstruct the run. Preprocessing or an immediately invoked case-local evidence step must also generate a report-ready whole-domain input-configuration figure from those prepared artifacts. At minimum it must visualize PHI or ignition and one field that controls the experiment. It must remain generatable before ELMFIRE outputs exist, identify the prescribed simulation condition and scientific meaning and construction of the input fields without displaying filenames, and never read or alter a sibling case or invent a substitute field.

### Generated variant ownership

The `variants/` directory is disposable generated output. Durable variant definitions belong in `scripts/preprocess.py`, case-root inputs, or case-local tables and templates outside `variants/`.

- Never repair a case by editing, deleting, or selectively preserving generated variant files.
- Trace every generated file back to its producer and correct that producer.
- Preprocessing must reproduce the entire intended variant set when `variants/` is absent or empty.
- A clean regeneration must not depend on outputs from a previous run.
- Keep variant labels and ordering deterministic so evidence maps to the same configuration on every regeneration.

### Terminal-time generation

Apply these rules when verification depends on a final raster or other output selected at `SIMULATION_TSTOP`:

1. Choose a finite, positive timestep that satisfies the case CFL, stability, and accuracy requirements.
2. Prefer a whole-second `SIMULATION_DT` when feasible, and set `SIMULATION_DTMAX` consistently.
3. Generate `SIMULATION_TSTOP` as an exact multiple of `SIMULATION_DT`. For a prescribed stop time, choose a conservative whole-second divisor of that duration when feasible.
4. Verify the invariant mechanically from the generated namelist rather than relying on formatted display values.
5. Do not force this policy onto timestep-convergence tests or physics that require fractional timesteps. In those cases, use an exactly representable aligned pair where possible and document the reason, selection rule, and evaluator consequences of any exception.

This alignment is preventive: it avoids shortened final steps interacting with ELMFIRE stalled-step final-dump behavior. It does not authorize relaxing the postprocessing evidence rules below.

## Postprocessing

Read only this case's outputs. Distinguish missing, empty, malformed, and valid outputs. Compute named metrics, compare them with explicit tolerances, write `outputs/metrics.json`, generate only case-local figures, and emit report macros mechanically.

### Terminal evidence selection

Use the regular path whenever possible:

- parse a unique configured `SIMULATION_TSTOP` from the generated namelist;
- require exactly one dump-times record representing the terminal output;
- require its recorded time to match `SIMULATION_TSTOP` within a small numerical tolerance;
- select each required terminal raster by its documented numeric stamp and require a unique match.

A compatibility path for an existing stalled-final dump is permitted only when all of the following are true:

1. `SIMULATION_TSTOP`, `SIMULATION_DT`, and `DT_METEOROLOGY` are each parsed uniquely from the same generated namelist.
2. Exactly one relevant dump-times file and exactly one final record are present.
3. The final recorded time is greater than `SIMULATION_TSTOP`.
4. Subtracting `DT_METEOROLOGY` from the final time yields a finite pre-jump solver time between zero and `SIMULATION_TSTOP`, inclusive within numerical tolerance, and that time lies on the configured `SIMULATION_DT` grid.
5. For a propagating variant, the pre-jump time is within at most one `SIMULATION_DT` of `SIMULATION_TSTOP`. An earlier stall is acceptable only when an independent oracle explicitly predicts no propagation and that expectation is passed to the selector as a dedicated condition. Do not infer it from optional TOA, missing outputs, or observed stalling.
6. Every required raster has exactly one matching artifact. A wildcard or an asterisk-filled timestamp may be considered only inside this compatibility path, never as a general fallback.

Treat missing, duplicate, malformed, or ambiguous evidence as `NOT EVALUABLE`. Do not infer success from process exit status, accept arbitrary overrun timestamps, choose the first wildcard match, or copy metrics from another run. Record which path selected the terminal evidence so the report remains auditable.

## Clean regeneration validation

Before acceptance, test regeneration from a clean state. For generated-variant cases, use an isolated temporary copy: remove the copied `variants/` tree, run preprocessing, and confirm that the intended labels, inputs, and manifest are recreated. Do not hand-repair the source `variants/` tree during validation.

When terminal-time alignment applies, parse every generated namelist and assert finite positive values, whole-second timesteps where required, exact stop-time divisibility, and the applicable CFL or stability bound. Confirm that the input-configuration generator succeeds from the cleanly prepared inputs alone and that the report references its nonempty case-local artifact. Then run the case end to end and reconcile required, completed, and missing variant counts.

If the postprocessor includes stalled-final compatibility, exercise these outcomes: a normal terminal dump is accepted; a timestep-aligned near-stop stall is accepted; an early stall is accepted only for an independently predicted no-propagation variant; the same early stall is rejected for a propagating variant; and incomplete, duplicated, off-grid, or otherwise ambiguous evidence is rejected as `NOT EVALUABLE`. Confirm that validation leaves no unintended changes under the source case `variants/` directory.

## Runner

Prefer a short, linear runner:

```bash
#!/usr/bin/env bash
set -euo pipefail

CASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_BIN="${PYTHON_BIN:-python3}"
ELMFIRE_BIN="${ELMFIRE_BIN:-elmfire}"

mkdir -p "$CASE_DIR/outputs" "$CASE_DIR/figures" "$CASE_DIR/logs"
"$PYTHON_BIN" "$CASE_DIR/scripts/preprocess.py"
(cd "$CASE_DIR" && "$ELMFIRE_BIN" elmfire.data.in > logs/elmfire.log 2>&1)
"$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
"$CASE_DIR/compile_case.sh"
```

Add explicit variant invocations when the contract requires them. Finish by invoking `compile_case.sh`. The runner coordinates; scientific logic belongs in readable Python scripts and ELMFIRE configuration.

## Presentation boundary

Follow [report_language.md](report_language.md). The filenames, metadata keys, and
implementation details in this construction reference are operational only.
Describe their scientific meaning in reports; never display these identifiers.
