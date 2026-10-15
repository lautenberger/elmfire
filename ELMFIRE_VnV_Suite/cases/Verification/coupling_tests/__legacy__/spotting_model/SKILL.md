---
name: spotting-suite
description: Build, update, diagnose, execute, and document independent ELMFIRE Eulerian spotting verification cases. Use whenever creating, generating, revising, polishing, or reviewing reports under cases/Verification/coupling_tests/spotting_model, and for case preprocessing, rasters, parametric variants, current namelists, buffered domains, execution, postprocessing, quantitative pass/fail metrics, input-layout and whole-domain result visualizations, reference-aligned PDF figures, and standalone LaTeX reports.
---

# ELMFIRE Spotting Verification Suite

Build one verification case at a time. Make each case independently runnable and scientifically auditable.

## Use canonical case identities

Assign every case one stable suite number and one short purpose abbreviation. Name the directory and machine-readable case ID `CASE##_<PURPOSE>`, where `##` is a zero-padded two-digit number and `<PURPOSE>` is a concise uppercase abbreviation containing only letters and digits. Examples are `CASE01_BET` for biomass emission time and `CASE05_TDW` for time-dependent wind. Do not encode a thesis chapter, test number, figure number, journal source, author, or publication date in directory names, IDs, variant names, figure filenames, script messages, or report titles.

Define report titles as `Case ##: <descriptive purpose>`. A title must state the verified behavior, not its publication origin. Cite source literature only in the report main body when it supports a formulation, parameter, or reference result. Preserve the canonical number when a case is revised or moved. Maintain a suite-level case registry mapping canonical IDs to purposes and source coverage.

## Establish the case contract

Before generating inputs or running ELMFIRE, define the verification decision contract. Specify the observable, reference value or solution, comparison region, aggregation rule, required output completeness, numerical tolerances, and Boolean logic that determines the overall result. Every evaluable case must finish with status `pass` or `fail`; missing or unusable output must use an explicit non-passing status such as `not_run`, `incomplete`, or `insufficient_output`. Never infer a pass merely because a script completed.

1. Read the user's requested case and its existing `case.yaml`, `case.json`, scripts, namelist, and report.
2. Read the relevant model description or source material when supplied, but express the finished report as a self-contained verification argument.
3. State the isolated behavior, controlled variables, parameter sweep, analytical or numerical reference, units, comparison region, and tolerance.
4. Separate these questions:
   - Is the configuration exercising the intended ELMFIRE model?
   - Is the observable normalized consistently across resolutions?
   - Does the current source implement the assumed physics?
5. Do not modify other cases unless explicitly requested.

## Keep every case independent

Use this minimum layout:

```text
CASE##_PURPOSE/
├── scripts/
│   ├── preprocess.py
│   └── postprocess.py
├── report/
│   ├── case_report.tex
│   ├── case_metadata.tex
│   ├── case_body.tex
│   ├── case_macros.tex
│   ├── technical_macros.tex
│   └── metrics_macros.tex
├── elmfire.data.in
├── run_case.sh
├── case.json
└── case.yaml
```

Do not import a suite-level common utility such as `thesis_case_common.py`. Put required raster writing, namelist editing, validation, metrics, and figure logic in the case scripts. Prefer the standard library, NumPy, and GDAL already required by ELMFIRE workflows.

Each case must generate a standalone, self-contained report. Keep all report
source, case macros, generated metric macros, and required figures within that
case directory. A case report must not `\input`, `\include`, copy at build
time, or otherwise depend on suite-level report content such as
`cases/Verification/coupling_tests/spotting_model/common_report_content.tex`.
Shared material may
be used while authoring, but the finished case must contain its own complete
technical description and compile when the rest of the spotting suite is not
available.

Treat these as generated:

- `data/inputs/`
- `variants/`
- `outputs/`
- `logs/`
- `figures/`
- `report/metrics_macros.tex`
- LaTeX scratch files

Preserve source files and generated PDF documents according to the suite `.gitignore`.

## Verify the current ELMFIRE interface

Before editing `elmfire.data.in`, inspect the current source rather than copying an old case blindly:

- `build/source/elmfire_namelists.f90` for accepted keys and defaults.
- `build/source/elmfire_init.f90` for supported value combinations and validation.
- `build/source/elmfire_vars.f90` for units and state.
- Spotting, level-set, and I/O routines for the exercised calculation and output semantics.

Confirm at least:

- `USE_SUPERSEDED_SPOTTING=.FALSE.`
- intended `GENERATION_MODEL`
- intended `SPOTTING_DISTANCE_MODEL`
- intended `ACCUMULATION_MODEL`
- intended `IGNITION_MODEL`
- whether crosswind distribution, consumption, and secondary ignition must be enabled
- output flags needed by the postprocessor
- simulation duration, output time, and timestep convention

Do not treat an MPI-finalization message as the root cause. Search earlier stderr for a Fortran bounds error, missing raster, invalid namelist, or other first failure.

## Build preprocessing carefully

### Expose preprocessing parameters first

Every `scripts/preprocess.py` must place a clearly labelled customizable
parameter block immediately after its imports and before helper functions or
execution logic. Collect all values a case author may reasonably tune there,
including domain dimensions and resolution, buffer width, projection, nodata,
ignition placement/value, fuel models, weather, moisture, building geometry,
variant lists, and simulation controls used to generate namelists.

Do not hide customizable scientific or numerical values as literals inside
functions, loops, raster definitions, or namelist substitutions. Derived
quantities may remain near their use when they are calculated solely from
top-level parameters. If `case.json` supplies a value, expose an optional
top-level override or clearly name the configuration field in the parameter
block so users can customize the preprocessor without searching its body.

### Document generated code

Generated scripts are part of the verification record and must be understandable
without reading the generator or another case.

For every generated Python or shell script:

- start with a concise module/header comment that states the script purpose,
  inputs, generated outputs, and whether it runs ELMFIRE
- document physical units and array/index conventions near their definitions
- explain the two-cell buffer, usable-domain bounds, ignition placement, and
  geotransform origin in preprocessing code
- add docstrings to reusable functions and describe parameters or return values
  when their meaning is not evident from the signature
- comment non-obvious namelist substitutions, unit conversions, normalization,
  tolerance calculations, file-selection rules, and stale-output checks
- divide long scripts into short, named sections so a reader can follow the
  prepare, validate, calculate, and write-artifact phases
- explain why a scientific or numerical choice is made; do not add comments
  that merely restate the next Python statement

Keep comments synchronized with the code whenever a case is regenerated.
Treat misleading comments as validation failures.


### Preserve the physical domain

Define physical dimensions independently from raster dimensions. ELMFIRE uses a two-cell buffer on all four raster boundaries for these cases:

```text
nx = physical_nx + 2 * buffer_cells
ny = physical_ny + 2 * buffer_cells
buffer_cells = 2
```

Keep the same physical extent across resolution variants. Place the buffer outside that extent through the geotransform; do not shrink the physical verification domain to make room for it.

Never place the initial ignition in a buffer cell or on a raster boundary. Express ignition placement relative to the first usable cell.

### Generate deterministic rasters

Write every required input explicitly with:

- identical shape and geotransform
- intended GDAL datatype
- explicit nodata value
- documented units
- deterministic values

Check that fuel, adjustment, moisture, terrain, wind, and ignition rasters overlap exactly. For time-dependent weather, verify band count and `DT_METEOROLOGY`.

### Generate parametric variants

For a parameter study:

1. Generate a directory per variant.
2. Write a complete local namelist and local inputs.
3. Write `variants/manifest.json` containing names, paths, grid sizes, cell size, buffer size, physical extent, varied parameters, and configured units.
4. Remove or reject stale scratch and outputs whose dimensions do not match the manifest.

When replacing multiple namelist values, apply every replacement to the already-updated text. Never restart a later replacement from the original template.

### Scale extensive and intensive quantities correctly

Derive scaling from the ELMFIRE source and the reference configuration. For a one-dimensional test representing a 1 m cross-stream strip:

```text
GR_1m = 33.3 pcs/s/MW
EMBER_GR_PER_MW_VEGE = GR_1m / dy
```

ELMFIRE applies the configured generation rate to pixel fire power:

```text
pixel power = FLIN * dy
pixel generation rate = pixel power * GR_ELMFIRE
```

Therefore `GR_ELMFIRE * dy` must remain constant when cell size changes. Use `GR` for generation-rate notation and `pcs/s/MW` for its units.

Apply the same dimensional analysis to all resolution-dependent quantities. Record configured and effective values in the manifest and metrics.

## Make execution reproducible

Assume the user configures executable paths before invoking a case. Use `ELMFIRE_BIN` as the only ELMFIRE executable interface and `PYTHON_BIN="${PYTHON_BIN:-python3}"` for Python. Do not search `PATH`, select repository builds, accept executable command-line options, or duplicate environment configuration in each runner. Let a missing executable fail at the simulation command rather than surrounding the workflow with discovery guards.

Keep `run_case.sh` limited to the scientific workflow and necessary file operations:

1. resolve the case directory from the script location
2. run `scripts/preprocess.py`
3. run the generated ELMFIRE configuration or runnable manifest variants sequentially
4. capture variant-local stdout and stderr and write completion fingerprints when postprocessing requires them
5. run `scripts/postprocess.py`
6. compile `report/case_report.tex`

Use clear stage comments and explain manifest iteration, output cleanup, log locations, and fingerprint writes. Do not add help text, argument parsing, prepare-only modes, resume modes, timeout wrappers, executable discovery, duplicate manifest validation, optional report-compilation guards, or unrelated diagnostics. Capability-only cases with no admissible ELMFIRE configuration may omit stage 3 but must explain why in the runner header and postprocess to an explicit non-passing state. Long cases remain ordinary executable workflows; decide whether to launch them outside the runner.

Remove only known generated output files immediately before a simulation. Stop on the first failing ELMFIRE command through `set -euo pipefail`; keep the captured stderr path obvious from the directory layout.

## Postprocess from actual simulation outputs

Treat postprocessing as part of the verification, not just plotting.

Explicitly calculate every acceptance metric from the generated ELMFIRE outputs. Write the observed values, their limits, component pass/fail checks, and an overall `verification_passed` decision to `outputs/metrics.json`; expose the same fields through `report/metrics_macros.tex`. The overall decision must implement the predeclared Boolean logic and must be false whenever a required component fails.

### Validate inputs

For every required raster:

- reject missing files
- reject stale shapes
- verify cell size against the manifest
- mask nodata
- distinguish transient and final dumps
- record the exact selected filename

Exclude the two-cell numerical buffer from physical profiles and statistics.

### Visualize the spatial configuration

Generate configuration figures from the prepared case inputs whenever spatial
layout is relevant to understanding or auditing the test. Require a
whole-domain configuration view when an input is nonuniform or the experiment
depends on a localized feature, such as a single emitting cell, ignition line,
fuel break, structure footprint, source mask, or spatially varying weather.
Choose the input fields that expose the tested mechanism; examples are fuel or
structure maps, ignition and firebrand-source masks, terrain, and wind fields.
The examples are not an exhaustive checklist.

Show the complete physical domain, excluding the numerical buffer or clearly
distinguishing it from usable cells. Label coordinates and units, include an
appropriate legend or colorbar, and annotate ignition or emission locations,
wind direction, and other features needed to interpret the setup. Add a zoomed
panel only as a supplement to the whole-domain view. For a genuinely uniform
case with no important spatial feature, omit a redundant map and document the
uniform configuration in the parameter table or a compact schematic instead.

### Calculate references from simulated state

When the reference depends on fire behavior, calculate it from the actual output rather than a nominal input. For the biomass-emission case:

```text
FLIN: flin_*_*.tif, kW/m
VS:   vs_*_*.tif, ft/min when SPREAD_RATE_IN_M is false

reference density =
    GR_1m * (FLIN / 1000) / (VS * 0.3048 / 60)
```

Select a physically justified cell, such as the fully developed head-fire cell, and read FLIN and VS at the same location. Store the location, converted values, formula, and result in `outputs/metrics.json`.

For a 1 m-wide representation, convert an accumulated pixel count to areal density with:

```text
density [pcs/m^2] = pixel_count / (dx * 1 m)
```

Do not label this quantity `pcs/m`.

### Detect hidden multidimensional artifacts

Do not validate only the row or column most likely to pass. Inspect the full output map, row/column totals, symmetry, conservation, and neighboring profiles.

For example, disabling crosswind dispersion prevents lateral spreading from each source trajectory; it does not prevent a two-dimensional fire from creating sources in multiple rows. Distinguish expected source geometry from a source-code defect. Check discontinuities near grid axes and trace suspicious behavior to the source before changing the test.

### Emit durable artifacts

Write:

- `outputs/metrics.json` with per-variant values and overall status
- `report/metrics_macros.tex` with escaped LaTeX values
- a vector PDF figure compatible with `pdflatex`

Generate plots with Matplotlib by default, using a noninteractive backend for unattended execution, and save report figures as vector PDF files. Use another plotting approach only when using Matplotlib would require a disproportionate detour; document that exception in the handoff. Do not generate SVG unless the user explicitly requests it. Include x tick labels on at least the lowest shared-axis panel and put physical units on every plotted quantity.

Align every colorbar with the rendered two-dimensional map that it describes.
For a vertical colorbar, make its top and bottom coincide with the map axes; for
a horizontal colorbar, make its left and right edges coincide. Do not size a
colorbar from the larger subplot slot when a fixed or equal data aspect shrinks
the map inside that slot. Prefer an explicitly attached colorbar axis, such as
one created with Matplotlib's axes-divider utilities, or compute an explicit
colorbar axis from the map's final position. Apply the same rule to individual
and shared colorbars, and verify the alignment in the generated PDF.

Keep the colorbar ticks and label fully legible without colliding with adjacent
panels or axis labels. Move the colorbar below the map and orient it horizontally
when a short map cannot accommodate a readable vertical scale. Size the figure
canvas from the rendered map aspect ratio, use compact layout spacing, and crop
the saved PDF to its content so large unused top, bottom, or side margins do not
remain. Preserve enough padding that titles, ticks, labels, and annotations are
not clipped.

For every completed, runnable case, generate at least one representative
whole-domain result from actual ELMFIRE output in addition to higher-level
profiles, convergence curves, tables, or scalar summaries. Select the spatial
observable that best demonstrates the tested behavior, such as time of arrival,
final fire extent, accumulated firebrand density, ember ignition, structure
ignition, or another scientifically relevant field. These examples are neither
mandatory nor exhaustive. State the selected variant, ensemble statistic or
member, and output time; mask nodata; use the correct normalization and units;
and distinguish instantaneous, interval-integrated, and cumulative quantities.
Show the complete physical domain, excluding the buffer or identifying it
unambiguously. Use comparable scales for panels intended for comparison.

Do not manufacture a spatial result when a run is missing. For a not-run,
incomplete, or capability-only case, state why no whole-domain result is
available, retain the configuration visualization when useful, and keep the
verification status non-passing. If the model exposes no suitable raster for a
completed case, document that limitation and provide the closest domain-wide
diagnostic supported by actual outputs.

## Write a self-contained report

Assume the reader knows ELMFIRE but has not read the original research document.

Use the same LaTeX project structure in every case. `case_report.tex` must be the suite-standard wrapper and preamble; it loads `case_metadata.tex`, `case_macros.tex`, `technical_macros.tex`, generated `metrics_macros.tex`, and then `case_body.tex`. Keep all six source/generated files present in every report directory, using intentionally empty macro files when a case needs no case-specific definitions. Store `CaseNumber`, `CaseID`, `CaseTitle`, and `PurposeAbbreviation` only in `case_metadata.tex`.

Compile from the case report directory and reject any report source
that reaches outside the case directory. Local `case_macros.tex`,
`technical_macros.tex`, and generated `metrics_macros.tex` inputs are allowed;
suite-level TeX inputs are not.

Use the following exact top-level section names and order whenever generating, updating, polishing, or reviewing a verification report. Use numbered `\section{...}` commands; do not combine sections or use publication-specific headings. Do not reorder the report around file-generation chronology. Build a scientific verification argument in which each section supplies the premise for the next.

### 1. Verification purpose

Explain the physical, mathematical, or numerical behavior being verified; why it matters to spotting; the exact ELMFIRE implementation path under examination; likely defects the test can expose; and what is controlled, varied, enabled, or disabled. Distinguish implementation verification from physical validation. End with one precise verification question that names the observable, tolerance, and required variants or parameter range.

### 2. Mathematical formulation and numerical implementation

Present the governing equations, assumptions, variables, and units. Connect them to the active namelist selectors, input rasters, source modules and routines, discrete state updates, timestep and grid dependence, and output fields. Include pseudocode or an algorithm summary when equations alone do not expose the implementation logic. Do not merely list source filenames.

### 3. Derivation of expected behavior

Derive the expectation for the configured experiment, not only for the general model. State the analytical or manufactured solution, conservation result, limiting behavior, convergence trend, expected profile or total, parameter dependence, invariances, and approximations as applicable. If the reference depends on simulated state such as FLIN or VS, explain how the run-derived quantities enter the reference.

### 4. Verification metrics and acceptance criteria

Define metrics only after deriving the expected behavior. Trace every metric to one expected property. For each metric provide its formula, source output, comparison region, masks and buffer exclusion, temporal selection, aggregation, completeness requirement, tolerance with justification, and component decision. State the complete Boolean acceptance rule. Missing, stale, or insufficient outputs must be non-passing and not evaluable; they must never be treated as a pass.

### 5. Simulation configuration and predicted outcome

Document enough configuration to reproduce the test: domain and coordinate system, physical and buffer cells, resolution, timestep, ignition, fuels, terrain, moisture, weather, spotting selectors, coupled or disabled processes, parameter sweep, duration, and output cadence. Use a table with columns for parameter, configured value, units, and role in isolating the mechanism. Include a whole-domain input-configuration figure when a field is nonuniform or a localized or otherwise special spatial feature controls the experiment. Identify fuel regions, ignition and emission cells, structures, wind direction, or other relevant features directly in the figure and narrative. Follow the configuration material with a predicted-results subsection stating, before actual results are shown, the expected appearance and values of every plotted curve, map, or scalar.

### 6. Actual simulation results

Use outputs from an actual ELMFIRE run. Record the executable or source revision when available, manifest, selected filenames, completed and required variants, and failed or incomplete runs. For every completed runnable case, include at least one representative result over the entire physical simulation domain before or alongside higher-level summarized results. Choose the domain field that most directly reveals the verified mechanism rather than requiring a particular raster type. Use figures analogous to the corresponding thesis figures when scientifically appropriate, without copying thesis artwork. Possible panels include analytical and simulated curves on common axes, accumulation or ignition maps, arrival-time fields and profiles, final fire extent, leading-edge trajectories, instantaneous and averaged ROS, convergence sweeps, probability responses, acceptance bands, and representative intermediate extraction steps. Every figure must identify actual simulation data, references, units, variants, output time or ensemble statistic, and acceptance limits where applicable. Never substitute a synthetic curve for missing model output, and never use a cropped detail as the only spatial result.

### 7. Calculated metrics and verification decision

Present one results-table row per acceptance component with columns for metric, expected value or limit, calculated value, and component status. State the overall result prominently as PASS, FAIL, or NOT EVALUABLE. Source this decision directly from postprocessing artifacts such as `outputs/metrics.json`; do not recompute or override it in LaTeX.

### 8. Technical interpretation

Explain why the result passed, failed, or could not be evaluated. For discrepancies distinguish among source-code defects, incorrect configuration, inadequate resolution, statistical variability, output selection, postprocessing errors, reference limitations, and missing output. Relate the evidence to the derived expectation rather than merely describing the plot or repeating the status.

### 9. Scope and limitations

Conclude with what was demonstrated, what remains unverified, assumptions restricting the conclusion, additional variants or observables that may be needed, and whether the result supplies code verification only or any limited physical-validation evidence.

Maintain this narrative progression:

```text
intention -> formulation -> expected behavior -> metrics -> configuration
          -> predicted outcome -> actual results -> decision -> interpretation
```

Avoid unexplained references such as “Equation 3.11” or “the thesis value.” Derive the equation in the report. Do not retain a fixed nominal number when FLIN or VS varies per case.

Embed the generated PDF figure using `graphicx`, constrain it with `width`, `height`, and `keepaspectratio`, give it a descriptive caption and label, and reference it in the narrative.

Escape underscores in case IDs and generated macros.

Require every `\Metric{...}` key used by the report to exist in the generated
`metrics_macros.tex`. Define the report accessor with an `\ifcsname` guard so
a missing key renders a conspicuous `MISSING` marker instead of an empty table
cell, and treat any such marker as a report validation failure.

## Validate in order

Run the strongest affordable checks:

```text
1. python3 -m py_compile scripts/preprocess.py scripts/postprocess.py
2. python3 scripts/preprocess.py
3. inspect manifest and raster shape/geotransform/value ranges
4. run_case.sh or representative variants
5. python3 scripts/postprocess.py
6. inspect metrics.json, input-layout figures, whole-domain result coverage, and generated PDF metadata
7. latexmk -pdf -interaction=nonstopmode -halt-on-error -file-line-error case_report.tex
8. search the LaTeX log for fatal errors, undefined references, and overfull boxes
```

Confirm that rerunning preprocessing and postprocessing is idempotent and does not reuse incompatible outputs.

Inspect each required spatial figure rather than relying on file existence.
Confirm that it covers the intended physical extent, treats the buffer and
nodata correctly, uses meaningful units and normalization, identifies the
variant and time or ensemble statistic, and makes localized input features
visible. Confirm that every map colorbar terminates at the corresponding map
edges rather than the surrounding subplot edges, has readable ticks and labels,
and does not create a large blank canvas margin. Reject a report that contains
only cropped maps or summarized curves when a completed case has domain-scale
outputs available.

Report numerical results, skipped checks, and remaining modeling assumptions. A plot that looks plausible is not sufficient evidence of a passing case.
