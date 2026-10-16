# ELMFIRE Verification and Validation Suite

The ELMFIRE Verification and Validation (V&V) Suite captures self-contained
scenarios that exercise targeted portions of the ELMFIRE wildfire spread model.
Cases are grouped first by whether they target verification or validation, and
then by verification type or validation scale (for example
`cases/Verification/coupling_tests/<case>/` or
`cases/Validation/landscape_scale/<case>/`).
Each case includes the inputs required to reproduce the simulation, a scripted
post-processing pipeline, and a LaTeX report that documents the expected
behaviour, results, and scientific decision criteria. The repository builds two
separate aggregate documents: an ELMFIRE Verification Summary Report and an
ELMFIRE Validation Summary Report. Each includes the applicable standalone case
reports and ends with a generated case-decision table.

---

## Repository layout

```text
ELMFIRE_VnV_Suite/
├── cases/                 # Category folders plus a reusable template
│   ├── Validation/
│   │   ├── landscape_scale/
│   │   │   └── <case>/     # Landscape-scale validation studies
│   │   └── structure_scale/
│   │       └── <case>/     # Structure-scale validation studies
│   ├── Verification/
│   │   ├── CASE_REGISTRY.md # Stable global verification identifiers
│   │   ├── unit_tests/
│   │   │   └── CASE##_<PURPOSE>/ # Isolated functionality and known-response tests
│   │   └── coupling_tests/
│   │       └── CASE##_<PURPOSE>/ # Coupled-component verification cases
│   └── case_template/      # Template used by tools/new_case.sh
│       ├── case.yaml       # Metadata and runtime settings for run_case.sh
│       ├── elmfire.data.in # Case-specific ELMFIRE configuration
│       ├── data/           # Optional raw input rasters or tables
│       ├── scripts/        # Post-processing utilities for this case
│       ├── figures/        # Auto-generated plots (outputs)
│       ├── outputs/        # Derived metrics (JSON, rasters, etc.)
│       ├── logs/           # Runtime logs captured by run_case.sh
│       └── report/         # LaTeX sources for the case report
├── common/                 # Shared resources (plot styling, latexmkrc)
├── main_report/            # Separate verification and validation summary reports
├── gcp_config/             # Dockerfile, Cloud Build + Batch templates for Google Cloud runs
├── tools/                  # Workflow helpers (create case, rebuild reports)
└── Makefile                # Convenience targets that wrap the scripts
```

---

## Prerequisites

### System packages

Install the following tools on the workstation or HPC login node where cases
will be prepared:

- **ELMFIRE binary**: a compiled executable matching the release you intend to
  verify. Store it in a shared location and/or expose it through the
  `ELMFIRE_BIN` environment variable for convenience.
- **GNU Make, Bash, Coreutils**: required for the helper scripts in `tools/` and
  the per-case `run_case.sh` pipelines (standard on Linux).
- **Python ≥ 3.9** with `pip`.
- **LaTeX**: install TeX Live with LuaLaTeX, `luaotfload`, `fontspec`, and `latexmk` (on Ubuntu, include `texlive-luatex`). Reports require genuine **Times New Roman** regular, bold, italic, and bold italic fonts; the Microsoft core-fonts package (`ttf-mscorefonts-installer`) supplies them. Verify with `fc-match "Times New Roman"`; a substitute font is not compliant.
- **GDAL/RasterIO dependencies** (e.g. `gdal-bin`, `libgdal-dev`) These are required by ELMFIRE and `rasterio` in some post-processing
  scripts.

### Python packages

Create and activate a virtual environment, then install the baseline Python
libraries used by the template and current cases:

```bash
python3 -m venv .venv
source .venv/bin/activate
pip install --upgrade pip
pip install numpy matplotlib rasterio
```

Individual cases may require additional dependencies—inspect
`cases/<case-path>/scripts/*.py` and install any extras (for example `scipy` or
`pandas`). Keep the virtual environment activated while running cases so the
correct packages and versions are available.

---

## Initial setup and environment configuration

1. **Clone the repository** and change into it:
   ```bash
   git clone https://github.com/berkeley-firelab/ELMFIRE_VnV_Suite.git
   cd ELMFIRE_VnV_Suite
   ```
2. **Point to the ELMFIRE executable**. Either set `ELMFIRE_BIN` globally:
   ```bash
   export ELMFIRE_BIN=/opt/elmfire/bin/elmfire_2025.0717
   ```
   or edit `cases/<case-path>/case.yaml` to reference the absolute path under the
   `elmfire.bin` key. Using the environment variable keeps the YAML portable.
3. **Configure the suite after cloning or copying it.** Run `make configure`
   to restore executable permissions on all shell scripts, including every
   `run_case.sh` and `compile_case.sh`. This is particularly important after a
   transfer that does not preserve Unix file modes. If ELMFIRE cannot resolve
   GDAL from the active environment, run
   `make configure PATH_TO_GDAL=/opt/my-gdal/bin`; this also updates the
   `PATH_TO_GDAL` namelist entry in all cases.
4. **Activate the Python virtual environment** prepared in the previous section
   before running any scripts.

---

## Running an existing case

Each case ships with a `run_case.sh` orchestrator that executes the simulation,
post-processes the results, and rebuilds the LaTeX report.

```bash
# From the repository root (specify the category + case ID):
make run CASE=Verification/coupling_tests/CASE19_WTH
# or
./cases/Verification/coupling_tests/CASE19_WTH/run_case.sh
```

The script performs the following steps:

1. Reads `case.yaml` to discover the ELMFIRE binary, input configuration, runtime
   guard (`runtime_limit_s`), and optional paths.
2. Creates `outputs/`, `figures/`, and `logs/` folders under the case directory.
3. Runs the ELMFIRE executable with the referenced `elmfire.data.in` file.
4. Launches the Python post-processing pipeline (`scripts/postprocess.py`). This
   script is expected to write plots into `figures/`, metrics into
   `outputs/metrics.json`, and any LaTeX fragments needed by the report.
5. (When present) converts metrics into reusable LaTeX macros, e.g.
   `scripts/metrics_to_macro.py` → `report/metrics_macros.tex`.
6. Builds the per-case LaTeX report (`report/case_report.tex` → PDF). Aggregate
   report inputs are refreshed later with `make reports`.

Inspect `logs/elmfire.stderr` if the simulation fails. Outputs are kept inside
`cases/<case-path>/` so they can be version-controlled when appropriate.

---

## Running multiple cases automatically

Use `tools/run_all.py` when you need to execute every case sequentially (or
shard the workload across multiple workers). The helper script discovers all
`run_case.sh` files under `cases/`, even when they live inside nested
category folders, skips the template, and honors optional
sharding environment variables so it can be reused on Slurm or in cloud
batch jobs. Suite selection is applied before sharding.

```bash
# Run every case sequentially on the local workstation
python3 tools/run_all.py

# Run only verification cases
python3 tools/run_all.py --suite verification

# Run only validation cases
python3 tools/run_all.py --suite validation

# Equivalent convenience aliases
python3 tools/run_all.py --verification-only
python3 tools/run_all.py --validation-only

# Show the planned commands without executing them
python3 tools/run_all.py --dry-run

# Submit through Slurm. Each case receives the matching header from common/.
python3 tools/run_all.py --suite verification --slurm
python3 tools/run_all.py --suite validation --slurm
```

Slurm resources and module setup are defined separately in
`common/slurm_verification_head.txt` and `common/slurm_validation_head.txt`.
The validation header allocates 50 MPI ranks—one per ensemble member—on one
Savio4 node. During wrapper generation, the generic header job name is replaced
by the canonical case ID, for example `elmfire-CASE08_FBC` or
`elmfire-camp_fire`. Because the log templates use Slurm's `%x` token, their
filenames contain the same case-specific job name. Before submission, export
`ELMFIRE_BIN` and `ELMFIRE_VNV_CONDA_ENV`; the latter must name the Conda
environment, or its absolute path, to activate on the compute node. The Slurm
headers fail before case execution if the environment is not exported or its
required dependencies are unavailable. The older `common/slurm_head.txt` is retained for
compatibility but is not used by `tools/run_all.py`.

`make clean` removes simulation/runtime products while preserving a complete
report-build snapshot. It empties active-case `logs/` and `scratch/`
directories and removes non-JSON simulation products from case `outputs/`.
Case figures, `outputs/*.json`, generated `metrics_macros.tex`, standalone case
PDFs, aggregate PDFs, and `main_report/generated/` are retained, so `make
reports` can rebuild both guides without rerunning ELMFIRE. Cleanup still
removes LaTeX auxiliary files, `run_case_slurm.sh` wrappers, and scheduler logs
named `slurm-*.stdout`, `slurm-*.stderr`, `slurm-*.out`, or `slurm-*.err`.

Every active verification `CASE*/variants/` tree is also a generated artifact:
`make clean` empties it, and the case preprocessor reconstructs all variant
namelists, deterministic inputs, and manifests before execution. Keep variant
definitions, special namelist templates, observations, and other source
material outside `variants/`; the GUIDE-derived cases use
`scripts/namelists/` when a variant differs from the root `elmfire.data.in`.

The same rules apply to CASE01--CASE14 as to every other active case. Cleanup
preserves case metadata, namelists, scripts, report source text, source inputs,
observations, and everything under archived `__legacy__` trees. Run
`python3 tools/clean_artifacts.py` without `--apply` to preview the exact scope.

Before starting a fresh suite evaluation, run `make prepare-run`. This performs
the normal cleanup and also invalidates the previous evaluation layer: all
active-case `outputs/` JSON, generated figures, `report/metrics_macros.tex`,
standalone `case_report.pdf` files, aggregate report PDFs, and
`main_report/generated/`. It preserves case definitions, scientific inputs,
observations, scripts, namelists, and authored report `.tex` sources. Preview
this broader cleanup with
`python3 tools/clean_artifacts.py --prepare-run` (without `--apply`).

Cloud environments set `CLOUD_RUN_TASK_*`, `BATCH_TASK_*`, or `TASK_COUNT`
automatically. You can also override the shard layout explicitly:

```bash
# Run only shard 1/4 locally
python3 tools/run_all.py --shard-index 1 --shard-count 4
```

The same selections are available through Make:

```bash
make run-verification
make run-validation
make run-all SUITE=verification
```

## Building the aggregate reports

Generate both report indexes and their status tables, then aggregate the current
standalone case PDFs with:

```bash
make reports
# or
./tools/build_all.sh
```

This produces:

- `main_report/verification_report.pdf` for cases under `cases/Verification/`;
- `main_report/validation_report.pdf` for cases under `cases/Validation/`.

Before generating the aggregate indexes, `tools/build_all.sh` compiles any
standalone `report/case_report.tex` whose `case_report.pdf` is missing. It does
not run simulations or postprocessing. If generated result graphics are absent,
the script retries the standalone report in LaTeX draft-graphics mode so the
case narrative remains available and the missing evidence stays visible.

The opening summaries are generated by `tools/generate_summary_reports.py`.
They record the aggregate-build environment, each case's declared executable,
namelist, MPI ranks and schema, and the decision read from
`outputs/metrics.json`. Case names link to their detailed report sections. A
successful shell command or LaTeX build is never treated as a scientific PASS.
Missing, incomplete, blocked, or unrecognized results are reported as `NOT
EVALUABLE`; validation results without a justified acceptance threshold may be
reported as `CHARACTERIZED`. Regenerable TeX inputs and machine-readable
summaries are written under `main_report/generated/` and are not source
artifacts.

---

## Using the repository skills

The skills provide instructions for AI-assisted case design, review, and
maintenance. They are not executable programs and do not replace the suite's
run and report-building commands.

| Skill | When to use it |
| --- | --- |
| [elmfire-verification-case](cases/Verification/skills/elmfire-verification-case/SKILL.md) | Create, extend, or review a self-contained verification case against an analytical solution, independent reference calculation, or known numerical response. Covers unit and coupling tests. |
| [elmfire-validation-case](cases/Validation/skills/elmfire-validation-case/SKILL.md) | Create, complete, or review a validation case against observations or experiments. Covers data provenance, configuration rationale, input statistics, comparisons, metrics, figures, and reports. |
| [elmfire-namelist-versioning](skills/elmfire-namelist-versioning/SKILL.md) | Check or adapt an existing case's namelist for another ELMFIRE revision while preserving the experiment and its evaluation criteria. |

The archived [spotting-suite skill](cases/Verification/coupling_tests/__legacy__/spotting_model/SKILL.md)
is a domain-specific historical reference. For current spotting-case work, use
`elmfire-verification-case` and consult the archived material only where relevant;
do not adopt its legacy locations or superseded instructions.

### Select a skill and describe the task

These skills are Markdown instructions that can be supplied to an AI assistant
or coding agent; using them does not require a platform-specific skill command.
Provide the instructions and supporting materials in a way the assistant can
access:

- **With repository access:** open this repository as the working folder,
  include the relative path to the desired `SKILL.md` in your request, and ask
  the assistant to read it completely together with its required references.
- **Without repository access:** attach or paste the skill and its required
  reference documents, along with the relevant case materials and ELMFIRE
  source excerpts. A local path alone does not give the assistant access to
  those files. Ask it to identify missing materials before proposing changes.

If your platform supports skill registration or selection, you may use that
facility, but do not assume it automatically discovers these nested folders.
The examples below use explicit file references; they are **chat prompts, not
shell commands**. Replace all angle-bracket placeholders with your actual
information, or identify the corresponding attachments when files are uploaded.
An assistant without file-editing or execution tools can propose changes and
commands for you to apply locally; it must distinguish proposed checks from
checks actually performed.

For new case design or namelist compatibility work, provide an **explicit
ELMFIRE source-root path and target revision**. An executable path alone does
not establish which equations, namelist entries, or defaults apply. Also supply:

- The scientific objective and the existing case or proposed case scope.
- Relevant papers, reference calculations, source datasets, and observations.
- Known domain, grid, time, ignition, fuel, weather, and model settings; identify
  quantities to vary and quantities to hold fixed.
- Proposed metrics and acceptance criteria, or the references from which they
  should be justified. Missing scientific choices should be raised for review,
  not invented.
- Whether the request is a proposal, implementation, scientific review, or
  report-only edit, and whether simulations or postprocessing may be run.

### Example: propose a verification case

```text
Read and follow cases/Verification/skills/elmfire-verification-case/SKILL.md
and its required references.
ELMFIRE source root: <absolute path to the source checkout>
Target revision: <release tag or commit>
Verification objective: <physical or numerical response to verify>
Reference material: <paper, analytical solution, or benchmark>
Prescribed and varied conditions: <settings and sweep values>
Propose the category, simulation design, expected response, metrics, and
acceptance criteria. Identify unsupported choices for my review. Consult the
verification case registry for the next unused CASE number; preserve existing
case identities. Do not create files or run simulations until I approve.
```

After approving the design, ask the assistant to implement it with the same
skill. It should produce a case with its own preprocessing, postprocessing,
namelist, simple execution script, metadata, and report, without depending on
another case's files or shared execution helpers.

### Example: complete a validation case

```text
Read and follow cases/Validation/skills/elmfire-validation-case/SKILL.md
and its required references.
Case: <existing case path or proposed event>
ELMFIRE source root: <absolute path to the source checkout>
Target revision: <release tag or commit>
Input and observation locations: <paths or dataset references>
Scientific references: <papers, thesis, or experimental documentation>
First review the available evidence and propose the remaining work. Explain
data provenance, ignition and model-setting choices, input statistics,
observation comparisons, metrics, and acceptance criteria. Ask me about
unsupported choices, including spotting or WUI parameters. Distinguish
calibration data from independent validation data. After approval, complete
the self-contained case scripts and report. Do not run ELMFIRE yet.
```

### Example: review compatibility with another ELMFIRE revision

```text
Read and follow skills/elmfire-namelist-versioning/SKILL.md and its required
references for <case path>.
Target ELMFIRE source root and revision: <absolute path> at <tag or commit>
Previous source root and revision, if available: <absolute path> at <revision>
Inspect the source-defined namelist entries and their scientific meaning.
Validate the existing case and propose only source-supported equivalent
changes. Write any migration candidate outside the canonical case for review;
do not overwrite its namelist or run simulations. Preserve the critical
conditions in case.yaml, all metrics, and all acceptance criteria. Report
changes that cannot preserve the experiment instead of applying them.
```

The supporting commands and review procedure are described in
[Namelist versioning](docs/namelist-versioning.md).

### Report-only work and review of the result

For language or formatting edits, use the corresponding verification or
validation skill and explicitly limit the request, for example:

```text
Use the verification-case skill at
cases/Verification/skills/elmfire-verification-case/SKILL.md to revise the
report for <case path>. This is a report-only edit: preserve scientific
meaning, equations, settings, numerical results, citations, and status
decisions. Define terms at first use and follow the common report format.
Do not run preprocessing, simulations, or numerical postprocessing. Compile
the existing report sources with LuaLaTeX and inspect the resulting PDF.
```

For all three skills, keep scientific intent and reasoning in the report and
only test-critical invariants in `case.yaml`; do not introduce a separate
`scientific_intent.yaml` unless the specification genuinely needs it. Review
the changed files, unresolved assumptions, and reported checks before running
or accepting a case. Successful execution or report compilation is not a
scientific pass: missing evidence remains `NOT EVALUABLE`, and validation
without justified acceptance thresholds may remain `CHARACTERIZED`.

---

## Creating a new verification case

1. **Bootstrap from the template** using the helper script or Makefile target:
   ```bash
   ./tools/new_case.sh Verification/coupling_tests/CASE30_EXM
   # or
   make new CASE=Verification/coupling_tests/CASE30_EXM
   ```
   Replace `Verification/coupling_tests/` with the appropriate group (e.g.,
   `Verification/unit_tests/` or `Validation/landscape_scale/`). The helper
   copies `cases/case_template/` into the requested case path and expands the
   `{{CASE_ID}}` tokens in
   the YAML and report macros. The helper does not allocate identifiers: consult
   `cases/Verification/CASE_REGISTRY.md` first and replace `CASE30_EXM` with the
   next unused number and an appropriate purpose abbreviation.

2. **Edit case metadata**:
   - `case.yaml` — update `case_title`, set path to the elmfire excutable (or rely on
     `ELMFIRE_BIN`), choose `elmfire.data.in`, and list any figures your
     post-processing will create.
   - `report/case_macros.tex` — fill in `\CaseTitle`, `\CaseOwner`,
     `\CaseVersion`, and `\CaseDate` so the report documents the targeted
     ELMFIRE release.

3. **Prepare the simulation inputs**:
   - Place the tailored `elmfire.data.in` and any required rasters or tables
     (or scripts for generating the input data) inside the case directory (`data/` is provided for convenience).
   - Document important parameters in `report/case_body.tex` under the
     “Simulation Setup” and “Assumptions” subsections.

4. **Implement post-processing** in `scripts/postprocess.py`:
   - Keep operational logic in the case; do not import a sibling or suite-level
     runtime helper.
   - Write figures to `figures/`, capture numerical metrics in a dictionary, and
     save them to `outputs/metrics.json`.
   - If you need LaTeX-ready macros, either extend the script to write them or
     add a helper like `metrics_to_macro.py` (see the
     `CASE19_WTH` case for an example).

5. **Draft the case report**:
   - Use `report/case_body.tex` to describe the problem, expected results, and
     acceptance criteria.
   - Reference generated figures via standard LaTeX commands. Additional macros
     can be created in `report/case_macros.tex`.

For new verification work, follow
`cases/Verification/skills/elmfire-verification-case/SKILL.md` and allocate the
next stable identifier in `cases/Verification/CASE_REGISTRY.md`.

When testing the suite against a different ELMFIRE revision, follow
[`docs/namelist-versioning.md`](docs/namelist-versioning.md) and the repository
skill at `skills/elmfire-namelist-versioning/SKILL.md`. The workflow extracts a
schema from the target source, creates review-only migration candidates, and
checks the test-critical invariants stored in each mutable case's `case.yaml`.
Scientific intent and reasoning remain in the case report; the suite does not
use per-case `scientific_intent.yaml` files.

For historical-fire, landscape, structure, or experimental validation work,
follow `cases/Validation/skills/elmfire-validation-case/SKILL.md`. It orchestrates
data provenance, configuration justification, preprocessing, postprocessing,
input statistics, observation comparisons, validation metrics, result
visualization, and a standalone report for the ELMFIRE Validation Guide.

6. **Run the end-to-end pipeline**:
   ```bash
   ./cases/Verification/coupling_tests/CASE30_EXM/run_case.sh
   ```
   Swap in the category path you selected in step 1.
   Iterate on the configuration, post-processing, or report content until the
   outputs and PDF look correct.

7. **Version-control the case** by adding its metadata, source inputs, scripts,
   and report sources to Git. Put irreplaceable observations or approved
   baselines under a clearly named source/reference directory, not under
   `outputs/`, `figures/`, `logs/`, or `scratch/`; those runtime directories are
   regenerated by the case pipeline and removed by `make clean`. Large raw
   rasters can be excluded if they are reproducible elsewhere (scripts should
   be provided); otherwise coordinate storage with the team.

---

## Running the suite in Google Cloud

The `gcp_config/` folder contains everything required to build a container that
packages the ELMFIRE source tree together with the V&V suite and to launch a
Google Cloud Batch job that executes `tools/run_all.py` across all cases.

### 1. Prepare infrastructure

1. Create an Artifact Registry Docker repository (for example
   `${REGION}-docker.pkg.dev/${PROJECT_ID}/elmfire-vnv-suite`).
2. Provision a Google Cloud Storage bucket that will receive the generated
   figures, metrics, and PDFs (e.g., `gs://elmfire_vnv_reports`). Grant the
   Cloud Build and Batch service accounts permission to write to the bucket.
3. Identify or create a service account that can submit Batch jobs and access
   Artifact Registry and the results bucket. Update the `_JOB_SA` substitution in
   `gcp_config/cloudbuild.yaml` accordingly.

### 2. Build the ELMFIRE + V&V image

The multi-stage Dockerfile at `gcp_config/Dockerfile` compiles ELMFIRE from
source and installs the suite dependencies. Build it with Cloud Build so the
image lands in Artifact Registry:

```bash
gcloud builds submit \
  --config gcp_config/cloudbuild.yaml \
  --substitutions _REGION=us-central1,_REPO=elmfire-vnv-suite,_IMAGE=elmfire-vnv,_JOB_NAME=elmfire-vnv,_RESULTS_BUCKET=gs://elmfire_vnv_reports,_JOB_SA=SERVICE_ACCOUNT_EMAIL,_TASK_COUNT=4,_PARALLELISM=4 \
  .
```

The build step renders `gcp_config/infra/batch_job.json` with the provided
substitutions and automatically submits a Cloud Batch job named
`elmfire-vnv-<commit>` that executes `python3 tools/run_all.py`. Adjust the
`_TASK_COUNT` and `_PARALLELISM` substitutions to control how many shards the
suite is split into.

### 3. Monitor and collect results

Once submitted, track the Batch job from the Cloud Console:

```
https://console.cloud.google.com/batch/jobs/details/${REGION}/elmfire-vnv-<commit>?project=${PROJECT_ID}
```

Each task uploads a snapshot of the suite (including generated reports) to the
`RESULTS_BUCKET`, using the commit SHA as a prefix. Download the artifacts with
`gsutil rsync` or from the console. Logs for each task are forwarded to Cloud
Logging for troubleshooting.

If you prefer Cloud Run Jobs instead of Batch, a template is also provided at
`gcp_config/infra/job.yaml`. Render it with `sed` (mirroring the Batch step in
`cloudbuild.yaml`) and deploy it via `gcloud run jobs replace` followed by
`gcloud run jobs execute`.

---

## Updating cases for a new ELMFIRE release

When validating a new target version of ELMFIRE, follow this standard process
for each affected case:

1. **Acquire or build the updated ELMFIRE executable** and update the path used
   by the case (`ELMFIRE_BIN` or `elmfire.bin` in `case.yaml`).
2. **Record the version in the report** by updating `\CaseVersion` (and
   optionally `\CaseDate`) within `report/case_macros.tex`.
3. **Review acceptance criteria** in `report/case_body.tex` to confirm they still
   apply. Adjust tolerances if model changes warrant it and document the
   rationale in the “Discussion” section.
4. **Re-run** `./cases/<case-path>/run_case.sh` to generate fresh outputs, metrics,
   and the updated PDF.
5. **Inspect diffs** in `outputs/metrics.json`, plots under `figures/`, and the
   LaTeX report. Highlight notable changes in the Discussion section.
6. **Regenerate both aggregate reports** (especially after multiple cases are
   refreshed):
   ```bash
   ./tools/build_all.sh
   # or
   make build-all
   ```
   This regenerates the report include lists and decision tables, then produces
   `main_report/verification_report.pdf` and
   `main_report/validation_report.pdf`. Existing standalone case PDFs are
   included intact.
7. **Commit and tag** the refreshed results. Include the ELMFIRE version number
   in your commit message or Git tag to keep an auditable history.

---

## Workflow summary

- Activate the Python environment and ensure the desired ELMFIRE binary is on
  hand before running any case scripts.
- Use `make run CASE=<case-path>` for spot checks, `make run-verification` or
  `make run-validation` for scoped execution, `./tools/new_case.sh` to seed new
  cases, and `./tools/build_all.sh` to rebuild both aggregate reports.
- Keep case metadata, source inputs, scripts, observations/baselines, and LaTeX
  sources under version control. `make clean` removes runtime products while
  preserving the current report-build snapshot; `make prepare-run` also removes
  prior metrics, generated figures, and compiled reports before a fresh run.
- Document all assumptions and parameter choices in the case report so future
  maintainers can understand and reproduce the verification scenario.
- Prefer referencing the executable via `ELMFIRE_BIN` to avoid hard-coding
  machine-specific paths in `case.yaml`.

### Report typography and formatting-only builds

All individual case reports and aggregate summaries use Times New Roman: 12 pt justified body text, 15 pt bold left-aligned section headings and report titles, and 12 pt regular justified figure and table captions. Omit case subtitles and title dates. Tables must remain at least 10 pt at their final printed size; wrap or split them instead of scaling them down. The canonical style is `main_report/report_style.tex`; keep identical local copies in each case and `cases/case_template/report/report_style.tex` for independent builds. The aggregate guides retain their separately maintained cover layout. Equations retain mathematical fonts and code identifiers retain monospace styling.

To rebuild only a report from its existing evidence, run `latexmk -lualatex -interaction=nonstopmode -halt-on-error case_report.tex` inside that case’s `report/` directory. This avoids preprocessing and simulation. After rebuilding the standalone PDFs, `make reports` refreshes both aggregate guides from existing results. Do not run `run_case.sh`, preprocessing, or numerical postprocessing for a typography-only change.

Both individual and summary wrappers use a 12 pt document-class base. Compact table sizing must be locally scoped; restore `\ReportBodyText` afterward so surrounding narrative remains 12 pt.
