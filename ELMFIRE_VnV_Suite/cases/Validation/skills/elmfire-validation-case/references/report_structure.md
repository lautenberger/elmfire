# Validation report structure

Write a standalone scientific validation record for a reader who has not read
the source paper, thesis, dataset documentation, or another case report. Use the
following numbered sections in order.

Apply [report_language.md](report_language.md) to all reader-facing text and figures.
Operational filenames below describe construction only, not report wording.

## 1. Validation objective and scope

Identify the event or experiment, physical behavior, ELMFIRE pathways exercised,
prediction quantities, reference observations, validation question, intended
claim, and exclusions. State whether any evaluated data were also used for
calibration.

## 2. Reference event, experiment, and observations

Describe geometry, chronology, environmental context, experimental apparatus or
historical event, sensors or observation products, sampling, uncertainty,
coverage, missingness, detection thresholds and citations. Explain what each
observation represents and what it cannot establish.

## 3. Input data provenance and characterization

Provide a source table for meteorology, terrain, fuels, moisture, canopy,
buildings/WUI, ignition, barriers, interventions and observations. Include
provider/citation, version/vintage, native resolution, units, CRS/time basis,
processing and uncertainty. Present the input statistics and figures required
by `statistics_and_visualization.md` so the reader understands the modeled
conditions rather than only filenames.

## 4. Model configuration and rationale

Document domain, mesh, duration, timestep, ensemble design, random seed,
ignition, outputs and active/disabled physical pathways. For consequential
namelist choices, state the configured value, units, role, evidence source and
basis label: documented, derived, inferred, user-specified or calibrated.
Explain spotting, WUI/building and other specialized parameters explicitly.
Unresolved unsupported choices are blocking.

## 5. Preprocessing and reproducibility

Explain extraction, integrity verification, projection, clipping, resampling,
interpolation, rasterization, unit conversion, time alignment, nodata treatment,
derived fields and software dependencies. Distinguish immutable raw evidence
from generated model inputs.

## 6. Validation metrics and comparison method

Define and justify every metric. Give formulas, data fields, physical-quantity/time/member
selection and uniqueness, spatial masks, alignment method, aggregation, uncertainty treatment,
baseline and acceptance rule. Explain why each metric is sensitive to the
phenomenon of interest and why plausible alternatives were not sufficient.
State the complete status rule before presenting calculated results.

## 7. Simulation results and field-level comparison

Identify the ELMFIRE release or source revision and simulation completeness, without installation paths. Show representative
whole-domain two-dimensional simulation fields with units and time/member
context. Compare them with observation or experiment fields when applicable.
Never substitute synthetic or historical figures for missing current outputs.

## 8. Integrated statistics and calculated metrics

Present higher-level time histories, ensemble distributions, area growth,
structure damage, heat exposure or other case-relevant summaries. Then present
one row per metric with baseline/observation, calculated value, uncertainty or
limit, and component status. Source values mechanically from
`outputs/metrics.json` through `metrics_macros.tex`.

## 9. Interpretation and uncertainty

Explain agreements and discrepancies using the evidence. Distinguish model-form
error, parameter uncertainty, input uncertainty, observation limitations,
resolution, stochastic variability, alignment choices and postprocessing error.
Do not equate visual agreement with validation.

## 10. Conclusions and limitations

State exactly what the evidence supports, what remains unevaluated, whether the
case passed a predeclared criterion or was only characterized, calibration
dependence, transferability limits and recommended additional evidence.

## Standalone and guide-compatible report

Use the six-file report project defined in `case_contract.md`. The case report
must compile independently and remain suitable for aggregation into an ELMFIRE
Validation Guide. Define symbols, acronyms, citations and case-specific macros
locally. Every calculated value must originate from case-local artifacts; absent
values must render visibly as missing and cannot produce a passing status.

## Required report typography

Use the current suite style for every standalone verification and validation case report and the body of both aggregate summary reports:

| Element | Font and size | Alignment |
| --- | --- | --- |
| Report title | Times New Roman, bold, 15 pt | Left |
| Main prose in individual reports and aggregate summaries | Times New Roman, regular, 12 pt | Justified |
| Section, subsection, subsubsection, and paragraph headings | Times New Roman, bold, 15 pt | Left |
| Figure and table captions | Times New Roman, regular, 12 pt | Justified |
| Tables | Times New Roman, normally 12 pt; compact text at least 10 pt | Appropriate to column contents |

Case titles contain only the report title: omit subtitle, author/organization subtitle blocks, and title dates. Retain scientifically relevant dates and provenance in the body. The aggregate guide's separately maintained cover layout is independent of the case title rule.

Keep figure labels readable at their final printed size. Wrap table cells, split tables, or use multipage tables rather than shrinking tables below 10 pt with `resizebox` or `scalebox`. Mathematical notation may use mathematical fonts. Operational identifiers must not appear in reader-facing prose or figures.

The canonical style is `main_report/report_style.tex`. When formatting changes are approved, update that style and synchronize exact local copies into every current case's `report/report_style.tex` and the case template. Load the local file last in the case wrapper's preamble. Cases must never load the canonical file by an external relative path at runtime: independent compilation must remain possible without the suite. Future cases inherit the template copy.

Compile using `latexmk -lualatex`. Genuine Times New Roman regular, bold, italic, and bold italic fonts are required system dependencies; do not silently substitute Nimbus Roman, Liberation Serif, or TeX Gyre Termes. The current style uses 15/17 pt size/leading for titles and headings, 12/14 pt for body text and figure/table captions, and 10/12 pt for compact text.

For a formatting-only request, edit report sources/styles, report build commands, and these instructions only. Do not run preprocessing, simulation, or numerical postprocessing; do not alter namelists, input data, model parameters, evaluation criteria, or result artifacts. Compile directly from existing report inputs, then refresh the guides from the standalone PDFs. Inspect actual PDF font embedding, title weight and size, page layout, captions, and tables. Resolve line overflow through report layout changes while preserving the approved style and scientific content. Distinguish pre-existing missing evidence from formatting failures.

Use `\documentclass[12pt]{article}` in both individual and aggregate report wrappers. Keep `\normalsize` at 12 pt with 14 pt leading, and use the local style's `\ReportBodyText` to restore normal body text after compact material. This rule includes introductions, summary narratives, lists, interpretation, and conclusions. Compact table sizes are exceptions for table content only: enclose their size commands in a local group and restore 12 pt prose afterward. Summary generators must emit these groups and resets so regeneration cannot introduce smaller narrative text. Verify 12 pt body text in the compiled individual PDFs and on aggregate summary pages, including prose after tables; source declarations alone are insufficient.

## Consistent case presentation

Do not add separate terminology or notation sections. Explain terms at first
use in the scientific narrative or beside their first equation. Do not repeat
the case ID in the opening purpose/objective section; keep identity in the
report title, metadata, and guide links. Verification reports retain the nine
section headings above; validation reports retain their ten section headings.
Use US Letter pages with 0.85-inch margins for standalone cases. Use the local
canonical style for 15-pt first-line paragraph indentation and 0-pt paragraph
spacing (with 1-pt stretch); do not override these in individual wrappers.
Keep scientific subsections and equations where needed rather than forcing
different experiments into identical paragraph lengths or table dimensions.
