# Verification report structure

Write the report as a self-contained scientific verification argument for a reader who knows ELMFIRE but has not read the source publication, another case report, or the implementation notes used by the author.

Use numbered `\\section{...}` commands with these exact names and in this order. Do not merge or reorder them around the chronology of file generation.

Apply [report_language.md](report_language.md) to all reader-facing text and figures.
Operational filenames below describe construction only, not report wording.

## 1. Verification purpose

State:

- the physical, mathematical, numerical, or algorithmic behavior being verified;
- why that behavior matters to ELMFIRE;
- the physical and numerical ELMFIRE processes exercised, including relevant model choices and state updates;
- the defects the case is capable of exposing;
- what is controlled, varied, enabled, and disabled; and
- why this is implementation verification rather than physical validation.

End with one precise verification question naming the observable, required variants or parameter range, and acceptance condition. Do not describe the case only by its publication origin or input geometry.

## 2. Mathematical formulation and numerical implementation

Present the governing equations, variables, units, assumptions, coordinate and sign conventions, discretization, timestep and mesh dependence, update order, source terms, boundary treatment, and output semantics needed to understand the test.

Connect the formulation to the actual ELMFIRE implementation. Explain what the relevant namelist settings and input fields cause the code to do; do not merely list filenames or routine names.

When equations do not adequately describe the behavior, provide readable pseudocode, an algorithm summary, state-transition logic, or a discrete update equation. If no nontrivial mathematical derivation applies, say so and explain the algorithmic property or invariant that replaces it.

## 3. Derivation of expected behavior

Derive the expectation for the configured experiment, not just the general model. Use the appropriate form of reference evidence:

- an analytical or manufactured solution;
- a conservation or symmetry property;
- a limiting behavior or invariant;
- a convergence order or mesh/time-step trend;
- an independently calculated algorithmic result;
- a controlled comparison between variants; or
- another documented known response.

Show enough intermediate reasoning that the expected value, profile, ordering, trend, or state can be reproduced. Identify approximations and distinguish exact expectations from numerical estimates. If the reference depends on run-derived quantities, explain how those quantities enter the reference without making the simulation output its own circular standard.

Do not rely on unexplained phrases such as “the thesis value,” “Equation 3.11,” or “the expected curve.” Restate and derive the material needed by this case, then cite the external source for provenance. When a supplied guide or publication contains the originating case, preserve scientifically relevant discussion, equations, and visualization intent where they remain correct, but allocate them to the required sections rather than copying the source chronology. Clearly label historical reference expectations and figures, current generated configuration, and current ELMFIRE results as distinct evidence classes. Generated inputs control configuration claims; explain any conflict with the reference explicitly.

## 4. Verification metrics and acceptance criteria

Define metrics only after deriving the expected behavior. Trace every metric to one expected property and explain both how and why it was selected.

For every metric document:

- its name and mathematical formula;
- the expected property it measures;
- why it is sensitive to the defect or implementation error of interest;
- why it is preferable to plausible alternatives and robust to irrelevant variation;
- the physical output quantity and exact semantic selection rule (member, time, uniqueness, and completeness);
- spatial region, masks, nodata handling, and buffer exclusion;
- temporal selection or ensemble statistic;
- normalization and aggregation;
- required output completeness;
- tolerance or limit and the scientific or numerical basis for that tolerance; and
- the component pass/fail rule.

Use a compact traceability table when several metrics are present, with columns such as expected property, observable, metric, selection rationale, tolerance rationale, and component decision.

State the complete Boolean rule for the overall decision. A metric must not be chosen merely because it is easy to extract or visually attractive. Missing, stale, malformed, or insufficient outputs are non-passing and `NOT EVALUABLE`; they are never evidence of a pass.

## 5. Simulation configuration and predicted outcome

Document enough information to reproduce the experiment: domain and coordinate system, physical and buffer cells, grid resolution, timestep, initialization or ignition, fuels, terrain, moisture, weather, relevant model selectors, coupled or disabled processes, variants, duration, and output cadence. When terminal evidence is required, state how `SIMULATION_DT`, `SIMULATION_DTMAX`, and `SIMULATION_TSTOP` were aligned and identify any justified exception.

Use a table with columns for parameter, configured value, units, and role in isolating the behavior. Every spatial case must include at least one whole-domain prepared-input figure before any actual-result figure, even when the inputs are uniform. Generate it mechanically from the current case-local preprocessing artifacts and show the initial PHI or ignition geometry together with a scientifically controlling fuel, terrain, moisture, weather, canopy, structure, or other field. For sweeps, identify the representative generated variant or visualize the varying input. Put coordinates, physical units or categories, the prescribed simulation condition, and the scientific input construction in the figure or caption. Do not display filenames or routine names. A schematic, hand-redrawn field, reference image, or expected solution cannot substitute for this input evidence. Before showing actual output, state the predicted qualitative appearance and quantitative values of every result that will be used in the decision.

## 6. Actual simulation results

Present only results read from actual case-local artifacts. Record the ELMFIRE release or source revision when available, the physical output quantities and their time/member selection, completed and required simulation conditions, and incomplete or failed simulations. If a stalled-final compatibility path selected terminal evidence, report the meteorology-interval reconstruction test, reconstructed pre-jump time, whether the stall was near-stop or independently expected no-propagation, and the physical quantities uniquely selected at that time; do not present the overrun timestamp as the nominal stop time.

For a completed spatial case, include at least one representative whole-domain result before or beside derived profiles, convergence curves, tables, or scalar summaries. Every figure must identify the data source, reference where applicable, physical units, variant, output time or ensemble statistic, and acceptance limits. Do not manufacture results, substitute a synthetic curve for missing output, or show only a cropped region when domain behavior matters.

## 7. Calculated metrics and verification decision

Present one row per acceptance component with the metric, expected value or limit, calculated value, and component status. State the overall result prominently as `PASS`, `FAIL`, or `NOT EVALUABLE`; use the more specific workflow states `NOT RUN`, `INCOMPLETE`, or `BLOCKED` in metadata when applicable.

Source values and the decision directly from `outputs/metrics.json` through generated `metrics_macros.tex`. Do not recompute, hand-copy, or override the decision in LaTeX.

## 8. Technical interpretation

Explain why the result passed, failed, or could not be evaluated. Relate the evidence to the derivation and metric rationale. Distinguish among source-code defects, incorrect configuration, inadequate resolution, statistical variability, output selection, postprocessing errors, reference limitations, and missing output. Do not merely restate the table or describe the plot.

## 9. Scope and limitations

Conclude with what was demonstrated, what remains unverified, assumptions restricting the conclusion, additional variants or observables that may be needed, and whether the evidence supplies code verification only or any limited physical-validation evidence.

Maintain this narrative progression:

```text
purpose -> formulation or algorithm -> derived expectation -> justified metrics
        -> configuration -> predicted outcome -> actual results -> decision
        -> interpretation and limitations
```

## Self-contained LaTeX project

Use `case_metadata.tex` as the single source for `CaseNumber`, `CaseID`, `CaseTitle`, and `PurposeAbbreviation`. Use `case_macros.tex` for stable case constants, `technical_macros.tex` for technical definitions, generated `metrics_macros.tex` for computed results, `case_body.tex` for the nine-section argument, and `case_report.tex` for the standalone wrapper and preamble. Keep all six files present, using intentionally empty macro files when appropriate.

Every `\\input`, `\\include`, `\\includegraphics`, bibliography, and style dependency must resolve inside the case directory. Define every acronym, symbol, and case-specific term locally. External scientific material may be cited when necessary, but the case must restate the formulation or reference result needed to understand and reproduce its verification logic.

Require every metric key used by the report to exist in `metrics_macros.tex`. Render a conspicuous `MISSING` marker for an absent key and treat that marker as report-validation failure.

## ELMFIRE Verification Guide integration

Each case report has two obligations:

1. It must compile independently from its own `report/` directory when no other case and no master-report source is available.
2. It must be consumable by the repository's master build to form the single ELMFIRE Verification Guide.

The master guide is an aggregator, not a source of case content. A case must not depend on the guide for its preamble fragments, definitions, macros, bibliography entries, figures, derivation, or scientific context. Prefer aggregation of the successfully compiled standalone report artifact when that avoids macro, label, bibliography, or package collisions. If the repository instead includes `case_body.tex` directly, keep the body free of document-level commands, use case-prefixed labels and macro names, and ensure the master explicitly loads the case's local metadata and macro files.

Use consistent terminology, units, section names, and status vocabulary across cases. Do not assume that the reader encountered another case first, and do not create cross-case references needed to understand or evaluate the current case. Cross-case comparisons may be added to the master guide as separate synthesis material, but they cannot replace a case's local derivation or decision argument.

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
