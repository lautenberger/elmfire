# Report-language revision and verification record

## Outcome and limits

The active reports, report-producing source, summary generator, case template,
and future-case guidance have been revised toward a scientific fire-modeling
voice. All 52 standalone reports and both aggregate guides compile using
LuaLaTeX. This is **not a claim of complete reader-facing compliance**:
older saved figures still contain internal names, abbreviated labels, and some
crowded text. Their plotting source has been revised, but regeneration from
matching scientific evidence has not been established as safe for every figure.
The saved-figure findings below are part of the handoff, not a passed check.

No ELMFIRE simulations were run. The initial working tree was clean. No changes
were committed, and no scientific evidence was deleted to improve presentation.

## Reports reviewed

Discovery used `tools/generate_summary_reports.py`, including its exclusion of
legacy material. The scope comprises CASE01–CASE49 and Camp, Thomas, and Tubbs
Fire: 49 verification cases and three landscape-scale validation cases. It also
includes the verification and validation guides, their opening summary tables,
and their computational-environment descriptions.

The [complete inventory](report_language_review/inventory.json) lists every
active case, report source, identified text/figure producer, and figure PDF.
The inventory contains 378 report-source files, 142 producer scripts, and 190
saved figure PDFs. It is a discovery inventory, not a claim that every source
line received an independent scientific review.

Scientific decisions remain:

| Decision | Cases |
| --- | --- |
| PASS | CASE01, CASE02, CASE04–06, CASE09, CASE15–16, CASE19–26, CASE28, CASE30–42 |
| FAIL | CASE03, CASE07–08, CASE12, CASE17–18, CASE27, CASE29, CASE45–46, CASE48–49 |
| NOT EVALUABLE | CASE10–11, CASE13–14, CASE43–44, CASE47 |
| CHARACTERIZED | Camp Fire, Thomas Fire, Tubbs Fire |

These are 30 PASS, 12 FAIL, seven NOT EVALUABLE, and three CHARACTERIZED.
Canonical names and exact per-case decisions are recorded in the
[baseline](report_language_review/baseline.json).

## Changes grouped by purpose

- **Case narratives and tables:** active case bodies and selected generated
  macro text now emphasize physical processes, mathematical expectations,
  prescribed conditions, simulated responses, and scientific interpretation.
  Operational instructions and visible internal references were replaced by
  descriptions of the represented quantities or procedures.
- **Report and figure producers:** presentation-only calls were added to the
  identified producers. Each case has its own `scripts/report_language.py`;
  cases do not need a common presentation script to produce their reports.
  Internal metric keys, file selection, and scientific calculations remain
  operationally unchanged.
- **Aggregate reports:** `tools/generate_summary_reports.py` and both guide
  introductions were revised. Generated summaries were regenerated, not
  hand-edited. `tools/build_all.sh` now gives a filename-free missing-figure
  message in its fallback rather than exposing image paths or suppressing all
  existing images in that report.
- **Future reports:** verification and validation skills, their report-writing
  references, and the case template now distinguish operational construction
  instructions from language allowed in rendered reports and figures.
- **Quality checks:** a read-only compiled-PDF language auditor and focused
  presentation tests were added. This record and its accompanying JSON/text
  records preserve the review evidence.

The [tracked changed-file list](report_language_review/changed-files.json),
[final working-tree status](report_language_review/git-status.txt), and
[diff statistics](report_language_review/git-diff-stat.txt) provide exact paths.
New, untracked helpers and guidance are listed in the status record; ordinary
`git diff --stat` does not include untracked files.

## Terminology and internal-reference policy

Replacements follow meaning: an analytical solution remains an analytical
reference solution; an independently calculated prediction is a reference
calculation; a prescribed condition is not described as a software fixture.
Simulation conditions, parameter combinations, and ensemble members are
distinguished where the case materials support that distinction. Physical
processes and numerical methods replace references to program branches and
routine names. Standard scientific terms, acceptance criteria, and formal
status names remain.

The [initial terminology audit](report_language_review/terminology_audit.json)
and [presentation vocabulary](report_language_review/terminology-policy.json)
record the audit candidates and contextual replacement policy. Published
reference titles retain their original wording, including “Framework” in a
thesis title. The scientific verb “contract,” describing decreasing intervals
or differences, is retained. These are not software-development terminology.

The [per-case visible-filename comparison](report_language_review/visible-filename-comparison.json)
lists extracted filename/path tokens removed from each standalone PDF and
tokens still present. It must be read alongside the final PDF audit: it is not
an OCR inventory of every possible embedded label. Removed references include
configuration and metric filenames, preprocessing/postprocessing script names,
report-source names, local model-component paths, and archive filenames.
Internal LaTeX resource arguments and calculation metadata are deliberately
retained where required, but are not intended for display.

## Reproducibility descriptions

Narratives now identify level-set initialization, fuel classification, wind
and moisture conditions, nonburnable treatments, spatial resolution, temporal
selection, and relevant physical output quantities without directing readers
to scripts or rasters. Existing values and distinctions between output sequence
numbers and physical output times were retained. Captions for CASE30–33 were
corrected to identify their actual single-panel prescribed fields and grid-cell
coordinates; they no longer incorrectly claim a two-panel ignition plot with
coordinates in metres.

The later WUI cases describe the reviewed physical/numerical model components
and the limits of source-identity evidence without displaying local source
paths or routine names. Component identity is not asserted to prove the
compiler or the source used for an earlier simulation.

Validation reports describe the supplied historical-fire datasets, observations,
meteorological conventions supported by the available settings, ignition
assumptions, and the uncertainty in recovered provenance. They retain external
providers, citations, and descriptive hyperlinks. Missing provider versions,
upstream processing details, and the scientific justification for recovered
parameter choices are not invented. These remain documented uncertainties;
this language task did not resolve them through new data or calibration.

## Figure changes and remaining stale figures

Thirty figure PDFs were regenerated without running simulations or full
postprocessing:

- Eight parameter-response figures: CASE34–39, CASE41, and CASE42, using existing
  calculated metric rows.
- Ten metric-summary figures: CASE20–29, using existing metric records. Some are
  saved supporting figures rather than figures included in the current case
  report.
- Eight input/observation figures: four each for Camp and Tubbs Fire. Available
  input and observation summaries were checked against their existing records
  before plotting.
- Two independent-reference figures: CASE43 and CASE44. Original and revised
  plotting calls produced exactly equal line coordinates, axis limits, and axis
  scales. CASE43's prescribed parameter matrix was also exactly equal.
- Two heat-response figures: CASE45, using the saved per-condition metric rows
  without entering the spatial-raster reader. Line coordinates, bar geometry,
  axis limits, and metric values were exactly equal before and after revision.

Labels identify physical quantities, units, and simulation conditions rather
than script or raster names. The CASE44 heat-release and heat-area labels were
wrapped after visual inspection to eliminate clipping. Recorded metrics were
not rewritten. Regeneration records are available for
[sweeps](report_language_review/regenerated-figures.json),
[metric summaries](report_language_review/regenerated-metric-figures.json),
[validation inputs](report_language_review/validation-input-regeneration.json),
and [reference predictions](report_language_review/regenerated-reference-figures.json).
The additional [heat-response records](report_language_review/regenerated-heat-metric-figures.json)
record the isolated, metric-only CASE45 regeneration.

Thomas Fire input-figure regeneration was stopped because the existing
wind-direction statistics did not match the available input fields. Its four
input/observation figures were left unchanged. No statistical values were
replaced to make that check pass.

The [saved-figure findings](report_language_review/saved-figure-text-findings.json)
give the exact remaining PDF paths, page/line locations, and offending text.
The final scan flags 79 saved figure PDFs for contextual review.
They include older input/domain-result figures in spotting cases, GUIDE-derived
cases, and parameter-sweep cases.
Raw results or the matching prepared conditions needed to safely redraw many
of these figures are absent locally. No figure was fabricated from a caption
or expected response. Remaining old labels include raster filenames, condition
directory names and “variant.” Other visual issues include crowded
titles in some older spotting figures and the Thomas Fire arrival-field plot.
The audit is conservative and also scans saved figures not included in a guide.

Thus source edits do not mean these existing figure PDFs are compliant. Matching
saved scientific evidence must be supplied or independently established before
they can all be safely regenerated. Text-only PDF inspection also does not
detect every abbreviation or label embedded in raster images.

## Aggregate summaries and future guidance

Both guides retain case-name/title hyperlinks and scientifically derived status
decisions. Comparison type and declared MPI process count replace internal
configuration-location descriptions. Environment tables show release when
declared, operating system, architecture, available MPI/compiler information,
Python, and scientific library versions without installation paths.

The tables explicitly describe the environment observed during report
preparation. It is not presented as the environment of earlier simulations, and
an available compiler is not claimed to have built ELMFIRE. Missing release or
environment evidence stays undeclared. A successfully compiled narrative with
missing evaluation evidence remains NOT EVALUABLE.

Both skills now link a report-language reference and require scientific input
construction descriptions, semantic output selection, and physical/numerical
process descriptions. Operational filenames remain in skill instructions only
where needed to construct and execute cases. They are explicitly prohibited in
reader-facing reports and figures. The case template follows the same boundary
and retains scientific sections rather than prescribing software terminology.

## Scientific and build checks

The [final checks](report_language_review/final-checks.json) record:

- All 764 baseline scientific-file SHA-256 checksums are unchanged. This covers
  the discovered non-report, non-figure case data, namelists, observations,
  available raw results, metrics, and metadata. It is not a hash claim about
  scientific files absent from the local checkout.
- All 52 case decisions are unchanged, including the seven NOT EVALUABLE cases.
  The CASE11 visible overall-status wording now follows the guide's existing
  NOT EVALUABLE normalization; its underlying records were not changed.
- Bibliography blocks are unchanged. Governing mathematical expressions are
  unchanged. Recorded presentation-only math differences comprise removal of
  literal inequality-symbol typesetting instructions in CASE28, removal of an
  inapplicable generic ignition-panel expression from CASE30–33 captions, and
  replacement of the word “variant” in CASE41's mathematical-table heading.
  None changes an equation, numerical result, threshold, or initialization rule.
- 197 Python sources parse without writing bytecode. The report build shell
  script passes `bash -n`. All 52 case-local language helpers match the central
  presentation helper.
- All 52 standalone LuaLaTeX builds and both aggregate builds succeed. Standalone
  results are listed in [build results](report_language_review/build-results.json).
  Final logs contain no overfull boxes, missing-character warnings, or undefined
  reference warnings matched by the checks.
- All 244 standalone, aggregate, and saved-figure PDFs permit text extraction.
  The [compiled-PDF audit](report_language_review/pdf-language-audit.json) retains
  518 contextual candidates, including duplicates inherited by aggregate guides
  and legitimate words in citation titles. It is not a clean language pass.
- All 339 standalone pages were rendered and reviewed in contact sheets.
  Selected figure/table pages and opening guide pages were inspected at larger
  size, including the corrected CASE09 parameter table, CASE43/44 reference
  figures, and validation environment/decision tables. This does **not** satisfy
  a full-size inspection of every figure or table, and no OCR completeness claim
  is made. Font inventories are retained in
  [PDF font records](report_language_review/pdf-fonts.json). All 52 standalone
  PDFs contain Times New Roman regular and bold faces; mathematical and plotted
  text also uses the case styles' existing mathematical and figure fonts.
- Both revised case-writing skills pass the skill validator.
- All 13 focused report/presentation tests pass. The broader combined run passes
  27 of 28 checks. Its one pre-existing failure expects
  `TARGET_FRONT_ADVANCE_FRACTION = 0.05` in unchanged CASE42 code. That unrelated
  scientific setup was not altered. See [test output](report_language_review/test-results.txt).
- `git diff --check` passes. Final status and diff-stat records are linked above.

No full postprocessor or ELMFIRE executable was run. The missing-figure fallback
branch was not separately exercised. No simulation-data cleanup was performed.
Full-resolution review of all figure pages and regeneration of the remaining
saved scientific figures are still outstanding.
