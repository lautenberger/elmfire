# Reader-facing scientific language

Write technical articles for a fire modeler familiar with ELMFIRE but unfamiliar
with the case's implementation. Explain the physical process, mathematical
expectation, prescribed conditions, measured response, and scientific
interpretation. Retain derivations, limitations, uncertainty, and numerical
detail. Define symbols and acronyms in the sentence where they first appear.
Do not create a separate terminology, notation, or acronym subsection or
paragraph. Introduce an abbreviation after its full scientific name and explain
mathematical symbols alongside their first equation. Use direct, impersonal prose.

This policy applies to titles, prose, tables, captions, footnotes, appendices,
generated LaTeX, status explanations, and every visible figure label. It takes
precedence over older examples that display operational names.

## Scientific vocabulary

Choose wording by meaning, not a global word substitution. An oracle may be an
analytical solution, independent reference calculation, conservation constraint,
or expected physical response; identify which one applies. Describe a fixture
as prescribed conditions and a variant as a parameter combination, simulation
condition, or ensemble member as appropriate. Describe results and supporting
evidence, not artifacts; source datasets, not payloads; evaluation definitions
or acceptance criteria, not contracts. Avoid software-development terminology
such as test harness, schema, registry, adapter, pipeline, workflow, backend,
stub, mock, framework, source tree, and code path. Explain the physical or
numerical process instead. Do not merely substitute a vague euphemism.

Retain scientific terms including verification, validation, preprocessing,
postprocessing, numerical method, ensemble, convergence, conservation, and
acceptance criterion. Preserve the exact formal status vocabulary PASS, FAIL,
CHARACTERIZED, and NOT EVALUABLE. Missing evidence cannot support PASS.

## No visible operational identifiers

Do not display internal filenames, extensions, directories, installation paths,
script names, configuration keys, routine names, or instructions to inspect
code. The only visible internal cross-reference is a canonical case name or
descriptive case title; case hyperlinks may hide the underlying destination.
External scientific sources remain cited by author, publication or dataset
title, provider, date, DOI, or descriptive hyperlink. Preserve published titles.
Describe ELMFIRE by its release or source revision, never its executable path.

Operational filenames in this skill, internal LaTeX resource arguments, Python
paths, metadata keys, and maintenance documentation remain necessary and are
allowed. They must not leak into compiled reports or figures. Keep exact
operational provenance in internal metadata, without displaying it as prose.

## Reproducible scientific descriptions

For each important input explain quantity, units, value or mathematical/spatial
distribution, extent, coordinate convention, grid dimensions and resolution,
coordinate reference system, time origin and sampling, and data source. Explain
fuel interpretation, moisture basis, wind height/direction, terrain and canopy,
structure geometry, ignition location/shape/time, buffers, nonburnable and
missing cells, transformations, resampling, and random distributions/seeds where
relevant. Cite provider, product, date/version and uncertainties for observations.
Use only established evidence. For unknown details write: "not documented in
the available case materials." Never infer a physical convention from a name.

Specify output selection semantically: physical quantity, ensemble member,
requested time or final-time rule, uniqueness, mask, spatial region, temporal
aggregation, and completeness. Preserve special rules for stalled propagation
and reconstructed output times explicitly; a vague "final result" is not enough.

## Figures and interpretation

Identify input, prediction, simulation, observation, or derived comparison in
each caption. Give physical labels with units, simulation conditions, spatial
and temporal context, and the figure's role in the scientific argument. Use
sentence case. Define abbreviations and distinguish observations from model
results. Labels must remain readable at final printed size. Never change data,
masking, alignment, normalization, limits, thresholds, or interpretation to make
a figure look better. Do not silently replace missing evidence with a sketch.

## Required checks

Record statuses and hash scientific inputs, settings, observations, outputs and
metrics before presentation edits. Edit text-producing scripts as well as
authored prose so regeneration preserves the policy. Do not rerun ELMFIRE for
language work. Before regenerating a plot establish that its producer cannot
alter scientific data or decisions. If numerical postprocessing is unavoidable,
use an isolated copy and require exact semantic equality of metric records.
Otherwise leave the existing figure untouched and report it as stale.

Compile all standalone reports and both guides using the required LuaLaTeX
procedure. Extract text from the compiled PDFs and audit terminology, visible
names/paths, and labels. Render and inspect every page containing a figure or
table for readability, clipping, missing glyphs, broken references and missing
evidence. Check scientific invariants and decisions again. Source searches do
not establish reader-facing compliance. Report skipped or failed checks and
stale figures explicitly; never claim completion while these remain unresolved.
