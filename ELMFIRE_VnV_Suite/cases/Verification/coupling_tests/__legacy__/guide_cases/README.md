# ELMFIRE Guide Verification Cases

These coupling-verification cases reformulate the simplified scenarios in
Section 3.2 of the ELMFIRE Guide into the repository-native case layout. Each
case owns its ELMFIRE namelist, input-generation wrapper, output postprocessor,
runner, metrics, figures, and standalone LaTeX report.

The deterministic raster generation and metric extraction once shared by these
historically related cases are retained in `tools/` for provenance only.  The
active CASE20--CASE29 directories contain newer case-local copies and do not
import this archived code. The unmodified source PDF, original all-in-one
harness, and original template namelists are retained in
`reference/original_guide/` for scientific provenance.

All scenarios are categorized as coupling tests because each executes the full
ELMFIRE simulation workflow and exercises interactions among multiple model
components. They are not isolated subroutine-level unit tests.
