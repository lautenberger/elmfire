# Validation case contract

Use the repository-native layout, creating only meaningful directories:

```text
<case_id>/
├── case.yaml
├── elmfire.data.in
├── run_case.sh
├── compile_case.sh
├── data/
│   ├── raw/ or archive_payload/
│   ├── derived/
│   ├── observations/
│   ├── misc/
│   └── source_manifest.json
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

## Metadata boundary

Use `case.yaml` for identity, category, runtime configuration, variant discovery,
observation identifiers, metrics/figure paths, and exact namelist values whose
change would invalidate the comparison. Keep scientific argument and provenance
reasoning in the report and source manifest.

Do not duplicate report prose in YAML. Do not add `scientific_intent.yaml` for an
ordinary case.

## Source manifest

For every raw data component record, as applicable:

- stable local identifier and role;
- original filename and case-local path;
- provider, publication, repository, DOI, URL, accession, or archive identity;
- acquisition or download date and version;
- license, redistribution constraint, and attribution requirement;
- SHA-256 and byte size;
- native CRS, resolution, dimensions, units, nodata, time basis and coverage;
- whether it is raw, extracted unchanged, converted, resampled, clipped,
  interpolated, rasterized, synthesized, or user-supplied; and
- parent sources and transformation parameters for derived data.

When raw data cannot be committed, include metadata and a deterministic retrieval
or staging procedure. Do not silently substitute a similar dataset.

## Script responsibilities

`preprocess.py` owns integrity checks, source-to-model transformations, spatial
and temporal alignment preparation, input statistics, and input figures.
`postprocess.py` owns output discovery, completeness checks, observation
alignment, calculated metrics, result figures, and status. Generated TeX macros
must read metrics rather than recompute them.

Use explicit paths relative to the case root. A case may use standard installed
scientific libraries, but must not require another case's Python module or data.

## Presentation boundary

Follow [report_language.md](report_language.md). The filenames, metadata keys, and
implementation details in this construction reference are operational only.
Describe their scientific meaning in reports; never display these identifiers.
