# Landscape-scale validation

These cases compare ELMFIRE landscape reconstructions with historical-fire or
landscape-risk evidence. The Camp, Thomas, and Tubbs cases were assembled from
the tracked archives in `cases/Validation/3_FIRES/`:

- `camp_fire/` - 2018 Camp Fire reconstruction;
- `thomas_fire/` - 2017 Thomas Fire reconstruction;
- `tubbs_fire/` - 2017 Tubbs Fire reconstruction.

Each case contains a modern current-source namelist, archive-derived inputs, a
SHA-256 integrity manifest, a source/observation provenance manifest, a read-only preprocessor, a non-destructive
postprocessor, a simple runner, and a standalone report. The original RAR files
and legacy notebook remain unchanged as provenance. The notebook is not part of
the active workflow because it contains hard-coded paths and destructive file
operations.

Run a case from any directory with, for example:

```bash
ELMFIRE_BIN=/path/to/elmfire \
  cases/Validation/landscape_scale/camp_fire/run_case.sh
```

The default launch uses 50 MPI ranks, one per ensemble member, and accepts a
smaller positive override through `ELMFIRE_MPI_RANKS`. The shared Savio header
keeps those ranks on one node and disables PROJ network caching to prevent MPI
ranks from contending for a shared user-cache database. Preprocessing requires
Python, NumPy, Matplotlib, Rasterio, PyYAML, and GDAL's `ogr2ogr`. Result comparison
additionally requires Pandas, GeoPandas, and Shapely with their geospatial
runtime libraries.

The standalone reports are included in the repository-wide
`main_report/validation_report.pdf`; generate it with `make validation-report`.
The repository-completion reports say `NOT EVALUABLE` because no long-running
ELMFIRE simulations were executed. A future run changes the status to
`CHARACTERIZED`; the cases intentionally do not claim PASS/FAIL because the
archives contain no justified acceptance thresholds and the VIIRS hull
uncertainty is not quantified.

For new cases or scientific revisions, follow
`cases/Validation/skills/elmfire-validation-case/SKILL.md`. The skill requires
traceable input provenance, justified namelist choices, comprehensive input
characterization, case-local preprocessing and postprocessing, observation-aware
metrics, representative raster results, and a standalone report suitable for an
aggregated ELMFIRE Validation Guide.
