# ELMFIRE source-interface audit

The three archive namelists were audited on 2026-08-26 against the local
ELMFIRE source tree at:

`/Users/yiren/Desktop/MyResearch/ELMFIRE/ELMFIRE_Git/elmfire-nick`

The source tree's base commit was
`efa0b8fa3935ea4cd30a2bd054c7e9b83db81163`. The working tree also contained
pre-existing modifications, including changes to `elmfire_level_set.f90`,
`elmfire_spotting.f90`, and `elmfire_spread_rate.f90`; this audit therefore
describes the inspected local interface, not a pristine release tag.

## Compatibility mappings

- Removed the legacy `COMPUTATIONAL_DOMAIN` namelist. The current source derives
  domain geometry and CRS from the input rasters.
- Removed obsolete `DEBUG_LEVEL` assignments.
- Replaced `USE_PHYSICAL_EMBER_NUMBER = .TRUE.` with
  `GENERATION_MODEL = 'PER-MW'`.
- Replaced the active use of `USE_UMD_SPOTTING_MODEL` with
  `SPOTTING_DISTANCE_MODEL = 'EMPIRICAL'`.
- Replaced `USE_EULERIAN_SPOTTING = .TRUE.` with
  `ACCUMULATION_MODEL = 'EULERIAN'`.
- Replaced `USE_EMBER_IGNITION_MODEL = .TRUE.` and
  `USE_SIMPLE_IGNITION_MODEL = .FALSE.` with
  `IGNITION_MODEL = 'PHYSICAL'`.
- Moved the Tubbs `HRR_ELLIPSE_ADJ` assignment from `SIMULATOR` to `WUI`.
- Set Tubbs `NUM_IGNITIONS = 0` and removed its archived `X_IGN`, `Y_IGN`, and
  `T_IGN` point. The same namelist enables `RANDOM_IGNITIONS` and
  `USE_IGNITION_MASK`; current ELMFIRE executes point and random ignitions
  cumulatively, while the case report and ensemble design specify random
  ignition-mask sampling only.
- Mapped archived building-spread model type 3 in the Thomas and Tubbs cases to
  current UCB/UMD type 2. The current source documents only types 1 and 2.
- Replaced host-specific GDAL paths with `PATH_TO_GDAL = 'auto'`, which the
  inspected source resolves from `PATH`.

A mechanical audit confirmed that every active assignment in all three
`elmfire.data.in` files belongs to the corresponding namelist declared in the
inspected `build/source/elmfire_namelists.f90`. This audit did not compile or run
ELMFIRE.
