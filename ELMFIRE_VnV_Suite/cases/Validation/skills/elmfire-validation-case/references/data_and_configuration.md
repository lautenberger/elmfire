# Data provenance and configuration rationale

Build a traceability table before finalizing the namelist. Use columns such as:

| Component | Configured choice | Evidence source | Basis | Uncertainty or gap |
|---|---|---|---|---|
| Ignition | location and time | incident record or experiment log | documented | geolocation/time uncertainty |
| Meteorology | product, stations, bands | provider dataset | documented/transformed | interpolation and temporal resolution |
| Fuels | model and vintage | LANDFIRE, survey, or experiment | documented | classification and age |
| Topography | DEM and derivatives | provider dataset | documented/transformed | resolution and vertical datum |
| Spotting | generation, transport, accumulation, ignition | paper/thesis/source audit | documented/inferred/user-specified | parameter identifiability |
| WUI | footprints and spread parameters | parcel/building data or experiment | documented/inferred/user-specified | completeness and model category |

Use these basis labels consistently:

- **documented:** stated directly by an authoritative source;
- **derived:** calculated deterministically from documented quantities;
- **inferred:** logically inferred from evidence, with the reasoning stated;
- **user-specified:** supplied explicitly because the sources do not resolve it;
- **calibrated:** estimated from separate calibration data, with method and split;
- **unsupported:** unresolved and blocking.

Do not use the validation observations both to choose parameters and to claim an
independent predictive validation without declaring the calibration dependence.
Separate calibration and evaluation events, locations, times, or data subsets
when possible.

## Consequential settings

Require evidence or a user decision for at least:

- ignition geometry, coordinates, timing, multiplicity and uncertainty;
- simulation start/stop, timestep/CFL, output cadence and ensemble size;
- meteorological source, temporal indexing, units, wind height and rotation;
- fuels, moisture, canopy and terrain products and conversions;
- random seeds and perturbation distributions;
- crown-fire, spotting and ember models and parameters;
- WUI/building model selection, fuel categories, separation, footprint and
  exposure/ignition parameters;
- barriers, suppression or other event-specific interventions; and
- the observation cutoff or comparison time.

Inspect the target ELMFIRE source for accepted keys, defaults, enums and units.
An old namelist proves historical use, not compatibility or continued semantic
equivalence.

## Reader-facing descriptions

Apply [report_language.md](report_language.md). Describe input quantities, sources,
transformations, uncertainty, and semantic output selection without filenames,
paths, script names, or software-development jargon in reports and figures.
