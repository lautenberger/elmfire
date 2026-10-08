# Input statistics, result visualization, and metrics

Choose analyses that explain the actual case. Do not create every possible plot.

## Input characterization

For continuous raster or tabular fields report valid/nodata counts, units,
minimum, maximum, mean, median, standard deviation, and useful quantiles. Show a
histogram or distribution plot and a representative two-dimensional map when
spatial organization matters.

- Meteorology: summarize space and time separately; show time series or
  time-band summaries in addition to pooled histograms. Use circular statistics
  or a wind rose for direction—an ordinary arithmetic mean of degrees is
  generally invalid. State wind reference height, units and rotation.
- Terrain: map elevation and slope; treat aspect as circular. State horizontal
  and vertical datums and resolutions.
- Fuels/buildings: report category counts and area fractions rather than numeric
  means of class codes. Map important categories and nonburnable coverage.
- Moisture/canopy: report distributions, units, conversions and spatial masks.
- Ignition: show location or footprint on the whole domain with time and
  uncertainty when known.
- Observations: summarize sampling times, spatial coverage, missingness,
  uncertainty and any censoring or detection threshold.

Avoid statistics that hide structure: a domain-wide mean alone is inadequate
for heterogeneous fields, and pooling all weather bands can obscure event
evolution.

## Field-level results

Show representative whole-domain two-dimensional fields applicable to the case,
such as time of arrival, burn probability, fireline intensity, flame length,
ember generation/deposition/flux, structure heat-flux exposure, ignition state,
or structure damage. Each figure must identify field, units, time, ensemble
member/statistic, CRS or spatial axes, nodata treatment, and color scale.

Select times and members by a declared rule, not because they look favorable.
Use shared limits for side-by-side comparisons when valid. Overlay or juxtapose
observations with clear visual distinction and cite their source. Do not
interpolate, crop, smooth, or resample without reporting the method and effect.

## Higher-level results

When supported, show:

- burned area or perimeter growth over time;
- structures ignited/damaged over time and by exposure pathway;
- heat flux, cumulative dose, ember deposition or ignition histories;
- ensemble median and uncertainty bands;
- spatial error or residual maps; and
- metric distributions across members, sensors, structures or experiments.

## Metric selection

Every metric needs a formula, observable, exact selection rule, rationale,
baseline, uncertainty treatment and decision role. Plausible examples include:

- footprint overlap: Jaccard/IoU, precision, recall and F1;
- probabilistic extent: Brier score, reliability or proper scoring rules;
- arrival timing: bias, MAE, RMSE and quantiles at observed locations;
- boundary position: distance-based errors, with raster resolution considered;
- growth: burned-area curve error or time-resolved area bias;
- heat flux: time-history error, peak error, time-to-peak and cumulative exposure;
- ignition/damage: ignition-time error, confusion measures, survival/time-to-event
  comparisons, or damaged-structure count error; and
- ensembles: coverage, calibration and uncertainty intervals.

Do not use overlap alone when overprediction and underprediction have different
meaning; report precision and recall or equivalent decomposition. Do not use a
percentile conditioned only on burned members when the no-burn probability is
scientifically important.

Baseline values must come from observations, an experiment, an independently
defined benchmark, a previously frozen model baseline, or a predeclared
acceptance rule. If no justified threshold exists, calculate and interpret the
metrics but classify the case as `CHARACTERIZED`, not `PASS`.

## Reader-facing descriptions

Apply [report_language.md](report_language.md). Describe input quantities, sources,
transformations, uncertainty, and semantic output selection without filenames,
paths, script names, or software-development jargon in reports and figures.
