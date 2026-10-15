#!/usr/bin/env python3
"""Evaluate transient firelines for the two-dimensional transport case.

Only ELMFIRE output rasters are treated as observations. The script selects the
requested perimeter times, calculates explicit acceptance metrics, generates the
vector figure, and writes case-local JSON and LaTeX artifacts.
"""
from __future__ import annotations

from report_language import polish_figure, report_text
import csv
import json
import re
from pathlib import Path
import matplotlib
import numpy as np
import rasterio
from spatial_evidence import generate_spatial_evidence

matplotlib.use('Agg')
import matplotlib.pyplot as plt

# -----------------------------------------------------------------------------
# Customizable postprocessing parameters and acceptance thresholds
# -----------------------------------------------------------------------------
CASE_DIR = Path(__file__).resolve().parents[1]
TARGET_TIMES_S = (30.0, 60.0, 90.0, 120.0)
MAX_TIME_MISMATCH_S = 2.0
BUFFER_CELLS = 2
MAX_NESTING_VIOLATION_FRACTION = 0.02
MIN_FRONT_ADVANCE_PER_INTERVAL_M = 20.0
MIN_FINAL_CROSSWIND_SPAN_M = 40.0
MIN_FIREBRAND_EXCESS_ADVANCE_M = 30.0
SURFACE_HEADFIRE_ROS_MPS = 0.71
IGNITION_X_M = 5.0
IGNITION_Y_M = 5.0
NODATA_LIMIT = -1000.0


def read_raster(path):
    """Read one GeoTIFF band with its geotransform."""
    with rasterio.open(path) as dataset:
        return dataset.read(1).astype(float), dataset.transform.to_gdal()


def dump_map(out):
    """Map transient dump indices to actual ELMFIRE times."""
    files = sorted(out.glob('dump_times_*.csv'))
    ans = {}
    if files:
        with files[-1].open(newline='') as f:
            for row in csv.DictReader(f):
                ans[int(row['dump_index'])] = float(row['time_seconds'])
    return ans



def write_field_diagnostics(out, figs, gt):
    """Plot the final ember-triggered ignition cells without duplicating flux."""
    ignition_files = sorted(out.glob('ember_ignition_[0-9]*.tif'))
    if not ignition_files:
        return
    ignition, _ = read_raster(ignition_files[-1])
    ignition = ignition[BUFFER_CELLS:-BUFFER_CELLS,
                          BUFFER_CELLS:-BUFFER_CELLS]
    ny, nx = ignition.shape
    x0 = gt[0] + BUFFER_CELLS * gt[1]
    y0 = gt[3] + (BUFFER_CELLS + ny) * gt[5]
    extent = (x0, x0 + nx * gt[1], y0, y0 - ny * gt[5])
    mask = np.isfinite(ignition) & (ignition > 0.0)
    fig, axis = plt.subplots(figsize=(8.2, 4.5), constrained_layout=True)
    axis.imshow(mask, origin='upper', extent=extent, aspect='equal',
                cmap='Reds', interpolation='nearest')
    axis.set(title='Cells ignited by deposited firebrands',
             xlabel='x [m]', ylabel='y [m]', xlim=(0, 800), ylim=(-100, 200))
    axis.grid(alpha=0.15)
    polish_figure(fig)
    fig.savefig(
        figs / 'two_dimensional_transport_fields.pdf',
        bbox_inches='tight', pad_inches=0.06)
    plt.close(fig)




def main():
    """Select target perimeters, calculate metrics, and write report artifacts."""
    out = CASE_DIR / 'outputs'
    figs = CASE_DIR / 'figures'
    report = CASE_DIR / 'report'
    figs.mkdir(exist_ok=True)
    report.mkdir(exist_ok=True)
    metrics = {
        'case_id': 'CASE06_F2D',
        'status': 'incomplete',
        'verification_passed': 'not_evaluated',
        'required_times_s': TARGET_TIMES_S,
        'maximum_time_mismatch_s': MAX_TIME_MISMATCH_S,
        'maximum_nesting_violation_fraction': MAX_NESTING_VIOLATION_FRACTION,
        'minimum_front_advance_per_interval_m': MIN_FRONT_ADVANCE_PER_INTERVAL_M,
        'minimum_final_crosswind_span_m': MIN_FINAL_CROSSWIND_SPAN_M,
        'minimum_firebrand_excess_advance_m': MIN_FIREBRAND_EXCESS_ADVANCE_M,
        'maximum_observed_nesting_violation_fraction': 'not computed',
        'interval_front_advances_m': 'not computed',
        'final_crosswind_span_m': 'not computed',
        'firebrand_excess_advance_m': 'not computed',
        'ember_ignition_cells': 'not computed',
        'nesting_passed': 'NOT EVALUABLE',
        'front_progression_passed': 'NOT EVALUABLE',
        'two_dimensional_growth_passed': 'NOT EVALUABLE',
        'firebrand_driven_advance_passed': 'NOT EVALUABLE'}
    times = dump_map(out)
    candidates = []
    for p in sorted(out.glob('phi_*_d*.tif')):
        m = re.search('_d(\\d+)\\.tif$', p.name)
        if m and int(m.group(1)) in times:
            candidates.append((times[int(m.group(1))], p))
    selected = []
    for target in TARGET_TIMES_S:
        if not candidates:
            break
        item = min(candidates, key=lambda q: abs(q[0] - target))
        already_selected = {path for _, path in selected}
        if (
            abs(item[0] - target) <= MAX_TIME_MISMATCH_S
            and item[1] not in already_selected
        ):
            selected.append(item)
    metrics['completed_perimeter_times_s'] = [round(t, 6) for (t, _) in selected]
    metrics['required_perimeters_complete'] = len(selected) == 4
    if len(selected) == 4:
        arrays = []
        masks = []
        xmaxs = []
        ymins = []
        ymaxs = []
        gt = None
        for (actual, path) in selected:
            (raw, gt) = read_raster(path)
            a = raw[BUFFER_CELLS:-BUFFER_CELLS, BUFFER_CELLS:-BUFFER_CELLS]
            burned = np.isfinite(a) & (a > NODATA_LIMIT) & (a <= 0.0)
            (rows, cols) = np.where(burned)
            if len(cols) == 0:
                raise RuntimeError(f'No burned cells in {path.name}')
            x = gt[0] + (np.arange(BUFFER_CELLS,
                                   raw.shape[1] - BUFFER_CELLS) + 0.5) * gt[1]
            y = gt[3] + (np.arange(BUFFER_CELLS,
                                   raw.shape[0] - BUFFER_CELLS) + 0.5) * gt[5]
            arrays.append(a)
            masks.append(burned)
            xmaxs.append(float(x[cols].max()))
            ymins.append(float(y[rows].min()))
            ymaxs.append(float(y[rows].max()))
        violations = [float(np.count_nonzero(masks[i] & np.logical_not(
            masks[i + 1])) / max(np.count_nonzero(masks[i]), 1)) for i in range(3)]
        advances = np.diff(xmaxs)
        final_span = ymaxs[-1] - ymins[-1]
        surface_bound = IGNITION_X_M + SURFACE_HEADFIRE_ROS_MPS * 120.0
        excess = xmaxs[-1] - surface_bound
        ignition_files = sorted(out.glob('ember_ignition*.tif'))
        ignition_cells = 0
        if ignition_files:
            (ign, _) = read_raster(ignition_files[-1])
            ignition_cells = int(np.count_nonzero(np.isfinite(ign) & (ign > 0)))
        nesting = max(violations) <= MAX_NESTING_VIOLATION_FRACTION
        advance = bool(np.all(advances >= MIN_FRONT_ADVANCE_PER_INTERVAL_M))
        span = final_span >= MIN_FINAL_CROSSWIND_SPAN_M
        ember = ignition_cells > 0 and excess >= MIN_FIREBRAND_EXCESS_ADVANCE_M
        passed = nesting and advance and span and ember
        metrics.update(
            status='pass' if passed else 'fail',
            verification_passed=passed,
            selected_phi_files=[path.name for _, path in selected],
            downwind_front_m=xmaxs,
            interval_front_advances_m=advances.tolist(),
            nesting_violation_fractions=violations,
            maximum_observed_nesting_violation_fraction=max(violations),
            final_crosswind_span_m=final_span,
            surface_only_front_bound_m=surface_bound,
            firebrand_excess_advance_m=excess,
            ember_ignition_cells=ignition_cells,
            nesting_passed=nesting,
            front_progression_passed=advance,
            two_dimensional_growth_passed=span,
            firebrand_driven_advance_passed=ember)
        (fig, ax) = plt.subplots(figsize=(9, 4.5), constrained_layout=True)
        colors = plt.cm.viridis(np.linspace(0.12, 0.9, 4))
        for ((actual, path), a, color) in zip(selected, arrays, colors):
            (ny, nx) = a.shape
            xs = gt[0] + (np.arange(BUFFER_CELLS, nx + BUFFER_CELLS) + 0.5) * gt[1]
            ys = gt[3] + (np.arange(BUFFER_CELLS, ny + BUFFER_CELLS) + 0.5) * gt[5]
            ax.contour(xs, ys, a, levels=[0.0], colors=[color], linewidths=2)
            ax.plot([], [], color=color, lw=2, label=f't = {actual:.1f} s')
        ax.scatter(
            [IGNITION_X_M],
            [IGNITION_Y_M],
            marker='*',
            s=80,
            color='black',
            label='initial ignition',
            zorder=5)
        ax.set(xlabel='x (m)', ylabel='y (m)', xlim=(0, 800), ylim=(-100, 200),
               title='ELMFIRE two-dimensional transport case: firebrand-driven 2-D fireline evolution')
        ax.set_aspect('equal', adjustable='box')
        ax.grid(alpha=0.2)
        ax.legend(ncol=3, loc='upper left')
        polish_figure(fig)
        fig.savefig(figs / 'two_dimensional_fireline_evolution.pdf')
        plt.close(fig)
        write_field_diagnostics(out, figs, gt)
    else:
        metrics['notes'] = 'Four transient phi rasters within the time tolerance are required.'
    (out / 'metrics.json').write_text(json.dumps(metrics, indent=2) + '\n')
    lines = []
    for (k, v) in metrics.items():
        if isinstance(v, bool):
            v = 'PASS' if v else 'FAIL'
        elif isinstance(v, list):
            v = ', '.join(
                (f'{x:.4g}' if isinstance(x, float) else str(x) for x in v))
        safe = re.sub('[^A-Za-z0-9]+', '', k)
        value = str(v).replace('_', '\\_')
        lines.append(f'\\expandafter\\def\\csname metric@{safe}\\endcsname{{{value}}}')
    (report / 'metrics_macros.tex').write_text(report_text('\n'.join(lines) + '\n'))
    print('[OK] two-dimensional transport case evaluated: {}'.format(metrics['status']))


if __name__ == '__main__':
    main()
    generate_spatial_evidence(
        CASE_DIR, output_preference=("ember_flux", "time_of_arrival"),
    )
