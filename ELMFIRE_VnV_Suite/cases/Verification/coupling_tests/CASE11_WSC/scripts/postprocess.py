#!/usr/bin/env python3
"""Evaluate the paired fine-grid WUI no-surface reference/surface-coupled variant experiment.

The script reads real ELMFIRE outputs for both current-fingerprint variants,
compares cell and structure ignition chronology, quantifies remaining
within-structure firebrand-load distortion, and writes standalone report
artifacts. It never runs ELMFIRE.
"""

from report_language import polish_figure, report_text

import rasterio
from spatial_evidence import generate_spatial_evidence
import numpy as np
from pathlib import Path
import json
import math

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# -----------------------------------------------------------------------------
# Customizable postprocessing parameters
# -----------------------------------------------------------------------------
FINAL_EMBER_GLOB = "ember_flux_[0-9]*.tif"
FINAL_TOA_GLOB = "time_of_arrival_[0-9]*.tif"
INVALID_OUTPUT_MIN = -1000.0
METRIC_DISPLAY_DIGITS = 5
FIGURE_DPI = 180
COLORS = {
    "no_surface": "#1f77b4",
    "surface_coupled": "#d62728",
}


def read_band(path):
    """Read one raster band and convert its NoData cells to NaN."""
    with rasterio.open(path) as dataset:
        values = dataset.read(1).astype(float)
        nodata = dataset.nodata
    if nodata is not None:
        values[np.isclose(values, nodata)] = np.nan
    return values


def final_output(directory, pattern):
    """Return the latest final raster while excluding transient dumps."""
    paths = [path for path in directory.glob(pattern) if "_transient_" not in path.name]
    return sorted(paths)[-1] if paths else None


def variant_profile(case_dir, case, item):
    """Extract cell-level and physical-structure profiles for one variant."""
    variant_dir = case_dir / item["working_directory"]
    fingerprint_path = variant_dir / "logs" / "completed_input_fingerprint.txt"
    if not fingerprint_path.exists():
        return None
    if fingerprint_path.read_text(
            encoding="utf-8").strip() != item["input_fingerprint"]:
        return None

    output_dir = variant_dir / "outputs"
    ember_path = final_output(output_dir, FINAL_EMBER_GLOB)
    toa_path = final_output(output_dir, FINAL_TOA_GLOB)
    if ember_path is None or toa_path is None:
        return None

    structure_ids = read_band(variant_dir / "data" / "inputs" / "structure_id.tif")
    ember_count = read_band(ember_path)
    toa = read_band(toa_path)
    row = int(item["center_row"])
    buffer_cells = int(item["buffer_cells"])
    stop = structure_ids.shape[1] - buffer_cells
    raw_ids = np.nan_to_num(
        structure_ids[row, buffer_cells:stop], nan=0.0
    ).astype(int)
    n_structures_x = int(item["n_structures_x"])
    raw_x_index = np.where(
        raw_ids > 0, (raw_ids - 1) % n_structures_x, -1
    )
    present_x = sorted(set(raw_x_index[raw_x_index >= 0]))
    ids = np.zeros(raw_ids.shape, dtype=int)
    for sequential_id, x_index in enumerate(present_x, start=1):
        ids[raw_x_index == x_index] = sequential_id
    counts = ember_count[row, buffer_cells:stop]
    arrival = toa[row, buffer_cells:stop]
    arrival[~np.isfinite(arrival) | (arrival <= INVALID_OUTPUT_MIN)] = np.nan

    dx = float(item["dx_m"])
    cell_area = dx * dx
    cell_load = counts / cell_area
    x = (np.arange(ids.size) + 0.5) * dx

    structure_rows = []
    for structure_id in sorted(set(ids[ids > 0])):
        mask = ids == structure_id
        represented_area = np.count_nonzero(mask) * cell_area
        valid_load = cell_load[mask]
        valid_load = valid_load[np.isfinite(valid_load)]
        valid_arrival = arrival[mask]
        valid_arrival = valid_arrival[np.isfinite(valid_arrival)]
        load_relative_range = math.nan
        if valid_load.size > 1 and abs(float(np.mean(valid_load))) > 1.0e-12:
            load_relative_range = float(
                (np.max(valid_load) - np.min(valid_load)) / np.mean(valid_load)
            )
        structure_rows.append({
            "id": int(structure_id),
            "x_m": float(np.mean(x[mask])),
            "load_pcs_m2": (
                float(np.nansum(counts[mask]) / represented_area)
                if valid_load.size else math.nan
            ),
            "load_relative_range": load_relative_range,
            "ignition_min_s": (
                float(np.min(valid_arrival)) if valid_arrival.size else math.nan
            ),
            "ignition_max_s": (
                float(np.max(valid_arrival)) if valid_arrival.size else math.nan
            ),
            "ignition_span_s": (
                float(np.max(valid_arrival) - np.min(valid_arrival))
                if valid_arrival.size > 1 else math.nan
            ),
        })

    return {
        "role": item["role"],
        "name": item["name"],
        "x_m": x,
        "structure_ids": ids,
        "cell_load_pcs_m2": cell_load,
        "cell_ignition_s": arrival,
        "structures": structure_rows,
        "ember_file": ember_path.name,
        "toa_file": toa_path.name,
    }


def finite_median(values):
    """Return the median of finite values, or NaN when none exist."""
    values = np.asarray(values, dtype=float)
    values = values[np.isfinite(values)]
    return float(np.median(values)) if values.size else math.nan


def structure_map(profile):
    """Index one profile's structure summaries by physical structure ID."""
    return {row["id"]: row for row in profile["structures"]}


def mean_ros(profile):
    """Fit first-ignition time against downwind structure position."""
    rows = [
        row for row in profile["structures"]
        if row["id"] >= 2 and np.isfinite(row["ignition_min_s"])
    ]
    if len(rows) < 2:
        return math.nan
    x = np.asarray([row["x_m"] for row in rows])
    t = np.asarray([row["ignition_min_s"] for row in rows])
    slope = float(np.polyfit(x, t, 1)[0])
    return 1.0 / slope if slope > 0.0 else math.nan


def metric(name, limit, value=None, passed=None, note=""):
    """Build one complete verification row with no blank calculated entry."""
    if value is None or (
        isinstance(value, (int, float, np.floating)) and not np.isfinite(value)
    ):
        return {
            "metric": name,
            "limit": limit,
            "calculated": "N/A",
            "status": "NOT EVALUABLE",
            "note": note or "The required finite comparison was unavailable.",
        }
    calculated = (
        f"{float(value):.{METRIC_DISPLAY_DIGITS}g}"
        if isinstance(value, (int, float, np.floating)) else str(value)
    )
    return {
        "metric": name,
        "limit": limit,
        "calculated": calculated,
        "status": "PASS" if passed else "FAIL",
        "note": note,
    }


def calculate_metrics(case, manifest, profiles):
    """Evaluate the explicit surface-coupled variant comparative acceptance contract."""
    limits = case["metrics"]
    rows = [
        metric(
            "Output completeness",
            "2 current-fingerprint variants",
            f"{len(profiles)}/2",
            len(profiles) == 2,
        )
    ]
    baseline = profiles.get("no_surface")
    coupled = profiles.get("surface_coupled")
    diagnostics = {}

    if baseline is None or coupled is None:
        rows.extend([
            metric(
                "Coupled earlier-ignition cell fraction",
                f">= {limits['coupled_earlier_cell_fraction_min']}",
            ),
            metric(
                "Coupled median ignition-time ratio",
                f"<= {limits['coupled_median_ignition_ratio_max']}",
            ),
            metric(
                "Coupled/no-surface ignition-span ratio",
                f"<= {limits['coupled_ignition_span_ratio_max']}",
            ),
            metric(
                "Coupled/no-surface mean-ROS ratio",
                f">= {limits['coupled_ros_ratio_min']}",
            ),
            metric(
                "Coupled within-structure load relative range",
                f">= {limits['coupled_load_relative_range_min']}",
            ),
        ])
        return rows, diagnostics, "NOT RUN"

    matched_cells = (
        (baseline["structure_ids"] >= 2)
        & (coupled["structure_ids"] == baseline["structure_ids"])
        & np.isfinite(baseline["cell_ignition_s"])
        & np.isfinite(coupled["cell_ignition_s"])
        & (baseline["cell_ignition_s"] > 0.0)
    )
    if np.any(matched_cells):
        earlier_fraction = float(np.mean(
            coupled["cell_ignition_s"][matched_cells]
            < baseline["cell_ignition_s"][matched_cells]
        ))
        ignition_ratio = finite_median(
            coupled["cell_ignition_s"][matched_cells]
            / baseline["cell_ignition_s"][matched_cells]
        )
    else:
        earlier_fraction = math.nan
        ignition_ratio = math.nan

    base_structures = structure_map(baseline)
    coupled_structures = structure_map(coupled)
    common_ids = sorted(set(base_structures).intersection(coupled_structures))
    span_ratios = []
    for structure_id in common_ids:
        if structure_id < 2:
            continue
        base_span = base_structures[structure_id]["ignition_span_s"]
        coupled_span = coupled_structures[structure_id]["ignition_span_s"]
        if np.isfinite(base_span) and np.isfinite(coupled_span) and base_span > 1.0e-9:
            span_ratios.append(coupled_span / base_span)
    span_ratio = finite_median(span_ratios)

    base_ros = mean_ros(baseline)
    coupled_ros = mean_ros(coupled)
    ros_ratio = (
        coupled_ros / base_ros
        if np.isfinite(base_ros) and np.isfinite(coupled_ros) and base_ros > 0.0
        else math.nan
    )
    coupled_load_range = finite_median([
        row["load_relative_range"]
        for row in coupled["structures"]
        if row["id"] >= 2
    ])

    rows.extend([
        metric(
            "Coupled earlier-ignition cell fraction",
            f">= {limits['coupled_earlier_cell_fraction_min']}",
            earlier_fraction,
            earlier_fraction >= limits["coupled_earlier_cell_fraction_min"]
            if np.isfinite(earlier_fraction) else None,
        ),
        metric(
            "Coupled median ignition-time ratio",
            f"<= {limits['coupled_median_ignition_ratio_max']}",
            ignition_ratio,
            ignition_ratio <= limits["coupled_median_ignition_ratio_max"]
            if np.isfinite(ignition_ratio) else None,
        ),
        metric(
            "Coupled/no-surface ignition-span ratio",
            f"<= {limits['coupled_ignition_span_ratio_max']}",
            span_ratio,
            span_ratio <= limits["coupled_ignition_span_ratio_max"]
            if np.isfinite(span_ratio) else None,
        ),
        metric(
            "Coupled/no-surface mean-ROS ratio",
            f">= {limits['coupled_ros_ratio_min']}",
            ros_ratio,
            ros_ratio >= limits["coupled_ros_ratio_min"]
            if np.isfinite(ros_ratio) else None,
        ),
        metric(
            "Coupled within-structure load relative range",
            f">= {limits['coupled_load_relative_range_min']}",
            coupled_load_range,
            coupled_load_range >= limits["coupled_load_relative_range_min"]
            if np.isfinite(coupled_load_range) else None,
        ),
    ])
    diagnostics = {
        "matched_downstream_structure_cells": int(np.count_nonzero(matched_cells)),
        "no_surface_mean_ros_m_s": base_ros if np.isfinite(base_ros) else None,
        "surface_coupled_mean_ros_m_s": coupled_ros if np.isfinite(coupled_ros) else None,
        "reference_surface_ros_reference_m_s": case["surface_ros_reference_m_s"],
        "no_surface_median_ignition_span_s": finite_median([
            row["ignition_span_s"] for row in baseline["structures"] if row["id"] >= 2
        ]),
        "surface_coupled_median_ignition_span_s": finite_median([
            row["ignition_span_s"] for row in coupled["structures"] if row["id"] >= 2
        ]),
        "no_surface_median_load_relative_range": finite_median([
            row["load_relative_range"] for row in baseline["structures"] if row["id"] >= 2
        ]),
        "surface_coupled_median_load_relative_range": coupled_load_range,
    }
    overall = (
        "NOT EVALUABLE"
        if any(row["status"] == "NOT EVALUABLE" for row in rows)
        else ("PASS" if all(row["status"] == "PASS" for row in rows) else "FAIL")
    )
    return rows, diagnostics, overall


def plot_surface_coupled_reference(case_dir, case, profiles):
    """Reproduce the two physical profiles for the surface-coupled experiment."""
    profile = profiles.get("surface_coupled")
    fig, axes = plt.subplots(1, 2, figsize=(11.0, 4.2), constrained_layout=True)
    if profile is None:
        for axis in axes:
            axis.text(0.5, 0.5, "Current matching output not available",
                      ha="center", va="center", transform=axis.transAxes)
    else:
        mask = profile["structure_ids"] > 0
        axes[0].plot(profile["x_m"][mask], profile["cell_load_pcs_m2"][mask],
                     color=COLORS["surface_coupled"], marker="o", markersize=2.8,
                     linewidth=1.1, label="ELMFIRE")
        axes[1].plot(profile["x_m"][mask], profile["cell_ignition_s"][mask],
                     color=COLORS["surface_coupled"], marker="o", markersize=2.8,
                     linewidth=1.1, label="ELMFIRE")
    axes[0].axhline(case["critical_load_pcs_m2"], color="0.25", linestyle="--",
                    linewidth=1.1, label="critical ignition load")
    axes[0].set(xlabel="Downwind distance [m]",
                ylabel=r"Accumulated firebrands [pcs/m$^2$]",
                title="(a) Accumulated load", ylim=(0.0, None))
    axes[1].set(xlabel="Downwind distance [m]", ylabel="Ignition time [s]",
                title="(b) Structural-cell ignition chronology", ylim=(0.0, None))
    for axis in axes:
        axis.grid(alpha=0.2)
        handles, _ = axis.get_legend_handles_labels()
        if handles:
            axis.legend(fontsize=8)
    polish_figure(fig)
    fig.savefig(case_dir / "figures" / "surface_coupled_reference.pdf",
                dpi=FIGURE_DPI)
    plt.close(fig)


def plot_results(case_dir, case, profiles):
    """Create the supplemental paired load and ignition comparison figure."""
    fig, axes = plt.subplots(1, 2, figsize=(11.0, 4.2), constrained_layout=True)
    labels = {
        "no_surface": r"no-surface reference: $ROS_{surface}=0$",
        "surface_coupled": "surface-coupled variant: surface spread active",
    }
    for role in ("no_surface", "surface_coupled"):
        profile = profiles.get(role)
        if profile is None:
            continue
        mask = profile["structure_ids"] > 0
        axes[0].plot(
            profile["x_m"][mask],
            profile["cell_load_pcs_m2"][mask],
            marker="o",
            markersize=2.8,
            linewidth=1.1,
            color=COLORS[role],
            label=labels[role],
        )
        axes[1].plot(
            profile["x_m"][mask],
            profile["cell_ignition_s"][mask],
            marker="o",
            markersize=2.8,
            linewidth=1.1,
            color=COLORS[role],
            label=labels[role],
        )

    axes[0].axhline(
        case["critical_load_pcs_m2"],
        color="0.25",
        linestyle="--",
        linewidth=1.1,
        label="critical load",
    )
    axes[0].set_xlabel("Downwind distance (m)")
    axes[0].set_ylabel("Accumulated firebrands (pcs/m$^2$)")
    axes[0].set_title("(a) Cell-level accumulated load")
    axes[0].set_ylim(bottom=0.0)
    axes[0].grid(alpha=0.25)
    axes[0].legend(fontsize=8)

    axes[1].set_xlabel("Downwind distance (m)")
    axes[1].set_ylabel("Ignition time (s)")
    axes[1].set_title("(b) Structural-cell ignition chronology")
    axes[1].set_ylim(bottom=0.0)
    axes[1].grid(alpha=0.25)
    axes[1].legend(fontsize=8)
    polish_figure(fig)
    fig.savefig(
        case_dir /
        "figures" /
        "surface_coupling_comparison.pdf",
        dpi=FIGURE_DPI)
    plt.close(fig)


def latex_escape(value):
    """Escape dynamically generated table text."""
    text = str(value)
    for old, new in (
        ("\\", r"\textbackslash{}"),
        ("_", r"\_"),
        ("%", r"\%"),
        ("&", r"\&"),
        ("#", r"\#"),
        ("<=", r"$\leq$"),
        (">=", r"$\geq$"),
    ):
        text = text.replace(old, new)
    return text


def write_artifacts(case_dir, case, manifest, profiles, rows, diagnostics, overall):
    """Write JSON evidence and complete LaTeX table macros."""
    output = {
        "case_id": case["id"],
        "overall_status": overall,
        "completed_variants": sorted(profiles),
        "unrun_variants": [
            item["name"] for item in manifest if item["role"] not in profiles
        ],
        "metrics": rows,
        "diagnostics": diagnostics,
        "controlled_difference": (
            "NO_SURFACE_FIRE=.TRUE. for no-surface reference and .FALSE. for surface-coupled variant; "
            "all generated rasters and other namelist controls are identical"
        ),
    }
    output_dir = case_dir / "outputs"
    output_dir.mkdir(exist_ok=True)
    (output_dir / "metrics.json").write_text(
        json.dumps(output, indent=2) + "\n", encoding="utf-8"
    )

    table_rows = [
        "{} & {} & {} & {} \\\\".format(
            latex_escape(row["metric"]),
            latex_escape(row["limit"]),
            latex_escape(row["calculated"]),
            latex_escape(row["status"]),
        )
        for row in rows
    ]
    macros = [
        rf"\def\OverallStatus{{{latex_escape(overall)}}}",
        rf"\def\CompletedVariantCount{{{len(profiles)}}}",
        rf"\def\RequestedVariantCount{{{len(manifest)}}}",
        r"\def\MetricRows{",
        *table_rows,
        "}",
    ]
    report_dir = case_dir / "report"
    report_dir.mkdir(exist_ok=True)
    (report_dir / "metrics_macros.tex").write_text(
        report_text("\n".join(macros) + "\n"), encoding="utf-8"
    )


def main():
    """Load current outputs, calculate metrics, and refresh report artifacts."""
    case_dir = Path(__file__).resolve().parents[1]
    case = json.loads((case_dir / "case.json").read_text(encoding="utf-8"))
    manifest = json.loads(
        (case_dir / "variants" / "manifest.json").read_text(encoding="utf-8")
    )
    profiles = {}
    for item in manifest:
        profile = variant_profile(case_dir, case, item)
        if profile is not None:
            profiles[item["role"]] = profile
    rows, diagnostics, overall = calculate_metrics(case, manifest, profiles)
    (case_dir / "figures").mkdir(exist_ok=True)
    plot_surface_coupled_reference(case_dir, case, profiles)
    plot_results(case_dir, case, profiles)
    write_artifacts(
        case_dir, case, manifest, profiles, rows, diagnostics, overall
    )
    print(
        f"[OK] postprocessed {len(profiles)}/{len(manifest)} variants; "
        f"status: {overall}"
    )


if __name__ == "__main__":
    main()
    generate_spatial_evidence(
        Path(__file__).resolve().parents[1],
        output_preference=("time_of_arrival", "ember_flux"),
        preferred_variant="surface_coupled_dx2p5",
    )
