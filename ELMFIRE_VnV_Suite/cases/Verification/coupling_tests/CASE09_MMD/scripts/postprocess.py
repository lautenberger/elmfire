#!/usr/bin/env python3
"""Evaluate the paired surface-dominated variant/mixed-mode variant ELMFIRE verification.

Only real GeoTIFF outputs beneath variants/ are treated as observations.
Analytical surface-fire curves are labelled as expectations. Missing or
incomplete model output is explicitly non-passing.
"""

from report_language import polish_figure, report_text

from spatial_evidence import generate_spatial_evidence
import numpy as np
import rasterio
from pathlib import Path
import json
import re

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt


# -----------------------------------------------------------------------------
# Customizable postprocessing parameters and acceptance limits
# These values are declared before processing logic so verification decisions
# can be reviewed or tightened without searching through implementation code.
# -----------------------------------------------------------------------------
MANIFEST_FILENAME = "variants/manifest.json"
FIGURE_FILENAME = "figures/mixed_mode_delay_effect.pdf"
BUFFER_CELLS_OVERRIDE = None
FIT_DISTANCE_MIN_M = 50.0
FIT_DISTANCE_MAX_M = 280.0
LEADING_EDGE_COMPARISON_TIME_S = 250.0
MIN_FIT_CELLS = 12
SPREAD_RATE_FTMIN_TO_MPS = 0.3048 / 60.0

SURFACE_DOMINATED_TOA_MEAN_REL_ERROR_LIMIT = 0.10
SURFACE_DOMINATED_ROS_REL_ERROR_LIMIT = 0.10
SURFACE_DOMINATED_MAX_EMBER_IGNITION_CELLS = 0
MIXED_MODE_MIN_EMBER_IGNITION_CELLS = 1
MIXED_MODE_MIN_ROS_RATIO_TO_SURFACE = 1.10
MIXED_MODE_MIN_LEADING_EDGE_GAIN_M = 20.0
MIXED_MODE_ACCELERATION_ONSET_MIN_S = 120.0
MIXED_MODE_ACCELERATION_ONSET_MAX_S = 220.0
PAIR_MIN_MEDIAN_TOA_LEAD_S = 20.0

COLORS = {
    "surface_dominated": "#1f77b4",
    "mixed_mode": "#d62728",
}


def read_json(path):
    """Read a UTF-8 JSON file."""
    return json.loads(Path(path).read_text(encoding="utf-8"))


def final_raster(output_dir, stem):
    """Select the latest final raster, excluding transient dump rasters."""
    candidates = [
        path for path in Path(output_dir).glob(f"{stem}*.tif")
        if "transient" not in path.name.lower() and "_d" not in path.stem.lower()
    ]
    return sorted(candidates)[-1] if candidates else None


def read_raster(path):
    """Read one GeoTIFF and return its array and georeferencing."""
    try:
        with rasterio.open(path) as dataset:
            return (
                dataset.read(1).astype(float),
                dataset.transform.to_gdal(),
                dataset.nodata,
            )
    except rasterio.errors.RasterioIOError as error:
        raise RuntimeError(f"Rasterio could not open {path}") from error


def valid_values(array, nodata):
    """Convert ELMFIRE nodata/sentinel values into NaNs."""
    clean = np.asarray(array, dtype=float).copy()
    clean[~np.isfinite(clean)] = np.nan
    clean[clean <= -9000.0] = np.nan
    if nodata is not None:
        clean[np.isclose(clean, nodata)] = np.nan
    return clean


def centre_profile(array, buffer_cells, average_rows=False):
    """Extract the physical downwind profile, excluding all halo cells."""
    physical = array[
        buffer_cells:array.shape[0] - buffer_cells,
        buffer_cells:array.shape[1] - buffer_cells,
    ]
    if average_rows:
        # Compute column means explicitly so all-NaN downwind columns remain
        # NaN without emitting NumPy's "mean of empty slice" warning.
        valid_count = np.count_nonzero(np.isfinite(physical), axis=0)
        column_sum = np.nansum(physical, axis=0)
        return np.divide(
            column_sum,
            valid_count,
            out=np.full(column_sum.shape, np.nan, dtype=float),
            where=valid_count > 0,
        )
    return physical[physical.shape[0] // 2, :]


def profile_coordinates(transform, nx, buffer_cells):
    """Return physical cell-centre downwind distance from the ignition line."""
    columns = np.arange(buffer_cells, nx - buffer_cells)
    x = transform[0] + (columns + 0.5) * transform[1]
    return x - x[0]


def fit_mean_ros(distance, toa):
    """Fit t=a+x/U over the declared interval and return U in m/s."""
    mask = (
        np.isfinite(distance) & np.isfinite(toa) & (toa >= 0.0)
        & (distance >= FIT_DISTANCE_MIN_M) & (distance <= FIT_DISTANCE_MAX_M)
    )
    if np.count_nonzero(mask) < MIN_FIT_CELLS:
        return np.nan, int(np.count_nonzero(mask))
    slope, _ = np.polyfit(distance[mask], toa[mask], 1)
    return (1.0 / slope if slope > 0.0 else np.nan), int(np.count_nonzero(mask))


def leading_edge_history(distance, toa, tstop, dt_plot=5.0):
    """Reconstruct x_LE(t) and a smoothed finite-difference ROS from final TOA."""
    times = np.arange(0.0, tstop + 0.5 * dt_plot, dt_plot)
    edge = np.zeros_like(times)
    finite = np.isfinite(toa) & (toa >= 0.0)
    for index, time_value in enumerate(times):
        reached = finite & (toa <= time_value)
        edge[index] = np.max(distance[reached]) if np.any(reached) else 0.0
    ros = np.gradient(edge, times)
    if ros.size >= 5:
        ros = np.convolve(ros, np.ones(5) / 5.0, mode="same")
    return times, edge, ros


def first_acceleration_time(times, edge, surface_ros, dx):
    """Find when the leading edge first exceeds surface spread by one cell."""
    gain = edge - surface_ros * times
    indices = np.flatnonzero(gain >= dx)
    return float(times[indices[0]]) if indices.size else np.nan


def bool_word(value):
    """Use compact human-readable values in JSON and the report table."""
    return "PASS" if value else "FAIL"


def latex_escape(value):
    """Escape scalar values written into csname-backed LaTeX macros."""
    text = str(value)
    return (
        text.replace("\\", r"\textbackslash{}")
        .replace("_", r"\_")
        .replace("%", r"\%")
        .replace("&", r"\&")
        .replace("#", r"\#")
    )


def format_metric(value, digits=3):
    """Format finite numerical results and preserve explicit N/A values."""
    if isinstance(value, (float, np.floating)):
        return f"{value:.{digits}f}" if np.isfinite(value) else "N/A"
    return value


def analyze_variant(case_dir, manifest, variant):
    """Read and reduce one variant's actual ELMFIRE output."""
    variant_dir = case_dir / variant["directory"]
    output_dir = variant_dir / variant["outputs"]
    paths = {
        "toa": final_raster(output_dir, "time_of_arrival"),
        "vs": final_raster(output_dir, "vs"),
        "embers": final_raster(output_dir, "ember_flux"),
        "ember_ignition": final_raster(output_dir, "ember_ignition"),
    }
    result = {
        "name": variant["name"],
        "variant_label": variant["variant_label"],
        "complete": all(
            path is not None for path in paths.values()),
        "selected_files": {
            key: (
                path.name if path else None) for key,
            path in paths.items()},
    }
    if not result["complete"]:
        return result

    toa_array, transform, toa_nodata = read_raster(paths["toa"])
    vs_array, vs_transform, vs_nodata = read_raster(paths["vs"])
    ember_array, ember_transform, ember_nodata = read_raster(paths["embers"])
    ign_array, ign_transform, ign_nodata = read_raster(paths["ember_ignition"])
    dx = float(manifest["dx_m"])
    if not np.isclose(abs(transform[1]), dx):
        raise ValueError(f"{variant['name']}: TOA cell size does not match manifest")
    if (toa_array.shape != vs_array.shape or toa_array.shape != ember_array.shape
            or toa_array.shape != ign_array.shape):
        raise ValueError(f"{variant['name']}: required rasters are not co-registered")
    if transform != vs_transform or transform != ember_transform or transform != ign_transform:
        raise ValueError(f"{variant['name']}: required raster geotransforms differ")

    buffer_cells = int(
        manifest["buffer_cells"] if BUFFER_CELLS_OVERRIDE is None
        else BUFFER_CELLS_OVERRIDE
    )
    toa_array = valid_values(toa_array, toa_nodata)
    vs_array = valid_values(vs_array, vs_nodata)
    ember_array = valid_values(ember_array, ember_nodata)
    ign_array = valid_values(ign_array, ign_nodata)
    distance = profile_coordinates(transform, toa_array.shape[1], buffer_cells)
    toa = centre_profile(toa_array, buffer_cells)
    vs_ftmin = centre_profile(vs_array, buffer_cells)
    # ELMFIRE's default VS GeoTIFF units are ft/min. The positive median,
    # excluding the partially initialized ignition cell, is the Rothermel
    # head-fire ROS used as the surface-spread reference for this executable.
    valid_vs = vs_ftmin[1:][np.isfinite(vs_ftmin[1:]) & (vs_ftmin[1:] > 0.0)]
    rothermel_ros = (
        float(np.median(valid_vs) * SPREAD_RATE_FTMIN_TO_MPS)
        if valid_vs.size else np.nan
    )
    embers = centre_profile(ember_array, buffer_cells, average_rows=True)
    physical_ignition = ign_array[
        buffer_cells:ign_array.shape[0] - buffer_cells,
        buffer_cells:ign_array.shape[1] - buffer_cells,
    ]
    ember_ignition_cells = int(np.count_nonzero(np.nan_to_num(physical_ignition) > 0.0))
    mean_ros, fit_cells = fit_mean_ros(distance, toa)
    times, edge, ros = leading_edge_history(
        distance, toa, float(variant["tstop_s"])
    )
    result.update({
        "distance_m": distance,
        "toa_s": toa,
        "accumulated_embers": embers,
        "ember_ignition_cells": ember_ignition_cells,
        "mean_ros_mps": mean_ros,
        "rothermel_ros_mps": rothermel_ros,
        "fit_cells": fit_cells,
        "times_s": times,
        "leading_edge_m": edge,
        "leading_edge_ros_mps": ros,
    })
    return result


def make_figure(case_dir, manifest, results):
    """Create a reference-style four-panel comparison from actual model outputs."""
    full = results.get("surface_dominated", {})
    surface_ros = float(
        full.get("rothermel_ros_mps", manifest["surface_ros_reference_mps"])
    )
    if not np.isfinite(surface_ros):
        surface_ros = float(manifest["surface_ros_reference_mps"])
    length = float(manifest["physical_length_m"])
    max_tstop = max(float(v["tstop_s"]) for v in manifest["variants"])
    expected_x = np.linspace(0.0, length, 301)
    expected_t = expected_x / surface_ros
    expected_times = np.linspace(0.0, max_tstop, 301)

    fig, axes = plt.subplots(2, 2, figsize=(10.0, 7.2), constrained_layout=True)
    ax_ember, ax_toa, ax_edge, ax_ros = axes.flat

    for variant in manifest["variants"]:
        name = variant["name"]
        result = results.get(name, {"complete": False})
        label = variant["variant_label"]
        color = COLORS[name]
        if result["complete"]:
            ax_ember.plot(result["distance_m"], result["accumulated_embers"],
                          lw=1.8, color=color, label=label)
            ax_toa.plot(result["distance_m"], result["toa_s"],
                        lw=1.8, color=color, label=label)
            ax_edge.plot(result["times_s"], result["leading_edge_m"],
                         lw=1.8, color=color, label=label)
            ax_ros.plot(result["times_s"], result["leading_edge_ros_mps"],
                        lw=1.4, color=color, label=label)

    ax_toa.plot(expected_x, expected_t, "k--", lw=1.3,
                label=r"surface expectation $x/U_s$")
    ax_edge.plot(expected_times, surface_ros * expected_times, "k--", lw=1.3,
                 label=r"surface expectation $U_s t$")
    ax_ros.axhline(surface_ros, color="k", ls="--", lw=1.3,
                   label=r"surface expectation $U_s$")

    if not any(result.get("complete", False) for result in results.values()):
        for axis in axes.flat:
            axis.text(0.5, 0.52, "ELMFIRE variants not run",
                      transform=axis.transAxes, ha="center", va="center",
                      color="0.35", bbox={"facecolor": "white", "alpha": 0.8})

    ax_ember.set(xlabel="Downwind distance (m)",
                 ylabel="Accumulated firebrands (pcs per cell)", xlim=(0, length))
    ax_toa.set(xlabel="Downwind distance (m)", ylabel="Time of arrival (s)",
               xlim=(0, length))
    ax_toa.set_ylim(bottom=0)
    ax_edge.set(xlabel="Simulation time (s)", ylabel="Leading-edge position (m)",
                xlim=(0, max_tstop))
    ax_edge.set_ylim(bottom=0)
    ax_ros.set(xlabel="Simulation time (s)", ylabel=r"Leading-edge ROS (m s$^{-1}$)",
               xlim=(0, max_tstop))
    ax_ros.set_ylim(bottom=0)
    for axis in axes.flat:
        axis.grid(True, alpha=0.25)
        handles, labels = axis.get_legend_handles_labels()
        if handles:
            axis.legend(fontsize=8)
    figure_path = case_dir / FIGURE_FILENAME
    figure_path.parent.mkdir(parents=True, exist_ok=True)
    polish_figure(fig)
    fig.savefig(figure_path, format="pdf")
    plt.close(fig)


def make_variant_reference_figures(case_dir, manifest, results):
    """Write one four-panel physical-state figure for each ignition regime."""
    full = results.get("surface_dominated", {})
    surface_ros = float(
        full.get("rothermel_ros_mps", manifest["surface_ros_reference_mps"])
    )
    if not np.isfinite(surface_ros):
        surface_ros = float(manifest["surface_ros_reference_mps"])
    length = float(manifest["physical_length_m"])
    for variant in manifest["variants"]:
        name = variant["name"]
        result = results.get(name, {"complete": False})
        fig, axes = plt.subplots(2, 2, figsize=(10.0, 7.2), constrained_layout=True)
        if result.get("complete", False):
            color = COLORS[name]
            axes[0, 0].plot(result["distance_m"], result["accumulated_embers"],
                            color=color, linewidth=1.5, label="ELMFIRE")
            axes[0, 1].plot(result["distance_m"], result["toa_s"],
                            color=color, marker="o", markersize=2.0,
                            linewidth=1.0, label="ELMFIRE")
            axes[1, 0].plot(result["times_s"], result["leading_edge_m"],
                            color=color, marker="o", markersize=2.0,
                            linewidth=1.0, label="ELMFIRE")
            axes[1, 1].plot(result["times_s"], result["leading_edge_ros_mps"],
                            color=color, marker="o", markersize=1.8,
                            linewidth=0.9, label="ELMFIRE")
        else:
            for axis in axes.flat:
                axis.text(0.5, 0.5, "Current matching output not available",
                          ha="center", va="center", transform=axis.transAxes)
        expected_x = np.linspace(0.0, length, 301)
        expected_times = np.linspace(0.0, float(variant["tstop_s"]), 301)
        axes[0, 1].plot(expected_x, expected_x / surface_ros, "k-",
                        label=r"surface reference $T=x/R_s$")
        axes[1, 0].plot(expected_times, surface_ros * expected_times, "k-",
                        label=r"surface reference $x_{LE}=R_s t$")
        axes[1, 1].axhline(surface_ros, color="black",
                           label=r"surface reference $ROS=R_s$")
        axes[0, 0].set(xlabel="Downwind distance [m]",
                       ylabel="Accumulated firebrands [pcs/cell]",
                       xlim=(0.0, length), ylim=(0.0, None))
        axes[0, 1].set(xlabel="Downwind distance [m]", ylabel="Time of arrival [s]",
                       xlim=(0.0, length), ylim=(0.0, None))
        axes[1, 0].set(xlabel="Time [s]", ylabel="Leading-edge position [m]",
                       xlim=(0.0, float(variant["tstop_s"])), ylim=(0.0, None))
        axes[1, 1].set(xlabel="Time [s]", ylabel="Leading-edge ROS [m/s]",
                       xlim=(0.0, float(variant["tstop_s"])), ylim=(0.0, None))
        for label, axis in zip(("(a)", "(b)", "(c)", "(d)"), axes.flat):
            axis.text(0.01, 0.98, label, transform=axis.transAxes,
                      ha="left", va="top", fontweight="bold")
            axis.grid(alpha=0.2)
            handles, _ = axis.get_legend_handles_labels()
            if handles:
                axis.legend(fontsize=8)
        polish_figure(fig)
        fig.savefig(case_dir / "figures" / f"{name}_reference.pdf", format="pdf")
        plt.close(fig)


def postprocess(case_dir):
    """Calculate declared metrics, make the PDF figure, and write report macros."""
    case_dir = Path(case_dir)
    manifest_path = case_dir / MANIFEST_FILENAME
    if not manifest_path.is_file():
        raise FileNotFoundError("Run scripts/preprocess.py before postprocessing")
    manifest = read_json(manifest_path)
    results = {
        variant["name"]: analyze_variant(case_dir, manifest, variant)
        for variant in manifest["variants"]
    }
    make_variant_reference_figures(case_dir, manifest, results)
    make_figure(case_dir, manifest, results)

    full = results["surface_dominated"]
    mixed = results["mixed_mode"]
    surface_ros = float(
        full.get("rothermel_ros_mps", manifest["surface_ros_reference_mps"])
    )
    if not np.isfinite(surface_ros):
        surface_ros = float(manifest["surface_ros_reference_mps"])
    metrics = {
        "case_id": manifest["case_id"],
        "status": "insufficient_output",
        "verification_passed": "NOT EVALUABLE",
        "required_variants": 2,
        "completed_variants": int(full["complete"]) + int(mixed["complete"]),
        "surface_ros_reference_mps": format_metric(surface_ros),
        "surface_dominated_toa_mean_relative_error": "N/A",
        "surface_dominated_toa_status": "NOT EVALUABLE",
        "surface_dominated_mean_ros_mps": "N/A",
        "surface_dominated_ros_relative_error": "N/A",
        "surface_dominated_ros_status": "NOT EVALUABLE",
        "surface_dominated_ember_ignition_cells": "N/A",
        "surface_dominated_ignition_status": "NOT EVALUABLE",
        "mixed_mode_mean_ros_mps": "N/A",
        "mixed_mode_ros_ratio_to_surface": "N/A",
        "mixed_mode_ros_status": "NOT EVALUABLE",
        "mixed_mode_ember_ignition_cells": "N/A",
        "mixed_mode_ignition_status": "NOT EVALUABLE",
        "mixed_mode_leading_edge_gain_m": "N/A",
        "mixed_mode_edge_gain_status": "NOT EVALUABLE",
        "mixed_mode_acceleration_onset_s": "N/A",
        "mixed_mode_onset_status": "NOT EVALUABLE",
        "paired_median_toa_lead_s": "N/A",
        "paired_toa_status": "NOT EVALUABLE",
        "notes": "Both ELMFIRE variants must produce final TOA, ember accumulation, and ember-ignition rasters.",
    }
    checks = []

    if full["complete"] and mixed["complete"]:
        distance = full["distance_m"]
        reference_toa = distance / surface_ros
        mask = (
            np.isfinite(full["toa_s"]) & (reference_toa > 0.0)
            & (distance >= FIT_DISTANCE_MIN_M) & (distance <= FIT_DISTANCE_MAX_M)
        )
        toa_error = np.abs(full["toa_s"][mask] -
                           reference_toa[mask]) / reference_toa[mask]
        surface_dominated_toa_error = float(np.mean(toa_error)) if toa_error.size else np.nan
        surface_dominated_ros_error = abs(full["mean_ros_mps"] - surface_ros) / surface_ros
        mixed_mode_ros_ratio = mixed["mean_ros_mps"] / surface_ros

        edge_at_time = float(np.interp(
            LEADING_EDGE_COMPARISON_TIME_S,
            mixed["times_s"], mixed["leading_edge_m"]
        ))
        edge_gain = edge_at_time - surface_ros * LEADING_EDGE_COMPARISON_TIME_S
        onset = first_acceleration_time(
            mixed["times_s"], mixed["leading_edge_m"], surface_ros,
            float(manifest["dx_m"])
        )

        pair_mask = (
            np.isfinite(full["toa_s"]) & np.isfinite(mixed["toa_s"])
            & (distance >= FIT_DISTANCE_MIN_M) & (distance <= FIT_DISTANCE_MAX_M)
        )
        paired_lead = (
            float(np.median(full["toa_s"][pair_mask] - mixed["toa_s"][pair_mask]))
            if np.any(pair_mask) else np.nan
        )

        checks = [
            ("surface_dominated_toa", np.isfinite(surface_dominated_toa_error)
             and surface_dominated_toa_error <= SURFACE_DOMINATED_TOA_MEAN_REL_ERROR_LIMIT),
            ("surface_dominated_ros", np.isfinite(surface_dominated_ros_error)
             and surface_dominated_ros_error <= SURFACE_DOMINATED_ROS_REL_ERROR_LIMIT),
            ("surface_dominated_ignition", full["ember_ignition_cells"]
             <= SURFACE_DOMINATED_MAX_EMBER_IGNITION_CELLS),
            ("mixed_mode_ros", np.isfinite(mixed_mode_ros_ratio)
             and mixed_mode_ros_ratio >= MIXED_MODE_MIN_ROS_RATIO_TO_SURFACE),
            ("mixed_mode_ignition", mixed["ember_ignition_cells"]
             >= MIXED_MODE_MIN_EMBER_IGNITION_CELLS),
            ("mixed_mode_edge_gain", np.isfinite(edge_gain)
             and edge_gain >= MIXED_MODE_MIN_LEADING_EDGE_GAIN_M),
            ("mixed_mode_onset", np.isfinite(onset)
             and MIXED_MODE_ACCELERATION_ONSET_MIN_S <= onset <= MIXED_MODE_ACCELERATION_ONSET_MAX_S),
            ("paired_toa", np.isfinite(paired_lead)
             and paired_lead >= PAIR_MIN_MEDIAN_TOA_LEAD_S),
        ]
        passed = all(value for _, value in checks)
        metrics.update({
            "status": "pass" if passed else "fail",
            "verification_passed": bool_word(passed),
            "surface_dominated_toa_mean_relative_error": format_metric(surface_dominated_toa_error),
            "surface_dominated_toa_status": bool_word(checks[0][1]),
            "surface_dominated_mean_ros_mps": format_metric(full["mean_ros_mps"]),
            "surface_dominated_ros_relative_error": format_metric(surface_dominated_ros_error),
            "surface_dominated_ros_status": bool_word(checks[1][1]),
            "surface_dominated_ember_ignition_cells": full["ember_ignition_cells"],
            "surface_dominated_ignition_status": bool_word(checks[2][1]),
            "mixed_mode_mean_ros_mps": format_metric(mixed["mean_ros_mps"]),
            "mixed_mode_ros_ratio_to_surface": format_metric(mixed_mode_ros_ratio),
            "mixed_mode_ros_status": bool_word(checks[3][1]),
            "mixed_mode_ember_ignition_cells": mixed["ember_ignition_cells"],
            "mixed_mode_ignition_status": bool_word(checks[4][1]),
            "mixed_mode_leading_edge_gain_m": format_metric(edge_gain),
            "mixed_mode_edge_gain_status": bool_word(checks[5][1]),
            "mixed_mode_acceleration_onset_s": format_metric(onset, 1),
            "mixed_mode_onset_status": bool_word(checks[6][1]),
            "paired_median_toa_lead_s": format_metric(paired_lead),
            "paired_toa_status": bool_word(checks[7][1]),
            "notes": "All metrics were calculated from the two actual ELMFIRE variant outputs; the surface reference is the Rothermel ROS read from the surface-dominated variant VS raster.",
        })

    metrics["limits"] = {
        "surface_dominated_toa_mean_relative_error_max": SURFACE_DOMINATED_TOA_MEAN_REL_ERROR_LIMIT,
        "surface_dominated_ros_relative_error_max": SURFACE_DOMINATED_ROS_REL_ERROR_LIMIT,
        "surface_dominated_ember_ignition_cells_max": SURFACE_DOMINATED_MAX_EMBER_IGNITION_CELLS,
        "mixed_mode_ember_ignition_cells_min": MIXED_MODE_MIN_EMBER_IGNITION_CELLS,
        "mixed_mode_ros_ratio_to_surface_min": MIXED_MODE_MIN_ROS_RATIO_TO_SURFACE,
        "mixed_mode_leading_edge_gain_m_min": MIXED_MODE_MIN_LEADING_EDGE_GAIN_M,
        "mixed_mode_acceleration_onset_s_range": [
            MIXED_MODE_ACCELERATION_ONSET_MIN_S, MIXED_MODE_ACCELERATION_ONSET_MAX_S
        ],
        "paired_median_toa_lead_s_min": PAIR_MIN_MEDIAN_TOA_LEAD_S,
    }
    metrics["variant_outputs"] = {
        name: {
            key: value for key, value in result.items()
            if key not in {
                "distance_m", "toa_s", "accumulated_embers", "times_s",
                "leading_edge_m", "leading_edge_ros_mps"
            }
        }
        for name, result in results.items()
    }

    output_dir = case_dir / "outputs"
    report_dir = case_dir / "report"
    output_dir.mkdir(exist_ok=True)
    report_dir.mkdir(exist_ok=True)
    (output_dir / "metrics.json").write_text(
        json.dumps(metrics, indent=2) + "\n", encoding="utf-8"
    )
    macro_lines = []
    for key, value in metrics.items():
        if isinstance(value, (dict, list)):
            continue
        macro_name = re.sub(r"[^A-Za-z0-9]+", "", key)
        macro_lines.append(
            f"\\expandafter\\def\\csname metric@{macro_name}\\endcsname"
            f"{{{latex_escape(value)}}}"
        )
    (report_dir / "metrics_macros.tex").write_text(
        report_text("\n".join(macro_lines) + "\n"), encoding="utf-8"
    )
    print(f"[OK] verification status: {metrics['status']}")


if __name__ == "__main__":
    postprocess(Path(__file__).resolve().parents[1])
    generate_spatial_evidence(
        Path(__file__).resolve().parents[1],
        output_preference=("time_of_arrival", "ember_flux"),
        preferred_variant="mixed_mode",
    )
