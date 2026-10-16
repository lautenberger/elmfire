#!/usr/bin/env python3
"""Evaluate deposited-firebrand consumption from paired ELMFIRE outputs.

Reads case.json, variants/manifest.json, and each variant's final active
``ember_flux`` and ``time_of_arrival`` GeoTIFF. It writes member provenance,
consumption ratios, profile-convergence errors, surface-TOA invariance,
nonnegativity checks, the Boolean decision, LaTeX macros, and a vector PDF.
It never runs ELMFIRE. Arrays are [row, column]; the two-cell halo is excluded.
"""

from report_language import polish_figure, report_text
import rasterio
from spatial_evidence import generate_spatial_evidence
import numpy as np
import json
import math
import re
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# -----------------------------------------------------------------------------
# Customizable postprocessing parameters and tolerances
# -----------------------------------------------------------------------------
BUFFER_CELLS = 2
FINAL_EMBER_PATTERN = "ember_flux_[0-9]*_[0-9]*.tif"
HISTORY_TARGET_X_M = 200.0
CONVERGENCE_PROFILE_TIME_S = 250.0
FINAL_TOA_PATTERN = "time_of_arrival_*_*.tif"
MIN_PROFILE_CELLS = 3
CASE_DIR = Path(__file__).resolve().parents[1]
OUT_DIR = CASE_DIR / "outputs"
FIG_DIR = CASE_DIR / "figures"
REPORT_DIR = CASE_DIR / "report"


def read_raster(path):
    """Read one raster and return array, geotransform, and cell size."""
    with rasterio.open(path) as dataset:
        array = dataset.read(1).astype(float)
        gt = dataset.transform.to_gdal()
        dx = float(dataset.transform.a)
    return array, gt, dx


def output_time(path):
    """Return the encoded simulation time from a standard final-raster name."""
    match = re.search(r"_(\d+)\.tif$", path.name)
    return float(match.group(1)) if match else math.nan


def select_final(directory, pattern):
    """Select the latest final product whose filename encodes physical seconds."""
    files = sorted(directory.glob(pattern), key=lambda path: (output_time(path), path.name))
    if not files:
        return None, f"expected at least one {pattern}, found 0"
    return files[-1], ""


def profile_record(entry, case):
    """Extract the active-ember centerline profile and TOA from one variant."""
    output_dir = CASE_DIR / entry["working_directory"] / "outputs"
    ember_file, ember_error = select_final(output_dir, FINAL_EMBER_PATTERN)
    toa_file, toa_error = select_final(output_dir, FINAL_TOA_PATTERN)
    if ember_file is None or toa_file is None:
        return {
            "entry": entry,
            "error": "; ".join(
                x for x in (
                    ember_error,
                    toa_error) if x)}
    ember, gt, dx = read_raster(ember_file)
    toa, gt_toa, dx_toa = read_raster(toa_file)
    expected_shape = (int(entry["ny"]), int(entry["nx"]))
    if ember.shape != expected_shape or toa.shape != expected_shape:
        return {
            "entry": entry,
            "error": f"stale shape: ember={ember.shape}, toa={toa.shape}, expected={expected_shape}"}
    if not np.isclose(dx, entry["dx_m"]) or not np.isclose(dx_toa, entry["dx_m"]):
        return {"entry": entry, "error": "raster cell size disagrees with manifest"}
    row = ember.shape[0] // 2
    sl = slice(BUFFER_CELLS, ember.shape[1] - BUFFER_CELLS)
    x = gt[0] + (np.arange(ember.shape[1]) + 0.5) * gt[1]
    # A one-cell-wide transect is converted from pixel count [pcs] to the reference
    # line-density observable [pcs/m] by dividing by its downwind bin width.
    active = ember[row, sl] / dx
    x_physical = x[sl]
    history_times = []
    history_target = []
    profile_times = []
    profile_values = []
    target_index = int(np.argmin(np.abs(x_physical - float(case["target_x_m"]))))
    # Each history variant terminates at one predeclared sample time, so its
    # final EMBER_FLUX is the active population at that physical time.
    for time_s, history_file in [(float(entry["evaluation_time_s"]), ember_file)]:
        history, _, history_dx = read_raster(history_file)
        if history.shape != expected_shape or not np.isclose(history_dx, entry["dx_m"]):
            return {"entry": entry, "error": f"stale history raster {history_file.name}"}
        density = history[row, sl] / dx
        history_times.append(time_s)
        history_target.append(float(density[target_index]))
        profile_times.append(time_s)
        profile_values.append(density)
    profile_index = int(np.argmin(np.abs(
        np.asarray(profile_times) - float(case["profile_target_time_s"])
    ))) if profile_times else None
    return {
        "entry": entry, "error": "", "x_m": x_physical,
        "active_pcs_per_m": active, "toa_s": toa[row, sl],
        "history_times_s": np.asarray(history_times),
        "history_target_pcs_per_m": np.asarray(history_target),
        "profile_time_s": profile_times[profile_index] if profile_index is not None else math.nan,
        "profile_target_pcs_per_m": profile_values[profile_index] if profile_index is not None else np.array([]),
        "minimum_full_raster_count": float(np.nanmin(ember)),
        "total_centerline_active_pcs": float(np.nansum(ember[row, sl])),
        "ember_file": ember_file.name, "toa_file": toa_file.name,
    }


def normalized_l1(reference_x, reference_y, x, y):
    """Return integral absolute error normalized by the fine-profile integral."""
    mask = (reference_x >= x.min()) & (reference_x <= x.max())
    xr = reference_x[mask]
    yr = reference_y[mask]
    yi = np.interp(xr, x, y)
    denominator = np.trapz(np.abs(yr), xr)
    return float(np.trapz(np.abs(yi - yr), xr) / max(denominator, 1e-12))


def macro_value(value):
    """Format one scalar safely for LaTeX."""
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, float):
        return "not available" if not np.isfinite(value) else f"{value:.5g}"
    return str(value).replace("_", "\\_")


def leading_edge_history(x, toa, tstop_s):
    """Invert a final arrival-time profile into leading-edge position and ROS."""
    times = np.linspace(0.0, tstop_s, 261)
    valid = np.isfinite(toa) & (toa >= 0.0) & (toa <= tstop_s)
    origin = float(x[valid][0]) if np.any(valid) else 0.0
    edge = np.asarray([
        float(np.max(x[valid & (toa <= time_s)]))
        if np.any(valid & (toa <= time_s)) else origin
        for time_s in times
    ])
    ros = np.full_like(times, np.nan)
    ros[1:] = np.diff(edge) / np.diff(times)
    return times, edge, ros


def write_reference_figures(case, by_key, history_by_dx):
    """Write the four-panel consumption result and two-panel convergence view."""
    fine = by_key[(1.0, True)]
    surface_ros = float(case["surface_ros_mps"])
    times, edge, ros = leading_edge_history(
        fine["x_m"], fine["toa_s"], float(case["evaluation_time_s"]))
    fig, axes = plt.subplots(2, 2, figsize=(10.0, 7.2), constrained_layout=True)
    axes[0, 0].plot(fine["x_m"], fine["active_pcs_per_m"], color="#1f77b4")
    axes[0, 0].set(xlabel="Downwind distance [m]",
                   ylabel=r"Active firebrands [pcs m$^{-1}$]",
                   xlim=(0.0, case["physical_length_m"]), ylim=(0.0, None))
    valid = np.isfinite(fine["toa_s"]) & (fine["toa_s"] >= 0.0)
    axes[0, 1].plot(fine["x_m"][valid], fine["toa_s"][valid],
                    color="#1f77b4", marker="o", markersize=2.2,
                    label="ELMFIRE")
    axes[0, 1].plot(fine["x_m"], fine["x_m"] / surface_ros, "k-",
                    label=r"$T=x/R_s$")
    axes[0, 1].set(xlabel="Downwind distance [m]", ylabel="Time of arrival [s]",
                   xlim=(0.0, case["physical_length_m"]), ylim=(0.0, None))
    axes[1, 0].plot(times, edge, color="#1f77b4", marker="o", markersize=2.0,
                    label="ELMFIRE")
    axes[1, 0].plot(times, surface_ros * times, "k-", label=r"$x_{LE}=R_s t$")
    axes[1, 0].set(xlabel="Time [s]", ylabel="Leading-edge position [m]",
                   ylim=(0.0, None))
    axes[1, 1].plot(times, ros, color="#1f77b4", marker="o", markersize=1.8,
                    label="ELMFIRE")
    axes[1, 1].axhline(surface_ros, color="black", label=r"$ROS=R_s$")
    axes[1, 1].set(xlabel="Time [s]", ylabel="Leading-edge ROS [m/s]",
                   ylim=(0.0, None))
    for label, axis in zip(("(a)", "(b)", "(c)", "(d)"), axes.flat):
        axis.text(0.01, 0.98, label, transform=axis.transAxes,
                  ha="left", va="top", fontweight="bold")
        axis.grid(alpha=0.2)
        handles, _ = axis.get_legend_handles_labels()
        if handles:
            axis.legend(fontsize=8)
    polish_figure(fig)
    fig.savefig(FIG_DIR / "firebrand_consumption_state.pdf", format="pdf")
    plt.close(fig)

    fig, axes = plt.subplots(1, 2, figsize=(10.5, 4.0), constrained_layout=True)
    for dx in sorted(float(value) for value in case["grid_sizes_m"]):
        samples = sorted(
            history_by_dx[dx],
            key=lambda item: float(item["entry"]["evaluation_time_s"]),
        )
        sample_times = np.asarray([
            float(item["entry"]["evaluation_time_s"]) for item in samples
        ])
        sample_values = np.asarray([
            float(item["history_target_pcs_per_m"][0]) for item in samples
        ])
        axes[0].plot(sample_times, sample_values, marker="o", markersize=3.0,
                     label=rf"$\Delta x={dx:g}$ m")
        profile = min(
            samples,
            key=lambda item: abs(
                float(item["entry"]["evaluation_time_s"])
                - float(case["profile_target_time_s"])
            ),
        )
        axes[1].plot(profile["x_m"], profile["active_pcs_per_m"],
                     label=rf"$\Delta x={dx:g}$ m")
    axes[0].set(xlabel="Time [s]", ylabel=r"Active firebrands [pcs m$^{-1}$]",
                title=rf"(a) History near $x={case['target_x_m']:g}$ m",
                ylim=(0.0, None))
    axes[1].set(xlabel="Downwind distance [m]",
                ylabel=r"Active firebrands [pcs m$^{-1}$]",
                title=rf"(b) Spatial profile near $t={case['profile_target_time_s']:g}$ s",
                xlim=(0.0, case["physical_length_m"]), ylim=(0.0, None))
    for axis in axes:
        axis.grid(alpha=0.2)
        axis.legend(fontsize=8)
    polish_figure(fig)
    fig.savefig(FIG_DIR / "firebrand_consumption_convergence.pdf", format="pdf")
    plt.close(fig)


def main():
    """Calculate every predeclared metric and write the verification artifacts."""
    OUT_DIR.mkdir(exist_ok=True)
    FIG_DIR.mkdir(exist_ok=True)
    REPORT_DIR.mkdir(exist_ok=True)
    case = json.loads((CASE_DIR / "case.json").read_text(encoding="utf-8"))
    manifest = json.loads(
        (CASE_DIR / "variants/manifest.json").read_text(encoding="utf-8"))
    required_paired = 2 * len(case["grid_sizes_m"])
    required_history = (
        len(case["grid_sizes_m"]) * len(case["history_times_s"])
    )
    required = required_paired + required_history
    if len(manifest) != required:
        raise RuntimeError(
            f"manifest has {len(manifest)} variants; expected {required}")
    extracted = [profile_record(entry, case) for entry in manifest]
    complete = all(not item["error"] for item in extracted)
    paired = [
        item for item in extracted
        if item["entry"].get("role", "paired") == "paired"
    ]
    histories = [
        item for item in extracted
        if item["entry"].get("role") == "history"
    ]
    records = []
    for item in extracted:
        entry = item["entry"]
        record = {"name": entry["name"], "role": entry.get("role", "paired"),
                  "dx_m": entry["dx_m"],
                  "evaluation_time_s": entry["evaluation_time_s"],
                  "consumption_enabled": entry["consumption_enabled"],
                  "error": item["error"]}
        if not item["error"]:
            record.update(
                total_centerline_active_pcs=item["total_centerline_active_pcs"],
                minimum_full_raster_count=item["minimum_full_raster_count"],
                ember_file=item["ember_file"],
                toa_file=item["toa_file"])
        records.append(record)

    retained = {}
    toa_differences = {}
    profile_errors = {}
    nonnegative = complete
    if complete:
        by_key = {(float(i["entry"]["dx_m"]), bool(
            i["entry"]["consumption_enabled"])): i for i in paired}
        for dx in map(float, case["grid_sizes_m"]):
            off = by_key[(dx, False)]
            on = by_key[(dx, True)]
            retained[dx] = on["total_centerline_active_pcs"] / \
                max(off["total_centerline_active_pcs"], 1e-12)
            valid = np.isfinite(
                off["toa_s"]) & np.isfinite(
                on["toa_s"]) & (
                off["toa_s"] >= 0) & (
                on["toa_s"] >= 0)
            toa_differences[dx] = float(
                np.max(
                    np.abs(
                        on["toa_s"][valid] -
                        off["toa_s"][valid]))) if valid.any() else math.nan
            nonnegative = nonnegative and off["minimum_full_raster_count"] >= case["acceptance"]["negative_count_tolerance"]
            nonnegative = nonnegative and on["minimum_full_raster_count"] >= case["acceptance"]["negative_count_tolerance"]
        fine = by_key[(1.0, True)]
        for dx in (5.0, 10.0, 30.0):
            coarse = by_key[(dx, True)]
            profile_errors[dx] = normalized_l1(
                fine["x_m"],
                fine["active_pcs_per_m"],
                coarse["x_m"],
                coarse["active_pcs_per_m"])

    retained_limit = float(case["acceptance"]["maximum_retained_fraction"])
    toa_limit = float(case["acceptance"]["maximum_toa_pair_difference_s"])
    dx5_limit = float(case["acceptance"]["maximum_dx5_normalized_l1_error"])
    consumption_effect = complete and all(
        value <= retained_limit for value in retained.values())
    toa_invariant = complete and all(
        np.isfinite(v) and v <= toa_limit for v in toa_differences.values())
    dx5_converged = complete and profile_errors.get(5.0, math.inf) <= dx5_limit
    ordered = complete and profile_errors.get(
        30.0, -math.inf) >= profile_errors.get(10.0, math.inf) >= profile_errors.get(5.0, math.inf)
    passed = complete and consumption_effect and toa_invariant and nonnegative and dx5_converged and ordered

    if complete:
        by_key = {(float(i["entry"]["dx_m"]), bool(
            i["entry"]["consumption_enabled"])): i for i in paired}
        fig, axes = plt.subplots(1, 3, figsize=(12.5, 3.8), constrained_layout=True)
        for dx in map(float, case["grid_sizes_m"]):
            on = by_key[(dx, True)]
            axes[0].plot(
                on["x_m"],
                on["active_pcs_per_m"],
                label=fr"$\Delta x={dx:g}$ m")
        axes[0].set(
            xlabel="Downwind distance (m)",
            ylabel="Active firebrands (pcs m$^{-1}$)",
            title="Consumption on at 260 s",
            xlim=(
                0,
                300),
            ylim=(
                0,
                None))
        axes[0].legend(fontsize=7)
        axes[0].grid(alpha=0.25)
        fine_off = by_key[(1.0, False)]
        fine_on = by_key[(1.0, True)]
        axes[1].plot(
            fine_off["x_m"],
            fine_off["active_pcs_per_m"],
            label="consumption off")
        axes[1].plot(
            fine_on["x_m"],
            fine_on["active_pcs_per_m"],
            label="consumption on")
        axes[1].set(
            xlabel="Downwind distance (m)",
            ylabel="Active firebrands (pcs m$^{-1}$)",
            title=r"Paired comparison, $\Delta x=1$ m",
            xlim=(
                0,
                300),
            ylim=(
                0,
                None))
        axes[1].legend(fontsize=8)
        axes[1].grid(alpha=0.25)
        dxs = np.asarray(sorted(retained))
        values = np.asarray([retained[d] for d in dxs])
        axes[2].bar([str(f"{d:g}") for d in dxs], values, color="#4472c4")
        axes[2].axhline(
            retained_limit,
            color="#c00000",
            linestyle="--",
            label="4% limit")
        axes[2].set(xlabel="Cell size (m)", ylabel="Active fraction, on/off (-)",
                    title="Retained active population", ylim=(0, max(1.05, values.max() * 1.08)))
        axes[2].legend(fontsize=8)
        axes[2].grid(axis="y", alpha=0.25)
        polish_figure(fig)
        fig.savefig(FIG_DIR / "firebrand_consumption_verification.pdf", format="pdf")
        plt.close(fig)
        history_by_dx = {
            dx: [
                item for item in histories
                if np.isclose(float(item["entry"]["dx_m"]), dx)
            ]
            for dx in map(float, case["grid_sizes_m"])
        }
        write_reference_figures(case, by_key, history_by_dx)

    metrics = {
        "case_id": case["id"],
        "status": "pass" if passed else ("fail" if complete else "insufficient_output"),
        "verification_passed": passed if complete else "not_evaluated",
        "required_variants": required,
        "required_paired_variants": required_paired,
        "required_history_variants": required_history,
        "evaluated_variants": sum(not r["error"] for r in records),
        "evaluation_time_s": case["evaluation_time_s"],
        "maximum_retained_fraction": max(retained.values()) if retained else None,
        "acceptance_maximum_retained_fraction": retained_limit,
        "consumption_effect_passed": consumption_effect,
        "maximum_paired_toa_difference_s": max(toa_differences.values()) if toa_differences else None,
        "acceptance_maximum_toa_difference_s": toa_limit,
        "toa_invariance_passed": toa_invariant,
        "minimum_active_count": min((r.get("minimum_full_raster_count", math.inf) for r in records), default=None),
        "nonnegative_active_counts_passed": nonnegative,
        "dx5_normalized_l1_profile_error": profile_errors.get(5.0),
        "acceptance_dx5_normalized_l1_error": dx5_limit,
        "dx5_profile_passed": dx5_converged,
        "ordered_profile_convergence_passed": ordered,
        "retained_fraction_by_dx": {f"{k:g}": v for k, v in retained.items()},
        "toa_difference_by_dx_s": {f"{k:g}": v for k, v in toa_differences.items()},
        "normalized_l1_profile_error_by_dx": {f"{k:g}": v for k, v in profile_errors.items()},
        "variants": records,
        "source_observation": "Current EMBER_CONSUMPTION hard-codes MIN_LIFETIME=1E9 s; the verification target specifies 10 s.",
    }
    (OUT_DIR / "metrics.json").write_text(json.dumps(metrics, indent=2) + "\n", encoding="utf-8")
    scalar = {k: v for k, v in metrics.items() if not isinstance(v, (dict, list))}
    lines = []
    for key, value in scalar.items():
        safe = re.sub(r"[^A-Za-z0-9]+", "", key)
        lines.append(
            f"\\expandafter\\def\\csname metric@{safe}\\endcsname{{{macro_value(value)}}}")
    (REPORT_DIR / "metrics_macros.tex").write_text(report_text("\n".join(lines) + "\n"), encoding="utf-8")
    print(
        f"[OK] evaluated {metrics['evaluated_variants']}/{required} variants: {metrics['status']}")


if __name__ == "__main__":
    main()
    generate_spatial_evidence(
        CASE_DIR, output_preference=("ember_flux", "time_of_arrival"),
        preferred_variant="dx10_consumption_on",
    )
