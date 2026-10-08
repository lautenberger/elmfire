#!/usr/bin/env python3
"""Postprocess the temporal-resolution verification variants.

The script reads only outputs carrying the current preprocessing fingerprint,
computes the declared comparison metrics, and writes standalone report
artifacts. It never launches ELMFIRE.
"""

from report_language import polish_figure, report_text

import rasterio
from spatial_evidence import generate_spatial_evidence
import numpy as np
from pathlib import Path
import csv
import json
import math
import re

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# Customizable postprocessing parameters and independent physical constants.
FINAL_EMBER_GLOB = "ember_flux_[0-9]*.tif"
FINAL_TOA_GLOB = "time_of_arrival_[0-9]*.tif"
TRANSIENT_EMBER_GLOB = "ember_flux_transient_*_d*.tif"
DUMP_TIMES_GLOB = "dump_times_*.csv"
INVALID_TOA_MIN_S = -1000.0
ANALYTICAL_DT_S = 5.0
AIR_DENSITY_KG_M3 = 1.1
AIR_HEAT_CAPACITY_KJ_KG_K = 1.0
AMBIENT_TEMPERATURE_K = 300.0
GRAVITY_M_S2 = 9.81
STRUCTURE_LENGTH_M = 10.0
FIREBRAND_DENSITY_KG_M3 = 100.0
FIREBRAND_DIAMETER_M = 5.0e-3
PLOT_ERROR_FLOOR = 1.0e-8
FIGURE_DPI = 180
METRIC_DIGITS = 5

COLORS = {
    "baseline": "#1f77b4",
    "cfl_1": "#ff7f0e",
    "cfl_10": "#2ca02c",
    "cfl_200": "#d62728",
}


def read_band(path, counts=False):
    """Read a GeoTIFF band; retain zero as a valid deposited-ember count."""
    with rasterio.open(path) as dataset:
        array = dataset.read(1).astype(float)
        nodata = dataset.nodata
    if nodata is not None and not (counts and np.isclose(nodata, 0.0)):
        array[np.isclose(array, nodata)] = np.nan
    return array


def final_output(directory, pattern):
    """Return the final non-transient raster selected by its timestamped name."""
    candidates = [p for p in directory.glob(pattern) if "_transient_" not in p.name]
    return sorted(candidates)[-1] if candidates else None


def output_matches_inputs(variant_dir, item):
    """Reject results produced before the current generated input fingerprint."""
    stamp = variant_dir / "logs" / "completed_input_fingerprint.txt"
    return (
        stamp.exists()
        and stamp.read_text(encoding="utf-8").strip() == item["input_fingerprint"]
    )


def transient_history(variant_dir, item, ids_row):
    """Integrate transient ember counts into per-structure accumulation histories."""
    output_dir = variant_dir / "outputs"
    time_files = sorted(output_dir.glob(DUMP_TIMES_GLOB))
    if not time_files:
        return np.empty(0), np.empty((0, 0)), np.full(ids_row.max(initial=0), np.nan)
    dump_times = {}
    with time_files[-1].open(newline="", encoding="utf-8") as stream:
        for row in csv.DictReader(stream):
            dump_times[int(row["dump_index"])] = float(row["time_seconds"])
    indexed_files = []
    for raster in sorted(output_dir.glob(TRANSIENT_EMBER_GLOB)):
        match = re.search(r"_d(\d+)\.tif$", raster.name)
        if match and int(match.group(1)) in dump_times:
            indexed_files.append((int(match.group(1)), raster))
    structure_ids = np.asarray(sorted(set(ids_row[ids_row > 0])), dtype=int)
    if not indexed_files or structure_ids.size == 0:
        return np.empty(0), np.empty(
            (0, structure_ids.size)), np.full(
            structure_ids.size, np.nan)
    row_index = int(item["center_row"])
    buffer_cells = int(item["buffer_cells"])
    stop = len(ids_row) + buffer_cells
    cell_area = float(item["dx_m"]) ** 2
    represented_areas = np.asarray([
        np.count_nonzero(ids_row == structure_id) * cell_area
        for structure_id in structure_ids
    ])
    cumulative = np.zeros(structure_ids.size, dtype=float)
    values = []
    times = []
    for index, raster in indexed_files:
        ember = read_band(raster, counts=True)[row_index, buffer_cells:stop]
        for position, structure_id in enumerate(structure_ids):
            cumulative[position] += float(np.nansum(
                ember[ids_row == structure_id]
            )) / represented_areas[position]
        times.append(dump_times[index])
        values.append(cumulative.copy())
    values = np.asarray(values, dtype=float)
    first_deposition = np.full(structure_ids.size, np.nan)
    for position in range(structure_ids.size):
        reached = np.flatnonzero(values[:, position] > 0.0)
        if reached.size:
            first_deposition[position] = times[int(reached[0])]
    return np.asarray(times, dtype=float), values, first_deposition


def structure_profile(case_dir, case, item):
    """Aggregate final and transient output over physical structures."""
    variant_dir = case_dir / item["working_directory"]
    if not output_matches_inputs(variant_dir, item):
        return None
    output_dir = variant_dir / "outputs"
    ember_path = final_output(output_dir, FINAL_EMBER_GLOB)
    toa_path = final_output(output_dir, FINAL_TOA_GLOB)
    if ember_path is None or toa_path is None:
        return None
    ids = read_band(variant_dir / "data" / "inputs" / "structure_id.tif")
    ember = read_band(ember_path, counts=True)
    toa = read_band(toa_path)
    row = int(item["center_row"])
    buffer_cells = int(item["buffer_cells"])
    stop = ids.shape[1] - buffer_cells
    raw_ids = np.nan_to_num(
        ids[row, buffer_cells:stop], nan=0.0
    ).astype(int)
    n_structures_x = int(item["n_structures_x"])
    raw_x_index = np.where(
        raw_ids > 0, (raw_ids - 1) % n_structures_x, -1
    )
    present_x = sorted(set(raw_x_index[raw_x_index >= 0]))
    ids_row = np.zeros(raw_ids.shape, dtype=int)
    for sequential_id, x_index in enumerate(present_x, start=1):
        ids_row[raw_x_index == x_index] = sequential_id
    ember_row = ember[row, buffer_cells:stop]
    toa_row = toa[row, buffer_cells:stop]
    structure_ids = np.asarray(sorted(set(ids_row[ids_row > 0])), dtype=int)
    cell_area = float(item["dx_m"]) ** 2
    centres = []
    loads = []
    ignition = []
    for structure_id in structure_ids:
        mask = ids_row == structure_id
        represented_area = np.count_nonzero(mask) * cell_area
        cell_centres = (np.flatnonzero(mask) + 0.5) * float(item["dx_m"])
        centres.append(float(np.mean(cell_centres)))
        loads.append(float(np.nansum(ember_row[mask])) / represented_area)
        valid_toa = toa_row[mask]
        valid_toa = valid_toa[np.isfinite(valid_toa) & (valid_toa > INVALID_TOA_MIN_S)]
        ignition.append(float(np.min(valid_toa)) if valid_toa.size else math.nan)
    times, histories, first_deposition = transient_history(variant_dir, item, ids_row)
    profile = {
        "name": item["name"],
        "role": item["role"],
        "cfl_wind": float(item["cfl_wind"]),
        "dt_s": float(item["simulation_dt_s"]),
        "ids": structure_ids,
        "x_m": np.asarray(centres, dtype=float),
        "load_pcs_m2": np.asarray(loads, dtype=float),
        "ignition_s": np.asarray(ignition, dtype=float),
        "history_time_s": times,
        "history_pcs_m2": histories,
        "first_deposition_s": first_deposition,
        "ember_file": ember_path.name,
        "toa_file": toa_path.name,
    }
    profile["analytical_load_pcs_m2"] = analytical_load(
        case, profile, float(item["tstop_s"])
    )
    return profile


def design_hrr_kw(case, elapsed_s):
    """Piecewise structural design HRR for one 10 m by 10 m structure."""
    if elapsed_s <= 0.0 or elapsed_s >= case["hrr_decay_end_s"]:
        return 0.0
    peak = (
        case["hrrpua_peak_kw_m2"]
        * case["structure_width_m"]
        * case["cross_grid_width_m"]
    )
    if elapsed_s < case["hrr_growth_end_s"]:
        return peak * elapsed_s / case["hrr_growth_end_s"]
    if elapsed_s <= case["hrr_steady_end_s"]:
        return peak
    return peak * (case["hrr_decay_end_s"] - elapsed_s) / (
        case["hrr_decay_end_s"] - case["hrr_steady_end_s"]
    )


def lognormal_cdf(distance, mu_log, sigma_log):
    """Evaluate the lognormal CDF used by the analytical deposition integral."""
    if distance <= 0.0:
        return 0.0
    z = (math.log(max(distance, 1.0e-6)) - mu_log) / (math.sqrt(2.0) * sigma_log)
    return 0.5 * (1.0 + math.erf(z))


def himoto_parameters(case, hrr_kw):
    """Derive log-space landing-distance parameters from the physical state."""
    wind = 0.447 * case["wind_speed_mph"] / 0.87
    length = STRUCTURE_LENGTH_M
    b_star = (
        wind / math.sqrt(GRAVITY_M_S2 * length)
        * (FIREBRAND_DENSITY_KG_M3 / AIR_DENSITY_KG_M3) ** (-0.75)
        * (FIREBRAND_DIAMETER_M / length) ** (-0.75)
        * (
            hrr_kw
            / (
                AIR_DENSITY_KG_M3
                * AIR_HEAT_CAPACITY_KJ_KG_K
                * AMBIENT_TEMPERATURE_K
                * math.sqrt(GRAVITY_M_S2)
                * length ** 2.5
            )
        ) ** 0.5
    )
    mean_x = max(0.47 * b_star ** (2.0 / 3.0) * length, 1.0e-5)
    sigma_x = max(0.88 * b_star ** (1.0 / 3.0) * length, 1.0e-5)
    mu_log = math.log(mean_x / math.sqrt((sigma_x / mean_x) ** 2 + 1.0))
    sigma_log = math.sqrt(math.log(1.0 + (sigma_x / mean_x) ** 2))
    return mu_log, sigma_log


def analytical_load(case, profile, tstop_s):
    """Integrate analytical deposited load using simulated source ignition times."""
    values = np.zeros(profile["ids"].size, dtype=float)
    width = float(case["structure_width_m"])
    area = width * float(case["cross_grid_width_m"])
    generation = float(case["ember_generation_building_pcs_mw_s"])
    for target in range(profile["ids"].size):
        target_center = profile["x_m"][target]
        for source in range(target):
            ignition = profile["ignition_s"][source]
            if not np.isfinite(ignition):
                continue
            elapsed_max = min(case["hrr_decay_end_s"], tstop_s - ignition)
            for elapsed in np.arange(
                    0.5 * ANALYTICAL_DT_S,
                    elapsed_max,
                    ANALYTICAL_DT_S):
                hrr_kw = design_hrr_kw(case, elapsed)
                if hrr_kw <= 0.0:
                    continue
                mu_log, sigma_log = himoto_parameters(case, hrr_kw)
                relative_center = target_center - profile["x_m"][source]
                probability = (lognormal_cdf(relative_center + 0.5 * width,
                                             mu_log,
                                             sigma_log) - lognormal_cdf(max(0.0,
                                                                            relative_center - 0.5 * width),
                               mu_log,
                               sigma_log))
                values[target] += (
                    generation * hrr_kw / 1000.0 * probability * ANALYTICAL_DT_S
                )
    return values / area


def relative_error_values(candidate, reference):
    """Return a full-size pointwise relative-error vector with invalid values masked."""
    candidate = np.asarray(candidate, dtype=float)
    reference = np.asarray(reference, dtype=float)
    result = np.full(reference.shape, np.nan)
    mask = np.isfinite(candidate) & np.isfinite(
        reference) & (np.abs(reference) > 1.0e-12)
    result[mask] = np.abs(candidate[mask] - reference[mask]) / np.abs(reference[mask])
    return result


def matched_values(candidate, baseline, field):
    """Return candidate and baseline values at common physical structure IDs."""
    common = sorted(set(candidate["ids"]).intersection(set(baseline["ids"])))
    left, right, x = [], [], []
    for structure_id in common:
        ci = np.where(candidate["ids"] == structure_id)[0][0]
        bi = np.where(baseline["ids"] == structure_id)[0][0]
        left.append(candidate[field][ci])
        right.append(baseline[field][bi])
        x.append(baseline["x_m"][bi])
    return np.asarray(left), np.asarray(right), np.asarray(x)


def history_nrmse(candidate, baseline, target_id):
    """Compare target-structure accumulation histories on baseline dump times."""
    if candidate["history_time_s"].size == 0 or baseline["history_time_s"].size == 0:
        return math.nan
    ci = np.where(candidate["ids"] == target_id)[0]
    bi = np.where(baseline["ids"] == target_id)[0]
    if not ci.size or not bi.size:
        return math.nan
    bt = baseline["history_time_s"]
    by = baseline["history_pcs_m2"][:, bi[0]]
    cy = np.interp(bt, candidate["history_time_s"],
                   candidate["history_pcs_m2"][:, ci[0]])
    scale = float(np.nanmax(by) - np.nanmin(by))
    return float(np.sqrt(np.nanmean((cy - by) ** 2)) /
                 scale) if scale > 0.0 else math.nan


def metric(
        name,
        limit,
        value=None,
        passed=None,
        note="",
        unavailable_status="NOT EVALUABLE"):
    """Create one explicit metric row without blank or non-finite calculated fields."""
    if value is None or (isinstance(value, (int, float, np.floating))
                         and not np.isfinite(value)):
        return {
            "metric": name,
            "limit": limit,
            "calculated": "N/A",
            "status": unavailable_status,
            "note": note}
    if passed is None:
        return {
            "metric": name,
            "limit": limit,
            "calculated": str(value),
            "status": "NOT EVALUABLE",
            "note": note}
    return {
        "metric": name,
        "limit": limit,
        "calculated": float(value) if isinstance(
            value,
            (int,
             float,
             np.floating)) else str(value),
        "status": "PASS" if passed else "FAIL",
        "note": note,
    }


def calculate_metrics(case, manifest, profiles):
    """Evaluate the quantitative temporal-resolution verification contract."""
    limits = case["metrics"]
    rows = [
        metric(
            "Output completeness",
            "4 variants",
            f"{len(profiles)}/4",
            True if len(profiles) == 4 else None,
            "Only outputs matching the current input fingerprint are counted.",
        )]
    load_specs = (
        ("baseline",
         "CFL 0.5 analytical-load mean error",
         "baseline_load_mean_relative_error_max",
         np.mean),
        ("cfl_1",
         "CFL 1 analytical-load maximum error",
         "cfl_1_load_max_relative_error_max",
         np.max),
        ("cfl_10",
         "CFL 10 analytical-load maximum error",
         "cfl_10_load_max_relative_error_max",
         np.max),
        ("cfl_200",
         "CFL 200 analytical-load maximum error",
         "cfl_200_load_max_relative_error_max",
         np.max),
    )
    for role, name, key, reducer in load_specs:
        profile = profiles.get(role)
        if profile is None:
            rows.append(metric(name, f"<= {limits[key]}"))
            continue
        errors = relative_error_values(
            profile["load_pcs_m2"],
            profile["analytical_load_pcs_m2"])
        finite = errors[np.isfinite(errors)]
        value = float(reducer(finite)) if finite.size else math.nan
        rows.append(
            metric(
                name,
                f"<= {limits[key]}",
                value,
                value <= limits[key] if np.isfinite(value) else None,
                "No finite load comparison could be formed from the completed output." if not np.isfinite(value) else "",
                "FAIL",
            ))
    baseline = profiles.get("baseline")
    for role, label, key in (
        ("cfl_1", "CFL 1 ignition-time mean relative error", "cfl_1_ignition_mean_relative_error_max"),
        ("cfl_10", "CFL 10 ignition-time mean relative error", "cfl_10_ignition_mean_relative_error_max"),
    ):
        candidate = profiles.get(role)
        if baseline is None or candidate is None:
            rows.append(metric(label, f"<= {limits[key]}"))
            continue
        left, right, _ = matched_values(candidate, baseline, "ignition_s")
        errors = relative_error_values(left, right)
        finite = errors[np.isfinite(errors)]
        value = float(np.mean(finite)) if finite.size else math.nan
        rows.append(
            metric(
                label,
                f"<= {limits[key]}",
                value,
                value <= limits[key] if np.isfinite(value) else None,
                "No downstream ignition-time pairs were produced by the completed simulations." if not np.isfinite(value) else "",
                "FAIL",
            ))
    target_id = int(case["history_structure_left_m"] //
                    (case["structure_width_m"] + case["firebreak_width_m"])) + 1
    for role, label, key in (
        ("cfl_1", "CFL 1 accumulation-history NRMSE", "cfl_1_history_nrmse_max"),
        ("cfl_10", "CFL 10 accumulation-history NRMSE", "cfl_10_history_nrmse_max"),
    ):
        value = history_nrmse(
            profiles[role],
            baseline,
            target_id) if baseline is not None and role in profiles else math.nan
        rows.append(
            metric(
                label,
                f"<= {limits[key]}",
                value,
                value <= limits[key] if np.isfinite(value) else None,
                "The completed baseline produced no nonconstant accumulation history for normalization." if not np.isfinite(value) else "",
                "FAIL",
            ))
    decision_rows = rows
    if len(profiles) < len(manifest):
        overall = "NOT RUN"
    elif any(row["status"] == "FAIL" for row in decision_rows):
        overall = "FAIL"
    elif any(row["status"] == "NOT EVALUABLE" for row in decision_rows):
        overall = "NOT EVALUABLE"
    else:
        overall = "PASS"
    diagnostics = {}
    if baseline is not None:
        for role in ("cfl_1", "cfl_10", "cfl_200"):
            if role not in profiles:
                continue
            ignition, base_ignition, _ = matched_values(
                profiles[role], baseline, "ignition_s")
            ignition_errors = relative_error_values(ignition, base_ignition)
            diagnostics[f"{role}_mean_ignition_relative_error"] = (
                float(np.nanmean(ignition_errors)) if np.isfinite(ignition_errors).any() else None
            )
            arrival, base_arrival, _ = matched_values(
                profiles[role], baseline, "first_deposition_s")
            arrival_errors = relative_error_values(arrival, base_arrival)
            diagnostics[f"{role}_mean_first_deposition_relative_error"] = (
                float(np.nanmean(arrival_errors)) if np.isfinite(arrival_errors).any() else None
            )
    return rows, overall, diagnostics


def label_for(profile):
    """Return the concise plotting label associated with one temporal-resolution role."""
    return f"CFL={profile['cfl_wind']:g}, dt={profile['dt_s']:.2f} s"


def plot_temporal_profiles(case_dir, case, manifest, profiles):
    """Generate the four temporal-resolution profile diagnostics."""
    fig, axes = plt.subplots(2, 2, figsize=(10.2, 7.4), constrained_layout=True)
    for item in manifest:
        profile = profiles.get(item["role"])
        if profile is None:
            continue
        error = relative_error_values(
            profile["load_pcs_m2"],
            profile["analytical_load_pcs_m2"])
        axes[0,
             0].semilogy(profile["x_m"],
                         np.maximum(error,
                                    PLOT_ERROR_FLOOR),
                         marker="o",
                         ms=3,
                         color=COLORS[item["role"]],
                         label=label_for(profile))
    axes[0,
         0].set(xlabel="Structure centre x (m)",
                ylabel="Relative accumulated-load error",
                title="(a) Analytical accumulated-load error")

    baseline = profiles.get("baseline")
    if baseline is not None:
        for role in ("cfl_1", "cfl_10", "cfl_200"):
            if role not in profiles:
                continue
            left, right, x = matched_values(
                profiles[role], baseline, "first_deposition_s")
            error = relative_error_values(left, right)
            axes[0,
                 1].semilogy(x,
                             np.maximum(error,
                                        PLOT_ERROR_FLOOR),
                             marker="o",
                             ms=3,
                             color=COLORS[role],
                             label=label_for(profiles[role]))
    axes[0, 1].axvline(80.0, color="0.35", ls="--", lw=1,
                       label="peak-HRR spotting range")
    axes[0,
         1].set(xlabel="Structure centre x (m)",
                ylabel="Relative first-deposition-time error",
                title="(b) First-deposition timing (dump-cadence diagnostic)")

    target_id = int(case["history_structure_left_m"] //
                    (case["structure_width_m"] + case["firebreak_width_m"])) + 1
    for item in manifest:
        profile = profiles.get(item["role"])
        if profile is None or profile["history_time_s"].size == 0:
            continue
        index = np.where(profile["ids"] == target_id)[0]
        if index.size:
            axes[1, 0].plot(profile["history_time_s"], profile["history_pcs_m2"][
                            :, index[0]], color=COLORS[item["role"]], label=label_for(profile))
    axes[1, 0].set(xlabel="Time (s)", ylabel="Accumulated firebrands (pcs/m$^2$)",
                   title=f"(c) Accumulation history, {case['history_structure_left_m']:g}--{case['history_structure_right_m']:g} m")

    if baseline is not None:
        for role in ("cfl_1", "cfl_10", "cfl_200"):
            if role not in profiles:
                continue
            left, right, x = matched_values(profiles[role], baseline, "ignition_s")
            error = relative_error_values(left, right)
            axes[1,
                 1].semilogy(x,
                             np.maximum(error,
                                        PLOT_ERROR_FLOOR),
                             marker="o",
                             ms=3,
                             color=COLORS[role],
                             label=label_for(profiles[role]))
    axes[1,
         1].set(xlabel="Structure centre x (m)",
                ylabel="Relative full-ignition-time error",
                title="(d) Full-ignition timing relative to CFL=0.5")

    for axis in axes.flat:
        axis.grid(True, which="both", alpha=0.25)
        handles, labels = axis.get_legend_handles_labels()
        if handles:
            axis.legend(fontsize=7)
        elif not axis.lines:
            axis.text(0.5, 0.5, "Current matching outputs not available",
                      ha="center", va="center", transform=axis.transAxes)
    polish_figure(fig)
    fig.savefig(
        case_dir /
        "figures" /
        "temporal_resolution_profiles.pdf",
        dpi=FIGURE_DPI)
    plt.close(fig)


def plot_ignition_error_convergence(case_dir, profiles):
    """Plot mean ignition-time relative error as a function of wind-based CFL."""
    fig, ax = plt.subplots(figsize=(6.6, 4.0), constrained_layout=True)
    baseline = profiles.get("baseline")
    values = []
    if baseline is not None:
        for role in ("cfl_1", "cfl_10", "cfl_200"):
            if role not in profiles:
                continue
            left, right, _ = matched_values(profiles[role], baseline, "ignition_s")
            errors = relative_error_values(left, right)
            if np.isfinite(errors).any():
                values.append((profiles[role]["cfl_wind"], float(np.nanmean(errors))))
    if values:
        values.sort()
        ax.loglog([v[0] for v in values], [max(v[1], PLOT_ERROR_FLOOR)
                  for v in values], marker="o", color="#1f77b4")
    else:
        ax.text(0.5, 0.5, "Current matching outputs not available",
                ha="center", va="center", transform=ax.transAxes)
    ax.axhline(1.0e-3, color="0.35", ls="--", label="0.1% relative-error criterion")
    ax.set_xlabel("Wind-based level-set CFL")
    ax.set_ylabel("Mean ignition-time relative error")
    ax.set_title("Mean ignition-time sensitivity")
    ax.grid(True, which="both", alpha=0.25)
    ax.legend(fontsize=8)
    polish_figure(fig)
    fig.savefig(case_dir / "figures" / "temporal_resolution_error.pdf", dpi=FIGURE_DPI)
    plt.close(fig)


def latex_escape(value):
    """Escape generated text before insertion into the standalone LaTeX report."""
    text = str(value)
    for old, new in (("\\", r"\textbackslash{}"), ("_", r"\_"),
                     ("%", r"\%"), ("&", r"\&"), ("#", r"\#"), ("<=", r"$\leq$")):
        text = text.replace(old, new)
    return text


def display_value(value):
    """Format a calculated metric for stable JSON and LaTeX presentation."""
    if isinstance(value, float):
        return f"{value:.{METRIC_DIGITS}g}"
    return str(value)


def write_artifacts(case_dir, case, manifest, profiles, rows, overall, diagnostics):
    """Write machine-readable evidence and report table macros."""
    output = {
        "case_id": case["id"],
        "overall_status": overall,
        "completed_variants": [item["name"] for item in manifest if item["role"] in profiles],
        "unrun_variants": [item["name"] for item in manifest if item["role"] not in profiles],
        "metrics": rows,
        "diagnostics": diagnostics,
        "normalization": "deposited ember count divided by the 10 m by 10 m physical structure footprint",
        "first_deposition_resolution_s": case.get("dump_interval_s", 500.0),
    }
    output_dir = case_dir / "outputs"
    output_dir.mkdir(exist_ok=True)
    (output_dir / "metrics.json").write_text(json.dumps(output, indent=2) + "\n", encoding="utf-8")
    table_rows = [
        "{} & {} & {} & {} \\\\".format(
            latex_escape(row["metric"]), latex_escape(row["limit"]),
            latex_escape(display_value(row["calculated"])), latex_escape(row["status"]),
        ) for row in rows
    ]
    variant_rows = [
        "{} & {:.1f} & {:.5g} & {:.0f} \\\\".format(
            latex_escape(
                item["name"]),
            item["cfl_wind"],
            item["simulation_dt_s"],
            item["tstop_s"]) for item in manifest]
    macros = [
        rf"\def\OverallStatus{{{latex_escape(overall)}}}",
        rf"\def\CompletedVariantCount{{{len(profiles)}}}",
        rf"\def\RequestedVariantCount{{{len(manifest)}}}",
        r"\def\MetricRows{", *table_rows, "}",
        r"\def\VariantRows{", *variant_rows, "}",
    ]
    (case_dir /
     "report" /
     "metrics_macros.tex").write_text(report_text("\n".join(macros) +
                                      "\n"), encoding="utf-8")


def main():
    """Postprocess all current variants and refresh figures, metrics, and macros."""
    case_dir = Path(__file__).resolve().parents[1]
    case = json.loads((case_dir / "case.json").read_text(encoding="utf-8"))
    manifest = json.loads(
        (case_dir /
         "variants" /
         "manifest.json").read_text(
            encoding="utf-8"))
    profiles = {}
    for item in manifest:
        profile = structure_profile(case_dir, case, item)
        if profile is not None:
            profiles[item["role"]] = profile
    rows, overall, diagnostics = calculate_metrics(case, manifest, profiles)
    (case_dir / "figures").mkdir(exist_ok=True)
    plot_temporal_profiles(case_dir, case, manifest, profiles)
    plot_ignition_error_convergence(case_dir, profiles)
    write_artifacts(case_dir, case, manifest, profiles, rows, overall, diagnostics)
    print(
        f"[OK] postprocessed {len(profiles)}/{len(manifest)} variants; status: {overall}")


if __name__ == "__main__":
    main()
    generate_spatial_evidence(
        Path(__file__).resolve().parents[1],
        output_preference=("time_of_arrival", "ember_flux"),
        preferred_variant="cfl0p5_dt0p28",
    )
