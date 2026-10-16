#!/usr/bin/env python3
"""Aggregate the combined ensemble-convergence case ensembles and write verification artifacts.

Reads ``case.json``, ``variants/manifest.json``, and each member's final
arrival-time GeoTIFF. For dedicated SFT members it also combines final ignition
times with timestep-resolved ember-flux dumps to reconstruct interval-censored
first-deposition times and actual ignition-delay samples. It writes quantitative
metrics, LaTeX macros, and two vector-PDF figures. It does not run ELMFIRE.
"""

from report_language import polish_figure, report_text
from spatial_evidence import generate_spatial_evidence
import numpy as np
import rasterio
import csv
import json
import math
import re
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# -----------------------------------------------------------------------------
# Customizable analysis and acceptance parameters
# -----------------------------------------------------------------------------
BUFFER_CELLS = 2
FIT_MIN_DISTANCE_M = 100.0
FIT_MAX_DISTANCE_M = 2800.0
MIN_FIT_CELLS = 3
ACCEPTANCE_MAX_MEAN_SPREAD = 0.10
ACCEPTANCE_MIN_P1_MEAN = 0.90
SFT_SAMPLE_MIN_X_M = 155.0
SFT_SAMPLE_MAX_X_M = 2800.0
STUDENT_T_95 = {2: 12.706, 3: 4.303, 4: 3.182, 5: 2.776,
                6: 2.571, 7: 2.447, 8: 2.365, 9: 2.306,
                10: 2.262, 11: 2.228, 12: 2.201, 13: 2.179,
                14: 2.160, 15: 2.145, 16: 2.131, 17: 2.120,
                18: 2.110, 19: 2.101, 20: 2.093, 21: 2.086,
                22: 2.080, 23: 2.074, 24: 2.069, 25: 2.064,
                26: 2.060, 27: 2.056, 28: 2.052, 29: 2.048,
                30: 2.045}

CASE_DIR = Path(__file__).resolve().parents[1]
FIG_DIR = CASE_DIR / "figures"
OUT_DIR = CASE_DIR / "outputs"
REPORT_DIR = CASE_DIR / "report"


def finite_json(value):
    """Return a JSON-safe float or None for unavailable numerical results."""
    return float(value) if np.isfinite(value) else None


def read_member_ros(entry):
    """Return normalized ROS and provenance from one final TOA raster.

    The regression uses cell-centre coordinates from the GeoTIFF geotransform,
    excludes the two-cell halo, and fits distance as a function of arrival time.
    PIGN=0 is assigned zero only when a valid final raster contains no propagated
    cells; missing rasters are never interpreted as physical zero.
    """
    output_dir = CASE_DIR / entry["working_directory"] / "outputs"
    files = sorted(output_dir.glob("time_of_arrival*.tif"))
    if not files:
        return math.nan, "", 0
    path = files[-1]
    try:
        dataset = rasterio.open(path)
    except rasterio.errors.RasterioIOError:
        return math.nan, path.name, 0
    with dataset:
        toa = dataset.read(1).astype(float)
        gt = dataset.transform.to_gdal()
    row = toa.shape[0] // 2
    times = toa[row]
    x = gt[0] + (np.arange(toa.shape[1]) + 0.5) * gt[1]
    usable = np.zeros(times.shape, dtype=bool)
    usable[BUFFER_CELLS:toa.shape[1] - BUFFER_CELLS] = True
    mask = (usable & np.isfinite(times) & (times > 0.0) & (times < 1.0e8)
            & (x >= FIT_MIN_DISTANCE_M) & (x <= FIT_MAX_DISTANCE_M))
    count = int(np.count_nonzero(mask))
    if count < MIN_FIT_CELLS:
        if float(entry["pign"]) == 0.0:
            return 0.0, path.name, count
        return math.nan, path.name, count
    ros_mps = float(np.polyfit(times[mask], x[mask], 1)[0])
    return ros_mps / float(entry["wind_speed_mps"]), path.name, count


def exactly_one_final(output_dir, prefix):
    """Return one final GeoTIFF, rejecting absent or ambiguous products."""
    files = sorted(p for p in output_dir.glob(f"{prefix}_*.tif")
                   if "transient" not in p.name)
    return files[0] if len(files) == 1 else None


def read_sft_delays(entry):
    """Reconstruct actual SFT delays for one timestep-resolved member.

    The first deposition lies between the previous and current transient dump
    times. Its midpoint is used for the plotted sample, while the timestep is
    retained in metrics as the censoring resolution. Only ember-ignited
    centerline cells beyond the complete-support distance are admitted.
    """
    output_dir = CASE_DIR / entry["working_directory"] / "outputs"
    toa_path = exactly_one_final(output_dir, "time_of_arrival")
    ignition_path = exactly_one_final(output_dir, "ember_ignition")
    time_files = sorted(output_dir.glob("dump_times_*.csv"))
    transient_files = sorted(output_dir.glob("ember_flux_transient_*_d*.tif"))
    if toa_path is None or ignition_path is None or len(
            time_files) != 1 or not transient_files:
        return np.array([], dtype=float), {}

    with time_files[0].open(newline="", encoding="utf-8") as stream:
        dump_rows = list(csv.DictReader(stream))
    dump_times = {int(row["dump_index"]): float(row["time_seconds"])
                  for row in dump_rows if row["is_final_dump"].strip() == "F"}
    ordered_indices = sorted(dump_times)
    previous_times = {index: (dump_times[ordered_indices[position - 1]]
                              if position else 0.0)
                      for position, index in enumerate(ordered_indices)}

    try:
        with rasterio.open(toa_path) as toa_dataset, rasterio.open(
                ignition_path) as ignition_dataset:
            toa = toa_dataset.read(1).astype(float)
            ignition = ignition_dataset.read(1).astype(float)
            gt = toa_dataset.transform.to_gdal()
    except rasterio.errors.RasterioIOError:
        return np.array([], dtype=float), {}
    if toa.shape != ignition.shape or toa.shape != (entry["ny"], entry["nx"]):
        return np.array([], dtype=float), {}
    row = BUFFER_CELLS + (entry["ny"] - 2 * BUFFER_CELLS) // 2
    x = gt[0] + (np.arange(entry["nx"]) + 0.5) * gt[1]
    first_midpoint = np.full(entry["nx"], np.nan, dtype=float)
    selected_transients = []
    for path in transient_files:
        match = re.search(r"_d(\d+)\.tif$", path.name)
        if not match:
            continue
        index = int(match.group(1))
        if index not in dump_times:
            continue
        try:
            dataset = rasterio.open(path)
        except rasterio.errors.RasterioIOError:
            return np.array([], dtype=float), {}
        with dataset:
            if (dataset.height, dataset.width) != toa.shape:
                return np.array([], dtype=float), {}
            deposited = dataset.read(1)[row].astype(float) > 0.0
        new = deposited & ~np.isfinite(first_midpoint)
        first_midpoint[new] = 0.5 * (previous_times[index] + dump_times[index])
        selected_transients.append(path.name)

    usable = np.zeros(entry["nx"], dtype=bool)
    usable[BUFFER_CELLS:entry["nx"] - BUFFER_CELLS] = True
    mask = (usable & (x >= SFT_SAMPLE_MIN_X_M) & (x <= SFT_SAMPLE_MAX_X_M)
            & (ignition[row] > 0) & np.isfinite(toa[row]) & (toa[row] >= 0)
            & np.isfinite(first_midpoint))
    delays = np.maximum(toa[row, mask] - first_midpoint[mask], 0.0)
    provenance = {
        "toa_file": toa_path.name,
        "ember_ignition_file": ignition_path.name,
        "dump_times_file": time_files[0].name,
        "transient_file_count": len(selected_transients),
        "sample_count": int(delays.size),
    }
    return delays, provenance


def t_multiplier(n):
    """Return the two-sided 95% Student-t multiplier for a sample of size n."""
    if n < 2:
        return math.nan
    return STUDENT_T_95.get(n, 1.96)


def write_macros(metrics):
    """Expose scalar decision fields to the standalone LaTeX report."""
    scalar_fields = {
        "status": metrics["status"],
        "verification_passed": metrics["verification_passed"],
        "required_members": metrics["required_members"],
        "evaluated_members": metrics["evaluated_members"],
        "ensemble_size": metrics["ensemble_size"],
        "monotone_with_uncertainty": metrics["monotone_with_uncertainty"],
        "max_cross_resolution_mean_spread": metrics["max_cross_resolution_mean_spread"],
        "acceptance_max_mean_spread": metrics["acceptance_max_mean_spread"],
        "minimum_p1_mean": metrics["minimum_p1_mean"],
        "acceptance_minimum_p1_mean": metrics["acceptance_minimum_p1_mean"],
        "p1_limit_passed": metrics["p1_limit_passed"],
        "required_sft_members": metrics["required_sft_members"],
        "evaluated_sft_members": metrics["evaluated_sft_members"],
        "sft_delay_samples": metrics["sft_delay_samples"],
        "sft_expected_quantile_s": metrics["sft_expected_quantile_s"],
        "sft_observed_quantile_s": metrics["sft_observed_quantile_s"],
        "sft_quantile_absolute_error_s": metrics["sft_quantile_absolute_error_s"],
        "sft_quantile_error_limit_s": metrics["sft_quantile_error_limit_s"],
        "sft_bootstrap_ci95_halfwidth_s": metrics["sft_bootstrap_ci95_halfwidth_s"],
        "sft_bootstrap_halfwidth_limit_s": metrics["sft_bootstrap_halfwidth_limit_s"],
        "sft_delay_passed": metrics["sft_delay_passed"],
    }
    lines = []
    for key, value in scalar_fields.items():
        safe = re.sub(r"[^A-Za-z0-9]+", "", key)
        if value is None:
            latex_value = "not available"
        elif isinstance(value, float):
            latex_value = "not available" if not np.isfinite(value) else f"{value:.4f}"
        else:
            latex_value = str(value).replace("_", "\\_")
        lines.append(
            f"\\expandafter\\def\\csname metric@{safe}\\endcsname{{{latex_value}}}")
    (REPORT_DIR / "metrics_macros.tex").write_text(report_text("\n".join(lines) + "\n"), encoding="utf-8")


def main():
    """Calculate member and ensemble statistics and the Boolean decision."""
    FIG_DIR.mkdir(exist_ok=True)
    OUT_DIR.mkdir(exist_ok=True)
    REPORT_DIR.mkdir(exist_ok=True)
    case = json.loads((CASE_DIR / "case.json").read_text(encoding="utf-8"))
    manifest = json.loads(
        (CASE_DIR / "variants/manifest.json").read_text(encoding="utf-8"))
    response_manifest = [v for v in manifest if v.get(
        "role", "probability_response") == "probability_response"]
    delay_manifest = [v for v in manifest if v.get("role") == "sft_delay"]
    required_members = len(case["variants"]) * int(case["ensemble"]["members"])
    required_sft_members = int(case["sft_delay_ensemble"]["members"])
    if len(response_manifest) != required_members or len(
            delay_manifest) != required_sft_members:
        raise RuntimeError(
            f"manifest has {len(response_manifest)} response and {len(delay_manifest)} "
            f"SFT members; expected {required_members} and {required_sft_members}"
        )

    member_records = []
    for entry in response_manifest:
        value, filename, fit_cells = read_member_ros(entry)
        member_records.append({
            "name": entry["name"], "dx_m": float(entry["dx"]),
            "pign": float(entry["pign"]), "member": int(entry["member"]),
            "seed": int(entry["seed"]), "normalized_ros": finite_json(value),
            "fit_cells": fit_cells, "toa_file": filename,
        })

    groups = []
    for dx in sorted({float(v["dx"]) for v in response_manifest}):
        for pign in sorted({float(v["pign"]) for v in response_manifest}):
            selected = [r for r in member_records if r["dx_m"]
                        == dx and r["pign"] == pign]
            values = np.asarray([r["normalized_ros"] for r in selected
                                 if r["normalized_ros"] is not None], dtype=float)
            n = values.size
            mean = float(np.mean(values)) if n else math.nan
            sd = float(np.std(values, ddof=1)) if n >= 2 else math.nan
            se = sd / math.sqrt(n) if n >= 2 else math.nan
            halfwidth = t_multiplier(n) * se if n >= 2 else math.nan
            groups.append({
                "dx_m": dx, "pign": pign, "n": int(n),
                "mean_normalized_ros": finite_json(mean),
                "sample_sd": finite_json(sd), "standard_error": finite_json(se),
                "ci95_halfwidth": finite_json(halfwidth),
                "ci95_lower": finite_json(mean - halfwidth),
                "ci95_upper": finite_json(mean + halfwidth),
            })

    complete = all(g["n"] == int(case["ensemble"]["members"]) for g in groups)
    monotone = complete
    monotonicity_checks = []
    for dx in sorted({g["dx_m"] for g in groups}):
        dx_groups = sorted(
            (g for g in groups if g["dx_m"] == dx),
            key=lambda g: g["pign"])
        for left, right in zip(dx_groups[:-1], dx_groups[1:]):
            available = all(
                value is not None
                for value in (
                    left["mean_normalized_ros"], right["mean_normalized_ros"],
                    left["ci95_halfwidth"], right["ci95_halfwidth"],
                )
            )
            if available:
                difference = right["mean_normalized_ros"] - left["mean_normalized_ros"]
                pooled_uncertainty = math.hypot(
                    left["ci95_halfwidth"], right["ci95_halfwidth"])
                passed = difference >= -pooled_uncertainty
            else:
                difference = math.nan
                pooled_uncertainty = math.nan
                passed = False
            monotone = monotone and passed
            monotonicity_checks.append({
                "dx_m": dx, "pign_low": left["pign"], "pign_high": right["pign"],
                "mean_difference": finite_json(difference),
                "pooled_ci95_halfwidth": finite_json(pooled_uncertainty), "passed": passed,
            })

    spread_checks = []
    for pign in sorted({g["pign"] for g in groups}):
        means = [g["mean_normalized_ros"] for g in groups
                 if g["pign"] == pign and g["mean_normalized_ros"] is not None]
        spread = max(means) - min(means) if len(means) == 4 else math.nan
        spread_checks.append(
            {
                "pign": pign,
                "mean_spread": finite_json(spread),
                "limit": ACCEPTANCE_MAX_MEAN_SPREAD,
                "passed": bool(
                    np.isfinite(spread) and spread <= ACCEPTANCE_MAX_MEAN_SPREAD)})
    max_spread = max((s["mean_spread"] for s in spread_checks
                      if s["mean_spread"] is not None), default=math.nan)
    spread_passed = len(spread_checks) == 5 and all(s["passed"] for s in spread_checks)
    p1_means = [g["mean_normalized_ros"] for g in groups if g["pign"]
                == 1.0 and g["mean_normalized_ros"] is not None]
    minimum_p1_mean = min(p1_means) if len(p1_means) == 4 else math.nan
    p1_limit_passed = bool(np.isfinite(minimum_p1_mean)
                           and minimum_p1_mean >= ACCEPTANCE_MIN_P1_MEAN)
    response_passed = complete and monotone and spread_passed and p1_limit_passed

    # Pool actual cell delays progressively by independent member. The model
    # defines tau as the PIGN quantile because F(tau)=PIGN. Bootstrap intervals
    # quantify sampling uncertainty; transient-dump spacing quantifies temporal
    # censoring separately and is recorded for audit.
    delay_config = case["sft_delay_ensemble"]
    delay_member_records = []
    cumulative_records = []
    pooled = []
    rng = np.random.default_rng(int(case["ensemble"]["base_seed"]) + 7919)
    for entry in sorted(delay_manifest, key=lambda item: int(item["member"])):
        delays, provenance = read_sft_delays(entry)
        delay_member_records.append({
            "name": entry["name"], "member": int(entry["member"]),
            "seed": int(entry["seed"]), "dt_s": float(entry["dt_s"]),
            "sample_count": int(delays.size), **provenance,
        })
        if delays.size:
            pooled.extend(delays.tolist())
        values = np.asarray(pooled, dtype=float)
        if values.size:
            quantile = float(np.quantile(values, float(delay_config["pign"])))
            bootstrap = np.empty(int(delay_config["bootstrap_replicates"]), dtype=float)
            for index in range(bootstrap.size):
                sample = rng.choice(values, size=values.size, replace=True)
                bootstrap[index] = np.quantile(sample, float(delay_config["pign"]))
            lower, upper = np.quantile(bootstrap, [0.025, 0.975])
            cumulative_records.append({
                "members": int(entry["member"]), "samples": int(values.size),
                "quantile_s": quantile, "ci95_lower_s": float(lower),
                "ci95_upper_s": float(upper),
                "ci95_halfwidth_s": float(0.5 * (upper - lower)),
            })

    evaluated_sft_members = sum(
        record["sample_count"] > 0 for record in delay_member_records)
    sft_complete = evaluated_sft_members == required_sft_members
    final_delay = cumulative_records[-1] if cumulative_records else None
    expected_quantile = float(delay_config["tau_s"])
    observed_quantile = final_delay["quantile_s"] if final_delay else math.nan
    quantile_error = abs(
        observed_quantile -
        expected_quantile) if final_delay else math.nan
    bootstrap_halfwidth = final_delay["ci95_halfwidth_s"] if final_delay else math.nan
    sample_passed = bool(
        final_delay and final_delay["samples"] >= int(
            delay_config["minimum_samples"]))
    quantile_passed = bool(
        np.isfinite(quantile_error) and quantile_error <= float(
            delay_config["quantile_absolute_error_max_s"]))
    uncertainty_passed = bool(
        np.isfinite(bootstrap_halfwidth) and bootstrap_halfwidth <= float(
            delay_config["bootstrap_ci95_halfwidth_max_s"]))
    sft_delay_passed = sft_complete and sample_passed and quantile_passed and uncertainty_passed
    all_complete = complete and sft_complete
    passed = response_passed and sft_delay_passed

    fig, ax = plt.subplots(figsize=(7.4, 4.7), constrained_layout=True)
    colors = plt.rcParams["axes.prop_cycle"].by_key()["color"]
    for color, dx in zip(colors, sorted({g["dx_m"] for g in groups})):
        dx_groups = sorted(
            (g for g in groups if g["dx_m"] == dx),
            key=lambda g: g["pign"])
        xs = np.asarray([g["pign"] for g in dx_groups])
        ys = np.asarray([g["mean_normalized_ros"] for g in dx_groups], dtype=float)
        err = np.asarray([g["ci95_halfwidth"] for g in dx_groups], dtype=float)
        ax.errorbar(xs, ys, yerr=err, marker="o", capsize=3, color=color,
                    linewidth=1.8, label=fr"$\Delta x={dx:g}$ m (mean $\pm$ 95% CI)")
        for group in dx_groups:
            member_y = [r["normalized_ros"] for r in member_records
                        if r["dx_m"] == dx and r["pign"] == group["pign"]
                        and r["normalized_ros"] is not None]
            ax.scatter(np.full(len(member_y), group["pign"]), member_y,
                       s=10, alpha=0.18, color=color, edgecolors="none")
    ax.set_xlabel(r"Small-firebrand ignition probability, $P_{\mathrm{ign,small}}$ (-)")
    ax.set_ylabel(r"Normalized leading-edge ROS, $\overline{R}/u_{\mathrm{wind}}$ (-)")
    ax.axhline(
        1.0,
        color="black",
        linestyle="--",
        linewidth=1.0,
        label="wind-speed limit")
    ax.set_ylim(0.0, 1.05)
    ax.grid(True, alpha=0.25)
    ax.legend(ncol=2, fontsize=8)
    polish_figure(fig)
    fig.savefig(FIG_DIR / "ignition_probability_convergence.pdf", format="pdf")
    plt.close(fig)

    # Reproduce the distribution-level model check before presenting the
    # supplemental ensemble-size convergence diagnostic.
    distribution_fig, distribution_ax = plt.subplots(
        figsize=(7.4, 4.5), constrained_layout=True)
    pooled_values = np.asarray(pooled, dtype=float)
    if pooled_values.size:
        bin_width = min(float(record["dt_s"]) for record in delay_member_records)
        upper = max(30.0, math.ceil(float(np.max(pooled_values)) / bin_width) * bin_width)
        bins = np.arange(0.0, upper + bin_width, bin_width)
        distribution_ax.hist(pooled_values, bins=bins, density=True,
                             color="#4c78a8", alpha=0.75,
                             label="ELMFIRE delay samples")
        distribution_ax.axvline(observed_quantile, color="black", linestyle="--",
                                linewidth=1.6,
                                label=rf"observed $q_{{0.9}}={observed_quantile:.2f}$ s")
    else:
        distribution_ax.text(0.5, 0.5, "SFT-delay ensemble not yet run",
                             ha="center", va="center",
                             transform=distribution_ax.transAxes)
    distribution_ax.axvline(expected_quantile, color="#d62728", linewidth=1.8,
                            label=rf"prescribed $q_{{0.9}}={expected_quantile:g}$ s")
    distribution_ax.set(xlabel=r"SFT ignition delay, $t_{\mathrm{ign,small}}$ [s]",
                        ylabel=r"Probability density [s$^{-1}$]",
                        xlim=(0.0, 30.0), ylim=(0.0, None))
    distribution_ax.grid(axis="y", alpha=0.2)
    distribution_ax.legend(fontsize=8)
    polish_figure(distribution_fig)
    distribution_fig.savefig(FIG_DIR / "sft_delay_distribution.pdf", format="pdf")
    plt.close(distribution_fig)

    delay_fig, delay_ax = plt.subplots(figsize=(7.4, 4.5), constrained_layout=True)
    if cumulative_records:
        members_x = np.asarray([record["members"] for record in cumulative_records])
        quantiles = np.asarray([record["quantile_s"] for record in cumulative_records])
        lower = np.asarray([record["ci95_lower_s"] for record in cumulative_records])
        upper = np.asarray([record["ci95_upper_s"] for record in cumulative_records])
        delay_ax.plot(members_x, quantiles, marker="o", color="#1f77b4",
                      label=r"ELMFIRE pooled $q_{0.9}$")
        delay_ax.fill_between(members_x, lower, upper, color="#1f77b4", alpha=.2,
                              label="bootstrap 95% CI")
    else:
        delay_ax.text(.5, .5, "SFT-delay ensemble not yet run",
                      ha="center", va="center", transform=delay_ax.transAxes)
    delay_ax.axhline(expected_quantile, color="black", ls="--",
                     label=r"model $q_{0.9}=\tau=10$ s")
    delay_ax.axhspan(
        expected_quantile -
        float(
            delay_config["quantile_absolute_error_max_s"]),
        expected_quantile +
        float(
            delay_config["quantile_absolute_error_max_s"]),
        color="#2ca02c",
        alpha=.15,
        label="acceptance band")
    delay_ax.set_xlabel("Number of independent ensemble members pooled (-)")
    delay_ax.set_ylabel(r"90th-percentile SFT delay, $q_{0.9}$ (s)")
    delay_ax.set_xlim(.8, required_sft_members + .2)
    delay_ax.set_ylim(0, max(15.0, expected_quantile + 3.0))
    delay_ax.grid(True, alpha=.25)
    delay_ax.legend(fontsize=8)
    polish_figure(delay_fig)
    delay_fig.savefig(FIG_DIR / "sft_delay_ensemble_convergence.pdf", format="pdf")
    plt.close(delay_fig)

    metrics = {
        "case_id": case["id"],
        "status": "pass" if passed else ("fail" if all_complete else "insufficient_output"),
        "verification_passed": passed if all_complete else "not_evaluated",
        "ensemble_size": int(case["ensemble"]["members"]),
        "required_members": required_members,
        "evaluated_members": sum(r["normalized_ros"] is not None for r in member_records),
        "confidence_level": float(case["ensemble"]["confidence_level"]),
        "fit_distance_m": [FIT_MIN_DISTANCE_M, FIT_MAX_DISTANCE_M],
        "monotone_with_uncertainty": monotone,
        "max_cross_resolution_mean_spread": finite_json(max_spread),
        "acceptance_max_mean_spread": ACCEPTANCE_MAX_MEAN_SPREAD,
        "spread_passed": spread_passed,
        "minimum_p1_mean": finite_json(minimum_p1_mean),
        "acceptance_minimum_p1_mean": ACCEPTANCE_MIN_P1_MEAN,
        "p1_limit_passed": p1_limit_passed,
        "response_passed": response_passed,
        "required_sft_members": required_sft_members,
        "evaluated_sft_members": evaluated_sft_members,
        "sft_delay_samples": final_delay["samples"] if final_delay else 0,
        "sft_minimum_samples": int(delay_config["minimum_samples"]),
        "sft_sample_count_passed": sample_passed,
        "sft_expected_quantile_s": expected_quantile,
        "sft_observed_quantile_s": finite_json(observed_quantile),
        "sft_quantile_absolute_error_s": finite_json(quantile_error),
        "sft_quantile_error_limit_s": float(delay_config["quantile_absolute_error_max_s"]),
        "sft_quantile_passed": quantile_passed,
        "sft_bootstrap_ci95_halfwidth_s": finite_json(bootstrap_halfwidth),
        "sft_bootstrap_halfwidth_limit_s": float(delay_config["bootstrap_ci95_halfwidth_max_s"]),
        "sft_uncertainty_passed": uncertainty_passed,
        "sft_delay_passed": sft_delay_passed,
        "sft_temporal_censoring_dt_s": float(delay_manifest[0]["dt_s"]),
        "sft_sample_distance_m": [SFT_SAMPLE_MIN_X_M, SFT_SAMPLE_MAX_X_M],
        "sft_cumulative_statistics": cumulative_records,
        "sft_members": delay_member_records,
        "ensemble_statistics": groups,
        "monotonicity_checks": monotonicity_checks,
        "cross_resolution_checks": spread_checks,
        "members": member_records,
    }
    (OUT_DIR / "metrics.json").write_text(json.dumps(metrics, indent=2) + "\n", encoding="utf-8")
    write_macros(metrics)
    print(f"[OK] response {metrics['evaluated_members']}/{required_members}; "
          f"SFT {evaluated_sft_members}/{required_sft_members}: {metrics['status']}")


if __name__ == "__main__":
    main()
    generate_spatial_evidence(
        CASE_DIR, output_preference=("ember_ignition", "time_of_arrival"),
        preferred_variant="dx10_pign0p5_r01",
    )
