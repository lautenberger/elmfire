#!/usr/bin/env python3
"""Evaluate actual outputs from both combined firebrand-transport variants.

The script validates manifest geometry and final GeoTIFF selection, excludes the
two-cell halo, calculates TOA/state-consistency metrics from final products,
extracts leading-edge/ROS histories directly from every dumped level-set field,
and evaluates each variant's accumulation plateau. It then writes JSON, LaTeX
macros, and vector PDFs. It never runs ELMFIRE.
"""

from report_language import polish_figure, report_text
from spatial_evidence import generate_spatial_evidence
from pathlib import Path
import csv
import json
import re
import numpy as np
import rasterio
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# -----------------------------------------------------------------------------
# Customizable postprocessing parameters and acceptance thresholds
# -----------------------------------------------------------------------------
NODATA_FLOOR = -1000.0
EXPECTED_ACCUMULATION_PCS = 1497.2
EMBER_GR_PER_MW_1M = 33.3  # pcs s-1 MW-1 for the represented strip.
REPRESENTED_STRIP_WIDTH_M = 1.0
PER_AREA_EMISSION_DURATION_S = 6.0  # TAU_EMBERGEN in the executed namelist.
PLATEAU_START_M = 155.0
PLATEAU_END_M = 195.0
ACCUMULATION_PLOT_MAX_M = 320.0

CASE_DIR = Path(__file__).resolve().parents[1]
BUFFER_CELLS = 2


def read_raster(path):
    """Read one raster and return its array, geotransform, and dimensions."""
    with rasterio.open(path) as dataset:
        return (
            dataset.read(1).astype(float),
            dataset.transform.to_gdal(),
            (dataset.height, dataset.width),
        )


def exactly_one_final(directory, prefix):
    """Select exactly one final product while excluding transient dumps."""
    files = sorted(p for p in directory.glob(
        f"{prefix}_*.tif") if "transient" not in p.name)
    if len(files) != 1:
        raise RuntimeError(
            f"expected one final {prefix} raster in {directory}, found {len(files)}")
    return files[0]


def read_dump_schedule(directory):
    """Return the unique level-set dump schedule keyed by integer dump index.

    ELMFIRE writes the physical time separately from the transient raster name.
    Reading ``dump_times`` avoids treating the sequential dump index as seconds
    and lets the postprocessor verify that no level-set time step is omitted.
    """
    files = sorted(directory.glob("dump_times_*.csv"))
    if len(files) != 1:
        raise RuntimeError(
            f"expected one dump_times CSV in {directory}, found {len(files)}")
    schedule = {}
    with files[0].open(newline="") as stream:
        for row in csv.DictReader(stream):
            index = int(row["dump_index"])
            if index in schedule:
                raise RuntimeError(f"duplicate dump index {index} in {files[0]}")
            schedule[index] = float(row["time_seconds"])
    if not schedule:
        raise RuntimeError(f"empty dump schedule in {files[0]}")
    return files[0], schedule


def rightmost_zero_crossing(phi_row, x):
    """Interpolate the rightmost finite ``phi=0`` crossing on one raster row.

    A negative level-set value denotes the ignited side of the interface and a
    positive value denotes the unignited side. Exact zeros are accepted, while
    sign-changing cell-centre samples are interpolated linearly. Returning NaN
    is intentional when the front has left the physical domain: a domain-edge
    cell must not be reported as a stationary fireline after no zero contour
    remains in the retained row.
    """
    finite = np.isfinite(phi_row) & (phi_row > NODATA_FLOOR)
    crossings = list(x[finite & np.isclose(phi_row, 0.0, atol=1.0e-12)])
    for index in range(phi_row.size - 1):
        if not (finite[index] and finite[index + 1]):
            continue
        left = phi_row[index]
        right = phi_row[index + 1]
        if left * right < 0.0:
            fraction = -left / (right - left)
            crossings.append(x[index] + fraction * (x[index + 1] - x[index]))
    return float(max(crossings)) if crossings else float("nan")


def extract_phi_leading_edge_history(directory, expected_shape, row, column_slice):
    """Extract one leading-edge sample from every available dumped phi field.

    Raster names provide dump indices; ``dump_times`` provides their physical
    times. The function requires exact one-to-one correspondence, validates all
    raster dimensions/geotransforms, removes the numerical halo, and evaluates
    the rightmost zero contour along the case's centre row.
    """
    schedule_file, schedule = read_dump_schedule(directory)
    phi_files = sorted(directory.glob("phi_*_d*.tif"))
    indexed_files = {}
    for path in phi_files:
        match = re.search(r"_d(\d+)\.tif$", path.name)
        if match is None:
            continue
        index = int(match.group(1))
        if index in indexed_files:
            raise RuntimeError(f"duplicate phi dump index {index} in {directory}")
        indexed_files[index] = path
    missing = sorted(set(schedule) - set(indexed_files))
    unexpected = sorted(set(indexed_files) - set(schedule))
    if missing or unexpected:
        raise RuntimeError(
            f"phi/dump_times mismatch in {directory}: "
            f"missing phi indices={missing}, unexpected phi indices={unexpected}")

    times = []
    leading = []
    reference_gt = None
    for index in sorted(schedule):
        phi, gt, shape = read_raster(indexed_files[index])
        if shape != expected_shape:
            raise RuntimeError(f"stale phi dump dimensions for {indexed_files[index]}")
        if reference_gt is None:
            reference_gt = gt
        elif not np.allclose(gt, reference_gt):
            raise RuntimeError(f"inconsistent phi geotransform for {indexed_files[index]}")
        x_full = gt[0] + (np.arange(shape[1]) + 0.5) * gt[1]
        x = x_full[column_slice]
        times.append(schedule[index])
        leading.append(rightmost_zero_crossing(phi[row, column_slice], x))

    return (schedule_file, np.asarray(times, dtype=float),
            np.asarray(leading, dtype=float), phi_files)


def latex_value(value):
    """Format a JSON scalar safely for a case-local LaTeX macro."""
    if isinstance(value, bool):
        return "PASS" if value else "FAIL"
    if isinstance(value, float):
        return f"{value:.4g}"
    return str(value).replace("_", r"\_")


def evaluate_accumulation_plateau(
        accumulation_density, reference_profile, plateau_mask, minimum_cells,
        relative_error_limit):
    """Evaluate accumulation only where an independent reference is defined.

    ``flin`` and ``vs`` are burned-cell products. A zero or nodata cell does
    not define the PER-MW residence-time reference and must not become an
    artificial zero reference value.
    """
    valid = (
        plateau_mask
        & np.isfinite(accumulation_density)
        & (accumulation_density >= 0.0)
        & np.isfinite(reference_profile)
        & (reference_profile > 0.0)
    )
    reference_cells = int(np.count_nonzero(valid))
    result = {
        "accumulation_reference_cells": reference_cells,
        "accumulation_reference_required_cells": int(minimum_cells),
    }
    if reference_cells < minimum_cells:
        result.update(
            accumulation_plateau_density_pcs_m2="not computed",
            accumulation_reference_density_pcs_m2="not computed",
            accumulation_relative_error="not computed",
            accumulation_passed="NOT EVALUABLE",
            accumulation_reference_status="NOT EVALUABLE",
            accumulation_evaluation_reason=(
                f"fewer than {minimum_cells} plateau cells have finite positive "
                "FLIN/VS reference values; inspect the completed wildland outputs"
            ),
        )
        return result, float("nan")

    plateau_density = float(np.median(accumulation_density[valid]))
    reference_density = float(np.median(reference_profile[valid]))
    # The validity mask guarantees a finite, strictly positive denominator.
    accumulation_error = abs(
        plateau_density - reference_density) / reference_density
    result.update(
        accumulation_plateau_density_pcs_m2=plateau_density,
        accumulation_reference_density_pcs_m2=reference_density,
        accumulation_relative_error=accumulation_error,
        accumulation_passed=accumulation_error <= relative_error_limit,
        accumulation_reference_status="PASS",
        accumulation_evaluation_reason="reference support is sufficient",
    )
    return result, reference_density


def write_reference_figure(profiles, figures):
    """Reproduce the four canonical steady-transport observables.

    The panels retain the analytical accumulation, arrival-time, leading-edge,
    and rate-of-spread references. Acceptance summaries are intentionally kept
    in a separate supplemental figure so they do not replace physical results.
    """
    fig, axes = plt.subplots(2, 2, figsize=(10.0, 7.2), constrained_layout=True)
    colors = {"transport_impulse": "#1f77b4", "wildland_no_delay": "#d62728"}
    labels = {"transport_impulse": "transport impulse",
              "wildland_no_delay": "immediate ignition"}
    for name in ("transport_impulse", "wildland_no_delay"):
        x, sim, ref, times, leading, expected = profiles[name]
        xa, density, reference_density, _ = profiles[name + "_accumulation"]
        display = (xa >= 0.0) & (xa <= ACCUMULATION_PLOT_MAX_M)
        axes[0, 0].plot(xa[display], density[display], color=colors[name],
                        marker="o", markersize=2.3, linewidth=1.0,
                        label=labels[name])
        axes[0, 0].axhline(reference_density, color=colors[name],
                           linestyle="--", linewidth=1.2,
                           label=f"{labels[name]} reference")
        valid = np.isfinite(sim) & (sim > NODATA_FLOOR)
        axes[0, 1].plot(x[valid], sim[valid], color=colors[name], marker="o",
                        markersize=2.3, linewidth=1.0, label=labels[name])
        axes[1, 0].plot(times, leading, color=colors[name], marker="o",
                        markersize=2.3, linewidth=1.0, label=labels[name])
        ros = np.full_like(times, np.nan, dtype=float)
        if times.size > 1:
            ros[1:] = np.diff(leading) / np.diff(times)
        axes[1, 1].plot(times, ros, color=colors[name], marker="o",
                        markersize=2.0, linewidth=0.8, label=labels[name])
    x, _, ref, times, _, expected = profiles["transport_impulse"]
    axes[0, 1].plot(x, ref, "k-", linewidth=1.5, label=r"$T=(x-x_0)/U$")
    axes[1, 0].plot(times, expected, "k-", linewidth=1.5,
                    label=r"$x_{LE}=x_0+Ut$")
    wind_speed = (expected[-1] - expected[0]) / (times[-1] - times[0])
    axes[1, 1].axhline(wind_speed, color="black", linewidth=1.5,
                       label=r"$ROS=U$")
    axes[0, 0].set(xlabel="Downwind distance [m]",
                   ylabel=r"Accumulated firebrands [pcs/m$^2$]",
                   xlim=(0.0, ACCUMULATION_PLOT_MAX_M), ylim=(0.0, None))
    axes[0, 1].set(xlabel="Downwind distance [m]", ylabel="Time of arrival [s]",
                   ylim=(0.0, None))
    axes[1, 0].set(xlabel="Time [s]", ylabel="Leading-edge position [m]",
                   ylim=(0.0, None))
    axes[1, 1].set(xlabel="Time [s]", ylabel="Leading-edge ROS [m/s]",
                   ylim=(0.0, None))
    for label, axis in zip(("(a)", "(b)", "(c)", "(d)"), axes.flat):
        axis.text(0.01, 0.98, label, transform=axis.transAxes,
                  ha="left", va="top", fontweight="bold")
        axis.grid(alpha=0.2)
        axis.legend(fontsize=7)
    polish_figure(fig)
    fig.savefig(figures / "steady_transport_reference.pdf")
    plt.close(fig)


def main():
    """Calculate the declared comprehensive verification decision."""
    config = json.loads((CASE_DIR / "case.json").read_text())
    manifest = json.loads((CASE_DIR / "variants" / "manifest.json").read_text())
    limits = config["acceptance"]
    results = {}
    profiles = {}
    expected_accumulation = EXPECTED_ACCUMULATION_PCS
    complete = set(manifest["required_variants"]) == {
        v["name"] for v in manifest["variants"]}
    for variant in manifest["variants"]:
        name = variant["name"]
        root = CASE_DIR / variant["path"]
        out = root / "outputs"
        if not any(out.glob("*.tif")):
            complete = False
            results[name] = {
                "phi_dump_count": "not computed",
                "scheduled_phi_dump_count": "not computed",
                "phi_observable_front_count": "not computed",
                "phi_dump_completeness_passed": "NOT EVALUABLE",
                "toa_mean_relative_error": "not computed",
                "toa_passed": "NOT EVALUABLE",
                "leading_edge_normalized_rmse": "not computed",
                "leading_edge_passed": "NOT EVALUABLE",
                "mean_ros_relative_error": "not computed",
                "mean_ros_passed": "NOT EVALUABLE",
                "ember_ignition_state_consistency_passed": "NOT EVALUABLE",
                "accumulation_relative_error": "not computed",
                "accumulation_passed": "NOT EVALUABLE",
                "accumulation_reference_cells": "not computed",
                "accumulation_reference_required_cells": int(
                    limits["minimum_accumulation_reference_cells"]),
                "accumulation_reference_status": "NOT EVALUABLE",
                "accumulation_evaluation_reason": "required raster outputs are missing",
                "total_accumulated_firebrands_domain_pcs": "not computed",
                "active_deposition_rows": "not computed",
            }
            continue
        toa_file = exactly_one_final(out, "time_of_arrival")
        ignition_file = exactly_one_final(out, "ember_ignition")
        toa, gt, shape = read_raster(toa_file)
        ignition, _, ignition_shape = read_raster(ignition_file)
        expected_shape = (variant["ny"], variant["nx"])
        if shape != expected_shape or ignition_shape != expected_shape:
            raise RuntimeError(f"stale raster dimensions for {name}")
        dx = variant["cell_size_m"]
        row = BUFFER_CELLS + variant["physical_ny"] // 2
        sl = slice(BUFFER_CELLS, -BUFFER_CELLS)
        x = gt[0] + (np.arange(shape[1]) + 0.5) * gt[1]
        x = x[sl]
        sim = toa[row, sl]
        ignition_x = x[0]
        reference = np.maximum(x - ignition_x, 0.0) / variant["wind_speed_mps"]
        valid = np.isfinite(sim) & (sim > NODATA_FLOOR) & (reference > 0)
        relative = np.abs(sim[valid] - reference[valid]) / reference[valid]
        toa_mean = float(relative.mean()) if relative.size else float("inf")
        toa_max = float(relative.max()) if relative.size else float("inf")
        # Build the front history from the instantaneous level-set solution,
        # not by inverting the final TOA field. Each scheduled phi dump is read;
        # samples after the zero contour leaves the domain are retained as NaN
        # in the completeness accounting but excluded from trajectory metrics.
        schedule_file, dump_times, dump_leading, phi_files = \
            extract_phi_leading_edge_history(out, expected_shape, row, sl)
        observable = np.isfinite(dump_leading)
        times = dump_times[observable]
        leading = dump_leading[observable]
        if times.size:
            initial_front_x = float(leading[0])
            expected_leading = initial_front_x + variant["wind_speed_mps"] * (
                times - times[0])
            reference_displacement = max(
                variant["wind_speed_mps"] * (times[-1] - times[0]), dx)
            leading_rmse = float(
                np.sqrt(np.mean((leading - expected_leading) ** 2)) /
                reference_displacement)
        else:
            initial_front_x = float("nan")
            expected_leading = np.array([], dtype=float)
            leading_rmse = float("inf")
        mean_ros = float(np.polyfit(times, leading, 1)[
                         0]) if times.size >= 2 else float("nan")
        ros_error = abs(mean_ros - variant["wind_speed_mps"]) / \
            variant["wind_speed_mps"] if np.isfinite(mean_ros) else float("inf")
        ignited = ignition[BUFFER_CELLS:-BUFFER_CELLS, BUFFER_CELLS:-BUFFER_CELLS] > 0
        toa_phys = toa[BUFFER_CELLS:-BUFFER_CELLS, BUFFER_CELLS:-BUFFER_CELLS]
        state_ok = bool(
            np.all(
                np.isfinite(
                    toa_phys[ignited]) & (
                    toa_phys[ignited] >= 0)))
        flux_file = exactly_one_final(out, "ember_flux")
        flux, _, flux_shape = read_raster(flux_file)
        if flux_shape != expected_shape:
            raise RuntimeError(f"stale ember accumulation dimensions for {name}")
        # The reference experiment is a one-dimensional, 1 m cross-wind strip.
        # ELMFIRE stores a count per raster pixel, so divide the centre-row
        # count by dx*1 m before labelling the profile as an areal density.
        accumulation_density = flux[row, sl] / (dx * REPRESENTED_STRIP_WIDTH_M)
        plateau_mask = (x >= PLATEAU_START_M) & (x <= PLATEAU_END_M)
        if not np.any(plateau_mask):
            raise RuntimeError(f"empty accumulation comparison interval for {name}")
        physical_flux = flux[BUFFER_CELLS:-BUFFER_CELLS, sl]

        item = {
            "selected_toa_file": str(
                toa_file.relative_to(CASE_DIR)),
            "selected_ignition_file": str(
                ignition_file.relative_to(CASE_DIR)),
            "selected_ember_flux_file": str(
                flux_file.relative_to(CASE_DIR)),
            "selected_phi_dump_times_file": str(
                schedule_file.relative_to(CASE_DIR)),
            "selected_phi_dump_pattern": str(
                (out / "phi_*_d*.tif").relative_to(CASE_DIR)),
            "leading_edge_extraction":
                "rightmost linearly interpolated phi=0 crossing on the physical centre row",
            "phi_dump_count": len(phi_files),
            "scheduled_phi_dump_count": int(dump_times.size),
            "phi_observable_front_count": int(np.count_nonzero(observable)),
            "phi_unobservable_after_exit_count": int(np.count_nonzero(~observable)),
            "phi_dump_completeness_passed": len(phi_files) == dump_times.size,
            "leading_edge_initial_position_m": initial_front_x,
            "leading_edge_first_time_s": float(times[0]) if times.size else float("nan"),
            "leading_edge_last_time_s": float(times[-1]) if times.size else float("nan"),
            "toa_cells": int(
                relative.size),
            "toa_mean_relative_error": toa_mean,
            "toa_max_relative_error": toa_max,
            "toa_passed": bool(
                relative.size >= limits["minimum_toa_cells"] and toa_mean <= limits["toa_mean_relative_error_max"] and toa_max <= limits["toa_max_relative_error_max"]),
            "leading_edge_normalized_rmse": leading_rmse,
            "leading_edge_passed": leading_rmse <= limits["leading_edge_normalized_rmse_max"],
            "mean_ros_mps": mean_ros,
            "mean_ros_relative_error": ros_error,
            "mean_ros_passed": ros_error <= limits["mean_ros_relative_error_max"],
            "ember_ignition_state_consistency_passed": state_ok,
            "accumulation_normalization": "centre-row pixel count / (dx * 1 m represented strip width)",
            "total_accumulated_firebrands_domain_pcs": float(
                np.sum(physical_flux)),
        }
        if name == "transport_impulse":
            # PER-AREA emits EMBER_GR*dx*dy*dt each step for TAU_EMBERGEN.
            # Here dy=dx in the raster, while the displayed density is for the
            # declared 1 m strip; the resulting reference is configuration-based.
            reference_density = (variant["ember_gr"] *
                                 dx *
                                 dx *
                                 PER_AREA_EMISSION_DURATION_S /
                                 (dx *
                                  REPRESENTED_STRIP_WIDTH_M))
            reference_profile = np.full_like(
                accumulation_density, reference_density, dtype=float)
        else:
            flin_file = exactly_one_final(out, "flin")
            flin, _, flin_shape = read_raster(flin_file)
            vs_file = exactly_one_final(out, "vs")
            vs, _, vs_shape = read_raster(vs_file)
            if flin_shape != expected_shape or vs_shape != expected_shape:
                raise RuntimeError("stale wildland accumulation-reference dimensions")
            flin_profile = flin[row, sl]
            vs_mps = vs[row, sl] * 0.3048 / 60.0
            source_reference_valid = (
                np.isfinite(flin_profile)
                & np.isfinite(vs_mps)
                & (flin_profile > 0.0)
                & (vs_mps > 0.0)
            )
            reference_profile = np.full_like(vs_mps, np.nan, dtype=float)
            reference_profile[source_reference_valid] = (
                EMBER_GR_PER_MW_1M
                * (flin_profile[source_reference_valid] / 1000.0)
                / vs_mps[source_reference_valid]
            )
            item.update(selected_flin_file=str(flin_file.relative_to(CASE_DIR)),
                        selected_vs_file=str(vs_file.relative_to(CASE_DIR)),
                        reference_nominal_accumulation_pcs=EXPECTED_ACCUMULATION_PCS)
        item["accumulation_observed_plateau_median_all_cells_pcs_m2"] = float(
            np.median(accumulation_density[plateau_mask]))
        accumulation_result, reference_density = evaluate_accumulation_plateau(
            accumulation_density,
            reference_profile,
            plateau_mask,
            int(limits["minimum_accumulation_reference_cells"]),
            float(limits["accumulation_relative_error_max"]),
        )
        active_rows = int(np.count_nonzero(np.sum(physical_flux, axis=1) > 0))
        item.update(
            active_deposition_rows=active_rows,
            **accumulation_result)
        profiles[name + "_accumulation"] = (x,
                                            accumulation_density,
                                            reference_density,
                                            plateau_mask)
        results[name] = item
        profiles[name] = (x, sim, reference, times, leading, expected_leading)
    components = [complete]
    for name, item in results.items():
        components += [item["phi_dump_completeness_passed"] is True,
                       item["toa_passed"] is True,
                       item["leading_edge_passed"] is True,
                       item["mean_ros_passed"] is True,
                       item["ember_ignition_state_consistency_passed"] is True]
        components.append(item["accumulation_passed"] is True)
    passed = bool(all(components))
    required_variant_count = len(manifest["required_variants"])
    evaluated_variant_count = sum(
        "selected_toa_file" in item for item in results.values())
    output_complete = complete and evaluated_variant_count == required_variant_count
    unevaluable_reasons = [
        f"{name}: {item.get('accumulation_evaluation_reason', 'metric unavailable')}"
        for name, item in results.items()
        if item.get("accumulation_passed") == "NOT EVALUABLE"
    ]
    evaluation_complete = output_complete and not unevaluable_reasons
    overall_status = (
        "PASS" if evaluation_complete and passed
        else "FAIL" if evaluation_complete
        else "NOT EVALUABLE"
    )
    metrics = {
        "case_id": config["id"],
        "overall_status": overall_status,
        "status": overall_status,
        "verification_passed": (passed if evaluation_complete else "not_evaluated"),
        "reason": (
            "; ".join(unevaluable_reasons)
            if unevaluable_reasons
            else "required output artifacts are incomplete"
            if not output_complete
            else "all required metrics were evaluated"
        ),
        "required_variants": required_variant_count,
        "evaluated_variants": evaluated_variant_count,
        "output_completeness_passed": output_complete,
        "evaluation_complete": evaluation_complete,
        "limits": limits,
        "expected_accumulation_pcs": expected_accumulation,
        "variants": results}
    outputs = CASE_DIR / "outputs"
    figures = CASE_DIR / "figures"
    report = CASE_DIR / "report"
    outputs.mkdir(exist_ok=True)
    figures.mkdir(exist_ok=True)
    report.mkdir(exist_ok=True)
    (outputs / "metrics.json").write_text(json.dumps(metrics, indent=2) + "\n")
    flat = {
        "status": metrics["status"],
        "verificationpassed": metrics["verification_passed"],
        "requiredvariants": metrics["required_variants"],
        "evaluatedvariants": metrics["evaluated_variants"],
        "outputcompletenesspassed": metrics["output_completeness_passed"]}
    for name, item in results.items():
        prefix = "transport" if name == "transport_impulse" else "wildland"
        for key, value in item.items():
            if isinstance(value, (str, int, float, bool)):
                flat[prefix + re.sub(r"[^A-Za-z0-9]", "", key)] = value
    lines = [
        rf"\expandafter\def\csname metric@{key}\endcsname{{{latex_value(value)}}}" for key,
        value in flat.items()]
    (report / "metrics_macros.tex").write_text(report_text("\n".join(lines) + "\n"))
    if all(name in profiles for name in (
            "transport_impulse", "wildland_no_delay",
            "transport_impulse_accumulation", "wildland_no_delay_accumulation")):
        write_reference_figure(profiles, figures)
    # fig, axes = plt.subplots(2, 2, figsize=(10, 7.5), constrained_layout=True)
    # for name, color in (("transport_impulse", "#1f77b4"),
    #                     ("wildland_no_delay", "#d62728")):
    #     x, sim, ref, times, leading, expected = profiles[name]
    #     mask = np.isfinite(sim) & (sim > NODATA_FLOOR)
    #     axes[0, 0].plot(x[mask], sim[mask], color=color, marker=".",
    #                     ms=2, label=name.replace("_", " "))
    #     axes[0, 1].plot(times, leading, color=color, marker=".",
    #                     ms=3, label=name.replace("_", " "))
    # axes[0, 0].plot(profiles["transport_impulse"][0],
    #                 profiles["transport_impulse"][2], "k--", label=r"$(x-x_0)/U$")
    # axes[0, 1].plot(profiles["transport_impulse"][3],
    #                 profiles["transport_impulse"][5], "k--", label=r"$x_0+Ut$")
    # ax = axes[1, 0]
    # colors = {"transport_impulse": "#1f77b4", "wildland_no_delay": "#d62728"}
    # offsets = {"transport_impulse": -2.1, "wildland_no_delay": 2.1}
    # ratios = []
    # for name in ("transport_impulse", "wildland_no_delay"):
    #     x, density, reference_density, plateau_mask = profiles[name + "_accumulation"]
    #     display = x <= ACCUMULATION_PLOT_MAX_M
    #     label = name.replace("_", " ")
    #     ax.bar(x[display] + offsets[name], density[display], width=4.0,
    #            color=colors[name], alpha=.72, label=label)
    #     ax.axhline(reference_density, color=colors[name], ls="--", lw=1.5,
    #                label=f"{label} reference ({reference_density:.1f})")
    #     ratios.append(
    #         results[name]["accumulation_plateau_density_pcs_m2"] /
    #         reference_density)
    # ax.axvspan(
    #     PLATEAU_START_M,
    #     PLATEAU_END_M,
    #     color="#ffbf00",
    #     alpha=.2,
    #     label="metric interval")
    # labels = ["transport\nimpulse", "wildland\nno delay"]
    # axes[1, 1].bar(labels, ratios, color=[colors["transport_impulse"],
    #                colors["wildland_no_delay"]], alpha=.75)
    # axes[1, 1].axhspan(0.9, 1.1, color="#2ca02c", alpha=.18,
    #                    label="10% acceptance band")
    # axes[1, 1].axhline(1.0, color="k", ls="--", label="reference ratio")
    # for axis in (axes[0, 0], axes[0, 1], axes[1, 0], axes[1, 1]):
    #     axis.grid(alpha=.25)
    #     axis.legend(fontsize=8)
    # axes[0, 0].set(xlabel="Downwind distance (m)",
    #                ylabel="Time of arrival (s)", ylim=(0, None))
    # axes[0, 1].set(xlabel="Time (s)",
    #                ylabel="Leading-edge position (m)", ylim=(0, None))
    # axes[1,
    #      0].set(xlabel="Downwind distance (m)",
    #             ylabel=r"Accumulated firebrand density (pcs/m$^2$)",
    #             xlim=(0,
    #                   ACCUMULATION_PLOT_MAX_M),
    #             ylim=(0,
    #                   None))
    # axes[1, 1].set(xlabel="Variant",
    #                ylabel="Plateau density / reference", ylim=(0.85, 1.15))
    # fig.savefig(figures / "combined_transport_no_delay_verification.pdf")
    # plt.close(fig)
    print(f"[OK] comprehensive verification status: {metrics['status']}")


if __name__ == "__main__":
    main()
    generate_spatial_evidence(
        CASE_DIR, output_preference=("ember_flux", "time_of_arrival"),
        preferred_variant="transport_impulse", strip_width_m=1.0,
    )
