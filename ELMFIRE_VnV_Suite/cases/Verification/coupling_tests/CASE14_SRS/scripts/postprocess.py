#!/usr/bin/env python3
"""Postprocess the two-dimensional spatial-resolution sweep."""

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

# Customizable postprocessing parameters.
CASE_FILENAME = "case.json"
MANIFEST_FILENAME = "variants/manifest.json"
FIGURE_FILENAME = "spatial_resolution_sweep.pdf"
GEOMETRY_WINDOW_M = 100.0
MIN_STRUCTURES_FOR_ROS = 3


def read_raster(path):
    """Read one GDAL raster into a floating-point array while preserving nodata handling at the caller."""
    with rasterio.open(path) as dataset:
        return dataset.read(1)


def unique_time_control(config, key):
    """Parse one finite positive time-control scalar from a generated namelist."""
    matches = re.findall(
        rf"(?mi)^\s*{re.escape(key)}\s*=\s*([0-9.eEdD+-]+)", config
    )
    if len(matches) != 1:
        raise ValueError(f"{key} is not unique")
    value = float(matches[0].replace("D", "E").replace("d", "e"))
    if not math.isfinite(value) or value <= 0.0:
        raise ValueError(f"{key} is not finite and positive")
    return value


def current_outputs(case_dir, item, expected_no_propagation=False):
    """Select one terminal TOA through the strict regular or stalled path."""
    variant = case_dir / item["working_directory"]
    evidence = {
        "selection_path": None,
        "reason": None,
        "final_time_s": None,
        "pre_jump_time_s": None,
        "timestep_grid_residual_s": None,
        "expected_no_propagation": bool(expected_no_propagation),
    }
    marker = variant / "logs" / "completed_input_fingerprint.txt"
    stdout = variant / "logs" / "elmfire.stdout"
    if not marker.exists() or marker.read_text().strip() != item["input_fingerprint"]:
        evidence["reason"] = "missing or mismatched input fingerprint"
        return None, evidence
    if not stdout.exists() or "End of simulation reached successfully" not in stdout.read_text(errors="ignore"):
        evidence["reason"] = "successful termination record is absent"
        return None, evidence
    try:
        config_path = variant / Path(item.get("config", "elmfire.data.in")).name
        config = config_path.read_text(encoding="utf-8")
        tstop = unique_time_control(config, "SIMULATION_TSTOP")
        timestep = unique_time_control(config, "SIMULATION_DT")
        met_step = unique_time_control(config, "DT_METEOROLOGY")
        control_tolerance = max(1.0e-6, 1.0e-9 * tstop)
        if not math.isclose(
            tstop, float(item["simulation_tstop_s"]), rel_tol=0.0,
            abs_tol=control_tolerance
        ):
            raise ValueError("manifest and namelist stop times disagree")
        if not math.isclose(
            timestep, float(item["simulation_dt_s"]), rel_tol=0.0,
            abs_tol=max(1.0e-12, 1.0e-9 * timestep)
        ):
            raise ValueError("manifest and namelist timesteps disagree")
        if abs(tstop - round(tstop / timestep) * timestep) > control_tolerance:
            raise ValueError("configured stop is not an exact timestep multiple")
        ledgers = sorted((variant / "outputs").glob("dump_times_*.csv"))
        if len(ledgers) != 1:
            raise ValueError(f"expected one dump-times CSV, found {len(ledgers)}")
        with ledgers[0].open(newline="", encoding="utf-8") as stream:
            rows = list(csv.DictReader(stream))
        final_rows = [
            row for row in rows
            if str(row.get("is_final_dump", "")).strip().upper()
            in {"T", "TRUE", "1", "Y", "YES"}
        ]
        if len(final_rows) != 1:
            raise ValueError(f"expected one final dump record, found {len(final_rows)}")
        final_time = float(final_rows[0]["time_seconds"])
        evidence["final_time_s"] = final_time
        tolerance = max(1.0e-3, 1.0e-6 * tstop)
        regular = math.isclose(final_time, tstop, rel_tol=0.0, abs_tol=tolerance)
        pre_jump = final_time - met_step
        grid_residual = abs(pre_jump - round(pre_jump / timestep) * timestep)
        if final_time > tstop:
            evidence["pre_jump_time_s"] = pre_jump
            evidence["timestep_grid_residual_s"] = grid_residual
        stalled = (
            final_time > tstop
            and math.isfinite(pre_jump)
            and -tolerance <= pre_jump <= tstop + tolerance
            and grid_residual <= tolerance
            and (
                tstop - pre_jump <= timestep + tolerance
                or expected_no_propagation
            )
        )
        if not (regular or stalled):
            raise ValueError(
                "terminal record is neither a regular final dump nor a "
                "compatible timestep-aligned stalled final dump"
            )
        stamp = int(math.floor(final_time + 0.5))
        if stalled:
            matches = sorted((variant / "outputs").glob("time_of_arrival*_*.tif"))
            evidence["selection_path"] = "stalled-final compatibility"
        else:
            matches = sorted(
                (variant / "outputs").glob(f"time_of_arrival*_{stamp:07d}.tif")
            )
            evidence["selection_path"] = "regular terminal dump"
        matches = [path for path in matches if "_transient_" not in path.name]
        if len(matches) != 1:
            raise ValueError(f"expected one terminal TOA raster, found {len(matches)}")
        evidence["reason"] = "accepted"
        return matches[0], evidence
    except (OSError, KeyError, TypeError, ValueError, csv.Error) as exc:
        evidence["reason"] = str(exc)
        return None, evidence


def mean_ros(case_dir, case, item, toa_path):
    """Calculate structure-scale mean ROS from grouped arrival times in the physical domain."""
    toa = read_raster(toa_path).astype(float)
    sid = read_raster(
        case_dir /
        item["working_directory"] /
        "data" /
        "inputs" /
        "structure_id.tif")
    period = float(case["structure_size_m"]) + float(case["structure_separation_m"])
    n_struct_x = int(math.ceil(float(case["domain_length_m"]) / period))
    points = []
    for sx in range(1, n_struct_x):
        ids = np.where(sid > 0, (sid - 1) % n_struct_x, -1)
        values = toa[(ids == sx) & np.isfinite(toa) & (toa > 0)]
        if values.size:
            points.append((float(np.median(values)),
                           sx * period + 0.5 * case["structure_size_m"]))
    if len(points) < MIN_STRUCTURES_FOR_ROS:
        return None, points
    points.sort()
    time = np.array([point[0] for point in points])
    distance = np.array([point[1] for point in points])
    slope = float(np.polyfit(time, distance, 1)[0])
    return max(0.0, slope), points


def metric(name, limit, value, passed=None, note="", status=None):
    """Build one explicit metric row, including unevaluated run states."""
    if status is None:
        if value is None:
            status = "NOT RUN"
        else:
            status = "PASS" if (passed is None or passed) else "FAIL"
    shown = "N/A" if value is None else (
        f"{value:.6g}" if isinstance(
            value, float) else str(value))
    return {"metric": name, "limit": limit, "calculated": shown,
            "status": status, "note": note}


def geometry_metrics(case, manifest):
    """Evaluate all preprocessing geometry checks against the declared acceptance limits."""
    limits = case["metrics"]
    by_dx = {float(item["dx_m"]): item for item in manifest}
    rows = []
    fraction = len(manifest) / len(case["dx_values_m"])
    rows.append(metric("Prepared resolution variants", ">= 100%", fraction,
                       fraction >= limits["prepared_variant_fraction_min"]))
    dx10 = by_dx[10.0]
    footprint_error = max(abs(dx10["component_area_min_m2"] - 100.0),
                          abs(dx10["component_area_max_m2"] - 100.0)) / 100.0
    gap_error = max(abs(dx10["gap_width_min_m"] - 10.0),
                    abs(dx10["gap_width_max_m"] - 10.0)) / 10.0
    rows.append(metric("dx=10 m footprint error", "<= 0.01", footprint_error,
                       footprint_error <= limits["dx10_footprint_relative_error_max"]))
    rows.append(metric("dx=10 m separation error", "<= 0.01", gap_error,
                       gap_error <= limits["dx10_gap_relative_error_max"]))
    dx7 = by_dx[7.0]
    dx7_ok = (
        dx7['component_area_min_m2'] >= limits["dx7_component_area_min_m2"] -
        1e-6 and dx7['component_area_max_m2'] <= limits["dx7_component_area_max_m2"] +
        1e-6)
    rows.append(
        metric(
            "dx=7 m component-area range",
            "49 to 196 m2",
            f"{dx7['component_area_min_m2']:.0f} to {dx7['component_area_max_m2']:.0f}",
            dx7_ok))
    dx30 = by_dx[30.0]
    error30 = abs(dx30["component_area_median_m2"] - limits["dx30_component_area_reference_m2"]
                  ) / limits["dx30_component_area_reference_m2"]
    rows.append(metric("dx=30 m median footprint error", "<= 0.01", error30,
                       error30 <= limits["dx30_component_area_relative_error_max"]))
    return rows


def write_figure(case_dir, case, manifest, ros_by_dx, terminal_evidence_by_dx):
    """Create the geometry/ROS figure using the 10-m run as its anchor.

    The 10-m grid exactly represents both the 10-m structure and 10-m gap, so
    its measured ELMFIRE ROS is the comparison scale. Deriving the dashed line
    from that run prevents a rounded external estimate from being presented as
    though it were a distinct simulation result.
    """
    representatives = case["representative_geometry_dx_m"]
    by_dx = {float(item["dx_m"]): item for item in manifest}
    fig = plt.figure(figsize=(10.5, 6.8))
    grid = fig.add_gridspec(2, 3, height_ratios=[1.0, 1.15], hspace=0.38, wspace=0.24)
    for column, dx in enumerate(representatives):
        item = by_dx[float(dx)]
        path = case_dir / item["working_directory"] / \
            "data" / "inputs" / "new_fbfm40.tif"
        array = read_raster(path)
        row0 = array.shape[0] - 2 - item["physical_ny"]
        physical = array[row0:row0 + item["physical_ny"], 2:2 + item["physical_nx"]]
        count = max(1, int(math.ceil(GEOMETRY_WINDOW_M / dx)))
        window = physical[-count:, :count]
        ax = fig.add_subplot(grid[0, column])
        ax.imshow(window == 91, origin="lower", cmap="copper", interpolation="nearest",
                  extent=(0, count * dx, 0, count * dx), vmin=0, vmax=1)
        ax.set_title(rf"$\Delta x=\Delta y={dx:g}$ m")
        ax.set_xlabel("Downwind x (m)")
        if column == 0:
            ax.set_ylabel("Crosswind y (m)")
    ax = fig.add_subplot(grid[1, :])
    dx_values = np.array(sorted(float(value) for value in case["dx_values_m"]))
    plotted_ros = np.array([
        ros_by_dx.get(dx) if ros_by_dx.get(dx) is not None else np.nan
        for dx in dx_values
    ])
    if np.any(np.isfinite(plotted_ros)):
        # NaN values break the line, avoiding visual interpolation across
        # resolutions where propagation was insufficient for a ROS fit.
        ax.plot(dx_values, plotted_ros, "o-", lw=1.5,
                label="ELMFIRE current run")
    else:
        ax.text(0.5, 0.55, "ELMFIRE sweep not run", transform=ax.transAxes,
                ha="center", va="center", fontsize=13)
    no_fit = [
        dx for dx in dx_values
        if ros_by_dx.get(dx) is None
        and terminal_evidence_by_dx.get(dx, {}).get("reason") == "accepted"
    ]
    rejected = [
        dx for dx in dx_values
        if terminal_evidence_by_dx.get(dx, {}).get("reason") != "accepted"
    ]
    if no_fit:
        ax.scatter(no_fit, np.zeros(len(no_fit)), marker="x", color="#c00000",
                   label="Accepted output: fewer than 3 ignited columns")
    if rejected:
        ax.scatter(rejected, np.zeros(len(rejected)), marker="^", color="#e68600",
                   label="Terminal evidence rejected")
    reference_ros = ros_by_dx.get(10.0)
    if reference_ros is not None:
        ax.axhline(reference_ros, color="k", ls="--",
                   label=rf"10 m ELMFIRE anchor: {reference_ros:.5f} m/s")
    ax.set_xlabel(r"Grid spacing $\Delta x=\Delta y$ (m)")
    ax.set_ylabel("Mean community ROS (m/s)")
    ax.set_xlim(0, max(dx_values) + 2)
    ax.grid(alpha=0.25)
    ax.legend(loc="best")
    fig.suptitle("Two-dimensional spatial-resolution verification", y=0.99)
    figure_dir = case_dir / "figures"
    figure_dir.mkdir(exist_ok=True)
    polish_figure(fig)
    fig.savefig(figure_dir / FIGURE_FILENAME, bbox_inches="tight")
    plt.close(fig)


def write_macros(case_dir, status, rows, completed, requested, dx10_reference=None):
    """Serialize calculated values and statuses into report-local LaTeX macros."""
    def tex(value):
        """Escape special comparison and percent symbols used in generated LaTeX table rows."""
        return str(value).replace(
            "%",
            r"\%").replace(
            ">=",
            r"$\geq$").replace(
            "<=",
            r"$\leq$")
    calculated = {row["metric"]: row["calculated"] for row in rows}
    lines = [f"\\def\\OverallStatus{{{status}}}",
             f"\\def\\CompletedVariantCount{{{completed}}}",
             f"\\def\\RequestedVariantCount{{{requested}}}",
             f"\\def\\DxTenReferenceROS{{{dx10_reference:.6g}}}" if dx10_reference is not None else "\\def\\DxTenReferenceROS{N/A}",
             f"\\def\\ROSEvaluableFraction{{{calculated.get('ROS-evaluable resolution fraction', 'N/A')}}}",
             f"\\def\\ROSMeshCV{{{calculated.get('ROS mesh-dependence CV', 'N/A')}}}",
             f"\\def\\ROSMaximumDeviation{{{calculated.get('Maximum ROS deviation from 10-m anchor', 'N/A')}}}",
             "\\def\\MetricRows{"]
    for row in rows:
        lines.append("{} & {} & {} & {} \\\\".format(tex(row["metric"]), tex(
            row["limit"]), tex(row["calculated"]), tex(row["status"])))
    lines.append("}")
    (case_dir / "report" / "metrics_macros.tex").write_text(report_text("\n".join(lines) + "\n"))


def main():
    """Run postprocessing from case inputs through final generated artifacts."""
    case_dir = Path(__file__).resolve().parents[1]
    case = json.loads((case_dir / CASE_FILENAME).read_text())
    manifest = json.loads((case_dir / MANIFEST_FILENAME).read_text())
    rows = geometry_metrics(case, manifest)
    ros_by_dx = {}
    toa_file_by_dx = {}
    ignited_columns_by_dx = {}
    terminal_evidence_by_dx = {}
    completed = []
    expected_no_propagation = {
        float(value) for value in case.get("reference_no_propagation_dx_m", [])
    }
    for item in manifest:
        dx = float(item["dx_m"])
        toa, terminal_evidence = current_outputs(
            case_dir, item, dx in expected_no_propagation
        )
        value, points = mean_ros(case_dir, case, item, toa) if toa else (None, [])
        ros_by_dx[dx] = value
        toa_file_by_dx[dx] = toa.name if toa else None
        ignited_columns_by_dx[dx] = len(points)
        terminal_evidence_by_dx[dx] = terminal_evidence
        if toa:
            completed.append(item["name"])
    executed = [
        item["name"] for item in manifest
        if terminal_evidence_by_dx[float(item["dx_m"])]["final_time_s"] is not None
    ]
    rejected = [item["name"] for item in manifest if item["name"] not in completed]
    completion_note = (
        "" if len(completed) == len(manifest)
        else "Every process may have exited, but strict terminal evidence was not accepted for every variant."
    )
    rows.append(metric("Current-fingerprint run completeness", f"{len(manifest)} variants",
                       f"{len(completed)}/{len(manifest)}", len(completed) == len(manifest),
                       completion_note,
                       status=("PASS" if len(completed) == len(manifest) else
                               "NOT RUN" if not completed else "NOT EVALUABLE")))
    if len(completed) == len(manifest):
        value10 = ros_by_dx.get(10.0)
        rows.append(metric(
            "10-m ELMFIRE ROS anchor",
            "finite fitted ROS",
            value10,
            value10 is not None and np.isfinite(value10),
            "The plotted reference line is this same run-derived value."))

        finite_pairs = [(dx, value) for dx, value in ros_by_dx.items()
                        if value is not None and np.isfinite(value)]
        evaluable_fraction = len(finite_pairs) / len(manifest)
        rows.append(metric(
            "ROS-evaluable resolution fraction",
            ">= 100%",
            evaluable_fraction,
            evaluable_fraction >= case["metrics"]["ros_evaluable_fraction_min"],
            "A missing fit means fewer than three downstream structure columns ignited."))

        finite = np.array([value for _, value in finite_pairs])
        cv = float(np.std(finite) / np.mean(finite)
                   ) if finite.size > 1 and np.mean(finite) > 0 else None
        rows.append(metric(
            "ROS mesh-dependence CV",
            "<= 0.25",
            cv,
            cv is not None and cv <=
            case["metrics"]["ros_mesh_independence_cv_max"]))

        deviations = [abs(value - value10) / value10
                      for _, value in finite_pairs
                      if value10 is not None and value10 > 0.0]
        maximum_deviation = max(deviations) if deviations else None
        rows.append(metric(
            "Maximum ROS deviation from 10-m anchor",
            "<= 0.50",
            maximum_deviation,
            maximum_deviation is not None and maximum_deviation <=
            case["metrics"]["ros_relative_deviation_from_dx10_max"]))
    else:
        rows.append(metric("10-m ELMFIRE ROS anchor", "finite fitted ROS", None,
                           note="Requires the complete ELMFIRE sweep."))
        rows.append(metric("ROS-evaluable resolution fraction", ">= 100%", None,
                           note="Requires the complete ELMFIRE sweep."))
        rows.append(metric("ROS mesh-dependence CV", "<= 0.25", None,
                           note="Requires the complete ELMFIRE sweep."))
        rows.append(metric("Maximum ROS deviation from 10-m anchor", "<= 0.50", None,
                           note="Requires the complete ELMFIRE sweep."))
    runtime_rows = [row for row in rows if row["metric"].startswith(
        "Current-") or "ROS" in row["metric"]]
    if not completed:
        status = "NOT RUN"
    elif len(completed) < len(manifest):
        status = "NOT EVALUABLE"
    else:
        status = "PASS" if all(row["status"] == "PASS" for row in rows) else "FAIL"
    payload = {
        "case_id": case["id"],
        "overall_status": status, "completed_variants": completed,
        "executed_variants": executed,
        "terminal_evidence_rejected_variants": rejected,
        "unrun_variants": [item["name"] for item in manifest if item["name"] not in executed],
        "metrics": rows,
        "geometry_status": "PASS" if all(row["status"] == "PASS" for row in rows[:5]) else "FAIL",
        "runtime_metric_count": len(runtime_rows),
        "dx10_reference_ros_m_s": ros_by_dx.get(10.0),
        "dx10_reference_definition": (
            "Run-derived mean community ROS from the current dx=10 m ELMFIRE output."
        ),
        "selected_toa_file_by_dx": {
            str(key): value for key, value in sorted(toa_file_by_dx.items())
        },
        "terminal_evidence_by_dx": {
            str(key): value for key, value in sorted(terminal_evidence_by_dx.items())
        },
        "ignited_structure_columns_by_dx": {
            str(key): value for key, value in sorted(ignited_columns_by_dx.items())
        },
        "ros_m_s_by_dx": {str(key): value for key, value in sorted(ros_by_dx.items())},
    }
    (case_dir / "outputs" / "metrics.json").write_text(json.dumps(payload, indent=2) + "\n")
    write_figure(case_dir, case, manifest, ros_by_dx, terminal_evidence_by_dx)
    write_macros(case_dir, status, rows, len(completed), len(manifest), ros_by_dx.get(10.0))
    print(
        f"[OK] postprocessed {len(completed)}/{len(manifest)} variants; status: {status}")


if __name__ == "__main__":
    main()
    generate_spatial_evidence(
        Path(__file__).resolve().parents[1],
        output_preference=("time_of_arrival", "ember_ignition"),
        preferred_variant="dx10",
    )
