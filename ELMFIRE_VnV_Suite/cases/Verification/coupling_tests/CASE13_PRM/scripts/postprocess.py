#!/usr/bin/env python3
"""Evaluate the parametric response response maps."""

from report_language import polish_figure, report_text
import numpy as np
from pathlib import Path
import csv
import json
import math

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1 import make_axes_locatable
import rasterio

# Customizable postprocessing parameters.
CASE_FILENAME = "case.json"
DESIGN_FILENAME = "outputs/parameter_design.csv"
MANIFEST_FILENAME = "variants/manifest.json"
RESULTS_FILENAME = "outputs/parametric_results.csv"
FIGURE_FILENAME = "parametric_response_maps.pdf"
INPUT_FIGURE_FILENAME = "input_configuration.pdf"
SEPARATION_FAMILY = "generation_separation"
WIND_FAMILY = "generation_wind"


def read_csv(path):
    """Read a case-local CSV artifact into a list of field-name dictionaries."""
    with path.open(newline="") as stream:
        return list(csv.DictReader(stream))


def read_raster(path):
    """Read one prepared raster and return its values and geotransform."""
    with rasterio.open(path) as dataset:
        return dataset.read(1), dataset.transform.to_gdal()


def add_horizontal_colorbar(figure, axis, image, label, ticks=None):
    """Place a full-width readable colorbar below a short map panel."""
    colorbar = figure.colorbar(
        image, ax=axis, orientation="horizontal", fraction=0.08, pad=0.24,
        ticks=ticks,
    )
    colorbar.set_label(label, fontsize=9)
    colorbar.ax.tick_params(labelsize=8)


def write_input_figure(case_dir, case, manifest):
    """Visualize one representative lattice and its full source column."""
    representative = next(
        item for item in manifest
        if item["family"] == SEPARATION_FAMILY
        and math.isclose(float(item["separation_m"]),
                         float(case["baseline_separation_m"]))
    )
    variant_dir = case_dir / representative["working_directory"]
    input_dir = variant_dir / "data" / "inputs"
    fraction, transform = read_raster(input_dir / "structure_fraction.tif")
    phi, _ = read_raster(input_dir / "new_phi.tif")
    buffer_cells = int(representative["buffer_cells"])
    physical_nx = int(representative["physical_nx"])
    physical_ny = int(representative["physical_ny"])
    row0 = fraction.shape[0] - buffer_cells - physical_ny
    col0 = buffer_cells
    region = np.s_[row0:row0 + physical_ny, col0:col0 + physical_nx]
    fraction = fraction[region]
    source = (phi[region] <= 0.0).astype(float)
    dx = float(transform[1])
    extent = (0.0, physical_nx * dx, 0.0, physical_ny * dx)

    figure, axes = plt.subplots(
        1, 2, figsize=(10.2, 3.6), constrained_layout=True
    )
    figure.set_constrained_layout_pads(
        w_pad=0.06, h_pad=0.04, wspace=0.12, hspace=0.04
    )
    fraction_image = axes[0].imshow(
        fraction, origin="upper", extent=extent, aspect="equal",
        interpolation="nearest", cmap="viridis", vmin=0.0, vmax=1.0,
    )
    source_image = axes[1].imshow(
        source, origin="upper", extent=extent, aspect="equal",
        interpolation="nearest", cmap="Reds", vmin=0.0, vmax=1.0,
    )
    add_horizontal_colorbar(
        figure, axes[0], fraction_image, "Structure fraction (-)"
    )
    add_horizontal_colorbar(
        figure, axes[1], source_image, "Initially ignited cells (-)", ticks=[0, 1]
    )
    axes[0].set_title("Uniform 2-D structure lattice")
    axes[1].set_title("Ignited first structure column")
    for axis in axes:
        axis.set_xlabel("Easting (m)")
        axis.set_ylabel("Northing (m)")
    figure.suptitle(
        f"Representative prepared geometry — {representative['name']}"
    )
    figure_dir = case_dir / "figures"
    figure_dir.mkdir(exist_ok=True)
    polish_figure(figure)
    figure.savefig(
        figure_dir / INPUT_FIGURE_FILENAME,
        bbox_inches="tight", pad_inches=0.06,
    )
    plt.close(figure)


def rankdata(values):
    """Assign average ranks to tied observations for the dependency-free Spearman calculation."""
    values = np.asarray(values, dtype=float)
    order = np.argsort(values)
    ranks = np.empty(len(values), dtype=float)
    index = 0
    while index < len(values):
        end = index + 1
        while end < len(values) and values[order[end]] == values[order[index]]:
            end += 1
        ranks[order[index:end]] = 0.5 * (index + end - 1) + 1.0
        index = end
    return ranks


def spearman(x_values, y_values):
    """Calculate Spearman rank correlation and return none when the sample cannot define it."""
    if len(x_values) < 3:
        return None
    x_rank = rankdata(x_values)
    y_rank = rankdata(y_values)
    if np.std(x_rank) == 0 or np.std(y_rank) == 0:
        return None
    return float(np.corrcoef(x_rank, y_rank)[0, 1])


def metric(name, limit, value, passed=None, status=None, note=""):
    """Create one explicit metric record containing its limit, calculated value, status, and note."""
    if status is None:
        status = "NOT EVALUABLE" if value is None else ("PASS" if passed else "FAIL")
    if value is None:
        shown = "N/A"
    elif isinstance(value, float):
        shown = f"{value:.6g}"
    else:
        shown = str(value)
    return {"metric": name, "limit": limit, "calculated": shown,
            "status": status, "note": note}


def numeric_result(row):
    """Convert one CSV result row from text into typed coordinates, outcome, and ROS values."""
    return {
        "family": row["family"], "generation": float(
            row["generation_rate_pcs_mw_s"]), "separation": float(
            row["separation_m"]), "wind": float(
                row["wind_mph"]), "success": str(
                    row["spread_success"]).strip().lower() in (
                        "1", "true", "yes", "pass"), "ros": float(
                            row["mean_ros_m_s"]), }


def thresholds(rows, family, coordinate, coordinates, generation_max):
    """Find the minimum successful generation rate at every coordinate in one parameter family."""
    values = []
    for value in coordinates:
        subset = [row for row in rows if row["family"] == family
                  and math.isclose(row[coordinate], value)]
        successful = [row["generation"] for row in subset if row["success"]]
        values.append(min(successful) if successful else generation_max + 5.0)
    return values


def calculate_metrics(case, design, results, capability):
    """Evaluate design completeness and all predeclared response-trend acceptance criteria."""
    limits = case["metrics"]
    expected = int(limits["design_point_count"])
    rows = [metric("Parameter-design completeness", "270/270", len(design),
                   len(design) == expected)]
    capability_ok = bool(capability["required_feature_available"] and
                         capability["source_adapter_implemented"])
    rows.append(metric("Exact single-structure capability", "available", None,
                       status="PASS" if capability_ok else "NOT EVALUABLE",
                       note=capability["status"]))
    completeness = len(results) / expected if results else 0.0
    rows.append(metric("Current result completeness", "100%", completeness,
                       completeness >= limits["result_completeness_min"],
                       status=("PASS" if completeness >= 1.0 else "NOT EVALUABLE")))
    if len(results) != expected:
        note = "All 270 current-design outputs are required."
        names = [
            ("Critical GR versus separation Spearman rho", ">= 0.85"),
            ("ROS versus separation median Spearman rho", "<= -0.50"),
            ("ROS versus generation median Spearman rho", ">= 0.70"),
            ("Low/high-wind threshold excess", ">= 5 pcs/MW/s"),
            ("Moderate/edge-wind ROS ratio", ">= 1.10"),
            ("Maximum firebrand-driven ROS", "<= 0.08 m/s"),
        ]
        rows.extend(metric(name, limit, None, status="NOT EVALUABLE", note=note)
                    for name, limit in names)
        return rows

    generations = case["generation_rates_pcs_mw_s"]
    separations = case["separation_distances_m"]
    winds = case["wind_speeds_mph"]
    max_generation = max(generations)
    sep_threshold = thresholds(results, SEPARATION_FAMILY, "separation",
                               separations, max_generation)
    sep_rho = spearman(separations, sep_threshold)

    sep_ros_rhos = []
    generation_rhos = []
    for generation in generations:
        subset = sorted((row for row in results
                         if row["family"] == SEPARATION_FAMILY
                         and math.isclose(row["generation"], generation)),
                        key=lambda row: row["separation"])
        rho = spearman([row["separation"] for row in subset],
                       [row["ros"] for row in subset])
        if rho is not None:
            sep_ros_rhos.append(rho)
    for family in (SEPARATION_FAMILY, WIND_FAMILY):
        coordinate = "separation" if family == SEPARATION_FAMILY else "wind"
        coordinate_values = separations if family == SEPARATION_FAMILY else winds
        for coordinate_value in coordinate_values:
            subset = sorted((row for row in results if row["family"] == family
                             and math.isclose(row[coordinate], coordinate_value)),
                            key=lambda row: row["generation"])
            rho = spearman([row["generation"] for row in subset],
                           [row["ros"] for row in subset])
            if rho is not None:
                generation_rhos.append(rho)
    sep_ros_rho = float(np.median(sep_ros_rhos)) if sep_ros_rhos else None
    generation_rho = float(np.median(generation_rhos)) if generation_rhos else None

    wind_threshold = thresholds(results, WIND_FAMILY, "wind", winds, max_generation)
    moderate_low, moderate_high = limits["moderate_wind_range_mph"]
    moderate_thresholds = [threshold for wind, threshold in zip(winds, wind_threshold)
                           if moderate_low <= wind <= moderate_high]
    edge_thresholds = [threshold for wind, threshold in zip(winds, wind_threshold)
                       if wind < moderate_low or wind > moderate_high]
    edge_excess = float(np.mean(edge_thresholds) - np.median(moderate_thresholds))
    moderate_ros = [row["ros"] for row in results if row["family"] == WIND_FAMILY
                    and moderate_low <= row["wind"] <= moderate_high]
    edge_ros = [row["ros"] for row in results if row["family"] == WIND_FAMILY
                and (row["wind"] < moderate_low or row["wind"] > moderate_high)]
    edge_mean = float(np.mean(edge_ros)) if edge_ros else 0.0
    wind_ratio = float(np.mean(moderate_ros) / edge_mean) if edge_mean > 0 else None
    max_ros = max(row["ros"] for row in results)

    rows.extend([
        metric("Critical GR versus separation Spearman rho", ">= 0.85", sep_rho,
               sep_rho is not None and sep_rho >= limits["separation_threshold_spearman_min"]),
        metric("ROS versus separation median Spearman rho", "<= -0.50", sep_ros_rho,
               sep_ros_rho is not None and sep_ros_rho <= limits["separation_ros_spearman_max"]),
        metric("ROS versus generation median Spearman rho", ">= 0.70", generation_rho,
               generation_rho is not None and generation_rho >= limits["generation_ros_spearman_min"]),
        metric("Low/high-wind threshold excess", ">= 5 pcs/MW/s", edge_excess,
               edge_excess >= limits["wind_edge_threshold_excess_min_pcs_mw_s"]),
        metric("Moderate/edge-wind ROS ratio", ">= 1.10", wind_ratio,
               wind_ratio is not None and wind_ratio >= limits["moderate_to_edge_ros_ratio_min"]),
        metric("Maximum firebrand-driven ROS", "<= 0.08 m/s", max_ros,
               max_ros <= limits["ros_upper_bound_m_s"]),
    ])
    return rows


def write_figure(case_dir, case, results):
    """Create the case vector-PDF figure from actual outputs and the declared reference solution."""
    figure_dir = case_dir / "figures"
    figure_dir.mkdir(exist_ok=True)
    fig, axes = plt.subplots(2, 2, figsize=(10.2, 7.2), constrained_layout=True)
    generations = np.array(case["generation_rates_pcs_mw_s"])
    separations = np.array(case["separation_distances_m"])
    winds = np.array(case["wind_speeds_mph"])
    if results:
        for family, x_name, x_values, column in (
                (SEPARATION_FAMILY, "separation", separations, 0),
                (WIND_FAMILY, "wind", winds, 1)):
            subset = [row for row in results if row["family"] == family]
            for success, marker, color, label in ((False, "o", "tab:blue", "failure"),
                                                  (True, "x", "tab:red", "success")):
                points = [row for row in subset if row["success"] == success]
                axes[0, column].scatter([row[x_name] for row in points],
                                        [row["generation"] for row in points],
                                        marker=marker, color=color, label=label)
            for generation in generations:
                curve = sorted((row for row in subset
                                if math.isclose(row["generation"], generation)),
                               key=lambda row: row[x_name])
                axes[1, column].plot([row[x_name] for row in curve],
                                     [row["ros"] for row in curve], lw=1.0,
                                     label=f"{generation:g}")
    else:
        axes[0,
             0].fill_between(separations,
                             0,
                             2.5 + 0.043 * separations ** 2,
                             color="tab:blue",
                             alpha=0.15,
                             label="expected failure region")
        axes[0, 0].plot(separations, 2.5 + 0.043 * separations ** 2,
                        "k--", label="expected rising threshold")
        wind_guide = 10.0 + 22.0 * ((winds - 75.0) / 75.0) ** 4
        axes[0, 1].fill_between(winds, 0, wind_guide, color="tab:blue", alpha=0.15)
        axes[0, 1].plot(winds, wind_guide, "k--", label="expected U-shaped threshold")
        axes[1,
             0].annotate("Expected: ROS decreases with separation\nand increases with generation rate",
                         (0.5,
                          0.5),
                         xycoords="axes fraction",
                         ha="center",
                         va="center")
        axes[1,
             1].annotate("Expected: moderate winds promote spread;\nvery low and high winds suppress it",
                         (0.5,
                          0.5),
                         xycoords="axes fraction",
                         ha="center",
                         va="center")
        for ax in axes.flat:
            ax.text(0.02, 0.96, "Expected behavior only; exact model not runnable",
                    transform=ax.transAxes, va="top", fontsize=8, color="0.35")
    axes[0, 0].set(xlabel="Structure separation d (m)", ylabel="GR (pcs/MW/s)",
                   xlim=(0, 32), ylim=(0, 52), title="Ignition map: separation sweep")
    axes[0, 1].set(xlabel="Wind speed (mph)", ylabel="GR (pcs/MW/s)",
                   xlim=(5, 155), ylim=(0, 52), title="Ignition map: wind sweep")
    axes[1, 0].set(xlabel="Structure separation d (m)", ylabel="Mean ROS (m/s)",
                   xlim=(0, 32), ylim=(0, 0.075), title="Spread-rate response")
    axes[1, 1].set(xlabel="Wind speed (mph)", ylabel="Mean ROS (m/s)",
                   xlim=(5, 155), ylim=(0, 0.075), title="Spread-rate response")
    for ax in axes.flat:
        ax.grid(alpha=0.2)
        handles, labels = ax.get_legend_handles_labels()
        if handles and len(handles) <= 12:
            ax.legend(fontsize=7, loc="best")
    polish_figure(fig)
    fig.savefig(figure_dir / FIGURE_FILENAME, bbox_inches="tight")
    plt.close(fig)


def write_macros(case_dir, status, rows, completed, requested):
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
    lines = [f"\\def\\OverallStatus{{{status}}}",
             f"\\def\\CompletedVariantCount{{{completed}}}",
             f"\\def\\RequestedVariantCount{{{requested}}}", "\\def\\MetricRows{"]
    for row in rows:
        lines.append("{} & {} & {} & {} \\\\".format(tex(row["metric"]), tex(
            row["limit"]), tex(row["calculated"]), tex(row["status"])))
    lines.append("}")
    (case_dir / "report" / "metrics_macros.tex").write_text(report_text("\n".join(lines) + "\n"))


def main():
    """Run postprocessing from case inputs through final generated artifacts."""
    case_dir = Path(__file__).resolve().parents[1]
    case = json.loads((case_dir / CASE_FILENAME).read_text())
    design = read_csv(case_dir / DESIGN_FILENAME)
    manifest = json.loads((case_dir / MANIFEST_FILENAME).read_text())
    capability = json.loads((case_dir / "outputs" / "capability.json").read_text())
    results_path = case_dir / RESULTS_FILENAME
    results = [numeric_result(row) for row in read_csv(
        results_path)] if results_path.exists() else []
    rows = calculate_metrics(case, design, results, capability)
    fully_evaluated = len(results) == case["metrics"]["design_point_count"]
    if not fully_evaluated:
        status = "NOT EVALUABLE"
    else:
        status = "PASS" if all(row["status"] == "PASS" for row in rows) else "FAIL"
    payload = {
        "case_id": case["id"], "overall_status": status,
        "completed_result_points": len(results), "requested_result_points": len(design),
        "capability": capability, "metrics": rows,
    }
    (case_dir / "outputs" / "metrics.json").write_text(json.dumps(payload, indent=2) + "\n")
    write_figure(case_dir, case, results)
    write_input_figure(case_dir, case, manifest)
    write_macros(case_dir, status, rows, len(results), len(design))
    print(
        f"[OK] postprocessed {len(results)}/{len(design)} design points; status: {status}")


if __name__ == "__main__":
    main()
