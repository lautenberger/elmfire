#!/usr/bin/env python3
"""Measure leading-edge transport speed from every transient phi raster."""
from __future__ import annotations

from report_language import polish_figure, report_text

import csv
import json
import math
import re
from pathlib import Path

import matplotlib
import numpy as np
import rasterio
from spatial_evidence import generate_spatial_evidence

matplotlib.use("Agg")
import matplotlib.pyplot as plt

CASE_DIR = Path(__file__).resolve().parents[1]
VARIANTS_DIR = CASE_DIR / "variants"
OUTPUT_DIR = CASE_DIR / "outputs"
FIGURE_DIR = CASE_DIR / "figures"
REPORT_DIR = CASE_DIR / "report"
WIND_SPEED_MPS = 6.71
FIT_START_M = 300.0
FIT_END_M = 1500.0
MAX_RELATIVE_ERROR = 0.10
ACCEPTANCE_CFL_MAX = 0.8
MARKERS = {5.0: "o", 10.0: "*", 20.0: "s", 30.0: "^"}
PHI_FILENAME_PATTERN = re.compile(r"^phi_\d{7}_d(?P<dump_index>\d{7})\.tif$")
REPRESENTATIVE_VARIANT = "dx10_cfl0p5"


def phi_rasters_by_dump(directory: Path) -> dict[int, Path]:
    """Index level-set rasters by dump number so transient fronts can be matched to output times."""
    rasters: dict[int, Path] = {}
    for path in sorted(directory.glob("phi_*_d*.tif")):
        match = PHI_FILENAME_PATTERN.fullmatch(path.name)
        if match is None:
            continue
        dump_index = int(match.group("dump_index"))
        if dump_index in rasters:
            raise ValueError(f"Duplicate phi raster for dump {dump_index}: {path}")
        rasters[dump_index] = path
    return rasters


def read_dump_times(directory: Path) -> tuple[Path, dict[int, float]]:
    """Read the ELMFIRE dump-index to physical-time mapping from the latest CSV output."""
    paths = sorted(directory.glob("dump_times_*.csv"))
    if len(paths) != 1:
        raise ValueError(
            f"Expected one dump_times CSV in {directory}; found {len(paths)}"
        )
    times: dict[int, float] = {}
    with paths[0].open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream)
        expected_fields = {"dump_index", "time_seconds", "is_final_dump"}
        if reader.fieldnames is None or not expected_fields.issubset(reader.fieldnames):
            raise ValueError(f"{paths[0]} does not contain {sorted(expected_fields)}")
        for row in reader:
            dump_index = int(row["dump_index"])
            if dump_index in times:
                raise ValueError(
                    f"Duplicate timestamp for dump {dump_index} in {paths[0]}")
            times[dump_index] = float(row["time_seconds"])
    if not times:
        raise ValueError(f"No dump timestamps in {paths[0]}")
    return paths[0], times


def leading_zero_crossing(x: np.ndarray, phi: np.ndarray) -> float:
    """Return the most downwind, linearly interpolated phi=0 location."""
    finite = np.isfinite(x) & np.isfinite(phi)
    exact = x[finite & (phi == 0.0)]
    crossings = [float(value) for value in exact]
    for index in range(phi.size - 1):
        if not (finite[index] and finite[index + 1]):
            continue
        left = float(phi[index])
        right = float(phi[index + 1])
        if left * right < 0.0:
            fraction = -left / (right - left)
            crossings.append(float(x[index] + fraction * (x[index + 1] - x[index])))
    return max(crossings) if crossings else float("nan")


def read_phi_centerline(path: Path, variant: dict) -> tuple[np.ndarray, np.ndarray]:
    """Read the physical centerline and exclude the two-cell numerical buffer."""
    with rasterio.open(path) as dataset:
        values = dataset.read(1).astype(float)
        nodata = dataset.nodata
        transform = dataset.transform.to_gdal()
    expected = (int(variant["ny"]), int(variant["nx"]))
    if values.shape != expected:
        raise ValueError(f"{path} shape {values.shape}; expected {expected}")
    dx = float(variant["dx_m"])
    if not math.isclose(abs(transform[1]), dx, abs_tol=1.0e-6):
        raise ValueError(f"{path} cell size does not match manifest dx={dx:g}")
    if not math.isclose(transform[2], 0.0, abs_tol=1.0e-12) or not math.isclose(
        transform[4], 0.0, abs_tol=1.0e-12
    ):
        raise ValueError(
            f"{path} has a rotated grid; centerline extraction is undefined")
    if nodata is not None:
        values[values == nodata] = np.nan
    buffer_cells = int(variant["buffer_cells"])
    columns = np.arange(buffer_cells, values.shape[1] - buffer_cells)
    row = int(variant["ignition_row"])
    if not buffer_cells <= row < values.shape[0] - buffer_cells:
        raise ValueError(f"Manifest ignition row {row} is outside the physical domain")
    x = transform[0] + (columns + 0.5) * transform[1]
    return x, values[row, columns]


def read_phi_front(path: Path, variant: dict) -> float:
    """Extract one zero-level-set front from the physical domain of a PHI raster."""
    x, phi = read_phi_centerline(path, variant)
    return leading_zero_crossing(x, phi)


def read_front_evolution(output_dir: Path, variant: dict) -> tuple[list[dict], Path]:
    """Assemble time-ordered fire-front positions from matching PHI rasters and dump times."""
    phi_rasters = phi_rasters_by_dump(output_dir)
    if not phi_rasters:
        raise FileNotFoundError(f"No transient phi rasters in {output_dir}")
    timestamp_path, dump_times = read_dump_times(output_dir)
    missing_phi = sorted(set(dump_times) - set(phi_rasters))
    missing_times = sorted(set(phi_rasters) - set(dump_times))
    if missing_phi or missing_times:
        raise ValueError(
            "Phi/timestamp mismatch: "
            f"missing phi dumps={missing_phi[:10]}, missing timestamps={missing_times[:10]}")
    dump_indices = sorted(dump_times)
    expected_indices = list(range(1, dump_indices[-1] + 1))
    if dump_indices != expected_indices:
        missing_indices = sorted(set(expected_indices) - set(dump_indices))
        raise ValueError(f"Nonconsecutive dump indices; missing {missing_indices[:10]}")
    ordered_times = np.asarray([dump_times[index]
                               for index in dump_indices], dtype=float)
    if np.any(np.diff(ordered_times) <= 0.0):
        raise ValueError("Dump times are not strictly increasing")
    evolution = []
    for dump_index in dump_indices:
        path = phi_rasters[dump_index]
        evolution.append({
            "dump_index": dump_index,
            "time_seconds": dump_times[dump_index],
            "phi_raster": path,
            "front_x_m": read_phi_front(path, variant),
        })
    return evolution, timestamp_path


def measure_ros(evolution: list[dict]) -> dict:
    """Fit the declared analysis interval to obtain a mean leading-edge rate of spread."""
    selected = [
        item for item in evolution
        if np.isfinite(item["front_x_m"])
        and FIT_START_M <= item["front_x_m"] <= FIT_END_M
    ]
    if len(selected) < 10:
        raise ValueError("Fewer than 10 phi=0 front observations in the fit interval")
    interval_pairs = [
        (left, right)
        for left, right in zip(evolution[:-1], evolution[1:])
        if np.isfinite(left["front_x_m"])
        and np.isfinite(right["front_x_m"])
        and FIT_START_M <= left["front_x_m"] <= FIT_END_M
        and FIT_START_M <= right["front_x_m"] <= FIT_END_M
    ]
    if len(interval_pairs) < 9:
        raise ValueError(
            "Fewer than 9 consecutive phi=0 ROS intervals in the fit interval")
    times = np.asarray([item["time_seconds"] for item in selected], dtype=float)
    positions = np.asarray([item["front_x_m"] for item in selected], dtype=float)
    dt = np.asarray([
        right["time_seconds"] - left["time_seconds"]
        for left, right in interval_pairs
    ], dtype=float)
    if np.any(dt <= 0.0):
        raise ValueError("Phi dump times are not strictly increasing")
    stepwise_ros = np.asarray([
        (right["front_x_m"] - left["front_x_m"])
        / (right["time_seconds"] - left["time_seconds"])
        for left, right in interval_pairs
    ], dtype=float)
    time_weighted_mean = float(np.average(stepwise_ros, weights=dt))
    unweighted_mean = float(np.mean(stepwise_ros))
    slope, intercept = np.polyfit(times, positions, 1)
    predicted = slope * times + intercept
    residual = positions - predicted
    ss_res = float(np.sum(residual ** 2))
    ss_tot = float(np.sum((positions - np.mean(positions)) ** 2))
    r_squared = 1.0 - ss_res / ss_tot if ss_tot > 0.0 else float("nan")
    all_front_positions = [
        item["front_x_m"] for item in evolution if np.isfinite(item["front_x_m"])
    ]
    return {
        "mean_ros_mps": time_weighted_mean,
        "unweighted_stepwise_mean_ros_mps": unweighted_mean,
        "fit_slope_mps": float(slope),
        "fit_r_squared": float(r_squared),
        "fit_front_observation_count": len(selected),
        "fit_ros_interval_count": len(interval_pairs),
        "fit_start_time_s": float(times[0]),
        "fit_end_time_s": float(times[-1]),
        "fit_start_x_m": float(positions[0]),
        "fit_end_x_m": float(positions[-1]),
        "leading_edge_x_m": float(max(all_front_positions)),
    }


def write_front_trajectory(path: Path, evolution: list[dict]) -> None:
    """Write the extracted front trajectory as an auditable comma-separated data artifact."""
    path.parent.mkdir(parents=True, exist_ok=True)
    previous_time = None
    previous_front = None
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(
            stream,
            fieldnames=[
                "dump_index", "time_seconds", "phi_raster",
                "leading_phi_zero_x_m", "stepwise_ros_mps",
            ],
        )
        writer.writeheader()
        for item in evolution:
            time_seconds = float(item["time_seconds"])
            front = float(item["front_x_m"])
            stepwise_ros = ""
            if (
                previous_time is not None
                and np.isfinite(previous_front)
                and np.isfinite(front)
                and time_seconds > previous_time
            ):
                stepwise_ros = (front - previous_front) / (time_seconds - previous_time)
            writer.writerow({
                "dump_index": item["dump_index"],
                "time_seconds": f"{time_seconds:.10g}",
                "phi_raster": item["phi_raster"].name,
                "leading_phi_zero_x_m": f"{front:.10g}" if np.isfinite(front) else "",
                "stepwise_ros_mps": f"{stepwise_ros:.10g}" if stepwise_ros != "" else "",
            })
            previous_time = time_seconds
            previous_front = front


def write_figure(results: list[dict]) -> None:
    """Create the case vector-PDF figure from actual outputs and the declared reference solution."""
    figure, axis = plt.subplots(figsize=(7.2, 4.6), constrained_layout=True)
    for dx in sorted(MARKERS):
        group = sorted(
            [r for r in results if r["dx_m"] == dx and "normalized_ros" in r],
            key=lambda r: r["wind_cfl"],
        )
        if group:
            axis.plot(
                [r["wind_cfl"] for r in group],
                [r["normalized_ros"] for r in group],
                marker=MARKERS[dx], linewidth=1.3, markersize=6,
                label=rf"$\Delta x={dx:g}$ m",
            )
        else:
            axis.plot(
                [],
                [],
                marker=MARKERS[dx],
                linewidth=1.3,
                label=rf"$\Delta x={dx:g}$ m")
    axis.axhline(
        1.0,
        color="black",
        linestyle="--",
        linewidth=1.2,
        label="Exact: ROS = wind speed")
    axis.axvspan(
        0.0,
        ACCEPTANCE_CFL_MAX,
        color="0.92",
        zorder=-10,
        label="Acceptance range")
    axis.set_xlim(0.0, 1.25)
    axis.set_ylim(0.6, 1.2)
    axis.set_xticks(np.arange(0.0, 1.21, 0.2))
    axis.set_xlabel(r"Wind-based CFL, $u_{wind}\Delta t/\Delta x$ [-]")
    axis.set_ylabel(r"Time-averaged $ROS/u_{wind}$ [-]")
    axis.grid(True, color="0.85", linewidth=0.7)
    axis.legend(loc="lower left", ncol=2, fontsize=8)
    polish_figure(figure)
    figure.savefig(FIGURE_DIR / "eulerian_transport_convergence.pdf", format="pdf")
    plt.close(figure)


def write_representative_figure(
    evolution: list[dict], measurement: dict, variant: dict
) -> None:
    """Visualize the exact extraction and averaging path for one computed case."""
    valid = [item for item in evolution if np.isfinite(item["front_x_m"])]
    selected = [
        item for item in valid
        if FIT_START_M <= item["front_x_m"] <= FIT_END_M
    ]
    pairs = [
        (left, right)
        for left, right in zip(evolution[:-1], evolution[1:])
        if np.isfinite(left["front_x_m"])
        and np.isfinite(right["front_x_m"])
        and FIT_START_M <= left["front_x_m"] <= FIT_END_M
        and FIT_START_M <= right["front_x_m"] <= FIT_END_M
    ]
    times = np.asarray([item["time_seconds"] for item in valid], dtype=float)
    positions = np.asarray([item["front_x_m"] for item in valid], dtype=float)
    interval_times = np.asarray([right["time_seconds"]
                                for _, right in pairs], dtype=float)
    dt = np.asarray([
        right["time_seconds"] - left["time_seconds"] for left, right in pairs
    ], dtype=float)
    stepwise_ros = np.asarray([
        (right["front_x_m"] - left["front_x_m"])
        / (right["time_seconds"] - left["time_seconds"])
        for left, right in pairs
    ], dtype=float)
    running_mean = np.cumsum(stepwise_ros * dt) / np.cumsum(dt)

    figure, axes = plt.subplots(2, 2, figsize=(8.2, 6.5), constrained_layout=True)
    dx = float(variant["dx_m"])
    item = selected[len(selected) // 2]
    x, phi = read_phi_centerline(item["phi_raster"], variant)
    relative_x = x - item["front_x_m"]
    local = np.isfinite(phi) & (np.abs(relative_x) <= 3.5 * dx)
    axes[0, 0].plot(
        relative_x[local], phi[local], marker="o", color="#4c78a8",
        label=rf"Cell-center $\phi$ at $t={item['time_seconds']:.1f}$ s",
    )
    axes[0, 0].axhline(0.0, color="black", linewidth=1.0)
    axes[0, 0].axvline(
        0.0, color="#e45756", linestyle="--", linewidth=1.2,
        label=rf"Interpolated $x_{{LE}}={item['front_x_m']:.3f}$ m",
    )
    axes[0, 0].scatter(
        [0.0], [0.0], marker="D", s=35, color="#e45756", zorder=5,
    )
    axes[0, 0].set_xlabel(r"Position relative to extracted $x_{LE}$ [m]")
    axes[0, 0].set_ylabel(r"Centerline level set, $\phi$ [-]")
    axes[0, 0].set_title("(a) Subcell front extraction", loc="left")
    axes[0, 0].legend(loc="best", fontsize=8)

    axes[0, 1].plot(times, positions, color="#4c78a8",
                    linewidth=1.2, label="Extracted $x_{LE}$")
    fit_times = np.asarray([item["time_seconds"] for item in selected], dtype=float)
    fitted = measurement["fit_slope_mps"] * fit_times + (
        np.mean([item["front_x_m"] for item in selected])
        - measurement["fit_slope_mps"] * np.mean(fit_times)
    )
    axes[0, 1].plot(fit_times, fitted, color="#e45756",
                    linestyle="--", label="Diagnostic linear fit")
    axes[0, 1].axvspan(
        measurement["fit_start_time_s"], measurement["fit_end_time_s"],
        color="0.92", zorder=-10, label="Averaging interval",
    )
    axes[0, 1].set_xlabel("Simulation time [s]")
    axes[0, 1].set_ylabel(r"Leading-edge position, $x_{LE}$ [m]")
    axes[0, 1].set_title("(b) Extracted front trajectory", loc="left")
    axes[0, 1].legend(loc="upper left", fontsize=8)

    axes[1, 0].step(interval_times, stepwise_ros, where="post",
                    color="#4c78a8", linewidth=1.0)
    axes[1, 0].axhline(WIND_SPEED_MPS, color="black",
                       linestyle="--", label="Exact wind speed")
    axes[1, 0].axhline(
        measurement["mean_ros_mps"], color="#e45756", linewidth=1.4,
        label=rf"Time-weighted mean: {measurement['mean_ros_mps']:.3f} m/s",
    )
    axes[1, 0].set_xlabel("Simulation time [s]")
    axes[1, 0].set_ylabel(r"Stepwise $ROS_i$ [m/s]")
    zoom_start = measurement["fit_start_time_s"]
    zoom_end = min(zoom_start + 20.0, measurement["fit_end_time_s"])
    zoom_mask = (interval_times >= zoom_start) & (interval_times <= zoom_end)
    if np.any(zoom_mask):
        zoom_peak = max(
            float(np.max(stepwise_ros[zoom_mask])),
            WIND_SPEED_MPS,
            measurement["mean_ros_mps"],
        )
        axes[1, 0].set_xlim(zoom_start, zoom_end)
        axes[1, 0].set_ylim(-0.04 * zoom_peak, 1.08 * zoom_peak)
    axes[1, 0].set_title("(c) Stepwise ROS: 20 s detail", loc="left")
    axes[1, 0].legend(loc="upper right", fontsize=8)

    axes[1, 1].plot(interval_times, running_mean /
                    WIND_SPEED_MPS, color="#4c78a8", linewidth=1.3)
    axes[1, 1].axhline(1.0, color="black", linestyle="--", label="Exact")
    axes[1, 1].axhline(
        measurement["mean_ros_mps"] / WIND_SPEED_MPS,
        color="#e45756", linewidth=1.4,
        label=rf"Final: {measurement['mean_ros_mps']/WIND_SPEED_MPS:.4f}",
    )
    axes[1, 1].set_xlabel("Simulation time [s]")
    axes[1, 1].set_ylabel(r"Cumulative mean $ROS/u_{wind}$ [-]")
    axes[1, 1].set_title("(d) Convergence of the reported mean", loc="left")
    axes[1, 1].legend(loc="best", fontsize=8)
    for axis in axes.flat:
        axis.grid(True, color="0.87", linewidth=0.6)
        axis.set_axisbelow(True)
    figure.suptitle(
        rf"Representative case: $\Delta x={variant['dx_m']:g}$ m, "
        rf"$CFL_{{wind}}={variant['wind_cfl']:.1f}$"
    )
    polish_figure(figure)
    figure.savefig(FIGURE_DIR / "representative_front_extraction.pdf", format="pdf")
    plt.close(figure)


def latex_escape(value: object) -> str:
    """Escape generated text before inserting it into a LaTeX macro or table cell."""
    return str(value).replace("_", "\\_")


def main() -> None:
    """Run postprocessing from case inputs through final generated artifacts."""
    for directory in (OUTPUT_DIR, FIGURE_DIR, REPORT_DIR):
        directory.mkdir(exist_ok=True)
    trajectory_dir = OUTPUT_DIR / "front_trajectories"
    trajectory_dir.mkdir(exist_ok=True)
    manifest_path = VARIANTS_DIR / "manifest.json"
    if not manifest_path.is_file():
        raise FileNotFoundError(f"Run preprocess.py first; missing {manifest_path}")
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    results = []
    representative = None
    for variant in manifest:
        result = {
            "name": variant["name"],
            "dx_m": float(variant["dx_m"]),
            "wind_cfl": float(variant["wind_cfl"]),
            "simulation_dt_s": float(variant["simulation_dt_s"]),
            "status": "not_run",
        }
        output_dir = CASE_DIR / variant["directory"] / "outputs"
        try:
            phi_rasters = phi_rasters_by_dump(output_dir)
            if not phi_rasters:
                result["notes"] = (
                    "No transient phi rasters; ELMFIRE has not been run with DUMP_PHI"
                )
            else:
                evolution, timestamp_path = read_front_evolution(output_dir, variant)
                measurement = measure_ros(evolution)
                trajectory_path = trajectory_dir / f"{variant['name']}.csv"
                write_front_trajectory(trajectory_path, evolution)
                normalized = measurement["mean_ros_mps"] / WIND_SPEED_MPS
                relative_error = abs(normalized - 1.0)
                required = result["wind_cfl"] <= ACCEPTANCE_CFL_MAX + 1.0e-12
                passed = relative_error <= MAX_RELATIVE_ERROR
                result.update({
                    "status": "pass" if (not required or passed) else "fail",
                    "acceptance_required": required,
                    "dump_times_file": str(timestamp_path.relative_to(CASE_DIR)),
                    "phi_raster_count": len(evolution),
                    "first_phi_raster": str(evolution[0]["phi_raster"].relative_to(CASE_DIR)),
                    "last_phi_raster": str(evolution[-1]["phi_raster"].relative_to(CASE_DIR)),
                    "front_trajectory_csv": str(trajectory_path.relative_to(CASE_DIR)),
                    "normalized_ros": normalized,
                    "relative_error": relative_error,
                    **measurement,
                })
                if variant["name"] == REPRESENTATIVE_VARIANT:
                    representative = (evolution, measurement, variant)
        except (FileNotFoundError, RuntimeError, ValueError) as error:
            result.update(status="insufficient_output", notes=str(error))
        results.append(result)

    computed = [r for r in results if "normalized_ros" in r]
    required = [r for r in results if r["wind_cfl"] <= ACCEPTANCE_CFL_MAX + 1.0e-12]
    required_computed = [r for r in required if "normalized_ros" in r]
    if all(result["status"] == "not_run" for result in results):
        status = "not_run"
    elif not computed:
        status = "incomplete"
    elif len(required_computed) != len(required):
        status = "incomplete"
    elif all(r["relative_error"] <= MAX_RELATIVE_ERROR for r in required_computed):
        status = "pass"
    else:
        status = "fail"
    metrics = {
        "case_id": "CASE03_ETC",
        "objective": "Eulerian firebrand-driven leading-edge speed converges to uniform wind speed",
        "reference_ros_mps": WIND_SPEED_MPS,
        "observable": (
            "time-weighted mean of stepwise leading-edge ROS from the centerline "
            "phi=0 contour over 300-1500 m"),
        "front_location_method": (
            "most downwind centerline phi sign change, linearly interpolated "
            "between adjacent cell centers"),
        "maximum_relative_error": MAX_RELATIVE_ERROR,
        "acceptance_cfl_max": ACCEPTANCE_CFL_MAX,
        "representative_variant": REPRESENTATIVE_VARIANT,
        "representative_figure": "figures/representative_front_extraction.pdf",
        "status": status,
        "num_variants": len(results),
        "num_variants_computed": len(computed),
        "num_required_variants": len(required),
        "num_required_variants_computed": len(required_computed),
        "variants": results,
    }
    metrics["verification_passed"] = status == "pass" if status in {
        "pass", "fail"} else "not_evaluated"
    (OUTPUT_DIR / "metrics.json").write_text(json.dumps(metrics, indent=2) + "\n", encoding="utf-8")
    write_figure(results)
    if representative is not None:
        write_representative_figure(*representative)
    else:
        missing_figure = FIGURE_DIR / "representative_front_extraction.pdf"
        if missing_figure.exists():
            missing_figure.unlink()
    representative_result = next(
        (item for item in results if item["name"] == REPRESENTATIVE_VARIANT), {}
    )
    coarse_cfl05_result = next(
        (item for item in results if item["name"] == "dx30_cfl0p5"), {}
    )
    macros = {
        "caseid": metrics["case_id"], "status": status,
        "numvariants": len(results), "numvariantscomputed": len(computed),
        "numrequiredvariants": len(required),
        "numrequiredvariantscomputed": len(required_computed),
        "referencewindmps": f"{WIND_SPEED_MPS:.2f}",
        "maxrelativeerror": f"{MAX_RELATIVE_ERROR:.2f}",
        "acceptancecflmax": f"{ACCEPTANCE_CFL_MAX:.1f}",
        "representativevariant": REPRESENTATIVE_VARIANT,
        "representativemeanros": (
            f"{representative_result['mean_ros_mps']:.4f}"
            if "mean_ros_mps" in representative_result else "not computed"
        ),
        "representativenormalizedros": (
            f"{representative_result['normalized_ros']:.5f}"
            if "normalized_ros" in representative_result else "not computed"
        ),
        "representativefitslope": (
            f"{representative_result['fit_slope_mps']:.4f}"
            if "fit_slope_mps" in representative_result else "not computed"
        ),
        "coarsecflofiveNormalizedros": (
            f"{coarse_cfl05_result['normalized_ros']:.5f}"
            if "normalized_ros" in coarse_cfl05_result else "not computed"
        ),
        "coarsecflofiveRelativeerror": (
            f"{coarse_cfl05_result['relative_error']:.5f}"
            if "relative_error" in coarse_cfl05_result else "not computed"
        ),
    }
    lines = [
        f"\\expandafter\\def\\csname metric@{re.sub(r'[^A-Za-z0-9]+', '', key)}\\endcsname"
        f"{{{latex_escape(value)}}}" for key,
        value in macros.items()]
    (REPORT_DIR / "metrics_macros.tex").write_text(report_text("\n".join(lines) + "\n"), encoding="utf-8")
    print(f"[OK] evaluated {len(computed)}/{len(results)} variants: {status}")


if __name__ == "__main__":
    main()
    generate_spatial_evidence(
        CASE_DIR, output_preference=("phi", "time_of_arrival"),
        preferred_variant="dx10_cfl0p2",
    )
