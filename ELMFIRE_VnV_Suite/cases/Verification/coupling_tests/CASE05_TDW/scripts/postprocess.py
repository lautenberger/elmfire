#!/usr/bin/env python3
"""Evaluate reference time-dependent-wind case from actual ELMFIRE arrival-time output.

The script integrates the prescribed leading-edge ODE, reconstructs ELMFIRE's
centre-row leading edge from the final TOA raster, calculates position and
nine-point running-average ROS metrics, and writes JSON, LaTeX, and PDF output.
It never runs ELMFIRE.
"""
from __future__ import annotations

from report_language import polish_figure, report_text
import rasterio
from spatial_evidence import generate_spatial_evidence
import numpy as np

import json
import re
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

CASE_DIR = Path(__file__).resolve().parents[1]
OUTPUT_DIR = CASE_DIR / "outputs"
FIGURE_DIR = CASE_DIR / "figures"
REPORT_DIR = CASE_DIR / "report"
BUFFER_CELLS = 2
NODATA = -9999.0
DX_M = 10.0
TSTOP_S = 120.0
SAMPLE_DT_S = 0.745
WIND_MEAN_MPS = 6.71
WIND_AMPLITUDE_MPS = 3.355
WIND_WAVELENGTH_M = 100.0
WIND_PERIOD_S = 60.0
ODE_DT_S = 0.005
RUNNING_WINDOW_POINTS = 9
POSITION_MAE_LIMIT_M = 10.0
POSITION_MAX_ERROR_LIMIT_M = 20.0
RUNNING_ROS_MAE_LIMIT_MPS = 2.0
MIN_TIME_COVERAGE_FRACTION = 0.90


def wind_mps(x_m: float | np.ndarray, time_s: float) -> float | np.ndarray:
    """Evaluate the reference space-time wind field in m/s."""
    return WIND_MEAN_MPS + WIND_AMPLITUDE_MPS * np.sin(
        2.0 * np.pi * np.asarray(x_m) / WIND_WAVELENGTH_M
    ) * np.sin(2.0 * np.pi * time_s / WIND_PERIOD_S)


def integrate_reference(times_s: np.ndarray, x0_m: float) -> np.ndarray:
    """Integrate dx/dt=u(x,t) with RK4 and return x at requested times."""
    result = np.empty_like(times_s)
    result[0] = x0_m
    x = x0_m
    time_s = float(times_s[0])
    for output_index in range(1, len(times_s)):
        target = float(times_s[output_index])
        while time_s < target - 1.0e-12:
            dt = min(ODE_DT_S, target - time_s)
            k1 = float(wind_mps(x, time_s))
            k2 = float(wind_mps(x + 0.5 * dt * k1, time_s + 0.5 * dt))
            k3 = float(wind_mps(x + 0.5 * dt * k2, time_s + 0.5 * dt))
            k4 = float(wind_mps(x + dt * k3, time_s + dt))
            x += dt * (k1 + 2.0 * k2 + 2.0 * k3 + k4) / 6.0
            time_s += dt
        result[output_index] = x
    return result


def read_toa(path: Path) -> tuple[np.ndarray, tuple[float, ...]]:
    """Read and validate the final TOA raster and its geotransform."""
    with rasterio.open(path) as dataset:
        array = dataset.read(1).astype(float)
        nodata = dataset.nodata
        transform = dataset.transform.to_gdal()
    if array.ndim != 2 or array.shape[0] <= 2 * BUFFER_CELLS:
        raise ValueError(f"Unexpected TOA shape {array.shape}")
    if not np.isclose(abs(transform[1]), DX_M, atol=1.0e-6):
        raise ValueError(f"TOA cell size {transform[1]} does not match {DX_M}")
    if nodata is not None:
        array[array == nodata] = np.nan
    array[array <= NODATA] = np.nan
    return array, transform


def running_mean(values: np.ndarray, window: int) -> np.ndarray:
    """Return a centred running mean, leaving incomplete end windows as NaN."""
    result = np.full_like(values, np.nan, dtype=float)
    half = window // 2
    for index in range(half, len(values) - half):
        segment = values[index - half:index + half + 1]
        if np.all(np.isfinite(segment)):
            result[index] = float(np.mean(segment))
    return result


def latex_escape(value: object) -> str:
    """Escape generated text for a LaTeX macro body."""
    return str(value).replace("_", "\\_")


def write_figure(times_s: np.ndarray, x_sim: np.ndarray, x_ref: np.ndarray,
                 ros_inst: np.ndarray, ros_running: np.ndarray,
                 ros_ref: np.ndarray) -> None:
    """Create a reference-Figure-4.15-style two-panel vector PDF."""
    figure, axes = plt.subplots(1, 2, figsize=(11.0, 4.4), constrained_layout=True)
    axes[0].plot(times_s, x_ref, color="black", linewidth=2.0, label="ODE reference")
    axes[0].plot(times_s, x_sim, color="#1f77b4", marker="o", markersize=2.5,
                 linewidth=1.0, label="ELMFIRE from TOA")
    axes[0].set_xlabel("Time [s]")
    axes[0].set_ylabel("Leading-edge position [m]")
    axes[0].set_xlim(0.0, TSTOP_S)
    axes[0].set_ylim(bottom=0.0)
    axes[0].grid(alpha=0.25)
    axes[0].legend()

    axes[1].plot(times_s, ros_ref, color="black", linewidth=2.0, label="ODE reference")
    axes[1].plot(times_s, ros_inst, color="#1f77b4", marker="o", markersize=2.3,
                 linewidth=0.8, label="ELMFIRE instantaneous")
    axes[1].plot(times_s, ros_running, color="#d62728", linestyle=":",
                 linewidth=2.0, label="ELMFIRE 9-point mean")
    axes[1].set_xlabel("Time [s]")
    axes[1].set_ylabel("Leading-edge ROS [m/s]")
    axes[1].set_xlim(0.0, TSTOP_S)
    axes[1].set_ylim(bottom=0.0)
    axes[1].grid(alpha=0.25)
    axes[1].legend()
    polish_figure(figure)
    figure.savefig(FIGURE_DIR / "time_dependent_wind_transport.pdf", format="pdf")
    plt.close(figure)


def write_error_figure(times_s: np.ndarray, x_sim: np.ndarray,
                       x_ref: np.ndarray, ros_running: np.ndarray,
                       ros_ref: np.ndarray) -> None:
    """Show time-resolved position and running-ROS residuals with limits."""
    figure, axes = plt.subplots(1, 2, figsize=(10.4, 3.8), constrained_layout=True)
    position_error = np.abs(x_sim - x_ref)
    ros_error = np.abs(ros_running - ros_ref)
    axes[0].plot(times_s, position_error, color="#1f77b4", linewidth=1.4)
    axes[0].axhline(POSITION_MAE_LIMIT_M, color="#d62728", linestyle="--",
                    label="MAE limit")
    axes[0].axhline(POSITION_MAX_ERROR_LIMIT_M, color="#d62728", linestyle=":",
                    label="maximum-error limit")
    axes[0].set(xlabel="Time [s]", ylabel="Absolute position error [m]",
                xlim=(0.0, TSTOP_S), ylim=(0.0, None))
    axes[1].plot(times_s, ros_error, color="#ff7f0e", linewidth=1.4)
    axes[1].axhline(RUNNING_ROS_MAE_LIMIT_MPS, color="#d62728", linestyle="--",
                    label="running-ROS MAE limit")
    axes[1].set(xlabel="Time [s]", ylabel="Absolute running-ROS error [m/s]",
                xlim=(0.0, TSTOP_S), ylim=(0.0, None))
    for axis in axes:
        axis.grid(alpha=0.25)
        axis.legend(fontsize=8)
    polish_figure(figure)
    figure.savefig(FIGURE_DIR / "time_dependent_wind_errors.pdf", format="pdf")
    plt.close(figure)


def main() -> None:
    """Calculate the verification decision from a real final TOA raster."""
    for directory in (OUTPUT_DIR, FIGURE_DIR, REPORT_DIR):
        directory.mkdir(exist_ok=True)
    times_s = np.arange(0.0, TSTOP_S + 0.5 * SAMPLE_DT_S, SAMPLE_DT_S)
    times_s[-1] = min(times_s[-1], TSTOP_S)
    if times_s[-1] < TSTOP_S:
        times_s = np.append(times_s, TSTOP_S)
    x0_m = 0.5 * DX_M
    x_ref = integrate_reference(times_s, x0_m)
    ros_ref = wind_mps(x_ref, times_s)

    metrics = {
        "case_id": "CASE05_TDW",
        "status": "not_run",
        "verification_passed": "not_evaluated",
        "position_mae_limit_m": POSITION_MAE_LIMIT_M,
        "position_max_error_limit_m": POSITION_MAX_ERROR_LIMIT_M,
        "running_ros_mae_limit_mps": RUNNING_ROS_MAE_LIMIT_MPS,
        "minimum_time_coverage_fraction": MIN_TIME_COVERAGE_FRACTION,
        "running_average_points": RUNNING_WINDOW_POINTS,
        "sample_dt_s": SAMPLE_DT_S,
        "position_mae_m": "not computed",
        "position_max_error_m": "not computed",
        "running_ros_mae_mps": "not computed",
        "time_coverage_fraction": 0.0,
        "position_mae_passed": "NOT EVALUABLE",
        "position_max_error_passed": "NOT EVALUABLE",
        "running_ros_mae_passed": "NOT EVALUABLE",
        "time_coverage_passed": "NOT EVALUABLE",
        "final_simulated_position_m": "not computed",
        "final_reference_position_m": float(x_ref[-1]),
    }
    toa_files = sorted(OUTPUT_DIR.glob("time_of_arrival_*_*.tif"))
    if not toa_files:
        write_figure(
            times_s, np.full_like(
                times_s, np.nan), x_ref, np.full_like(
                times_s, np.nan), np.full_like(
                times_s, np.nan), ros_ref)
        write_error_figure(times_s, np.full_like(times_s, np.nan), x_ref,
                           np.full_like(times_s, np.nan), ros_ref)
    else:
        toa_path = toa_files[-1]
        toa, transform = read_toa(toa_path)
        row = toa.shape[0] // 2
        columns = np.arange(BUFFER_CELLS, toa.shape[1] - BUFFER_CELLS)
        x_cells = transform[0] + (columns + 0.5) * transform[1]
        toa_profile = toa[row, columns]
        x_sim = np.full_like(times_s, np.nan)
        for index, time_s in enumerate(times_s):
            reached = np.isfinite(toa_profile) & (
                toa_profile >= 0.0) & (
                toa_profile <= time_s + 1.0e-6)
            if np.any(reached):
                x_sim[index] = float(np.max(x_cells[reached]))
        ros_inst = np.full_like(times_s, np.nan)
        ros_inst[1:] = np.diff(x_sim) / np.diff(times_s)
        ros_running = running_mean(ros_inst, RUNNING_WINDOW_POINTS)
        valid_position = np.isfinite(x_sim) & np.isfinite(x_ref)
        valid_ros = np.isfinite(ros_running) & np.isfinite(ros_ref)
        coverage = float(np.count_nonzero(valid_position) / len(times_s))
        if np.any(valid_position) and np.any(valid_ros):
            position_errors = np.abs(x_sim[valid_position] - x_ref[valid_position])
            position_mae = float(np.mean(position_errors))
            position_max = float(np.max(position_errors))
            ros_mae = float(
                np.mean(
                    np.abs(
                        ros_running[valid_ros] -
                        ros_ref[valid_ros])))
            position_mae_pass = position_mae <= POSITION_MAE_LIMIT_M
            position_max_pass = position_max <= POSITION_MAX_ERROR_LIMIT_M
            ros_pass = ros_mae <= RUNNING_ROS_MAE_LIMIT_MPS
            coverage_pass = coverage >= MIN_TIME_COVERAGE_FRACTION
            passed = position_mae_pass and position_max_pass and ros_pass and coverage_pass
            metrics.update(
                status="pass" if passed else "fail",
                verification_passed=passed,
                selected_toa_file=toa_path.name,
                position_mae_m=position_mae,
                position_max_error_m=position_max,
                running_ros_mae_mps=ros_mae,
                time_coverage_fraction=coverage,
                position_mae_passed=position_mae_pass,
                position_max_error_passed=position_max_pass,
                running_ros_mae_passed=ros_pass,
                time_coverage_passed=coverage_pass,
                comparison_samples=int(np.count_nonzero(valid_position)),
                final_simulated_position_m=float(x_sim[valid_position][-1]),
                final_reference_position_m=float(x_ref[valid_position][-1]),
            )
        else:
            metrics.update(
                status="insufficient_output",
                time_coverage_fraction=coverage)
        write_figure(times_s, x_sim, x_ref, ros_inst, ros_running, ros_ref)
        write_error_figure(times_s, x_sim, x_ref, ros_running, ros_ref)

    (OUTPUT_DIR / "metrics.json").write_text(
        json.dumps(metrics, indent=2) + "\n", encoding="utf-8"
    )
    lines = []
    for key, value in metrics.items():
        macro = re.sub(r"[^A-Za-z0-9]+", "", key)
        lines.append(
            f"\\expandafter\\def\\csname metric@{macro}\\endcsname"
            f"{{{latex_escape(value)}}}"
        )
    (REPORT_DIR / "metrics_macros.tex").write_text(
        report_text("\n".join(lines) + "\n"), encoding="utf-8"
    )
    print(f"[OK] time-dependent-wind case evaluated: {metrics['status']}")


if __name__ == "__main__":
    main()
    generate_spatial_evidence(
        CASE_DIR, output_preference=("time_of_arrival", "ember_flux"),
    )
