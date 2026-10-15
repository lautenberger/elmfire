#!/usr/bin/env python3
"""Calculate verification metrics from case-local ELMFIRE outputs."""

from __future__ import annotations

from report_language import polish_figure

import json
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import contourpy
import numpy as np
import rasterio

CASE_DIR = Path(__file__).resolve().parents[1]
FIG_DIR = CASE_DIR / "figures"
OUT_DIR = CASE_DIR / "outputs"
FIG_DIR.mkdir(parents=True, exist_ok=True)
OUT_DIR.mkdir(parents=True, exist_ok=True)


def latest_raster(output_dir: Path, prefix: str) -> Path | None:
    files = sorted(output_dir.glob(f"{prefix}_*.tif"))
    return files[-1] if files else None


def read_raster(path: Path) -> tuple[np.ndarray, dict]:
    with rasterio.open(path) as src:
        return src.read(1, masked=True).astype(float), {
            "transform": src.transform, "crs": str(src.crs), "nodata": src.nodata,
        }


def finite_values(array: np.ndarray) -> np.ndarray:
    values = np.ma.asarray(array).compressed()
    return values[np.isfinite(values)]


def equivalent_burned_radius(phi: np.ndarray, transform) -> float:
    """Return the radius of the largest closed, linearly interpolated PHI=0 contour."""
    values = np.ma.filled(phi, np.nan).astype(float)
    rows, cols = values.shape
    x = transform.c + (np.arange(cols) + 0.5) * transform.a
    y = transform.f + (np.arange(rows) + 0.5) * transform.e
    generator = contourpy.contour_generator(x=x, y=y, z=values)
    areas = []
    closure_tolerance = max(abs(float(transform.a)), abs(float(transform.e)))
    for line in generator.lines(0.0):
        if len(line) < 4 or np.linalg.norm(line[0] - line[-1]) > closure_tolerance:
            continue
        xx, yy = line[:, 0], line[:, 1]
        areas.append(0.5 * abs(float(np.dot(xx, np.roll(yy, 1)) - np.dot(yy, np.roll(xx, 1)))))
    if not areas or max(areas) <= 0.0:
        raise ValueError("No closed PHI=0 contour is available")
    return float(np.sqrt(max(areas) / np.pi))


def plot_domain(path: Path, title: str, output_path: Path) -> None:
    array, meta = read_raster(path)
    transform = meta["transform"]
    left = transform.c
    right = left + array.shape[1] * transform.a
    top = transform.f
    bottom = top + array.shape[0] * transform.e
    fig, ax = plt.subplots(figsize=(6.5, 5.2))
    image = ax.imshow(array, extent=(left, right, bottom, top), origin="upper", cmap="viridis")
    ax.set(xlabel="Easting (m)", ylabel="Northing (m)", title=title)
    fig.colorbar(image, ax=ax, label="ELMFIRE field value")
    fig.tight_layout()
    polish_figure(fig)
    fig.savefig(output_path, format="pdf", bbox_inches="tight")
    plt.close(fig)


def write_payload(payload: dict) -> None:
    (OUT_DIR / "metrics.json").write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
    print(f"[OK] {payload['case_id']}: {payload['overall_status']}")

CASE_ID = "CASE18_TCV"
TIME_GRIDS = [45, 90, 180, 360]
SIMULATION_DURATION_S = 360.0
FINEST_ERROR_TOLERANCE_M = 0.5

REFERENCE_ROS_M_PER_S = 0.0116332
INITIAL_RADIUS_M = 20.0
FINAL_TIME_S = 360.0
MINIMUM_OBSERVED_ORDER = 0.8


def observed_order(step: np.ndarray, error: np.ndarray) -> float | None:
    mask = np.isfinite(step) & np.isfinite(error) & (step > 0.0) & (error > 0.0)
    if np.count_nonzero(mask) < 2:
        return None
    return float(np.polyfit(np.log(step[mask]), np.log(error[mask]), 1)[0])


def convergence_plot(step: np.ndarray, error: np.ndarray, xlabel: str, output_path: Path) -> None:
    order = np.argsort(step)
    fig, ax = plt.subplots(figsize=(6.3, 4.7))
    ax.loglog(step[order], error[order], "o-", label="equivalent-radius error")
    ax.set(xlabel=xlabel, ylabel="Absolute radius error (m)", title=f"{CASE_ID} convergence")
    ax.grid(True, which="both", alpha=0.3)
    ax.legend()
    fig.tight_layout()
    polish_figure(fig)
    fig.savefig(output_path, format="pdf", bbox_inches="tight")
    plt.close(fig)


def main() -> None:
    expected_radius = INITIAL_RADIUS_M + REFERENCE_ROS_M_PER_S * FINAL_TIME_S
    rows = []
    missing = []
    finest_phi = None
    for time_grid in TIME_GRIDS:
        output_dir = CASE_DIR / "data" / "outputs" / str(time_grid)
        phi_path = latest_raster(output_dir, "phi")
        if phi_path is None:
            missing.append(time_grid)
            continue
        phi, meta = read_raster(phi_path)
        dx = abs(float(meta["transform"].a))
        radius = equivalent_burned_radius(phi, meta["transform"])
        dt = SIMULATION_DURATION_S / time_grid
        rows.append({"time_grid": time_grid, "dt_s": dt, "radius_m": radius,
                     "absolute_error_m": abs(radius - expected_radius),
                     "source": str(phi_path.relative_to(CASE_DIR))})
        if time_grid == max(TIME_GRIDS):
            finest_phi = phi_path

    if missing or len(rows) != len(TIME_GRIDS):
        write_payload({
            "case_id": CASE_ID, "overall_status": "NOT EVALUABLE",
            "verification_passed": False, "required_outputs_complete": False,
            "missing_time_grids": missing, "metrics": [],
        })
        return

    dt = np.array([row["dt_s"] for row in rows])
    error = np.array([row["absolute_error_m"] for row in rows])
    order = observed_order(dt, error)
    finest = min(rows, key=lambda row: row["dt_s"])
    order_pass = order is not None and order >= MINIMUM_OBSERVED_ORDER
    finest_pass = finest["absolute_error_m"] <= FINEST_ERROR_TOLERANCE_M
    passed = order_pass and finest_pass
    convergence_plot(dt, error, "Time step (s)", FIG_DIR / "convergence.pdf")
    plot_domain(finest_phi, "CASE18 finest-time-grid final level-set field", FIG_DIR / "whole_domain_result.pdf")

    metrics = [
        {"name": "observed temporal order", "expected": MINIMUM_OBSERVED_ORDER,
         "calculated": order, "units": "-", "tolerance": ">= threshold",
         "status": "PASS" if order_pass else "FAIL"},
        {"name": "finest-time-grid radius error", "expected": 0.0,
         "calculated": finest["absolute_error_m"], "units": "m",
         "tolerance": f"<= {FINEST_ERROR_TOLERANCE_M:g}",
         "status": "PASS" if finest_pass else "FAIL"},
    ]
    write_payload({
        "case_id": CASE_ID, "overall_status": "PASS" if passed else "FAIL",
        "verification_passed": passed, "required_outputs_complete": True,
        "expected_radius_m": expected_radius, "variants": rows, "metrics": metrics,
    })


if __name__ == "__main__":
    main()
