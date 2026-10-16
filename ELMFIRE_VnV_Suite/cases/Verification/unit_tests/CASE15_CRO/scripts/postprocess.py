#!/usr/bin/env python3
"""Calculate verification metrics from case-local ELMFIRE outputs."""

from __future__ import annotations

from report_language import polish_figure

import json
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
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


def equivalent_burned_radius(phi: np.ndarray, cell_size_m: float) -> float:
    burned_cells = int(np.count_nonzero(np.ma.filled(phi < 0.0, False)))
    return float(np.sqrt(burned_cells * cell_size_m**2 / np.pi))


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

CASE_ID = "CASE15_CRO"
OUTPUT_DIR = CASE_DIR / "data" / "outputs"
INPUT_PHI = CASE_DIR / "data" / "inputs" / "phi.tif"
REFERENCE_ROS_M_PER_S = 0.0116332
REFERENCE_ROS_M_PER_MIN = 60.0 * REFERENCE_ROS_M_PER_S
INITIAL_RADIUS_M = 5.0
FINAL_TIME_S = 360.0
ROS_RELATIVE_TOLERANCE = 0.05
RADIUS_TOLERANCE_CELLS = 2.0


def main() -> None:
    phi_path = latest_raster(OUTPUT_DIR, "phi")
    rate_path = latest_raster(OUTPUT_DIR, "vs")
    required = [phi_path, rate_path]
    if any(path is None for path in required) or not INPUT_PHI.exists():
        write_payload({
            "case_id": CASE_ID, "overall_status": "NOT EVALUABLE",
            "verification_passed": False, "required_outputs_complete": False,
            "metrics": [], "reason": "Required phi, spread-rate, or input raster is missing.",
        })
        return

    phi, meta = read_raster(phi_path)
    rate, _ = read_raster(rate_path)
    cell_size = abs(float(meta["transform"].a))
    rate_values = finite_values(rate)
    measured_rate = float(np.max(rate_values)) if rate_values.size else None
    measured_radius = equivalent_burned_radius(phi, cell_size)
    expected_radius = INITIAL_RADIUS_M + REFERENCE_ROS_M_PER_S * FINAL_TIME_S
    rate_error = None if measured_rate is None else abs(measured_rate - REFERENCE_ROS_M_PER_MIN) / REFERENCE_ROS_M_PER_MIN
    radius_error = abs(measured_radius - expected_radius)
    rate_pass = rate_error is not None and rate_error <= ROS_RELATIVE_TOLERANCE
    radius_pass = radius_error <= RADIUS_TOLERANCE_CELLS * cell_size
    passed = rate_pass and radius_pass

    plot_domain(phi_path, "CASE15 final level-set field", FIG_DIR / "whole_domain_result.pdf")
    plot_domain(INPUT_PHI, "CASE15 initial level-set configuration", FIG_DIR / "input_configuration.pdf")
    metrics = [
        {"name": "maximum spread rate", "expected": REFERENCE_ROS_M_PER_MIN,
         "calculated": measured_rate, "units": "m/min", "tolerance": "5% relative",
         "status": "PASS" if rate_pass else "FAIL"},
        {"name": "equivalent final radius", "expected": expected_radius,
         "calculated": measured_radius, "units": "m",
         "tolerance": f"{RADIUS_TOLERANCE_CELLS:g} cells ({RADIUS_TOLERANCE_CELLS * cell_size:g} m)",
         "status": "PASS" if radius_pass else "FAIL"},
    ]
    write_payload({
        "case_id": CASE_ID, "overall_status": "PASS" if passed else "FAIL",
        "verification_passed": passed, "required_outputs_complete": True,
        "reference_ros_m_per_s": REFERENCE_ROS_M_PER_S, "cell_size_m": cell_size,
        "source_files": [str(phi_path.relative_to(CASE_DIR)), str(rate_path.relative_to(CASE_DIR))],
        "metrics": metrics,
    })


if __name__ == "__main__":
    main()
