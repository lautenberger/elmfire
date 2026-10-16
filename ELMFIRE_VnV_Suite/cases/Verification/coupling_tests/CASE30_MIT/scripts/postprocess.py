#!/usr/bin/env python3
"""Evaluate symmetry, forcing response, ignition timing, and boundary handling."""

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
VARIANTS = [
    line.strip()
    for line in (CASE_DIR / "scripts/variants.txt").read_text().splitlines()
    if line.strip()
]
OUT = CASE_DIR / "outputs"
FIG = CASE_DIR / "figures"
OUT.mkdir(parents=True, exist_ok=True)
FIG.mkdir(parents=True, exist_ok=True)


def find_toa(name: str) -> Path | None:
    root = CASE_DIR / "variants" / name / "outputs"
    paths = sorted(root.glob("time_of_arrival*.tif"))
    return paths[-1] if paths else None


def read(path: Path) -> tuple[np.ndarray, rasterio.Affine]:
    with rasterio.open(path) as src:
        data = src.read(1, masked=True).filled(np.nan).astype(float)
        data[(data < 0.0) | (data > 1.0e8)] = np.nan
        return data, src.transform


def paired_error(a: np.ndarray, b: np.ndarray) -> float:
    valid = np.isfinite(a) & np.isfinite(b)
    if not np.any(valid):
        return float("nan")
    scale = max(float(np.nanpercentile(a[valid], 90)), 1.0)
    return float(np.mean(np.abs(a[valid] - b[valid])) / scale)


def elmfire_indices(data: np.ndarray, transform, x: float, y: float) -> tuple[int, int]:
    """Return zero-based array indices from ELMFIRE's CEILING mapping."""
    cell_size = float(transform.a)
    x_lower_left = float(transform.c)
    y_lower_left = float(transform.f + data.shape[0] * transform.e)
    elmfire_col = int(np.ceil((x - x_lower_left) / cell_size))
    elmfire_row = int(np.ceil((y - y_lower_left) / cell_size))
    return data.shape[0] - elmfire_row, elmfire_col - 1


def sample(data: np.ndarray, transform, x: float, y: float) -> float:
    """Sample the cell selected by ELMFIRE's one-based CEILING mapping."""
    row, col = elmfire_indices(data, transform, x, y)
    if 0 <= row < data.shape[0] and 0 <= col < data.shape[1]:
        return float(data[row, col])
    return float("nan")


def centered_reflection_error(data: np.ndarray, center: int, axis: int) -> float:
    """Compare a balanced window reflected about one discrete cell axis."""
    radius = min(center, data.shape[axis] - 1 - center)
    selection = [slice(None), slice(None)]
    selection[axis] = slice(center - radius, center + radius + 1)
    window = data[tuple(selection)]
    return paired_error(window, np.flip(window, axis=axis))


def metric(
    name: str,
    expected: str,
    value: float,
    tolerance: str,
    passed: bool,
    units: str = "",
) -> dict:
    evaluable = np.isfinite(value)
    return {
        "name": name,
        "expected": expected,
        "calculated": None if not np.isfinite(value) else value,
        "units": units,
        "tolerance": tolerance,
        "status": "NOT EVALUABLE" if not evaluable else ("PASS" if passed else "FAIL"),
    }


def main() -> None:
    paths = {name: find_toa(name) for name in VARIANTS}
    if any(path is None for path in paths.values()):
        missing = [name for name, path in paths.items() if path is None]
        payload = {
            "case_id": "CASE30_MIT",
            "overall_status": "NOT EVALUABLE",
            "verification_passed": False,
            "required_outputs_complete": False,
            "variants": VARIANTS,
            "metrics": [],
            "reason": "Missing time-of-arrival output for: " + ", ".join(missing),
        }
        (OUT / "metrics.json").write_text(json.dumps(payload, indent=2) + "\n")
        return

    arrays = {}
    transforms = {}
    for name, path in paths.items():
        arrays[name], transforms[name] = read(path)

    dual = arrays["dual_nowind"]
    ignition_row, _ = elmfire_indices(
        dual, transforms["dual_nowind"], 0.0, 0.0
    )
    mirror_error = max(
        paired_error(dual, np.fliplr(dual)),
        centered_reflection_error(dual, ignition_row, axis=0),
    )
    wind_difference = paired_error(arrays["dual_wind"], dual)
    staggered_points = [
        (0.0, 600.0, 0.0),
        (0.0, 300.0, 1200.0),
        (0.0, 0.0, 2400.0),
        (0.0, -300.0, 3600.0),
        (0.0, -600.0, 4800.0),
    ]
    staggered_errors = [
        abs(sample(arrays["staggered_merge"], transforms["staggered_merge"], x, y) - t)
        for x, y, t in staggered_points
    ]
    finite_timing_errors = [value for value in staggered_errors if np.isfinite(value)]
    max_timing_error = (
        max(finite_timing_errors)
        if len(finite_timing_errors) == len(staggered_points)
        else float("nan")
    )
    boundary_points = [
        (975.0, 0.0),
        (0.0, 975.0),
        (975.0, 975.0),
        (-975.0, 0.0),
        (0.0, -975.0),
        (-975.0, -975.0),
        (975.0, -975.0),
        (-975.0, 975.0),
    ]
    boundary_values = [
        sample(arrays["near_boundary"], transforms["near_boundary"], x, y)
        for x, y in boundary_points
    ]
    initialized = sum(
        np.isfinite(value) and value <= 1200.0 for value in boundary_values
    )

    metrics = [
        metric(
            "dual-ignition reflection error",
            "<= 0.02",
            mirror_error,
            "2% normalized L1",
            mirror_error <= 0.02,
        ),
        metric(
            "wind-forcing response",
            ">= 0.05",
            wind_difference,
            "5% normalized L1",
            wind_difference >= 0.05,
        ),
        metric(
            "maximum scheduled-ignition timing error",
            "<= 1200",
            max_timing_error,
            "one output interval",
            max_timing_error <= 1200.0,
            "s",
        ),
        metric(
            "boundary ignitions initialized",
            "8",
            float(initialized),
            "all eight points",
            initialized == 8,
            "count",
        ),
    ]
    complete = all(row["status"] != "NOT EVALUABLE" for row in metrics)
    passed = complete and all(row["status"] == "PASS" for row in metrics)

    fig, axes = plt.subplots(2, 2, figsize=(10, 8))
    for ax, name in zip(axes.flat, VARIANTS):
        image = ax.imshow(arrays[name], cmap="inferno", origin="upper")
        ax.set_title(name.replace("_", " "))
        fig.colorbar(image, ax=ax, shrink=0.78, label="arrival time (s)")
    fig.suptitle("CASE30 ELMFIRE time-of-arrival fields")
    fig.tight_layout()
    polish_figure(fig)
    fig.savefig(FIG / "verification_summary.pdf", bbox_inches="tight")
    plt.close(fig)

    fig, ax = plt.subplots(figsize=(6, 5))
    ax.imshow(dual, cmap="inferno", origin="upper")
    ax.set_title("Two simultaneous point ignitions; no wind")
    fig.tight_layout()
    polish_figure(fig)
    fig.savefig(FIG / "input_configuration.pdf", bbox_inches="tight")
    plt.close(fig)

    payload = {
        "case_id": "CASE30_MIT",
        "overall_status": "PASS" if passed else ("FAIL" if complete else "NOT EVALUABLE"),
        "verification_passed": passed,
        "required_outputs_complete": complete,
        "variants": VARIANTS,
        "metrics": metrics,
        "source_files": [str(path.relative_to(CASE_DIR)) for path in paths.values()],
    }
    (OUT / "metrics.json").write_text(
        json.dumps(payload, indent=2) + "\n", encoding="utf-8"
    )
    print(f"[OK] CASE30_MIT: {payload['overall_status']}")


if __name__ == "__main__":
    main()
