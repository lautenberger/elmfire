#!/usr/bin/env python3
"""Measure isotropy of the control and directional elongation on slope."""
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
OUT = CASE_DIR / "outputs"
FIG = CASE_DIR / "figures"
OUT.mkdir(parents=True, exist_ok=True)
FIG.mkdir(parents=True, exist_ok=True)


def latest(name):
    p = sorted(
        (CASE_DIR / f"variants/{name}/outputs").glob("time_of_arrival*.tif")
    )
    return p[-1] if p else None


def read(path):
    with rasterio.open(path) as src:
        a = src.read(1, masked=True).filled(np.nan).astype(float)
        a[(a < 0) | (a > 1e8)] = np.nan
        return a


def extents(a):
    mask = np.isfinite(a)
    rows, cols = np.where(mask)
    return (
        (float(cols.max() - cols.min() + 1), float(rows.max() - rows.min() + 1))
        if rows.size
        else (float("nan"), float("nan"))
    )


def difference(a, b):
    v = np.isfinite(a) & np.isfinite(b)
    if not v.any():
        return float("nan")
    return float(np.mean(np.abs(a[v] - b[v])) / max(np.nanpercentile(a[v], 90), 1.0))


def main():
    paths = {n: latest(n) for n in ("flat", "slope_30")}
    if any(p is None for p in paths.values()):
        p = {
            "case_id": "CASE33_SDO",
            "overall_status": "NOT EVALUABLE",
            "verification_passed": False,
            "required_outputs_complete": False,
            "variants": list(paths),
            "metrics": [],
            "reason": "A required time-of-arrival raster is missing.",
        }
        (OUT / "metrics.json").write_text(json.dumps(p, indent=2) + "\n")
        return
    flat, slope = read(paths["flat"]), read(paths["slope_30"])
    fx, fy = extents(flat)
    sx, sy = extents(slope)
    flat_aniso = abs(fx - fy) / max(fx, fy)
    slope_elong = max(sx, sy) / min(sx, sy)
    response = difference(slope, flat)
    metrics = [
        {
            "name": "flat-control anisotropy",
            "expected": "<= 0.05",
            "calculated": flat_aniso,
            "units": "fraction",
            "tolerance": "5%",
            "status": "PASS" if flat_aniso <= 0.05 else "FAIL",
        },
        {
            "name": "slope-front elongation ratio",
            "expected": ">= 1.10",
            "calculated": slope_elong,
            "units": "ratio",
            "tolerance": "10% directional elongation",
            "status": "PASS" if slope_elong >= 1.10 else "FAIL",
        },
        {
            "name": "slope-forcing response",
            "expected": ">= 0.05",
            "calculated": response,
            "units": "normalized L1",
            "tolerance": "5% difference from flat",
            "status": "PASS" if response >= 0.05 else "FAIL",
        },
    ]
    passed = all(m["status"] == "PASS" for m in metrics)
    fig, axes = plt.subplots(1, 3, figsize=(12, 4))
    for ax, a, title in zip(
        axes,
        (flat, slope, slope - flat),
        ("flat control", "30 degree slope", "slope minus flat"),
    ):
        im = ax.imshow(
            a, origin="upper", cmap="coolwarm" if "minus" in title else "inferno"
        )
        ax.set_title(title)
        fig.colorbar(im, ax=ax, shrink=0.75)
    fig.tight_layout()
    polish_figure(fig)
    fig.savefig(FIG / "verification_summary.pdf", bbox_inches="tight")
    plt.close(fig)
    with rasterio.open(CASE_DIR / "variants/slope_30/inputs/slp.tif") as src:
        a = src.read(1)
    fig, ax = plt.subplots(figsize=(5, 5))
    ax.imshow(a, origin="upper")
    ax.set_title("Uniform 30 degree slope input")
    fig.tight_layout()
    polish_figure(fig)
    fig.savefig(FIG / "input_configuration.pdf", bbox_inches="tight")
    plt.close(fig)
    p = {
        "case_id": "CASE33_SDO",
        "overall_status": "PASS" if passed else "FAIL",
        "verification_passed": passed,
        "required_outputs_complete": True,
        "variants": list(paths),
        "metrics": metrics,
        "source_files": [str(x.relative_to(CASE_DIR)) for x in paths.values()],
    }
    (OUT / "metrics.json").write_text(json.dumps(p, indent=2) + "\n")
    print(f"[OK] CASE33_SDO: {p['overall_status']}")


if __name__ == "__main__":
    main()
