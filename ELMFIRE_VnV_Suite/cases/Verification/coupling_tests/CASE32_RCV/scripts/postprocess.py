#!/usr/bin/env python3
"""Compare the rotated ELMFIRE solution with an exact raster rotation."""
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


def main():
    paths = {n: latest(n) for n in ("original", "rotated_ccw")}
    if any(p is None for p in paths.values()):
        payload = {
            "case_id": "CASE32_RCV",
            "overall_status": "NOT EVALUABLE",
            "verification_passed": False,
            "required_outputs_complete": False,
            "variants": list(paths),
            "metrics": [],
            "reason": "A required time-of-arrival raster is missing.",
        }
        (OUT / "metrics.json").write_text(json.dumps(payload, indent=2) + "\n")
        return
    original = read(paths["original"])
    rotated = read(paths["rotated_ccw"])
    expected = np.rot90(original, 1)
    valid = np.isfinite(expected) & np.isfinite(rotated)
    union = np.isfinite(expected) | np.isfinite(rotated)
    mask_agreement = float(valid.sum() / union.sum()) if union.any() else float("nan")
    scale = (
        max(float(np.nanpercentile(expected[valid], 90)), 1.0) if valid.any() else 1.0
    )
    value_error = (
        float(np.mean(np.abs(expected[valid] - rotated[valid])) / scale)
        if valid.any()
        else float("nan")
    )
    metrics = [
        {
            "name": "rotated finite-mask agreement",
            "expected": ">= 0.99",
            "calculated": mask_agreement,
            "units": "fraction",
            "tolerance": "1% disagreement",
            "status": "PASS" if mask_agreement >= 0.99 else "FAIL",
        },
        {
            "name": "rotated arrival-time error",
            "expected": "<= 0.02",
            "calculated": value_error,
            "units": "normalized L1",
            "tolerance": "2%",
            "status": "PASS" if value_error <= 0.02 else "FAIL",
        },
    ]
    passed = all(m["status"] == "PASS" for m in metrics)
    fig, axes = plt.subplots(1, 3, figsize=(12, 4))
    for ax, a, title in zip(
        axes,
        (original, rotated, rotated - expected),
        ("original", "rotated run", "rotated minus expected"),
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
    with rasterio.open(CASE_DIR / "variants/original/inputs/phi.tif") as src:
        phi = src.read(1)
    fig, ax = plt.subplots(figsize=(5, 5))
    ax.imshow(phi, origin="upper", cmap="gray")
    ax.set_title("Asymmetric initial level-set mask")
    fig.tight_layout()
    polish_figure(fig)
    fig.savefig(FIG / "input_configuration.pdf", bbox_inches="tight")
    plt.close(fig)
    payload = {
        "case_id": "CASE32_RCV",
        "overall_status": "PASS" if passed else "FAIL",
        "verification_passed": passed,
        "required_outputs_complete": True,
        "variants": list(paths),
        "metrics": metrics,
        "source_files": [str(p.relative_to(CASE_DIR)) for p in paths.values()],
    }
    (OUT / "metrics.json").write_text(json.dumps(payload, indent=2) + "\n")
    print(f"[OK] CASE32_RCV: {payload['overall_status']}")


if __name__ == "__main__":
    main()
