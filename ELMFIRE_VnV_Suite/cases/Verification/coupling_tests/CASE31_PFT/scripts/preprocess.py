#!/usr/bin/env python3
"""Generate planar-front, fuel-transition, and fuel-break inputs locally."""

from __future__ import annotations
import json
import re
import shutil
from pathlib import Path
import numpy as np
import rasterio
from rasterio.transform import from_origin

CASE_DIR = Path(__file__).resolve().parents[1]
BASE = (CASE_DIR / "elmfire.data.in").read_text(encoding="utf-8")
SIZE, CELL, HALF = 300, 5.0, 750.0
TRANSFORM = from_origin(-HALF, HALF, CELL, CELL)
CRS, NODATA = "EPSG:32610", -9999.0
VARIANTS = (
    "diagonal",
    "fuel_jump",
    "fuel_jump_wind",
    "continuous_break",
    "gapped_break",
)


def begin_run() -> None:
    """Invalidate prior decisions before any generated variant is replaced."""
    output = CASE_DIR / "outputs"
    output.mkdir(parents=True, exist_ok=True)
    status = {
        "case_id": "CASE31_PFT",
        "overall_status": "NOT EVALUABLE",
        "workflow_status": "INCOMPLETE",
        "verification_passed": False,
        "required_outputs_complete": False,
        "required_variant_count": len(VARIANTS),
        "completed_variant_count": 0,
        "variants": list(VARIANTS),
        "metrics": [],
        "reason": "Preprocessing started a new run; required ELMFIRE outputs are not complete.",
    }
    (output / "metrics.json").write_text(
        json.dumps(status, indent=2) + "\n", encoding="utf-8"
    )
    for path in (
        CASE_DIR / "figures/input_configuration.pdf",
        CASE_DIR / "figures/verification_summary.pdf",
        CASE_DIR / "report/case_report.pdf",
    ):
        path.unlink(missing_ok=True)


def write(path: Path, data: np.ndarray, dtype: str) -> None:
    with rasterio.open(
        path,
        "w",
        driver="GTiff",
        height=SIZE,
        width=SIZE,
        count=1,
        dtype=dtype,
        crs=CRS,
        transform=TRANSFORM,
        nodata=NODATA,
        compress="deflate",
    ) as dst:
        dst.write(data.astype(dtype), 1)


def replace(text: str, key: str, value: object) -> str:
    pattern = rf"(?mi)^(\s*{re.escape(key)}\s*=\s*).*$"
    text, count = re.subn(pattern, rf"\g<1>{value}", text, count=1)
    if count != 1:
        raise KeyError(key)
    return text


def finite_strip_level_set(normal: np.ndarray, tangent: np.ndarray) -> np.ndarray:
    """Build a 50 m deep finite strip in ELMFIRE's bounded PHI interval."""
    q_normal = np.abs(normal + 25.0) - 25.0
    q_tangent = np.abs(tangent) - 500.0
    outside = np.hypot(np.maximum(q_normal, 0.0), np.maximum(q_tangent, 0.0))
    signed_distance = outside + np.minimum(
        np.maximum(q_normal, q_tangent), 0.0
    )
    phi = np.clip(signed_distance / CELL, -1.0, 1.0).astype(np.float32)
    border = np.concatenate((phi[0, :], phi[-1, :], phi[:, 0], phi[:, -1]))
    if (
        not np.any(phi < 0.0)
        or not np.any(phi > 0.0)
        or np.any(border <= 0.0)
        or np.any(np.isclose(phi, 0.0, atol=1.0e-7))
    ):
        raise ValueError(
            "Finite PHI strip must be ignited, bounded, interior, and off-grid"
        )
    return phi


def main() -> None:
    begin_run()
    x = -HALF + (np.arange(SIZE) + 0.5) * CELL
    y = HALF - (np.arange(SIZE) + 0.5) * CELL
    xx, yy = np.meshgrid(x, y)
    for name in VARIANTS:
        root = CASE_DIR / "variants" / name
        if root.exists():
            shutil.rmtree(root)
        inputs = root / "inputs"
        for directory in (inputs, root / "outputs", root / "scratch"):
            directory.mkdir(parents=True, exist_ok=True)

        wind = 1.0 if name == "fuel_jump_wind" else 0.0
        if name == "diagonal":
            normal = (xx + yy) / np.sqrt(2.0) + 300.0
            tangent = (xx - yy) / np.sqrt(2.0)
        else:
            # Put both strip faces between cell centers so ELMFIRE's mandatory
            # +/-5e-4 PHI input noise cannot randomly choose ignition cells.
            normal = xx + 95.0
            tangent = yy
        # ELMFIRE resets PHI below -1.1 as missing data.  Use a clipped signed
        # distance so the complete ignition interior and a nonzero interface
        # gradient survive input, including when no cell center lies at PHI=0.
        phi = finite_strip_level_set(normal, tangent)
        expected_negative_cells = 1981 if name == "diagonal" else 2000
        if np.count_nonzero(phi < 0.0) != expected_negative_cells:
            raise ValueError(
                f"{name}: expected {expected_negative_cells} negative PHI cells"
            )
        fbfm = np.full((SIZE, SIZE), 10, dtype=np.int16)
        if name in {"fuel_jump", "fuel_jump_wind"}:
            fbfm[xx >= 0.0] = 3
        elif name in {"continuous_break", "gapped_break"}:
            fbfm[(xx >= 0.0) & (xx < 30.0)] = 91
            if name == "gapped_break":
                fbfm[(xx >= 0.0) & (xx < 30.0) & (np.abs(yy) <= 50.0)] = 10

        floats = {
            "ws": wind,
            "wd": 270.0,
            "m1": 3.0,
            "m10": 5.0,
            "m100": 6.0,
            "adj": 1.0,
            "phi": phi,
        }
        ints = {
            "slp": 0,
            "asp": 0,
            "dem": 0,
            "fbfm40": fbfm,
            "cc": 0,
            "ch": 0,
            "cbh": 0,
            "cbd": 0,
        }
        for field, value in floats.items():
            array = (
                value if isinstance(value, np.ndarray) else np.full((SIZE, SIZE), value)
            )
            write(inputs / f"{field}.tif", array, "float32")
        for field, value in ints.items():
            array = (
                value if isinstance(value, np.ndarray) else np.full((SIZE, SIZE), value)
            )
            write(inputs / f"{field}.tif", array, "int16")

        config = BASE
        for key, value in {
            "FUELS_AND_TOPOGRAPHY_DIRECTORY": f"'./variants/{name}/inputs'",
            "WEATHER_DIRECTORY": f"'./variants/{name}/inputs'",
            "OUTPUTS_DIRECTORY": f"'./variants/{name}/outputs'",
            "SCRATCH": f"'./variants/{name}/scratch'",
        }.items():
            config = replace(config, key, value)
        (root / "elmfire.data").write_text(config, encoding="utf-8")
    print(f"[OK] Generated {len(VARIANTS)} CASE31 variants")


if __name__ == "__main__":
    main()
