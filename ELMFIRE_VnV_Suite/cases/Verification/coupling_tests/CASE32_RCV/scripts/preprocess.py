#!/usr/bin/env python3
"""Create an asymmetric ignition raster and its exact 90-degree rotation."""
from __future__ import annotations
import re
import shutil
from pathlib import Path
import numpy as np
import rasterio
from rasterio.transform import from_origin

CASE_DIR = Path(__file__).resolve().parents[1]
BASE = (CASE_DIR / "elmfire.data.in").read_text(encoding="utf-8")
SIZE, CELL, NODATA = 400, 5.0, -9999.0
TRANSFORM = from_origin(-1000.0, 1000.0, CELL, CELL)


def write(path, data, dtype):
    with rasterio.open(
        path,
        "w",
        driver="GTiff",
        height=SIZE,
        width=SIZE,
        count=1,
        dtype=dtype,
        crs="EPSG:32610",
        transform=TRANSFORM,
        nodata=NODATA,
        compress="deflate",
    ) as dst:
        dst.write(np.asarray(data, dtype=dtype), 1)


def replace(text, key, value):
    text, count = re.subn(
        rf"(?mi)^(\s*{re.escape(key)}\s*=\s*).*$", rf"\g<1>{value}", text, count=1
    )
    if count != 1:
        raise KeyError(key)
    return text


def main():
    phi = np.ones((SIZE, SIZE), dtype=np.float32)
    phi[175:225, 188:198] = -1.0
    phi[215:225, 188:245] = -1.0
    variants = {"original": phi, "rotated_ccw": np.rot90(phi, 1)}
    for name, ignition in variants.items():
        root = CASE_DIR / f"variants/{name}"
        if root.exists():
            shutil.rmtree(root)
        inputs = root / "inputs"
        for d in (inputs, root / "outputs", root / "scratch"):
            d.mkdir(parents=True, exist_ok=True)
        floats = {
            "ws": 0.0,
            "wd": 0.0,
            "m1": 3.0,
            "m10": 5.0,
            "m100": 6.0,
            "adj": 1.0,
            "phi": ignition,
        }
        ints = {
            "slp": 0,
            "asp": 0,
            "dem": 0,
            "fbfm40": 10,
            "cc": 0,
            "ch": 0,
            "cbh": 0,
            "cbd": 0,
        }
        for field, value in floats.items():
            write(
                inputs / f"{field}.tif",
                (
                    value
                    if isinstance(value, np.ndarray)
                    else np.full((SIZE, SIZE), value)
                ),
                "float32",
            )
        for field, value in ints.items():
            write(inputs / f"{field}.tif", np.full((SIZE, SIZE), value), "int16")
        config = BASE
        for key, value in {
            "FUELS_AND_TOPOGRAPHY_DIRECTORY": f"'./variants/{name}/inputs'",
            "WEATHER_DIRECTORY": f"'./variants/{name}/inputs'",
            "OUTPUTS_DIRECTORY": f"'./variants/{name}/outputs'",
            "SCRATCH": f"'./variants/{name}/scratch'",
        }.items():
            config = replace(config, key, value)
        (root / "elmfire.data").write_text(config)
    print("[OK] Generated original and rotated CASE32 inputs")


if __name__ == "__main__":
    main()
