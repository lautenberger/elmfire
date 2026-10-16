#!/usr/bin/env python3
"""Generate the flat control and Adam Laird's 30-degree slope variant."""
from __future__ import annotations
import re
import shutil
from pathlib import Path
import numpy as np
import rasterio
from rasterio.transform import from_origin

CASE_DIR = Path(__file__).resolve().parents[1]
BASE = (CASE_DIR / "elmfire.data.in").read_text()
SIZE = 400
CELL = 5.0
NODATA = -9999.0


def write(path, value, dtype):
    a = np.full((SIZE, SIZE), value, dtype=dtype)
    with rasterio.open(
        path,
        "w",
        driver="GTiff",
        height=SIZE,
        width=SIZE,
        count=1,
        dtype=dtype,
        crs="EPSG:32610",
        transform=from_origin(-1000, 1000, CELL, CELL),
        nodata=NODATA,
        compress="deflate",
    ) as dst:
        dst.write(a, 1)


def replace(text, key, value):
    text, n = re.subn(
        rf"(?mi)^(\s*{re.escape(key)}\s*=\s*).*$", rf"\g<1>{value}", text, count=1
    )
    if n != 1:
        raise KeyError(key)
    return text


def main():
    for name, slope in (("flat", 0), ("slope_30", 30)):
        root = CASE_DIR / f"variants/{name}"
        if root.exists():
            shutil.rmtree(root)
        inputs = root / "inputs"
        for d in (inputs, root / "outputs", root / "scratch"):
            d.mkdir(parents=True, exist_ok=True)
        for field, value in {
            "ws": 0.0,
            "wd": 0.0,
            "m1": 3.0,
            "m10": 5.0,
            "m100": 6.0,
            "adj": 1.0,
            "phi": 1.0,
        }.items():
            write(inputs / f"{field}.tif", value, "float32")
        for field, value in {
            "slp": slope,
            "asp": 270,
            "dem": 0,
            "fbfm40": 10,
            "cc": 70,
            "ch": 400,
            "cbh": 20,
            "cbd": 15,
        }.items():
            write(inputs / f"{field}.tif", value, "int16")
        config = BASE
        for key, value in {
            "FUELS_AND_TOPOGRAPHY_DIRECTORY": f"'./variants/{name}/inputs'",
            "WEATHER_DIRECTORY": f"'./variants/{name}/inputs'",
            "OUTPUTS_DIRECTORY": f"'./variants/{name}/outputs'",
            "SCRATCH": f"'./variants/{name}/scratch'",
        }.items():
            config = replace(config, key, value)
        (root / "elmfire.data").write_text(config)
    print("[OK] Generated CASE33 flat and slope variants")


if __name__ == "__main__":
    main()
