#!/usr/bin/env python3
"""Build the four case-local variants derived from Adam Laird's ignition tests."""

from __future__ import annotations

import re
import shutil
from pathlib import Path

import numpy as np
import rasterio
from rasterio.transform import from_origin

CASE_DIR = Path(__file__).resolve().parents[1]
BASE_CONFIG = (CASE_DIR / "elmfire.data.in").read_text(encoding="utf-8")
CRS = "EPSG:32610"
NODATA = -9999.0
DOMAIN_M = 2000.0
CELL_M = 5.0
SIZE = int(DOMAIN_M / CELL_M)

VARIANTS = {
    "dual_nowind": {
        "wind": (0.0, 0.0),
        "tstop": 14400.0,
        "ignitions": [(-52.5, 0.0, 0.0), (52.5, 0.0, 0.0)],
    },
    "dual_wind": {
        "wind": (5.0, 225.0),
        "tstop": 14400.0,
        "ignitions": [(-52.5, 0.0, 0.0), (52.5, 0.0, 0.0)],
    },
    "staggered_merge": {
        "wind": (0.0, 0.0),
        "tstop": 12000.0,
        "ignitions": [
            # Anchor the first source at the simulation time origin.  With no
            # active front, some ELMFIRE versions advance directly to the end
            # of the run before a strictly-future first ignition is applied.
            # Translating every scheduled time by the same 6000 s preserves
            # all 1200 s relative delays and the merger experiment.
            (0.0, 600.0, 0.0),
            (0.0, 300.0, 1200.0),
            (0.0, 0.0, 2400.0),
            (0.0, -300.0, 3600.0),
            (0.0, -600.0, 4800.0),
        ],
    },
    "near_boundary": {
        "wind": (0.0, 0.0),
        "tstop": 2400.0,
        "ignitions": [
            (975.0, 0.0, 0.0),
            (0.0, 975.0, 0.0),
            (975.0, 975.0, 0.0),
            (-975.0, 0.0, 0.0),
            (0.0, -975.0, 0.0),
            (-975.0, -975.0, 0.0),
            (975.0, -975.0, 0.0),
            (-975.0, 975.0, 0.0),
        ],
    },
}


def write_raster(path: Path, value: float, dtype: str) -> None:
    data = np.full((SIZE, SIZE), value, dtype=dtype)
    with rasterio.open(
        path,
        "w",
        driver="GTiff",
        height=SIZE,
        width=SIZE,
        count=1,
        dtype=dtype,
        crs=CRS,
        transform=from_origin(-1000.0, 1000.0, CELL_M, CELL_M),
        nodata=NODATA,
        compress="deflate",
    ) as dst:
        dst.write(data, 1)


def replace_scalar(text: str, key: str, value: object) -> str:
    pattern = rf"(?mi)^(\s*{re.escape(key)}\s*=\s*).*$"
    updated, count = re.subn(pattern, rf"\g<1>{value}", text, count=1)
    if count != 1:
        raise KeyError(f"Namelist key not found exactly once: {key}")
    return updated


def replace_ignitions(text: str, ignitions: list[tuple[float, float, float]]) -> str:
    text = re.sub(r"(?mi)^\s*[XYT]_IGN\(\d+\)\s*=.*\n?", "", text)
    rows = []
    for index, (x, y, time_s) in enumerate(ignitions, 1):
        rows.extend(
            (
                f"X_IGN({index}) = {x:.1f}",
                f"Y_IGN({index}) = {y:.1f}",
                f"T_IGN({index}) = {time_s:.1f}",
            )
        )
    insertion = "\n".join(rows) + "\n"
    return text.replace("BANDTHICKNESS = 1", insertion + "BANDTHICKNESS = 1", 1)


def main() -> None:
    for name, settings in VARIANTS.items():
        root = CASE_DIR / "variants" / name
        if root.exists():
            shutil.rmtree(root)
        inputs = root / "inputs"
        for directory in (inputs, root / "outputs", root / "scratch"):
            directory.mkdir(parents=True, exist_ok=True)
        floats = {
            "ws": settings["wind"][0],
            "wd": settings["wind"][1],
            "m1": 3.0,
            "m10": 5.0,
            "m100": 6.0,
            "adj": 1.0,
            "phi": 1.0,
        }
        integers = {
            "slp": 0,
            "asp": 0,
            "dem": 0,
            "fbfm40": 10,
            "cc": 70,
            "ch": 400,
            "cbh": 20,
            "cbd": 15,
        }
        for field, value in floats.items():
            write_raster(inputs / f"{field}.tif", value, "float32")
        for field, value in integers.items():
            write_raster(inputs / f"{field}.tif", value, "int16")

        config = BASE_CONFIG
        config = replace_scalar(
            config, "FUELS_AND_TOPOGRAPHY_DIRECTORY", f"'./variants/{name}/inputs'"
        )
        config = replace_scalar(
            config, "WEATHER_DIRECTORY", f"'./variants/{name}/inputs'"
        )
        config = replace_scalar(
            config, "OUTPUTS_DIRECTORY", f"'./variants/{name}/outputs'"
        )
        config = replace_scalar(config, "SCRATCH", f"'./variants/{name}/scratch'")
        config = replace_scalar(config, "SIMULATION_TSTOP", settings["tstop"])
        config = replace_scalar(config, "NUM_IGNITIONS", len(settings["ignitions"]))
        config = replace_ignitions(config, settings["ignitions"])
        (root / "elmfire.data").write_text(config, encoding="utf-8")
    print(f"[OK] Generated {len(VARIANTS)} self-contained CASE30 variants")


if __name__ == "__main__":
    main()
