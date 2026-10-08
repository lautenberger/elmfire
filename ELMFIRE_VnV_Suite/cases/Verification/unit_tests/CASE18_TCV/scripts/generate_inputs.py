#!/usr/bin/env python3
"""Generate deterministic, case-local ELMFIRE rasters and namelists."""

from __future__ import annotations

import shutil
from pathlib import Path

import numpy as np
import rasterio
from rasterio.transform import from_origin

CRS = "EPSG:32610"
NODATA = -9999.0
DOMAIN_SIZE_M = 256.0
FIXED_RESOLUTION = 1024
TIME_GRIDS = [45, 90, 180, 360]
IGNITION_RADIUS_M = 20.0
SPREAD_ADJUSTMENT = 1.0
SIMULATION_DURATION_S = 360.0

FLOAT_FIELDS = {
    "ws": 0.0, "wd": 0.0, "m1": 0.0, "m10": 0.0, "m100": 0.0,
}
INTEGER_FIELDS = {
    "slp": 0, "asp": 0, "dem": 0, "fbfm40": 102,
    "cc": 0, "ch": 0, "cbh": 0, "cbd": 0,
}


def write_raster(path: Path, data: np.ndarray, *, transform, dtype: str) -> None:
    """Write one north-up GeoTIFF with explicit CRS, nodata, and compression."""
    with rasterio.open(
        path, "w", driver="GTiff", height=data.shape[0], width=data.shape[1],
        count=1, dtype=dtype, crs=CRS, transform=transform,
        nodata=NODATA, compress="deflate",
    ) as dst:
        dst.write(data.astype(dtype), 1)


def write_inputs(inputs_dir: Path, *, resolution: int, domain_size_m: float,
                 adjustment: float, ignition_radius_m: float | None) -> None:
    """Write a square grid; row zero is north and cell centers define geometry."""
    inputs_dir.mkdir(parents=True, exist_ok=True)
    cell_size = domain_size_m / resolution
    xmin = ymin = -0.5 * domain_size_m
    ymax = 0.5 * domain_size_m
    transform = from_origin(xmin, ymax, cell_size, cell_size)

    for name, value in {**FLOAT_FIELDS, "adj": adjustment}.items():
        write_raster(
            inputs_dir / f"{name}.tif",
            np.full((resolution, resolution), value, dtype=np.float32),
            transform=transform, dtype="float32",
        )
    for name, value in INTEGER_FIELDS.items():
        write_raster(
            inputs_dir / f"{name}.tif",
            np.full((resolution, resolution), value, dtype=np.int16),
            transform=transform, dtype="int16",
        )

    phi = np.ones((resolution, resolution), dtype=np.float32)
    if ignition_radius_m is not None:
        x = xmin + (np.arange(resolution) + 0.5) * cell_size
        y = ymax - (np.arange(resolution) + 0.5) * cell_size
        xx, yy = np.meshgrid(x, y)
        # Initialize the level set as a resolved signed-distance field. A
        # discontinuous +/-1 mask changes its effective radius with dx and
        # contaminates the measured convergence order.
        signed_distance = np.hypot(xx, yy) - ignition_radius_m
        phi = np.clip(signed_distance / cell_size, -1.0, 1.0).astype(np.float32)
    write_raster(inputs_dir / "phi.tif", phi, transform=transform, dtype="float32")


def replace_value(text: str, key: str, value: str) -> str:
    """Replace one existing scalar namelist assignment."""
    lines = text.splitlines()
    for index, line in enumerate(lines):
        if line.strip().upper().startswith(key.upper()):
            lines[index] = f"{line.split('=', 1)[0].rstrip()} = {value}"
            return "\n".join(lines) + "\n"
    raise KeyError(f"Required namelist key not found: {key}")

def main() -> None:
    case_dir = Path(__file__).resolve().parents[1]
    data_dir = case_dir / "data"
    inputs_dir = data_dir / "inputs"
    for generated in (inputs_dir, data_dir / "outputs", data_dir / "scratch"):
        if generated.exists():
            shutil.rmtree(generated)
        generated.mkdir(parents=True, exist_ok=True)

    write_inputs(
        inputs_dir, resolution=FIXED_RESOLUTION, domain_size_m=DOMAIN_SIZE_M,
        adjustment=SPREAD_ADJUSTMENT, ignition_radius_m=IGNITION_RADIUS_M,
    )
    base_text = (case_dir / "elmfire.data.in").read_text(encoding="utf-8")
    for time_grid in TIME_GRIDS:
        dt_s = SIMULATION_DURATION_S / time_grid
        text = replace_value(base_text, "SIMULATION_DT", f"{dt_s:.9g}")
        text = replace_value(text, "SIMULATION_DTMAX", f"{dt_s:.9g}")
        text = replace_value(text, "OUTPUTS_DIRECTORY", f"'./data/outputs/{time_grid}'")
        text = replace_value(text, "SCRATCH", f"'./data/scratch/{time_grid}'")
        (inputs_dir / f"elmfire_{time_grid}.data").write_text(text, encoding="utf-8")
        (data_dir / "outputs" / str(time_grid)).mkdir(parents=True, exist_ok=True)
        (data_dir / "scratch" / str(time_grid)).mkdir(parents=True, exist_ok=True)
        print(f"[OK] Generated temporal grid Nt={time_grid}, dt={dt_s:g} s")


if __name__ == "__main__":
    main()
