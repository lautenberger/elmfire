#!/usr/bin/env python3
"""Prepare the two timestep variants for the generation-duration verification."""
from __future__ import annotations

import json
import math
import re
import shutil
from pathlib import Path

import numpy as np
import rasterio
from rasterio.transform import from_origin

# -----------------------------------------------------------------------------
# Customizable preprocessing parameters (physical quantities use SI units)
# -----------------------------------------------------------------------------
CASE_DIR = Path(__file__).resolve().parents[1]
VARIANTS_DIR = CASE_DIR / "variants"
CELL_SIZE_M = 10.0
PHYSICAL_LENGTH_M = 500.0
PHYSICAL_WIDTH_M = 60.0
BUFFER_CELLS = 2
TIMESTEPS_S = (0.13, 12.7)
SIMULATION_TSTOP_S = 120.0
WIND_SPEED_MPH = 10.0
WIND_DIRECTION_DEG = 270.0
GENERATION_RATE_PCS_S = 10.0
GENERATION_RATE_PCS_S_M2 = GENERATION_RATE_PCS_S / CELL_SIZE_M**2
RESIDENCE_TIME_S = 10.0
MU_DOWNWIND = 2.18
SIGMA_DOWNWIND = 1.23
P_EPS = 0.01
NODATA = -9999.0
PROJECTION_EPSG = 32610
INITIAL_PHI = -1.0
WILDLAND_FUEL_MODEL = 102


def replace_assignment(config: str, name: str, value: str) -> str:
    """Replace one namelist assignment while preserving all unrelated configuration text."""
    pattern = rf"(?m)^(\s*{re.escape(name)}\s*=\s*)[^\n/]+"
    result, count = re.subn(pattern, rf"\g<1>{value}", config, count=1)
    if count != 1:
        raise RuntimeError(f"Expected one {name} assignment in elmfire.data.in")
    return result


def write_tif(path: Path, array: np.ndarray, dtype: int) -> None:
    """Write a deterministic, georeferenced GeoTIFF on the common buffered verification grid."""
    rows, columns = array.shape
    physical_rows = rows - 2 * BUFFER_CELLS
    transform = from_origin(
        -BUFFER_CELLS * CELL_SIZE_M,
        (physical_rows + BUFFER_CELLS) * CELL_SIZE_M,
        CELL_SIZE_M,
        CELL_SIZE_M,
    )
    with rasterio.open(
        path, "w", driver="GTiff", height=rows, width=columns, count=1,
        dtype=np.dtype(dtype).name, crs=f"EPSG:{PROJECTION_EPSG}",
        transform=transform, nodata=NODATA, compress="deflate",
    ) as dataset:
        dataset.write(np.asarray(array, dtype=dtype), 1)


def write_inputs(input_dir: Path, nx: int, ny: int, ignition_row: int) -> None:
    """Generate one isolated variant, including aligned rasters, local tables, namelist, and metadata."""
    zeros = np.zeros((ny, nx), dtype=np.float32)
    ones = np.ones((ny, nx), dtype=np.float32)
    phi = ones.copy()
    phi[ignition_row, BUFFER_CELLS] = INITIAL_PHI
    rasters = {
        "asp": zeros, "cbd": zeros, "cbh": zeros, "cc": zeros,
        "ch": zeros, "dem": zeros, "slp": zeros, "adj": ones,
        "new_phi": phi,
        "ws": np.full((ny, nx), WIND_SPEED_MPH, dtype=np.float32),
        "wd": np.full((ny, nx), WIND_DIRECTION_DEG, dtype=np.float32),
        "m1": zeros, "m10": zeros, "m100": zeros,
    }
    for name, array in rasters.items():
        write_tif(input_dir / f"{name}.tif", array, np.float32)
    write_tif(input_dir / "new_fbfm40.tif",
              np.full((ny, nx), WILDLAND_FUEL_MODEL, dtype=np.int16), np.int16)


def main() -> None:
    """Run preprocessing from case inputs through final generated artifacts."""
    interior_nx = round(PHYSICAL_LENGTH_M / CELL_SIZE_M)
    interior_ny = round(PHYSICAL_WIDTH_M / CELL_SIZE_M)
    if not math.isclose(interior_nx * CELL_SIZE_M, PHYSICAL_LENGTH_M):
        raise ValueError("Physical length must be divisible by the cell size")
    if not math.isclose(interior_ny * CELL_SIZE_M, PHYSICAL_WIDTH_M):
        raise ValueError("Physical width must be divisible by the cell size")
    nx = interior_nx + 2 * BUFFER_CELLS
    ny = interior_ny + 2 * BUFFER_CELLS
    ignition_row = BUFFER_CELLS + interior_ny // 2
    ignition_column = BUFFER_CELLS

    template = (CASE_DIR / "elmfire.data.in").read_text(encoding="utf-8")
    fuel_source = CASE_DIR / "data" / "misc"
    VARIANTS_DIR.mkdir(parents=True, exist_ok=True)
    manifest = []

    for dt in TIMESTEPS_S:
        name = "dt0p13" if math.isclose(dt, 0.13) else "dt12p7"
        variant_dir = VARIANTS_DIR / name
        for relative in ("data/inputs", "data/misc", "outputs", "logs"):
            target = variant_dir / relative
            if target.exists():
                shutil.rmtree(target)
        for relative in ("data/inputs", "data/misc", "outputs", "logs/scratch"):
            (variant_dir / relative).mkdir(parents=True, exist_ok=True)

        write_inputs(variant_dir / "data/inputs", nx, ny, ignition_row)
        for filename in ("fuel_models.csv", "building_fuel_models.csv"):
            source = fuel_source / filename
            if not source.is_file():
                raise FileNotFoundError(f"Missing required case-local table: {source}")
            shutil.copy2(source, variant_dir / "data/misc" / filename)

        step_count = int(math.ceil(SIMULATION_TSTOP_S / dt - 1.0e-12))
        simulation_tstop_s = step_count * dt
        config = replace_assignment(template, "SIMULATION_DT", f"{dt:.17g}")
        config = replace_assignment(config, "SIMULATION_DTMAX", f"{dt:.17g}")
        config = replace_assignment(config, "DTDUMP", f"{simulation_tstop_s:.17g}")
        config = replace_assignment(
            config,
            "SIMULATION_TSTOP",
            f"{simulation_tstop_s:.17g}")
        (variant_dir / "elmfire.data.in").write_text(config, encoding="utf-8")

        manifest.append({
            "name": name,
            "directory": str(variant_dir.relative_to(CASE_DIR)),
            "config": "elmfire.data.in",
            "dt_s": dt, "dx_m": CELL_SIZE_M, "nx": nx, "ny": ny,
            "interior_nx": interior_nx, "interior_ny": interior_ny,
            "physical_length_m": PHYSICAL_LENGTH_M,
            "physical_width_m": PHYSICAL_WIDTH_M,
            "buffer_cells": BUFFER_CELLS,
            "ignition_row": ignition_row, "ignition_column": ignition_column,
            "generation_model": "PER-AREA",
            "ember_gr_pcs_s_m2": GENERATION_RATE_PCS_S_M2,
            "pixel_generation_rate_pcs_s": GENERATION_RATE_PCS_S,
            "residence_time_s": RESIDENCE_TIME_S,
            "expected_total_firebrands": GENERATION_RATE_PCS_S * RESIDENCE_TIME_S,
            "mu_downwind": MU_DOWNWIND, "sigma_downwind": SIGMA_DOWNWIND,
            "p_eps": P_EPS, "wind_speed_mph": WIND_SPEED_MPH,
            "wind_direction_deg": WIND_DIRECTION_DEG,
            "requested_tstop_s": SIMULATION_TSTOP_S,
            "simulation_tstop_s": simulation_tstop_s,
            "step_count": step_count,
        })

    (VARIANTS_DIR / "manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n", encoding="utf-8"
    )
    print(f"[OK] prepared {len(manifest)} residence-time variants")


if __name__ == "__main__":
    main()
