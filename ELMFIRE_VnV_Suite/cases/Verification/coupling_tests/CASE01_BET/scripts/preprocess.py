#!/usr/bin/env python3
"""Prepare the three biomass-emission meshes.

This script creates input rasters, fuel tables, namelists, run directories, and
a manifest without importing any repository-level Python utility.
"""
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
RESOLUTIONS_M = (5.0, 10.0, 30.0)
DOMAIN_LENGTH_M = 1200.0
DOMAIN_WIDTH_M = 300.0
BUFFER_CELLS = 2
WIND_SPEED_MPH = 15.0
WIND_SPEED_MPS = 6.71
WIND_DIRECTION_DEG = 270.0
WIND_CFL = 0.5
SIMULATION_TSTOP_S = 500.0
UNIT_WIDTH_EMBER_GR_PER_MW_S = 33.3
NODATA = -9999.0
PROJECTION_EPSG = 32610
INITIAL_PHI = -1.0
WILDLAND_FUEL_MODEL = 102


def write_tif(path: Path, array: np.ndarray, dx: float, dtype: int) -> None:
    """Write a deterministic, georeferenced GeoTIFF on the common buffered verification grid."""
    rows, columns = array.shape
    active_rows = rows - 2 * BUFFER_CELLS
    transform = from_origin(
        -BUFFER_CELLS * dx, (active_rows + BUFFER_CELLS) * dx, dx, dx
    )
    with rasterio.open(
        path, "w", driver="GTiff", height=rows, width=columns, count=1,
        dtype=np.dtype(dtype).name, crs=f"EPSG:{PROJECTION_EPSG}",
        transform=transform, nodata=NODATA, compress="deflate",
    ) as dataset:
        dataset.write(np.asarray(array, dtype=dtype), 1)


def replace_assignment(config: str, name: str, value: str) -> str:
    """Replace one namelist assignment while preserving all unrelated configuration text."""
    pattern = rf"(?m)^(\s*{re.escape(name)}\s*=\s*)[^\n/]+"
    result, replacements = re.subn(pattern, rf"\g<1>{value}", config, count=1)
    if replacements != 1:
        raise RuntimeError(f"Expected one {name} assignment in elmfire.data.in")
    return result


def write_inputs(input_dir: Path, dx: float, nx: int, ny: int) -> None:
    """Generate one isolated variant, including aligned rasters, local tables, namelist, and metadata."""
    zeros = np.zeros((ny, nx), dtype=np.float32)
    ones = np.ones((ny, nx), dtype=np.float32)
    phi = ones.copy()
    phi[ny // 2, BUFFER_CELLS] = INITIAL_PHI
    rasters = {
        "asp": zeros, "cbd": zeros, "cbh": zeros, "cc": zeros,
        "ch": zeros, "dem": zeros, "slp": zeros, "adj": ones,
        "new_phi": phi,
        "ws": np.full((ny, nx), WIND_SPEED_MPH, dtype=np.float32),
        "wd": np.full((ny, nx), WIND_DIRECTION_DEG, dtype=np.float32),
        "m1": zeros, "m10": zeros, "m100": zeros,
    }
    for name, array in rasters.items():
        write_tif(input_dir / f"{name}.tif", array, dx, np.float32)
    fuel_model = np.full((ny, nx), WILDLAND_FUEL_MODEL, dtype=np.int16)
    write_tif(input_dir / "new_fbfm40.tif", fuel_model, dx, np.int16)


def main() -> None:
    """Run preprocessing from case inputs through final generated artifacts."""
    template = (CASE_DIR / "elmfire.data.in").read_text(encoding="utf-8")
    source_dir = CASE_DIR / "data" / "misc"
    VARIANTS_DIR.mkdir(parents=True, exist_ok=True)
    manifest = []
    for dx in RESOLUTIONS_M:
        name = f"dx{dx:g}"
        variant_dir = VARIANTS_DIR / name
        interior_nx = round(DOMAIN_LENGTH_M / dx)
        interior_ny = round(DOMAIN_WIDTH_M / dx)
        if not math.isclose(
                interior_nx * dx,
                DOMAIN_LENGTH_M) or not math.isclose(
                interior_ny * dx,
                DOMAIN_WIDTH_M):
            raise ValueError(f"Interior domain is not divisible by dx={dx:g} m")
        nx = interior_nx + 2 * BUFFER_CELLS
        ny = interior_ny + 2 * BUFFER_CELLS
        scratch_dir = variant_dir / "logs" / "scratch"
        if scratch_dir.exists():
            shutil.rmtree(scratch_dir)
        for relative in (
            "data/inputs",
            "data/misc",
            "outputs",
            "figures",
                "logs/scratch"):
            (variant_dir / relative).mkdir(parents=True, exist_ok=True)
        write_inputs(variant_dir / "data/inputs", dx, nx, ny)
        for filename in ("fuel_models.csv", "building_fuel_models.csv"):
            source = source_dir / filename
            if not source.is_file():
                raise FileNotFoundError(f"Required case-local input is missing: {source}")
            shutil.copy2(source, variant_dir / "data/misc" / filename)

        nominal_dt = WIND_CFL * dx / WIND_SPEED_MPS
        whole_second_divisors = [
            value for value in range(1, int(math.floor(nominal_dt)) + 1)
            if math.isclose(SIMULATION_TSTOP_S / value,
                            round(SIMULATION_TSTOP_S / value))
        ]
        if whole_second_divisors:
            dt = float(max(whole_second_divisors))
        else:
            step_count = int(math.ceil(SIMULATION_TSTOP_S / nominal_dt - 1.0e-12))
            dt = SIMULATION_TSTOP_S / step_count
        step_count = int(round(SIMULATION_TSTOP_S / dt))
        simulation_tstop_s = step_count * dt
        config = replace_assignment(template, "SIMULATION_DT", f"{dt:.17g}")
        config = replace_assignment(config, "SIMULATION_DTMAX", f"{dt:.17g}")
        config = replace_assignment(config, "SIMULATION_TSTOP", f"{simulation_tstop_s:.17g}")
        config = replace_assignment(config, "TARGET_CFL", f"{WIND_CFL:g}")
        configured_ember_gr = UNIT_WIDTH_EMBER_GR_PER_MW_S / dx
        config = replace_assignment(
            config, "EMBER_GR_PER_MW_VEGE", f"{configured_ember_gr:.12g}"
        )
        (variant_dir / "elmfire.data.in").write_text(config, encoding="utf-8")
        manifest.append({
            "name": name,
            "directory": str(variant_dir.relative_to(CASE_DIR)),
            "working_directory": str(variant_dir.relative_to(CASE_DIR)),
            "config": "elmfire.data.in",
            "dx_m": dx, "nx": nx, "ny": ny,
            "interior_nx": interior_nx, "interior_ny": interior_ny,
            "buffer_cells": BUFFER_CELLS,
            "interior_length_m": DOMAIN_LENGTH_M,
            "interior_width_m": DOMAIN_WIDTH_M,
            "unit_width_ember_gr_per_mw_s": UNIT_WIDTH_EMBER_GR_PER_MW_S,
            "configured_ember_gr_per_mw_s": configured_ember_gr,
            "generation_scaling": "configured rate = unit-width rate / dx",
            "wind_speed_mps": WIND_SPEED_MPS,
            "requested_wind_cfl": WIND_CFL,
            "wind_cfl": WIND_SPEED_MPS * dt / dx,
            "nominal_simulation_dt_s": nominal_dt,
            "simulation_dt_s": dt,
            "requested_tstop_s": SIMULATION_TSTOP_S,
            "simulation_tstop_s": simulation_tstop_s,
            "step_count": step_count,
        })
    (VARIANTS_DIR / "manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n", encoding="utf-8"
    )
    print(f"[OK] prepared {len(manifest)} biomass-emission variants in {VARIANTS_DIR}")


if __name__ == "__main__":
    main()
