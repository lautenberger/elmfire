#!/usr/bin/env python3
"""Create every grid/CFL variant for the Eulerian transport convergence test."""
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
DX_VALUES_M = (5.0, 10.0, 20.0, 30.0)
WIND_CFL_VALUES = (0.2, 0.4, 0.5, 0.6, 0.8, 1.0, 1.2)
DOMAIN_LENGTH_M = 2400.0
DOMAIN_WIDTH_M = 300.0
BUFFER_CELLS = 2
WIND_SPEED_MPS = 6.71
WIND_SPEED_MPH = 15.0
WIND_DIRECTION_DEG = 270.0
SIMULATION_TSTOP_S = 260.0
EMISSION_DURATION_S = 10.0
AREAL_GR_PCS_M2_S = 10.0
NODATA = -9999.0
PROJECTION_EPSG = 32610
INITIAL_PHI = -1.0
WILDLAND_FUEL_MODEL = 102


def replace_assignment(text: str, name: str, value: str) -> str:
    """Replace one namelist assignment while preserving all unrelated configuration text."""
    pattern = rf"(?m)^(\s*{re.escape(name)}\s*=\s*)[^\n/]+"
    updated, count = re.subn(pattern, rf"\g<1>{value}", text, count=1)
    if count != 1:
        raise RuntimeError(f"Expected one {name} assignment in elmfire.data.in")
    return updated


def write_tif(path: Path, values: np.ndarray, dx: float, dtype: int) -> None:
    """Write a deterministic, georeferenced GeoTIFF on the common buffered verification grid."""
    ny, nx = values.shape
    transform = from_origin(
        -BUFFER_CELLS * dx, DOMAIN_WIDTH_M + BUFFER_CELLS * dx, dx, dx
    )
    with rasterio.open(
        path, "w", driver="GTiff", height=ny, width=nx, count=1,
        dtype=np.dtype(dtype).name, crs=f"EPSG:{PROJECTION_EPSG}",
        transform=transform, nodata=NODATA, compress="deflate",
    ) as dataset:
        dataset.write(np.asarray(values, dtype=dtype), 1)


def write_inputs(directory: Path, dx: float, nx: int, ny: int) -> tuple[int, int]:
    """Generate one isolated variant, including aligned rasters, local tables, namelist, and metadata."""
    zeros = np.zeros((ny, nx), dtype=np.float32)
    ones = np.ones((ny, nx), dtype=np.float32)
    ignition_row = BUFFER_CELLS + int(round(DOMAIN_WIDTH_M / dx)) // 2
    ignition_column = BUFFER_CELLS
    phi = ones.copy()
    phi[ignition_row, ignition_column] = INITIAL_PHI
    float_rasters = {
        "asp": zeros, "cbd": zeros, "cbh": zeros, "cc": zeros,
        "ch": zeros, "dem": zeros, "slp": zeros, "adj": ones,
        "new_phi": phi,
        "ws": np.full((ny, nx), WIND_SPEED_MPH, dtype=np.float32),
        "wd": np.full((ny, nx), WIND_DIRECTION_DEG, dtype=np.float32),
        "m1": zeros, "m10": zeros, "m100": zeros,
    }
    for name, values in float_rasters.items():
        write_tif(directory / f"{name}.tif", values, dx, np.float32)
    write_tif(
        directory / "new_fbfm40.tif",
        np.full((ny, nx), WILDLAND_FUEL_MODEL, dtype=np.int16), dx, np.int16,
    )
    return ignition_row, ignition_column


def main() -> None:
    """Run preprocessing from case inputs through final generated artifacts."""
    template = (CASE_DIR / "elmfire.data.in").read_text(encoding="utf-8")
    source_dir = CASE_DIR / "data" / "misc"
    VARIANTS_DIR.mkdir(parents=True, exist_ok=True)
    manifest = []
    for dx in DX_VALUES_M:
        physical_nx = int(round(DOMAIN_LENGTH_M / dx))
        physical_ny = int(round(DOMAIN_WIDTH_M / dx))
        if not math.isclose(physical_nx * dx, DOMAIN_LENGTH_M):
            raise ValueError(f"Domain length is not divisible by dx={dx:g}")
        if not math.isclose(physical_ny * dx, DOMAIN_WIDTH_M):
            raise ValueError(f"Domain width is not divisible by dx={dx:g}")
        nx = physical_nx + 2 * BUFFER_CELLS
        ny = physical_ny + 2 * BUFFER_CELLS
        for wind_cfl in WIND_CFL_VALUES:
            name = f"dx{dx:g}_cfl{wind_cfl:.1f}".replace(".", "p")
            variant_dir = VARIANTS_DIR / name
            scratch = variant_dir / "logs" / "scratch"
            if scratch.exists():
                shutil.rmtree(scratch)
            for relative in ("data/inputs", "data/misc", "outputs", "logs/scratch"):
                (variant_dir / relative).mkdir(parents=True, exist_ok=True)
            ignition_row, ignition_column = write_inputs(
                variant_dir / "data" / "inputs", dx, nx, ny
            )
            for filename in ("fuel_models.csv", "building_fuel_models.csv"):
                source = source_dir / filename
                if not source.is_file():
                    raise FileNotFoundError(source)
                shutil.copy2(source, variant_dir / "data" / "misc" / filename)

            dt = wind_cfl * dx / WIND_SPEED_MPS
            step_count = int(math.ceil(SIMULATION_TSTOP_S / dt - 1.0e-12))
            simulation_tstop_s = step_count * dt
            configured_gr = AREAL_GR_PCS_M2_S
            config = replace_assignment(template, "SIMULATION_DT", f"{dt:.17g}")
            config = replace_assignment(config, "SIMULATION_DTMAX", f"{dt:.17g}")
            config = replace_assignment(
                config, "TARGET_CFL", f"{min(wind_cfl, 0.95):.6g}")
            config = replace_assignment(config, "EMBER_GR", f"{configured_gr:.12g}")
            config = replace_assignment(
                config, "SIMULATION_TSTOP", f"{simulation_tstop_s:.17g}"
            )
            (variant_dir / "elmfire.data.in").write_text(config, encoding="utf-8")
            manifest.append({
                "name": name,
                "directory": str(variant_dir.relative_to(CASE_DIR)),
                "config": "elmfire.data.in",
                "dx_m": dx,
                "wind_cfl": wind_cfl,
                "simulation_dt_s": dt,
                "requested_tstop_s": SIMULATION_TSTOP_S,
                "simulation_tstop_s": simulation_tstop_s,
                "step_count": step_count,
                "nx": nx, "ny": ny,
                "physical_nx": physical_nx, "physical_ny": physical_ny,
                "physical_length_m": DOMAIN_LENGTH_M,
                "physical_width_m": DOMAIN_WIDTH_M,
                "buffer_cells": BUFFER_CELLS,
                "ignition_row": ignition_row,
                "ignition_column": ignition_column,
                "ignition_x_m": 0.5 * dx,
                "wind_speed_mps": WIND_SPEED_MPS,
                "emission_duration_s": EMISSION_DURATION_S,
                "phi_output_every_step": True, "configured_gr_pcs_m2_s": configured_gr,
                "generation_scaling": "constant intensive areal rate; emitted count scales with dx^2 and dt",
            })
    (VARIANTS_DIR / "manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n", encoding="utf-8"
    )
    print(f"[OK] prepared {len(manifest)} transport-convergence variants")


if __name__ == "__main__":
    main()
