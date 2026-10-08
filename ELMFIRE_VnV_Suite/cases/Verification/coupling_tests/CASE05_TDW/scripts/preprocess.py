#!/usr/bin/env python3
"""Generate the deterministic multiband inputs for reference time-dependent-wind case.

The script reads no model output and does not run ELMFIRE. It writes a flat,
buffered one-dimensional domain, 121 meteorology bands representing the
prescribed space-time wind, required fuel/weather rasters, and input metadata.
"""
from __future__ import annotations

import json
import math
from pathlib import Path

import numpy as np
import rasterio
from rasterio.transform import from_origin

# -----------------------------------------------------------------------------
# Customizable verification parameters (SI units unless explicitly noted)
# -----------------------------------------------------------------------------
CASE_DIR = Path(__file__).resolve().parents[1]
DX_M = 10.0
PHYSICAL_LENGTH_M = 1200.0
PHYSICAL_WIDTH_M = 100.0
BUFFER_CELLS = 2
SIMULATION_TSTOP_S = 120.0
METEOROLOGY_DT_S = 1.0
WIND_MEAN_MPS = 6.71
WIND_AMPLITUDE_MPS = 3.355
WIND_WAVELENGTH_M = 100.0
WIND_PERIOD_S = 60.0
WIND_DIRECTION_DEG = 270.0
MPS_PER_MPH = 0.44704
WIND_CFL = 0.5
EMISSION_DURATION_S = 10.0
EMBER_GR_PCS_M2_S = 1.0
PROJECTION_EPSG = 32610
NODATA = -9999.0
INITIAL_PHI = -1.0
WILDLAND_FUEL_MODEL = 102


def write_tif(path: Path, values: np.ndarray, dtype: int) -> None:
    """Write a 2-D array or a band-first 3-D array with the common grid."""
    bands = values[np.newaxis, ...] if values.ndim == 2 else values
    nbands, rows, columns = bands.shape
    transform = from_origin(
        -BUFFER_CELLS * DX_M,
        PHYSICAL_WIDTH_M + BUFFER_CELLS * DX_M,
        DX_M,
        DX_M,
    )
    with rasterio.open(
        path, "w", driver="GTiff", height=rows, width=columns, count=nbands,
        dtype=np.dtype(dtype).name, crs=f"EPSG:{PROJECTION_EPSG}",
        transform=transform, nodata=NODATA, compress="deflate",
    ) as dataset:
        dataset.write(np.asarray(bands, dtype=dtype))


def main() -> None:
    """Create co-registered inputs and document the manufactured forcing."""
    interior_nx = round(PHYSICAL_LENGTH_M / DX_M)
    interior_ny = round(PHYSICAL_WIDTH_M / DX_M)
    if not math.isclose(interior_nx * DX_M, PHYSICAL_LENGTH_M):
        raise ValueError("Physical length must be divisible by DX_M")
    if not math.isclose(interior_ny * DX_M, PHYSICAL_WIDTH_M):
        raise ValueError("Physical width must be divisible by DX_M")
    nx = interior_nx + 2 * BUFFER_CELLS
    ny = interior_ny + 2 * BUFFER_CELLS
    input_dir = CASE_DIR / "data" / "inputs"
    misc_dir = CASE_DIR / "data" / "misc"
    input_dir.mkdir(parents=True, exist_ok=True)
    misc_dir.mkdir(parents=True, exist_ok=True)

    zeros = np.zeros((ny, nx), dtype=np.float32)
    ones = np.ones((ny, nx), dtype=np.float32)
    phi = ones.copy()
    ignition_row = BUFFER_CELLS + interior_ny // 2
    ignition_column = BUFFER_CELLS
    phi[ignition_row, ignition_column] = INITIAL_PHI

    static = {
        "asp": zeros, "cbd": zeros, "cbh": zeros, "cc": zeros, "ch": zeros,
        "dem": zeros, "slp": zeros, "adj": ones, "new_phi": phi,
    }
    for name, array in static.items():
        write_tif(input_dir / f"{name}.tif", array, np.float32)
    write_tif(
        input_dir / "new_fbfm40.tif",
        np.full((ny, nx), WILDLAND_FUEL_MODEL, dtype=np.int16),
        np.int16,
    )

    # Wind samples are stored at t_b=(b-1) DT_METEOROLOGY. ELMFIRE linearly
    # interpolates adjacent bands. Coordinates are cell centres including halo.
    x_m = (np.arange(nx, dtype=float) - BUFFER_CELLS + 0.5) * DX_M
    times_s = np.arange(0.0, SIMULATION_TSTOP_S + 0.5 * METEOROLOGY_DT_S,
                        METEOROLOGY_DT_S)
    wind_mps = np.empty((len(times_s), ny, nx), dtype=np.float32)
    for band_index, time_s in enumerate(times_s):
        profile = WIND_MEAN_MPS + WIND_AMPLITUDE_MPS * np.sin(
            2.0 * np.pi * x_m / WIND_WAVELENGTH_M
        ) * np.sin(2.0 * np.pi * time_s / WIND_PERIOD_S)
        wind_mps[band_index, :, :] = profile[np.newaxis, :]
    write_tif(input_dir / "ws.tif", wind_mps / MPS_PER_MPH, np.float32)
    write_tif(
        input_dir / "wd.tif",
        np.full_like(wind_mps, WIND_DIRECTION_DEG, dtype=np.float32),
        np.float32,
    )
    for name in ("m1", "m10", "m100"):
        write_tif(input_dir / f"{name}.tif", np.zeros_like(wind_mps), np.float32)

    for filename in ("fuel_models.csv", "building_fuel_models.csv"):
        source = CASE_DIR / "data" / "misc" / filename
        if not source.is_file():
            raise FileNotFoundError(source)

    for directory in ("outputs", "figures", "logs/scratch"):
        (CASE_DIR / directory).mkdir(parents=True, exist_ok=True)
    metadata = {
        "equation": "u(x,t)=6.71+3.355 sin(2 pi x/100) sin(2 pi t/60) m/s",
        "dx_m": DX_M, "nx": nx, "ny": ny,
        "interior_nx": interior_nx, "interior_ny": interior_ny,
        "buffer_cells": BUFFER_CELLS,
        "physical_length_m": PHYSICAL_LENGTH_M,
        "physical_width_m": PHYSICAL_WIDTH_M,
        "meteorology_dt_s": METEOROLOGY_DT_S,
        "meteorology_bands": len(times_s),
        "wind_min_mps": float(np.min(wind_mps)),
        "wind_max_mps": float(np.max(wind_mps)),
        "wind_cfl": WIND_CFL,
        "simulation_dt_s": WIND_CFL * DX_M / (WIND_MEAN_MPS + WIND_AMPLITUDE_MPS),
        "emission_duration_s": EMISSION_DURATION_S,
        "generation_model": "PER-AREA",
        "ember_gr_pcs_m2_s": EMBER_GR_PCS_M2_S,
        "ignition_row": ignition_row,
        "ignition_column": ignition_column,
        "ignition_x_m": 0.5 * DX_M,
    }
    (CASE_DIR / "outputs" / "input_manifest.json").write_text(
        json.dumps(metadata, indent=2) + "\n", encoding="utf-8"
    )
    print(
        f"[OK] wrote {len(times_s)} time-dependent-wind case meteorology bands on a {nx}x{ny} grid")


if __name__ == "__main__":
    main()
