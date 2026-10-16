#!/usr/bin/env python3
"""Generate inputs for the two-dimensional firebrand-transport case.

The script writes a buffered fuel/terrain stack, a multiband vector-wind field,
and an auditable input manifest. It prepares data only and never runs ELMFIRE.
"""
from __future__ import annotations
import json
import math
from pathlib import Path
import numpy as np
import rasterio
from rasterio.transform import from_origin

# -----------------------------------------------------------------------------
# Customizable preprocessing parameters (SI units unless explicitly noted)
# -----------------------------------------------------------------------------
CASE_DIR = Path(__file__).resolve().parents[1]
DX_M = 10.0
X_MIN_M = 0.0
X_MAX_M = 800.0
Y_MIN_M = -100.0
Y_MAX_M = 200.0
BUFFER_CELLS = 2
TSTOP_S = 120.0
OUTPUT_TIMES_S = (30.0, 60.0, 90.0, 120.0)
MET_DT_S = 1.0
U_MEAN_MPS = 6.71
U_AMPLITUDE_MPS = 3.355
V_MEAN_MPS = 1.0
V_AMPLITUDE_MPS = 0.5
X_WAVELENGTH_M = 100.0
Y_WAVELENGTH_M = 100.0
WIND_PERIOD_S = 60.0
MPS_PER_MPH = 0.44704
WIND_CFL = 0.5
SURFACE_HEADFIRE_ROS_MPS = 0.71
EMBER_GR_PCS_M2_S = 10.0
EPSG = 32610
NODATA = -9999.0
INITIAL_PHI = -1.0
FUEL_MODEL = 102
IGNITION_X_M = 5.0
IGNITION_Y_M = 5.0


def write_tif(path, values, dtype):
    """Write a 2-D or band-first 3-D array on the common buffered grid."""
    a = values[np.newaxis, ...] if values.ndim == 2 else values
    (nb, ny, nx) = a.shape
    transform = from_origin(
        X_MIN_M - BUFFER_CELLS * DX_M,
        Y_MAX_M + BUFFER_CELLS * DX_M,
        DX_M,
        DX_M,
    )
    with rasterio.open(
        path, 'w', driver='GTiff', height=ny, width=nx, count=nb,
        dtype=np.dtype(dtype).name, crs=f'EPSG:{EPSG}',
        transform=transform, nodata=NODATA, compress='deflate',
    ) as dataset:
        dataset.write(np.asarray(a, dtype=dtype))


def main():
    """Create flat terrain, burnable fuel, point ignition, and vector-wind bands."""
    pnx = round((X_MAX_M - X_MIN_M) / DX_M)
    pny = round((Y_MAX_M - Y_MIN_M) / DX_M)
    x_extent_m = X_MAX_M - X_MIN_M
    y_extent_m = Y_MAX_M - Y_MIN_M
    if (
        not math.isclose(pnx * DX_M, x_extent_m)
        or not math.isclose(pny * DX_M, y_extent_m)
    ):
        raise ValueError('Extents must be divisible by DX_M')
    nx = pnx + 2 * BUFFER_CELLS
    ny = pny + 2 * BUFFER_CELLS
    inp = CASE_DIR / 'data/inputs'
    misc = CASE_DIR / 'data/misc'
    inp.mkdir(parents=True, exist_ok=True)
    misc.mkdir(parents=True, exist_ok=True)
    z = np.zeros((ny, nx), np.float32)
    one = np.ones((ny, nx), np.float32)
    phi = one.copy()
    col = BUFFER_CELLS + int((IGNITION_X_M - X_MIN_M) // DX_M)
    row = BUFFER_CELLS + int((Y_MAX_M - IGNITION_Y_M) // DX_M)
    if not (
        BUFFER_CELLS <= col < nx - BUFFER_CELLS
        and BUFFER_CELLS <= row < ny - BUFFER_CELLS
    ):
        raise ValueError('Ignition outside physical domain')
    phi[row, col] = INITIAL_PHI
    continuous_inputs = {
        'asp': z, 'cbd': z, 'cbh': z, 'cc': z, 'ch': z,
        'dem': z, 'slp': z, 'adj': one, 'new_phi': phi,
    }
    for name, values in continuous_inputs.items():
        write_tif(inp / f'{name}.tif', values, np.float32)
    fuel = np.full((ny, nx), FUEL_MODEL, dtype=np.int16)
    write_tif(inp / 'new_fbfm40.tif', fuel, np.int16)
    x = X_MIN_M + (np.arange(nx) - BUFFER_CELLS + 0.5) * DX_M
    y = Y_MAX_M - (np.arange(ny) - BUFFER_CELLS + 0.5) * DX_M
    times = np.arange(0.0, TSTOP_S + 0.5 * MET_DT_S, MET_DT_S)
    ws = np.empty((len(times), ny, nx), np.float32)
    wd = np.empty_like(ws)
    for (k, t) in enumerate(times):
        phase = np.sin(2 * np.pi * t / WIND_PERIOD_S)
        u = U_MEAN_MPS + U_AMPLITUDE_MPS * \
            np.sin(2 * np.pi * x / X_WAVELENGTH_M) * phase
        v = V_MEAN_MPS + V_AMPLITUDE_MPS * \
            np.sin(2 * np.pi * y / Y_WAVELENGTH_M) * phase
        uu = np.broadcast_to(u[None, :], (ny, nx))
        vv = np.broadcast_to(v[:, None], (ny, nx))
        ws[k] = np.hypot(uu, vv) / MPS_PER_MPH
        wd[k] = (np.degrees(np.arctan2(uu, vv)) - 180.0) % 360.0
    write_tif(inp / 'ws.tif', ws, np.float32)
    write_tif(inp / 'wd.tif', wd, np.float32)
    for name in ('m1', 'm10', 'm100'):
        write_tif(inp / f'{name}.tif', np.zeros_like(ws), np.float32)
    for fn in ('fuel_models.csv', 'building_fuel_models.csv'):
        src = CASE_DIR / 'data' / 'misc' / fn
        if not src.is_file():
            raise FileNotFoundError(src)
    for d in ('outputs', 'figures', 'logs/scratch'):
        (CASE_DIR / d).mkdir(parents=True, exist_ok=True)
    vmax = math.hypot(U_MEAN_MPS + U_AMPLITUDE_MPS, V_MEAN_MPS + V_AMPLITUDE_MPS)
    manifest = {
        'case_id': 'CASE06_F2D',
        'dx_m': DX_M,
        'nx': nx,
        'ny': ny,
        'physical_nx': pnx,
        'physical_ny': pny,
        'buffer_cells': BUFFER_CELLS,
        'physical_extent_m': [X_MIN_M, X_MAX_M, Y_MIN_M, Y_MAX_M],
        'simulation_tstop_s': TSTOP_S,
        'requested_output_times_s': OUTPUT_TIMES_S,
        'meteorology_dt_s': MET_DT_S,
        'meteorology_bands': len(times),
        'u_range_mps': [
            U_MEAN_MPS - U_AMPLITUDE_MPS,
            U_MEAN_MPS + U_AMPLITUDE_MPS],
        'v_range_mps': [
            V_MEAN_MPS - V_AMPLITUDE_MPS,
            V_MEAN_MPS + V_AMPLITUDE_MPS],
        'wind_cfl': WIND_CFL,
        'simulation_dt_s': WIND_CFL * DX_M / vmax,
        'surface_headfire_ros_mps': SURFACE_HEADFIRE_ROS_MPS,
        'emission_duration_s': DX_M / SURFACE_HEADFIRE_ROS_MPS,
        'generation_model': 'PER-AREA',
        'ember_gr_pcs_m2_s': EMBER_GR_PCS_M2_S,
        'ignition_cell': [col, row],
        'ignition_coordinate_m': [IGNITION_X_M, IGNITION_Y_M],
    }
    (CASE_DIR / 'outputs/input_manifest.json').write_text(json.dumps(manifest, indent=2) + '\n')
    print(
        f'[OK] wrote {len(times)} two-dimensional transport case weather bands on a {nx}x{ny} grid')


if __name__ == '__main__':
    main()
