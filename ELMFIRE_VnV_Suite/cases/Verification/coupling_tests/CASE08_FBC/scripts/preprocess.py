#!/usr/bin/env python3
"""Generate paired consumption-on/off resolution variants for deposited-firebrand consumption case.

Reads case.json and elmfire.data.in and writes one independent ELMFIRE working
folder per (grid size, consumption state), including deterministic GeoTIFFs and
variants/manifest.json. It does not run ELMFIRE. Arrays use [row, column], rows
increase southward, and physical quantities are SI unless documented otherwise.
"""
import json
import math
import shutil
from pathlib import Path

import numpy as np
import rasterio
from rasterio.transform import from_origin

# -----------------------------------------------------------------------------
# Customizable preprocessing parameters
# -----------------------------------------------------------------------------
CASE_CONFIG_FILENAME = "case.json"
BUFFER_CELLS = 2
PROJECTION_EPSG = 32610
NODATA = -9999.0
FUEL_MODEL = 102
INITIAL_PHI = -1.0
WIND_DIRECTION_DEG = 270.0
WIND_MPH_TO_MPS = 0.44704
BASE_DT_S = 7.0422535
BASE_DTMAX_S = 7.0422535
BASE_TSTOP_S = 260.0
BASE_DTDUMP_S = 260.0
BASE_GR_PER_MW_VEGE = 33.3       # pcs/s/MW for a represented 1 m strip.
BASE_CONSUMPTION = ".FALSE."
BASE_SEED = 5520

CASE_DIR = Path(__file__).resolve().parents[1]


def token(value):
    """Return a compact filesystem-safe number."""
    return f"{float(value):g}".replace(".", "p")


def replace_once(text, old, new, label):
    """Replace one required template assignment, rejecting stale templates."""
    count = text.count(old)
    if count != 1:
        raise RuntimeError(f"expected one {label} template {old!r}, found {count}")
    return text.replace(old, new, 1)


def write_tif(path, array, dx, dtype):
    """Write a north-up raster with the two-cell halo outside the physical domain."""
    transform = from_origin(
        -BUFFER_CELLS * dx,
        (array.shape[0] - BUFFER_CELLS) * dx,
        dx,
        dx,
    )
    with rasterio.open(
        path, "w", driver="GTiff", height=array.shape[0], width=array.shape[1],
        count=1, dtype=np.dtype(dtype).name, crs=f"EPSG:{PROJECTION_EPSG}",
        transform=transform, nodata=NODATA,
    ) as dataset:
        dataset.write(np.asarray(array, dtype=dtype), 1)


def write_inputs(input_dir, nx, ny, dx, wind_mph):
    """Write flat, uniform inputs and a centerline ignition outside the halo."""
    zeros = np.zeros((ny, nx), dtype=np.float32)
    ones = np.ones((ny, nx), dtype=np.float32)
    phi = ones.copy()
    source_row = BUFFER_CELLS + (ny - 2 * BUFFER_CELLS) // 2
    phi[source_row, BUFFER_CELLS] = INITIAL_PHI
    fuel = np.full((ny, nx), FUEL_MODEL, dtype=np.int16)
    rasters = [
        ("asp", zeros, np.float32), ("cbd", zeros, np.float32),
        ("cbh", zeros, np.float32), ("cc", zeros, np.float32),
        ("ch", zeros, np.float32), ("dem", zeros, np.float32),
        ("slp", zeros, np.float32), ("adj", ones, np.float32),
        ("new_phi", phi, np.float32),
        ("new_fbfm40", fuel, np.int16),
        ("ws", np.full((ny, nx), wind_mph, np.float32), np.float32),
        ("wd", np.full((ny, nx), WIND_DIRECTION_DEG, np.float32), np.float32),
        ("m1", zeros, np.float32), ("m10", zeros, np.float32),
        ("m100", zeros, np.float32),
    ]
    for name, array, dtype in rasters:
        write_tif(input_dir / f"{name}.tif", array, dx, dtype)


def main():
    """Create paired metric runs plus active-stock stop-time samples."""
    case = json.loads((CASE_DIR / CASE_CONFIG_FILENAME).read_text(encoding="utf-8"))
    template = (CASE_DIR / "elmfire.data.in").read_text(encoding="utf-8")
    length = float(case["physical_length_m"])
    width = float(case["physical_width_m"])
    wind_mph = float(case["wind_speed_mph"])
    wind_mps = wind_mph * WIND_MPH_TO_MPS
    cfl = float(case["wind_cfl"])
    tstop = float(case["evaluation_time_s"])
    requested_stops_s = [int(round(tstop))] + [
        int(round(float(value))) for value in case["history_times_s"]
    ]
    common_duration_s = requested_stops_s[0]
    for value in requested_stops_s[1:]:
        common_duration_s = math.gcd(common_duration_s, value)
    variants_dir = CASE_DIR / "variants"
    variants_dir.mkdir(exist_ok=True)
    manifest = []

    for dx in map(float, case["grid_sizes_m"]):
        physical_nx = round(length / dx)
        physical_ny = round(width / dx)
        if not np.isclose(
                physical_nx * dx,
                length) or not np.isclose(
                physical_ny * dx,
                width):
            raise ValueError(f"physical dimensions must be divisible by dx={dx:g} m")
        nx = physical_nx + 2 * BUFFER_CELLS
        ny = physical_ny + 2 * BUFFER_CELLS
        # delayed-ignition baseline uses temporal CFL 0.5 based on the 0.71 m/s
        # surface front.
        nominal_dt = cfl * dx / float(case["surface_ros_mps"])
        whole_second_divisors = [
            value for value in range(1, int(math.floor(nominal_dt)) + 1)
            if common_duration_s % value == 0
        ]
        if whole_second_divisors:
            dt = float(max(whole_second_divisors))
        else:
            duration_steps = int(math.ceil(
                common_duration_s / nominal_dt - 1.0e-12
            ))
            dt = common_duration_s / duration_steps
        # The paired runs at the evaluation time determine pass/fail.  Auxiliary
        # consumption-on stop-time runs provide true active-stock samples because
        # ELMFIRE writes EMBER_FLUX only at a run's final dump.  Its transient
        # product is interval deposition and cannot represent the decaying stock.
        variant_specs = [
            ("paired", bool(enabled), tstop)
            for enabled in case["consumption_states"]
        ]
        variant_specs.extend(
            ("history", True, float(stop_time))
            for stop_time in case["history_times_s"]
        )
        for role, enabled, stop_time in variant_specs:
            requested_stop_time_s = stop_time
            step_count = int(round(requested_stop_time_s / dt))
            simulation_tstop_s = step_count * dt
            state = "on" if enabled else "off"
            if role == "paired":
                name = f"dx{token(dx)}_consumption_{state}"
            else:
                name = (
                    f"dx{token(dx)}_consumption_on_"
                    f"t{int(round(stop_time)):04d}"
                )
            vdir = variants_dir / name
            input_dir = vdir / "data/inputs"
            misc_dir = vdir / "data/misc"
            for directory in (
                    input_dir,
                    misc_dir,
                    vdir / "outputs",
                    vdir / "logs/scratch"):
                directory.mkdir(parents=True, exist_ok=True)
            write_inputs(input_dir, nx, ny, dx, wind_mph)
            for table in ("fuel_models.csv", "building_fuel_models.csv"):
                shutil.copyfile(CASE_DIR / "data/misc" / table, misc_dir / table)

            cfg = template
            cfg = replace_once(cfg, f"SIMULATION_DT={BASE_DT_S}",
                               f"SIMULATION_DT={dt:.17g}", "time step")
            # Cap adaptive stepping at the designed CFL; otherwise the comparison
            # would mix consumption error with resolution-dependent time stepping.
            cfg = replace_once(cfg, f"SIMULATION_DTMAX={BASE_DTMAX_S}",
                               f"SIMULATION_DTMAX={dt:.17g}", "maximum time step")
            cfg = replace_once(cfg, f"SIMULATION_TSTOP={BASE_TSTOP_S}",
                               f"SIMULATION_TSTOP={simulation_tstop_s:.17g}", "stop time")
            # Request only the final dump.  A history run's final EMBER_FLUX is
            # the active stock at its predeclared sample time.
            cfg = replace_once(
                cfg,
                f"DTDUMP={BASE_DTDUMP_S}",
                f"DTDUMP={simulation_tstop_s:.17g}",
                "dump time")
            # SPOTTING multiplies the per-MW rate by pixel fire power FLIN*dy.
            # Dividing by dy keeps the represented 1 m strip source invariant.
            gr_per_mw = BASE_GR_PER_MW_VEGE / dx
            cfg = replace_once(
                cfg,
                f"EMBER_GR_PER_MW_VEGE={BASE_GR_PER_MW_VEGE}",
                f"EMBER_GR_PER_MW_VEGE={gr_per_mw:.8g}",
                "vegetation generation rate")
            cfg = replace_once(
                cfg,
                f"USE_EMBER_CONSUMPTION={BASE_CONSUMPTION}",
                f"USE_EMBER_CONSUMPTION={str(bool(enabled)).upper().join(['.','.'])}",
                "consumption switch")
            seed = BASE_SEED + int(dx)
            cfg = replace_once(cfg, f"SEED={BASE_SEED}", f"SEED={seed}", "random seed")
            (vdir / "elmfire.data.in").write_text(cfg, encoding="utf-8")
            manifest.append({
                "name": name, "working_directory": str(vdir.relative_to(CASE_DIR)),
                "config": "elmfire.data.in", "role": role, "dx_m": dx,
                "consumption_enabled": bool(enabled), "seed": seed,
                "nx": nx, "ny": ny, "buffer_cells": BUFFER_CELLS,
                "physical_length_m": length, "physical_width_m": width,
                "nominal_dt_s": nominal_dt,
                "dt_s": dt, "dtmax_s": dt,
                "requested_surface_cfl": cfl,
                "surface_cfl": float(case["surface_ros_mps"]) * dt / dx,
                "wind_speed_mph": wind_mph, "wind_speed_mps": wind_mps,
                "base_gr_1m_pcs_per_s_per_mw": BASE_GR_PER_MW_VEGE,
                "configured_gr_pcs_per_s_per_mw": gr_per_mw,
                "requested_evaluation_time_s": requested_stop_time_s,
                "evaluation_time_s": simulation_tstop_s,
                "output_dump_interval_s": simulation_tstop_s,
                "step_count": step_count,
                "transient_ember_flux": False,
                "expected_outputs": ["final active ember_flux", "time_of_arrival"],
            })

    (variants_dir /
     "manifest.json").write_text(json.dumps(manifest, indent=2) +
                                 "\n", encoding="utf-8")
    print(f"[OK] wrote {len(manifest)} paired and stop-time variants")


if __name__ == "__main__":
    main()
