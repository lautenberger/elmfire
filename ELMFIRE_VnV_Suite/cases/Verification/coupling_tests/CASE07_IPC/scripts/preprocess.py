#!/usr/bin/env python3
"""Prepare the reproducible combined ensemble-convergence case ensemble.

Reads ``case.json`` and ``elmfire.data.in`` and writes one isolated ELMFIRE
working directory for each response-sweep member and SFT-delay member. The
delay members enable timestep-resolved transient deposition output so actual
first-deposition and ignition times can be compared. The script writes
co-registered GeoTIFF inputs and ``variants/manifest.json``; it does not run
ELMFIRE. Physical quantities use SI units unless the namelist says otherwise.
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
BUFFER_CELLS = 2                 # ELMFIRE numerical halo on every side.
PROJECTION_EPSG = 32610
NODATA = -9999.0
WILDLAND_FUEL_MODEL = 102
INITIAL_PHI = -1.0
WIND_SPEED_MPH = 15.0            # ELMFIRE wind-input unit.
WIND_SPEED_MPS = WIND_SPEED_MPH * 0.44704
WIND_DIRECTION_DEG = 270.0
DEFAULT_WIND_CFL = 0.5
BASE_SIMULATION_DT_S = 0.74515648
BASE_SIMULATION_DTMAX_S = 20.0       # Template value; each variant is capped at dt.
BASE_TARGET_CFL = 0.45
BASE_PIGN_PERCENT = 90.0          # PIGN is a percentage in ELMFIRE.
BASE_SIMULATION_TSTOP_S = 240.0
BASE_RANDOM_SEED = 51400

CASE_DIR = Path(__file__).resolve().parents[1]


def token(value):
    """Return a filesystem-safe numeric token used in variant names."""
    return f"{float(value):g}".replace(".", "p")


def replace_once(text, old, new, label):
    """Replace exactly one namelist assignment or reject a stale template."""
    count = text.count(old)
    if count != 1:
        raise RuntimeError(f"expected one {label} template {old!r}, found {count}")
    return text.replace(old, new, 1)


def set_fixed_pign(config, pign_fraction):
    """Pin the stochastic-spotting PIGN bounds to one designed probability."""
    pign_percent = 100.0 * pign_fraction
    for parameter in ("PIGN_MIN", "PIGN", "PIGN_MAX"):
        config = replace_once(
            config,
            f"{parameter}={BASE_PIGN_PERCENT}",
            f"{parameter}={pign_percent:g}",
            f"{parameter} ignition probability",
        )
    return config


def write_tif(path, array, dx, dtype):
    """Write one north-up raster whose two-cell halo lies outside the domain."""
    # Usable x begins at 0 m. The upper-left origin includes the two-cell halo.
    transform = from_origin(
        -BUFFER_CELLS * dx,
        (array.shape[0] - BUFFER_CELLS) * dx,
        dx,
        dx,
    )
    with rasterio.open(
        path,
        "w",
        driver="GTiff",
        height=array.shape[0],
        width=array.shape[1],
        count=1,
        dtype=np.dtype(dtype).name,
        crs=f"EPSG:{PROJECTION_EPSG}",
        transform=transform,
        nodata=NODATA,
    ) as dataset:
        dataset.write(np.asarray(array, dtype=dtype), 1)


def make_rasters(input_dir, nx, ny, dx):
    """Write deterministic, co-registered terrain, fuel, weather, and PHI inputs."""
    zeros = np.zeros((ny, nx), dtype=np.float32)
    ones = np.ones((ny, nx), dtype=np.float32)
    fuel = np.full((ny, nx), WILDLAND_FUEL_MODEL, dtype=np.int16)
    phi = ones.copy()
    # Put the source on the physical centerline at the first usable column; the
    # two outer rows/columns on every side remain ELMFIRE's numerical buffer.
    source_row = BUFFER_CELLS + (ny - 2 * BUFFER_CELLS) // 2
    phi[source_row, BUFFER_CELLS] = INITIAL_PHI
    rasters = [
        ("asp", zeros, np.float32), ("cbd", zeros, np.float32),
        ("cbh", zeros, np.float32), ("cc", zeros, np.float32),
        ("ch", zeros, np.float32), ("dem", zeros, np.float32),
        ("slp", zeros, np.float32), ("adj", ones, np.float32),
        ("new_phi", phi, np.float32),
        ("new_fbfm40", fuel, np.int16),
        ("ws", np.full((ny, nx), WIND_SPEED_MPH, np.float32), np.float32),
        ("wd", np.full((ny, nx), WIND_DIRECTION_DEG, np.float32), np.float32),
        ("m1", zeros, np.float32), ("m10", zeros, np.float32),
        ("m100", zeros, np.float32),
    ]
    for name, array, dtype in rasters:
        write_tif(input_dir / f"{name}.tif", array, dx, dtype)


def main():
    """Expand the design sweep into reproducibly seeded ensemble members."""
    case = json.loads((CASE_DIR / CASE_CONFIG_FILENAME).read_text(encoding="utf-8"))
    base_input = (CASE_DIR / "elmfire.data.in").read_text(encoding="utf-8")
    members = int(case["ensemble"]["members"])
    base_seed = int(case["ensemble"]["base_seed"])
    if members < 2:
        raise ValueError("ensemble.members must be at least 2 for sample statistics")

    physical_length_m = float(case["physical_length_m"])
    physical_width_m = float(case["physical_width_m"])
    variants_dir = CASE_DIR / "variants"
    variants_dir.mkdir(exist_ok=True)
    manifest = []

    for design in case["variants"]:
        dx = float(design["dx"])
        pign = float(design["pign"])
        cfl = float(design.get("cfl", DEFAULT_WIND_CFL))
        physical_nx = round(physical_length_m / dx)
        physical_ny = round(physical_width_m / dx)
        if not np.isclose(physical_nx * dx, physical_length_m):
            raise ValueError(f"physical length is not divisible by dx={dx:g} m")
        if not np.isclose(physical_ny * dx, physical_width_m):
            raise ValueError(f"physical width is not divisible by dx={dx:g} m")
        nx = physical_nx + 2 * BUFFER_CELLS
        ny = physical_ny + 2 * BUFFER_CELLS
        nominal_dt = cfl * dx / WIND_SPEED_MPS
        requested_tstop_s = float(case["tstop"])
        step_count = int(math.ceil(requested_tstop_s / nominal_dt - 1.0e-12))
        dt = requested_tstop_s / step_count
        simulation_tstop_s = step_count * dt

        for member in range(1, members + 1):
            # A member has the same seed across dx/PIGN, permitting paired
            # comparisons, while members are independent and reproducible.
            seed = base_seed + member - 1
            vname = f"dx{token(dx)}_pign{token(pign)}_r{member:02d}"
            vdir = variants_dir / vname
            input_dir = vdir / "data" / "inputs"
            misc_dir = vdir / "data" / "misc"
            for directory in (
                    input_dir,
                    misc_dir,
                    vdir / "outputs",
                    vdir / "logs/scratch"):
                directory.mkdir(parents=True, exist_ok=True)
            make_rasters(input_dir, nx, ny, dx)
            for table_name in ("fuel_models.csv", "building_fuel_models.csv"):
                shutil.copyfile(
                    CASE_DIR / "data/misc" / table_name,
                    misc_dir / table_name)

            cfg = base_input
            cfg = replace_once(cfg, f"SIMULATION_DT={BASE_SIMULATION_DT_S}",
                               f"SIMULATION_DT={dt:.17g}", "time step")
            # With surface spread disabled, ELMFIREs level-set CFL controller sees
            # negligible velocity and otherwise expands DT to SIMULATION_DTMAX.
            # Capping DTMAX at the wind-CFL step preserves cfl = u_wind*dt/dx.
            cfg = replace_once(cfg, f"SIMULATION_DTMAX={BASE_SIMULATION_DTMAX_S}",
                               f"SIMULATION_DTMAX={dt:.17g}", "maximum time step")
            cfg = replace_once(cfg, f"TARGET_CFL={BASE_TARGET_CFL}",
                               f"TARGET_CFL={min(cfl, 0.95):.6g}", "target CFL")
            cfg = replace_once(
                cfg,
                f"SIMULATION_TSTOP={BASE_SIMULATION_TSTOP_S}",
                f"SIMULATION_TSTOP={simulation_tstop_s:.17g}",
                "stop time")
            cfg = set_fixed_pign(cfg, pign)
            cfg = replace_once(cfg, f"SEED={BASE_RANDOM_SEED}", f"SEED={seed}", "seed")
            (vdir / "elmfire.data.in").write_text(cfg, encoding="utf-8")
            manifest.append({
                "name": vname,
                "role": "probability_response",
                "working_directory": str(vdir.relative_to(CASE_DIR)),
                "config": "elmfire.data.in",
                "dx": dx, "pign": pign, "member": member, "seed": seed,
                "requested_cfl": cfl,
                "cfl": WIND_SPEED_MPS * dt / dx,
                "nominal_dt_s": nominal_dt, "dt_s": dt, "dtmax_s": dt, "nx": nx, "ny": ny,
                "buffer_cells": BUFFER_CELLS,
                "physical_length_m": physical_length_m,
                "physical_width_m": physical_width_m,
                "wind_speed_mps": WIND_SPEED_MPS,
                "requested_tstop_s": requested_tstop_s,
                "tstop_s": simulation_tstop_s,
                "step_count": step_count,
            })

    # ensemble-convergence case also requires direct statistics of the sampled SFT waiting time.
    # These members use the reference P=0.9, tau=10 s configuration at dx=10 m.
    # Transient ember-flux dumps at every wind-CFL step bracket the first
    # deposition time without requiring a synthetic arrival-time estimate.
    delay = case["sft_delay_ensemble"]
    dx = float(delay["dx"])
    pign = float(delay["pign"])
    cfl = DEFAULT_WIND_CFL
    physical_nx = round(physical_length_m / dx)
    physical_ny = round(physical_width_m / dx)
    nx = physical_nx + 2 * BUFFER_CELLS
    ny = physical_ny + 2 * BUFFER_CELLS
    nominal_dt = cfl * dx / WIND_SPEED_MPS
    requested_tstop_s = float(case["tstop"])
    step_count = int(math.ceil(requested_tstop_s / nominal_dt - 1.0e-12))
    dt = requested_tstop_s / step_count
    simulation_tstop_s = step_count * dt
    for member in range(1, int(delay["members"]) + 1):
        seed = base_seed + 1000 + member - 1
        vname = f"sft_dx{token(dx)}_pign{token(pign)}_r{member:02d}"
        vdir = variants_dir / vname
        input_dir = vdir / "data" / "inputs"
        misc_dir = vdir / "data" / "misc"
        for directory in (input_dir, misc_dir, vdir / "outputs", vdir / "logs/scratch"):
            directory.mkdir(parents=True, exist_ok=True)
        make_rasters(input_dir, nx, ny, dx)
        for table_name in ("fuel_models.csv", "building_fuel_models.csv"):
            shutil.copyfile(CASE_DIR / "data/misc" / table_name, misc_dir / table_name)

        cfg = base_input
        cfg = replace_once(cfg, f"SIMULATION_DT={BASE_SIMULATION_DT_S}",
                           f"SIMULATION_DT={dt:.17g}", "time step")
        cfg = replace_once(cfg, f"SIMULATION_DTMAX={BASE_SIMULATION_DTMAX_S}",
                           f"SIMULATION_DTMAX={dt:.17g}", "maximum time step")
        cfg = replace_once(cfg, f"TARGET_CFL={BASE_TARGET_CFL}",
                           f"TARGET_CFL={min(cfl, 0.95):.6g}", "target CFL")
        cfg = replace_once(cfg, f"SIMULATION_TSTOP={BASE_SIMULATION_TSTOP_S}",
                           f"SIMULATION_TSTOP={simulation_tstop_s:.17g}", "stop time")
        cfg = set_fixed_pign(cfg, pign)
        cfg = replace_once(cfg, f"SEED={BASE_RANDOM_SEED}", f"SEED={seed}", "seed")
        cfg = replace_once(cfg, "DTDUMP=20.0", f"DTDUMP={dt:.17g}", "dump interval")
        cfg = replace_once(cfg, "DUMP_EMBER_FLUX_TRANSIENT=.FALSE.",
                           "DUMP_EMBER_FLUX_TRANSIENT=.TRUE.", "transient ember flux")
        cfg = replace_once(cfg, "DUMP_EMBER_IGNITION=.FALSE.",
                           "DUMP_EMBER_IGNITION=.TRUE.", "ember ignition map")
        (vdir / "elmfire.data.in").write_text(cfg, encoding="utf-8")
        manifest.append({
            "name": vname, "role": "sft_delay",
            "working_directory": str(vdir.relative_to(CASE_DIR)),
            "config": "elmfire.data.in",
            "dx": dx, "pign": pign, "member": member, "seed": seed,
            "requested_cfl": cfl,
            "cfl": WIND_SPEED_MPS * dt / dx,
            "nominal_dt_s": nominal_dt, "dt_s": dt, "dtmax_s": dt, "nx": nx, "ny": ny,
            "buffer_cells": BUFFER_CELLS,
            "physical_length_m": physical_length_m,
            "physical_width_m": physical_width_m,
            "wind_speed_mps": WIND_SPEED_MPS,
            "requested_tstop_s": requested_tstop_s,
            "tstop_s": simulation_tstop_s,
            "step_count": step_count,
            "tau_s": float(delay["tau_s"]),
            "transient_dump_interval_s": dt,
        })

    (variants_dir / "manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n", encoding="utf-8"
    )
    response_count = sum(v["role"] == "probability_response" for v in manifest)
    delay_count = sum(v["role"] == "sft_delay" for v in manifest)
    print(f"[OK] wrote {response_count} response and {delay_count} SFT-delay members")


if __name__ == "__main__":
    main()
