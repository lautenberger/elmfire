#!/usr/bin/env python3
"""Portable generator for ELMFIRE CFFDRS/FBP verification cases.

The source parameter table is the repository's verification/FBP_tests/parameters.csv.
Two input profiles are supported:

  legacy
      Cross-version path for old and new ELMFIRE. BUI comes from START_DMC/DC
      + daily_weather.txt and curing comes from LH_MOISTURE_CONTENT. M1/M10/M100
      are written as fractions and the deck explicitly sets
      DEAD_MC_IN_PERCENT=.FALSE., so the same physical moisture input can be used
      by both implementations.

  dynamic
      Latest implementation only. BUI and GCF are explicit rasters and wind is
      supplied in m/s using WS_IN_MPS=.TRUE. The dynamic inputs are constructed
      to be physically equivalent to the legacy test values:
        BUI = parameters.csv BUI
        GCF = 100 * clip(1.33 - 1.11 * LH_fraction, 0, 1)
        ws_mps = legacy ws raster value [ELMFIRE default mph] * 0.44704

The script has no user-specific absolute paths.
"""

from __future__ import annotations

import argparse
import re
import shutil
from pathlib import Path

import numpy as np
import pandas as pd
import rasterio
from rasterio.transform import from_origin

NROWS = 100
NCOLS = 100
CELLSIZE = 1.0
NODATA = -9999.0
A_SRS = "+proj=utm +zone=35 +datum=WGS84 +units=m +no_defs"
XLL = 0.0
YLL = 0.0
MPH_TO_MPS = 0.44704


def parse_args() -> argparse.Namespace:
    here = Path(__file__).resolve().parent
    p = argparse.ArgumentParser()
    p.add_argument("--root", type=Path, default=here,
                   help="Directory containing parameters.csv, targets.csv and daily_weather.txt")
    p.add_argument("--output-root", type=Path, default=None,
                   help="Case output directory; defaults to --root")
    p.add_argument("--profile", choices=("legacy", "dynamic"), default="legacy")
    p.add_argument("--mode", type=int, choices=(2, 3), default=3)
    p.add_argument("--n-iters", type=int, default=20)
    p.add_argument("--debug", action="store_true", help="Enable DUMP_CFFDRS_DEBUG")
    p.add_argument("--timings", action="store_true", help="Enable ELMFIRE DUMP_TIMINGS")
    p.add_argument("--fuel-model-table", type=Path, default=None,
                   help="Optional explicit fuel_models.csv source")
    p.add_argument("--clean", action="store_true",
                   help="Remove generated case directories before recreating them")
    return p.parse_args()


def set_value(text: str, key: str, value: str | int | float, *, required: bool = True) -> str:
    pattern = rf"(^\s*{re.escape(key)}\s*=\s*).*$"
    repl = rf"\g<1>{value}"
    out, n = re.subn(pattern, repl, text, flags=re.MULTILINE)
    if required and n == 0:
        raise ValueError(f"Key not found in template: {key}")
    return out


def transform() -> rasterio.Affine:
    return from_origin(XLL, YLL + NROWS * CELLSIZE, CELLSIZE, CELLSIZE)


def write_tif(path: Path, value: float, dtype: str) -> None:
    np_dtype = np.dtype(dtype)
    arr = np.full((NROWS, NCOLS), value, dtype=np_dtype)
    with rasterio.open(
        path,
        "w",
        driver="GTiff",
        height=NROWS,
        width=NCOLS,
        count=1,
        dtype=arr.dtype,
        crs=A_SRS,
        transform=transform(),
        nodata=NODATA,
        compress="lzw",
    ) as dst:
        dst.write(arr, 1)


def write_array_tif(path: Path, arr: np.ndarray, dtype: str) -> None:
    arr = np.asarray(arr, dtype=np.dtype(dtype))
    with rasterio.open(
        path,
        "w",
        driver="GTiff",
        height=NROWS,
        width=NCOLS,
        count=1,
        dtype=arr.dtype,
        crs=A_SRS,
        transform=transform(),
        nodata=NODATA,
        compress="lzw",
    ) as dst:
        dst.write(arr, 1)


def build_dem(slope_deg: float, aspect_deg: float) -> np.ndarray:
    """Plane with GIS downslope aspect, clockwise from north."""
    slp = np.deg2rad(float(slope_deg))
    asp = np.deg2rad(float(aspect_deg))
    g = np.tan(slp)
    ux = np.sin(asp)
    uy = np.cos(asp)
    dzdx = -g * ux
    dzdy = -g * uy
    xs = np.arange(NCOLS, dtype=np.float64) * CELLSIZE
    ys = np.arange(NROWS, dtype=np.float64)[::-1] * CELLSIZE
    x, y = np.meshgrid(xs, ys)
    dem = dzdx * x + dzdy * y
    dem -= np.nanmin(dem)
    return dem.astype(np.float32)


def find_fuel_model_table(root: Path, explicit: Path | None) -> Path:
    candidates = []
    if explicit is not None:
        candidates.append(explicit)
    candidates += [
        root / "01" / "inputs" / "fuel_models.csv",
        root / "fuel_models.csv",
        root.parent.parent / "build" / "source" / "fuel_models.csv",
    ]
    for c in candidates:
        if c.is_file():
            return c.resolve()
    raise FileNotFoundError(
        "Could not find fuel_models.csv. Supply --fuel-model-table or retain "
        "the repository verification/FBP_tests/01/inputs/fuel_models.csv."
    )


def template_path(script_dir: Path, profile: str, mode: int) -> Path:
    name = (
        f"elmfire_legacy_mode{mode}.data"
        if profile == "legacy"
        else f"elmfire_latest_dynamic_mode{mode}.data"
    )
    p = script_dir / name
    if not p.is_file():
        raise FileNotFoundError(f"Missing template {p}")
    return p


def main() -> None:
    args = parse_args()
    script_dir = Path(__file__).resolve().parent
    root = args.root.resolve()
    out_root = (args.output_root or root).resolve()
    out_root.mkdir(parents=True, exist_ok=True)

    params_path = root / "parameters.csv"
    targets_path = root / "targets.csv"
    daily_weather = root / "daily_weather.txt"
    for p in (params_path, daily_weather):
        if not p.is_file():
            raise FileNotFoundError(p)

    params = pd.read_csv(params_path, sep=None, engine="python")
    params.columns = params.columns.str.strip()
    required = {
        "fuel", "slp", "asp", "ws", "wd", "tstop", "fmc", "lh",
        "BUI", "DC", "DMC (calculated)", "1h FMC (calculated)"
    }
    missing = sorted(required.difference(params.columns))
    if missing:
        raise ValueError(f"parameters.csv missing columns: {missing}")
    if len(params) < args.n_iters:
        raise ValueError(f"Need {args.n_iters} parameter rows, found {len(params)}")

    fuel_table = find_fuel_model_table(root, args.fuel_model_table)
    template = template_path(script_dir, args.profile, args.mode).read_text()

    manifest_rows = []

    for i in range(1, args.n_iters + 1):
        case = f"{i:02d}"
        case_dir = out_root / case
        if args.clean and case_dir.exists():
            shutil.rmtree(case_dir)
        inputs = case_dir / "inputs"
        outputs = case_dir / "outputs"
        scratch = case_dir / "scratch"
        inputs.mkdir(parents=True, exist_ok=True)
        outputs.mkdir(parents=True, exist_ok=True)
        scratch.mkdir(parents=True, exist_ok=True)

        row = params.iloc[i - 1]
        fuel = int(row["fuel"])
        slope = float(row["slp"])
        aspect = float(row["asp"])
        ws_legacy = float(row["ws"])
        wd = float(row["wd"])
        tstop = float(row["tstop"])
        fmc = float(row["fmc"])
        lh_pct = float(row["lh"])
        dc = float(row["DC"])
        dmc = float(row["DMC (calculated)"])
        bui = float(row["BUI"])
        m1_frac = float(row["1h FMC (calculated)"]) / 100.0
        gcf = 100.0 * np.clip(1.33 - 1.11 * (lh_pct / 100.0), 0.0, 1.0)

        # Static landscape / controls.
        write_tif(inputs / "fbfm13.tif", fuel, "int16")
        write_tif(inputs / "slp.tif", slope, "float32")
        write_tif(inputs / "asp.tif", aspect, "float32")
        write_tif(inputs / "cc.tif", 60, "int16")
        write_tif(inputs / "ch.tif", 200, "int16")
        write_tif(inputs / "adj.tif", 1.0, "float32")
        write_tif(inputs / "phi.tif", 1.0, "float32")
        write_array_tif(inputs / "dem.tif", build_dem(slope, aspect), "float32")

        # Common physical fine-fuel moisture is stored as a fraction. This avoids
        # old/new ambiguity around DEAD_MC_IN_PERCENT defaults.
        write_tif(inputs / "m1.tif", m1_frac, "float32")
        write_tif(inputs / "wd.tif", wd, "float32")

        if args.profile == "legacy":
            write_tif(inputs / "m10.tif", m1_frac, "float32")
            write_tif(inputs / "m100.tif", m1_frac, "float32")
            write_tif(inputs / "ws.tif", ws_legacy, "float32")
            shutil.copy2(daily_weather, inputs / "daily_weather.txt")
        else:
            # The legacy deck has no WS unit flag, so ELMFIRE interprets its raw
            # ws raster as mph. Convert that exact numerical input to m/s to test
            # the new WS_IN_MPS route without changing the physical wind.
            write_tif(inputs / "ws_mps.tif", ws_legacy * MPH_TO_MPS, "float32")
            write_tif(inputs / "bui.tif", bui, "float32")
            write_tif(inputs / "gcf.tif", gcf, "float32")
            shutil.copy2(daily_weather, inputs / "daily_weather.txt")

        shutil.copy2(fuel_table, inputs / "fuel_models.csv")

        data = template
        data = set_value(data, "SIMULATION_TSTOP", f"{tstop:.6g}")
        data = set_value(data, "FOLIAR_MOISTURE_CONTENT", f"{fmc:.6g}")
        data = set_value(data, "DUMP_CFFDRS_DEBUG", ".TRUE." if args.debug else ".FALSE.")
        data = set_value(data, "DUMP_TIMINGS", ".TRUE." if args.timings else ".FALSE.")
        if args.profile == "legacy":
            data = set_value(data, "START_DC", f"{dc:.6g}")
            data = set_value(data, "START_DMC", f"{dmc:.6g}")
            data = set_value(data, "LH_MOISTURE_CONTENT", f"{lh_pct:.6g}")
        (case_dir / f"{case}.data").write_text(data)

        manifest_rows.append({
            "case": case,
            "profile": args.profile,
            "mode": args.mode,
            "fuel": fuel,
            "slope_deg": slope,
            "aspect_deg": aspect,
            "legacy_ws_raw": ws_legacy,
            "dynamic_ws_mps": ws_legacy * MPH_TO_MPS,
            "m1_fraction": m1_frac,
            "BUI": bui,
            "LH_percent": lh_pct,
            "equivalent_GCF_percent": gcf,
            "FMC_percent": fmc,
            "START_DMC": dmc,
            "START_DC": dc,
            "tstop_s": tstop,
        })
        print(f"Created {case_dir} [{args.profile}, MODE={args.mode}]")

    pd.DataFrame(manifest_rows).to_csv(out_root / "case_manifest.csv", index=False)
    shutil.copy2(params_path, out_root / "parameters.csv")
    if targets_path.is_file():
        shutil.copy2(targets_path, out_root / "targets.csv")
    print(f"\nWrote {args.n_iters} cases to {out_root}")


if __name__ == "__main__":
    main()
