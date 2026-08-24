# -*- coding: utf-8 -*-
"""
Create iterative folder structure + input GeoTIFFs + per-iteration .data files.

Requirements:
    pip install rasterio pandas numpy

Files expected in ROOT_DIR:
    - parameters.csv
    - daily_weather.txt
    - template.data
"""

import os
import re
import shutil

import numpy as np
import pandas as pd
import rasterio
from rasterio.transform import from_origin


# ----------------------------
# User settings
# ----------------------------
ROOT_DIR = r"/home/nick/elmfire/elmfire/verification/FBP_tests/"   # <-- CHANGE THIS
PARAMETERS_CSV = os.path.join(ROOT_DIR, "parameters.csv")
TEMPLATE_DATA = os.path.join(ROOT_DIR, "template.data")
DAILY_WEATHER = os.path.join(ROOT_DIR, "daily_weather.txt")

N_ITERS = 20  # number of iteration folders to create (01..N_ITERS)

# Raster specs
NROWS, NCOLS = 100, 100
CELLSIZE = 1.0
NODATA = -9999

A_SRS = "+proj=utm +zone=35 +datum=WGS84 +units=m +no_defs"
XLL = 0.0
YLL = 0.0

# Filenames (inputs)
# Note: user wrote "slp:" and "asp:" without .tif; we standardize to *.tif.
INPUT_TIFS = {
    "fbfm13.tif": ("Int16", "fuel"),
    "slp.tif":    ("Int16", "slp"),
    "asp.tif":    ("Int16", "asp"),
    "cc.tif":     ("Int16", None),  # constant 0
    "dem.tif":    ("Float32", None),# derived from slope+aspect
    "ch.tif":     ("Int16", None),  # constant 20
    "m1.tif":     ("Float32", "1h FMC (calculated)"),
    "m10.tif":    ("Float32", "1h FMC (calculated)"),
    "m100.tif":   ("Float32", "1h FMC (calculated)"),
    "ws.tif":     ("Float32", "ws"),
    "wd.tif":     ("Float32", "wd"),
    "adj.tif":    ("Float32", None),# constant 1
    "phi.tif":    ("Float32", None) # constant 1
}


# ----------------------------
# Helpers
# ----------------------------
def ensure_dir(path: str) -> None:
    os.makedirs(path, exist_ok=True)


def rasterio_dtype(dtype_name: str):
    """
    Map human-friendly dtype names to numpy/rasterio-friendly types.
    """
    mapping = {
        "Int16": np.int16,
        "Float32": np.float32,
    }
    if dtype_name not in mapping:
        raise ValueError(f"Unsupported dtype '{dtype_name}'. Add it to mapping.")
    return mapping[dtype_name]


def make_transform(xll: float, yll: float, nrows: int, cellsize: float):
    """
    User provided XLL/YLL as lower-left corner.
    rasterio.from_origin expects upper-left corner.
    """
    y_top = yll + (nrows * cellsize)
    return from_origin(xll, y_top, cellsize, cellsize)


def write_geotiff(path: str, array: np.ndarray, dtype, transform, crs, nodata: float) -> None:
    """
    Writes a single-band GeoTIFF.
    """
    height, width = array.shape
    with rasterio.open(
        path,
        "w",
        driver="GTiff",
        height=height,
        width=width,
        count=1,
        dtype=array.dtype,
        crs=crs,
        transform=transform,
        nodata=nodata,
        compress="lzw"
    ) as dst:
        dst.write(array, 1)


def replace_datafile_values(text: str, key: str, value) -> str:
    """
    Replace a line like:
      KEY = XXX
    with:
      KEY = <value>

    Uses safe backreference syntax (\g<1>) so numeric values don't break replacement.
    """
    pattern = rf"(^\s*{re.escape(key)}\s*=\s*).*$"
    repl = rf"\g<1>{value}"
    new_text, n = re.subn(pattern, repl, text, flags=re.MULTILINE)

    # Optional: fail loud if the key wasn't found (helps catch template mismatches)
    if n == 0:
        raise ValueError(f"Key not found in template.data: {key}")

    return new_text



def build_dem_from_slope_aspect(slp_deg: float, asp_deg: float, cellsize: float) -> np.ndarray:
    """
    Build a DEM whose gradient matches slope/aspect (assuming GIS aspect: clockwise from North).
    For constant slope/aspect, this produces a plane.

    Anchors DEM so min elevation is 0 (as requested).
    """
    slp_rad = np.deg2rad(float(slp_deg))
    asp_rad = np.deg2rad(float(asp_deg))

    g = np.tan(slp_rad)  # gradient magnitude (rise/run)

    # Downslope direction (x east, y north)
    ux = np.sin(asp_rad)
    uy = np.cos(asp_rad)

    dzdx = -g * ux
    dzdy = -g * uy

    # Coordinate grids in meters
    # x increases to the right, y increases downward in array indices, so:
    # We'll define y as +north upward by flipping sign or define y_south positive.
    # Easiest: define y coordinate positive NORTH by using (NROWS-1 - r)
    xs = np.arange(NCOLS, dtype=np.float32) * cellsize
    ys = (np.arange(NROWS, dtype=np.float32)[::-1]) * cellsize  # north-up

    X, Y = np.meshgrid(xs, ys)  # X east, Y north

    dem = dzdx * X + dzdy * Y

    # Anchor min elevation to 0
    dem = dem - dem.min()
    return dem.astype(np.int32)



# ----------------------------
# Main
# ----------------------------
def main():
    # Load parameters
    if not os.path.exists(PARAMETERS_CSV):
        raise FileNotFoundError(f"Missing {PARAMETERS_CSV}")
    if not os.path.exists(TEMPLATE_DATA):
        raise FileNotFoundError(f"Missing {TEMPLATE_DATA}")
    if not os.path.exists(DAILY_WEATHER):
        raise FileNotFoundError(f"Missing {DAILY_WEATHER}")

    df = pd.read_csv(PARAMETERS_CSV, sep=None, engine="python")
    df.columns = df.columns.str.strip()

    required_cols = [
    "fuel", "slp", "asp", "ws", "wd", "tstop", "fmc",
    "DC", "DMC (calculated)", "1h FMC (calculated)"
    ]
    missing = [c for c in required_cols if c not in df.columns]
    if missing:
        raise ValueError(f"Missing columns: {missing}\nFound: {list(df.columns)}")

    if len(df) < N_ITERS:
        raise ValueError(f"parameters.csv has only {len(df)} rows, but N_ITERS={N_ITERS}")

    # Spatial metadata
    transform = make_transform(XLL, YLL, NROWS, CELLSIZE)
    crs = A_SRS  # rasterio accepts PROJ string

    # Read template.data once
    with open(TEMPLATE_DATA, "r", encoding="utf-8") as f:
        template_text = f.read()

    # Create iterations
    for i in range(1, N_ITERS + 1):
        folder_name = f"{i:02d}"
        iter_dir = os.path.join(ROOT_DIR, folder_name)
        inputs_dir = os.path.join(iter_dir, "inputs")
        outputs_dir = os.path.join(iter_dir, "outputs")
        scratch_dir = os.path.join(iter_dir, "scratch")

        ensure_dir(iter_dir)
        ensure_dir(inputs_dir)
        ensure_dir(outputs_dir)
        ensure_dir(scratch_dir)

        # Copy daily_weather.txt into inputs
        shutil.copy2(DAILY_WEATHER, os.path.join(inputs_dir, "daily_weather.txt"))

        # Create per-iteration .data file by copying template and replacing values
        row = df.iloc[i - 1]

        data_text = template_text
        data_text = replace_datafile_values(data_text, "SIMULATION_TSTOP", row["tstop"])
        data_text = replace_datafile_values(data_text, "FOLIAR_MOISTURE_CONTENT", row["fmc"])
        data_text = replace_datafile_values(data_text, "START_DC", row["DC"])
        data_text = replace_datafile_values(data_text, "START_DMC", row["DMC (calculated)"])
        data_text = replace_datafile_values(data_text, "LH_MOISTURE_CONTENT", row["lh"])

        out_data_name = f"{folder_name}.data"
        out_data_path = os.path.join(iter_dir, out_data_name)
        with open(out_data_path, "w", encoding="utf-8") as f:
            f.write(data_text)

        # Prepare frequently used parameter values
        fuel = row["fuel"]
        slp = row["slp"]
        asp = row["asp"]
        fmc1h = row["1h FMC (calculated)"]
        ws = row["ws"]
        wd = row["wd"]

        # Create GeoTIFFs
        for tif_name, (dtype_name, col_name) in INPUT_TIFS.items():
            dtype = rasterio_dtype(dtype_name)

            if tif_name == "cc.tif":
                arr = np.full((NROWS, NCOLS), 60, dtype=dtype)
            elif tif_name == "ch.tif":
                arr = np.full((NROWS, NCOLS), 200, dtype=dtype)
            elif tif_name in ("adj.tif", "phi.tif"):
                arr = np.ones((NROWS, NCOLS), dtype=dtype)
            elif tif_name == "dem.tif":
                arr = build_dem_from_slope_aspect(slp_deg=slp, asp_deg=asp, cellsize=CELLSIZE)
            else:
                # Pull scalar value from row and fill the raster
                if col_name is None:
                    raise RuntimeError(f"{tif_name} has no source column and no special handler.")
                val = row[col_name]
                arr = np.full((NROWS, NCOLS), val, dtype=dtype)

            # Apply nodata: by default we keep everything valid; nodata is just the metadata value.
            # If you'd like some cells to be nodata, you can set arr[...] = NODATA.

            tif_path = os.path.join(inputs_dir, tif_name)
            write_geotiff(
                path=tif_path,
                array=arr,
                dtype=dtype,
                transform=transform,
                crs=crs,
                nodata=NODATA
            )

        print(f"Created iteration {folder_name} in {iter_dir}")

    print("\nDone.")


if __name__ == "__main__":
    main()
