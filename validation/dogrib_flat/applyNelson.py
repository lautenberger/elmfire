#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jan 23 16:29:39 2026

@author: nick
"""

import pandas as pd
from pathlib import Path
import subprocess

def read_raws(wxs_path: Path) -> pd.DataFrame:
    df = pd.read_csv(wxs_path, sep=r"\s+", skiprows=3)
    required = ["Year", "Mth", "Day", "Time", "Temp", "RH", "HrlyPcp", "CloudCov"]
    missing = [c for c in required if c not in df.columns]
    if missing:
        raise KeyError(f"{wxs_path} missing columns: {missing}. Found: {list(df.columns)}")

    time_str = df["Time"].astype(str).str.zfill(4)
    hours = time_str.str.slice(0, 2).astype(int)
    mins = time_str.str.slice(2, 4).astype(int)

    dt_series = pd.to_datetime(
        dict(year=df["Year"], month=df["Mth"], day=df["Day"], hour=hours, minute=mins),
        errors="coerce",
        utc=True,
    ).dt.tz_convert("UTC").dt.tz_localize(None)

    if dt_series.isna().any():
        bad = df[dt_series.isna()].head(5)
        raise ValueError(f"Failed to parse some datetimes from rows like:\n{bad}")

    df = df.copy()
    df["datetime"] = dt_series
    for c in ["Temp", "RH", "HrlyPcp", "CloudCov"]:
        df[c] = pd.to_numeric(df[c], errors="coerce")

    return df

def geotiff_to_bsq(input_tif: Path, output_bil: Path):
    cmd = [
        "gdal_translate",
        "--config", "GDAL_CACHEMAX", "512",     # MB
        "-of", "ENVI",
        "-co", "INTERLEAVE=BSQ",
        str(input_tif),
        str(output_bil),
    ]
    subprocess.run(cmd, check=True)

def bsq_to_geotiff(input_bsq: Path, output_tif: Path):
    cmd = [
        "gdal_translate",
        "--config", "GDAL_CACHEMAX", "512",     # MB
        "-of", "GTiff",
        "-co", "COMPRESS=ZSTD",
        "-co", "BIGTIFF=YES",
        "-co", "NUM_THREADS=8",                # try 4 first (not ALL_CPUS)
        str(input_bsq),
        str(output_tif),
    ]
    subprocess.run(cmd, check=True)
    
def delete_bsq_hdr_xml(folder):
    folder = Path(folder)
    for ext in ("*.bsq", "*.hdr", "*.xml"):
        for f in folder.glob(ext):
            f.unlink()
#------------------- INPUT CONSTANTS ---------------------------

FIRE_ROOT = Path(r"~/elmfire/elmfire/validation/dogrib").expanduser()
PREIGNITION_DAYS = 20
RAWS_FILENAME = "dogrib.wxs"
CC_FILENAME = "cc_zip__clip"
DEM_FILENAME = "dem_zip__clip"
SLOPE_FILENAME = 'slp_zip__clip'
ASPECT_FILENAME = "asp_zip__clip"
M1_FILENAME = "m1"
M10_FILENAME = "m10"
M100_FILENAME = "m100"
INPUTS = "inputs/"

workspace_folder = Path("~/elmfire_validation/Data/nelson_csharp/bin/Release/net8.0/nelson_csharp").expanduser()


RAWS_FILE   = FIRE_ROOT / INPUTS / RAWS_FILENAME
CC_FILE     = FIRE_ROOT / INPUTS / CC_FILENAME
DEM_FILE    = FIRE_ROOT / INPUTS / DEM_FILENAME
SLOPE_FILE  = FIRE_ROOT / INPUTS / SLOPE_FILENAME
ASPECT_FILE = FIRE_ROOT / INPUTS / ASPECT_FILENAME
M1_FILE     = FIRE_ROOT / INPUTS / M1_FILENAME
M10_FILE    = FIRE_ROOT / INPUTS / M10_FILENAME
M100_FILE   = FIRE_ROOT / INPUTS / M100_FILENAME
WXS_FILE    = FIRE_ROOT / INPUTS / RAWS_FILENAME

delete_bsq_hdr_xml(FIRE_ROOT / INPUTS)

geotiff_to_bsq(CC_FILE.with_suffix(".tif"), CC_FILE.with_suffix(".bsq"))
geotiff_to_bsq(DEM_FILE.with_suffix(".tif"), DEM_FILE.with_suffix(".bsq"))
geotiff_to_bsq(SLOPE_FILE.with_suffix(".tif"), SLOPE_FILE.with_suffix(".bsq"))
geotiff_to_bsq(ASPECT_FILE.with_suffix(".tif"), ASPECT_FILE.with_suffix(".bsq"))

cmd = [
    str(workspace_folder),
    str(RAWS_FILE),
    str(DEM_FILE.with_suffix(".bsq")),
    str(SLOPE_FILE.with_suffix(".bsq")),
    str(ASPECT_FILE .with_suffix(".bsq")),
    str(CC_FILE.with_suffix(".bsq")),
    str(PREIGNITION_DAYS),
]

result = subprocess.run(
    cmd,
    cwd=str(workspace_folder.parent),
    check=True,   # set True if you want exceptions on nonzero exit
)

bsq_to_geotiff(M1_FILE.with_suffix(".bsq"), M1_FILE.with_suffix(".tif"))
bsq_to_geotiff(M10_FILE.with_suffix(".bsq"), M10_FILE.with_suffix(".tif"))
bsq_to_geotiff(M100_FILE.with_suffix(".bsq"), M100_FILE.with_suffix(".tif"))

delete_bsq_hdr_xml(FIRE_ROOT / INPUTS)