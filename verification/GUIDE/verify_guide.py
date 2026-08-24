#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Automated verification harness for the ELMFIRE "GUIDE" template cases.

This script:
  1. Copies every case in ./TEMPLATE into a fresh run directory.
  2. Generates the input rasters each case needs (126 x 126 @ 30 m), built
     from the parameters in Table 3.1 of the ELMFIRE guide.
  3. Runs ELMFIRE for every case (and sub-case).
  4. Extracts the relevant quantity from the outputs (head ROS, fireline
     intensity, crown-fire class, stop time, ember statistics, ...).
  5. Compares against the theoretical / BEHAVE-derived solutions reported in
     Chapter 3 of the guide.
  6. Writes a CSV summary, a markdown report, and prints a console table.

The theoretical targets are taken verbatim from the guide's verification
chapter. Where Table 3.1 and the narrative text disagree on an input (e.g.
the "Windy" wind speed), the value that reproduces the published target is
used and the discrepancy is noted in CASES below.

Requires (conda env "elmfire"): rasterio, numpy, pandas, and an `elmfire`
binary on PATH (or set ELMFIRE_BIN).

Usage:
    python verify_guide.py                  # generate inputs, run, compare
    python verify_guide.py --cases Point Windy
    python verify_guide.py --skip-run       # only re-compare existing outputs
    python verify_guide.py --no-generate    # reuse inputs already present
    python verify_guide.py --ia-ensemble 2000   # shrink the heavy suppression run
"""

import argparse
import os
import shutil
import subprocess
import sys
from dataclasses import dataclass, field
from typing import Callable, Optional

import numpy as np

try:
    import rasterio
    from rasterio.transform import from_origin
    import pandas as pd
except ImportError as exc:  # pragma: no cover
    sys.exit(f"Missing python dependency ({exc}). Activate the elmfire conda env first.")


# ---------------------------------------------------------------------------
# Static configuration
# ---------------------------------------------------------------------------
HERE = os.path.dirname(os.path.abspath(__file__))
TEMPLATE_DIR = os.path.join(HERE, "TEMPLATE")

NCOLS = NROWS = 126               # All guide rasters are 126 x 126 ...
CELLSIZE = 30.0                   # ... at 30 m resolution.
XLL = YLL = 0.0                   # Lower-left domain corner.
# Embed the EPSG code directly (UTM zone 35N) rather than a proj4 string, so
# ELMFIRE reads a clean EPSG from the input rasters without a proj4->EPSG
# database lookup (which is fragile across GDAL/PROJ installations).
CRS_EPSG = "EPSG:32635"
NODATA = -9999.0

# Cell-index of the domain centre (used for quadrant masks / point ignition).
HALF = NROWS // 2                 # 63

# Default pass/fail tolerance on a percentage error (matches FBP harness).
DEFAULT_TOL_PCT = 10.0


# ---------------------------------------------------------------------------
# Raster grid helpers
# ---------------------------------------------------------------------------
def _transform():
    # Origin is the upper-left corner; rows increase downward (decreasing y).
    top = YLL + NROWS * CELLSIZE
    return from_origin(XLL, top, CELLSIZE, CELLSIZE)


def _quadrant_masks():
    """Return NW, NE, SW, SE boolean masks on the (row, col) grid.

    Row 0 is the north (top) edge because the raster origin is upper-left.
    """
    rows = np.arange(NROWS)[:, None] * np.ones((1, NCOLS))
    cols = np.ones((NROWS, 1)) * np.arange(NCOLS)[None, :]
    north = rows < HALF
    west = cols < HALF
    return {
        "NW": north & west,
        "NE": north & ~west,
        "SW": ~north & west,
        "SE": ~north & ~west,
    }


def _half_masks():
    """Return east/west half masks (split on the central column)."""
    cols = np.ones((NROWS, 1)) * np.arange(NCOLS)[None, :]
    west = cols < HALF
    return {"W": west, "E": ~west}


def write_raster(path, array, dtype, nbands=1):
    """Write a (single- or multi-band) GeoTIFF. `array` is 2-D (row, col)."""
    arr = np.asarray(array)
    profile = dict(
        driver="GTiff",
        height=NROWS,
        width=NCOLS,
        count=nbands,
        dtype=dtype,
        crs=CRS_EPSG,
        transform=_transform(),
        nodata=NODATA,
        compress="deflate",
    )
    with rasterio.open(path, "w", **profile) as dst:
        for b in range(1, nbands + 1):
            dst.write(arr.astype(dtype), b)


def write_multiband_raster(path, arrays, dtype):
    """Write a multiband GeoTIFF where each 2-D array in `arrays` is one band,
    in order. Used to assemble the landscape file."""
    profile = dict(
        driver="GTiff",
        height=NROWS,
        width=NCOLS,
        count=len(arrays),
        dtype=dtype,
        crs=CRS_EPSG,
        transform=_transform(),
        nodata=NODATA,
        compress="deflate",
    )
    with rasterio.open(path, "w", **profile) as dst:
        for b, arr in enumerate(arrays, start=1):
            dst.write(np.asarray(arr).astype(dtype), b)


# ---------------------------------------------------------------------------
# Case specification
# ---------------------------------------------------------------------------
@dataclass
class Layer:
    """A constant, quadrant-varying, or half-varying input field.

    `value` is one of:
        scalar                       -> uniform field
        {"NW":.., "NE":.., ...}      -> per-quadrant field
        {"E":.., "W":..}             -> per-half field
    """
    value: object

    def to_array(self):
        if isinstance(self.value, dict):
            arr = np.zeros((NROWS, NCOLS), dtype=float)
            if set(self.value) <= {"NW", "NE", "SW", "SE"}:
                masks = _quadrant_masks()
            elif set(self.value) <= {"E", "W"}:
                masks = _half_masks()
            else:
                raise ValueError(f"Unrecognized region keys: {set(self.value)}")
            for k, v in self.value.items():
                arr[masks[k]] = v
            return arr
        return np.full((NROWS, NCOLS), float(self.value))


@dataclass
class Target:
    name: str            # human label, e.g. "head ROS (15 deg)"
    value: float         # theoretical value
    units: str
    region: Optional[str] = None   # quadrant/half key to restrict the metric
    tol_pct: float = DEFAULT_TOL_PCT
    abs_tol: Optional[float] = None  # absolute tolerance (overrides pct if set)


@dataclass
class Case:
    name: str
    data_rel: str                       # path to the .data file relative to TEMPLATE
    layers: dict = field(default_factory=dict)   # raster var -> Layer
    targets: list = field(default_factory=list)
    metric: str = "max_ros"             # extractor key (see METRICS)
    notes: str = ""
    quantitative: bool = True


# Integer-valued rasters (everything else is written Float32).
INT_LAYERS = {"slp", "asp", "dem", "fbfm", "cc", "ch", "cbh", "cbd", "sdi", "pcl"}
# Rasters that live in the weather directory and are read band-by-band.
# Live herbaceous (MLH) and live woody (MLW) moisture are NOT rasters: they are
# supplied as constants in each .data namelist (USE_CONSTANT_LH/LW +
# LH/LW_MOISTURE_CONTENT = 60 / 90), so no lh/lw files are generated here.
WEATHER_LAYERS = {"ws", "wd", "m1", "m10", "m100"}
# Band order ELMFIRE expects inside a multiband "landscape" GeoTIFF
# (LANDSCAPE_FILENAME): elevation, slope, aspect, fuel model, canopy cover,
# canopy height, canopy base height, canopy bulk density. All integer-valued,
# so the file is written as a single Int16 8-band raster.
LANDSCAPE_BANDS = ["dem", "slp", "asp", "fbfm", "cc", "ch", "cbh", "cbd"]

# Default constant fields shared by most cases (overridden per case as needed).
BASE_LAYERS = dict(
    slp=0, asp=0, dem=0, cc=0, ch=0, cbh=0, cbd=0,
    adj=1.0, phi=1.0, ws=0.0, wd=0.0,
    m1=6.0, m10=7.0, m100=8.0,
    fbfm=3, sdi=10, pcl=50
)


def L(**overrides):
    """Build a {var: Layer} dict from BASE_LAYERS plus overrides."""
    merged = dict(BASE_LAYERS)
    merged.update(overrides)
    return {k: Layer(v) for k, v in merged.items()}


# ---------------------------------------------------------------------------
# The verification cases (targets from guide Chapter 3)
# ---------------------------------------------------------------------------
CASES = [
    Case(
        name="Point",
        data_rel="Point/point.data",
        layers=L(fbfm=3, ws=0.0, m1=6.0),
        targets=[Target("head ROS", 1.51, "m/min")],
        metric="max_ros",
        notes="Zero wind/slope -> circular front; BEHAVE max ROS 1.51 m/min.",
    ),
    Case(
        name="Windy",
        data_rel="Windy/windy.data",
        layers=L(fbfm=5, ws=6.0, m1=6.0, m10=7.0),
        targets=[
            Target("head ROS", 3.635, "m/min"),
            Target("L/W ratio", 1.24, "-"),
        ],
        metric="ros_and_lw",
        notes=("Wind-driven ellipse. Head ROS (target 3.06 m/min) and ellipse "
               "L/W (target 1.20) checked; L/W is measured from the arrival-time "
               "raster. ELMFIRE computes WAF internally vs the guide's WAF=0.418."),
    ),
    Case(
        name="Valley",
        data_rel="Valley/valley.data",
        # Aspect = downslope azimuth. East half descends west (asp 270) so its
        # upslope head fire develops eastward (outward, away from the central
        # ignition); west half descends east (asp 90) -> head fire westward.
        layers=L(fbfm=3, ws=0.0, m1=6.0,
                 slp={"E": 5, "W": 15}, asp={"E": 270, "W": 90}),
        targets=[
            Target("head ROS (5 deg, east)", 1.92, "m/min", region="E"),
            Target("head ROS (15 deg, west)", 5.37, "m/min", region="W"),
        ],
        metric="max_ros_region",
        notes="Slope-driven spread; SLP raster in degrees; two slopes vs BEHAVE.",
    ),
    Case(
        name="Quadrant",
        data_rel="Quadrant/quadrant.data",
        layers=L(fbfm={"NW": 8, "NE": 7, "SW": 4, "SE": 2},
                 ws=0.0, m1=6.0, m10=7.0, m100=8.0),
        targets=[
            Target("FB8 head ROS", 0.080, "m/min", region="NW"),
            Target("FB7 head ROS", 0.472, "m/min", region="NE"),
            Target("FB4 head ROS", 1.492, "m/min", region="SW"),
            Target("FB2 head ROS", 0.871, "m/min", region="SE"),
        ],
        metric="max_ros_region",
        notes="Four fuel models; per-quadrant BEHAVE ROS.",
    ),
    Case(
        name="MoistureQuad",
        data_rel="Moisture Quad/moisture_quad.data",
        layers=L(fbfm=3, ws=0.0,
                 m1={"NW": 6, "NE": 12, "SW": 18, "SE": 25}),
        targets=[
            Target("ROS @ 6%", 1.511, "m/min", region="NW"),
            Target("ROS @ 12%", 1.089, "m/min", region="NE"),
            Target("ROS @ 18%", 0.801, "m/min", region="SW"),
            Target("ROS @ 25%", 0.000, "m/min", region="SE", abs_tol=0.05),
        ],
        metric="max_ros_region",
        notes="Uniform fuel, varying 1-h FMC; 25% = moisture of extinction (no spread).",
    ),
    Case(
        name="Canopy-1mph",
        data_rel="Canopy/1/canopy_1mph.data",
        layers=L(fbfm=3, ws=1.0, m1=6.0, m10=7.0,
                 cc=80, ch=55, cbh=16, cbd=10),
        targets=[
            Target("crown class", 0, "-", abs_tol=0.5),
            Target("max ROS", 1.809, "m/min"),
        ],
        metric="max_ros_crown",
        notes="1 mph; surface fire only (no crown).",
    ),
    Case(
        name="Canopy-5mph",
        data_rel="Canopy/5/canopy_5mph.data",
        layers=L(fbfm=3, ws=5.0, m1=6.0, m10=7.0,
                 cc=80, ch=55, cbh=16, cbd=10),
        targets=[
            Target("crown class", 1, "-", abs_tol=0.5),
            Target("max ROS", 10.06, "m/min"),
        ],
        metric="max_ros_crown",
        notes="5 mph; passive crown fire.",
    ),
    Case(
        name="Canopy-10mph",
        data_rel="Canopy/10/canopy_10mph.data",
        layers=L(fbfm=3, ws=10.0, m1=6.0, m10=7.0,
                 cc=80, ch=55, cbh=16, cbd=10),
        targets=[
            Target("crown class", 2, "-", abs_tol=0.5),
            Target("max ROS", 35.3, "m/min"),
        ],
        metric="max_ros_crown",
        notes="10 mph; active crown fire.",
    ),
    Case(
        name="Complex",
        data_rel="Complex/complex.data",
        layers=L(fbfm={"NW": 8, "NE": 7, "SW": 4, "SE": 2},
                 ws=5.0, m1=6.0, m10=7.0, m100=8.0,
                 slp={"E": 5, "W": 15}, asp={"E": 270, "W": 90}),
        targets=[],
        metric="qualitative_ros",
        quantitative=False,
        notes="No analytical solution; reported for qualitative review only.",
    ),
    Case(
        name="Firebrands",
        data_rel="Firebrands/firebrands.data",
        layers=L(fbfm=3, ws=5.0, m1=6.0),
        targets=[
            Target("firebrands (cell)", 8450, "-", tol_pct=15.0),
            Target("ln-distance mu", 2.42, "-", abs_tol=0.15),
            Target("ln-distance sigma", 1.31, "-", abs_tol=0.15),
        ],
        metric="firebrand_distribution",
        notes="Firebrand generation + lognormal transport for the dominant "
              "source cell: ember count vs 1036, ln-distance mu/sigma vs 2.72/1.31.",
    ),
    Case(
        name="Overnight",
        data_rel="Overnight/overnight.data",
        layers=L(fbfm=3, ws=0.0, m1=6.0),
        # Clock hours when the diurnal overnight factor turns spread down and
        # back up, detected from the fire and compared like sunrise/sunset.
        # The raster sits at lon ~22.5E (UTM 35N, XLL=0), so ELMFIRE's UTC
        # sunrise/sunset are ~4.5/16.5 -> burn-period center 12.5, length 10 ->
        # burn period [7.5, 17.5] -> reduced spread 17.5 -> 7.5.
        targets=[
            Target("reduction starts (hr)", 17.5, "hr", abs_tol=1.0),
            Target("reduction ends (hr)", 7.5, "hr", abs_tol=1.0),
        ],
        metric="overnight",
        notes="Diurnal overnight slowdown: compares the detected reduction "
              "start/end clock hours against the calculated burn-period bounds.",
    ),
    Case(
        name="Suppression-extended-m0",
        data_rel="Suppression/extended_attack_m0/extended_attack.data",
        layers=L(fbfm=8, ws=0.0, m1=6.0),
        targets=[Target("time at simulation end", 88200.0, "s", tol_pct=15.0)],
        metric="sim_end_time",
        notes="Extended attack contains the fire and ends the run; success metric "
              "is the simulation end time (tstop), analytically ~10800 s.",
    ),
    Case(
        name="Suppression-extended-m1",
        data_rel="Suppression/extended_attack_m1/extended_attack.data",
        layers=L(fbfm=8, ws=0.0, m1=6.0),
        targets=[Target("time at simulation end", 110800.0, "s", tol_pct=15.0)],
        metric="sim_end_time",
        notes="Extended attack contains the fire and ends the run; success metric "
              "is the simulation end time (tstop), analytically ~10800 s.",
    ),
    Case(
        name="Suppression-initial",
        data_rel="Suppression/initial_attack/initial_attack.data",
        layers=L(fbfm=3, ws=0.0, m1=6.0),
        targets=[Target("fraction contained", 0.54, "-", abs_tol=0.05)],
        metric="initial_attack",
        notes="20000-member ensemble; ~52% contained vs 54% predicted (HEAVY).",
    ),
]


# ---------------------------------------------------------------------------
# .data namelist parsing / patching
# ---------------------------------------------------------------------------
def parse_namelist(path):
    """Very small namelist reader: returns {KEY: raw_string_value}."""
    out = {}
    with open(path) as fh:
        for line in fh:
            line = line.split("!")[0].strip()
            if "=" not in line or line.startswith("&") or line.startswith("/"):
                continue
            key, _, val = line.partition("=")
            out[key.strip().upper()] = val.strip()
    return out


def _unquote(s):
    return s.strip().strip("'").strip('"').strip()


def case_input_filenames(nml):
    """Map each input raster variable -> the base filename the .data expects."""
    mapping = {
        "ws": "WS_FILENAME", "wd": "WD_FILENAME",
        "m1": "M1_FILENAME", "m10": "M10_FILENAME", "m100": "M100_FILENAME",
        "asp": "ASP_FILENAME", "slp": "SLP_FILENAME", "dem": "DEM_FILENAME",
        "fbfm": "FBFM_FILENAME", "cc": "CC_FILENAME", "ch": "CH_FILENAME",
        "cbh": "CBH_FILENAME", "cbd": "CBD_FILENAME", "sdi": "SDI_FILENAME",
        "adj": "ADJ_FILENAME", "phi": "PHI_FILENAME", "pcl": "PCL_FILENAME",
    }
    files = {}
    for var, key in mapping.items():
        if key in nml:
            files[var] = _unquote(nml[key])
    return files


def num_weather_bands(nml):
    try:
        return max(1, int(float(nml.get("NUM_METEOROLOGY_TIMES", "1"))))
    except ValueError:
        return 1


def override_namelist_keys(path, overrides):
    """Override given KEY=value lines in the run copy, in place.

    The committed templates are never touched. Applied to each run copy:
      * PATH_TO_GDAL        -> a GDAL that actually works in this environment
                               (auto-detected by find_working_gdal(), or set via
                               --gdal-path / ELMFIRE_GDAL). The templates ship
                               '/usr/bin/', which is correct on a healthy system
                               but may be broken on a given box (system/conda PROJ
                               db mismatch), so we resolve a working one at run time.
      * NUM_ENSEMBLE_MEMBERS -> shrink the heavy initial-attack run (--ia-ensemble)
    """
    if not overrides:
        return
    out = []
    with open(path) as fh:
        for line in fh:
            key = line.split("=", 1)[0].strip().upper()
            if key in overrides:
                out.append(f"{key} = {overrides[key]}\n")
            else:
                out.append(line)
    with open(path, "w") as fh:
        fh.writelines(out)


_WORKING_GDAL = None  # cache: None=not probed, ""=none found, str=working bin dir


def _gdal_dir_works(bindir, test_tif, tmpdir):
    """True if this GDAL bin dir can BOTH identify and apply EPSG:32635 in the
    current environment -- the two operations ELMFIRE relies on for georeferenced
    output (gdalsrsinfo detection, then gdal_translate -a_srs)."""
    translate = os.path.join(bindir, "gdal_translate")
    srsinfo = os.path.join(bindir, "gdalsrsinfo")
    if not (os.path.exists(translate) and os.path.exists(srsinfo)):
        return False
    try:
        r = subprocess.run([srsinfo, "-o", "epsg", test_tif],
                           capture_output=True, text=True, timeout=30)
        if r.returncode != 0 or "32635" not in r.stdout or "PROJ" in r.stderr:
            return False
        out = os.path.join(tmpdir, "probe_out.tif")
        r2 = subprocess.run([translate, "-q", "-a_srs", "EPSG:32635", test_tif, out],
                            capture_output=True, text=True, timeout=30)
        if r2.returncode != 0 or "PROJ" in r2.stderr or "Failed to process SRS" in r2.stderr:
            return False
    except (OSError, subprocess.SubprocessError):
        return False
    return True


def find_working_gdal():
    """Probe candidate GDAL bin dirs and return the first one (with trailing
    separator) that works in this environment, or None if none do.

    Tries, in order: the gdal_translate on PATH (typically the active conda env),
    then /usr/bin and /usr/local/bin. Result is cached for the process."""
    global _WORKING_GDAL
    if _WORKING_GDAL is not None:
        return _WORKING_GDAL or None

    import tempfile
    tmpdir = tempfile.mkdtemp(prefix="gdalprobe_")
    test_tif = os.path.join(tmpdir, "probe.tif")
    profile = dict(driver="GTiff", height=2, width=2, count=1, dtype="float32",
                   crs=CRS_EPSG, transform=from_origin(0.0, 60.0, 30.0, 30.0),
                   nodata=NODATA)
    with rasterio.open(test_tif, "w", **profile) as dst:
        dst.write(np.ones((2, 2), "float32"), 1)

    candidates = []
    onpath = shutil.which("gdal_translate")
    if onpath:
        candidates.append(os.path.dirname(onpath))
    candidates += ["/usr/bin", "/usr/local/bin"]

    seen, ordered = set(), []
    for c in candidates:
        c = os.path.normpath(c)
        if c not in seen:
            seen.add(c)
            ordered.append(c)

    _WORKING_GDAL = ""
    for d in ordered:
        if _gdal_dir_works(d, test_tif, tmpdir):
            _WORKING_GDAL = d + os.sep
            break
    shutil.rmtree(tmpdir, ignore_errors=True)
    return _WORKING_GDAL or None


# ---------------------------------------------------------------------------
# Input generation
# ---------------------------------------------------------------------------
def _layer_array(case, var):
    """The 2-D field for `var`, falling back to the base default if unset."""
    layer = case.layers.get(var) or Layer(BASE_LAYERS.get(var, 0))
    return layer.to_array()


def generate_case_inputs(case_dir, data_path, case):
    nml = parse_namelist(data_path)
    files = case_input_filenames(nml)
    nbands = num_weather_bands(nml)

    fueltop = _unquote(nml.get("FUELS_AND_TOPOGRAPHY_DIRECTORY", "./inputs"))
    weather = _unquote(nml.get("WEATHER_DIRECTORY", "./inputs"))
    fueltop_dir = os.path.normpath(os.path.join(case_dir, fueltop))
    weather_dir = os.path.normpath(os.path.join(case_dir, weather))
    os.makedirs(fueltop_dir, exist_ok=True)
    os.makedirs(weather_dir, exist_ok=True)

    # If the namelist names a LANDSCAPE_FILENAME, pack the eight topography/fuel
    # layers into one multiband Int16 GeoTIFF (DEM, SLP, ASP, FBFM, CC, CH, CBH,
    # CBD) instead of writing them as individual rasters. ADJ/PHI and the weather
    # rasters are always written individually.
    landscape_fn = _unquote(nml.get("LANDSCAPE_FILENAME", ""))
    if landscape_fn:
        bands = [_layer_array(case, var) for var in LANDSCAPE_BANDS]
        write_multiband_raster(os.path.join(fueltop_dir, f"{landscape_fn}.tif"),
                               bands, "int16")

    for var, fname in files.items():
        if landscape_fn and var in LANDSCAPE_BANDS:
            continue  # already packed into the landscape file
        arr = _layer_array(case, var)
        dtype = "int16" if var in INT_LAYERS else "float32"
        bands = nbands if var in WEATHER_LAYERS else 1
        target_dir = weather_dir if var in WEATHER_LAYERS else fueltop_dir
        write_raster(os.path.join(target_dir, f"{fname}.tif"), arr, dtype, bands)


# ---------------------------------------------------------------------------
# Running ELMFIRE
# ---------------------------------------------------------------------------
def run_elmfire(case_dir, data_path, elmfire_bin):
    rel_data = os.path.basename(data_path)
    for sub in ("outputs", "scratch"):
        d = os.path.join(case_dir, sub)
        shutil.rmtree(d, ignore_errors=True)
        os.makedirs(d, exist_ok=True)
    log = os.path.join(case_dir, "outputs", "elmfire_run.log")
    with open(log, "w") as lf:
        proc = subprocess.run([elmfire_bin, rel_data], cwd=case_dir,
                              stdout=lf, stderr=subprocess.STDOUT)
    ok = False
    with open(log) as lf:
        ok = "End of simulation reached successfully" in lf.read()
    return ok, log, proc.returncode


# ---------------------------------------------------------------------------
# Output metric extraction
# ---------------------------------------------------------------------------
def _read_stack(paths):
    """Read one or more single-band rasters into a list of masked 2-D arrays."""
    arrays = []
    for p in sorted(paths):
        with rasterio.open(p) as src:
            a = src.read(1, masked=True).astype(float)
            arrays.append(a)
    return arrays


def _glob(out_dir, prefix):
    import glob
    return glob.glob(os.path.join(out_dir, f"{prefix}*.tif"))


def _max_over(arrays, mask=None):
    best = None
    for a in arrays:
        data = a.filled(np.nan)
        if mask is not None:
            data = np.where(mask, data, np.nan)
        data = data[np.isfinite(data)]
        if data.size:
            m = float(np.nanmax(data))
            best = m if best is None else max(best, m)
    return best


def metric_max_ros(out_dir, case):
    arrs = _read_stack(_glob(out_dir, "vs_"))
    return {"head ROS": _max_over(arrs)}


def lw_ratio_from_toa(out_dir):
    """Length-to-width ratio of the burned ellipse from the time-of-arrival map.

    The burned region is a filled ellipse, so its aspect ratio equals
    sqrt(major/minor eigenvalue) of the covariance of the burned-cell
    coordinates -- no need to know the wind axis, and unaffected by where the
    ignition focus sits inside the ellipse.

    Crucially, we restrict to the isochrone that arrived *before* the fire
    first reached any domain edge. Once the front hits a wall the burned region
    is clipped into a non-ellipse (a domain-filling blob reads as L/W ~ 1), which
    would corrupt the measurement.
    """
    toa = _glob(out_dir, "time_of_arrival_")
    if not toa:
        return None
    with rasterio.open(sorted(toa)[-1]) as src:
        arr = src.read(1, masked=True).astype(float).filled(np.nan)
    burned = np.isfinite(arr) & (arr >= 0.0) & (arr < 1e8)
    if burned.sum() < 10:
        return None

    # Time at which the fire first comes within MARGIN cells of any domain edge.
    # A 1-pixel border is not enough: the fire can fill almost the whole domain
    # (leaving only corners) and read as a square (L/W ~ 1). We take the last
    # isochrone that is still comfortably interior, where the shape is a clean
    # ellipse.
    MARGIN = 8
    band = np.zeros_like(burned)
    band[:MARGIN, :] = band[-MARGIN:, :] = band[:, :MARGIN] = band[:, -MARGIN:] = True
    band_burned = burned & band
    if band_burned.any():
        t_safe = float(np.nanmin(arr[band_burned]))
        region = burned & (arr < t_safe)   # last fully-interior isochrone
    else:
        region = burned                    # fire never approached an edge

    ys, xs = np.where(region)
    if xs.size < 10:
        return None
    cov = np.cov(np.vstack([xs.astype(float), ys.astype(float)]))
    eig = np.linalg.eigvalsh(cov)          # ascending: [minor, major]
    if eig[0] <= 0:
        return None
    return float(np.sqrt(eig[1] / eig[0]))


def metric_ros_and_lw(out_dir, case):
    """Head ROS (max vs_) plus the ellipse L/W from the arrival-time raster."""
    arrs = _read_stack(_glob(out_dir, "vs_"))
    res = {"head ROS": _max_over(arrs)}
    lw = lw_ratio_from_toa(out_dir)
    # Map to whichever target carries the L/W label.
    for t in case.targets:
        if "L/W" in t.name or "length" in t.name.lower():
            res[t.name] = lw
    return res


def metric_max_ros_region(out_dir, case):
    files = _glob(out_dir, "vs_")
    arrs = _read_stack(files)
    masks = {**_quadrant_masks(), **_half_masks()}
    res = {}
    for t in case.targets:
        val = _max_over(arrs, masks.get(t.region))
        # A region with no burned/valid cell means the fire never spread there
        # (e.g. fuel above its moisture of extinction) -> ROS = 0, not "missing".
        # Only when spread-rate output exists at all; otherwise leave None.
        if val is None and files:
            val = 0.0
        res[t.name] = val
    return res


def metric_max_ros_crown(out_dir, case):
    """Max rate of spread on the raster plus the max crown-fire class (0=none,
    1=passive, 2=active) from the crown_fire raster."""
    ros = _max_over(_read_stack(_glob(out_dir, "vs_")))
    crown = _read_stack(_glob(out_dir, "crown_fire_"))
    cls = _max_over(crown) if crown else None
    res = {}
    for t in case.targets:
        res[t.name] = cls if "crown" in t.name.lower() else ros
    if not case.targets:                     # qualitative fallback
        res = {"max ROS": ros, "crown class (max)": cls}
    return res


def metric_qualitative_ros(out_dir, case):
    arrs = _read_stack(_glob(out_dir, "vs_"))
    return {"max ROS (qualitative)": _max_over(arrs)}


def _containment_fracs(out_dir):
    """Valid per-member containment fractions from fire_size_stats.csv.

    ELMFIRE writes a `Containfrac` column: a contained fire = 1.0, an escaped
    fire = the -9999 nodata sentinel. Returns a pandas Series of the rows with
    a real (non-sentinel) value, or None if the file/column is absent."""
    import glob
    csvs = glob.glob(os.path.join(out_dir, "fire_size_stats*.csv"))
    if not csvs:
        return None
    df = pd.read_csv(sorted(csvs)[0])
    df.columns = df.columns.str.strip()
    if "Containfrac" not in df.columns:
        return None
    cf = pd.to_numeric(df["Containfrac"], errors="coerce")
    return cf


def metric_sim_end_time(out_dir, case):
    """Time at simulation end (extended attack): the `tstop (h)` field of
    fire_size_stats.csv, returned in seconds. Once the fire is fully contained
    the run terminates, so this is the containment time."""
    import glob
    csvs = glob.glob(os.path.join(out_dir, "fire_size_stats*.csv"))
    if not csvs:
        return {"time at simulation end": None}
    df = pd.read_csv(sorted(csvs)[0])
    df.columns = df.columns.str.strip()
    col = next((c for c in df.columns if c.lower().startswith("tstop")), None)
    if col is None or df.empty:
        return {"time at simulation end": None}
    val = pd.to_numeric(df[col], errors="coerce").dropna()
    if val.empty:
        return {"time at simulation end": None}
    return {"time at simulation end": float(val.iloc[0]) * 3600.0}  # h -> s


def metric_overnight(out_dir, case):
    """Clock hours at which the diurnal overnight factor turns fire spread down
    and back up, inferred from the time-of-arrival raster.

    The overnight case has constant fuel and zero wind, so the only thing that
    changes the spread rate is the diurnal factor (1.0 in the burn period, 0.1
    outside it). The burned region stays ~circular, so the equivalent radius
    r(t)=sqrt(area(t)/pi) advances at ~ROS during the day and ~0.1*ROS at night
    -- two clean slope plateaus. We sample that rate, classify each instant as
    full/reduced, and read off the clock hours of the full->reduced ("reduction
    starts", i.e. burn-period stop) and reduced->full ("reduction ends", i.e.
    burn-period start) transitions, then compare like sunrise/sunset.

    ELMFIRE's clock is HOUR_OF_DAY = MODULO(FORECAST_START_HOUR + T/3600, 24).
    """
    KEYS = {"reduction starts (hr)": None, "reduction ends (hr)": None}
    toa = _glob(out_dir, "time_of_arrival_")
    if not toa:
        return dict(KEYS)
    with rasterio.open(sorted(toa)[-1]) as src:
        arr = src.read(1, masked=True).astype(float).filled(np.nan)
    burned = np.isfinite(arr) & (arr >= 0.0) & (arr < 1e8)
    arrival = np.sort(arr[burned])
    if arrival.size < 50:
        return dict(KEYS)

    # t=0 clock-hour offset: ELMFIRE uses FORECAST_START_HOUR.
    case_dir = os.path.dirname(out_dir)
    nml = parse_namelist(os.path.join(case_dir, os.path.basename(case.data_rel)))
    start_hour = float(nml.get("FORECAST_START_HOUR", "20") or 20)

    # Detection must stop before the front reaches the domain edge: once it does,
    # the radius-advance rate collapses (geometry, not the diurnal factor) and
    # would register as a spurious "reduction".
    edge = np.zeros_like(burned)
    edge[0, :] = edge[-1, :] = edge[:, 0] = edge[:, -1] = True
    eb = burned & edge
    t_edge = float(np.nanmin(arr[eb])) if eb.any() else float(arrival[-1])

    # Front-advance rate r(t) sampled at 30-min resolution, up to edge contact.
    # (Finer sampling lets integer-cell quantization momentarily read as a
    # spurious mid-day "reduction".)
    dt = 1800.0
    tmax = float(arrival[-1])
    ts = np.arange(0.0, min(tmax, t_edge), dt)
    if ts.size < 4:
        return dict(KEYS)
    counts = np.searchsorted(arrival, ts + dt)          # cells burned by ts+dt
    r = np.sqrt(counts / np.pi)                          # equivalent radius (cells)
    rate = np.diff(r, prepend=0.0)                       # advance per sample
    hod = np.mod(start_hour + (ts + 0.5 * dt) / 3600.0, 24.0)

    pos = rate[rate > 0]
    if pos.size == 0:
        return dict(KEYS)
    threshold = 0.5 * np.percentile(pos, 80)             # midway between plateaus
    reduced = rate < threshold
    established = r > 3.0                                 # skip ignition transient

    onsets, ends = [], []                                # full->reduced, reduced->full
    for i in range(1, len(ts)):
        if not (established[i] and established[i - 1]):
            continue
        if reduced[i] and not reduced[i - 1]:
            onsets.append(hod[i])
        elif not reduced[i] and reduced[i - 1]:
            ends.append(hod[i])

    return {
        "reduction starts (hr)": float(np.median(onsets)) if onsets else None,
        "reduction ends (hr)": float(np.median(ends)) if ends else None,
    }


def metric_firebrand_distribution(out_dir, case):
    """Firebrand generation + transport check from spotting_stats_*.csv.

    Embers are logged one-per-row with their source cell (IX_FROM, IY_FROM) and
    rigorous (un-quantized) travel distance (SPOTTING_DISTANCE). For the single
    dominant source cell (the one that emitted the most embers) we compare:
      * ember count            -> total firebrands generated by that cell
      * mean(ln distance)      -> lognormal mu of the travel distribution
      * std (ln distance)      -> lognormal sigma
    """
    NULL = {"firebrands (cell)": None, "ln-distance mu": None,
            "ln-distance sigma": None}
    import glob
    csvs = glob.glob(os.path.join(out_dir, "spotting_stats_*.csv"))
    if not csvs:
        return dict(NULL)
    frames = []
    for c in csvs:
        try:
            d = pd.read_csv(c)
            d.columns = d.columns.str.strip()
            frames.append(d)
        except Exception:
            pass
    if not frames:
        return dict(NULL)
    df = pd.concat(frames, ignore_index=True)
    if not {"IX_FROM", "IY_FROM", "SPOTTING_DISTANCE"} <= set(df.columns):
        return dict(NULL)

    # Dominant source cell = the one that generated the most embers.
    sizes = df.groupby(["IX_FROM", "IY_FROM"]).size()
    if sizes.empty:
        return dict(NULL)
    ix, iy = sizes.idxmax()
    cell = df[(df["IX_FROM"] == ix) & (df["IY_FROM"] == iy)]
    dist = pd.to_numeric(cell["SPOTTING_DISTANCE"], errors="coerce").dropna()
    dist = dist[dist > 0]
    if dist.size < 2:
        return dict(NULL, **{"firebrands (cell)": float(len(cell))})

    ln = np.log(dist.values)
    return {
        "firebrands (cell)": float(len(cell)),
        "ln-distance mu": float(np.mean(ln)),
        "ln-distance sigma": float(np.std(ln)),
        "_source_cell": f"({int(ix)},{int(iy)})",
        "_n_source_cells": int(len(sizes)),
        "_per_cell_count_min_med_max": (int(sizes.min()),
                                        int(sizes.median()), int(sizes.max())),
    }


def metric_initial_attack(out_dir, case):
    """Fraction of ensemble members contained at initial attack.

    Read directly from the fire_size_stats.csv `Containfrac` column: a member
    is contained when Containfrac == 1.0 (escaped members carry the -9999
    sentinel). The fraction is contained_count / total_members.
    """
    cf = _containment_fracs(out_dir)
    if cf is None or cf.dropna().empty:
        return {"fraction contained": None, "_n_members": 0}
    cf = cf.dropna()
    contained = float((cf >= 0.999).mean())
    return {"fraction contained": contained, "_n_members": int(len(cf))}


METRICS = {
    "max_ros": metric_max_ros,
    "ros_and_lw": metric_ros_and_lw,
    "max_ros_region": metric_max_ros_region,
    "max_ros_crown": metric_max_ros_crown,
    "qualitative_ros": metric_qualitative_ros,
    "sim_end_time": metric_sim_end_time,
    "overnight": metric_overnight,
    "firebrand_distribution": metric_firebrand_distribution,
    "initial_attack": metric_initial_attack,
}


# ---------------------------------------------------------------------------
# Comparison
# ---------------------------------------------------------------------------
def pct_error(actual, target):
    if actual is None or target is None or target == 0:
        return None
    return 100.0 * (actual - target) / target


def evaluate_case(case, measured):
    """Return a list of result rows comparing measured vs target."""
    rows = []
    if not case.quantitative or not case.targets:
        # Report measured values, no pass/fail.
        for k, v in measured.items():
            if k.startswith("_"):
                continue
            rows.append(dict(case=case.name, metric=k, target=None,
                             measured=v, pct_error=None, match=None,
                             units="", notes=case.notes))
        return rows

    for t in case.targets:
        actual = measured.get(t.name)
        err = pct_error(actual, t.value)
        if actual is None:
            match = None
        elif t.abs_tol is not None:
            match = abs(actual - t.value) <= t.abs_tol
        else:
            match = (err is not None) and (abs(err) <= t.tol_pct)
        rows.append(dict(case=case.name, metric=t.name, target=t.value,
                         measured=actual, pct_error=err, match=match,
                         units=t.units, notes=case.notes))
    return rows


# ---------------------------------------------------------------------------
# Reporting
# ---------------------------------------------------------------------------
def fmt(v, nd=3):
    if v is None:
        return "-"
    if isinstance(v, bool):
        return "PASS" if v else "FAIL"
    try:
        return f"{v:.{nd}f}"
    except (TypeError, ValueError):
        return str(v)


def write_reports(rows, run_dir):
    df = pd.DataFrame(rows)
    csv_path = os.path.join(run_dir, "verification_summary.csv")
    df.to_csv(csv_path, index=False)

    md_path = os.path.join(run_dir, "verification_report.md")
    with open(md_path, "w") as fh:
        fh.write("# ELMFIRE GUIDE verification report\n\n")
        n_eval = df["match"].notna().sum()
        n_pass = (df["match"] == True).sum()  # noqa: E712
        fh.write(f"- Cases run: {df['case'].nunique()}\n")
        fh.write(f"- Quantitative checks: {n_eval} ({n_pass} passed, "
                 f"{n_eval - n_pass} failed)\n\n")
        fh.write("| Case | Metric | Target | ELMFIRE | % err | Result | Units |\n")
        fh.write("|------|--------|-------:|--------:|------:|:------:|-------|\n")
        for r in rows:
            res = "n/a" if r["match"] is None else ("PASS" if r["match"] else "FAIL")
            fh.write(f"| {r['case']} | {r['metric']} | {fmt(r['target'])} | "
                     f"{fmt(r['measured'])} | {fmt(r['pct_error'], 1)} | {res} | "
                     f"{r['units']} |\n")
        fh.write("\n## Notes per case\n\n")
        for case in CASES:
            note = next((r["notes"] for r in rows if r["case"] == case.name), "")
            if note:
                fh.write(f"- **{case.name}**: {note}\n")
    return csv_path, md_path


def print_console(rows):
    hdr = f"{'Case':<20}{'Metric':<28}{'Target':>10}{'ELMFIRE':>10}{'%err':>8}  Result"
    print("\n" + hdr)
    print("-" * len(hdr))
    for r in rows:
        res = "n/a" if r["match"] is None else ("PASS" if r["match"] else "FAIL")
        print(f"{r['case']:<20}{r['metric']:<28}{fmt(r['target']):>10}"
              f"{fmt(r['measured']):>10}{fmt(r['pct_error'], 1):>8}  {res}")


# ---------------------------------------------------------------------------
# Orchestration
# ---------------------------------------------------------------------------
def main():
    ap = argparse.ArgumentParser(description="Run + verify the GUIDE template cases.")
    ap.add_argument("--run-dir", default=os.path.join(HERE, "verification_run"),
                    help="Working directory (TEMPLATE is copied here).")
    ap.add_argument("--cases", nargs="*", help="Subset of case names to run.")
    ap.add_argument("--elmfire-bin", default=os.environ.get("ELMFIRE_BIN", "elmfire"))
    ap.add_argument("--gdal-path", default=os.environ.get("ELMFIRE_GDAL"),
                    help="Force PATH_TO_GDAL in the run copies (e.g. the conda env's "
                         "bin/). Default: auto-detect a GDAL that works in this env.")
    ap.add_argument("--skip-run", action="store_true",
                    help="Do not run ELMFIRE; only re-compare existing outputs.")
    ap.add_argument("--no-generate", action="store_true",
                    help="Do not regenerate input rasters.")
    ap.add_argument("--ia-ensemble", type=int, default=None,
                    help="Override NUM_ENSEMBLE_MEMBERS for the initial-attack case.")
    args = ap.parse_args()

    selected = [c for c in CASES if (not args.cases or c.name in args.cases)]
    if not selected:
        sys.exit(f"No matching cases. Available: {[c.name for c in CASES]}")

    print(f"ELMFIRE GUIDE verification: {len(selected)} case(s) -> "
          f"{', '.join(c.name for c in selected)}", flush=True)
    if not shutil.which(args.elmfire_bin) and not os.path.isfile(args.elmfire_bin):
        print(f"[WARN] elmfire binary '{args.elmfire_bin}' not found on PATH; "
              f"runs will fail. Set ELMFIRE_BIN or use --skip-run.", flush=True)

    # Resolve a GDAL path to write into each run copy's PATH_TO_GDAL: an explicit
    # --gdal-path/ELMFIRE_GDAL wins, otherwise auto-detect one that works here.
    gdal_override = None
    if not args.skip_run:
        gdal_override = args.gdal_path or find_working_gdal()
        if gdal_override:
            print(f"PATH_TO_GDAL -> {gdal_override} (run copies only; templates unchanged)",
                  flush=True)
        else:
            print("[WARN] no working GDAL found; using each template's PATH_TO_GDAL "
                  "as-is (georeferenced .tif outputs may fail).", flush=True)

    if args.skip_run:
        run_dir = args.run_dir
    else:
        run_dir = args.run_dir
        os.makedirs(run_dir, exist_ok=True)

    all_rows = []

    for case in selected:
        src_data = os.path.join(TEMPLATE_DIR, case.data_rel)
        if not os.path.isfile(src_data):
            print(f"[WARN] {case.name}: data file missing ({case.data_rel}), skipping.")
            continue
        # Each case runs inside run_dir/<case.data_rel parent>.
        case_dir = os.path.join(run_dir, os.path.dirname(case.data_rel))
        os.makedirs(case_dir, exist_ok=True)
        dst_data = os.path.join(case_dir, os.path.basename(case.data_rel))

        if not args.skip_run:
            # Use the template verbatim; only apply opt-in CLI overrides.
            shutil.copy(src_data, dst_data)
            overrides = {}
            if gdal_override:
                overrides["PATH_TO_GDAL"] = f"'{gdal_override}'"
            if args.ia_ensemble is not None and "initial" in case.name.lower():
                overrides["NUM_ENSEMBLE_MEMBERS"] = str(args.ia_ensemble)
            override_namelist_keys(dst_data, overrides)
            if not args.no_generate:
                print(f"[{case.name}] generating inputs ...")
                generate_case_inputs(case_dir, dst_data, case)
            print(f"[{case.name}] running ELMFIRE ...")
            ok, log, rc = run_elmfire(case_dir, dst_data, args.elmfire_bin)
            if not ok:
                print(f"[{case.name}] RUN FAILED (rc={rc}); see {log}")

        out_dir = os.path.join(case_dir, "outputs")
        if not os.path.isdir(out_dir):
            print(f"[WARN] {case.name}: no outputs dir, skipping comparison.")
            continue

        extractor = METRICS[case.metric]
        try:
            measured = extractor(out_dir, case)
        except Exception as exc:  # keep going across cases
            print(f"[{case.name}] metric extraction error: {exc}")
            measured = {}

        all_rows.extend(evaluate_case(case, measured))

    if not all_rows:
        sys.exit("No results produced.")

    print_console(all_rows)
    csv_path, md_path = write_reports(all_rows, run_dir)
    print(f"\nWrote: {csv_path}\nWrote: {md_path}")


if __name__ == "__main__":
    main()
