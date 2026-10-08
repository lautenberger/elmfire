#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Complete, runnable postprocess script for one ELMFIRE case.

What it does:
- Loads/plots fuel map (categorized colormap)
- Loads weather rasters and plots histograms (WS, WD, M1, M10, M100)
- Loads TOA stack, builds burn-area history
- Loads VIIRS points, filters to extent, groups by half-day, convex hulls, burn-area history
- Overlays VIIRS dots on base map and simulated burned extent for selected times
- Computes Cohen's Kappa per selected frames by rasterizing VIIRS hulls and comparing to φ-binary
- Saves figures + metrics.json

Adjust the PATH CONSTANTS block for your case.
"""

from __future__ import annotations
import os, re, glob, json, math
from pathlib import Path
from typing import Dict, List, Tuple, Optional

import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from matplotlib.patches import Patch

import rasterio
from rasterio.features import rasterize
from rasterio.warp import calculate_default_transform, reproject, Resampling
from rasterio.enums import Resampling as ResampEnum

import geopandas as gpd
from shapely.geometry import Point, Polygon, MultiPoint, MultiPolygon
from shapely.ops import unary_union
import pandas as pd

from sklearn.metrics import cohen_kappa_score
from datetime import timedelta
from zoneinfo import ZoneInfo
from datetime import datetime

from pyproj import Geod

# --- concave hulls per half-day (fallback to convex if needed) ---
from typing import Dict, Optional, Tuple, List, Union
from shapely.geometry import MultiPoint, Polygon, MultiPolygon
from shapely.geometry.base import BaseGeometry

# Prefer Shapely 2.x API; fallback for 1.8.x
try:
    from shapely import concave_hull as _concave_hull  # Shapely 2.x
except Exception:  # pragma: no cover
    from shapely.ops import concave_hull as _concave_hull  # older Shapely

os.environ.setdefault("SHAPE_RESTORE_SHX", "YES")
# --- geodesic areas (km²) from EPSG:4326 hulls ---
_geod = Geod(ellps="WGS84")

# -------------------------
# ===== RASTER Operations ====
# -------------------------

def read_raster(path: Path) -> Tuple[np.ndarray, rasterio.Affine, dict]:
    with rasterio.open(path) as src:
        arr = src.read(1, masked=True)
        transform = src.transform
        meta = src.meta.copy()
    return arr, transform, meta

def reproject_to(src_path: Path, dst_crs: str) -> Tuple[np.ndarray, rasterio.Affine, dict]:
    with rasterio.open(src_path) as src:
        transform, width, height = calculate_default_transform(
            src.crs, dst_crs, src.width, src.height, *src.bounds
        )
        dst = np.empty((height, width), dtype=src.meta["dtype"])
        reproject(
            source=rasterio.band(src, 1),
            destination=dst,
            src_transform=src.transform,
            src_crs=src.crs,
            dst_transform=transform,
            dst_crs=dst_crs,
            resampling=Resampling.nearest
        )
        meta = src.meta.copy()
        meta.update({"crs": dst_crs, "transform": transform, "width": width, "height": height})
    return dst, transform, meta

def array_extent(transform: rasterio.Affine, width: int, height: int) -> List[float]:
    left, bottom = transform * (0, height)
    right, top   = transform * (width, 0)
    return [left, right, bottom, top]

def rasterize_polygon_to_ref(poly: Polygon, ref_path: Path, burn_value: int = 1) -> np.ndarray:
    with rasterio.open(ref_path) as ref:
        out_shape = (ref.height, ref.width)
        transform = ref.transform
    if poly.is_empty:
        return np.zeros(out_shape, dtype=np.uint8)
    return rasterize([(poly, burn_value)], out_shape=out_shape, transform=transform,
                     fill=0, dtype="uint8")

# -------------------------
# ====== FUEL MAP =========
# -------------------------
def create_custom_fuel_colormap(fuel_data: np.ndarray):
    # Reclassify: 0=structure(91), 1=water(98), 2=nonburnable(92/93/99), 3=vegetation(other)
    reclassified = np.full(fuel_data.shape, 3, dtype=np.uint8)
    reclassified[fuel_data == 91] = 0
    reclassified[fuel_data == 98] = 1
    reclassified[np.isin(fuel_data, [92, 93, 99])] = 2

    colors = ["saddlebrown", "lightblue", "gray", "lightgreen"]
    cmap = mcolors.ListedColormap(colors, name="custom_fuel")
    norm = mcolors.BoundaryNorm(np.arange(-0.5, 4.5, 1), cmap.N)
    return reclassified, cmap, norm

def plot_fuel_map(fuel_map_path: Path, ax: Optional[plt.Axes] = None, show_colorbar: bool = True,
                  dst_crs: str = "EPSG:4326", **imshow_kwargs):
    dst, transform, meta = reproject_to(fuel_map_path, dst_crs)
    # mask nodata
    nodata = meta.get("nodata", None)
    if nodata is not None:
        dst = np.ma.masked_equal(dst, nodata)

    fuel_reclass, fuel_cmap, fuel_norm = create_custom_fuel_colormap(dst)

    if ax is None:
        fig, ax = plt.subplots(figsize=(10, 8))
    extent = array_extent(transform, meta["width"], meta["height"])
    imshow_kwargs.setdefault("extent", extent)
    imshow_kwargs.setdefault("origin", "upper")

    im = ax.imshow(fuel_reclass, cmap=fuel_cmap, norm=fuel_norm, **imshow_kwargs)
    ax.set_aspect("equal")
    if show_colorbar:
        cbar = plt.colorbar(im, ax=ax, fraction=0.046, pad=0.02, ticks=[0,1,2,3])
        cbar.set_label("Fuel Type")
        cbar.ax.set_yticklabels(["S(91)", "W(98)", "N(92,93,99)", "V(others)"])
    return im, ax, extent

# -------------------------
# ====== WEATHER HISTS =====
# -------------------------
def plot_wx_hist(ax: plt.Axes, filepath: Path, bins: int = 60, title: Optional[str] = None) -> None:
    arr, _, meta = read_raster(filepath)
    data = np.asarray(arr).ravel()
    if np.ma.isMaskedArray(data):
        data = data.compressed()
    data = data[np.isfinite(data)]

    ax.hist(data, bins=bins, density=True)
    ax.set_ylabel("PDF [-]")
    if title:
        ax.set_title(title)

# -------------------------
# ===== TOA LOADING ==
# -------------------------
def load_toa_stack(toa_glob: str) -> Tuple[List[Path], List[np.ndarray], List[float], rasterio.Affine, dict]:
    paths = sorted([Path(p) for p in glob.glob(toa_glob)])
    if not paths:
        raise FileNotFoundError(f"No TOA rasters found with pattern: {toa_glob}")
    arrays, times = [], []
    transform, meta = None, None
    for p in paths:
        arr, tform, m = read_raster(p)
        arrays.append(np.array(arr))
        # infer time from filename if present, else index
        m2 = re.search(r"time_of_arrival_(\d+)\.tif$", p.name)
        times.append(float(m2.group(1)) if m2 else float(len(times)))
        if transform is None:
            transform, meta = tform, m
    return paths, arrays, times, transform, meta
    
# -------------------------
# ===== VIIRS PROCESSING ===
# -------------------------
def load_viirs_points(viirs_dir: Path, columns=None) -> gpd.GeoDataFrame:
    # recurse: pick up shapefiles in subdirectories too
    shps = sorted(viirs_dir.rglob("*.shp"))
    if not shps:
        raise FileNotFoundError(f"No shapefiles under: {viirs_dir}")

    gdfs = []
    for shp in shps:
        try:
            # fast + robust path; skips invalid records instead of failing
            g = gpd.read_file(
                shp,
                engine="pyogrio",
                use_arrow=True,
                on_invalid="skip",
                columns=columns,
            )
        except Exception:
            # slower fallback
            g = gpd.read_file(shp, engine="fiona")
        if not g.empty:
            gdfs.append(g)

    gdf = pd.concat(gdfs, ignore_index=True) if gdfs else gpd.GeoDataFrame(geometry=[])
    # normalize CRS to WGS84
    if not gdf.empty:
        gdf = gdf.set_crs(4326, allow_override=True) if gdf.crs is None else gdf.to_crs(4326)
        # drop rows without geometry
        gdf = gdf[~gdf.geometry.isna() & ~gdf.geometry.is_empty].copy()
    return gdf

def add_viirs_obstime(gdf: gpd.GeoDataFrame) -> gpd.GeoDataFrame:
    """
    Expects:
      - ACQ_DATE as datetime-like (or parseable)
      - ACQ_TIME as 'HHMM' string (e.g., '1324')
    """
    if not gnp_is_datetime64_any(gdf["ACQ_DATE"]):
        gdf["ACQ_DATE"] = gpd.pd.to_datetime(gdf["ACQ_DATE"])
    def combine(row):
        hm = row["ACQ_TIME"]
        hh = int(str(hm)[:2])
        mm = int(str(hm)[2:])
        return row["ACQ_DATE"] + timedelta(hours=hh, minutes=mm)
    gdf["observation_time"] = gdf.apply(combine, axis=1)
    gdf = gdf.sort_values("observation_time").reset_index(drop=True)
    # half-day bin index (integer): day_number*2 + (0 or 1)
    gdf["half_day"] = (gdf["observation_time"].dt.day * 2 + (gdf["observation_time"].dt.hour // 12)).astype(int)
    return gdf

def gnp_is_datetime64_any(series) -> bool:
    return np.issubdtype(series.dtype, np.datetime64)

def viirs_concave_hulls_by_halfday( gdf: gpd.GeoDataFrame, ratio: float = 0.5, allow_holes: bool = True, tiny_buffer_deg: float = 1e-9) -> Dict[int, Polygon]:
    """
    Build cumulative concave hulls (EPSG:4326):
    for each half_day k, hull_k uses all points with half_day <= k.
    """
    if "half_day" not in gdf.columns:
        raise KeyError("Expected 'half_day' column in gdf.")
    g = gdf[~gdf.geometry.isna() & ~gdf.geometry.is_empty].copy()

    # Ensure WGS84
    if g.crs is None:
        g = g.set_crs(4326, allow_override=True)
    elif g.crs.to_epsg() != 4326:
        g = g.to_crs(4326)

    # Stable ordering
    sort_cols = ["half_day"] + (["observation_time"] if "observation_time" in g.columns else [])
    g = g.sort_values(sort_cols).reset_index(drop=True)

    hulls: Dict[int, Polygon] = {}
    cum_points = []

    for hd in sorted(g["half_day"].unique()):
        # add this bin’s points to cumulative set
        mask = g["half_day"] == hd
        cum_points.extend(g.loc[mask, "geometry"].tolist())
        mp = MultiPoint(cum_points)

        # concave hull if possible, else convex
        if len(cum_points) < 3:
            hull: BaseGeometry = mp.convex_hull
        else:
            try:
                hull = _concave_hull(mp, ratio=ratio, allow_holes=allow_holes)
            except Exception:
                hull = mp.convex_hull

        # Ensure polygonal geometry
        if hull.geom_type in ("Point", "LineString"):
            hull = hull.buffer(tiny_buffer_deg)

        # representative time for this half_day → median obs time (UTC)
        t_rep = g.loc[mask, "observation_time"].max()
        hulls[t_rep] = hull  # key is observation_time, not half_day

    return hulls

def _geodesic_area_m2(poly: Polygon) -> float:
    x, y = poly.exterior.xy
    a_ext, _ = _geod.polygon_area_perimeter(x, y)
    area = abs(a_ext)
    for ring in poly.interiors:
        xi, yi = ring.xy
        a_h, _ = _geod.polygon_area_perimeter(xi, yi)
        area -= abs(a_h)
    return area

def _parse_start_to_utc(start_time: Union[str, pd.Timestamp, "datetime"]) -> pd.Timestamp:
    """
    Parse start_time without letting pandas infer tz from tokens like 'PST'.
    Returns a tz-aware Timestamp in UTC.
    """
    if isinstance(start_time, str):
        s = start_time.replace(",", " ").strip()
        # grab trailing TZ token (PST/PDT/UTC/GMT) if present
        m = re.search(r"\b(PST|PDT|UTC|GMT)\s*$", s, flags=re.IGNORECASE)
        tz_map = {"PST": "America/Los_Angeles", "PDT": "America/Los_Angeles",
                  "UTC": "UTC", "GMT": "UTC"}
        tz = None
        if m:
            tz = tz_map[m.group(1).upper()]
            s = s[:m.start()].strip()  # remove the TZ token
        ts = pd.to_datetime(s, errors="raise")       # no TZ token in the string now
        ts = ts.tz_localize(ZoneInfo(tz or "UTC"))   # attach explicit tz
    else:
        ts = pd.to_datetime(start_time)
        ts = ts.tz_localize(ZoneInfo("UTC")) if ts.tzinfo is None else ts.tz_convert(ZoneInfo("UTC"))
    return ts.astimezone(ZoneInfo("UTC"))

def _to_utc(ts) -> pd.Timestamp:
    ts = pd.to_datetime(ts)
    return ts.tz_localize(ZoneInfo("UTC")) if ts.tzinfo is None else ts.tz_convert(ZoneInfo("UTC"))

def viirs_burn_area_history_from_hulls(
    hulls: Dict[int, Polygon],
    start_time: Union[str, pd.Timestamp, "datetime"],
    halfday_times_utc: Optional[Dict[int, Union[pd.Timestamp, "datetime"]]] = None
) -> Tuple[List[float], List[float]]:
    """
    Returns (times_sec, areas_km2).
    - halfday_times_utc: dict half_day -> datetime **in UTC** (naive treated as UTC).
    - start_time: e.g. "2025-10-08, 21:45 PST" (any tz string; converted to UTC cleanly).
    """
    t0 = _parse_start_to_utc(start_time)

    # sort by observation_time
    obs_times = sorted(hulls.keys())
    
    areas_km2: List[float] = []
    times_sec: List[float] = []
    for t in obs_times:
        g = hulls[t]
        if g.is_empty:
            areas_km2.append(0.0)
        elif isinstance(g, MultiPolygon):
            areas_km2.append(sum(_geodesic_area_m2(p) for p in g.geoms) / 1e6)
        else:
            areas_km2.append(_geodesic_area_m2(g) / 1e6)
        # compute seconds since start      
        ts = pd.to_datetime(t)
        if ts.tzinfo is None:
            ts = ts.tz_localize("UTC")
        else:
            ts = ts.tz_convert("UTC")
        times_sec.append((ts - t0).total_seconds())

    return times_sec, areas_km2
    
# -------------------------
# ======= BURN HISTORY =====
# -------------------------
def burn_area_history_from_toa(
    toa_array: np.ndarray,
    pixel_area_m2: Optional[float] = None,
    n_steps: Optional[int] = None
) -> Tuple[List[float], List[float], List[float]]:
    """
    Compute cumulative burned area vs time from a single per-pixel TOA raster.

    Parameters
    ----------
    toa_array : np.ndarray
        2D array (or masked array) where each finite element is the time of arrival.
        NaNs / masked cells are treated as no-data.
    pixel_area_m2 : float, optional
        Area of one pixel (m^2). If None, areas are returned in pixel counts.
    n_steps : int, optional
        If provided, compute areas at this many evenly spaced time thresholds
        between min and max TOA (faster, coarser). If None, use every unique TOA.

    Returns
    -------
    times : List[float]
        Time thresholds (either unique TOA values or evenly spaced thresholds).
    areas : List[float]
        Cumulative burned area at each time (same length as `times`).
    """
    # Normalize input to a masked array and extract finite values
    a = np.ma.asarray(toa_array)
    finite_mask = np.isfinite(a) & (~a.mask if np.ma.isMaskedArray(a) else True)
    vals = np.asarray(a[finite_mask], dtype=float)

    if vals.size == 0:
        return [], []

    # Sort TOA values once; cumulative count yields burned pixels vs time
    vals.sort()

    # Drop negative 
    vals = vals[vals > 0]
    if vals.size == 0:
        return [], [], []

    scale = pixel_area_m2 if pixel_area_m2 else 1.0

    if n_steps is None:
        unique_times, counts = np.unique(vals, return_counts=True)
        cum_counts = np.cumsum(counts)
        areas = (cum_counts * scale).astype(float).tolist()
        return unique_times.tolist(), cum_counts.tolist(), areas
    else:
        # Coarse curve at evenly spaced thresholds (much faster if many unique TOAs)
        t_min, t_max = float(vals[0]), float(vals[-1])
        thresholds = np.linspace(t_min, t_max, int(n_steps))
        # Number of vals <= threshold via binary search
        idx = np.searchsorted(vals, thresholds, side="right")
        areas = (idx * scale).astype(float).tolist()
        return thresholds.tolist(), areas


# -------------------------
# ====== PLOTTING (TOA vs VIIRS)
# -------------------------
def plot_burnt_map_from_toa(ax: plt.Axes,toa_array: np.ndarray,time_now: float, extent, alpha: float = 0.4) -> None:
    """
    Show burned region where TOA <= time_now on the same grid/extent as TOA.
    """
    # finite domain + threshold
    finite = np.isfinite(toa_array) & (~toa_array.mask if np.ma.isMaskedArray(toa_array) else True)
    burned = (finite & (toa_array <= time_now) & (toa_array > 0))
    toa_max = np.nanmax(toa_array[burned])
    burned = burned.astype(float)
    burned[burned == 0] = np.nan
    
    ax.imshow(burned, extent=extent, origin="upper", alpha=alpha, cmap="Grays", vmin=0, vmax=1)
    return toa_max


def plot_viirs_points(ax: plt.Axes, gdf_proj: gpd.GeoDataFrame, **kwargs):
    xs = gdf_proj.geometry.x.values
    ys = gdf_proj.geometry.y.values
    ax.scatter(xs, ys, **kwargs)

# -------------------------
# ====== KAPPA METRIC ======
# -------------------------
def calc_cohen_kappa_for_case(
    toa_field: np.ndarray,
    hulls_by_halfday: Dict[Union[pd.Timestamp, "datetime"], Polygon],
    start_time: Union[str, pd.Timestamp, "datetime"],
    ref_raster_path: Path,
) -> Tuple[List[float], List[float], List[float]]:
    """
    Compare simulated burned mask (TOA <= t) vs rasterized VIIRS hull at each observation time.

    Returns:
      kappas:     Cohen's kappa per time
      times_simu: simulated cutoff time actually realized (max TOA within mask) in seconds (NaN if no pixels)
      times_viirs:VIIRS observation time in seconds since start_time
    """
    # Parse start_time to tz-aware UTC
    t0 = _parse_start_to_utc(start_time)

    # Ensure ref grid matches TOA grid
    with rasterio.open(ref_raster_path) as ref:
        ref_shape = (ref.height, ref.width)
        ref_crs = ref.crs
        
    if toa_field.shape != ref_shape:
        raise ValueError(
            f"Grid mismatch: TOA shape {toa_field.shape} != ref raster shape {ref_shape}"
        )

    # Normalize TOA array and valid mask
    arr = np.ma.asarray(toa_field)
    valid = np.isfinite(arr) & (~arr.mask if np.ma.isMaskedArray(arr) else True)

    kappas: List[float] = []
    times_simu: List[float] = []
    times_viirs: List[float] = []

    # Sort observation times; normalize each to UTC
    obs_times = sorted(hulls_by_halfday.keys()) 
        
    for i,t in enumerate(obs_times): 
        # compute seconds since start 
        ts = pd.to_datetime(t) 
        if ts.tzinfo is None: 
            ts = ts.tz_localize("UTC") 
        else: 
            ts = ts.tz_convert("UTC") 
        time_sec = (ts - t0).total_seconds()
        
        times_viirs.append(float(time_sec))

        # Rasterize VIIRS hull on ref grid
        hull = hulls_by_halfday[t]
        hull_gdf = gpd.GeoDataFrame(geometry=[hull], crs="EPSG:4326").to_crs(ref_crs)
        hull_proj = hull_gdf.iloc[0].geometry  # extract reprojected polygon
        viirs_bin = rasterize_polygon_to_ref(hull_proj, ref_raster_path, burn_value=1)
        
        # Build simulated burned mask at this time threshold
        burnt = np.zeros(arr.shape, dtype=np.uint8)
        sim_time = np.nan
        if time_sec > 0:
            mask = valid & (arr <= time_sec) & (arr > 0.0)
            if mask.any():
                burnt[mask] = 1
                # actual simulated cutoff realized (max TOA within mask)
                sim_time = float(np.nanmax(np.asarray(arr)[mask]))
        times_simu.append(sim_time)

        # Compare only over valid simulation pixels to avoid nodata bias
        y_sim = burnt[valid].ravel()
        y_obs = viirs_bin[valid].ravel()

        # If only one class present, kappa is undefined; guard it
        if (y_sim.max() == y_sim.min()) and (y_obs.max() == y_obs.min()):
            kappa = np.nan
        else:
            kappa = cohen_kappa_score(y_obs, y_sim)
        
        kappas.append(float(kappa) if np.isfinite(kappa) else np.nan)

    return kappas, times_simu, times_viirs
