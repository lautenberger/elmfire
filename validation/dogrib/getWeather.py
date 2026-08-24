#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Feb 13 12:09:10 2026

@author: nick
"""

#!/usr/bin/env python
from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
import numpy as np
import pandas as pd
import rasterio

from parallel_api import get_thread_session, retry_call


# =============================================================================
# USER SETTINGS (set these in Spyder)
# =============================================================================

# Location (EPSG:4326)
LAT = 51.72291
LON = -115.35615

# UTC datetime range (inclusive start, inclusive end)
START_UTC = pd.Timestamp("2021-10-16 07:00:00")  # naive treated as UTC
END_UTC   = pd.Timestamp("2021-10-16 13:42:00")  # naive treated as UTC

# Output folder + names
OUT_DIR = Path(r"~/elmfire/elmfire/validation/dogrib/inputs").expanduser() # <-- change me
WXS_OUT = OUT_DIR / "dogrib.wxs"
WS_TIF_OUT = OUT_DIR / "ws.tif"
WD_TIF_OUT = OUT_DIR / "wd.tif"

# Elevation used in WXS header. If you don't know it, set None to use Open-Meteo elevation if available.
ELEVATION_M = 1826

# If you want raster stacks: provide a template GeoTIFF (defines grid/CRS/shape).
WRITE_RASTERS = True
TEMPLATE_TIF = Path(r"~/elmfire/elmfire/validation/dogrib/inputs/fbp_zip__clip.tif").expanduser()  # only used if WRITE_RASTERS=True

# Optional conditioning days (matches your old behavior). If 0, it fetches just the range.
DAYS_BEFORE = 20

# =============================================================================
# CONFIG (kept from your project)
# =============================================================================
OPENMETEO_URL       = "https://archive-api.open-meteo.com/v1/era5"
OPENMETEO_MODEL     = "era5"


# =============================================================================
# Data model
# =============================================================================
@dataclass(frozen=True)
class PointWeatherTask:
    lat: float
    lon: float
    start_time_utc: pd.Timestamp
    end_time_utc: pd.Timestamp
    out_dir: Path
    wxs_path: Path
    ws_tif_path: Path | None = None
    wd_tif_path: Path | None = None
    template_tif: Path | None = None
    elevation_m: float | None = None


# =============================================================================
# Helpers
# =============================================================================
def _ensure_utc_naive(ts: pd.Timestamp) -> pd.Timestamp:
    """
    Return a timezone-naive timestamp that represents UTC time.
    Accepts naive or tz-aware; if tz-aware, converts to UTC then drops tz.
    """
    ts = pd.to_datetime(ts)
    if ts.tzinfo is not None:
        ts = ts.tz_convert("UTC").tz_localize(None)
    return ts


def fetch_hourly_data(task: PointWeatherTask):
    """
    Fetch hourly Open-Meteo data for a point.
    Open-Meteo uses date-based start_date/end_date; we overfetch by whole days.
    """
    start_time = _ensure_utc_naive(task.start_time_utc)
    end_time = _ensure_utc_naive(task.end_time_utc)

    # Overfetch: include DAYS_BEFORE and also ensure we cover entire end day
    fetch_start = (start_time - pd.Timedelta(days=DAYS_BEFORE)).date().isoformat()
    fetch_end = end_time.date().isoformat()

    params = {
        "latitude": float(task.lat),
        "longitude": float(task.lon),
        "start_date": fetch_start,
        "end_date": fetch_end,
        "hourly": ",".join([
            "temperature_2m",
            "relative_humidity_2m",
            "precipitation",
            "wind_speed_10m",
            "wind_direction_10m",
            "cloud_cover",
        ]),
        "timezone": "UTC",
        "model": OPENMETEO_MODEL,
    }

    s = get_thread_session()

    def _do():
        r = s.get(OPENMETEO_URL, params=params, timeout=60)
        if not r.ok:
            raise RuntimeError(
                f"Open-Meteo request failed\n"
                f"URL: {r.url}\n"
                f"Status: {r.status_code}\n"
                f"Response: {r.text[:500]}"
            )
        data = r.json()
        if "hourly" not in data or "time" not in data["hourly"]:
            raise RuntimeError(f"Open-Meteo response missing hourly/time. JSON keys: {list(data.keys())}")
        return data

    data = retry_call(_do, tries=4)

    hourly = data["hourly"]
    times = pd.to_datetime(hourly["time"], utc=True).tz_convert("UTC").tz_localize(None)

    # Try to pull elevation if caller didn't provide one
    elev = task.elevation_m
    if elev is None:
        # Open-Meteo often returns "elevation" at top-level
        elev = data.get("elevation", None)

    df = pd.DataFrame(hourly)
    df["time"] = times
    return df, elev


def subset_hourly_range(df: pd.DataFrame, start_utc: pd.Timestamp, end_utc: pd.Timestamp) -> pd.DataFrame:
    start_utc = _ensure_utc_naive(start_utc)
    end_utc = _ensure_utc_naive(end_utc)
    mask = (df["time"] >= start_utc) & (df["time"] <= end_utc)
    out = df.loc[mask].copy()
    if out.empty:
        raise RuntimeError(f"No hourly rows in requested range {start_utc} .. {end_utc}")
    return out


def write_hourly_raster(template_path: Path, out_path: Path, values: np.ndarray, times: pd.DatetimeIndex | None = None):
    values = np.asarray(values, dtype=float)
    if values.size == 0:
        raise RuntimeError(f"No values to write for raster {out_path}")

    template_path = Path(template_path)
    if not template_path.exists():
        raise FileNotFoundError(f"Template raster not found: {template_path}")

    with rasterio.open(template_path) as src:
        profile = src.profile.copy()
        height, width = src.height, src.width

    profile.update(
        driver="GTiff",
        count=int(values.size),
        dtype="float32",
        nodata=None,
        compress="lzw",
    )

    out_path = Path(out_path)
    out_path.parent.mkdir(parents=True, exist_ok=True)

    print(f"Writing {values.size} bands to {out_path}")

    with rasterio.open(out_path, "w", **profile) as dst:
        for i, val in enumerate(values, start=1):
            band = np.full((height, width), float(val), dtype=np.float32)
            dst.write(band, i)
            if times is not None and len(times) >= i:
                ts = pd.to_datetime(times[i - 1])
                ts_str = ts.strftime("%Y-%m-%dT%H:%M:%SZ")
                try:
                    dst.set_band_description(i, ts_str)
                except Exception as exc:
                    print(f"  Warning: could not set band description for band {i}: {exc}")


def write_wxs(df: pd.DataFrame, out_path: Path, elevation_m: float | None):
    """
    Writes all rows in df (already subset to desired window) in your existing WXS format.
    Assumes:
      df columns: time, temperature_2m, relative_humidity_2m, precipitation,
                  wind_speed_10m, wind_direction_10m, cloud_cover
    """
    out_path = Path(out_path)
    out_path.parent.mkdir(parents=True, exist_ok=True)

    elev_int = int(round(float(elevation_m))) if elevation_m is not None else 0

    with open(out_path, "w", encoding="utf-8") as f:
        f.write(f"RAWS_ELEVATION: {elev_int}\n")
        f.write("RAWS_UNITS: METRIC\n")
        f.write("RAWS_WINDS: OpenMeteo_ERA5_point\n")
        f.write("Year Mth Day Time Temp RH HrlyPcp WindSpd WindDir CloudCov\n")

        for _, row in df.iterrows():
            t = pd.to_datetime(row["time"]).to_pydatetime()
            time_int = t.hour * 100 + t.minute

            temp_int = int(round(float(row["temperature_2m"])))
            rh_int = max(0, min(99, int(round(float(row["relative_humidity_2m"])))))
            precip_mm = float(row["precipitation"])
            wspd_kmh_int = int(round(float(row["wind_speed_10m"])))
            wdir_int = int(round(float(row["wind_direction_10m"]))) % 360
            cloud_int = max(0, min(100, int(round(float(row["cloud_cover"])))))

            f.write(
                f"{t.year:4d} {t.month:2d} {t.day:2d} "
                f"{time_int:04d} "
                f"{temp_int:4d} {rh_int:3d} "
                f"{precip_mm:7.3f} "
                f"{wspd_kmh_int:3d} {wdir_int:3d} {cloud_int:3d}\n"
            )

    print(f"Wrote WXS: {out_path}")


# =============================================================================
# Main
# =============================================================================
def main():
    task = PointWeatherTask(
        lat=LAT,
        lon=LON,
        start_time_utc=_ensure_utc_naive(START_UTC),
        end_time_utc=_ensure_utc_naive(END_UTC),
        out_dir=OUT_DIR,
        wxs_path=WXS_OUT,
        ws_tif_path=WS_TIF_OUT if WRITE_RASTERS else None,
        wd_tif_path=WD_TIF_OUT if WRITE_RASTERS else None,
        template_tif=TEMPLATE_TIF if WRITE_RASTERS else None,
        elevation_m=ELEVATION_M,
    )

    OUT_DIR.mkdir(parents=True, exist_ok=True)

    df_all, elev = fetch_hourly_data(task)
    wxs_start = task.start_time_utc - pd.Timedelta(days=DAYS_BEFORE)
    wxs_end   = task.end_time_utc
    
    df_wxs = subset_hourly_range(df_all, wxs_start, wxs_end)
    
    write_wxs(df_wxs, task.wxs_path, elev)
    df = subset_hourly_range(df_all, task.start_time_utc, task.end_time_utc)

    # Convert wind speed if you want mph like your old code did:
    # old: ws = wind_speed_10m * 0.621 (km/h -> mph approx)
    # Keep km/h for WXS (since RAWS_UNITS: METRIC). If you *actually* want mph in rasters, do it only there.
    ws_kmh = df["wind_speed_10m"].to_numpy(dtype=float)
    wd_deg = df["wind_direction_10m"].to_numpy(dtype=float)


    # Optional rasters
    if WRITE_RASTERS:
        if task.template_tif is None:
            raise ValueError("WRITE_RASTERS=True but TEMPLATE_TIF is not set.")
        # If you want mph in rasters (matching your old outputs), convert here:
        ws_mph = ws_kmh * 0.621371
        times = pd.to_datetime(df["time"]).to_numpy()

        write_hourly_raster(task.template_tif, task.ws_tif_path, ws_mph, times=times)
        write_hourly_raster(task.template_tif, task.wd_tif_path, wd_deg, times=times)

    print(f"Done. Rows written: {len(df)}")
    if elev is not None:
        print(f"Elevation used: {elev} m")


if __name__ == "__main__":
    main()