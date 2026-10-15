#!/usr/bin/env python3
"""Validate immutable inputs and characterize the data used by this case.

This script is case-local and never edits archived inputs. Its only products
are outputs/input_statistics.json and reproducible PDF figures.
"""

from __future__ import annotations

from report_language import polish_figure

import hashlib
import json
import math
import re
import shutil
import subprocess
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import rasterio
from rasterio.enums import Resampling

CASE_DIR = Path(__file__).resolve().parents[1]
ARCHIVE_MANIFEST = CASE_DIR / "data" / "archive_manifest.json"
SOURCE_MANIFEST = CASE_DIR / "data" / "source_manifest.json"
NAMELIST = CASE_DIR / "elmfire.data.in"
OUTPUT_DIR = CASE_DIR / "outputs"
FIGURE_DIR = CASE_DIR / "figures"


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def namelist_string(name: str) -> str:
    text = NAMELIST.read_text(encoding="utf-8")
    match = re.search(rf"(?mi)^\s*{re.escape(name)}\s*=\s*['\"]([^'\"]+)", text)
    if not match:
        raise ValueError(f"Missing {name} in {NAMELIST.name}")
    return match.group(1)


def verify_file(record: dict[str, object], errors: list[str]) -> None:
    path = CASE_DIR / str(record["relative_path"])
    if not path.is_file():
        errors.append(f"missing file: {record['relative_path']}")
    elif path.stat().st_size != int(record["size"]):
        errors.append(f"size mismatch: {record['relative_path']}")
    elif sha256(path) != record["sha256"]:
        errors.append(f"checksum mismatch: {record['relative_path']}")


def same_grid(reference: Path, candidate: Path) -> bool:
    with rasterio.open(reference) as ref, rasterio.open(candidate) as src:
        return (
            ref.width == src.width
            and ref.height == src.height
            and ref.crs == src.crs
            and np.allclose(tuple(ref.transform), tuple(src.transform), rtol=0.0, atol=1.0e-3)
        )


def sample_raster(path: Path, bands: int = 1, limit: int = 512) -> np.ma.MaskedArray:
    """Return a deterministic nearest-neighbour sample for plots/quantiles."""
    with rasterio.open(path) as src:
        height = min(limit, src.height)
        width = min(limit, src.width)
        indexes = list(range(1, min(bands, src.count) + 1))
        return src.read(
            indexes,
            out_shape=(len(indexes), height, width),
            masked=True,
            resampling=Resampling.nearest,
        )


def scalar_statistics(
    path: Path, bands: int = 1, circular: bool = False
) -> tuple[dict[str, object], list[float]]:
    """Calculate exact moments and means; quantiles use a fixed raster sample."""
    count = 0
    total = 0.0
    total_square = 0.0
    minimum = math.inf
    maximum = -math.inf
    sin_total = 0.0
    cos_total = 0.0
    band_means: list[float] = []
    with rasterio.open(path) as src:
        used = min(bands, src.count)
        metadata = {
            "relative_path": str(path.relative_to(CASE_DIR)),
            "width": src.width,
            "height": src.height,
            "band_count": src.count,
            "bands_analyzed": used,
            "crs": src.crs.to_string() if src.crs else None,
            "bounds": list(src.bounds),
            "resolution": [abs(src.transform.a), abs(src.transform.e)],
            "nodata": src.nodata,
        }
        for band in range(1, used + 1):
            values = src.read(band, masked=True).compressed().astype(np.float64)
            values = values[np.isfinite(values)]
            if values.size == 0:
                band_means.append(float("nan"))
                continue
            count += int(values.size)
            total += float(values.sum())
            total_square += float(np.square(values).sum())
            minimum = min(minimum, float(values.min()))
            maximum = max(maximum, float(values.max()))
            band_means.append(float(values.mean()))
            if circular:
                radians = np.deg2rad(values)
                sin_total += float(np.sin(radians).sum())
                cos_total += float(np.cos(radians).sum())
    sample = sample_raster(path, bands).compressed().astype(np.float64)
    sample = sample[np.isfinite(sample)]
    mean = total / count if count else float("nan")
    variance = max(0.0, total_square / count - mean * mean) if count else float("nan")
    statistics: dict[str, object] = {
        **metadata,
        "valid_count": count,
        "minimum": minimum if count else None,
        "maximum": maximum if count else None,
        "mean": mean if count else None,
        "standard_deviation": math.sqrt(variance) if count else None,
        "sample_median": float(np.median(sample)) if sample.size else None,
        "sample_p05": float(np.percentile(sample, 5)) if sample.size else None,
        "sample_p95": float(np.percentile(sample, 95)) if sample.size else None,
        "quantile_method": "deterministic nearest-neighbour sample, at most 512 x 512 cells per band",
    }
    if circular and count:
        statistics["circular_mean_degrees"] = float(
            np.degrees(np.arctan2(sin_total, cos_total)) % 360.0
        )
    return statistics, band_means


def categorical_statistics(path: Path) -> dict[str, object]:
    with rasterio.open(path) as src:
        values = src.read(1, masked=True).compressed()
    counts = Counter(str(int(value)) for value in values[np.isfinite(values)])
    total = sum(counts.values())
    return {
        "relative_path": str(path.relative_to(CASE_DIR)),
        "valid_count": total,
        "class_counts": dict(sorted(counts.items(), key=lambda item: int(item[0]))),
        "class_fractions": {key: value / total for key, value in counts.items()} if total else {},
    }


def read_observations() -> list[dict[str, object]]:
    """Read the case-local shapefile through GDAL without changing it."""
    ogr2ogr = shutil.which("ogr2ogr")
    if not ogr2ogr:
        candidate = Path("/opt/homebrew/anaconda3/bin/ogr2ogr")
        ogr2ogr = str(candidate) if candidate.is_file() else None
    if not ogr2ogr:
        raise RuntimeError("ogr2ogr is required to characterize the VIIRS shapefile")
    shapefile = next((CASE_DIR / "data" / "viirs_observation").glob("*.shp"))
    result = subprocess.run(
        [ogr2ogr, "-f", "GeoJSON", "/vsistdout/", str(shapefile)],
        check=True,
        stdout=subprocess.PIPE,
        text=True,
    )
    return json.loads(result.stdout)["features"]


def observation_statistics(
    features: list[dict[str, object]], cutoff_text: str
) -> tuple[dict[str, object], list[tuple[float, float, datetime]]]:
    cutoff = datetime.fromisoformat(cutoff_text.replace("Z", "+00:00"))
    retained: list[tuple[float, float, datetime]] = []
    frp: list[float] = []
    all_times: list[datetime] = []
    satellites: set[str] = set()
    instruments: set[str] = set()
    versions: set[str] = set()
    for feature in features:
        props = feature["properties"]
        hhmm = str(props["ACQ_TIME"]).zfill(4)
        timestamp = datetime.strptime(
            f"{props['ACQ_DATE']} {hhmm}", "%Y-%m-%d %H%M"
        ).replace(tzinfo=timezone.utc)
        all_times.append(timestamp)
        satellites.add(str(props.get("SATELLITE", "")))
        instruments.add(str(props.get("INSTRUMENT", "")))
        versions.add(str(props.get("VERSION", "")))
        value = props.get("FRP")
        if value is not None:
            frp.append(float(value))
        if timestamp <= cutoff:
            lon, lat = feature["geometry"]["coordinates"][:2]
            retained.append((float(lon), float(lat), timestamp))
    coordinates = np.asarray([(x, y) for x, y, _ in retained], dtype=float)
    return {
        "total_detection_count": len(features),
        "retained_detection_count": len(retained),
        "first_detection_utc": min(all_times).isoformat() if all_times else None,
        "last_detection_utc": max(all_times).isoformat() if all_times else None,
        "comparison_cutoff_utc": cutoff.isoformat(),
        "retained_longitude_range": [float(coordinates[:, 0].min()), float(coordinates[:, 0].max())] if len(coordinates) else None,
        "retained_latitude_range": [float(coordinates[:, 1].min()), float(coordinates[:, 1].max())] if len(coordinates) else None,
        "frp_minimum_mw": min(frp) if frp else None,
        "frp_mean_mw": float(np.mean(frp)) if frp else None,
        "frp_maximum_mw": max(frp) if frp else None,
        "satellites": sorted(satellites),
        "instruments": sorted(instruments),
        "versions": sorted(versions),
    }, retained


def save_figure(fig: plt.Figure, name: str) -> None:
    fig.tight_layout()
    polish_figure(fig)
    fig.savefig(FIGURE_DIR / name, format="pdf", bbox_inches="tight")
    plt.close(fig)


def create_figures(
    samples: dict[str, np.ndarray],
    series: dict[str, list[float]],
    observations: list[tuple[float, float, datetime]],
    event_start: datetime,
    weather_interval_s: float,
) -> None:
    fig, axes = plt.subplots(2, 2, figsize=(10.0, 7.6))
    panels = [
        ("dem", "Elevation (m)"),
        ("slp", "Slope (degrees)"),
        ("fbfm40b", "Fuel-model classification"),
        ("ignition_mask", "Ignition eligibility (dimensionless)"),
    ]
    for axis, (key, title) in zip(axes.ravel(), panels):
        image = axis.imshow(
            samples[key], cmap="terrain" if key == "dem" else "viridis", interpolation="nearest"
        )
        fig.colorbar(image, ax=axis, shrink=0.82, label=title)
        axis.set(title=title, xlabel="Sample column (index)", ylabel="Sample row (index)")
    fig.suptitle(f"{CASE_DIR.name}: active landscape and ignition inputs")
    save_figure(fig, "input_overview.pdf")

    fig, axes = plt.subplots(2, 3, figsize=(11.0, 6.8))
    labels = {
        "ws": "20-ft wind speed (mph)",
        "wd": "Wind direction (degrees)",
        "m1": "1-h dead-fuel moisture (%)",
        "m10": "10-h dead-fuel moisture (%)",
        "m100": "100-h dead-fuel moisture (%)",
    }
    for axis, key in zip(axes.ravel(), labels):
        values = samples[key]
        axis.hist(values[np.isfinite(values)], bins=40, color="#35618f", alpha=0.9)
        axis.set(title=labels[key], xlabel=labels[key], ylabel="Sample count")
    axes.ravel()[-1].axis("off")
    fig.suptitle(f"{CASE_DIR.name}: distributions over the 25 active weather bands")
    save_figure(fig, "weather_summary.pdf")

    fig, axes = plt.subplots(3, 1, figsize=(9.5, 8.0), sharex=True)
    hours = np.arange(len(series["ws"])) * (weather_interval_s / 3600.0)
    axes[0].plot(hours, series["ws"], marker="o", ms=3, label="wind speed")
    axes[0].set_ylabel("Mean wind speed (mph)")
    axes[0].legend()
    axes[1].plot(hours, series["wd"], marker="o", ms=3, color="#8c4f2b", label="wind direction")
    axes[1].set_ylabel("Arithmetic mean direction (degrees)")
    axes[1].legend()
    for key, label in (("m1", "1-h"), ("m10", "10-h"), ("m100", "100-h")):
        axes[2].plot(hours, series[key], label=label)
    axes[2].set(xlabel="Time from first weather sample (h)", ylabel="Dead-fuel moisture (%)")
    axes[2].legend(ncol=3)
    fig.suptitle(f"{CASE_DIR.name}: domain-mean weather evolution")
    save_figure(fig, "weather_time_series.pdf")

    fig, ax = plt.subplots(figsize=(8.0, 6.2))
    if observations:
        longitude = [row[0] for row in observations]
        latitude = [row[1] for row in observations]
        elapsed = [(row[2] - event_start).total_seconds() / 3600.0 for row in observations]
        scatter = ax.scatter(longitude, latitude, c=elapsed, s=8, cmap="plasma", alpha=0.7)
        fig.colorbar(scatter, ax=ax, label="Time from configured event start (h)")
    ax.set(
        title=f"{CASE_DIR.name}: VIIRS detections retained for comparison",
        xlabel="Longitude (degrees east)",
        ylabel="Latitude (degrees north)",
    )
    save_figure(fig, "observation_summary.pdf")


def main() -> None:
    archive = json.loads(ARCHIVE_MANIFEST.read_text(encoding="utf-8"))
    source = json.loads(SOURCE_MANIFEST.read_text(encoding="utf-8"))
    errors: list[str] = []
    source_archive = CASE_DIR / archive["source_archive"]["relative_path"]
    if not source_archive.is_file():
        errors.append(f"missing source archive: {source_archive}")
    elif sha256(source_archive) != archive["source_archive"]["sha256"]:
        errors.append("source archive checksum mismatch")
    for record in archive["payload_files"]:
        verify_file(record, errors)
    for record in source["observation"]["files"]:
        verify_file(record, errors)

    fuel_dir = CASE_DIR / archive["fuel_directory"]
    weather_dir = CASE_DIR / archive["weather_directory"]
    fuel_paths = [fuel_dir / f"{stem}.tif" for stem in archive["required_fuel_rasters"]]
    for path in fuel_paths:
        if not path.is_file():
            errors.append(f"missing required fuel/topography raster: {path.relative_to(CASE_DIR)}")
    for path in fuel_paths[1:]:
        if path.is_file() and not same_grid(fuel_paths[0], path):
            errors.append(f"fuel-grid mismatch: {path.relative_to(CASE_DIR)}")

    weather_names = {
        key: namelist_string(f"{key.upper()}_FILENAME")
        for key in ("ws", "wd", "m1", "m10", "m100")
    }
    weather_paths = {key: weather_dir / f"{stem}.tif" for key, stem in weather_names.items()}
    for path in weather_paths.values():
        if not path.is_file():
            errors.append(f"missing active weather raster: {path.relative_to(CASE_DIR)}")
        else:
            with rasterio.open(path) as src:
                if src.count < int(source["analysis"]["weather_bands_used"]):
                    errors.append(f"insufficient weather bands: {path.relative_to(CASE_DIR)}")
    available_weather = [path for path in weather_paths.values() if path.is_file()]
    for path in available_weather[1:]:
        if not same_grid(available_weather[0], path):
            errors.append(f"weather-grid mismatch: {path.relative_to(CASE_DIR)}")
    if available_weather and fuel_paths[0].is_file():
        with rasterio.open(fuel_paths[0]) as fuel, rasterio.open(available_weather[0]) as weather:
            if fuel.crs != weather.crs:
                errors.append("weather and landscape CRS differ")
            if not np.allclose(tuple(fuel.bounds), tuple(weather.bounds), rtol=0.0, atol=1.0e-3):
                errors.append("weather and landscape extents differ")
    if errors:
        raise SystemExit("Input validation failed:\n  - " + "\n  - ".join(errors))

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    FIGURE_DIR.mkdir(parents=True, exist_ok=True)
    paths = {
        key: fuel_dir / f"{key}.tif"
        for key in ("dem", "slp", "asp", "fbfm40b", "ignition_mask")
    }
    fields: dict[str, object] = {}
    series: dict[str, list[float]] = {}
    plot_samples: dict[str, np.ndarray] = {}
    for key in ("dem", "slp", "asp"):
        fields[key], _ = scalar_statistics(paths[key], circular=(key == "asp"))
        plot_samples[key] = sample_raster(paths[key])[0].astype(float).filled(np.nan)
    for key in ("fbfm40b", "ignition_mask"):
        fields[key] = categorical_statistics(paths[key])
        plot_samples[key] = sample_raster(paths[key])[0].astype(float).filled(np.nan)
    used_bands = int(source["analysis"]["weather_bands_used"])
    for key, path in weather_paths.items():
        fields[key], series[key] = scalar_statistics(path, used_bands, circular=(key == "wd"))
        plot_samples[key] = sample_raster(path, used_bands, 256).compressed().astype(float)

    features = read_observations()
    obs_stats, retained = observation_statistics(
        features, source["observation"]["comparison_cutoff_utc"]
    )
    event_start = datetime.fromisoformat(source["event_start_utc"].replace("Z", "+00:00"))
    create_figures(
        plot_samples,
        series,
        retained,
        event_start,
        float(source["analysis"]["weather_interval_s"]),
    )
    result = {
        "method_version": "landscape_inputs_v2",
        "case_id": CASE_DIR.name,
        "archive_payload_files_verified": len(archive["payload_files"]),
        "observation_files_verified": len(source["observation"]["files"]),
        "fields": fields,
        "weather_band_means": series,
        "observation": obs_stats,
    }
    (OUTPUT_DIR / "input_statistics.json").write_text(
        json.dumps(result, indent=2, allow_nan=False) + "\n", encoding="utf-8"
    )
    print(f"[OK] {CASE_DIR.name}: inputs verified, characterized, and plotted.")


if __name__ == "__main__":
    main()
