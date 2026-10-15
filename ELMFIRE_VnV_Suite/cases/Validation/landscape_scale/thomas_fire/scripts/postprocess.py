#!/usr/bin/env python3
"""Evaluate one complete ELMFIRE ensemble against case-local observations.

The script is non-destructive and case-local. It refuses to calculate spatial
skill from a partial ensemble or from outputs that do not reach the declared
comparison time.
"""

from __future__ import annotations

from report_language import polish_figure

import json
import re
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import rasterio
import yaml
from rasterio.features import rasterize

CASE_DIR = Path(__file__).resolve().parents[1]
OUTPUT_DIR = CASE_DIR / "outputs"
FIGURE_DIR = CASE_DIR / "figures"
SOURCE_MANIFEST = CASE_DIR / "data" / "source_manifest.json"


def load_configuration() -> dict[str, object]:
    case = yaml.safe_load((CASE_DIR / "case.yaml").read_text(encoding="utf-8"))
    source = json.loads(SOURCE_MANIFEST.read_text(encoding="utf-8"))
    return {
        "title": case["case_title"],
        "comparison_s": float(case["validation"]["comparison_time_s"]),
        "probability_threshold": float(case["validation"]["probability_threshold"]),
        "expected_members": int(case["namelist_contract"]["files"][0]["invariants"]["MONTE_CARLO.NUM_ENSEMBLE_MEMBERS"]),
        "event_start": source["event_start_utc"],
    }


CONFIG = load_configuration()


def save_figure(fig: plt.Figure, name: str) -> None:
    fig.tight_layout()
    polish_figure(fig)
    fig.savefig(FIGURE_DIR / name, format="pdf", bbox_inches="tight")
    plt.close(fig)


def parse_output_name(path: Path) -> tuple[int, int]:
    numbers = [int(value) for value in re.findall(r"\d+", path.stem)]
    if len(numbers) < 2:
        raise ValueError(f"Cannot parse member and output time from {path.name}")
    return numbers[-2], numbers[-1]


def final_toa_files() -> tuple[list[Path], dict[str, object]]:
    latest: dict[int, tuple[int, Path]] = {}
    parse_errors: list[str] = []
    for path in sorted(OUTPUT_DIR.glob("time_of_arrival_*.tif")):
        try:
            member, stamp = parse_output_name(path)
        except ValueError:
            parse_errors.append(path.name)
            continue
        if member not in latest or stamp > latest[member][0]:
            latest[member] = (stamp, path)
    paths = [latest[member][1] for member in sorted(latest)]
    stamps = {str(member): stamp for member, (stamp, _) in sorted(latest.items())}
    completeness = {
        "expected_members": CONFIG["expected_members"],
        "members_found": len(paths),
        "member_ids": sorted(latest),
        "latest_output_stamps_s": stamps,
        "unparseable_files": parse_errors,
        "all_members_reach_comparison_time": bool(paths) and all(
            stamp >= CONFIG["comparison_s"] for stamp, _ in latest.values()
        ),
    }
    return paths, completeness


def viirs_observation(target_crs, target_time):
    """Filter VIIRS by UTC time, project it, and form the declared point hull."""
    import geopandas as gpd
    import pandas as pd

    shapefiles = sorted((CASE_DIR / "data" / "viirs_observation").glob("*.shp"))
    if len(shapefiles) != 1:
        raise ValueError(f"Expected exactly one observation shapefile; found {len(shapefiles)}")
    observations = gpd.read_file(shapefiles[0])
    date = pd.to_datetime(observations["ACQ_DATE"])
    hhmm = observations["ACQ_TIME"].astype(str).str.zfill(4)
    timestamp = (
        date
        + pd.to_timedelta(hhmm.str[:2].astype(int), unit="h")
        + pd.to_timedelta(hhmm.str[2:].astype(int), unit="m")
    ).dt.tz_localize("UTC")
    observations = observations.loc[timestamp <= target_time.tz_convert("UTC")].copy()
    if observations.empty:
        raise ValueError("No VIIRS detections occur before the comparison time")
    observations = observations.to_crs(target_crs)
    points = observations.geometry.union_all() if hasattr(observations.geometry, "union_all") else observations.geometry.unary_union
    try:
        from shapely import concave_hull

        footprint = concave_hull(points, ratio=0.2, allow_holes=False)
        method = "concave hull (ratio 0.2, no holes)"
    except (ImportError, TypeError):
        footprint = points.convex_hull
        method = "convex hull fallback"
    return observations, footprint, method


def score(
    predicted: np.ndarray,
    observed: np.ndarray,
    valid: np.ndarray,
    probability: np.ndarray,
) -> dict[str, float | int]:
    pred = predicted[valid]
    obs = observed[valid]
    tp = int(np.count_nonzero(pred & obs))
    fp = int(np.count_nonzero(pred & ~obs))
    fn = int(np.count_nonzero(~pred & obs))
    tn = int(np.count_nonzero(~pred & ~obs))
    union = tp + fp + fn
    precision = tp / (tp + fp) if tp + fp else 0.0
    recall = tp / (tp + fn) if tp + fn else 0.0
    return {
        "true_positive_cells": tp,
        "false_positive_cells": fp,
        "false_negative_cells": fn,
        "true_negative_cells": tn,
        "jaccard": tp / union if union else 0.0,
        "precision": precision,
        "recall": recall,
        "f1": 2.0 * precision * recall / (precision + recall) if precision + recall else 0.0,
        "brier_score": float(np.mean((probability[valid] - obs.astype(float)) ** 2)),
        "area_ratio": float(np.count_nonzero(pred) / np.count_nonzero(obs)) if np.count_nonzero(obs) else 0.0,
    }


def namelist_string(name: str) -> str:
    """Read one quoted path or filename from the case-local ELMFIRE namelist."""
    text = (CASE_DIR / "elmfire.data.in").read_text(encoding="utf-8")
    match = re.search(
        rf"(?mi)^\s*{re.escape(name)}\s*=\s*['\"]([^'\"]+)['\"]",
        text,
    )
    if match is None:
        raise ValueError(f"Missing quoted namelist assignment: {name}")
    return match.group(1)


def analysis_domain_mask(metadata: dict[str, object]) -> tuple[np.ndarray, str]:
    """Return the fixed spatial domain mask from the static aspect raster."""
    directory = namelist_string("FUELS_AND_TOPOGRAPHY_DIRECTORY")
    stem = namelist_string("ASP_FILENAME")
    filename = stem if Path(stem).suffix else f"{stem}.tif"
    path = CASE_DIR / directory / filename
    if not path.is_file():
        raise FileNotFoundError(f"Analysis-domain raster is missing: {path}")
    with rasterio.open(path) as src:
        if src.shape != (metadata["height"], metadata["width"]):
            raise ValueError(f"Analysis-domain shape does not match TOA outputs: {path.name}")
        if src.crs != metadata["crs"] or not np.allclose(
            tuple(src.transform), tuple(metadata["transform"])
        ):
            raise ValueError(
                f"Analysis-domain georeferencing does not match TOA outputs: {path.name}"
            )
        valid = src.dataset_mask() > 0
    if not np.any(valid):
        raise ValueError(f"Analysis-domain raster contains no valid cells: {path.name}")
    return valid, str(path.relative_to(CASE_DIR))


def read_ensemble(paths: list[Path]) -> tuple[np.ndarray, np.ndarray, dict[str, object]]:
    arrival: list[np.ndarray] = []
    with rasterio.open(paths[0]) as reference:
        metadata = {
            "height": reference.height,
            "width": reference.width,
            "crs": reference.crs,
            "transform": reference.transform,
            "pixel_area_m2": abs(reference.transform.a * reference.transform.e),
        }
    valid, mask_source = analysis_domain_mask(metadata)
    metadata["analysis_domain_mask_source"] = mask_source
    metadata["analysis_domain_valid_cells"] = int(np.count_nonzero(valid))
    for path in paths:
        with rasterio.open(path) as src:
            if src.shape != (metadata["height"], metadata["width"]):
                raise ValueError(f"TOA shape mismatch: {path.name}")
            if src.crs != metadata["crs"] or not np.allclose(tuple(src.transform), tuple(metadata["transform"])):
                raise ValueError(f"TOA georeferencing mismatch: {path.name}")
            masked = src.read(1, masked=True)
            values = masked.filled(np.nan).astype(np.float32)
            values[~valid] = np.nan
            arrival.append(values)
    return np.stack(arrival), valid, metadata


def plot_comparison(probability: np.ndarray, observed: np.ndarray, predicted: np.ndarray) -> None:
    fig, axes = plt.subplots(1, 2, figsize=(11.0, 5.0))
    image = axes[0].imshow(probability, cmap="inferno", vmin=0.0, vmax=1.0)
    axes[0].contour(observed.astype(float), levels=[0.5], colors="cyan", linewidths=1.0)
    fig.colorbar(image, ax=axes[0], label="Ensemble burn probability (dimensionless)")
    axes[0].set_title("Probability; cyan = VIIRS-derived hull")
    agreement = np.zeros(predicted.shape, dtype=np.uint8)
    agreement[predicted & observed] = 1
    agreement[predicted & ~observed] = 2
    agreement[~predicted & observed] = 3
    cmap = plt.matplotlib.colors.ListedColormap(["white", "#4daf4a", "#e41a1c", "#377eb8"])
    axes[1].imshow(agreement, cmap=cmap, vmin=0, vmax=3, interpolation="nearest")
    axes[1].set_title("Green: overlap; red: simulated only; blue: observed only")
    for axis in axes:
        axis.set(xlabel="Grid column (index)", ylabel="Grid row (index)")
    fig.suptitle(str(CONFIG["title"]))
    save_figure(fig, "validation_comparison.pdf")


def plot_arrival(arrival: np.ndarray, valid: np.ndarray) -> None:
    burned = (arrival > 0.0) & (arrival <= CONFIG["comparison_s"])
    conditioned = np.where(burned, arrival, np.nan)
    has_arrival = valid & np.any(burned, axis=0)
    median = np.full(valid.shape, np.nan, dtype=float)
    median[has_arrival] = (
        np.nanmedian(conditioned[:, has_arrival], axis=0) / 3600.0
    )
    fig, ax = plt.subplots(figsize=(8.2, 6.0))
    image = ax.imshow(median, cmap="viridis")
    fig.colorbar(image, ax=ax, label="conditional median arrival time (h)")
    ax.set(title=f"{CONFIG['title']}: arrival time where at least one member burns", xlabel="Grid column (index)", ylabel="Grid row (index)")
    save_figure(fig, "arrival_time_summary.pdf")


def plot_area_growth(arrival: np.ndarray, pixel_area_m2: float) -> dict[str, list[float]]:
    times = np.linspace(0.0, float(CONFIG["comparison_s"]), 25)
    areas = np.asarray(
        [[np.count_nonzero((member > 0.0) & (member <= time)) * pixel_area_m2 / 1.0e6 for time in times] for member in arrival]
    )
    median = np.median(areas, axis=0)
    low = np.percentile(areas, 5, axis=0)
    high = np.percentile(areas, 95, axis=0)
    fig, ax = plt.subplots(figsize=(8.5, 5.2))
    hours = times / 3600.0
    ax.fill_between(hours, low, high, alpha=0.25, color="#35618f", label="5th--95th percentiles")
    ax.plot(hours, median, color="#17395c", label="Ensemble median")
    ax.set(xlabel="simulation time (h)", ylabel="burned area (km$^2$)", title=f"{CONFIG['title']}: burned-area growth")
    ax.legend()
    save_figure(fig, "burned_area_growth.pdf")
    return {
        "time_s": times.tolist(),
        "median_area_km2": median.tolist(),
        "p05_area_km2": low.tolist(),
        "p95_area_km2": high.tolist(),
    }


def plot_optional_fields(final_stamp: int) -> list[str]:
    field_names = ("flame_length", "ember_deposition", "ember_flux", "fireline_intensity", "heat_flux")
    selected: list[tuple[str, Path]] = []
    for stem in field_names:
        candidates = []
        for path in OUTPUT_DIR.glob(f"{stem}_*.tif"):
            try:
                _, stamp = parse_output_name(path)
            except ValueError:
                continue
            if stamp == final_stamp:
                candidates.append(path)
        if candidates:
            selected.append((stem, sorted(candidates)[0]))
    if not selected:
        return []
    columns = min(3, len(selected))
    rows = int(np.ceil(len(selected) / columns))
    fig, axes = plt.subplots(rows, columns, figsize=(4.3 * columns, 3.8 * rows), squeeze=False)
    for axis, (stem, path) in zip(axes.ravel(), selected):
        with rasterio.open(path) as src:
            values = src.read(1, masked=True)
        image = axis.imshow(values, cmap="magma")
        fig.colorbar(image, ax=axis, shrink=0.8)
        axis.set_title(stem.replace("_", " "))
    for axis in axes.ravel()[len(selected):]:
        axis.axis("off")
    fig.suptitle(f"{CONFIG['title']}: available final-time fields (representative member)")
    save_figure(fig, "optional_output_fields.pdf")
    return [path.name for _, path in selected]


def not_evaluable(reason: str, completeness: dict[str, object]) -> dict[str, object]:
    return {
        "method_version": "landscape_validation_v3",
        "status": "NOT EVALUABLE",
        "reason": reason,
        "output_completeness": completeness,
        "ensemble_members_found": completeness["members_found"],
    }


def evaluate() -> dict[str, object]:
    paths, completeness = final_toa_files()
    if not paths:
        return not_evaluable(
            "No current time_of_arrival GeoTIFFs were found for this case.",
            completeness,
        )
    if completeness["members_found"] != completeness["expected_members"]:
        return not_evaluable("The ensemble is incomplete, so validation metrics were not calculated.", completeness)
    if not completeness["all_members_reach_comparison_time"]:
        return not_evaluable("One or more members do not reach the declared comparison time.", completeness)

    import pandas as pd

    target_time = pd.Timestamp(str(CONFIG["event_start"])) + pd.to_timedelta(CONFIG["comparison_s"], unit="s")
    arrival, valid, metadata = read_ensemble(paths)
    burned = (arrival > 0.0) & (arrival <= CONFIG["comparison_s"])
    probability = np.mean(burned, axis=0)
    prediction = probability >= CONFIG["probability_threshold"]
    points, footprint, footprint_method = viirs_observation(metadata["crs"], target_time)
    observed = rasterize(
        [(footprint, 1)],
        out_shape=prediction.shape,
        transform=metadata["transform"],
        fill=0,
        dtype="uint8",
    ).astype(bool)
    values: dict[str, object] = score(prediction, observed, valid, probability)
    pixel_area = float(metadata["pixel_area_m2"])
    values.update(
        {
            "method_version": "landscape_validation_v3",
            "status": "CHARACTERIZED",
            "reason": "Metrics are descriptive because no independent predeclared acceptance thresholds are available.",
            "output_completeness": completeness,
            "ensemble_members_found": len(paths),
            "comparison_time_s": CONFIG["comparison_s"],
            "probability_threshold": CONFIG["probability_threshold"],
            "predicted_area_km2": float(np.count_nonzero(prediction & valid) * pixel_area / 1.0e6),
            "observed_area_km2": float(np.count_nonzero(observed & valid) * pixel_area / 1.0e6),
            "viirs_detection_count": len(points),
            "observation_footprint_method": footprint_method,
            "analysis_domain_mask_source": metadata["analysis_domain_mask_source"],
            "analysis_domain_valid_cells": metadata["analysis_domain_valid_cells"],
        }
    )
    plot_comparison(probability, observed, prediction)
    plot_arrival(arrival, valid)
    values["burned_area_growth"] = plot_area_growth(arrival, pixel_area)
    final_stamp = min(int(value) for value in completeness["latest_output_stamps_s"].values())
    values["optional_output_fields_plotted"] = plot_optional_fields(final_stamp)
    return values


def main() -> None:
    FIGURE_DIR.mkdir(parents=True, exist_ok=True)
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    metrics = evaluate()
    (OUTPUT_DIR / "metrics.json").write_text(
        json.dumps(metrics, indent=2, allow_nan=False) + "\n", encoding="utf-8"
    )
    print(f"[OK] {CASE_DIR.name}: {metrics['status']}; wrote outputs/metrics.json")


if __name__ == "__main__":
    main()
