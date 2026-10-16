#!/usr/bin/env python3
"""Small case-local helpers for deterministic raster variants."""
from __future__ import annotations

import json
import math
import re
import shutil
from pathlib import Path

import numpy as np
import rasterio
from rasterio.transform import from_origin

SIZE = 140
CELL_SIZE_M = 10.0
HALF_WIDTH_M = 0.5 * SIZE * CELL_SIZE_M
TRANSFORM = from_origin(-HALF_WIDTH_M, HALF_WIDTH_M, CELL_SIZE_M, CELL_SIZE_M)
CRS = "EPSG:32610"
NODATA = -9999.0
INITIAL_FRONT_X_M = -400.0
IGNITION_STRIP_WIDTH_M = 50.0
IGNITION_STRIP_HALF_HEIGHT_M = 300.0
TARGET_FRONT_ADVANCE_FRACTION = 0.05
TARGET_CFL = 0.20
ZERO_ROS_EPS = 1.0e-6
ZERO_ROS_TIMESTEP_S = 5.0


def begin_run(case_dir: Path, case_id: str, required_variant_count: int) -> None:
    """Invalidate prior generated decisions before variant generation starts."""
    output = case_dir / "outputs"
    output.mkdir(parents=True, exist_ok=True)
    status = {
        "case_id": case_id,
        "overall_status": "NOT EVALUABLE",
        "workflow_status": "INCOMPLETE",
        "verification_passed": False,
        "required_outputs_complete": False,
        "required_variant_count": required_variant_count,
        "completed_variant_count": 0,
        "metrics": [],
        "reason": "Preprocessing started a new run; required ELMFIRE outputs are not complete.",
    }
    (output / "metrics.json").write_text(
        json.dumps(status, indent=2) + "\n", encoding="utf-8"
    )
    for path in (
        case_dir / "figures/sweep_response.pdf",
        case_dir / "figures/vector_response.pdf",
        case_dir / "report/case_report.pdf",
    ):
        path.unlink(missing_ok=True)


def reset_generated_directory(path: Path) -> None:
    """Recreate one explicitly generated runtime directory without stale files."""
    if path.exists():
        shutil.rmtree(path)
    path.mkdir(parents=True, exist_ok=True)


def write_raster(path: Path, values: float | np.ndarray, dtype: str) -> None:
    """Write one single-band GeoTIFF with the case grid convention."""
    array = (
        values
        if isinstance(values, np.ndarray)
        else np.full((SIZE, SIZE), values, dtype=dtype)
    )
    with rasterio.open(
        path,
        "w",
        driver="GTiff",
        height=SIZE,
        width=SIZE,
        count=1,
        dtype=dtype,
        crs=CRS,
        transform=TRANSFORM,
        nodata=NODATA,
        compress="deflate",
    ) as dataset:
        dataset.write(np.asarray(array, dtype=dtype), 1)


def replace_assignment(text: str, key: str, value: object) -> str:
    """Replace one scalar namelist assignment and reject ambiguous templates."""
    pattern = rf"(?mi)^(\s*{re.escape(key)}\s*=\s*).*$"
    replaced, count = re.subn(pattern, rf"\g<1>{value}", text, count=1)
    if count != 1:
        raise KeyError(f"Expected exactly one assignment for {key}")
    return replaced


def bounded_level_set(signed_distance_m: np.ndarray) -> np.ndarray:
    """Scale a signed-distance field into ELMFIRE's supported [-1, 1] range."""
    phi = np.clip(signed_distance_m / CELL_SIZE_M, -1.0, 1.0).astype(np.float32)
    if not (np.any(phi < 0.0) and np.any(phi > 0.0)):
        raise ValueError("The PHI field must contain both ignited and unignited cells")
    if np.any(np.isclose(phi, 0.0, atol=1.0e-7)):
        raise ValueError("The PHI interface must lie between cell centers")
    if not (
        np.any((phi > -0.9999) & (phi < 0.0))
        and np.any((phi > 0.0) & (phi < 0.9999))
    ):
        raise ValueError("The PHI interface must contain a resolved transition")
    return phi


def finite_strip_level_set(xx: np.ndarray, yy: np.ndarray) -> np.ndarray:
    """Return a bounded signed-distance field for the finite planar strip."""
    center_x = INITIAL_FRONT_X_M - 0.5 * IGNITION_STRIP_WIDTH_M
    qx = np.abs(xx - center_x) - 0.5 * IGNITION_STRIP_WIDTH_M
    qy = np.abs(yy) - IGNITION_STRIP_HALF_HEIGHT_M
    outside = np.hypot(np.maximum(qx, 0.0), np.maximum(qy, 0.0))
    signed_distance = outside + np.minimum(np.maximum(qx, qy), 0.0)
    return bounded_level_set(signed_distance)


def timestep_for_ros(ros_m_min: float) -> tuple[float, float]:
    """Choose a stable step that advances a positive-speed front 5% of a cell."""
    if not np.isfinite(ros_m_min) or ros_m_min < 0.0:
        raise ValueError(f"Expected ROS must be finite and nonnegative, got {ros_m_min}")
    if ros_m_min <= ZERO_ROS_EPS:
        return ZERO_ROS_TIMESTEP_S, ZERO_ROS_TIMESTEP_S
    speed_m_s = ros_m_min / 60.0
    timestep_s = TARGET_FRONT_ADVANCE_FRACTION * CELL_SIZE_M / speed_m_s
    cfl_limit_s = TARGET_CFL * CELL_SIZE_M / speed_m_s
    timestep_s = min(timestep_s, cfl_limit_s)
    return timestep_s, timestep_s


def make_planar_variant(
    case_dir: Path,
    base_namelist: str,
    *,
    variant_id: str,
    fuel_model: int,
    slope_degrees: float,
    aspect_degrees: float,
    wind_20ft_mph: float,
    wind_from_degrees: float,
    m1_percent: float,
    m10_percent: float,
    m100_percent: float,
    live_herb_percent: float,
    live_woody_percent: float,
    expected_ros_m_min: float,
    tstop_seconds: float,
    canopy_cover_percent: float = 0.0,
    canopy_height_m: float = 0.0,
    fuel_table_directory: str = "./data/misc/",
) -> Path:
    """Create a homogeneous planar-front variant and its concrete namelist."""
    root = case_dir / "variants" / variant_id
    inputs = root / "inputs"
    inputs.mkdir(parents=True, exist_ok=True)
    reset_generated_directory(root / "outputs")
    reset_generated_directory(root / "scratch")

    x = -HALF_WIDTH_M + (np.arange(SIZE) + 0.5) * CELL_SIZE_M
    y = HALF_WIDTH_M - (np.arange(SIZE) + 0.5) * CELL_SIZE_M
    xx, yy = np.meshgrid(x, y)
    # A front that spans the full grid edge is immediately classified as
    # contained by ELMFIRE.  Use a finite strip with a planar east face so the
    # interior measurement corridor still observes one-dimensional spread.
    # ELMFIRE resets PHI below -1.1 as missing data and clamps the evolved
    # field to [-1, 1].  A clipped signed-distance strip preserves a usable
    # interface gradient without depending on an exactly zero-valued cell.
    phi = finite_strip_level_set(xx, yy)
    floats = {
        "slp": slope_degrees,
        "asp": aspect_degrees,
        "ws": wind_20ft_mph,
        "wd": wind_from_degrees,
        "m1": m1_percent,
        "m10": m10_percent,
        "m100": m100_percent,
        "adj": 1.0,
        "phi": phi,
        "dem": 0.0,
        "cc": canopy_cover_percent,
        "ch": 10.0 * canopy_height_m,
        "cbh": 0.0,
        "cbd": 0.0,
    }
    for name, value in floats.items():
        write_raster(inputs / f"{name}.tif", value, "float32")
    write_raster(inputs / "fbfm40.tif", fuel_model, "int16")

    timestep_s, timestep_max_s = timestep_for_ros(expected_ros_m_min)
    if expected_ros_m_min > ZERO_ROS_EPS:
        # ELMFIRE replaces DT with DT_METEOROLOGY when it detects a stalled
        # front. A shortened fractional final step can trigger that branch and
        # corrupt the terminal timestamp. Use a conservative whole-second step
        # and make TSTOP its exact integer multiple.
        timestep_s = max(1.0, float(math.floor(timestep_s)))
        timestep_max_s = timestep_s
        tstop_seconds = timestep_s * math.ceil(tstop_seconds / timestep_s)
    else:
        tstop_seconds = float(math.ceil(tstop_seconds))
    replacements = {
        "FUELS_AND_TOPOGRAPHY_DIRECTORY": f"'./variants/{variant_id}/inputs'",
        "WEATHER_DIRECTORY": f"'./variants/{variant_id}/inputs'",
        "OUTPUTS_DIRECTORY": f"'./variants/{variant_id}/outputs'",
        "SCRATCH": f"'./variants/{variant_id}/scratch'",
        "MISCELLANEOUS_INPUTS_DIRECTORY": f"'{fuel_table_directory}'",
        "LH_MOISTURE_CONTENT": live_herb_percent,
        "LW_MOISTURE_CONTENT": live_woody_percent,
        "SIMULATION_DT": round(timestep_s, 6),
        "SIMULATION_DTMAX": round(timestep_max_s, 6),
        "SIMULATION_TSTOP": float(tstop_seconds),
    }
    config = base_namelist
    for key, value in replacements.items():
        config = replace_assignment(config, key, value)
    path = root / "elmfire.data"
    path.write_text(config, encoding="utf-8")
    return path


def duration_for_ros(ros_m_min: float) -> float:
    """Run long enough for about 300 m of travel while retaining safe bounds."""
    if ros_m_min <= 1.0e-6:
        return 600.0
    return min(21600.0, max(120.0, 300.0 / ros_m_min * 60.0))


def write_expected(case_dir: Path, document: dict[str, object]) -> None:
    for item in document["variants"]:
        item.setdefault(
            "toa_required", float(item.get("expected_ros_m_min", 0.0)) >= 0.10
        )
    path = case_dir / "variants" / "expected.json"
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(document, indent=2) + "\n", encoding="utf-8")
    variant_ids = [str(item["id"]) for item in document["variants"]]
    (case_dir / "variants" / "variant_ids.txt").write_text(
        "\n".join(variant_ids) + "\n", encoding="utf-8"
    )
    output = case_dir / "outputs"
    output.mkdir(parents=True, exist_ok=True)
    status = {
        "case_id": document["case_id"],
        "overall_status": "NOT EVALUABLE",
        "workflow_status": "INCOMPLETE",
        "verification_passed": False,
        "required_outputs_complete": False,
        "required_variant_count": len(variant_ids),
        "completed_variant_count": 0,
        "metrics": [],
        "reason": "Preprocessing started a new run; required ELMFIRE outputs are not complete.",
    }
    (output / "metrics.json").write_text(
        json.dumps(status, indent=2) + "\n", encoding="utf-8"
    )
