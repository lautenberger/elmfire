#!/usr/bin/env python3
"""Prepare the two-dimensional spatial-resolution resolution sweep.

All user-adjustable parameters are grouped below.  This script generates
inputs only; it never launches ELMFIRE.
"""
from pathlib import Path
import csv
import hashlib
import json
import math
import shutil

import numpy as np
import rasterio
from rasterio.transform import from_origin

# Customizable preprocessing parameters.
CASE_FILENAME = "case.json"
NAMELIST_TEMPLATE = "elmfire.data.in"
VARIANTS_DIRECTORY = "variants"
BUFFER_CELLS = 2
MIN_RASTER_DIMENSION = 10
EPSG = 32610
NODATA_FLOAT = -9999.0
NODATA_INT = -9999
BACKGROUND_FUEL_MODEL = 93
STRUCTURE_FUEL_MODEL = 91
BUILDING_FUEL_MODEL_ID = 14
WIND_DIRECTION_DEG = 270.0
MOISTURE_PERCENT = 0.0
BUILDING_HEIGHT_M = 8.0
BUILDING_FUEL_LOAD_KG_M2 = 1560.0
BUILDING_CRITICAL_FLUX_W_M2 = 10500.0
BUILDING_CRITICAL_TEMPERATURE = 9.0
BUILDING_ABSORPTIVITY = 0.89
BUILDING_NONBURNABLE_FRACTION = 0.5
BUILDING_IGNITION_PROBABILITY_PERCENT = 90.0


def write_tif(path, array, dx, dtype):
    """Write one aligned north-up GeoTIFF."""
    path.parent.mkdir(parents=True, exist_ok=True)
    transform = from_origin(
        -BUFFER_CELLS * dx,
        (array.shape[0] - BUFFER_CELLS) * dx,
        dx,
        dx,
    )
    nodata = NODATA_INT if np.issubdtype(array.dtype, np.integer) else NODATA_FLOAT
    with rasterio.open(
        path, "w", driver="GTiff", height=array.shape[0], width=array.shape[1],
        count=1, dtype=np.dtype(dtype).name, crs=f"EPSG:{EPSG}",
        transform=transform, nodata=nodata, compress="deflate", predictor=2,
    ) as dataset:
        dataset.write(np.asarray(array, dtype=dtype), 1)


def interval_overlap(left, right, a, b):
    """Return the physical overlap length between two one-dimensional intervals."""
    return max(0.0, min(right, b) - max(left, a))


def axis_structure_fraction(left, right, size, separation):
    """Return the fraction covered by repeating structure intervals."""
    period = size + separation
    first = math.floor((left - size) / period)
    last = math.ceil(right / period)
    overlap = 0.0
    for index in range(first, last + 1):
        start = index * period
        overlap += interval_overlap(left, right, start, start + size)
    return min(1.0, overlap / (right - left))


def nearest_structure_index(center, size, separation):
    """Map a cell-center coordinate to the nearest physical structure index."""
    period = size + separation
    return max(0, int(math.floor((center + 0.5 * separation) / period)))


def ignition_coordinates(source_mask, structure_id, dx):
    """Return one ignition point within every upwind-column structure."""
    source_ids = np.unique(structure_id[source_mask])
    source_ids = source_ids[source_ids > 0]
    lines = []
    for index, source_id in enumerate(source_ids, start=1):
        rows, columns = np.where(source_mask & (structure_id == source_id))
        centre = np.argmin(
            (rows - np.mean(rows)) ** 2 + (columns - np.mean(columns)) ** 2
        )
        row, column = int(rows[centre]), int(columns[centre])
        x = (column - BUFFER_CELLS + 0.5) * dx
        y = (source_mask.shape[0] - BUFFER_CELLS - row - 0.5) * dx
        lines.extend((f"X_IGN({index})={x:.8g}", f"Y_IGN({index})={y:.8g}"))
    return int(source_ids.size), "\n".join(lines)


def component_areas(mask, dx):
    """Return four-neighbor component areas without a SciPy dependency."""
    visited = np.zeros(mask.shape, dtype=bool)
    areas = []
    ny, nx = mask.shape
    for row, col in zip(*np.where(mask)):
        if visited[row, col]:
            continue
        stack = [(int(row), int(col))]
        visited[row, col] = True
        count = 0
        while stack:
            rr, cc = stack.pop()
            count += 1
            for nr, nc in ((rr - 1, cc), (rr + 1, cc), (rr, cc - 1), (rr, cc + 1)):
                if 0 <= nr < ny and 0 <= nc < nx and mask[nr,
                                                          nc] and not visited[nr, nc]:
                    visited[nr, nc] = True
                    stack.append((nr, nc))
        areas.append(count * dx * dx)
    return areas


def run_widths(values, target, dx):
    """Measure contiguous represented structure or gap widths along one raster transect."""
    widths = []
    count = 0
    for value in values:
        if bool(value) == target:
            count += 1
        elif count:
            widths.append(count * dx)
            count = 0
    if count:
        widths.append(count * dx)
    return widths


def input_fingerprint(variant_dir):
    """Hash every generated input that can affect one ELMFIRE variant result."""
    digest = hashlib.sha256()
    paths = [variant_dir / "elmfire.data.in"]
    paths.extend(sorted((variant_dir / "data" / "inputs").glob("*")))
    paths.extend(sorted((variant_dir / "data" / "misc").glob("*")))
    for path in paths:
        digest.update(str(path.relative_to(variant_dir)).encode())
        digest.update(b"\0")
        digest.update(path.read_bytes())
        digest.update(b"\0")
    return digest.hexdigest()


def building_model_row(case):
    """Create the case-local structural fuel-model row using documented physical units."""
    values = [
        BUILDING_FUEL_MODEL_ID, "JCH6", case["hrr_growth_end_s"],
        case["hrr_growth_end_s"], case["hrr_steady_end_s"],
        case["hrr_decay_end_s"], BUILDING_FUEL_LOAD_KG_M2,
        case["hrrpua_peak_kw_m2"], BUILDING_CRITICAL_FLUX_W_M2,
        BUILDING_CRITICAL_TEMPERATURE, BUILDING_ABSORPTIVITY,
        BUILDING_HEIGHT_M, BUILDING_NONBURNABLE_FRACTION,
        BUILDING_IGNITION_PROBABILITY_PERCENT, 1.0,
        case["small_ignition_delay_s"],
    ]
    return ",".join(str(value) for value in values)


def write_variant(case_dir, template, case, dx):
    """Rasterize one requested grid spacing with two-dimensional mode sampling."""
    size = float(case["structure_size_m"])
    separation = float(case["structure_separation_m"])
    length = float(case["domain_length_m"])
    width = float(case["domain_width_m"])
    physical_nx = int(math.ceil(length / dx))
    physical_ny = int(math.ceil(width / dx))
    nx = max(MIN_RASTER_DIMENSION, physical_nx + 2 * BUFFER_CELLS)
    ny = max(MIN_RASTER_DIMENSION, physical_ny + 2 * BUFFER_CELLS)
    name = f"dx{dx:g}".replace(".", "p")
    variant_dir = case_dir / VARIANTS_DIRECTORY / name
    inputs = variant_dir / "data" / "inputs"
    misc = variant_dir / "data" / "misc"
    for directory in (
        inputs,
        misc,
        variant_dir /
        "outputs",
        variant_dir /
        "logs" /
            "scratch"):
        directory.mkdir(parents=True, exist_ok=True)

    zeros = np.zeros((ny, nx), np.float32)
    ones = np.ones((ny, nx), np.float32)
    fbfm = np.full((ny, nx), BACKGROUND_FUEL_MODEL, np.int16)
    phi = np.ones((ny, nx), np.float32)
    fraction = np.zeros((ny, nx), np.float32)
    structure_id = np.zeros((ny, nx), np.int32)
    structure_x_index = np.full((ny, nx), -1, np.int32)

    x_fraction = np.array([
        axis_structure_fraction(j * dx, min((j + 1) * dx, length), size, separation)
        for j in range(physical_nx)
    ])
    # Raster rows are north-to-south; physical y only affects the periodic pattern.
    y_fraction = np.array([
        axis_structure_fraction(i * dx, min((i + 1) * dx, width), size, separation)
        for i in range(physical_ny)
    ])
    mode_fraction = np.outer(y_fraction[::-1], x_fraction)
    # Apply the 1-D categorical mode decision on each coordinate before
    # forming square footprints. This deliberately exposes cell-local
    # rasterization: at 7 m, represented sides are 7 or 14 m; at 30 m,
    # one represented cell is
    # a 30 m by 30 m structure.
    physical_mask = np.outer(y_fraction[::-1] >= 0.5, x_fraction >= 0.5)
    row0 = ny - BUFFER_CELLS - physical_ny
    col0 = BUFFER_CELLS
    region = np.s_[row0:row0 + physical_ny, col0:col0 + physical_nx]
    fbfm[region] = np.where(physical_mask, STRUCTURE_FUEL_MODEL,
                            BACKGROUND_FUEL_MODEL).astype(np.int16)
    fraction[region] = mode_fraction.astype(np.float32)

    n_struct_x = int(math.ceil(length / (size + separation)))
    for local_row, local_col in zip(*np.where(physical_mask)):
        x_center = (local_col + 0.5) * dx
        y_center = (physical_ny - local_row - 0.5) * dx
        sx = nearest_structure_index(x_center, size, separation)
        sy = nearest_structure_index(y_center, size, separation)
        row = row0 + local_row
        column = col0 + local_col
        structure_x_index[row, column] = sx
        structure_id[row, column] = 1 + sx + n_struct_x * sy

    building_mask = structure_id > 0
    if not building_mask.any():
        raise RuntimeError(f"{name}: mode resampling removed every structure")
    first_structure_column = int(structure_x_index[building_mask].min())
    source_mask = building_mask & (structure_x_index == first_structure_column)
    phi[source_mask] = -1.0
    num_ignitions, ignition_text = ignition_coordinates(
        source_mask, structure_id, dx
    )

    wind = np.full((ny, nx), case["wind_speed_mph"], np.float32)
    direction = np.full((ny, nx), WIND_DIRECTION_DEG, np.float32)
    moisture = np.full((ny, nx), MOISTURE_PERCENT, np.float32)
    rasters = {
        "asp": (zeros, np.float32), "cbd": (zeros, np.float32),
        "cbh": (zeros, np.float32), "cc": (zeros, np.float32),
        "ch": (zeros, np.float32), "dem": (zeros, np.float32),
        "slp": (zeros, np.float32), "adj": (ones, np.float32),
        "new_phi": (phi, np.float32),
        "new_fbfm40": (fbfm, np.int16),
        "ws": (wind, np.float32), "wd": (direction, np.float32),
        "m1": (moisture, np.float32), "m10": (moisture, np.float32),
        "m100": (moisture, np.float32),
        "structure_fraction": (fraction, np.float32),
        "structure_id": (structure_id, np.int32),
    }
    for filename, (array, dtype) in rasters.items():
        write_tif(inputs / f"{filename}.tif", array, dx, dtype)

    def building_values(value, dtype):
        """Broadcast one physical building property over represented structure cells and nodata elsewhere."""
        return np.where(building_mask, value, NODATA_FLOAT).astype(dtype)
    props = {
        "bldg_area": (size, np.float32, np.float32),
        "bldg_sep": (separation, np.float32, np.float32),
        "bldg_nonburnable": (BUILDING_NONBURNABLE_FRACTION, np.float32, np.float32),
        "bldg_footprint_frac": (1.0, np.float32, np.float32),
    }
    for filename, (value, dtype, gtype) in props.items():
        write_tif(inputs / f"{filename}.tif", building_values(value, dtype), dx, gtype)
    write_tif(
        inputs /
        "bldg_fuel_model.tif",
        np.where(
            building_mask,
            BUILDING_FUEL_MODEL_ID,
            NODATA_INT).astype(
            np.int16),
        dx,
        np.int16)

    shutil.copyfile(case_dir / "data" / "misc" / "fuel_models.csv",
                    misc / "fuel_models.csv")
    original_models = (
        case_dir /
        "data" /
        "misc" /
        "building_fuel_models.csv").read_text().rstrip()
    (misc / "building_fuel_models.csv").write_text(
        original_models + "\n" + building_model_row(case) + "\n")

    dt = float(case["cfl"]) * dx / (float(case["wind_speed_mph"]) * 0.44704)
    requested_tstop = float(case["simulation_tstop_s"])
    step_count = int(math.ceil(requested_tstop / dt - 1.0e-12))
    aligned_tstop = step_count * dt
    config = template
    replacements = {
        "@DT@": f"{dt:.17g}",
        "@TSTOP@": f"{aligned_tstop:.17g}",
        "@NUM_IGNITIONS@": str(num_ignitions),
        "@IGNITION_COORDINATES@": ignition_text,
    }
    for token, value in replacements.items():
        config = config.replace(token, value)
    if "@" in config:
        raise RuntimeError(f"{name}: unresolved namelist token")
    (variant_dir / "elmfire.data.in").write_text(config)

    physical_building = building_mask[region]
    areas = component_areas(physical_building, dx)
    sample_rows = np.where(physical_building.any(axis=1))[0]
    sample = physical_building[int(
        sample_rows[len(sample_rows) // 2])] if sample_rows.size else np.zeros(physical_nx, bool)
    building_widths = run_widths(sample, True, dx)
    gap_widths = run_widths(sample, False, dx)
    return {
        "name": name, "dx_m": dx, "working_directory": str(variant_dir.relative_to(case_dir)),
        "config": "elmfire.data.in", "runnable": True,
        "input_fingerprint": input_fingerprint(variant_dir),
        "nx": nx, "ny": ny, "physical_nx": physical_nx, "physical_ny": physical_ny,
        "buffer_cells": BUFFER_CELLS,
        "n_structures_x": n_struct_x,
        "simulation_dt_s": dt,
        "simulation_tstop_s": aligned_tstop,
        "requested_tstop_s": requested_tstop, "step_count": step_count,
        "source_structure_count": num_ignitions,
        "source_cell_count": int(source_mask.sum()),
        "building_cell_count": int(building_mask.sum()),
        "component_count": len(areas),
        "component_area_min_m2": min(areas) if areas else None,
        "component_area_median_m2": float(np.median(areas)) if areas else None,
        "component_area_max_m2": max(areas) if areas else None,
        "building_width_min_m": min(building_widths) if building_widths else None,
        "building_width_max_m": max(building_widths) if building_widths else None,
        "gap_width_min_m": min(gap_widths) if gap_widths else None,
        "gap_width_max_m": max(gap_widths) if gap_widths else None,
    }


def main():
    """Run preprocessing from case inputs through final generated artifacts."""
    case_dir = Path(__file__).resolve().parents[1]
    case = json.loads((case_dir / CASE_FILENAME).read_text())
    template = (case_dir / NAMELIST_TEMPLATE).read_text()
    manifest = [write_variant(case_dir, template, case, float(dx))
                for dx in case["dx_values_m"]]
    variants_dir = case_dir / VARIANTS_DIRECTORY
    variants_dir.mkdir(parents=True, exist_ok=True)
    (variants_dir / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
    (case_dir / "outputs").mkdir(parents=True, exist_ok=True)
    with (case_dir / "outputs" / "geometry_summary.csv").open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=[
            "name", "dx_m", "component_count", "component_area_min_m2",
            "component_area_median_m2", "component_area_max_m2",
            "building_width_min_m", "building_width_max_m",
            "gap_width_min_m", "gap_width_max_m"])
        writer.writeheader()
        writer.writerows({key: item.get(key) for key in writer.fieldnames}
                         for item in manifest)
    print(f"[OK] prepared {len(manifest)} spatial-resolution variants")


if __name__ == "__main__":
    main()
