#!/usr/bin/env python3
"""Prepare the complete parametric response design.

This script creates the auditable 270-point design and capability manifest. It
does not approximate the required single-structure treatment with native
cell-local ELMFIRE behavior.
"""
from pathlib import Path
import csv
import hashlib
import json
import math

import numpy as np
import rasterio
from rasterio.transform import from_origin

# Customizable preprocessing parameters.
CASE_FILENAME = "case.json"
DESIGN_FILENAME = "outputs/parameter_design.csv"
MANIFEST_FILENAME = "variants/manifest.json"
SEPARATION_FAMILY = "generation_separation"
WIND_FAMILY = "generation_wind"
BUFFER_CELLS = 2
EPSG = 32610
NODATA_FLOAT = -9999.0
NODATA_INT = -9999
BACKGROUND_FUEL_MODEL = 93
STRUCTURE_FUEL_MODEL = 91


def write_tif(path, array, dx, dtype):
    """Write one aligned north-up raster with a two-cell numerical buffer."""
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


def interval_overlap(left, right, interval_left, interval_right):
    """Return the overlap of two half-open physical intervals in metres."""
    return max(0.0, min(right, interval_right) - max(left, interval_left))


def axis_structure_fraction(left, right, size, separation):
    """Return cell coverage by an infinite periodic structure/gap pattern."""
    period = size + separation
    first = math.floor((left - size) / period)
    last = math.ceil(right / period)
    overlap = 0.0
    for index in range(first, last + 1):
        start = index * period
        overlap += interval_overlap(left, right, start, start + size)
    return min(1.0, overlap / (right - left))


def structure_index(center, size, separation):
    """Map one represented cell centre to a zero-based physical lattice index."""
    return max(0, int(math.floor(
        (center + 0.5 * separation) / (size + separation)
    )))


def geometry_fingerprint(input_dir):
    """Hash the generated spatial inputs for stale-artifact detection."""
    digest = hashlib.sha256()
    for path in sorted(input_dir.glob("*.tif")):
        digest.update(path.name.encode("utf-8"))
        digest.update(b"\0")
        digest.update(path.read_bytes())
        digest.update(b"\0")
    return digest.hexdigest()


def write_geometry(case_dir, case, row, name):
    """Create a uniform 2-D structure lattice and ignite its first column."""
    dx = float(row["grid_spacing_m"])
    length = float(case["domain_length_m"])
    width = float(case["domain_width_m"])
    size = float(case["structure_size_m"])
    separation = float(row["separation_m"])
    physical_nx = int(math.ceil(length / dx))
    physical_ny = int(math.ceil(width / dx))
    nx = physical_nx + 2 * BUFFER_CELLS
    ny = physical_ny + 2 * BUFFER_CELLS
    row0 = ny - BUFFER_CELLS - physical_ny
    col0 = BUFFER_CELLS
    region = np.s_[row0:row0 + physical_ny, col0:col0 + physical_nx]

    x_fraction = np.asarray([
        axis_structure_fraction(j * dx, (j + 1) * dx, size, separation)
        for j in range(physical_nx)
    ])
    y_fraction = np.asarray([
        axis_structure_fraction(i * dx, (i + 1) * dx, size, separation)
        for i in range(physical_ny)
    ])
    physical_fraction = np.outer(y_fraction[::-1], x_fraction)
    physical_mask = np.outer(y_fraction[::-1] >= 0.5, x_fraction >= 0.5)

    fbfm = np.full((ny, nx), BACKGROUND_FUEL_MODEL, np.int16)
    fraction = np.zeros((ny, nx), np.float32)
    structure_id = np.zeros((ny, nx), np.int32)
    structure_x_index = np.full((ny, nx), -1, np.int32)
    phi = np.ones((ny, nx), np.float32)
    fbfm[region] = np.where(
        physical_mask, STRUCTURE_FUEL_MODEL, BACKGROUND_FUEL_MODEL
    ).astype(np.int16)
    fraction[region] = physical_fraction.astype(np.float32)

    n_structures_x = int(math.ceil(length / (size + separation)))
    for local_row, local_column in zip(*np.where(physical_mask)):
        x_center = (local_column + 0.5) * dx
        y_center = (physical_ny - local_row - 0.5) * dx
        sx = structure_index(x_center, size, separation)
        sy = structure_index(y_center, size, separation)
        raster_row = row0 + local_row
        raster_column = col0 + local_column
        structure_x_index[raster_row, raster_column] = sx
        structure_id[raster_row, raster_column] = 1 + sx + n_structures_x * sy

    building_mask = structure_id > 0
    if not building_mask.any():
        raise RuntimeError(f"{name}: no structures survived mode sampling")
    first_column = int(structure_x_index[building_mask].min())
    source_mask = building_mask & (structure_x_index == first_column)
    phi[source_mask] = -1.0

    input_dir = case_dir / "variants" / name / "data" / "inputs"
    write_tif(input_dir / "new_fbfm40.tif", fbfm, dx, np.int16)
    write_tif(input_dir / "structure_fraction.tif", fraction, dx, np.float32)
    write_tif(input_dir / "structure_id.tif", structure_id, dx, np.int32)
    write_tif(input_dir / "new_phi.tif", phi, dx, np.float32)
    return {
        "working_directory": str((input_dir.parents[1]).relative_to(case_dir)),
        "physical_nx": physical_nx,
        "physical_ny": physical_ny,
        "buffer_cells": BUFFER_CELLS,
        "n_structures_x": n_structures_x,
        "structure_count": int(np.unique(structure_id[building_mask]).size),
        "source_structure_count": int(np.unique(structure_id[source_mask]).size),
        "source_cell_count": int(source_mask.sum()),
        "geometry_fingerprint": geometry_fingerprint(input_dir),
    }


def design_rows(case):
    """Enumerate both parameter families without duplicate baseline points."""
    rows = []
    for separation in case["separation_distances_m"]:
        for generation in case["generation_rates_pcs_mw_s"]:
            rows.append({
                "family": SEPARATION_FAMILY,
                "generation_rate_pcs_mw_s": generation,
                "separation_m": separation,
                "wind_mph": case["baseline_wind_mph"],
                "grid_spacing_m": case["grid_spacing_m"],
                "cfl": case["cfl"],
            })
    for wind in case["wind_speeds_mph"]:
        for generation in case["generation_rates_pcs_mw_s"]:
            rows.append({
                "family": WIND_FAMILY,
                "generation_rate_pcs_mw_s": generation,
                "separation_m": case["baseline_separation_m"],
                "wind_mph": wind,
                "grid_spacing_m": case["grid_spacing_m"],
                "cfl": case["cfl"],
            })
    return rows


def variant_name(row):
    """Create a filesystem-safe, stable point name."""
    def token(value):
        """Convert one numeric parameter value to a filesystem-safe name token."""
        return f"{value:g}".replace(".", "p")
    generation = token(row["generation_rate_pcs_mw_s"])
    if row["family"] == SEPARATION_FAMILY:
        return "sep{}_gr{}".format(token(row["separation_m"]), generation)
    return "wind{}_gr{}".format(token(row["wind_mph"]), generation)


def main():
    """Run preprocessing from case inputs through final generated artifacts."""
    case_dir = Path(__file__).resolve().parents[1]
    case = json.loads((case_dir / CASE_FILENAME).read_text())
    rows = design_rows(case)
    expected = int(case["metrics"]["design_point_count"])
    if len(rows) != expected:
        raise RuntimeError(f"design has {len(rows)} points; expected {expected}")

    feature = bool(case.get("required_feature_available", False))
    adapter = bool(case["source_adapter_implemented"])
    runnable = feature and adapter
    if not feature:
        reason = ("NOT EVALUABLE: case metadata does not enable "
                  + case["required_feature_keyword"])
    elif not adapter:
        reason = "NOT EVALUABLE: case adapter for the required feature is not enabled"
    else:
        reason = "available"

    design_path = case_dir / DESIGN_FILENAME
    design_path.parent.mkdir(parents=True, exist_ok=True)
    fields = ["name", "family", "generation_rate_pcs_mw_s", "separation_m",
              "wind_mph", "grid_spacing_m", "cfl", "runnable",
              "capability_status"]
    manifest = []
    with design_path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=fields)
        writer.writeheader()
        for row in rows:
            name = variant_name(row)
            geometry = write_geometry(case_dir, case, row, name)
            item = {"name": name, **row, **geometry, "runnable": runnable,
                    "capability_status": reason}
            writer.writerow({field: item[field] for field in fields})
            manifest.append(item)

    digest = hashlib.sha256()
    digest.update((case_dir / CASE_FILENAME).read_bytes())
    digest.update(Path(__file__).read_bytes())
    fingerprint = digest.hexdigest()
    for item in manifest:
        item["design_fingerprint"] = fingerprint
    manifest_path = case_dir / MANIFEST_FILENAME
    manifest_path.parent.mkdir(parents=True, exist_ok=True)
    manifest_path.write_text(json.dumps(manifest, indent=2) + "\n")
    capability = {
        "required_feature": case["required_feature_keyword"],
        "required_feature_available": feature,
        "source_adapter_implemented": adapter,
        "runnable_points": sum(item["runnable"] for item in manifest),
        "total_points": len(manifest),
        "status": reason,
        "design_fingerprint": fingerprint,
    }
    (case_dir / "outputs" / "capability.json").write_text(
        json.dumps(capability, indent=2) + "\n")
    print(f"[OK] prepared {len(manifest)} parametric design points")
    print(f"[INFO] {reason}")


if __name__ == "__main__":
    main()
