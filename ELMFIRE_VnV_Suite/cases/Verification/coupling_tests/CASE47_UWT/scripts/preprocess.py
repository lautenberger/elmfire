#!/usr/bin/env python3
"""Generate the complete deterministic CASE47_UWT factorial experiment."""
from __future__ import annotations

import hashlib
import json
import math
import re
import shutil
from pathlib import Path

import numpy as np
import rasterio
from rasterio.transform import from_origin

CASE_DIR = Path(__file__).resolve().parents[1]
CASE_ID = "CASE47_UWT"
SOURCE_COMMIT = "a2dfbcdf72209733c000e5d3431e44723e15efea"
NX, NY = 31, 31
DX = 10.0
TRANSFORM = from_origin(0.0, NY * DX, DX, DX)
CRS = "EPSG:32610"
NODATA = -9999.0
HEAT_SOURCE = (8, 10)
ISOLATED = (8, 13)
PATH_SOURCE = (22, 10)
NEAR = (22, 11)
BARRIER = (22, 12)
DISTAL = (22, 13)
PEAKS = (100, 400)
ADJUSTMENTS = (0, 1)
CORRIDORS = ("open", "barrier")
TSTOP = 600.0
DT = 1.0


def replace_assignment(text: str, key: str, value: object) -> str:
    pattern = rf"(?mi)^(\s*{re.escape(key)}\s*=\s*).*$"
    result, count = re.subn(pattern, rf"\g<1>{value}", text, count=1)
    if count != 1:
        raise KeyError(f"Expected one assignment for {key}; found {count}")
    return result


def write_raster(path: Path, values: np.ndarray, dtype: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with rasterio.open(
        path,
        "w",
        driver="GTiff",
        height=NY,
        width=NX,
        count=1,
        dtype=dtype,
        crs=CRS,
        transform=TRANSFORM,
        nodata=NODATA,
        compress="deflate",
    ) as target:
        target.write(np.asarray(values, dtype=dtype), 1)


def constant(value: float, dtype: str = "float32") -> np.ndarray:
    return np.full((NY, NX), value, dtype=dtype)


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def world(row_col: tuple[int, int]) -> tuple[float, float]:
    row, col = row_col
    return (DX * (col + 0.5), NY * DX - DX * (row + 0.5))


def level_set() -> np.ndarray:
    """One-cell urban ignition with a bounded, resolved interface."""
    source_centres = (world(HEAT_SOURCE), world(PATH_SOURCE))
    x = (np.arange(NX) + 0.5) * DX
    y = NY * DX - (np.arange(NY) + 0.5) * DX
    xx, yy = np.meshgrid(x, y)
    signed_distance = np.minimum.reduce(
        [np.hypot(xx - px, yy - py) - 0.45 * DX for px, py in source_centres]
    )
    phi = np.clip(signed_distance / DX, -1.0, 1.0).astype(np.float32)
    if np.count_nonzero(phi < 0.0) != 2 or np.any(np.isclose(phi, 0.0)):
        raise RuntimeError("Initial PHI must ignite exactly two separated urban cells")
    return phi


def fuel_layout(corridor: str) -> np.ndarray:
    fbfm = constant(99, "int16")
    fbfm[HEAT_SOURCE] = 91
    fbfm[PATH_SOURCE] = 91
    fbfm[NEAR] = 1
    fbfm[DISTAL] = 1
    fbfm[ISOLATED] = 1
    if corridor == "open":
        fbfm[BARRIER] = 1
    elif corridor != "barrier":
        raise ValueError(corridor)
    return fbfm


def make_variant(base: str, peak: int, adjustment: int, corridor: str) -> dict[str, object]:
    variant_id = f"hrr{peak:03d}_adj{adjustment}_{corridor}"
    root = CASE_DIR / "variants" / variant_id
    inputs = root / "inputs"
    misc = inputs / "misc"
    outputs = root / "outputs"
    scratch = root / "scratch"
    inputs.mkdir(parents=True)
    misc.mkdir()
    outputs.mkdir()
    scratch.mkdir()

    fbfm = fuel_layout(corridor)
    adj = constant(0.0)
    if adjustment == 1:
        adj[fbfm == 1] = 1.0
    rasters: dict[str, tuple[np.ndarray, str]] = {
        "fbfm40": (fbfm, "int16"),
        "phi": (level_set(), "float32"),
        "adj": (adj, "float32"),
        "dem": (constant(0.0), "float32"),
        "slp": (constant(0.0), "float32"),
        "asp": (constant(0.0), "float32"),
        "cc": (constant(0.0), "float32"),
        "ch": (constant(0.0), "float32"),
        "cbh": (constant(0.0), "float32"),
        "cbd": (constant(0.0), "float32"),
        "ws": (constant(20.0), "float32"),
        "wd": (constant(270.0), "float32"),
        "m1": (constant(5.0), "float32"),
        "m10": (constant(7.0), "float32"),
        "m100": (constant(9.0), "float32"),
    }
    for name, (array, dtype) in rasters.items():
        write_raster(inputs / f"{name}.tif", array, dtype)

    building_table = f"building_{'low' if peak == 100 else 'high'}.csv"
    shutil.copy2(CASE_DIR / "data/misc/fuel_models.csv", misc / "fuel_models.csv")
    shutil.copy2(CASE_DIR / f"data/misc/{building_table}", misc / building_table)

    replacements = {
        "FUELS_AND_TOPOGRAPHY_DIRECTORY": f"'./variants/{variant_id}/inputs'",
        "WEATHER_DIRECTORY": f"'./variants/{variant_id}/inputs'",
        "OUTPUTS_DIRECTORY": f"'./variants/{variant_id}/outputs'",
        "SCRATCH": f"'./variants/{variant_id}/scratch'",
        "MISCELLANEOUS_INPUTS_DIRECTORY": f"'./variants/{variant_id}/inputs/misc'",
        "BUILDING_FUEL_MODEL_FILE": f"'{building_table}'",
    }
    config = base
    for key, value in replacements.items():
        config = replace_assignment(config, key, value)
    (root / "elmfire.data").write_text(config, encoding="utf-8")

    manifest = {
        "id": variant_id,
        "source_hrrpua_peak_kw_m2": float(peak),
        "wildland_adjustment": float(adjustment),
        "corridor": corridor,
        "source_commit": SOURCE_COMMIT,
        "grid": {
            "shape_rows_cols": [NY, NX],
            "cell_size_m": DX,
            "crs": CRS,
            "geotransform": list(TRANSFORM)[:6],
            "nodata": NODATA,
        },
        "cells": {
            "heat_source": {"row_col_zero_based": list(HEAT_SOURCE), "xy_m": list(world(HEAT_SOURCE)), "fbfm": 91},
            "propagation_source": {"row_col_zero_based": list(PATH_SOURCE), "xy_m": list(world(PATH_SOURCE)), "fbfm": 91},
            "adjacent_wildland": {"row_col_zero_based": list(NEAR), "xy_m": list(world(NEAR)), "fbfm": 1},
            "corridor_control": {"row_col_zero_based": list(BARRIER), "xy_m": list(world(BARRIER)), "fbfm": 1 if corridor == "open" else 99},
            "distal_wildland": {"row_col_zero_based": list(DISTAL), "xy_m": list(world(DISTAL)), "fbfm": 1},
            "isolated_wildland": {"row_col_zero_based": list(ISOLATED), "xy_m": list(world(ISOLATED)), "fbfm": 1},
        },
        "timing": {"dt_s": DT, "dtmax_s": DT, "tstop_s": TSTOP, "dump_s": 60.0},
        "disabled": {"firebrands": True, "crown_fire": True, "suppression": True},
    }
    manifest_path = root / "input_manifest.json"
    manifest_path.write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    artifact_paths = sorted(path for path in inputs.rglob("*") if path.is_file())
    artifact_paths.append(root / "elmfire.data")
    manifest["artifact_sha256"] = {
        str(path.relative_to(root)): sha256(path) for path in artifact_paths
    }
    fingerprint = hashlib.sha256(
        json.dumps(manifest["artifact_sha256"], sort_keys=True).encode("utf-8")
    ).hexdigest()
    manifest["input_fingerprint"] = fingerprint
    (root / "input_fingerprint.txt").write_text(fingerprint + "\n", encoding="utf-8")
    manifest_path.write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return manifest


def main() -> None:
    for stale_attempt in (
        CASE_DIR / "logs/run_attempts.json",
        CASE_DIR / "logs/run_attempts.json.tmp",
    ):
        stale_attempt.unlink(missing_ok=True)
    for pattern in ("*.stdout", "*.stderr"):
        for stale_log in (CASE_DIR / "logs").glob(pattern):
            stale_log.unlink()
    output_dir = CASE_DIR / "outputs"
    output_dir.mkdir(exist_ok=True)
    sentinel = {
        "case_id": CASE_ID,
        "overall_status": "NOT EVALUABLE",
        "workflow_status": "NOT RUN",
        "verification_passed": False,
        "required_outputs_complete": False,
        "required_variant_count": 8,
        "completed_variant_count": 0,
        "implementation_characterization_status": "NOT EVALUABLE",
        "intended_capability_status": "NOT EVALUABLE",
        "metrics": [],
        "reason": "Input regeneration started; no current ELMFIRE result is available.",
        "source_commit": SOURCE_COMMIT,
    }
    sentinel_tmp = output_dir / "metrics.json.tmp"
    sentinel_tmp.write_text(json.dumps(sentinel, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    sentinel_tmp.replace(output_dir / "metrics.json")
    for stale in (
        CASE_DIR / "figures/implementation_characterization.pdf",
        CASE_DIR / "figures/whole_domain_result.pdf",
        CASE_DIR / "figures/input_configuration.pdf",
        CASE_DIR / "report/case_report.pdf",
        CASE_DIR / "report/metrics_macros.tex",
    ):
        stale.unlink(missing_ok=True)
    if not math.isclose(TSTOP / DT, round(TSTOP / DT), abs_tol=1.0e-12):
        raise RuntimeError("TSTOP must be an exact multiple of DT")
    gate = json.loads((CASE_DIR / "data/misc/source_capability.json").read_text(encoding="utf-8"))
    if gate.get("source_commit") != SOURCE_COMMIT or gate.get("supported") is not False:
        raise RuntimeError("The embedded U-to-W source-capability gate is missing or inconsistent")
    variants_dir = CASE_DIR / "variants"
    if variants_dir.exists():
        shutil.rmtree(variants_dir)
    variants_dir.mkdir()
    base = (CASE_DIR / "elmfire.data.in").read_text(encoding="utf-8")
    records = [
        make_variant(base, peak, adjustment, corridor)
        for peak in PEAKS
        for adjustment in ADJUSTMENTS
        for corridor in CORRIDORS
    ]
    ids = [str(record["id"]) for record in records]
    (variants_dir / "variant_ids.txt").write_text("\n".join(ids) + "\n", encoding="utf-8")
    experiment = {
        "case_id": CASE_ID,
        "source_commit": SOURCE_COMMIT,
        "variant_count": len(records),
        "factor_order": ["source_hrrpua_peak_kw_m2", "wildland_adjustment", "corridor"],
        "variants": records,
        "implementation_oracles": {
            "source_peak_ratio_high_over_low": 4.0,
            "received_heat_ratio_high_over_low": 4.0,
            "isolated_receiver_heat_positive": True,
            "isolated_receiver_arrival_in_current_source": False,
            "barrier_is_nonburnable": True,
            "open_adj1_distal_arrival_is_ordinary_surface_path": True,
        },
        "intended_heat_only_u_to_w_capability": {
            "status": "NOT EVALUABLE",
            "capability_gate": "data/misc/source_capability.json",
            "reason": "The pinned implementation has no heat-to-ignition state transition for FBFM != 91 targets.",
        },
    }
    (variants_dir / "manifest.json").write_text(
        json.dumps(experiment, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )
    status = {
        "case_id": CASE_ID,
        "overall_status": "NOT EVALUABLE",
        "workflow_status": "NOT RUN",
        "verification_passed": False,
        "required_outputs_complete": False,
        "required_variant_count": len(records),
        "completed_variant_count": 0,
        "implementation_characterization_status": "NOT EVALUABLE",
        "intended_capability_status": "NOT EVALUABLE",
        "metrics": [],
        "reason": "Inputs are prepared, but ELMFIRE has not been run for the eight required variants.",
        "source_commit": SOURCE_COMMIT,
    }
    (output_dir / "metrics.json").write_text(
        json.dumps(status, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )
    print(f"[OK] {CASE_ID}: generated {len(records)} deterministic variants")


if __name__ == "__main__":
    main()
