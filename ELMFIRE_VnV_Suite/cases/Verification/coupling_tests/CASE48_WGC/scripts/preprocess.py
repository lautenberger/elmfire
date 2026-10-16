#!/usr/bin/env python3
"""Generate deterministic, physically matched WU-E grid-refinement variants."""

from __future__ import annotations

from report_language import polish_figure

import hashlib
import json
import math
import shutil
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

# Fonts are sized for a 7-inch figure printed at full report width.
plt.rcParams.update({
    "font.size": 13, "axes.labelsize": 13, "axes.titlesize": 14,
    "xtick.labelsize": 12, "ytick.labelsize": 12, "legend.fontsize": 12,
    "figure.titlesize": 14, "lines.linewidth": 1.8,
    "pdf.fonttype": 42, "savefig.pad_inches": 0.12,
})

import numpy as np
import rasterio
from rasterio.transform import from_origin


CASE_DIR = Path(__file__).resolve().parents[1]
VARIANT_ROOT = CASE_DIR / "variants"
# ELMFIRE rejects analysis rasters with fewer than 10 rows or columns.  Five
# nonburnable cells on each side keep every refinement level safely above that
# implementation minimum while the evaluated physical community is unchanged.
BUFFER_CELLS = 5
EPSG = 32610
NODATA_FLOAT = -9999.0
NODATA_INT = -9999
URBAN_FUEL = 91
NONBURNABLE_FUEL = 256


def write_tif(path: Path, values: np.ndarray, dx: float) -> None:
    """Write a one-band, north-up GeoTIFF on the case coordinate system."""
    path.parent.mkdir(parents=True, exist_ok=True)
    dtype = values.dtype
    nodata = NODATA_INT if np.issubdtype(dtype, np.integer) else NODATA_FLOAT
    transform = from_origin(
        -BUFFER_CELLS * dx,
        (values.shape[0] - BUFFER_CELLS) * dx,
        dx,
        dx,
    )
    with rasterio.open(
        path,
        "w",
        driver="GTiff",
        width=values.shape[1],
        height=values.shape[0],
        count=1,
        dtype=dtype.name,
        nodata=nodata,
        crs=f"EPSG:{EPSG}",
        transform=transform,
    ) as dataset:
        dataset.write(values, 1)


def fingerprint(variant_dir: Path) -> str:
    """Hash every prepared input that can affect a variant execution."""
    digest = hashlib.sha256()
    paths = [variant_dir / "elmfire.data.in"]
    paths.extend(sorted((variant_dir / "data" / "inputs").glob("*")))
    paths.extend(sorted((variant_dir / "data" / "misc").glob("*")))
    for path in paths:
        digest.update(str(path.relative_to(variant_dir)).encode("utf-8"))
        digest.update(b"\0")
        digest.update(path.read_bytes())
        digest.update(b"\0")
    return digest.hexdigest()


def replace_tokens(template: str, replacements: dict[str, str], variant_id: str) -> str:
    result = template
    for token, value in replacements.items():
        count = result.count(token)
        if count < 1:
            raise ValueError(f"{variant_id}: token {token} is absent")
        result = result.replace(token, value)
    if "@" in result:
        raise ValueError(f"{variant_id}: unresolved token remains in namelist")
    return result


def make_variant(spec: dict, template: str, dx: float) -> dict:
    """Create one grid level with exactly the requested physical support."""
    length = float(spec["physical_domain_length_m"])
    width = float(spec["physical_domain_width_m"])
    source_width = float(spec["initial_source_width_m"])
    if not all(math.isclose(value / dx, round(value / dx), abs_tol=1.0e-10)
               for value in (length, width, source_width)):
        raise ValueError(f"dx={dx:g} does not exactly divide the physical geometry")

    physical_cols = int(round(length / dx))
    physical_rows = int(round(width / dx))
    nx = physical_cols + 2 * BUFFER_CELLS
    ny = physical_rows + 2 * BUFFER_CELLS
    region = np.s_[BUFFER_CELLS:BUFFER_CELLS + physical_rows,
                   BUFFER_CELLS:BUFFER_CELLS + physical_cols]
    zeros = np.zeros((ny, nx), dtype=np.float32)
    ones = np.ones((ny, nx), dtype=np.float32)
    fbfm = np.full((ny, nx), NONBURNABLE_FUEL, dtype=np.int16)
    fbfm[region] = URBAN_FUEL

    x_centres = (np.arange(physical_cols, dtype=float) + 0.5) * dx
    phi_line = np.clip((x_centres - source_width) / dx, -1.0, 1.0)
    phi = np.ones((ny, nx), dtype=np.float32)
    phi[region] = np.broadcast_to(phi_line, (physical_rows, physical_cols))
    source_cells = int(np.count_nonzero(phi[region] <= 0.0))
    expected_source_cells = int(round(source_width / dx)) * physical_rows
    if source_cells != expected_source_cells:
        raise RuntimeError(f"dx={dx:g}: source footprint is not resolution consistent")

    wind = np.full((ny, nx), float(spec["wind_speed_mph"]), dtype=np.float32)
    wind_direction = np.full(
        (ny, nx), float(spec["wind_from_degrees"]), dtype=np.float32
    )
    moisture = np.zeros((ny, nx), dtype=np.float32)
    variant_id = f"dx_{str(dx).replace('.', 'p')}m"
    variant_dir = VARIANT_ROOT / variant_id
    if variant_dir.exists():
        shutil.rmtree(variant_dir)
    input_dir = variant_dir / "data" / "inputs"
    misc_dir = variant_dir / "data" / "misc"
    for directory in (input_dir, misc_dir, variant_dir / "outputs",
                      variant_dir / "logs" / "scratch"):
        directory.mkdir(parents=True, exist_ok=True)

    rasters = {
        "asp": zeros,
        "cbd": zeros,
        "cbh": zeros,
        "cc": zeros,
        "ch": zeros,
        "dem": zeros,
        "slp": zeros,
        "adj": ones,
        "phi": phi,
        "fbfm": fbfm,
        "ws": wind,
        "wd": wind_direction,
        "m1": moisture,
        "m10": moisture,
        "m100": moisture,
    }
    for name, values in rasters.items():
        write_tif(input_dir / f"{name}.tif", values, dx)

    for table in ("fuel_models.csv", "building_fuel_models.csv"):
        source = CASE_DIR / "data" / "misc" / table
        if not source.is_file():
            raise FileNotFoundError(f"missing case-local table: {source}")
        shutil.copyfile(source, misc_dir / table)

    dt = float(spec["fixed_timestep_s"])
    tstop = float(spec["simulation_tstop_s"])
    if not math.isclose(tstop / dt, round(tstop / dt), abs_tol=1.0e-10):
        raise ValueError(f"dx={dx:g}: stop time is not an exact timestep multiple")
    dump_interval = float(spec["dump_interval_s"])
    if not math.isclose(dump_interval / dt, round(dump_interval / dt), abs_tol=1.0e-10):
        raise ValueError(f"dx={dx:g}: dump interval is not an exact timestep multiple")
    band = int(math.ceil(max(length, width) / dx)) + BUFFER_CELLS
    config = replace_tokens(
        template,
        {
            "@DORMANT_IGNITION_TIME@": f"{tstop + dt:.17g}",
            "@BANDTHICKNESS_WUI@": str(band),
        },
        variant_id,
    )
    (variant_dir / "elmfire.data.in").write_text(config, encoding="utf-8")
    digest = fingerprint(variant_dir)
    (variant_dir / "input_fingerprint.txt").write_text(digest + "\n", encoding="utf-8")
    return {
        "id": variant_id,
        "working_directory": str(variant_dir.relative_to(CASE_DIR)),
        "config": "elmfire.data.in",
        "runnable": True,
        "dx_m": dx,
        "dt_s": dt,
        "tstop_s": tstop,
        "dump_interval_s": float(spec["dump_interval_s"]),
        "bandthickness_wui_cells": band,
        "physical_rows": physical_rows,
        "physical_columns": physical_cols,
        "buffer_cells": BUFFER_CELLS,
        "source_cell_count": source_cells,
        "source_area_m2": source_cells * dx * dx,
        "input_fingerprint": digest,
    }


def plot_inputs(representative: dict) -> None:
    """Render evidence directly from a representative prepared variant."""
    variant = CASE_DIR / representative["working_directory"]
    paths = [variant / "data/inputs/fbfm.tif", variant / "data/inputs/phi.tif"]
    arrays = []
    extents = []
    for path in paths:
        with rasterio.open(path) as dataset:
            arrays.append(dataset.read(1, masked=True))
            bounds = dataset.bounds
            extents.append((bounds.left, bounds.right, bounds.bottom, bounds.top))
    fig, axes = plt.subplots(2, 1, figsize=(7.2, 6.4), constrained_layout=True)
    fuel_image = axes[0].imshow(arrays[0], extent=extents[0], origin="upper",
                                vmin=90, vmax=256, cmap="viridis")
    fig.colorbar(fuel_image, ax=axes[0], label="FBFM code")
    phi_image = axes[1].imshow(arrays[1], extent=extents[1], origin="upper",
                               vmin=-1, vmax=1, cmap="coolwarm")
    axes[1].contour(arrays[1], levels=[0], colors="black", linewidths=1,
                    extent=extents[1], origin="upper")
    fig.colorbar(phi_image, ax=axes[1], label=r"Initial $\phi$")
    axes[0].set_title(rf"Urban community, $\Delta x={representative['dx_m']:g}$ m")
    axes[1].set_title(r"Physical ignition strip ($\phi\leq0$)")
    for axis in axes:
        axis.set_xlabel("Easting (m)")
        axis.set_ylabel("Northing (m)")
    (CASE_DIR / "figures").mkdir(parents=True, exist_ok=True)
    polish_figure(fig)
    fig.savefig(
        CASE_DIR / "figures/input_configuration.pdf",
        bbox_inches="tight",
        metadata={"CreationDate": None, "ModDate": None},
    )
    plt.close(fig)


def main() -> None:
    for stale_attempt in (
        CASE_DIR / "logs/run_attempts.json",
        CASE_DIR / "logs/run_attempts.json.tmp",
    ):
        stale_attempt.unlink(missing_ok=True)
    output_dir = CASE_DIR / "outputs"
    output_dir.mkdir(exist_ok=True)
    sentinel = {
        "case_id": "CASE48_WGC",
        "overall_status": "NOT EVALUABLE",
        "workflow_status": "NOT RUN",
        "verification_passed": False,
        "required_outputs_complete": False,
        "required_variant_count": 4,
        "completed_variant_count": 0,
        "reason": "Input regeneration started; no current ELMFIRE result is available.",
        "metrics": [],
    }
    sentinel_tmp = output_dir / "metrics.json.tmp"
    sentinel_tmp.write_text(json.dumps(sentinel, indent=2) + "\n", encoding="utf-8")
    sentinel_tmp.replace(output_dir / "metrics.json")
    for stale in (
        CASE_DIR / "figures/convergence.pdf",
        CASE_DIR / "figures/domain_result.pdf",
        CASE_DIR / "figures/input_configuration.pdf",
        CASE_DIR / "report/case_report.pdf",
        CASE_DIR / "report/metrics_macros.tex",
    ):
        stale.unlink(missing_ok=True)
    spec = json.loads((CASE_DIR / "case.json").read_text(encoding="utf-8"))
    template = (CASE_DIR / "elmfire.data.in").read_text(encoding="utf-8")
    if VARIANT_ROOT.exists():
        shutil.rmtree(VARIANT_ROOT)
    VARIANT_ROOT.mkdir(parents=True)
    variants = [make_variant(spec, template, float(dx))
                for dx in spec["grid_spacings_m"]]
    payload = {
        "case_id": spec["case_id"],
        "source_revision": spec["source_revision"],
        "physical_configuration": {
            "domain_length_m": spec["physical_domain_length_m"],
            "domain_width_m": spec["physical_domain_width_m"],
            "initial_source_width_m": spec["initial_source_width_m"],
            "source_area_m2": (spec["initial_source_width_m"]
                               * spec["physical_domain_width_m"]),
            "building_plan_dimension_m": spec["building_plan_dimension_m"],
            "building_separation_m": spec["building_separation_m"],
        },
        "variants": variants,
    }
    (VARIANT_ROOT / "manifest.json").write_text(
        json.dumps(payload, indent=2) + "\n", encoding="utf-8"
    )
    (VARIANT_ROOT / "variant_ids.txt").write_text(
        "".join(f"{item['id']}\n" for item in variants), encoding="utf-8"
    )
    (VARIANT_ROOT / "run_plan.tsv").write_text(
        "".join(f"{item['id']}\t{item['input_fingerprint']}\n" for item in variants),
        encoding="utf-8",
    )
    representative = min(variants, key=lambda item: abs(item["dx_m"] - 5.0))
    plot_inputs(representative)
    (output_dir / "metrics.json").write_text(
        json.dumps({
            "case_id": spec["case_id"],
            "overall_status": "NOT EVALUABLE",
            "workflow_status": "NOT RUN",
            "verification_passed": False,
            "required_outputs_complete": False,
            "required_variant_count": len(variants),
            "completed_variant_count": 0,
            "reason": "Inputs are prepared, but ELMFIRE has not been run for the current fingerprints.",
            "metrics": [],
        }, indent=2) + "\n",
        encoding="utf-8",
    )
    print(f"[OK] {spec['case_id']}: prepared {len(variants)} spatial levels")


if __name__ == "__main__":
    main()
