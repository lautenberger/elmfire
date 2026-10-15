#!/usr/bin/env python3
"""Generate all CASE43 variants, deterministic rasters, and reference figures."""
from __future__ import annotations

from report_language import polish_figure

import json
import re
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

from input_fingerprint import write_expected_record
from reference import WIND_MPH_TO_MPS, ellipse_ucb


CASE_DIR = Path(__file__).resolve().parents[1]
VARIANT_DIR = CASE_DIR / "variants"
FIGURE_DIR = CASE_DIR / "figures"
OUTPUT_DIR = CASE_DIR / "outputs"
GRID_SIZE = 55
CELL_SIZE_M = 20.0
SOURCE_ROW = GRID_SIZE // 2
SOURCE_COL = GRID_SIZE // 2
BAND_CELLS = 24
NODATA = -9999.0
CRS = "EPSG:32610"
TRANSFORM = from_origin(0.0, GRID_SIZE * CELL_SIZE_M, CELL_SIZE_M, CELL_SIZE_M)
SOURCE_COMMIT = "a2dfbcdf72209733c000e5d3431e44723e15efea"
TOKEN_RE = re.compile(r"@[A-Z][A-Z0-9_]*@")


def write_raster(path: Path, value: float | int | np.ndarray, dtype: str) -> None:
    """Write a single-band, north-up GeoTIFF on the documented case grid."""
    array = value if isinstance(value, np.ndarray) else np.full((GRID_SIZE, GRID_SIZE), value)
    path.parent.mkdir(parents=True, exist_ok=True)
    with rasterio.open(
        path,
        "w",
        driver="GTiff",
        height=GRID_SIZE,
        width=GRID_SIZE,
        count=1,
        dtype=dtype,
        crs=CRS,
        transform=TRANSFORM,
        nodata=NODATA,
        compress="deflate",
    ) as dataset:
        dataset.write(np.asarray(array, dtype=dtype), 1)


def build_variants() -> list[dict[str, object]]:
    """Return the deterministic, deliberately redundant verification matrix."""
    variants: list[dict[str, object]] = []

    def add(
        variant_id: str,
        family: str,
        *,
        speed_mps: float = 12.0,
        hamada_a_m: float = 10.0,
        hamada_d_m: float = 10.0,
        wind_direction_deg: float = 270.0,
        constant_parameters: bool = True,
        source_is_vegetation: bool = False,
    ) -> None:
        wind_speed_mph = speed_mps / WIND_MPH_TO_MPS
        source_fuel_factor = 1.0 / 3.0 if source_is_vegetation else 1.0
        ellipse = ellipse_ucb(wind_speed_mph, hamada_a_m, hamada_d_m, source_fuel_factor)
        variants.append(
            {
                "id": variant_id,
                "family": family,
                "source_wind_mps_requested": speed_mps,
                "wind_speed_mph": wind_speed_mph,
                "source_wind_mps_oracle": ellipse["wind_speed_mps"],
                "wind_direction_deg": wind_direction_deg,
                "hamada_a_m": hamada_a_m,
                "hamada_d_m": hamada_d_m,
                "constant_parameters": constant_parameters,
                "source_is_vegetation": source_is_vegetation,
                "source_fuel_factor": source_fuel_factor,
                "source_row": SOURCE_ROW,
                "source_col": SOURCE_COL,
                "cell_size_m": CELL_SIZE_M,
                "band_cells": BAND_CELLS,
                "peak_hrrpua_kw_m2": 100.0,
                "ellipse": ellipse,
            }
        )

    for label, speed in (
        ("below_10", 9.99),
        ("at_10", 10.0),
        ("above_10", 10.01),
        ("below_17p3", 17.29),
        ("at_17p3", 17.3),
        ("above_17p3", 17.31),
    ):
        add(f"branch_{label}", "wind branch", speed_mps=speed)
    for value in (5.0, 10.0, 20.0, 40.0):
        add(f"area_{int(value):02d}", "Hamada A", hamada_a_m=value)
    for value in (0.0, 10.0, 25.0, 50.0, 75.0):
        add(f"distance_{int(value):02d}", "Hamada D", hamada_d_m=value)
    for direction in (0.0, 90.0, 180.0, 270.0):
        add(f"rotation_wd{int(direction):03d}", "rotation", wind_direction_deg=direction)
    add("equiv_constant", "constant/raster equivalence", constant_parameters=True)
    add("equiv_raster", "constant/raster equivalence", constant_parameters=False)
    add("fuel_urban", "source fuel effect", source_is_vegetation=False)
    add("fuel_forest", "source fuel effect", source_is_vegetation=True)
    ids = [str(item["id"]) for item in variants]
    if len(ids) != len(set(ids)):
        raise ValueError("Variant identifiers must be unique")
    return variants


def expand_namelist(template: str, variant: dict[str, object]) -> str:
    """Expand only the four standalone token directives in the canonical deck."""
    variant_id = str(variant["id"])
    base = f"./variants/{variant_id}"
    x_ign = (SOURCE_COL + 0.5) * CELL_SIZE_M
    y_ign = (GRID_SIZE - SOURCE_ROW - 0.5) * CELL_SIZE_M
    replacements = {
        "@VARIANT_INPUT_DIRECTORIES@": (
            f"FUELS_AND_TOPOGRAPHY_DIRECTORY = '{base}/inputs/'\n"
            f"WEATHER_DIRECTORY = '{base}/inputs/'"
        ),
        "@VARIANT_OUTPUT_DIRECTORY@": f"OUTPUTS_DIRECTORY = '{base}/outputs/'",
        "@VARIANT_DORMANT_IGNITION@": (
            f"X_IGN(1) = {x_ign:.1f}\nY_IGN(1) = {y_ign:.1f}"
        ),
        "@VARIANT_BUILDING_PARAMETERS@": (
            f"BLDG_AREA_CONSTANT = {float(variant['hamada_a_m']):.8g}\n"
            f"BLDG_SEPARATION_DIST_CONSTANT = {float(variant['hamada_d_m']):.8g}\n"
            "BLDG_NONBURNABLE_FRAC_CONSTANT = 0.0\n"
            "BLDG_FOOTPRINT_FRAC_CONSTANT = 1.0\n"
            "USE_CONSTANT_BLDG_SPREAD_MODEL_PARAMS = "
            + (".TRUE." if variant["constant_parameters"] else ".FALSE.")
        ),
        "@VARIANT_SCRATCH_DIRECTORY@": f"SCRATCH = '{base}/scratch/'",
    }
    concrete = template
    for token, value in replacements.items():
        if concrete.count(token) != 1:
            raise ValueError(f"Expected exactly one {token} directive")
        concrete = concrete.replace(token, value)
    remaining = TOKEN_RE.findall(concrete)
    if remaining:
        raise ValueError(f"Unexpanded namelist directives: {remaining}")
    return concrete


def generate_variant(template: str, variant: dict[str, object]) -> None:
    root = VARIANT_DIR / str(variant["id"])
    inputs = root / "inputs"
    outputs = root / "outputs"
    scratch = root / "scratch"
    inputs.mkdir(parents=True, exist_ok=True)
    outputs.mkdir(parents=True, exist_ok=True)
    scratch.mkdir(parents=True, exist_ok=True)

    phi = np.ones((GRID_SIZE, GRID_SIZE), dtype=np.float32)
    phi[SOURCE_ROW, SOURCE_COL] = -1.0
    fbfm = np.full((GRID_SIZE, GRID_SIZE), 93, dtype=np.int16)
    fbfm[SOURCE_ROW, SOURCE_COL] = 1 if variant["source_is_vegetation"] else 91
    building_model = np.ones((GRID_SIZE, GRID_SIZE), dtype=np.int16)
    fields: dict[str, tuple[float | int | np.ndarray, str]] = {
        "phi": (phi, "float32"),
        "fbfm40": (fbfm, "int16"),
        "adj": (0.0, "float32"),
        "asp": (0.0, "float32"),
        "slp": (0.0, "float32"),
        "dem": (0.0, "float32"),
        "cbd": (0.0, "float32"),
        "cbh": (0.0, "float32"),
        "cc": (0.0, "float32"),
        "ch": (0.0, "float32"),
        "m1": (5.0, "float32"),
        "m10": (7.0, "float32"),
        "m100": (9.0, "float32"),
        "ws": (float(variant["wind_speed_mph"]), "float32"),
        "wd": (float(variant["wind_direction_deg"]), "float32"),
        "bldg_area": (float(variant["hamada_a_m"]), "float32"),
        "bldg_separation": (float(variant["hamada_d_m"]), "float32"),
        "bldg_nonburnable": (0.0, "float32"),
        "bldg_footprint": (1.0, "float32"),
        "bldg_fuel_model": (building_model, "int16"),
    }
    for name, (value, dtype) in fields.items():
        write_raster(inputs / f"{name}.tif", value, dtype)
    (root / "elmfire.data").write_text(expand_namelist(template, variant), encoding="utf-8")


def plot_inputs(representative: dict[str, object]) -> None:
    root = VARIANT_DIR / str(representative["id"]) / "inputs"
    names = ('phi', 'fbfm40', 'ws', 'bldg_area')
    labels = ('Initial PHI (-)', 'Fuel model code', 'Wind speed (mph)', 'Building dimension (m)')
    fig, axes = plt.subplots(2, 2, figsize=(7.2, 7.0), constrained_layout=True)
    extent = (0.0, GRID_SIZE * CELL_SIZE_M, 0.0, GRID_SIZE * CELL_SIZE_M)
    for axis, name, label in zip(axes.flat, names, labels):
        with rasterio.open(root / f"{name}.tif") as dataset:
            values = dataset.read(1, masked=True)
        raster = axis.imshow(values, origin="upper", extent=extent, interpolation="nearest")
        unique = np.unique(values.compressed())
        if len(unique) > 1:
            bar = fig.colorbar(raster, ax=axis, orientation="horizontal", shrink=0.85, pad=0.03)
            if len(unique) <= 5:
                bar.set_ticks(unique)
        else:
            label += f"\nUniform: {unique[0]:g}"
        axis.set(xlabel="Easting (m)", ylabel="Northing (m)", title=label)
        axis.tick_params(axis="x", rotation=30)
    fig.suptitle("CASE43 prepared inputs")
    FIGURE_DIR.mkdir(parents=True, exist_ok=True)
    polish_figure(fig)
    fig.savefig(FIGURE_DIR / "input_configuration.pdf", bbox_inches="tight",
                metadata={"CreationDate": None, "ModDate": None})
    plt.close(fig)


def plot_expected(variants: list[dict[str, object]]) -> None:
    fig, axes = plt.subplots(3, 1, figsize=(7.2, 8.0), constrained_layout=True)
    branches = [item for item in variants if item["family"] == "wind branch"]
    speed = [float(item["ellipse"]["wind_speed_mps"]) for item in branches]
    for key, label in (
        ("dist_downwind_m", "downwind"),
        ("dist_upwind_m", "upwind"),
        ("dist_sidewind_m", "sidewind"),
    ):
        axes[0].plot(speed, [float(item["ellipse"][key]) for item in branches], "o-", label=label)
    axes[0].axvline(10.0, color="black", ls="--", lw=0.8)
    axes[0].axvline(17.3, color="black", ls="--", lw=0.8)
    axes[0].set(xlabel="Source wind speed (m/s)", ylabel="Predicted flame reach (m)", title="Reference response at wind-regression boundaries")
    axes[0].legend(fontsize=12)

    area = [item for item in variants if item["family"] == "Hamada A"]
    axes[1].plot(
        [float(item["hamada_a_m"]) for item in area],
        [float(item["ellipse"]["dist_downwind_m"]) for item in area],
        "o-",
    )
    axes[1].set(xlabel="Characteristic building dimension, A (m)", ylabel="Downwind reach (m)", title="Building-dimension response")

    distance = [item for item in variants if item["family"] == "Hamada D"]
    axes[2].plot(
        [float(item["hamada_d_m"]) for item in distance],
        [float(item["ellipse"]["dist_downwind_m"]) for item in distance],
        "o-",
    )
    axes[2].axvline(50.0, color="black", ls="--", lw=0.8, label="50 m separation limit")
    axes[2].set(xlabel="Building separation, D (m)", ylabel="Downwind reach (m)", title="Response to building separation")
    axes[2].legend(fontsize=12)
    for axis in axes:
        axis.grid(alpha=0.25)
    polish_figure(fig)
    fig.savefig(
        FIGURE_DIR / "expected_response.pdf", bbox_inches="tight",
        metadata={"CreationDate": None, "ModDate": None},
    )
    plt.close(fig)


def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    initial_status = {
        "case_id": "CASE43_WER",
        "source_commit": SOURCE_COMMIT,
        "overall_status": "NOT EVALUABLE",
        "workflow_status": "NOT RUN",
        "verification_passed": False,
        "required_outputs_complete": False,
        "required_variant_count": 23,
        "completed_variant_count": 0,
        "executed_binary_sha256": None,
        "metrics": [],
        "reason": "Input regeneration started; no current ELMFIRE result is available.",
    }
    (OUTPUT_DIR / "metrics.json").write_text(
        json.dumps(initial_status, indent=2) + "\n", encoding="utf-8"
    )
    for stale in (
        FIGURE_DIR / "input_configuration.pdf",
        FIGURE_DIR / "expected_response.pdf",
        FIGURE_DIR / "heat_map_comparison.pdf",
        OUTPUT_DIR / "run_attempt.json",
        CASE_DIR / "report/case_report.pdf",
        CASE_DIR / "report/metrics_macros.tex",
    ):
        stale.unlink(missing_ok=True)
    if VARIANT_DIR.exists():
        shutil.rmtree(VARIANT_DIR)
    VARIANT_DIR.mkdir(parents=True)
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    FIGURE_DIR.mkdir(parents=True, exist_ok=True)
    template = (CASE_DIR / "elmfire.data.in").read_text(encoding="utf-8")
    variants = build_variants()
    for variant in variants:
        variant_id = str(variant["id"])
        (CASE_DIR / "logs" / f"{variant_id}.stdout").unlink(missing_ok=True)
        (CASE_DIR / "logs" / f"{variant_id}.stderr").unlink(missing_ok=True)
    for variant in variants:
        generate_variant(template, variant)
        fingerprint = write_expected_record(CASE_DIR, str(variant["id"]))
        variant["input_fingerprint_sha256"] = fingerprint["input_fingerprint_sha256"]
    specification = {
        "case_id": "CASE43_WER",
        "source_commit": SOURCE_COMMIT,
        "grid": {
            "shape": [GRID_SIZE, GRID_SIZE],
            "cell_size_m": CELL_SIZE_M,
            "crs": CRS,
            "transform_gdal": list(TRANSFORM.to_gdal()),
        },
        "curve": {"t_early_s": 1.0, "t_full_developed_s": 100.0, "t_decay_s": 110.0, "peak_hrrpua_kw_m2": 100.0},
        "tolerances": {
            "hrr_normalized_l1": 0.005,
            "heat_map_normalized_l1": 0.005,
            "paired_map_normalized_l1": 0.00001,
            "rotation_normalized_l1": 0.005,
            "unexpected_arrivals": 0,
        },
        "variants": variants,
    }
    (VARIANT_DIR / "expected.json").write_text(json.dumps(specification, indent=2) + "\n", encoding="utf-8")
    (VARIANT_DIR / "variant_ids.txt").write_text(
        "\n".join(str(item["id"]) for item in variants) + "\n", encoding="utf-8"
    )
    status = {
        "case_id": "CASE43_WER",
        "source_commit": SOURCE_COMMIT,
        "overall_status": "NOT EVALUABLE",
        "workflow_status": "NOT RUN",
        "verification_passed": False,
        "required_outputs_complete": False,
        "required_variant_count": len(variants),
        "completed_variant_count": 0,
        "executed_binary_sha256": None,
        "metrics": [],
        "reason": "Inputs and reference expectations were generated; ELMFIRE has not been run.",
    }
    (OUTPUT_DIR / "metrics.json").write_text(json.dumps(status, indent=2) + "\n", encoding="utf-8")
    plot_inputs(next(item for item in variants if item["id"] == "rotation_wd270"))
    plot_expected(variants)
    print(f"[OK] CASE43_WER generated {len(variants)} variants")


if __name__ == "__main__":
    main()
