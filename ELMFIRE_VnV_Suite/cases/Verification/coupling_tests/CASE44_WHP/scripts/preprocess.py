#!/usr/bin/env python3
"""Regenerate every deterministic CASE44 variant and reference-only figure."""
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
from reference import WIND_MPH_TO_MPS, ellipse_ucb, hrr_transient


CASE_DIR = Path(__file__).resolve().parents[1]
VARIANT_DIR = CASE_DIR / "variants"
FIGURE_DIR = CASE_DIR / "figures"
OUTPUT_DIR = CASE_DIR / "outputs"
GRID_SIZE = 55
CELL_SIZE_M = 20.0
SOURCE_CENTER = (27, 27)
SOURCE_LEFT = (27, 24)
SOURCE_RIGHT = (27, 30)
BAND_CELLS = 24
NODATA = -9999.0
CRS = "EPSG:32610"
TRANSFORM = from_origin(0.0, GRID_SIZE * CELL_SIZE_M, CELL_SIZE_M, CELL_SIZE_M)
SOURCE_COMMIT = "a2dfbcdf72209733c000e5d3431e44723e15efea"
WIND_SPEED_MPH = float(np.float32(12.0 / WIND_MPH_TO_MPS))
TOKEN_RE = re.compile(r"@[A-Z][A-Z0-9_]*@")

CURVES: dict[str, dict[str, float | int]] = {
    "base": {"model_id": 1, "t_early_s": 4.0, "t_full_developed_s": 8.0, "t_decay_s": 12.0, "peak_hrrpua_kw_m2": 100.0},
    "double_peak": {"model_id": 2, "t_early_s": 4.0, "t_full_developed_s": 8.0, "t_decay_s": 12.0, "peak_hrrpua_kw_m2": 200.0},
    "short": {"model_id": 3, "t_early_s": 2.0, "t_full_developed_s": 4.0, "t_decay_s": 6.0, "peak_hrrpua_kw_m2": 100.0},
    "long_plateau": {"model_id": 4, "t_early_s": 4.0, "t_full_developed_s": 10.0, "t_decay_s": 14.0, "peak_hrrpua_kw_m2": 100.0},
}

TARGET_MODELS: dict[str, dict[str, float | int]] = {
    "base": {"model_id": 10, "nonburnable_fraction": 0.2, "absorptivity": 0.8},
    "nbf_0": {"model_id": 11, "nonburnable_fraction": 0.0, "absorptivity": 0.8},
    "nbf_0p5": {"model_id": 12, "nonburnable_fraction": 0.5, "absorptivity": 0.8},
    "nbf_1": {"model_id": 13, "nonburnable_fraction": 1.0, "absorptivity": 0.8},
    "abs_0": {"model_id": 14, "nonburnable_fraction": 0.2, "absorptivity": 0.0},
    "abs_0p4": {"model_id": 15, "nonburnable_fraction": 0.2, "absorptivity": 0.4},
    "abs_1": {"model_id": 16, "nonburnable_fraction": 0.2, "absorptivity": 1.0},
}


def write_raster(path: Path, value: float | int | np.ndarray, dtype: str) -> None:
    """Write one north-up, single-band GeoTIFF on the common case grid."""
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


def source(row_col: tuple[int, int], curve: str = "base") -> dict[str, object]:
    return {"row": row_col[0], "col": row_col[1], "curve": curve}


def build_variants() -> list[dict[str, object]]:
    """Return 19 variants with deliberate baseline repetitions for pair tests."""
    variants: list[dict[str, object]] = []

    def add(
        variant_id: str,
        family: str,
        *,
        curve: str = "base",
        target: str = "base",
        hrr_ellipse_adj: float = 1.0,
        sources: list[dict[str, object]] | None = None,
        diagnostic_nonburnable: float = 0.2,
        diagnostic_footprint: float = 0.6,
    ) -> None:
        source_list = sources if sources is not None else [source(SOURCE_CENTER, curve)]
        variants.append(
            {
                "id": variant_id,
                "family": family,
                "sources": source_list,
                "target_model": target,
                "target_model_id": int(TARGET_MODELS[target]["model_id"]),
                "target_nonburnable_fraction": float(TARGET_MODELS[target]["nonburnable_fraction"]),
                "target_absorptivity": float(TARGET_MODELS[target]["absorptivity"]),
                "hrr_ellipse_adj": hrr_ellipse_adj,
                "diagnostic_nonburnable_raster": diagnostic_nonburnable,
                "diagnostic_footprint_raster": diagnostic_footprint,
                "cell_size_m": CELL_SIZE_M,
                "band_cells": BAND_CELLS,
                "wind_speed_mph": WIND_SPEED_MPH,
                "wind_direction_deg": 270.0,
                "hamada_a_m": 10.0,
                "hamada_d_m": 10.0,
            }
        )

    add("curve_base", "HRR curve", curve="base")
    add("curve_double_peak", "HRR curve", curve="double_peak")
    add("curve_short", "HRR curve", curve="short")
    add("curve_long_plateau", "HRR curve", curve="long_plateau")
    add("target_nbf_0", "spatial nonburnable fraction", target="nbf_0", diagnostic_nonburnable=0.0)
    add("target_nbf_0p5", "spatial nonburnable fraction", target="nbf_0p5", diagnostic_nonburnable=0.5)
    add("target_nbf_1", "spatial nonburnable fraction", target="nbf_1", diagnostic_nonburnable=1.0)
    add("target_abs_0", "target table absorptivity", target="abs_0")
    add("target_abs_0p4", "target table absorptivity", target="abs_0p4")
    add("target_abs_1", "target table absorptivity", target="abs_1")
    add("adj_0p25", "HRR_ELLIPSE_ADJ", hrr_ellipse_adj=0.25)
    add("adj_0p5", "HRR_ELLIPSE_ADJ", hrr_ellipse_adj=0.5)
    add("adj_0p75", "HRR_ELLIPSE_ADJ", hrr_ellipse_adj=0.75)
    add("adj_1p0", "HRR_ELLIPSE_ADJ", hrr_ellipse_adj=1.0)
    add("source_left", "source superposition", sources=[source(SOURCE_LEFT)])
    add("source_right", "source superposition", sources=[source(SOURCE_RIGHT)])
    add("source_pair", "source superposition", sources=[source(SOURCE_LEFT), source(SOURCE_RIGHT)])
    add("inert_rasters_low", "spatial raster precedence", diagnostic_nonburnable=0.0, diagnostic_footprint=0.0)
    add("inert_rasters_high", "spatial raster precedence", diagnostic_nonburnable=1.0, diagnostic_footprint=1.0)
    identifiers = [str(item["id"]) for item in variants]
    if len(identifiers) != 19 or len(identifiers) != len(set(identifiers)):
        raise ValueError("CASE44 requires exactly 19 unique variants")
    return variants


def expand_namelist(template: str, variant: dict[str, object]) -> str:
    """Expand only the standalone directives in the canonical input deck."""
    variant_id = str(variant["id"])
    base = f"./variants/{variant_id}"
    first_source = variant["sources"][0]
    x_ign = (int(first_source["col"]) + 0.5) * CELL_SIZE_M
    y_ign = (GRID_SIZE - int(first_source["row"]) - 0.5) * CELL_SIZE_M
    replacements = {
        "@VARIANT_INPUT_DIRECTORIES@": (
            f"FUELS_AND_TOPOGRAPHY_DIRECTORY = '{base}/inputs/'\n"
            f"WEATHER_DIRECTORY = '{base}/inputs/'"
        ),
        "@VARIANT_OUTPUT_DIRECTORY@": f"OUTPUTS_DIRECTORY = '{base}/outputs/'",
        "@VARIANT_DORMANT_IGNITION@": (
            f"X_IGN(1) = {x_ign:.1f}\nY_IGN(1) = {y_ign:.1f}"
        ),
        "@VARIANT_HRR_ELLIPSE_ADJ@": f"HRR_ELLIPSE_ADJ = {float(variant['hrr_ellipse_adj']):.8g}",
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
    (root / "outputs").mkdir(parents=True, exist_ok=True)
    (root / "scratch").mkdir(parents=True, exist_ok=True)
    inputs.mkdir(parents=True, exist_ok=True)

    phi = np.ones((GRID_SIZE, GRID_SIZE), dtype=np.float32)
    fbfm = np.full((GRID_SIZE, GRID_SIZE), 93, dtype=np.int16)
    building_model = np.full(
        (GRID_SIZE, GRID_SIZE), int(variant["target_model_id"]), dtype=np.int16
    )
    for item in variant["sources"]:
        row, col = int(item["row"]), int(item["col"])
        phi[row, col] = -1.0
        fbfm[row, col] = 91
        building_model[row, col] = int(CURVES[str(item["curve"])]["model_id"])

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
        "bldg_nonburnable": (float(variant["diagnostic_nonburnable_raster"]), "float32"),
        "bldg_footprint": (float(variant["diagnostic_footprint_raster"]), "float32"),
        "bldg_fuel_model": (building_model, "int16"),
    }
    for name, (value, dtype) in fields.items():
        write_raster(inputs / f"{name}.tif", value, dtype)
    (root / "elmfire.data").write_text(expand_namelist(template, variant), encoding="utf-8")


def plot_inputs(representative: dict[str, object]) -> None:
    root = VARIANT_DIR / str(representative["id"]) / "inputs"
    names = ('phi', 'bldg_fuel_model', 'bldg_nonburnable', 'bldg_footprint')
    labels = ('Initial PHI (-)', 'Building fuel model', 'Nonburnable fraction', 'Footprint fraction')
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
    fig.suptitle("CASE44 prepared inputs")
    FIGURE_DIR.mkdir(parents=True, exist_ok=True)
    polish_figure(fig)
    fig.savefig(FIGURE_DIR / "input_configuration.pdf", bbox_inches="tight",
                metadata={"CreationDate": None, "ModDate": None})
    plt.close(fig)


def plot_expected() -> None:
    times = np.arange(0.0, 17.0)
    fig, axes = plt.subplots(3, 1, figsize=(7.2, 8.0), constrained_layout=True)
    for name, curve in CURVES.items():
        axes[0].plot(times, [hrr_transient(time, curve) for time in times], marker="o", ms=2.5, label=name.replace("_", " "))
    axes[0].set(xlabel="Time after ignition (s)", ylabel="Heat-release rate\nper unit area\n(kW m$^{-2}$)", title="Source design-fire curves")
    axes[0].legend(fontsize=12)

    targets = ["nbf_0", "base", "nbf_0p5", "nbf_1"]
    x_values = [float(TARGET_MODELS[name]["nonburnable_fraction"]) for name in targets]
    axes[1].plot(x_values, [1.0 - value for value in x_values], "o-", label="Direct-flame-contact multiplier")
    axes[1].plot(x_values, [(1.0 - value) * 0.8 for value in x_values], "s--", label="Radiative heat-transfer multiplier")
    axes[1].set(xlabel="Spatial nonburnable fraction (-)", ylabel="Coefficient (-)", title="Spatial target response")
    axes[1].legend(fontsize=12)

    adjustment = np.array([0.25, 0.5, 0.75, 1.0])
    axes[2].plot(adjustment, adjustment ** -2, "o-")
    axes[2].set(xlabel="Heat-release ellipse scale factor (-)", ylabel="Relative heat-area\ncoefficient, $C_h$ (-)", title="Inverse-square normalization")
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
        "case_id": "CASE44_WHP",
        "source_commit": SOURCE_COMMIT,
        "overall_status": "NOT EVALUABLE",
        "workflow_status": "NOT RUN",
        "verification_passed": False,
        "required_outputs_complete": False,
        "required_variant_count": 19,
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
        FIGURE_DIR / "heat_response_comparison.pdf",
        OUTPUT_DIR / "run_attempt.json",
        CASE_DIR / "report/case_report.pdf",
        CASE_DIR / "report/metrics_macros.tex",
    ):
        stale.unlink(missing_ok=True)
    if VARIANT_DIR.exists():
        shutil.rmtree(VARIANT_DIR)
    VARIANT_DIR.mkdir(parents=True)
    FIGURE_DIR.mkdir(parents=True, exist_ok=True)
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
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
    ellipse = ellipse_ucb(WIND_SPEED_MPH, 10.0, 10.0)
    specification = {
        "case_id": "CASE44_WHP",
        "source_commit": SOURCE_COMMIT,
        "grid": {"shape": [GRID_SIZE, GRID_SIZE], "cell_size_m": CELL_SIZE_M, "crs": CRS, "transform_gdal": list(TRANSFORM.to_gdal())},
        "time": {"start_s": 0.0, "stop_s": 16.0, "dt_s": 1.0, "dump_interval_s": 1.0},
        "curves": CURVES,
        "target_models": TARGET_MODELS,
        "ellipse": ellipse,
        "radiation_distance_m": 100.0,
        "radiation_fraction": 0.35,
        "dfc_fraction": 0.65,
        "default_effective_flame_factor": 1.0,
        "building_dimension_semantics": "linear characteristic building dimension in metres",
        "tolerances": {
            "hrr_normalized_l1": 0.005,
            "heat_map_normalized_l1": 0.005,
            "total_integration_normalized_l1": 0.005,
            "paired_map_normalized_l1": 0.00001,
            "radiation_leakage_kw_m2": 0.000001,
            "unexpected_arrivals": 0,
        },
        "variants": variants,
    }
    (VARIANT_DIR / "expected.json").write_text(json.dumps(specification, indent=2) + "\n", encoding="utf-8")
    (VARIANT_DIR / "variant_ids.txt").write_text("\n".join(str(item["id"]) for item in variants) + "\n", encoding="utf-8")
    status = {
        "case_id": "CASE44_WHP",
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
    plot_inputs(next(item for item in variants if item["id"] == "source_pair"))
    plot_expected()
    print(f"[OK] CASE44_WHP generated {len(variants)} variants")


if __name__ == "__main__":
    main()
