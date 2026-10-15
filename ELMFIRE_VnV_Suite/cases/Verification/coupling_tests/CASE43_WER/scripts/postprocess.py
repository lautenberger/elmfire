#!/usr/bin/env python3
"""Fail-closed comparison of CASE43 ELMFIRE rasters with the local oracle."""
from __future__ import annotations

from report_language import polish_figure

import csv
import json
import math
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

from input_fingerprint import (
    build_fingerprint,
    evaluator_artifact_hashes,
    output_artifact_hashes,
    sha256_file,
)
from reference import heat_maps, hrr_transient


CASE_DIR = Path(__file__).resolve().parents[1]
EXPECTED_PATH = CASE_DIR / "variants" / "expected.json"
METRICS_PATH = CASE_DIR / "outputs" / "metrics.json"


def normalized_l1(reference: np.ndarray, actual: np.ndarray) -> float:
    denominator = float(np.sum(np.abs(reference)))
    numerator = float(np.sum(np.abs(actual - reference)))
    if denominator <= 1.0e-14:
        return 0.0 if numerator <= 1.0e-14 else math.inf
    return numerator / denominator


def validate_grid(dataset: rasterio.io.DatasetReader, grid: dict[str, object], path: Path) -> None:
    expected_shape = tuple(int(value) for value in grid["shape"])
    expected_transform = rasterio.Affine.from_gdal(*grid["transform_gdal"])
    expected_crs = rasterio.crs.CRS.from_string(str(grid["crs"]))
    if dataset.count != 1:
        raise ValueError(f"{path.name} has {dataset.count} bands, expected one")
    if dataset.shape != expected_shape:
        raise ValueError(f"{path.name} has shape {dataset.shape}, expected {expected_shape}")
    if dataset.crs != expected_crs or not dataset.transform.almost_equals(expected_transform):
        raise ValueError(f"{path.name} grid metadata differs from the prepared-input grid")


def read_raster(path: Path, grid: dict[str, object]) -> np.ndarray:
    with rasterio.open(path) as dataset:
        validate_grid(dataset, grid, path)
        masked = dataset.read(1, masked=True)
        if np.any(np.ma.getmaskarray(masked)):
            raise ValueError(f"{path.name} contains nodata pixels")
        values = np.asarray(masked, dtype=np.float64)
    if not np.all(np.isfinite(values)):
        raise ValueError(f"{path.name} contains non-finite values")
    return values


def one_output(output_dir: Path, pattern: str) -> Path:
    matches = sorted(output_dir.glob(pattern))
    if len(matches) != 1:
        raise ValueError(f"expected one {pattern}, found {len(matches)}")
    return matches[0]


def require_current_success(
    variant: dict[str, object], specification: dict[str, object]
) -> dict[str, object]:
    """Require current-input identity and an exit-zero marker for this variant."""
    variant_id = str(variant["id"])
    root = CASE_DIR / "variants" / variant_id
    current = build_fingerprint(CASE_DIR, variant_id)
    current_digest = str(current["input_fingerprint_sha256"])
    expected_digest = str(variant.get("input_fingerprint_sha256", ""))
    if not expected_digest or current_digest != expected_digest:
        raise ValueError("current runtime-input fingerprint differs from expected.json")
    record_path = root / "input_fingerprint.json"
    record = json.loads(record_path.read_text(encoding="utf-8"))
    if record.get("input_fingerprint_sha256") != current_digest:
        raise ValueError("input_fingerprint.json does not match current runtime inputs")
    marker_path = root / "outputs" / "completion_marker.json"
    if not marker_path.is_file():
        raise ValueError("successful-run completion marker is absent")
    marker = json.loads(marker_path.read_text(encoding="utf-8"))
    required = {
        "case_id": "CASE43_WER",
        "variant_id": variant_id,
        "source_commit": specification["source_commit"],
        "status": "ELMFIRE_EXIT_0",
        "input_fingerprint_sha256": current_digest,
    }
    for key, expected in required.items():
        if marker.get(key) != expected:
            raise ValueError(f"completion marker {key} does not match current run")
    if not marker.get("completed_utc"):
        raise ValueError("completion marker has no completion timestamp")
    executable_digest = str(marker.get("executable_sha256", ""))
    if (
        not marker.get("executable_requested")
        or not marker.get("executable_resolved")
        or len(executable_digest) != 64
        or any(character not in "0123456789abcdef" for character in executable_digest.lower())
    ):
        raise ValueError("completion marker does not identify the executed binary")
    executable_path = Path(str(marker["executable_resolved"])).resolve()
    if not executable_path.is_file() or sha256_file(executable_path) != executable_digest:
        raise ValueError("executed binary is absent or differs from the completion marker")
    stdout_path = CASE_DIR / str(marker.get("stdout_path", ""))
    stdout_digest = str(marker.get("stdout_sha256", ""))
    if (
        not stdout_path.is_file()
        or len(stdout_digest) != 64
        or sha256_file(stdout_path) != stdout_digest
    ):
        raise ValueError("ELMFIRE stdout is absent or differs from the completion marker")
    stderr_path = CASE_DIR / str(marker.get("stderr_path", ""))
    stderr_digest = str(marker.get("stderr_sha256", ""))
    if (
        not stderr_path.is_file()
        or len(stderr_digest) != 64
        or sha256_file(stderr_path) != stderr_digest
    ):
        raise ValueError("ELMFIRE stderr is absent or differs from the completion marker")
    evaluator_artifacts = marker.get("evaluator_artifact_sha256")
    if evaluator_artifacts != evaluator_artifact_hashes(CASE_DIR):
        raise ValueError("case contract or evaluator differs from the completion marker")
    expected_artifacts = marker.get("output_artifact_sha256")
    current_artifacts = output_artifact_hashes(root)
    if not isinstance(expected_artifacts, dict) or not expected_artifacts:
        raise ValueError("completion marker does not bind an output snapshot")
    if expected_artifacts != current_artifacts:
        raise ValueError("current outputs differ from the completion-marker snapshot")
    return {
        "input_fingerprint_sha256": current_digest,
        "completion_receipt": marker_path.relative_to(CASE_DIR).as_posix(),
        "executable_sha256": executable_digest,
        "stdout_sha256": stdout_digest,
        "stderr_sha256": stderr_digest,
        "evaluator_artifact_sha256": evaluator_artifacts,
        "output_artifact_sha256": current_artifacts,
    }


def load_variant(
    variant: dict[str, object],
    specification: dict[str, object],
    shape: tuple[int, int],
) -> dict[str, object]:
    root = CASE_DIR / "variants" / str(variant["id"])
    output = root / "outputs"
    receipt = require_current_success(variant, specification)
    manifests = sorted(output.glob("dump_times_*.csv"))
    if len(manifests) != 1:
        raise ValueError(f"expected one dump-times manifest, found {len(manifests)}")
    with manifests[0].open(newline="", encoding="utf-8") as stream:
        rows = list(csv.DictReader(stream, skipinitialspace=True))
    required = {"dump_index", "time_seconds", "is_final_dump"}
    if not rows or not required.issubset(rows[0]):
        raise ValueError("dump-times manifest is empty or malformed")
    manifest_times = np.asarray([float(row["time_seconds"]) for row in rows])
    expected_times = np.asarray([60.0, 120.0])
    if manifest_times.shape != expected_times.shape or not np.allclose(
        manifest_times, expected_times, rtol=0.0, atol=1.0e-6
    ):
        raise ValueError("dump times must be exactly 60 and 120 s")
    indices = [int(row["dump_index"]) for row in rows]
    if len(indices) != len(set(indices)):
        raise ValueError("dump indices are not unique")
    final_rows = [
        index for index, row in enumerate(rows)
        if row["is_final_dump"].strip().upper() in {"T", "TRUE", "1"}
    ]
    if final_rows != [len(rows) - 1]:
        raise ValueError("only the terminal 120 s dump may be final")

    times: list[float] = []
    hrr_fields: list[np.ndarray] = []
    dfc_fields: list[np.ndarray] = []
    rad_fields: list[np.ndarray] = []
    for row in rows:
        index = int(row["dump_index"])
        times.append(float(row["time_seconds"]))
        hrr_fields.append(read_raster(one_output(output, f"hrr_transient_*_d{index:07d}.tif"), specification["grid"]))
        dfc_fields.append(read_raster(one_output(output, f"hf_dfc_transient_*_d{index:07d}.tif"), specification["grid"]))
        rad_fields.append(read_raster(one_output(output, f"hf_rad_transient_*_d{index:07d}.tif"), specification["grid"]))
    times_array = np.asarray(times, dtype=float)
    if np.any(np.diff(times_array) <= 0.0):
        raise ValueError("dump times must be strictly increasing")
    hrr_stack = np.stack(hrr_fields)
    dfc_stack = np.stack(dfc_fields)
    rad_stack = np.stack(rad_fields)

    expected_hrr = np.zeros_like(hrr_stack)
    for index, time_s in enumerate(times_array):
        expected_hrr[index, int(variant["source_row"]), int(variant["source_col"])] = hrr_transient(time_s)
    expected_heat = [
        heat_maps(
            shape,
            int(variant["source_row"]),
            int(variant["source_col"]),
            cell_size_m=float(variant["cell_size_m"]),
            band_cells=int(variant["band_cells"]),
            wind_direction_deg=float(variant["wind_direction_deg"]),
            wind_speed_mph=float(variant["wind_speed_mph"]),
            hamada_a_m=float(variant["hamada_a_m"]),
            hamada_d_m=float(variant["hamada_d_m"]),
            hrrpua_kw_m2=hrr_transient(float(time_s)),
        )
        for time_s in times_array
    ]
    expected_dfc_stack = np.stack([item[0] for item in expected_heat])
    expected_rad_stack = np.stack([item[1] for item in expected_heat])
    selected = int(np.argmax([hrr_transient(time_s) for time_s in times_array]))
    source_mask = np.zeros(shape, dtype=bool)
    source_mask[int(variant["source_row"]), int(variant["source_col"])] = True
    terminal_stamp = int(math.floor(float(times_array[-1]) + 0.5))
    toa_path = one_output(output, f"time_of_arrival_*_{terminal_stamp:07d}.tif")
    with rasterio.open(toa_path) as dataset:
        validate_grid(dataset, specification["grid"], toa_path)
        arrivals = dataset.read(1, masked=True)
        arrival_count = int(arrivals.count())
        arrival_mask = ~np.ma.getmaskarray(arrivals)
    arrival_mask_mismatch = int(np.count_nonzero(arrival_mask != source_mask))
    unexpected_hrr_cells = max(
        int(np.count_nonzero(np.abs(field[~source_mask]) > 1.0e-7)) for field in hrr_stack
    )
    return {
        "id": variant["id"],
        "times": times_array,
        "hrr_error": normalized_l1(expected_hrr, hrr_stack),
        "dfc_error": normalized_l1(expected_dfc_stack, dfc_stack),
        "rad_error": normalized_l1(expected_rad_stack, rad_stack),
        "selected_time_s": float(times_array[selected]),
        "arrival_count": arrival_count,
        "arrival_mask_mismatch_cells": arrival_mask_mismatch,
        "unexpected_hrr_cells": unexpected_hrr_cells,
        **receipt,
        "actual_dfc": dfc_stack[selected],
        "actual_rad": rad_stack[selected],
        "expected_dfc": expected_dfc_stack[selected],
        "expected_rad": expected_rad_stack[selected],
    }


def metric(name: str, expected: str, calculated: object, passed: bool, complete: bool, units: str = "-") -> dict[str, object]:
    return {
        "name": name,
        "expected": expected,
        "calculated": calculated,
        "units": units,
        "status": "PASS" if complete and passed else ("FAIL" if complete else "NOT EVALUABLE"),
    }


def plot_comparison(result: dict[str, object]) -> None:
    expected = np.asarray(result["expected_dfc"]) + np.asarray(result["expected_rad"])
    actual = np.asarray(result["actual_dfc"]) + np.asarray(result["actual_rad"])
    difference = actual - expected
    fig, axes = plt.subplots(1, 3, figsize=(10.0, 4.0), constrained_layout=True)
    vmax = max(float(np.max(expected)), float(np.max(actual)), 1.0e-12)
    for axis, field, title in zip(axes, (expected, actual, difference), ("Independent oracle", "ELMFIRE", "ELMFIRE - oracle")):
        if title == "ELMFIRE - oracle":
            limit = max(float(np.max(np.abs(field))), 1.0e-12)
            image = axis.imshow(field, origin="upper", cmap="coolwarm", vmin=-limit, vmax=limit)
        else:
            image = axis.imshow(field, origin="upper", cmap="inferno", vmin=0.0, vmax=vmax)
        fig.colorbar(image, ax=axis, shrink=0.80, label=r"Heat flux (kW m$^{-2}$)")
        axis.set(title=title, xlabel="Raster column", ylabel="Raster row")
    fig.suptitle(f"CASE43 full-domain heat map at t={result['selected_time_s']:g} s ({result['id']})")
    polish_figure(fig)
    fig.savefig(
        CASE_DIR / "figures" / "heat_map_comparison.pdf", bbox_inches="tight",
        metadata={"CreationDate": None, "ModDate": None},
    )
    plt.close(fig)


def main() -> None:
    (CASE_DIR / "figures" / "heat_map_comparison.pdf").unlink(missing_ok=True)
    if not EXPECTED_PATH.is_file():
        payload = {
            "case_id": "CASE43_WER",
            "source_commit": "a2dfbcdf72209733c000e5d3431e44723e15efea",
            "overall_status": "NOT EVALUABLE",
            "workflow_status": "BLOCKED",
            "verification_passed": False,
            "required_outputs_complete": False,
            "required_variant_count": 23,
            "completed_variant_count": 0,
            "executed_binary_sha256": None,
            "metrics": [],
            "reason": "variants/expected.json is missing; run preprocessing first.",
        }
        METRICS_PATH.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
        return
    specification = json.loads(EXPECTED_PATH.read_text(encoding="utf-8"))
    shape = tuple(int(value) for value in specification["grid"]["shape"])
    results: dict[str, dict[str, object]] = {}
    failures: dict[str, str] = {}
    for variant in specification["variants"]:
        try:
            results[str(variant["id"])] = load_variant(variant, specification, shape)
        except (OSError, ValueError, KeyError, json.JSONDecodeError, csv.Error, rasterio.errors.RasterioError) as error:
            failures[str(variant["id"])] = str(error)
    executable_hashes = {str(row["executable_sha256"]) for row in results.values()}
    if len(executable_hashes) > 1:
        failures["_execution"] = "variants were produced by different ELMFIRE binaries"
    complete = len(results) == len(specification["variants"]) and not failures
    tolerances = specification["tolerances"]

    def maximum(key: str) -> float | None:
        return max((float(row[key]) for row in results.values()), default=None)

    hrr_max = maximum("hrr_error")
    dfc_max = maximum("dfc_error")
    rad_max = maximum("rad_error")
    unexpected_arrivals = max((int(row["arrival_mask_mismatch_cells"]) for row in results.values()), default=None)
    unexpected_hrr = max((int(row["unexpected_hrr_cells"]) for row in results.values()), default=None)

    def pair_error(first: str, second: str, rotation: int = 0) -> float | None:
        if first not in results or second not in results:
            return None
        left = np.asarray(results[first]["actual_dfc"]) + np.asarray(results[first]["actual_rad"])
        right = np.asarray(results[second]["actual_dfc"]) + np.asarray(results[second]["actual_rad"])
        if rotation:
            left = np.rot90(left, k=rotation)
        return normalized_l1(left, right)

    clamp_error = pair_error("distance_50", "distance_75")
    equivalence_error = pair_error("equiv_constant", "equiv_raster")
    rotation_errors = [
        pair_error("rotation_wd270", "rotation_wd000", -1),
        pair_error("rotation_wd270", "rotation_wd090", 2),
        pair_error("rotation_wd270", "rotation_wd180", 1),
    ]
    rotation_max = max((value for value in rotation_errors if value is not None), default=None)
    vegetation_error = None
    if "fuel_forest" in results:
        vegetation_error = max(float(results["fuel_forest"]["dfc_error"]), float(results["fuel_forest"]["rad_error"]))
    metrics = [
        metric("maximum HRRPUA normalized L1 error", f"<= {tolerances['hrr_normalized_l1']}", hrr_max, hrr_max is not None and hrr_max <= tolerances["hrr_normalized_l1"], complete, "fraction"),
        metric("maximum DFC full-map normalized L1 error", f"<= {tolerances['heat_map_normalized_l1']}", dfc_max, dfc_max is not None and dfc_max <= tolerances["heat_map_normalized_l1"], complete, "fraction"),
        metric("maximum radiation full-map normalized L1 error", f"<= {tolerances['heat_map_normalized_l1']}", rad_max, rad_max is not None and rad_max <= tolerances["heat_map_normalized_l1"], complete, "fraction"),
        metric("D greater than 50 m clamp pair error", f"<= {tolerances['paired_map_normalized_l1']}", clamp_error, clamp_error is not None and clamp_error <= tolerances["paired_map_normalized_l1"], complete, "fraction"),
        metric("constant/raster parameter equivalence error", f"<= {tolerances['paired_map_normalized_l1']}", equivalence_error, equivalence_error is not None and equivalence_error <= tolerances["paired_map_normalized_l1"], complete, "fraction"),
        metric("maximum 90-degree covariance error", f"<= {tolerances['rotation_normalized_l1']}", rotation_max, rotation_max is not None and rotation_max <= tolerances["rotation_normalized_l1"], complete, "fraction"),
        metric("vegetation-source fuel-factor heat error", f"<= {tolerances['heat_map_normalized_l1']}", vegetation_error, vegetation_error is not None and vegetation_error <= tolerances["heat_map_normalized_l1"], complete, "fraction"),
        metric("maximum arrival-mask mismatch count", "0", unexpected_arrivals, unexpected_arrivals == 0, complete, "cells"),
        metric("maximum unexpected HRR source count", "0", unexpected_hrr, unexpected_hrr == 0, complete, "cells"),
    ]
    passed = complete and all(row["status"] == "PASS" for row in metrics)
    serializable_rows = []
    for variant in specification["variants"]:
        variant_id = str(variant["id"])
        if variant_id not in results:
            continue
        row = results[variant_id]
        serializable_rows.append({key: row[key] for key in ("id", "input_fingerprint_sha256", "completion_receipt", "executable_sha256", "stdout_sha256", "stderr_sha256", "evaluator_artifact_sha256", "output_artifact_sha256", "hrr_error", "dfc_error", "rad_error", "selected_time_s", "arrival_count", "arrival_mask_mismatch_cells", "unexpected_hrr_cells")})
    attempt_path = CASE_DIR / "outputs" / "run_attempt.json"
    attempted = attempt_path.is_file()
    never_run = (
        not results
        and len(failures) == len(specification["variants"])
        and all(reason == "successful-run completion marker is absent" for reason in failures.values())
        and not attempted
        and not any(
            (CASE_DIR / "logs" / f"{variant['id']}.{suffix}").is_file()
            for variant in specification["variants"]
            for suffix in ("stdout", "stderr")
        )
    )
    payload = {
        "case_id": "CASE43_WER",
        "source_commit": specification["source_commit"],
        "overall_status": "PASS" if passed else ("FAIL" if complete else "NOT EVALUABLE"),
        "workflow_status": "COMPLETE" if complete else ("NOT RUN" if never_run else "INCOMPLETE"),
        "verification_passed": passed,
        "required_outputs_complete": complete,
        "required_variant_count": len(specification["variants"]),
        "completed_variant_count": len(results),
        "executed_binary_sha256": next(iter(executable_hashes)) if len(executable_hashes) == 1 else None,
        "run_attempt": attempt_path.relative_to(CASE_DIR).as_posix() if attempted else None,
        "missing_evidence": failures,
        "metrics": metrics,
        "variant_results": serializable_rows,
    }
    if never_run:
        payload["reason"] = "ELMFIRE has not been run for the current prepared-input fingerprints."
    elif attempted and not results:
        payload["reason"] = "ELMFIRE execution was attempted, but no variant has complete current evidence."
    elif failures:
        payload["reason"] = "; ".join(f"{name}: {reason}" for name, reason in failures.items())
    elif passed:
        payload["reason"] = "All required variants and acceptance metrics passed."
    else:
        payload["reason"] = "One or more acceptance metrics failed."
    METRICS_PATH.parent.mkdir(parents=True, exist_ok=True)
    METRICS_PATH.write_text(json.dumps(payload, indent=2, allow_nan=False) + "\n", encoding="utf-8")
    if "branch_at_10" in results:
        plot_comparison(results["branch_at_10"])
    print(f"[OK] CASE43_WER: {payload['overall_status']}")


if __name__ == "__main__":
    main()
