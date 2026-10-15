#!/usr/bin/env python3
"""Fail-closed CASE44 comparison against the case-local intended-model oracle."""
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
from reference import combined_heat_maps


CASE_DIR = Path(__file__).resolve().parents[1]
EXPECTED_PATH = CASE_DIR / "variants" / "expected.json"
METRICS_PATH = CASE_DIR / "outputs" / "metrics.json"


def normalized_l1(reference: np.ndarray, actual: np.ndarray) -> float:
    """Return an all-cell L1 error; use absolute L1 when reference is zero."""
    denominator = float(np.sum(np.abs(reference)))
    numerator = float(np.sum(np.abs(actual - reference)))
    return numerator / denominator if denominator > 1.0e-14 else numerator


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
        "case_id": "CASE44_WHP",
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


def target_coefficients(
    variant: dict[str, object],
    specification: dict[str, object],
    shape: tuple[int, int],
) -> tuple[np.ndarray, np.ndarray]:
    raster_path = CASE_DIR / "variants" / str(variant["id"]) / "inputs" / "bldg_nonburnable.tif"
    nonburnable = read_raster(raster_path, specification["grid"])
    absorptivity = np.full(shape, float(variant["target_absorptivity"]), dtype=np.float64)
    for source in variant["sources"]:
        row, col = int(source["row"]), int(source["col"])
        nonburnable[row, col] = 0.2
        absorptivity[row, col] = 0.8
    return nonburnable, absorptivity


def load_manifest(output: Path) -> list[dict[str, str]]:
    manifests = sorted(output.glob("dump_times_*.csv"))
    if len(manifests) != 1:
        raise ValueError(f"expected one dump-times manifest, found {len(manifests)}")
    with manifests[0].open(newline="", encoding="utf-8") as stream:
        rows = list(csv.DictReader(stream, skipinitialspace=True))
    required = {"dump_index", "time_seconds", "is_final_dump"}
    if not rows or not required.issubset(rows[0]):
        raise ValueError("dump-times manifest is empty or malformed")
    times = np.asarray([float(row["time_seconds"]) for row in rows])
    expected_times = np.arange(1.0, 17.0, 1.0)
    if times.shape != expected_times.shape or not np.allclose(times, expected_times, rtol=0.0, atol=1.0e-6):
        raise ValueError(f"dump times {times.tolist()} do not equal 1..16 s")
    indices = [int(row["dump_index"]) for row in rows]
    if len(indices) != len(set(indices)):
        raise ValueError("dump indices are not unique")
    final = [index for index, row in enumerate(rows) if row["is_final_dump"].strip().upper() in {"T", "TRUE", "1"}]
    if final != [len(rows) - 1]:
        raise ValueError("only the terminal 16 s dump may be final")
    return rows


def load_variant(
    variant: dict[str, object],
    specification: dict[str, object],
    shape: tuple[int, int],
) -> dict[str, object]:
    output = CASE_DIR / "variants" / str(variant["id"]) / "outputs"
    receipt = require_current_success(variant, specification)
    rows = load_manifest(output)
    times = np.asarray([float(row["time_seconds"]) for row in rows])
    hrr_fields: list[np.ndarray] = []
    dfc_fields: list[np.ndarray] = []
    rad_fields: list[np.ndarray] = []
    for row in rows:
        dump_index = int(row["dump_index"])
        hrr_fields.append(read_raster(one_output(output, f"hrr_transient_*_d{dump_index:07d}.tif"), specification["grid"]))
        dfc_fields.append(read_raster(one_output(output, f"hf_dfc_transient_*_d{dump_index:07d}.tif"), specification["grid"]))
        rad_fields.append(read_raster(one_output(output, f"hf_rad_transient_*_d{dump_index:07d}.tif"), specification["grid"]))
    actual_hrr = np.stack(hrr_fields)
    actual_dfc = np.stack(dfc_fields)
    actual_rad = np.stack(rad_fields)

    target_nbf, target_abs = target_coefficients(variant, specification, shape)
    expected_hrr: list[np.ndarray] = []
    expected_dfc: list[np.ndarray] = []
    expected_rad: list[np.ndarray] = []
    for time_s in times:
        hrr, dfc, rad = combined_heat_maps(
            variant,
            shape,
            float(time_s),
            specification["curves"],
            target_nbf,
            target_abs,
        )
        expected_hrr.append(hrr)
        expected_dfc.append(dfc)
        expected_rad.append(rad)
    oracle_hrr = np.stack(expected_hrr)
    oracle_dfc = np.stack(expected_dfc)
    oracle_rad = np.stack(expected_rad)

    terminal_stamp = int(math.floor(float(times[-1]) + 0.5))
    total_dfc = read_raster(
        one_output(output, f"total_dfc_received_*_{terminal_stamp:07d}.tif"),
        specification["grid"],
    )
    total_rad = read_raster(
        one_output(output, f"total_rad_received_*_{terminal_stamp:07d}.tif"),
        specification["grid"],
    )
    dt = float(specification["time"]["dt_s"])
    cell_area = float(specification["grid"]["cell_size_m"]) ** 2
    integrated_dfc = np.sum(actual_dfc, axis=0) * dt * cell_area
    integrated_rad = np.sum(actual_rad, axis=0) * dt * cell_area

    source_mask = np.zeros(shape, dtype=bool)
    for source in variant["sources"]:
        source_mask[int(source["row"]), int(source["col"])] = True
    toa_path = one_output(output, f"time_of_arrival_*_{terminal_stamp:07d}.tif")
    with rasterio.open(toa_path) as dataset:
        validate_grid(dataset, specification["grid"], toa_path)
        arrivals = dataset.read(1, masked=True)
        arrival_count = int(arrivals.count())
        arrival_mask = ~np.ma.getmaskarray(arrivals)
    arrival_mask_mismatch = int(np.count_nonzero(arrival_mask != source_mask))
    unexpected_hrr_cells = max(
        int(np.count_nonzero(np.abs(field[~source_mask]) > 1.0e-7))
        for field in actual_hrr
    )
    zero_radiation = np.abs(oracle_rad) <= 1.0e-14
    leakage = float(np.max(np.abs(actual_rad[zero_radiation]))) if np.any(zero_radiation) else 0.0
    return {
        "id": str(variant["id"]),
        "times": times,
        "hrr_error": normalized_l1(oracle_hrr, actual_hrr),
        "dfc_error": normalized_l1(oracle_dfc, actual_dfc),
        "rad_error": normalized_l1(oracle_rad, actual_rad),
        "total_dfc_error": normalized_l1(integrated_dfc, total_dfc),
        "total_rad_error": normalized_l1(integrated_rad, total_rad),
        "radiation_leakage": leakage,
        "arrival_count": arrival_count,
        "source_count": len(variant["sources"]),
        "arrival_mask_mismatch_cells": arrival_mask_mismatch,
        "unexpected_hrr_cells": unexpected_hrr_cells,
        **receipt,
        "actual_hrr": actual_hrr,
        "actual_dfc": actual_dfc,
        "actual_rad": actual_rad,
        "oracle_hrr": oracle_hrr,
        "oracle_dfc": oracle_dfc,
        "oracle_rad": oracle_rad,
    }


def metric(
    name: str,
    expected: str,
    calculated: object,
    passed: bool,
    complete: bool,
    units: str = "-",
) -> dict[str, object]:
    return {
        "name": name,
        "expected": expected,
        "calculated": calculated,
        "units": units,
        "status": "PASS" if complete and passed else ("FAIL" if complete else "NOT EVALUABLE"),
    }


def scaled_error(
    results: dict[str, dict[str, object]],
    baseline: str,
    comparison: str,
    component: str,
    expected_scale: float,
) -> float | None:
    if baseline not in results or comparison not in results:
        return None
    reference = np.asarray(results[baseline][component]) * expected_scale
    actual = np.asarray(results[comparison][component])
    return normalized_l1(reference, actual)


def exact_pair_error(
    results: dict[str, dict[str, object]],
    first: str,
    second: str,
    components: tuple[str, ...] = ("actual_dfc", "actual_rad"),
) -> float | None:
    if first not in results or second not in results:
        return None
    left = np.concatenate([np.asarray(results[first][name]).ravel() for name in components])
    right = np.concatenate([np.asarray(results[second][name]).ravel() for name in components])
    return normalized_l1(left, right)


def plot_comparison(result: dict[str, object]) -> None:
    sums = np.sum(np.asarray(result["actual_dfc"]) + np.asarray(result["actual_rad"]), axis=(1, 2))
    oracle_sums = np.sum(np.asarray(result["oracle_dfc"]) + np.asarray(result["oracle_rad"]), axis=(1, 2))
    peak = int(np.argmax(oracle_sums))
    actual = np.asarray(result["actual_dfc"])[peak] + np.asarray(result["actual_rad"])[peak]
    expected = np.asarray(result["oracle_dfc"])[peak] + np.asarray(result["oracle_rad"])[peak]
    difference = actual - expected
    fig, axes = plt.subplots(1, 3, figsize=(10.0, 4.0), constrained_layout=True)
    vmax = max(float(np.max(expected)), float(np.max(actual)), 1.0e-12)
    for axis, field, title in zip(axes, (expected, actual, difference), ("Independent oracle", "ELMFIRE", "ELMFIRE - oracle")):
        if title.endswith("oracle") and title.startswith("ELMFIRE"):
            limit = max(float(np.max(np.abs(field))), 1.0e-12)
            image = axis.imshow(field, origin="upper", cmap="coolwarm", vmin=-limit, vmax=limit)
        else:
            image = axis.imshow(field, origin="upper", cmap="inferno", vmin=0.0, vmax=vmax)
        fig.colorbar(image, ax=axis, shrink=0.80, label=r"Heat flux (kW m$^{-2}$)")
        axis.set(title=title, xlabel="Raster column", ylabel="Raster row")
    fig.suptitle(f"CASE44 heat response at t={result['times'][peak]:g} s ({result['id']})")
    polish_figure(fig)
    fig.savefig(
        CASE_DIR / "figures" / "heat_response_comparison.pdf", bbox_inches="tight",
        metadata={"CreationDate": None, "ModDate": None},
    )
    plt.close(fig)


def write_blocked(reason: str) -> None:
    METRICS_PATH.parent.mkdir(parents=True, exist_ok=True)
    payload = {
        "case_id": "CASE44_WHP",
        "source_commit": "a2dfbcdf72209733c000e5d3431e44723e15efea",
        "overall_status": "NOT EVALUABLE",
        "workflow_status": "BLOCKED",
        "verification_passed": False,
        "required_outputs_complete": False,
        "required_variant_count": 19,
        "completed_variant_count": 0,
        "executed_binary_sha256": None,
        "metrics": [],
        "reason": reason,
    }
    METRICS_PATH.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")


def main() -> None:
    (CASE_DIR / "figures" / "heat_response_comparison.pdf").unlink(missing_ok=True)
    if not EXPECTED_PATH.is_file():
        write_blocked("variants/expected.json is missing; run preprocessing first.")
        return
    specification = json.loads(EXPECTED_PATH.read_text(encoding="utf-8"))
    shape = tuple(int(value) for value in specification["grid"]["shape"])
    results: dict[str, dict[str, object]] = {}
    failures: dict[str, str] = {}
    for variant in specification["variants"]:
        try:
            results[str(variant["id"])] = load_variant(variant, specification, shape)
        except (OSError, ValueError, TypeError, KeyError, json.JSONDecodeError, csv.Error, rasterio.errors.RasterioError) as error:
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
    total_max = max((maximum("total_dfc_error"), maximum("total_rad_error")), key=lambda value: -1.0 if value is None else value)
    leakage_max = maximum("radiation_leakage")
    unexpected_arrivals = max((int(row["arrival_mask_mismatch_cells"]) for row in results.values()), default=None)
    unexpected_hrr = max((int(row["unexpected_hrr_cells"]) for row in results.values()), default=None)

    hrr_scaling = scaled_error(results, "curve_base", "curve_double_peak", "actual_hrr", 2.0)
    nbf_errors = [
        scaled_error(results, "curve_base", "target_nbf_0", component, 1.25)
        for component in ("actual_dfc", "actual_rad")
    ] + [
        scaled_error(results, "curve_base", "target_nbf_0p5", component, 0.625)
        for component in ("actual_dfc", "actual_rad")
    ] + [
        scaled_error(results, "curve_base", "target_nbf_1", component, 0.0)
        for component in ("actual_dfc", "actual_rad")
    ]
    nbf_max = max((value for value in nbf_errors if value is not None), default=None)
    absorptivity_errors = [
        scaled_error(results, "curve_base", name, "actual_dfc", 1.0)
        for name in ("target_abs_0", "target_abs_0p4", "target_abs_1")
    ] + [
        scaled_error(results, "curve_base", "target_abs_0", "actual_rad", 0.0),
        scaled_error(results, "curve_base", "target_abs_0p4", "actual_rad", 0.5),
        scaled_error(results, "curve_base", "target_abs_1", "actual_rad", 1.25),
    ]
    absorptivity_max = max((value for value in absorptivity_errors if value is not None), default=None)
    adjustment_errors = [
        scaled_error(results, "adj_0p5", "adj_0p25", component, 4.0)
        for component in ("actual_dfc", "actual_rad")
    ] + [
        scaled_error(results, "adj_0p5", "adj_0p75", component, (0.5 / 0.75) ** 2)
        for component in ("actual_dfc", "actual_rad")
    ] + [
        scaled_error(results, "adj_0p5", "adj_1p0", component, 0.25)
        for component in ("actual_dfc", "actual_rad")
    ]
    adjustment_max = max((value for value in adjustment_errors if value is not None), default=None)
    superposition_error = None
    if all(name in results for name in ("source_left", "source_right", "source_pair")):
        single_sum = np.concatenate([
            (np.asarray(results["source_left"][component]) + np.asarray(results["source_right"][component])).ravel()
            for component in ("actual_hrr", "actual_dfc", "actual_rad")
        ])
        pair = np.concatenate([
            np.asarray(results["source_pair"][component]).ravel()
            for component in ("actual_hrr", "actual_dfc", "actual_rad")
        ])
        superposition_error = normalized_l1(single_sum, pair)
    raster_response = None
    if all(name in results for name in ("inert_rasters_low", "inert_rasters_high")):
        low = np.concatenate([
            np.asarray(results["inert_rasters_low"][component]).ravel()
            for component in ("actual_dfc", "actual_rad")
        ])
        high = np.concatenate([
            np.asarray(results["inert_rasters_high"][component]).ravel()
            for component in ("actual_dfc", "actual_rad")
        ])
        raster_response = float(np.sum(np.abs(high))) / max(float(np.sum(np.abs(low))), 1.0e-14)

    primary_tol = float(tolerances["heat_map_normalized_l1"])
    pair_tol = float(tolerances["paired_map_normalized_l1"])
    total_tol = float(tolerances["total_integration_normalized_l1"])
    metrics = [
        metric("maximum HRR curve normalized L1 error", f"<= {tolerances['hrr_normalized_l1']}", hrr_max, hrr_max is not None and hrr_max <= tolerances["hrr_normalized_l1"], complete, "fraction"),
        metric("double-peak HRR scaling error", f"<= {pair_tol}", hrr_scaling, hrr_scaling is not None and hrr_scaling <= pair_tol, complete, "fraction"),
        metric("maximum DFC full-stack normalized L1 error", f"<= {primary_tol}", dfc_max, dfc_max is not None and dfc_max <= primary_tol, complete, "fraction"),
        metric("maximum radiation full-stack normalized L1 error", f"<= {primary_tol}", rad_max, rad_max is not None and rad_max <= primary_tol, complete, "fraction"),
        metric("spatial nonburnable scaling error", f"<= {pair_tol}", nbf_max, nbf_max is not None and nbf_max <= pair_tol, complete, "fraction"),
        metric("target-table absorptivity scaling error", f"<= {pair_tol}", absorptivity_max, absorptivity_max is not None and absorptivity_max <= pair_tol, complete, "fraction"),
        metric("HRR_ELLIPSE_ADJ inverse-square error", f"<= {pair_tol}", adjustment_max, adjustment_max is not None and adjustment_max <= pair_tol, complete, "fraction"),
        metric("maximum radiation outside source cutoff", f"<= {tolerances['radiation_leakage_kw_m2']}", leakage_max, leakage_max is not None and leakage_max <= tolerances["radiation_leakage_kw_m2"], complete, "kW m^-2"),
        metric("maximum transient-to-total integration error", f"<= {total_tol}", total_max, total_max is not None and total_max <= total_tol, complete, "fraction"),
        metric("two-source superposition error", f"<= {pair_tol}", superposition_error, superposition_error is not None and superposition_error <= pair_tol, complete, "fraction"),
        metric("spatial nonburnable-raster response", f"fully nonburnable / combustible heat <= {pair_tol}", raster_response, raster_response is not None and raster_response <= pair_tol, complete, "ratio"),
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
        serializable_rows.append({key: row[key] for key in ("id", "input_fingerprint_sha256", "completion_receipt", "executable_sha256", "stdout_sha256", "stderr_sha256", "evaluator_artifact_sha256", "output_artifact_sha256", "hrr_error", "dfc_error", "rad_error", "total_dfc_error", "total_rad_error", "radiation_leakage", "arrival_count", "arrival_mask_mismatch_cells", "unexpected_hrr_cells")})
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
        "case_id": "CASE44_WHP",
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
    if "curve_base" in results:
        plot_comparison(results["curve_base"])
    print(f"[OK] CASE44_WHP: {payload['overall_status']}")


if __name__ == "__main__":
    main()
