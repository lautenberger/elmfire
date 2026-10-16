#!/usr/bin/env python3
"""Evaluate current-fingerprint WU-E temporal-convergence outputs, fail closed."""

from __future__ import annotations

from report_language import polish_figure

import csv
import hashlib
import json
import math
import re
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


CASE_DIR = Path(__file__).resolve().parents[1]
ATTEMPT_LEDGER = CASE_DIR / "logs/run_attempts.json"
OUT_DIR = CASE_DIR / "outputs"
FIG_DIR = CASE_DIR / "figures"


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def output_artifact_hashes(variant_dir: Path) -> dict[str, str]:
    return {
        str(path.relative_to(variant_dir)): sha256(path)
        for path in sorted((variant_dir / "outputs").rglob("*"))
        if path.is_file()
    }


def current_fingerprint(variant_dir: Path) -> str:
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


def completion_receipt(
    variant_dir: Path, item: dict, expected_fingerprint: str, source_revision: str
) -> dict[str, object]:
    path = variant_dir / "logs" / "completion_marker.json"
    payload = json.loads(path.read_text(encoding="utf-8"))
    required = {
        "case_id": "CASE49_WTC",
        "variant_id": item["id"],
        "status": "ELMFIRE_EXIT_0",
        "input_fingerprint": expected_fingerprint,
        "oracle_source_revision": source_revision,
    }
    for key, expected in required.items():
        if payload.get(key) != expected:
            raise ValueError(f"completion receipt {key} does not match the run contract")
    digest = str(payload.get("executable_sha256", ""))
    if (
        not payload.get("completed_utc")
        or not payload.get("executable_requested")
        or not payload.get("executable_resolved")
        or len(digest) != 64
        or any(character not in "0123456789abcdef" for character in digest.lower())
    ):
        raise ValueError("completion receipt does not identify the executed binary")
    executable = Path(str(payload["executable_resolved"]))
    if not executable.is_absolute() or not executable.is_file() or sha256(executable) != digest:
        raise ValueError("executed binary is absent or no longer matches its recorded SHA-256")
    stdout = variant_dir / "logs" / "elmfire.stdout"
    if not stdout.is_file() or payload.get("stdout_sha256") != sha256(stdout):
        raise ValueError("ELMFIRE stdout is absent or differs from the completion receipt")
    stderr = variant_dir / "logs" / "elmfire.stderr"
    if not stderr.is_file() or payload.get("stderr_sha256") != sha256(stderr):
        raise ValueError("ELMFIRE stderr is absent or differs from the completion receipt")
    recorded_outputs = payload.get("output_artifact_sha256")
    if not isinstance(recorded_outputs, dict) or not recorded_outputs:
        raise ValueError("completion receipt does not bind the output artifact snapshot")
    if recorded_outputs != output_artifact_hashes(variant_dir):
        raise ValueError("output artifacts differ from the successful-run receipt")
    payload["receipt_path"] = str(path.relative_to(CASE_DIR))
    return payload


def unique(path: Path, pattern: str) -> Path:
    matches = sorted(path.glob(pattern))
    if len(matches) != 1:
        raise ValueError(f"{path}: expected one {pattern}, found {len(matches)}")
    return matches[0]


def namelist_value(config: Path, key: str) -> str:
    matches = re.findall(
        rf"(?mi)^\s*{re.escape(key)}\s*=\s*([^!,/\n]+)",
        config.read_text(encoding="utf-8"),
    )
    if len(matches) != 1:
        raise ValueError(f"{config}: expected one namelist assignment for {key}")
    return matches[0].strip()


def namelist_float(config: Path, key: str) -> float:
    value = float(namelist_value(config, key).replace("D", "E").replace("d", "e"))
    if not math.isfinite(value):
        raise ValueError(f"{config}: {key} is not finite")
    return value


def namelist_bool(config: Path, key: str) -> bool:
    value = namelist_value(config, key).upper()
    if value not in {".TRUE.", ".FALSE."}:
        raise ValueError(f"{config}: {key} is not a Fortran logical")
    return value == ".TRUE."


def validate_variant_contract(spec: dict, item: dict, variant_dir: Path) -> None:
    """Cross-check manifest claims against the concrete namelist, table, and rasters."""
    regimes = {str(row["id"]): row for row in spec["regimes"]}
    regime_id = str(item.get("regime"))
    if regime_id not in regimes:
        raise ValueError("variant regime is not in the case specification")
    regime = regimes[regime_id]
    if item.get("sampled_field_expectation") != regime.get("sampled_field_expectation"):
        raise ValueError("manifest sampled-field expectation differs from the case specification")
    dt = float(item["dt_s"])
    expected_id = f"{regime_id}_dt_{str(dt).replace('.', 'p')}s"
    if item.get("id") != expected_id:
        raise ValueError("variant identity does not match its regime and timestep")
    if item.get("working_directory") != f"variants/{expected_id}":
        raise ValueError("variant working directory is not canonical")
    if item.get("config") != "elmfire.data.in" or item.get("runnable") is not True:
        raise ValueError("variant run contract is malformed")
    if not any(math.isclose(dt, float(value), abs_tol=1.0e-12)
               for value in spec["time_steps_s"]):
        raise ValueError("manifest timestep is not in the case specification")
    expected_values = {
        "dx_m": float(spec["grid_spacing_m"]),
        "tstop_s": float(spec["simulation_tstop_s"]),
        "dump_interval_s": float(spec["dump_interval_s"]),
        "building_fuel_model": int(regime["building_fuel_model"]),
        "hrrpua_peak_kw_m2": float(regime["hrrpua_peak_kw_m2"]),
    }
    for key, expected in expected_values.items():
        if not math.isclose(float(item[key]), float(expected),
                            rel_tol=0.0, abs_tol=1.0e-12):
            raise ValueError(f"manifest {key} differs from the case specification")

    config = variant_dir / "elmfire.data.in"
    scalar_checks = {
        "SIMULATION_TSTART": 0.0,
        "SIMULATION_TSTOP": expected_values["tstop_s"],
        "SIMULATION_DT": dt,
        "SIMULATION_DTMAX": dt,
        "DTDUMP": expected_values["dump_interval_s"],
        "BANDTHICKNESS_WUI": float(item["bandthickness_wui_cells"]),
        "FEEDBACK_LEVEL": 1.0,
        "BLDG_FUEL_MODEL_CONSTANT": float(expected_values["building_fuel_model"]),
    }
    for key, expected in scalar_checks.items():
        if not math.isclose(namelist_float(config, key), expected,
                            rel_tol=0.0, abs_tol=1.0e-12):
            raise ValueError(f"namelist {key} differs from the variant contract")
    logical_checks = {
        "DUMP_EVERY_STEP": False,
        "DUMP_TIME_OF_ARRIVAL": True,
        "DUMP_TOTAL_DFC_RECEIVED": True,
        "DUMP_TOTAL_RAD_RECEIVED": True,
        "DUMP_TRANSIENT_DFC": True,
        "DUMP_TRANSIENT_RAD": True,
        "DUMP_HRR_TRANSIENT": True,
        "USE_BLDG_SPREAD_MODEL": True,
        "USE_CONSTANT_BLDG_SPREAD_MODEL_PARAMS": True,
    }
    for key, expected in logical_checks.items():
        if namelist_bool(config, key) is not expected:
            raise ValueError(f"namelist {key} differs from the variant contract")

    table_path = variant_dir / "data" / "misc" / "building_fuel_models.csv"
    with table_path.open(newline="", encoding="utf-8") as stream:
        table_rows = [row for row in csv.reader(stream) if row]
    selected = [
        row for row in table_rows
        if int(row[0]) == expected_values["building_fuel_model"]
    ]
    if len(selected) != 1 or len(selected[0]) < 8:
        raise ValueError("building fuel table does not contain the selected unique row")
    table_row = selected[0]
    table_checks = {
        3: float(spec["hrr_growth_end_s"]),
        4: float(spec["hrr_steady_end_s"]),
        5: float(spec["hrr_decay_end_s"]),
        7: expected_values["hrrpua_peak_kw_m2"],
    }
    if any(not math.isclose(float(table_row[index]), expected,
                            rel_tol=0.0, abs_tol=1.0e-12)
           for index, expected in table_checks.items()):
        raise ValueError("selected building-fuel row differs from the design-fire specification")

    reference = variant_dir / "data" / "inputs" / "fbfm.tif"
    phi_path = variant_dir / "data" / "inputs" / "phi.tif"
    with rasterio.open(reference) as fbfm_source, rasterio.open(phi_path) as phi_source:
        expected_shape = (
            int(item["physical_rows"]) + 2 * int(item["buffer_cells"]),
            int(item["physical_columns"]) + 2 * int(item["buffer_cells"]),
        )
        if fbfm_source.shape != expected_shape or phi_source.shape != expected_shape:
            raise ValueError("prepared raster shape differs from the manifest geometry")
        dx = expected_values["dx_m"]
        if (not math.isclose(fbfm_source.transform.a, dx, abs_tol=1.0e-12)
                or not math.isclose(-fbfm_source.transform.e, dx, abs_tol=1.0e-12)):
            raise ValueError("prepared raster resolution differs from the case specification")
        reference_transform = fbfm_source.transform
        reference_crs = fbfm_source.crs
        reference_nodata = fbfm_source.nodata
        phi = phi_source.read(1)
    input_paths = sorted((variant_dir / "data" / "inputs").glob("*.tif"))
    if len(input_paths) != 15:
        raise ValueError(f"expected 15 prepared input rasters, found {len(input_paths)}")
    for path in input_paths:
        with rasterio.open(path) as source:
            if (
                source.count != 1
                or source.shape != expected_shape
                or not source.transform.almost_equals(reference_transform)
                or source.crs != reference_crs
                or source.nodata != reference_nodata
            ):
                raise ValueError(f"prepared input raster grid contract failed: {path.name}")
    buffer_cells = int(item["buffer_cells"])
    region = np.s_[
        buffer_cells:buffer_cells + int(item["physical_rows"]),
        buffer_cells:buffer_cells + int(item["physical_columns"]),
    ]
    source_count = int(np.count_nonzero(phi[region] <= 0.0))
    if source_count != int(item["source_cell_count"]):
        raise ValueError("prepared source-cell count differs from the manifest")
    if not math.isclose(source_count * dx * dx, float(item["source_area_m2"]),
                        rel_tol=0.0, abs_tol=1.0e-9):
        raise ValueError("prepared source area differs from the manifest")


def terminal_paths(
    variant_dir: Path, item: dict, source_revision: str
) -> tuple[dict[str, Path], float, list[int], dict[str, object]]:
    marker = variant_dir / "logs" / "completed_input_fingerprint.txt"
    if not marker.is_file():
        raise ValueError("successful-run fingerprint marker is absent")
    expected = item["input_fingerprint"]
    if marker.read_text(encoding="utf-8").strip() != expected:
        raise ValueError("successful-run fingerprint marker is stale")
    if current_fingerprint(variant_dir) != expected:
        raise ValueError("prepared inputs differ from the manifest fingerprint")
    receipt = completion_receipt(variant_dir, item, expected, source_revision)
    output_dir = variant_dir / "outputs"
    dump_manifest = unique(output_dir, "dump_times_*.csv")
    with dump_manifest.open(newline="", encoding="utf-8") as stream:
        raw_rows = list(csv.DictReader(stream))
    rows = [
        {str(key).strip(): str(value).strip() for key, value in row.items()}
        for row in raw_rows
    ]
    final_rows = [row for row in rows if row.get("is_final_dump", "").upper()
                  in {"T", "TRUE", "1", "Y", "YES"}]
    if len(final_rows) != 1:
        raise ValueError(f"expected one final dump record, found {len(final_rows)}")
    final_time = float(final_rows[0]["time_seconds"])
    if not math.isclose(final_time, float(item["tstop_s"]), rel_tol=0.0,
                        abs_tol=1.0e-3):
        raise ValueError(
            f"final dump {final_time:g} s does not equal requested stop {item['tstop_s']:g} s"
        )
    dump_indices = [int(row["dump_index"]) for row in rows]
    if dump_indices != list(range(1, len(rows) + 1)):
        raise ValueError("dump indices are duplicated, missing, or out of order")
    dump_times = [float(row["time_seconds"]) for row in rows]
    interval = float(item["dump_interval_s"])
    expected_times = [interval * index for index in dump_indices]
    if len(dump_times) != int(round(final_time / interval)) or any(
        not math.isclose(actual, expected, rel_tol=0.0, abs_tol=1.0e-3)
        for actual, expected in zip(dump_times, expected_times)
    ):
        raise ValueError("dump records are not on the exact common-time schedule")
    stamp = int(math.floor(final_time + 0.5))
    return {
        "toa": unique(output_dir, f"time_of_arrival_*_{stamp:07d}.tif"),
        "dfc": unique(output_dir, f"total_dfc_received_*_{stamp:07d}.tif"),
        "rad": unique(output_dir, f"total_rad_received_*_{stamp:07d}.tif"),
    }, final_time, dump_indices, receipt


def aligned_array(path: Path, reference: Path) -> tuple[np.ndarray, object]:
    with rasterio.open(reference) as expected, rasterio.open(path) as source:
        if (source.count != 1 or source.shape != expected.shape
                or not source.transform.almost_equals(expected.transform)
                or source.crs != expected.crs or source.nodata != expected.nodata):
            raise ValueError(f"{path.name} is not aligned with current prepared inputs")
        return source.read(1, masked=True).filled(np.nan).astype(float), source.transform


def transient_diagnostics(
    variant_dir: Path,
    item: dict,
    reference: Path,
    expected_indices: list[int],
) -> tuple[float, int, int, float]:
    """Audit complete transient stacks and the actual logged solver steps."""
    with rasterio.open(reference) as source:
        urban_mask = source.read(1) == 91
    if not np.any(urban_mask):
        raise ValueError("prepared grid has no FBFM91 cells for the sampled heat-field diagnostic")
    output_dir = variant_dir / "outputs"
    dfc_paths = sorted(output_dir.glob("hf_dfc_transient_*_d*.tif"))
    rad_paths = sorted(output_dir.glob("hf_rad_transient_*_d*.tif"))
    hrr_paths = sorted(output_dir.glob("hrr_transient_*_d*.tif"))
    if not dfc_paths or len(dfc_paths) != len(rad_paths) or len(dfc_paths) != len(hrr_paths):
        raise ValueError("transient DFC, radiation, and HRR dump sets are incomplete")
    def indices(paths: list[Path]) -> list[int]:
        parsed = []
        for path in paths:
            match = re.search(r"_d([0-9]{7})\.tif$", path.name)
            if match is None:
                raise ValueError(f"malformed transient dump name: {path.name}")
            parsed.append(int(match.group(1)))
        return parsed
    for label, paths in (("DFC", dfc_paths), ("radiation", rad_paths), ("HRR", hrr_paths)):
        if indices(paths) != expected_indices:
            raise ValueError(f"{label} transient dumps do not match the dump manifest")
    dt = float(item["dt_s"])
    cell_area = float(item["dx_m"]) ** 2
    maximum_step_energy = 0.0
    final_transient_maximum = math.nan
    for dfc_path, rad_path, hrr_path in zip(dfc_paths, rad_paths, hrr_paths):
        dfc, _ = aligned_array(dfc_path, reference)
        rad, _ = aligned_array(rad_path, reference)
        hrr, _ = aligned_array(hrr_path, reference)
        if (not np.all(np.isfinite(dfc)) or not np.all(np.isfinite(rad))
                or not np.all(np.isfinite(hrr))):
            raise ValueError("transient output contains nodata/non-finite cells")
        if np.any(dfc < -1.0e-6) or np.any(rad < -1.0e-6) or np.any(hrr < -1.0e-6):
            raise ValueError("transient output contains a negative physical quantity")
        energy = (dfc + rad) * dt * cell_area
        maximum_step_energy = max(
            maximum_step_energy, float(np.max(energy[urban_mask]))
        )
        final_transient_maximum = max(
            float(np.max(np.abs(dfc))),
            float(np.max(np.abs(rad))),
            float(np.max(np.abs(hrr))),
        )
    if not math.isfinite(final_transient_maximum) or final_transient_maximum > 1.0e-6:
        raise ValueError(
            "the exact-stop transient dump is not reset-zero after the final solver step"
        )

    log_path = variant_dir / "logs" / "elmfire.stdout"
    log_text = log_path.read_text(encoding="utf-8", errors="replace")
    logged_times = [
        float(value)
        for value in re.findall(r"Current Timestep:\s*([0-9.+\-Ee]+)\s+of", log_text)
    ]
    expected_steps = int(round(float(item["tstop_s"]) / dt))
    if len(logged_times) != expected_steps:
        raise ValueError(
            f"logged {len(logged_times)} solver steps; expected {expected_steps} at configured DT"
        )
    expected_times = np.arange(expected_steps, dtype=float) * dt
    maximum_step_deviation = float(np.max(np.abs(
        np.asarray(logged_times, dtype=float) - expected_times
    ))) if expected_steps else 0.0
    if maximum_step_deviation > 0.051:
        raise ValueError(
            f"logged solver-step timestamps differ from 0..TSTOP-DT by {maximum_step_deviation:g} s"
        )
    return maximum_step_energy, len(dfc_paths), len(logged_times), maximum_step_deviation


def profile_variant(spec: dict, item: dict) -> dict:
    variant_dir = CASE_DIR / item["working_directory"]
    validate_variant_contract(spec, item, variant_dir)
    paths, final_time, dump_indices, receipt = terminal_paths(
        variant_dir, item, str(spec["source_revision"])
    )
    reference = variant_dir / "data" / "inputs" / "fbfm.tif"
    toa, _ = aligned_array(paths["toa"], reference)
    dfc, _ = aligned_array(paths["dfc"], reference)
    rad, _ = aligned_array(paths["rad"], reference)
    maximum_step_energy, dump_count, logged_step_count, maximum_step_deviation = transient_diagnostics(
        variant_dir, item, reference, dump_indices
    )
    buffer_cells = int(item["buffer_cells"])
    nrows = int(item["physical_rows"])
    ncols = int(item["physical_columns"])
    region = np.s_[buffer_cells:buffer_cells + nrows,
                   buffer_cells:buffer_cells + ncols]
    toa_region = toa[region]
    dfc_region = dfc[region]
    rad_region = rad[region]
    if not np.all(np.isfinite(dfc_region)) or not np.all(np.isfinite(rad_region)):
        raise ValueError("terminal heat fields contain nodata/non-finite cells in the physical region")
    if np.any(dfc_region < -1.0e-6) or np.any(rad_region < -1.0e-6):
        raise ValueError("terminal accumulated heat fields contain negative energy")
    heat_region = dfc_region + rad_region
    if not float(np.sum(heat_region)) > 0.0:
        raise ValueError("terminal accumulated heat is identically zero")
    finite_toa_values = toa_region[np.isfinite(toa_region)]
    if np.any(finite_toa_values < 0.0) or np.any(finite_toa_values > final_time + 1.0e-3):
        raise ValueError("terminal TOA contains a finite value outside [0,TSTOP]")
    dx = float(item["dx_m"])
    x = (np.arange(ncols, dtype=float) + 0.5) * dx
    row_lo = max(0, nrows // 4)
    row_hi = min(nrows, nrows - nrows // 4)
    corridor = toa_region[row_lo:row_hi, :]
    median_toa = np.full(ncols, np.nan, dtype=float)
    for column in range(ncols):
        samples = corridor[:, column]
        samples = samples[np.isfinite(samples) & (samples >= 0.0)]
        if samples.size:
            median_toa[column] = float(np.median(samples))

    landmarks = []
    for requested_x in spec["landmark_x_m"]:
        column = int(np.argmin(np.abs(x - float(requested_x))))
        value = median_toa[column]
        landmarks.append(float(value) if np.isfinite(value) else None)
    source_width = float(spec["initial_source_width_m"])
    fit_mask = np.isfinite(median_toa) & (x > source_width + 0.25 * dx)
    ros = None
    r2 = None
    if np.count_nonzero(fit_mask) >= 3:
        slope, intercept = np.polyfit(x[fit_mask], median_toa[fit_mask], 1)
        fitted = slope * x[fit_mask] + intercept
        residual = float(np.sum((median_toa[fit_mask] - fitted) ** 2))
        total = float(np.sum((median_toa[fit_mask] - np.mean(median_toa[fit_mask])) ** 2))
        candidate_r2 = 1.0 - residual / total if total > 0.0 else math.nan
        if slope > 0.0 and math.isfinite(candidate_r2):
            ros = float(1.0 / slope)
            r2 = candidate_r2

    finite_toa = np.isfinite(toa_region) & (toa_region >= 0.0)
    return {
        **item,
        "final_time_s": final_time,
        "landmark_toa_s": landmarks,
        "toa_ros_m_s": ros,
        "toa_fit_r2": r2,
        "burned_fraction": float(np.count_nonzero(finite_toa) / finite_toa.size),
        "total_received_heat_kj": float(np.sum(heat_region)),
        "maximum_sampled_cell_step_energy_kj": maximum_step_energy,
        "transient_dump_count": dump_count,
        "logged_solver_step_count": logged_step_count,
        "maximum_logged_step_deviation_s": maximum_step_deviation,
        "completion_receipt": receipt["receipt_path"],
        "executable_sha256": receipt["executable_sha256"],
        "profile_x_m": x.tolist(),
        "profile_toa_s": [float(value) if np.isfinite(value) else None
                          for value in median_toa],
        "source_files": {key: str(value.relative_to(CASE_DIR))
                         for key, value in paths.items()},
    }


def relative_change(a: float, b: float) -> float:
    return abs(a - b) / max(abs(b), 1.0e-12)


def relative_vector_change(a: list[float], b: list[float]) -> float:
    aa = np.asarray(a, dtype=float)
    bb = np.asarray(b, dtype=float)
    return float(np.linalg.norm(aa - bb) / max(np.linalg.norm(bb), 1.0e-12))


def observed_order(values: list[float | list[float]]) -> float | None:
    def distance(first, second):
        return float(np.linalg.norm(np.asarray(first, dtype=float)
                                    - np.asarray(second, dtype=float)))
    coarse_difference = distance(values[-3], values[-2])
    fine_difference = distance(values[-2], values[-1])
    if fine_difference <= 1.0e-14:
        return None
    if coarse_difference <= 1.0e-14:
        return -math.inf
    return math.log(coarse_difference / fine_difference) / math.log(2.0)


def order_metric(name: str, values: list[float | list[float]], minimum: float) -> dict:
    """Accept only a three-level roundoff-flat sequence as the zero-difference alternative."""
    arrays = [np.asarray(value, dtype=float) for value in values]
    coarse_difference = float(np.linalg.norm(arrays[-3] - arrays[-2]))
    fine_difference = float(np.linalg.norm(arrays[-2] - arrays[-1]))
    scale = max(*(float(np.linalg.norm(value)) for value in arrays[-3:]), 1.0)
    flat_tolerance = 1.0e-12 * scale
    if fine_difference <= flat_tolerance:
        passed = coarse_difference <= flat_tolerance
        return metric(
            name,
            f">= {minimum} or roundoff-flat three-level sequence",
            ("roundoff-flat three-level sequence" if passed
             else "flat finest pair after nonzero coarse change"),
            "-",
            passed,
        )
    order = (-math.inf if coarse_difference <= flat_tolerance else
             math.log(coarse_difference / fine_difference) / math.log(2.0))
    return metric(
        name,
        f">= {minimum} or roundoff-flat three-level sequence",
        safe_number(order),
        "-",
        order >= minimum,
    )


def metric(name: str, expected, calculated, units: str, passed: bool | None) -> dict:
    return {
        "name": name,
        "expected": expected,
        "calculated": calculated,
        "units": units,
        "status": "NOT EVALUABLE" if passed is None else ("PASS" if passed else "FAIL"),
    }


def safe_number(value: float | None) -> float | str | None:
    if value is None:
        return None
    if math.isinf(value):
        return "+infinity" if value > 0.0 else "-infinity"
    return value


def plot_results(spec: dict, groups: dict[str, list[dict]]) -> None:
    FIG_DIR.mkdir(parents=True, exist_ok=True)
    fig, axes = plt.subplots(2, 2, figsize=(7.2, 7.5), constrained_layout=True)
    threshold = float(spec["metrics"]["source_deviation_diagnostic_kj"])
    for regime_id, rows in groups.items():
        dt = [row["dt_s"] for row in rows]
        axes[0, 0].plot(dt, [row["total_received_heat_kj"] for row in rows],
                        "o-", label="Low HRR" if regime_id == "subthreshold" else "High HRR")
        axes[0, 1].plot(dt,
                        [row["maximum_sampled_cell_step_energy_kj"] for row in rows],
                        "o-", label="Low HRR" if regime_id == "subthreshold" else "High HRR")
    high = groups["threshold_stress"]
    axes[1, 0].plot([row["dt_s"] for row in high],
                    [row["toa_ros_m_s"] for row in high], "o-")
    for index, landmark in enumerate(spec["landmark_x_m"]):
        axes[1, 1].plot([row["dt_s"] for row in high],
                        [row["landmark_toa_s"][index] for row in high], "o-",
                        label=f"x={landmark:g} m")
    axes[0, 1].axhline(threshold, color="black", linestyle="--",
                       linewidth=1.0, label="30,000 kJ (diagnostic)")
    axes[0, 0].set_ylabel("Domain received\nheat (kJ)")
    axes[0, 1].set_ylabel("Maximum cell-step\nenergy (kJ)")
    axes[1, 0].set_ylabel(r"TOA-derived ROS (m s$^{-1}$)")
    axes[1, 1].set_ylabel("Arrival time (s)")
    for axis in axes.flat:
        axis.set_xscale("log", base=2)
        axis.invert_xaxis()
        axis.set_xlabel(r"Time step $\Delta t$ (s)")
        axis.grid(alpha=0.25)
    axes[0, 0].legend(fontsize=12)
    axes[0, 1].legend(fontsize=12)
    axes[1, 1].legend(fontsize=12)
    polish_figure(fig)
    fig.savefig(FIG_DIR / "convergence.pdf", bbox_inches="tight",
                metadata={"CreationDate": None, "ModDate": None})
    plt.close(fig)

    finest = high[-1]
    toa_path = CASE_DIR / finest["source_files"]["toa"]
    with rasterio.open(toa_path) as source:
        values = source.read(1, masked=True)
        bounds = source.bounds
    fig, ax = plt.subplots(figsize=(7.0, 3.7), constrained_layout=True)
    image = ax.imshow(values, origin="upper",
                      extent=(bounds.left, bounds.right, bounds.bottom, bounds.top),
                      cmap="plasma")
    fig.colorbar(image, ax=ax, label="Time of arrival (s)")
    ax.set(xlabel="Easting (m)", ylabel="Northing (m)",
           title=rf"High-HRR TOA ($\Delta t={finest['dt_s']:g}$ s)")
    polish_figure(fig)
    fig.savefig(FIG_DIR / "domain_result.pdf", bbox_inches="tight",
                metadata={"CreationDate": None, "ModDate": None})
    plt.close(fig)


def write_payload(payload: dict) -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    (OUT_DIR / "metrics.json").write_text(
        json.dumps(payload, indent=2, allow_nan=False) + "\n", encoding="utf-8"
    )
    print(f"[OK] {payload['case_id']}: {payload['overall_status']}")


def main() -> None:
    for stale in (FIG_DIR / "convergence.pdf", FIG_DIR / "domain_result.pdf"):
        stale.unlink(missing_ok=True)
    try:
        spec = json.loads((CASE_DIR / "case.json").read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        write_payload({
            "case_id": "CASE49_WTC", "overall_status": "NOT EVALUABLE",
            "workflow_status": "INCOMPLETE", "verification_passed": False,
            "required_outputs_complete": False, "required_variant_count": 8,
            "completed_variant_count": 0,
            "reason": f"case specification is missing or malformed: {error}",
            "metrics": [],
        })
        return
    manifest_path = CASE_DIR / "variants" / "manifest.json"
    required = len(spec["time_steps_s"]) * len(spec["regimes"])
    if not manifest_path.is_file():
        attempt_recorded = ATTEMPT_LEDGER.exists()
        write_payload({
            "case_id": spec["case_id"], "overall_status": "NOT EVALUABLE",
            "workflow_status": "INCOMPLETE" if attempt_recorded else "NOT RUN",
            "verification_passed": False, "required_outputs_complete": False,
            "required_variant_count": required, "completed_variant_count": 0,
            "reason": (
                "preprocessing manifest is absent after an execution attempt"
                if attempt_recorded else "preprocessing manifest is absent"
            ),
            "metrics": [],
            **({"attempt_ledger": str(ATTEMPT_LEDGER.relative_to(CASE_DIR))}
               if attempt_recorded else {}),
        })
        return
    try:
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        write_payload({
            "case_id": spec["case_id"], "overall_status": "NOT EVALUABLE",
            "workflow_status": "INCOMPLETE", "verification_passed": False,
            "required_outputs_complete": False, "required_variant_count": required,
            "completed_variant_count": 0,
            "reason": f"variant manifest is malformed: {error}", "metrics": [],
        })
        return
    expected_ids = {
        f"{regime['id']}_dt_{str(float(dt)).replace('.', 'p')}s"
        for regime in spec["regimes"]
        for dt in spec["time_steps_s"]
    }
    variants = manifest.get("variants", [])
    manifest_ids = [str(item.get("id")) for item in variants if isinstance(item, dict)]
    if (
        manifest.get("case_id") != spec["case_id"]
        or manifest.get("source_revision") != spec["source_revision"]
        or len(variants) != required
        or len(manifest_ids) != len(set(manifest_ids))
        or set(manifest_ids) != expected_ids
    ):
        write_payload({
            "case_id": spec["case_id"], "overall_status": "NOT EVALUABLE",
            "workflow_status": "INCOMPLETE", "verification_passed": False,
            "required_outputs_complete": False, "required_variant_count": required,
            "completed_variant_count": 0,
            "reason": "variant manifest identity/provenance contract failed",
            "metrics": [],
        })
        return
    rows = []
    unavailable = {}
    for item in variants:
        try:
            rows.append(profile_variant(spec, item))
        except (OSError, ValueError, KeyError, csv.Error, rasterio.errors.RasterioError) as error:
            unavailable[item["id"]] = str(error)
    if unavailable or len(rows) != required:
        attempt_recorded = ATTEMPT_LEDGER.exists()
        never_run = not attempt_recorded and bool(unavailable) and not rows and all(
            reason == "successful-run fingerprint marker is absent"
            for reason in unavailable.values()
        )
        write_payload({
            "case_id": spec["case_id"], "overall_status": "NOT EVALUABLE",
            "workflow_status": "NOT RUN" if never_run else "INCOMPLETE",
            "verification_passed": False, "required_outputs_complete": False,
            "required_variant_count": required, "completed_variant_count": len(rows),
            "unavailable_variants": unavailable, "variants": rows,
            "reason": (
                "ELMFIRE execution was attempted, but one or more current-fingerprint variants lack valid required evidence."
                if attempt_recorded else (
                    "ELMFIRE has not been run for the current prepared-input fingerprints."
                    if never_run else
                    "One or more current-fingerprint variants lack valid required evidence."
                )
            ),
            "metrics": [metric("current-fingerprint output completeness", required,
                               len(rows), "variants", None)],
            **({"attempt_ledger": str(ATTEMPT_LEDGER.relative_to(CASE_DIR))}
               if attempt_recorded else {}),
        })
        return

    executable_hashes = {str(row["executable_sha256"]) for row in rows}
    if len(executable_hashes) != 1:
        write_payload({
            "case_id": spec["case_id"], "overall_status": "NOT EVALUABLE",
            "workflow_status": "INCOMPLETE", "verification_passed": False,
            "required_outputs_complete": False, "required_variant_count": required,
            "completed_variant_count": len(rows), "variants": rows,
            "reason": "Variants were executed with more than one binary SHA-256.",
            "metrics": [metric("single executable identity", 1,
                               len(executable_hashes), "SHA-256 values", None)],
        })
        return

    groups = {
        regime["id"]: sorted(
            [row for row in rows if row["regime"] == regime["id"]],
            key=lambda row: row["dt_s"], reverse=True,
        )
        for regime in spec["regimes"]
    }
    high = groups["threshold_stress"]
    low = groups["subthreshold"]
    high_propagation_complete = all(
        row["toa_ros_m_s"] is not None
        and row["toa_fit_r2"] is not None
        and all(value is not None for value in row["landmark_toa_s"])
        for row in high
    )
    limits = spec["metrics"]
    maximum_low_energy = max(row["maximum_sampled_cell_step_energy_kj"] for row in low)
    minimum_high_energy = min(row["maximum_sampled_cell_step_energy_kj"] for row in high)
    maximum_step_deviation = max(row["maximum_logged_step_deviation_s"] for row in rows)
    high_heat_change = relative_change(high[-2]["total_received_heat_kj"],
                                       high[-1]["total_received_heat_kj"])
    low_heat_change = relative_change(low[-2]["total_received_heat_kj"],
                                      low[-1]["total_received_heat_kj"])
    minimum_order = float(limits["minimum_observed_order"])
    metrics = [
        metric("current-fingerprint output completeness", required, len(rows),
               "variants", len(rows) == required),
        metric("low-HRR positive sampled FBFM91 cell-step energy", "> 0 and finite",
               maximum_low_energy, "kJ", math.isfinite(maximum_low_energy) and maximum_low_energy > 0.0),
        metric("high-HRR positive sampled FBFM91 cell-step energy", "> 0 and finite",
               minimum_high_energy, "kJ", math.isfinite(minimum_high_energy) and minimum_high_energy > 0.0),
        metric("maximum logged solver-step deviation",
               f"<= {limits['maximum_logged_step_deviation_s']}",
               maximum_step_deviation, "s",
               maximum_step_deviation <= limits["maximum_logged_step_deviation_s"]),
    ]
    if not high_propagation_complete:
        reason = (
            "high-HRR TOA observables are incomplete"
        )
        metrics.extend([
            metric("finest-pair landmark TOA relative L2 change",
                   f"<= {limits['finest_pair_landmark_toa_relative_l2_max']}",
                   None, "fraction", None),
            metric("finest-pair community ROS relative change",
                   f"<= {limits['finest_pair_ros_relative_change_max']}",
                   None, "fraction", None),
            metric("high-HRR finest-pair total-heat relative change",
                   f"<= {limits['finest_pair_total_heat_relative_change_max']}",
                   high_heat_change, "fraction",
                   high_heat_change <= limits["finest_pair_total_heat_relative_change_max"]),
            metric("low-HRR finest-pair total-heat relative change",
                   f"<= {limits['finest_pair_total_heat_relative_change_max']}",
                   low_heat_change, "fraction",
                   low_heat_change <= limits["finest_pair_total_heat_relative_change_max"]),
            metric("landmark TOA observed temporal order",
                   f">= {minimum_order} or roundoff-flat three-level sequence",
                   None, "-", None),
            metric("community ROS observed temporal order",
                   f">= {minimum_order} or roundoff-flat three-level sequence",
                   None, "-", None),
            order_metric("high-HRR heat observed temporal order",
                         [row["total_received_heat_kj"] for row in high], minimum_order),
            order_metric("low-HRR heat observed temporal order",
                         [row["total_received_heat_kj"] for row in low], minimum_order),
            metric("minimum high-HRR TOA-fit coefficient of determination",
                   f">= {limits['minimum_toa_fit_r2']}", None, "-", None),
        ])
        write_payload({
            "case_id": spec["case_id"], "overall_status": "NOT EVALUABLE",
            "workflow_status": "INCOMPLETE",
            "verification_passed": False, "required_outputs_complete": False,
            "required_variant_count": required, "completed_variant_count": len(rows),
            "reason": reason, "source_revision": spec["source_revision"],
            "executed_binary_sha256": next(iter(executable_hashes)),
            "variants": rows, "metrics": metrics,
        })
        return

    high_toa_change = relative_vector_change(high[-2]["landmark_toa_s"],
                                              high[-1]["landmark_toa_s"])
    high_ros_change = relative_change(high[-2]["toa_ros_m_s"], high[-1]["toa_ros_m_s"])
    minimum_r2 = min(row["toa_fit_r2"] for row in high)
    metrics.extend([
        metric("finest-pair landmark TOA relative L2 change",
               f"<= {limits['finest_pair_landmark_toa_relative_l2_max']}",
               high_toa_change, "fraction",
               high_toa_change <= limits["finest_pair_landmark_toa_relative_l2_max"]),
        metric("finest-pair community ROS relative change",
               f"<= {limits['finest_pair_ros_relative_change_max']}", high_ros_change,
               "fraction", high_ros_change <= limits["finest_pair_ros_relative_change_max"]),
        metric("high-HRR finest-pair total-heat relative change",
               f"<= {limits['finest_pair_total_heat_relative_change_max']}",
               high_heat_change, "fraction",
               high_heat_change <= limits["finest_pair_total_heat_relative_change_max"]),
        metric("low-HRR finest-pair total-heat relative change",
               f"<= {limits['finest_pair_total_heat_relative_change_max']}",
               low_heat_change, "fraction",
               low_heat_change <= limits["finest_pair_total_heat_relative_change_max"]),
        order_metric("landmark TOA observed temporal order",
                     [row["landmark_toa_s"] for row in high], minimum_order),
        order_metric("community ROS observed temporal order",
                     [row["toa_ros_m_s"] for row in high], minimum_order),
        order_metric("high-HRR heat observed temporal order",
                     [row["total_received_heat_kj"] for row in high], minimum_order),
        order_metric("low-HRR heat observed temporal order",
                     [row["total_received_heat_kj"] for row in low], minimum_order),
        metric("minimum high-HRR TOA-fit coefficient of determination",
               f">= {limits['minimum_toa_fit_r2']}", minimum_r2, "-",
               minimum_r2 >= limits["minimum_toa_fit_r2"]),
    ])
    passed = all(item["status"] == "PASS" for item in metrics)
    plot_results(spec, groups)
    write_payload({
        "case_id": spec["case_id"], "overall_status": "PASS" if passed else "FAIL",
        "workflow_status": "COMPLETE",
        "verification_passed": passed, "required_outputs_complete": True,
        "required_variant_count": required, "completed_variant_count": len(rows),
        "source_revision": spec["source_revision"], "variants": rows,
        "executed_binary_sha256": next(iter(executable_hashes)),
        "reason": ("All temporal-convergence acceptance components passed."
                   if passed else "One or more temporal-convergence acceptance components failed."),
        "metrics": metrics,
    })


if __name__ == "__main__":
    main()
