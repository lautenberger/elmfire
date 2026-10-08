#!/usr/bin/env python3
"""Evaluate current-fingerprint WU-E spatial-convergence outputs, fail closed."""

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
        "case_id": "CASE48_WGC",
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
    """Cross-check manifest claims against the concrete namelist and rasters."""
    dx = float(item["dx_m"])
    expected_id = f"dx_{str(dx).replace('.', 'p')}m"
    if item.get("id") != expected_id:
        raise ValueError("variant identity does not match its grid spacing")
    if item.get("working_directory") != f"variants/{expected_id}":
        raise ValueError("variant working directory is not canonical")
    if item.get("config") != "elmfire.data.in" or item.get("runnable") is not True:
        raise ValueError("variant run contract is malformed")
    if not any(math.isclose(dx, float(value), abs_tol=1.0e-12)
               for value in spec["grid_spacings_m"]):
        raise ValueError("manifest grid spacing is not in the case specification")
    expected_values = {
        "dt_s": float(spec["fixed_timestep_s"]),
        "tstop_s": float(spec["simulation_tstop_s"]),
        "dump_interval_s": float(spec["dump_interval_s"]),
    }
    for key, expected in expected_values.items():
        if not math.isclose(float(item[key]), expected, rel_tol=0.0, abs_tol=1.0e-12):
            raise ValueError(f"manifest {key} differs from the case specification")

    config = variant_dir / "elmfire.data.in"
    scalar_checks = {
        "SIMULATION_TSTART": 0.0,
        "SIMULATION_TSTOP": expected_values["tstop_s"],
        "SIMULATION_DT": expected_values["dt_s"],
        "SIMULATION_DTMAX": expected_values["dt_s"],
        "DTDUMP": expected_values["dump_interval_s"],
        "BANDTHICKNESS_WUI": float(item["bandthickness_wui_cells"]),
        "FEEDBACK_LEVEL": 1.0,
        "BLDG_FUEL_MODEL_CONSTANT": 14.0,
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
        "USE_BLDG_SPREAD_MODEL": True,
        "USE_CONSTANT_BLDG_SPREAD_MODEL_PARAMS": True,
    }
    for key, expected in logical_checks.items():
        if namelist_bool(config, key) is not expected:
            raise ValueError(f"namelist {key} differs from the variant contract")

    reference = variant_dir / "data" / "inputs" / "fbfm.tif"
    phi_path = variant_dir / "data" / "inputs" / "phi.tif"
    with rasterio.open(reference) as fbfm_source, rasterio.open(phi_path) as phi_source:
        expected_shape = (
            int(item["physical_rows"]) + 2 * int(item["buffer_cells"]),
            int(item["physical_columns"]) + 2 * int(item["buffer_cells"]),
        )
        if fbfm_source.shape != expected_shape or phi_source.shape != expected_shape:
            raise ValueError("prepared raster shape differs from the manifest geometry")
        if (not math.isclose(fbfm_source.transform.a, dx, abs_tol=1.0e-12)
                or not math.isclose(-fbfm_source.transform.e, dx, abs_tol=1.0e-12)):
            raise ValueError("prepared raster resolution differs from manifest dx")
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
) -> tuple[dict[str, Path], float, dict[str, object]]:
    """Resolve only a unique, exact-stop final output set."""
    marker = variant_dir / "logs" / "completed_input_fingerprint.txt"
    if not marker.is_file():
        raise ValueError("successful-run fingerprint marker is absent")
    expected = item["input_fingerprint"]
    if marker.read_text(encoding="utf-8").strip() != expected:
        raise ValueError("successful-run fingerprint marker is stale")
    if current_fingerprint(variant_dir) != expected:
        raise ValueError("prepared inputs differ from the manifest fingerprint")
    receipt = completion_receipt(variant_dir, item, expected, source_revision)

    manifest_path = unique(variant_dir / "outputs", "dump_times_*.csv")
    with manifest_path.open(newline="", encoding="utf-8") as stream:
        raw_rows = list(csv.DictReader(stream))
    rows = [
        {str(key).strip(): str(value).strip() for key, value in row.items()}
        for row in raw_rows
    ]
    required_columns = {"dump_index", "time_seconds", "is_final_dump"}
    if not rows or not required_columns.issubset(rows[0]):
        raise ValueError("dump-times manifest is empty or malformed")
    dump_indices = [int(row["dump_index"]) for row in rows]
    if dump_indices != list(range(1, len(rows) + 1)):
        raise ValueError("dump indices are duplicated, missing, or out of order")
    interval = float(item["dump_interval_s"])
    dump_times = [float(row["time_seconds"]) for row in rows]
    expected_times = [interval * index for index in dump_indices]
    expected_count = int(round(float(item["tstop_s"]) / interval))
    if len(rows) != expected_count or any(
        not math.isclose(actual, expected, rel_tol=0.0, abs_tol=1.0e-3)
        for actual, expected in zip(dump_times, expected_times)
    ):
        raise ValueError("dump records are not on the exact configured schedule")
    final_rows = [row for row in rows if row.get("is_final_dump", "").upper()
                  in {"T", "TRUE", "1", "Y", "YES"}]
    if len(final_rows) != 1 or final_rows[0] is not rows[-1]:
        raise ValueError(f"expected one final dump record, found {len(final_rows)}")
    final_time = float(final_rows[0]["time_seconds"])
    if not math.isclose(final_time, float(item["tstop_s"]), rel_tol=0.0,
                        abs_tol=1.0e-3):
        raise ValueError(
            f"final dump {final_time:g} s does not equal requested stop {item['tstop_s']:g} s"
        )
    stamp = int(math.floor(final_time + 0.5))
    output_dir = variant_dir / "outputs"
    return {
        "toa": unique(output_dir, f"time_of_arrival_*_{stamp:07d}.tif"),
        "dfc": unique(output_dir, f"total_dfc_received_*_{stamp:07d}.tif"),
        "rad": unique(output_dir, f"total_rad_received_*_{stamp:07d}.tif"),
    }, final_time, receipt


def aligned_array(path: Path, reference: Path) -> tuple[np.ndarray, object]:
    with rasterio.open(reference) as expected, rasterio.open(path) as source:
        if (source.count != 1 or source.shape != expected.shape
                or not source.transform.almost_equals(expected.transform)
                or source.crs != expected.crs or source.nodata != expected.nodata):
            raise ValueError(f"{path.name} is not aligned with current prepared inputs")
        values = source.read(1, masked=True).filled(np.nan).astype(float)
        transform = source.transform
    return values, transform


def logged_step_audit(variant_dir: Path, item: dict) -> tuple[int, float]:
    """Require feedback timestamps to prove the fixed step was executed."""
    log_path = variant_dir / "logs" / "elmfire.stdout"
    log_text = log_path.read_text(encoding="utf-8", errors="replace")
    logged_times = [
        float(value)
        for value in re.findall(r"Current Timestep:\s*([0-9.+\-Ee]+)\s+of", log_text)
    ]
    dt = float(item["dt_s"])
    expected_steps = int(round(float(item["tstop_s"]) / dt))
    if len(logged_times) != expected_steps:
        raise ValueError(
            f"logged {len(logged_times)} solver steps; expected {expected_steps} at fixed DT"
        )
    expected_times = np.arange(expected_steps, dtype=float) * dt
    maximum_deviation = float(np.max(np.abs(
        np.asarray(logged_times, dtype=float) - expected_times
    ))) if expected_steps else 0.0
    if maximum_deviation > 0.051:
        raise ValueError(
            f"logged solver-step timestamps differ from 0..TSTOP-DT by {maximum_deviation:g} s"
        )
    return len(logged_times), maximum_deviation


def profile_variant(spec: dict, item: dict) -> dict:
    variant_dir = CASE_DIR / item["working_directory"]
    validate_variant_contract(spec, item, variant_dir)
    paths, final_time, receipt = terminal_paths(
        variant_dir, item, str(spec["source_revision"])
    )
    logged_step_count, maximum_step_deviation = logged_step_audit(variant_dir, item)
    reference = variant_dir / "data" / "inputs" / "fbfm.tif"
    toa, transform = aligned_array(paths["toa"], reference)
    dfc, _ = aligned_array(paths["dfc"], reference)
    rad, _ = aligned_array(paths["rad"], reference)
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
    finite_profile = np.isfinite(median_toa)
    for requested_x in spec["landmark_x_m"]:
        requested_x = float(requested_x)
        lower = np.where(finite_profile & (x <= requested_x))[0]
        upper = np.where(finite_profile & (x >= requested_x))[0]
        if lower.size == 0 or upper.size == 0:
            landmarks.append(math.nan)
            continue
        left = int(lower[-1])
        right = int(upper[0])
        if left == right:
            landmarks.append(float(median_toa[left]))
        else:
            landmarks.append(float(np.interp(
                requested_x,
                [x[left], x[right]],
                [median_toa[left], median_toa[right]],
            )))
    source_width = float(spec["initial_source_width_m"])
    fit_mask = np.isfinite(median_toa) & (x > source_width + 0.25 * dx)
    if np.count_nonzero(fit_mask) < 3:
        raise ValueError("fewer than three ignited downwind columns are available for TOA ROS")
    slope, intercept = np.polyfit(x[fit_mask], median_toa[fit_mask], 1)
    fitted = slope * x[fit_mask] + intercept
    residual = float(np.sum((median_toa[fit_mask] - fitted) ** 2))
    total = float(np.sum((median_toa[fit_mask] - np.mean(median_toa[fit_mask])) ** 2))
    r2 = 1.0 - residual / total if total > 0.0 else math.nan
    if slope <= 0.0 or not math.isfinite(r2):
        raise ValueError("TOA regression does not define a finite positive propagation speed")
    landmarks_array = np.asarray(landmarks, dtype=float)
    if not np.all(np.isfinite(landmarks_array)):
        raise ValueError("one or more fixed physical landmarks did not ignite")

    finite_toa = np.isfinite(toa_region) & (toa_region >= 0.0)
    return {
        **item,
        "final_time_s": final_time,
        "landmark_toa_s": landmarks,
        "toa_ros_m_s": float(1.0 / slope),
        "toa_fit_r2": r2,
        "burned_fraction": float(np.count_nonzero(finite_toa) / finite_toa.size),
        "total_received_heat_kj": float(np.sum(heat_region)),
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
    """Estimate p from the two finest successive differences for ratio-two grids."""
    def distance(first, second):
        aa = np.asarray(first, dtype=float)
        bb = np.asarray(second, dtype=float)
        return float(np.linalg.norm(aa - bb))
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


def plot_results(spec: dict, rows: list[dict]) -> None:
    FIG_DIR.mkdir(parents=True, exist_ok=True)
    dx = np.asarray([row["dx_m"] for row in rows], dtype=float)
    fig, axes = plt.subplots(3, 1, figsize=(7.2, 8.0), constrained_layout=True)
    axes[0].plot(dx, [row["toa_ros_m_s"] for row in rows], "o-")
    axes[0].set_ylabel("TOA-derived ROS\n" + r"(m s$^{-1}$)")
    axes[1].plot(dx, [row["total_received_heat_kj"] for row in rows], "o-")
    axes[1].set_ylabel("Domain received heat (kJ)")
    for index, landmark in enumerate(spec["landmark_x_m"]):
        axes[2].plot(dx, [row["landmark_toa_s"][index] for row in rows], "o-",
                     label=f"x={landmark:g} m")
    axes[2].set_ylabel("Arrival time (s)")
    axes[2].legend(fontsize=12)
    for axis in axes:
        axis.set_xscale("log", base=2)
        axis.invert_xaxis()
        axis.set_xlabel(r"Grid spacing $\Delta x$ (m)")
        axis.grid(alpha=0.25)
    polish_figure(fig)
    fig.savefig(FIG_DIR / "convergence.pdf", bbox_inches="tight",
                metadata={"CreationDate": None, "ModDate": None})
    plt.close(fig)

    finest = rows[-1]
    toa_path = CASE_DIR / finest["source_files"]["toa"]
    with rasterio.open(toa_path) as source:
        values = source.read(1, masked=True)
        bounds = source.bounds
    fig, ax = plt.subplots(figsize=(7.0, 3.5), constrained_layout=True)
    image = ax.imshow(values, origin="upper",
                      extent=(bounds.left, bounds.right, bounds.bottom, bounds.top),
                      cmap="plasma")
    fig.colorbar(image, ax=ax, label="Time of arrival (s)")
    ax.set(xlabel="Easting (m)", ylabel="Northing (m)",
           title=rf"Finest-grid terminal TOA ($\Delta x={finest['dx_m']:g}$ m)")
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


def safe_number(value: float | None) -> float | str | None:
    if value is None:
        return None
    if math.isinf(value):
        return "+infinity" if value > 0.0 else "-infinity"
    return value


def main() -> None:
    for stale in (FIG_DIR / "convergence.pdf", FIG_DIR / "domain_result.pdf"):
        stale.unlink(missing_ok=True)
    try:
        spec = json.loads((CASE_DIR / "case.json").read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        write_payload({
            "case_id": "CASE48_WGC", "overall_status": "NOT EVALUABLE",
            "workflow_status": "INCOMPLETE", "verification_passed": False,
            "required_outputs_complete": False, "required_variant_count": 4,
            "completed_variant_count": 0,
            "reason": f"case specification is missing or malformed: {error}",
            "metrics": [],
        })
        return
    manifest_path = CASE_DIR / "variants" / "manifest.json"
    if not manifest_path.is_file():
        attempt_recorded = ATTEMPT_LEDGER.exists()
        write_payload({
            "case_id": spec["case_id"], "overall_status": "NOT EVALUABLE",
            "workflow_status": "INCOMPLETE" if attempt_recorded else "NOT RUN",
            "verification_passed": False, "required_outputs_complete": False,
            "required_variant_count": len(spec["grid_spacings_m"]),
            "completed_variant_count": 0,
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
            "required_outputs_complete": False,
            "required_variant_count": len(spec["grid_spacings_m"]),
            "completed_variant_count": 0,
            "reason": f"variant manifest is malformed: {error}", "metrics": [],
        })
        return
    required = len(spec["grid_spacings_m"])
    expected_ids = {
        f"dx_{str(float(dx)).replace('.', 'p')}m"
        for dx in spec["grid_spacings_m"]
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
    variants = sorted(variants, key=lambda item: item["dx_m"], reverse=True)
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
        completeness = metric("current-fingerprint output completeness", required,
                              len(rows), "variants", None)
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
            "metrics": [completeness],
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

    limits = spec["metrics"]
    coarse_fine, finest = rows[-2], rows[-1]
    toa_change = relative_vector_change(coarse_fine["landmark_toa_s"],
                                        finest["landmark_toa_s"])
    ros_change = relative_change(coarse_fine["toa_ros_m_s"], finest["toa_ros_m_s"])
    heat_change = relative_change(coarse_fine["total_received_heat_kj"],
                                  finest["total_received_heat_kj"])
    minimum_r2 = min(row["toa_fit_r2"] for row in rows)
    maximum_step_deviation = max(row["maximum_logged_step_deviation_s"] for row in rows)
    minimum_order = float(limits["minimum_observed_order"])
    metrics = [
        metric("current-fingerprint output completeness", required, len(rows),
               "variants", len(rows) == required),
        metric("finest-pair landmark TOA relative L2 change",
               f"<= {limits['finest_pair_landmark_toa_relative_l2_max']}",
               toa_change, "fraction",
               toa_change <= limits["finest_pair_landmark_toa_relative_l2_max"]),
        metric("finest-pair community ROS relative change",
               f"<= {limits['finest_pair_ros_relative_change_max']}", ros_change,
               "fraction", ros_change <= limits["finest_pair_ros_relative_change_max"]),
        metric("finest-pair total-heat relative change",
               f"<= {limits['finest_pair_total_heat_relative_change_max']}", heat_change,
               "fraction", heat_change <= limits["finest_pair_total_heat_relative_change_max"]),
        order_metric("landmark TOA observed spatial order",
                     [row["landmark_toa_s"] for row in rows], minimum_order),
        order_metric("community ROS observed spatial order",
                     [row["toa_ros_m_s"] for row in rows], minimum_order),
        order_metric("total-heat observed spatial order",
                     [row["total_received_heat_kj"] for row in rows], minimum_order),
        metric("minimum TOA-fit coefficient of determination",
               f">= {limits['minimum_toa_fit_r2']}", minimum_r2, "-",
               minimum_r2 >= limits["minimum_toa_fit_r2"]),
        metric("maximum logged solver-step deviation",
               f"<= {limits['maximum_logged_step_deviation_s']}",
               maximum_step_deviation, "s",
               maximum_step_deviation <= limits["maximum_logged_step_deviation_s"]),
    ]
    passed = all(item["status"] == "PASS" for item in metrics)
    plot_results(spec, rows)
    write_payload({
        "case_id": spec["case_id"], "overall_status": "PASS" if passed else "FAIL",
        "workflow_status": "COMPLETE",
        "verification_passed": passed, "required_outputs_complete": True,
        "required_variant_count": required, "completed_variant_count": len(rows),
        "source_revision": spec["source_revision"], "variants": rows,
        "executed_binary_sha256": next(iter(executable_hashes)),
        "reason": ("All spatial-convergence acceptance components passed."
                   if passed else "One or more spatial-convergence acceptance components failed."),
        "metrics": metrics,
    })


if __name__ == "__main__":
    main()
