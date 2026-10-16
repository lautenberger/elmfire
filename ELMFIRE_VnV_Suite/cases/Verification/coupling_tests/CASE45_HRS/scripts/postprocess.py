#!/usr/bin/env python3
"""Evaluate CASE45 from case-local black-box raster outputs only."""
from __future__ import annotations

from report_language import polish_figure

import csv
import json
import math
import re
from datetime import datetime, timezone
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

from fingerprint import (
    aggregate_fingerprint,
    file_snapshot,
    output_snapshot,
    sha256_file,
    variant_fingerprint,
)

CASE_DIR = Path(__file__).resolve().parents[1]
OUT_PATH = CASE_DIR / "outputs/metrics.json"
EXPECTED_PATH = CASE_DIR / "variants/expected.json"
SOURCE_COMMIT = "a2dfbcdf72209733c000e5d3431e44723e15efea"
DX_M = 20.0
DT_S = 1.0
ROS_REL_TOL = 0.005
ENERGY_TARGET_REL_TOL = 0.002
PAIR_REL_TOL = 2.0e-5
TOA_ABS_TOL_S = 1.0e-6
REQUIRED_IDS = [
    "energy_below",
    "energy_equal",
    "energy_above",
    "wind_below_35",
    "wind_equal_35",
    "wind_above_35",
    "normal_back",
    "normal_side",
    "hrr_half",
    "hrr_double",
    "ftp_low",
    "ftp_high",
]
TRANSIENT_INDEX_RE = re.compile(r"_d(\d{7})\.tif$", re.IGNORECASE)
PDF_METADATA = {
    "Creator": "CASE45_HRS",
    "Producer": "Matplotlib",
    "CreationDate": datetime(2026, 1, 1, tzinfo=timezone.utc),
    "ModDate": datetime(2026, 1, 1, tzinfo=timezone.utc),
}


def load_fingerprint_manifest() -> dict[str, object]:
    path = CASE_DIR / "variants/run_fingerprints.json"
    data = json.loads(path.read_text(encoding="utf-8"))
    variants = data.get("variants")
    source_review = data.get("source_review")
    if (
        data.get("schema_version") != 2
        or data.get("case_id") != "CASE45_HRS"
        or data.get("source_commit") != SOURCE_COMMIT
        or not isinstance(variants, dict)
        or not isinstance(source_review, dict)
        or source_review.get("reviewed_files_match_commit") is not True
        or source_review.get("runtime_source_inspection") is not False
        or aggregate_fingerprint(variants) != data.get("aggregate_sha256")
        or set(variants) != set(REQUIRED_IDS)
        or data.get("oracle_manifest_sha256") != sha256_file(EXPECTED_PATH)
        or data.get("oracle_evaluator_sha256") != sha256_file(Path(__file__))
    ):
        raise ValueError("generated runtime-input fingerprint manifest is invalid")
    return data


def require_current_success(
    root: Path,
    variant_id: str,
    expected: dict[str, object],
    specification: dict[str, object],
    fingerprints: dict[str, object],
) -> dict[str, object]:
    current = variant_fingerprint(root)
    if current["sha256"] != expected.get("sha256"):
        raise ValueError("current runtime inputs do not match their generated fingerprint")
    marker_path = root / "outputs/run_complete.json"
    marker = json.loads(marker_path.read_text(encoding="utf-8"))
    variant_metadata_path = root / "variant.json"
    variant_metadata = json.loads(variant_metadata_path.read_text(encoding="utf-8"))
    if (
        marker.get("schema_version") != 2
        or marker.get("case_id") != "CASE45_HRS"
        or marker.get("variant_id") != variant_id
        or marker.get("source_commit") != SOURCE_COMMIT
        or marker.get("status") != "ELMFIRE_EXIT_0"
        or marker.get("successful_exit") is not True
        or marker.get("input_fingerprint_sha256") != current["sha256"]
        or marker.get("runtime_input_file_count") != current["file_count"]
        or marker.get("oracle_manifest_sha256") != fingerprints.get("oracle_manifest_sha256")
        or marker.get("oracle_evaluator_sha256") != fingerprints.get("oracle_evaluator_sha256")
        or marker.get("variant_metadata_sha256") != sha256_file(variant_metadata_path)
        or variant_metadata != specification
        or not marker.get("completed_utc")
    ):
        raise ValueError("successful-run marker is missing, stale, or fingerprint-mismatched")

    executable_path = Path(str(marker.get("executable_resolved", "")))
    executable_digest = str(marker.get("executable_sha256", ""))
    if (
        not executable_path.is_absolute()
        or not executable_path.is_file()
        or len(executable_digest) != 64
        or sha256_file(executable_path) != executable_digest
        or marker.get("executable_size_bytes") != executable_path.stat().st_size
    ):
        raise ValueError("completion marker does not verify the executed binary")

    expected_logs = [
        CASE_DIR / f"logs/{variant_id}.stdout",
        CASE_DIR / f"logs/{variant_id}.stderr",
    ]
    if marker.get("log_snapshot") != file_snapshot(expected_logs, CASE_DIR):
        raise ValueError("stdout/stderr snapshot changed after the recorded run")
    if marker.get("output_snapshot") != output_snapshot(root):
        raise ValueError("ELMFIRE output snapshot changed after the recorded run")
    return marker


def validate_grid(src: rasterio.io.DatasetReader, path: Path, grid: dict[str, object]) -> None:
    expected_transform = rasterio.Affine.from_gdal(*[float(v) for v in grid["transform_gdal"]])
    expected_crs = rasterio.crs.CRS.from_string(str(grid["crs"]))
    expected_nodata = float(grid["nodata"])
    if (
        src.count != 1
        or src.shape != tuple(int(v) for v in grid["shape"])
        or src.crs != expected_crs
        or not src.transform.almost_equals(expected_transform)
        or src.nodata is None
        or not math.isclose(float(src.nodata), expected_nodata, rel_tol=0.0, abs_tol=1.0e-6)
    ):
        raise ValueError(f"unexpected raster metadata in {path}")


def read_cell(path: Path, row: int, col: int, grid: dict[str, object]) -> float:
    with rasterio.open(path) as src:
        validate_grid(src, path, grid)
        values = src.read(1, masked=True)
        if bool(np.ma.getmaskarray(values)[row, col]):
            raise ValueError(f"receiver is nodata in {path}")
        value = float(values[row, col])
    if not math.isfinite(value):
        raise ValueError(f"nonfinite receiver value in {path}")
    return value


def dump_manifest(
    output_dir: Path, expected_records: list[dict[str, object]]
) -> tuple[list[dict[str, object]], int, int]:
    paths = sorted(output_dir.glob("dump_times_*.csv"))
    if len(paths) != 1:
        raise ValueError(f"expected one dump-times CSV, found {len(paths)}")
    with paths[0].open(newline="", encoding="utf-8") as stream:
        rows = list(csv.DictReader(stream, skipinitialspace=True))
    required_columns = {"dump_index", "time_seconds", "is_final_dump"}
    if not rows or not required_columns.issubset(rows[0]):
        raise ValueError("dump-times CSV is empty")
    parsed: list[dict[str, object]] = []
    for row in rows:
        flag = str(row["is_final_dump"]).strip().upper()
        if flag not in {"T", "TRUE", "1", "F", "FALSE", "0"}:
            raise ValueError("dump-times final flag is malformed")
        parsed.append(
            {
                "dump_index": int(row["dump_index"]),
                "time_seconds": float(row["time_seconds"]),
                "is_final_dump": flag in {"T", "TRUE", "1"},
            }
        )
    if len(parsed) != len(expected_records):
        raise ValueError("dump-times record count differs from the preregistered schedule")
    for actual, expected in zip(parsed, expected_records):
        if (
            actual["dump_index"] != int(expected["dump_index"])
            or not math.isclose(
                float(actual["time_seconds"]),
                float(expected["time_seconds"]),
                rel_tol=0.0,
                abs_tol=1.0e-6,
            )
            or actual["is_final_dump"] is not bool(expected["is_final_dump"])
        ):
            raise ValueError("dump-times records differ from indices 1--5 at 0--4 s")
    indices = [int(record["dump_index"]) for record in parsed]
    if len(indices) != len(set(indices)):
        raise ValueError("dump-times indices are duplicated")
    finals = [record for record in parsed if bool(record["is_final_dump"])]
    if len(finals) != 1 or finals[0] != parsed[-1]:
        raise ValueError("the final dump flag is not unique and terminal")
    return parsed, int(finals[0]["dump_index"]), int(round(float(finals[0]["time_seconds"])))


def indexed(paths: list[Path]) -> dict[int, Path]:
    result: dict[int, Path] = {}
    for path in paths:
        match = TRANSIENT_INDEX_RE.search(path.name)
        if match is None:
            raise ValueError(f"unrecognized dump suffix: {path.name}")
        key = int(match.group(1))
        if key in result:
            raise ValueError(f"duplicate dump index {key}")
        result[key] = path
    return result


def transient_cell(
    output_dir: Path,
    stem: str,
    row: int,
    col: int,
    valid_indices: list[int],
    grid: dict[str, object],
) -> tuple[np.ndarray, dict[int, Path]]:
    paths = sorted(output_dir.glob(f"{stem}_*_d*.tif"))
    files = indexed(paths)
    if not files or set(files) != set(valid_indices):
        raise ValueError(f"{stem} stack is missing, stale, or not aligned to dump_times")
    values: list[float] = []
    for index in valid_indices:
        path = files[index]
        with rasterio.open(path) as src:
            validate_grid(src, path, grid)
            raster = src.read(1, masked=True)
            if np.any(np.ma.getmaskarray(raster)):
                raise ValueError(f"{path.name} contains nodata in a physical heat field")
            array = np.asarray(raster, dtype=np.float64)
        if not np.all(np.isfinite(array)):
            raise ValueError(f"{path.name} contains nonfinite heat values")
        values.append(float(array[row, col]))
    return np.asarray(values, dtype=float), files


def terminal_cell(
    output_dir: Path,
    stem: str,
    terminal_stamp: int,
    row: int,
    col: int,
    grid: dict[str, object],
) -> tuple[float, str]:
    matches = sorted(output_dir.glob(f"{stem}_*_{terminal_stamp:07d}.tif"))
    matches = [path for path in matches if TRANSIENT_INDEX_RE.search(path.name) is None]
    if len(matches) != 1:
        raise ValueError(f"expected one terminal {stem} raster, found {len(matches)}")
    return (
        read_cell(matches[0], row, col, grid),
        str(matches[0].relative_to(CASE_DIR)),
    )


def local_velocity(recorded_energy: float, ftp_crit_kj_m2: float, ellipse: dict[str, float], normal: list[float]) -> float:
    """Downscaled WU-E Section-1 heat/FTP velocity oracle."""
    absolute_u = min(1.0e5, 60.0 * recorded_energy / (0.3048 * DT_S * DX_M * ftp_crit_kj_m2))
    front = ellipse["major_m"] + ellipse["eccentricity_m"]
    side = 2.0 * ellipse["minor_m"]
    back = ellipse["major_m"] - ellipse["eccentricity_m"]
    total = max(1.0e-5, front + side + back)
    v_head, v_back, v_side = absolute_u * front / total, absolute_u * back / total, absolute_u * side / total
    low = min((v_head + v_back) / (2.0 * v_side), 10.0) if v_side > 1.0e-4 else 1.0
    nx, ny = float(normal[0]), float(normal[1])
    cosang, sinang = nx, -ny  # DMS unit vector is east for wind from 270 degrees.
    aa = max(0.5 * (v_head + v_back), 1.0e-10)
    bb = 0.5 * max((v_head + v_back) / low, 1.0e-10)
    denom = max(math.sqrt(aa**2 * cosang**2 + bb**2 * sinang**2), 1.0e-10)
    dydt = aa**2 * cosang / denom + 0.5 * (v_head - v_back)
    dxdt = bb**2 * sinang / denom
    return math.hypot(dydt, dxdt)


def relerr(actual: float, expected: float) -> float:
    return abs(actual - expected) / max(abs(expected), 1.0e-12)


def metric(name: str, expected: str, calculated: object, units: str, passed: bool) -> dict[str, object]:
    return {
        "name": name,
        "expected": expected,
        "calculated": calculated,
        "units": units,
        "status": "PASS" if passed else "FAIL",
    }


def unavailable(
    reason: str, required: int, completed: int, workflow_status: str = "INCOMPLETE"
) -> dict[str, object]:
    names = [
        "recorded heat-to-ROS oracle relative error",
        "receiver LIST_BURNED observability",
        "fixed-FTP continuity across 30000 kJ",
        "wind continuity across 35 mph",
        "HRR amplitude response",
        "head/back/side response",
        "FTP_CRIT inverse response",
    ]
    return {
        "case_id": "CASE45_HRS",
        "source_commit": SOURCE_COMMIT,
        "overall_status": "NOT EVALUABLE",
        "workflow_status": workflow_status,
        "verification_passed": False,
        "required_outputs_complete": False,
        "required_variant_count": required,
        "completed_variant_count": completed,
        "reason": reason,
        "metrics": [
            {"name": n, "expected": "case criterion", "calculated": None, "units": "--", "status": "NOT EVALUABLE"}
            for n in names
        ],
    }


def save_figure(rows: list[dict[str, object]]) -> None:
    """Plot measured speeds, heat stimuli, and spatial output on separate figures."""
    condition_names = {
        "energy_below": "Energy below 30 MJ",
        "energy_equal": "Energy equal to 30 MJ",
        "energy_above": "Energy above 30 MJ",
        "wind_below_35": "Wind speed 34.9 mph",
        "wind_equal_35": "Wind speed 35 mph",
        "wind_above_35": "Wind speed 35.1 mph",
        "normal_back": "Backward-facing front",
        "normal_side": "Crosswind-facing front",
        "hrr_half": "Half reference heat release",
        "hrr_double": "Twice reference heat release",
        "ftp_low": "Lower material\nfire-thermal property",
        "ftp_high": "Higher material\nfire-thermal property",
    }
    labels = [condition_names.get(str(r["id"]), str(r["id"]).replace("_", " ")) for r in rows]
    y = np.arange(len(rows))
    for filename, title in (("response_summary", "Receiver spread: reference and simulation"), ("heat_stimulus", "Simulated energy at receiver arrival")):
        fig, ax = plt.subplots(figsize=(7.2, 5.8), constrained_layout=True)
        if filename == "response_summary":
            ax.plot([float(r["oracle_vs_ft_min"]) for r in rows], y, "o", label="Independent prediction\n(fixed material property)")
            ax.plot([float(r["measured_vs_ft_min"]) for r in rows], y, "x", ms=8, label="ELMFIRE")
            ax.set_xlabel("Local spread rate (ft/min)")
        else:
            ax.barh(y, [float(r["recorded_step_energy_kj"]) / 1000 for r in rows])
            ax.axvline(30, ls="--", color="black", label="30 MJ (diagnostic)")
            ax.set_xlabel("Arrival-step energy (MJ)")
        ax.set_yticks(y, labels)
        ax.invert_yaxis()
        ax.grid(axis="x", alpha=0.25)
        ax.set_title(title)
        ax.legend(loc="lower right")
        polish_figure(fig)
        fig.savefig(CASE_DIR / f"figures/{filename}.pdf", metadata=PDF_METADATA)
        plt.close(fig)
    row = next(r for r in rows if r["id"] == "wind_equal_35")
    with rasterio.open(CASE_DIR / str(row["selected_files"]["spread_rate"])) as src:
        a = src.read(1, masked=True)
        extent = (src.bounds.left, src.bounds.right, src.bounds.bottom, src.bounds.top)
        points = [rasterio.transform.xy(src.transform, *row[key]) for key in ("source_row_col", "receiver_row_col")]
    fig, ax = plt.subplots(figsize=(7.2, 5.6), constrained_layout=True)
    im = ax.imshow(a, origin="upper", extent=extent)
    for xy, marker, label in zip(points, ("o", "*"), ("Source", "Receiver")):
        ax.plot(*xy, marker=marker, ms=10, mfc="white", mec="black", label=label)
    ax.legend()
    fig.colorbar(im, ax=ax, label="Spread rate (ft/min)")
    ax.set(xlabel="Easting (m)", ylabel="Northing (m)", title="Whole domain: wind_equal_35, t = 4 s")
    polish_figure(fig)
    fig.savefig(CASE_DIR / "figures/domain_result.pdf", metadata=PDF_METADATA)
    plt.close(fig)



def validate_expected(
    expected: dict[str, object], fingerprints: dict[str, object]
) -> list[dict[str, object]]:
    specs = expected.get("variants")
    grid = expected.get("grid")
    time_control = expected.get("time_control")
    observability = expected.get("receiver_observability")
    if (
        expected.get("schema_version") != 2
        or expected.get("case_id") != "CASE45_HRS"
        or expected.get("source_commit") != SOURCE_COMMIT
        or expected.get("runtime_input_fingerprint_sha256")
        != fingerprints.get("aggregate_sha256")
        or not isinstance(specs, list)
        or [str(item.get("id")) for item in specs if isinstance(item, dict)] != REQUIRED_IDS
        or not isinstance(grid, dict)
        or grid.get("shape") != [25, 25]
        or not math.isclose(float(grid.get("cell_size_m", math.nan)), DX_M)
        or grid.get("crs") != "EPSG:32610"
        or not isinstance(time_control, dict)
        or not math.isclose(float(time_control.get("simulation_tstart_s", math.nan)), 0.0)
        or not math.isclose(float(time_control.get("simulation_tstop_s", math.nan)), 4.0)
        or not math.isclose(float(time_control.get("simulation_dt_s", math.nan)), DT_S)
        or not math.isclose(float(time_control.get("simulation_dtmax_s", math.nan)), DT_S)
        or not math.isclose(float(time_control.get("dormant_ignition_time_s", math.nan)), 5.0)
        or not isinstance(time_control.get("expected_dump_records"), list)
        or not isinstance(observability, dict)
        or not math.isclose(float(observability.get("receiver_phi", math.nan)), 0.001)
        or not math.isclose(float(observability.get("required_receiver_toa_s", math.nan)), 1.0)
    ):
        raise ValueError("expected.json violates the fixed CASE45 identity or experiment contract")
    fingerprint_variants = fingerprints.get("variants")
    if not isinstance(fingerprint_variants, dict):
        raise ValueError("fingerprint manifest has no variant mapping")
    for spec in specs:
        if not isinstance(spec, dict):
            raise ValueError("variant specification is not an object")
        variant_id = str(spec["id"])
        fingerprint = fingerprint_variants.get(variant_id)
        if (
            not isinstance(fingerprint, dict)
            or spec.get("runtime_input_fingerprint_sha256") != fingerprint.get("sha256")
            or float(spec.get("receiver_phi_margin_factor", 0.0)) <= 1.0
        ):
            raise ValueError(f"{variant_id} has inconsistent inputs or no crossing margin")
    return specs


def main() -> None:
    (CASE_DIR / "figures/response_summary.pdf").unlink(missing_ok=True)
    markers_present = len(list((CASE_DIR / "variants").glob("*/outputs/run_complete.json")))
    attempted = any(
        (CASE_DIR / "logs" / f"{variant_id}.{stream}").is_file()
        for variant_id in REQUIRED_IDS
        for stream in ("stdout", "stderr")
    )
    try:
        expected = json.loads(EXPECTED_PATH.read_text(encoding="utf-8"))
        fingerprints = load_fingerprint_manifest()
        specs = validate_expected(expected, fingerprints)
    except (OSError, TypeError, ValueError, KeyError, json.JSONDecodeError) as exc:
        workflow = "NOT RUN" if markers_present == 0 and not attempted else "INCOMPLETE"
        payload = unavailable(
            f"generated variant/oracle manifest is unavailable: {exc}",
            len(REQUIRED_IDS),
            0,
            workflow,
        )
        OUT_PATH.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
        print(f"[NOT EVALUABLE] CASE45_HRS: {payload['reason']}")
        return

    grid = dict(expected["grid"])
    expected_records = list(expected["time_control"]["expected_dump_records"])
    required_toa = float(expected["receiver_observability"]["required_receiver_toa_s"])
    if markers_present == 0 and not attempted:
        payload = unavailable(
            "ELMFIRE has not been run; no variant completion markers exist.",
            len(specs),
            0,
            "NOT RUN",
        )
        payload["runtime_input_fingerprint_sha256"] = fingerprints["aggregate_sha256"]
        payload["oracle_manifest_sha256"] = fingerprints["oracle_manifest_sha256"]
        payload["oracle_evaluator_sha256"] = fingerprints["oracle_evaluator_sha256"]
        OUT_PATH.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
        print("[NOT EVALUABLE] CASE45_HRS: ELMFIRE has not been run")
        return
    rows: list[dict[str, object]] = []
    failures: dict[str, str] = {}
    executable_identities: set[tuple[str, str, int]] = set()
    for spec in specs:
        variant_id = str(spec["id"])
        root = CASE_DIR / "variants" / variant_id
        output_dir = root / "outputs"
        row, col = (int(v) for v in spec["receiver_row_col"])
        try:
            fingerprint = fingerprints["variants"].get(variant_id)
            if not isinstance(fingerprint, dict):
                raise ValueError("variant fingerprint metadata is missing or inconsistent")
            marker = require_current_success(
                root, variant_id, fingerprint, spec, fingerprints
            )
            executable_identities.add(
                (
                    str(marker["executable_resolved"]),
                    str(marker["executable_sha256"]),
                    int(marker["executable_size_bytes"]),
                )
            )
            records, final_index, terminal_stamp = dump_manifest(
                output_dir, expected_records
            )
            indices = [int(record["dump_index"]) for record in records]
            times = np.asarray([float(record["time_seconds"]) for record in records])
            dfc, dfc_files = transient_cell(
                output_dir, "hf_dfc_transient", row, col, indices, grid
            )
            rad, rad_files = transient_cell(
                output_dir, "hf_rad_transient", row, col, indices, grid
            )
            if dfc.shape != rad.shape or not np.all(np.isfinite(dfc + rad)):
                raise ValueError("transient receiver histories are malformed")
            if np.any(dfc < -1.0e-7) or np.any(rad < -1.0e-7):
                raise ValueError("recorded heat contains a negative receiver value")
            if abs(float(dfc[-1])) > 1.0e-7 or abs(float(rad[-1])) > 1.0e-7:
                raise ValueError("exact-stop transient heat fields were not reset before the final dump")

            receiver_toa, toa_path = terminal_cell(
                output_dir, "time_of_arrival", terminal_stamp, row, col, grid
            )
            measured, vs_path = terminal_cell(
                output_dir, "vs", terminal_stamp, row, col, grid
            )
            if not math.isclose(
                receiver_toa, required_toa, rel_tol=0.0, abs_tol=TOA_ABS_TOL_S
            ):
                raise ValueError(
                    f"receiver TOA {receiver_toa} s is not the required first-heated step at {required_toa} s"
                )
            arrival_positions = np.flatnonzero(
                np.isclose(times, receiver_toa, rtol=0.0, atol=TOA_ABS_TOL_S)
            )
            if arrival_positions.size != 1:
                raise ValueError("receiver arrival does not select exactly one transient heat record")
            arrival_position = int(arrival_positions[0])
            heat = dfc + rad
            q = float(heat[arrival_position])
            if q <= 0.0:
                raise ValueError("the receiver's arrival-step heat stimulus is not positive")
            energy = q * DT_S * DX_M**2
            binary32_design = dict(spec["binary32_input_design"])
            ellipse_oracle = {
                "major_m": float(binary32_design["ellipse_major_m"]),
                "minor_m": float(binary32_design["ellipse_minor_m"]),
                "eccentricity_m": float(binary32_design["ellipse_eccentricity_m"]),
            }
            oracle = local_velocity(
                energy,
                float(spec["ftp_crit_table_kj_m2"]),
                ellipse_oracle,
                list(spec["normal_xy"]),
            )
            rows.append(
                {
                    **spec,
                    "recorded_peak_heat_kw_m2": q,
                    "recorded_step_energy_kj": energy,
                    "receiver_toa_s": receiver_toa,
                    "selected_heat_time_s": float(times[arrival_position]),
                    "selected_heat_dump_index": indices[arrival_position],
                    "measured_vs_ft_min": measured,
                    "oracle_vs_ft_min": oracle,
                    "ros_relative_error": relerr(measured, oracle),
                    "selected_files": {
                        "dfc": str(dfc_files[indices[arrival_position]].relative_to(CASE_DIR)),
                        "radiation": str(rad_files[indices[arrival_position]].relative_to(CASE_DIR)),
                        "spread_rate": vs_path,
                        "time_of_arrival": toa_path,
                        "dump_times": str(
                            next(output_dir.glob("dump_times_*.csv")).relative_to(CASE_DIR)
                        ),
                    },
                    "terminal_dump_index": final_index,
                    "terminal_time_stamp_s": terminal_stamp,
                    "run_evidence": {
                        "completion_marker": str(
                            (root / "outputs/run_complete.json").relative_to(CASE_DIR)
                        ),
                        "executable_resolved": marker["executable_resolved"],
                        "executable_sha256": marker["executable_sha256"],
                        "log_snapshot_sha256": marker["log_snapshot"]["sha256"],
                        "output_snapshot_sha256": marker["output_snapshot"]["sha256"],
                    },
                }
            )
        except (
            OSError,
            TypeError,
            ValueError,
            KeyError,
            csv.Error,
            json.JSONDecodeError,
            rasterio.errors.RasterioError,
        ) as exc:
            failures[variant_id] = str(exc)
    if len(executable_identities) > 1:
        failures["_binary_consistency"] = (
            "variants were not executed by one resolved binary path/hash/size identity"
        )
    if failures or len(rows) != len(specs):
        workflow = "NOT RUN" if markers_present == 0 and not attempted else "INCOMPLETE"
        payload = unavailable(
            f"Required black-box artifacts are incomplete or ambiguous for {len(failures)} entries; see variant_errors.",
            len(specs),
            len(rows),
            workflow,
        )
        payload["variant_errors"] = failures
        payload["runtime_input_fingerprint_sha256"] = fingerprints["aggregate_sha256"]
        payload["oracle_manifest_sha256"] = fingerprints["oracle_manifest_sha256"]
        payload["oracle_evaluator_sha256"] = fingerprints["oracle_evaluator_sha256"]
        OUT_PATH.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
        print("[NOT EVALUABLE] CASE45_HRS: incomplete black-box evidence")
        return

    by_id = {str(r["id"]): r for r in rows}
    max_error = max(float(r["ros_relative_error"]) for r in rows)
    energy_ids = ["energy_below", "energy_equal", "energy_above"]
    energy_design_ok = all(
        relerr(float(by_id[i]["recorded_step_energy_kj"]), float(by_id[i]["designed_step_energy_kj"])) <= ENERGY_TARGET_REL_TOL
        for i in energy_ids
    )
    normalized = [
        float(by_id[i]["measured_vs_ft_min"]) / float(by_id[i]["recorded_step_energy_kj"])
        for i in energy_ids
    ]
    energy_continuity = (max(normalized) - min(normalized)) / max(abs(sum(normalized) / len(normalized)), 1.0e-12)
    energy_ok = energy_design_ok and energy_continuity <= ROS_REL_TOL
    wind_ids = ["wind_below_35", "wind_equal_35", "wind_above_35"]
    wind_error = max(float(by_id[i]["ros_relative_error"]) for i in wind_ids)
    wind_ok = wind_error <= ROS_REL_TOL
    hrr_denominator = float(by_id["hrr_half"]["recorded_peak_heat_kw_m2"])
    hrr_ratio = (
        float(by_id["hrr_double"]["recorded_peak_heat_kw_m2"]) / hrr_denominator
        if hrr_denominator > 0.0
        else math.inf
    )
    hrr_ok = abs(hrr_ratio - 4.0) <= 0.04
    normal_ids = ["wind_equal_35", "normal_back", "normal_side"]
    normal_error = max(float(by_id[i]["ros_relative_error"]) for i in normal_ids)
    observability_ok = all(
        math.isclose(
            float(row["receiver_toa_s"]), required_toa, rel_tol=0.0, abs_tol=TOA_ABS_TOL_S
        )
        and math.isfinite(float(row["measured_vs_ft_min"]))
        for row in rows
    )
    ftp_ratio = float(by_id["ftp_low"]["measured_vs_ft_min"]) / max(float(by_id["ftp_high"]["measured_vs_ft_min"]), 1.0e-12)
    ftp_ok = abs(ftp_ratio - 2.0) <= 0.01
    metrics = [
        metric("recorded heat-to-ROS oracle relative error", f"<= {ROS_REL_TOL}", max_error, "fraction", max_error <= ROS_REL_TOL),
        metric("receiver LIST_BURNED observability", "TOA = 1 s and terminal VS is finite", str(observability_ok), "--", observability_ok),
        metric("fixed-FTP continuity across 30000 kJ", f"normalized speed spread <= {ROS_REL_TOL}", energy_continuity, "fraction", energy_ok),
        metric("wind continuity across 35 mph", f"oracle error <= {ROS_REL_TOL}", wind_error, "fraction", wind_ok),
        metric("HRR amplitude response", "double/half heat ratio = 4 +/- 0.04", hrr_ratio, "ratio", hrr_ok),
        metric("head/back/side response", f"matched-wind maximum relative error <= {ROS_REL_TOL}", normal_error, "fraction", normal_error <= ROS_REL_TOL),
        metric("FTP_CRIT inverse response", "speed ratio for 3000/6000 kJ m^-2 = 2 +/- 0.01", ftp_ratio, "ratio", ftp_ok),
    ]
    passed = all(m["status"] == "PASS" for m in metrics)
    payload = {
        "case_id": "CASE45_HRS",
        "source_commit": SOURCE_COMMIT,
        "overall_status": "PASS" if passed else "FAIL",
        "workflow_status": "COMPLETE",
        "verification_passed": passed,
        "required_outputs_complete": True,
        "required_variant_count": len(specs),
        "completed_variant_count": len(rows),
        "reason": "All required run evidence was complete; every scientific metric passed." if passed else "All required run evidence was complete, but one or more scientific metrics failed.",
        "runtime_input_fingerprint_sha256": fingerprints["aggregate_sha256"],
        "oracle_manifest_sha256": fingerprints["oracle_manifest_sha256"],
        "oracle_evaluator_sha256": fingerprints["oracle_evaluator_sha256"],
        "executed_binary": {
            "resolved_path": next(iter(executable_identities))[0],
            "sha256": next(iter(executable_identities))[1],
            "size_bytes": next(iter(executable_identities))[2],
        },
        "oracle_semantics": "recorded transient heat is an input stimulus; the downstream UMD_UCB_BLDG_SPREAD mapping is recomputed independently",
        "metrics": metrics,
        "variants": rows,
    }
    OUT_PATH.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
    save_figure(rows)
    print(f"[OK] CASE45_HRS: {payload['overall_status']}")


if __name__ == "__main__":
    main()
