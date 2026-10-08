#!/usr/bin/env python3
"""Fail-closed CASE46 evaluation of current, content-bound black-box outputs."""
from __future__ import annotations

from report_language import polish_figure

import csv
import datetime as dt
import hashlib
import json
import math
import os
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
from rasterio.transform import from_origin

from fingerprint import (
    aggregate_fingerprint,
    oracle_artifact_fingerprint,
    sha256_file,
    variant_fingerprint,
)

CASE_DIR = Path(__file__).resolve().parents[1]
CASE_ID = "CASE46_WUT"
SOURCE_COMMIT = "a2dfbcdf72209733c000e5d3431e44723e15efea"
METRICS_PATH = CASE_DIR / "outputs/metrics.json"
PREFLIGHT_PATH = CASE_DIR / "outputs/source_selector_preflight.json"
EXPECTED_PATH = CASE_DIR / "variants/expected.json"
FINGERPRINT_PATH = CASE_DIR / "variants/run_fingerprints.json"
ATTEMPT_LEDGER_PATH = CASE_DIR / "outputs/run_attempts.json"
EXPECTED_SHAPE = (25, 25)
EXPECTED_EPSG = 32610
EXPECTED_NODATA = -9999.0
EXPECTED_TRANSFORM = from_origin(-125.0, 125.0, 10.0, 10.0)
FINAL_TIME_S = 60.0
FINAL_TIME_STAMP = "0000060"
FINAL_DUMP_INDEX = 61
FINAL_DUMP_STAMP = "0000061"
METRIC_NAMES = [
    "evidence and exact dump ledger",
    "source fireline-intensity setup",
    "terminal PHI/TOA state consistency",
    "intended W-to-U receiver heat exposure",
    "intended W-to-U finite ignition delay",
    "contiguous strict-threshold matrix",
    "isolated distance/threshold matrix",
    "first-update transition timing",
    "zero-source control",
]
VARIANT_CONTRACT = [
    ("contiguous_below", "contiguous_interface", 1, 999.0, False),
    ("contiguous_equal", "contiguous_interface", 1, 1000.0, False),
    ("contiguous_above", "contiguous_interface", 1, 1001.0, True),
    ("isolated_d1_insufficient", "isolated_pixel", 1, 999.0, False),
    ("isolated_d1_sufficient", "isolated_pixel", 1, 1001.0, True),
    ("isolated_d2_sufficient", "isolated_pixel", 2, 1001.0, False),
    ("zero_control", "isolated_pixel", 1, 0.0, False),
]


def write_json_atomic(path: Path, payload: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f".{path.name}.tmp")
    temporary.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
    temporary.replace(path)


def metric(name: str, expected: str, calculated: object, units: str, status: str) -> dict[str, object]:
    if status not in {"PASS", "FAIL", "NOT EVALUABLE"}:
        raise ValueError(f"invalid scientific metric status {status}")
    return {
        "name": name,
        "expected": expected,
        "calculated": calculated,
        "units": units,
        "status": status,
    }


def empty_metrics() -> list[dict[str, object]]:
    return [
        metric(METRIC_NAMES[0], "61 rows: indices 1..61, times 0..60 s, only row 61 final", None, "--", "NOT EVALUABLE"),
        metric(METRIC_NAMES[1], "maximum absolute error <=0.5 kW/m and source TOA=0 s", None, "kW/m", "NOT EVALUABLE"),
        metric(METRIC_NAMES[2], "PHI<=0 iff TOA is finite at each designated target", None, "--", "NOT EVALUABLE"),
        metric(METRIC_NAMES[3], "positive DFC+radiation at sufficient-heat receivers", None, "kW/m2", "NOT EVALUABLE"),
        metric(METRIC_NAMES[4], "finite target TOA with delay > 0 s", None, "s", "NOT EVALUABLE"),
        metric(METRIC_NAMES[5], "below=false, equal=false, above=true", None, "--", "NOT EVALUABLE"),
        metric(METRIC_NAMES[6], "d1-insufficient=false, d1-sufficient=true, d2-sufficient=false", None, "--", "NOT EVALUABLE"),
        metric(METRIC_NAMES[7], "eligible target TOA=0 s and target-source delay=0 s", None, "s", "NOT EVALUABLE"),
        metric(METRIC_NAMES[8], "target remains PHI>0 with nodata TOA", None, "--", "NOT EVALUABLE"),
    ]


def base_payload(reason: str, workflow: str, completed: int = 0) -> dict[str, object]:
    if workflow not in {"NOT RUN", "INCOMPLETE", "COMPLETE"}:
        raise ValueError(f"invalid workflow status {workflow}")
    return {
        "schema_version": 2,
        "case_id": CASE_ID,
        "source_commit": SOURCE_COMMIT,
        "overall_status": "NOT EVALUABLE",
        "workflow_status": workflow,
        "verification_passed": False,
        "required_outputs_complete": False,
        "required_variant_count": len(VARIANT_CONTRACT),
        "completed_variant_count": completed,
        "runtime_input_fingerprint_sha256": None,
        "oracle_artifact_fingerprint_sha256": None,
        "preflight": "outputs/source_selector_preflight.json",
        "reason": reason,
        "metrics": empty_metrics(),
    }


def load_expected() -> tuple[dict[str, object], list[dict[str, object]]]:
    expected = json.loads(EXPECTED_PATH.read_text(encoding="utf-8"))
    if (
        expected.get("schema_version") != 2
        or expected.get("case_id") != CASE_ID
        or expected.get("source_commit") != SOURCE_COMMIT
        or expected.get("grid", {}).get("shape") != [25, 25]
        or expected.get("grid", {}).get("crs") != "EPSG:32610"
    ):
        raise ValueError("expected.json identity/grid contract is invalid")
    dump = expected.get("time_and_dump_oracle", {})
    required_dump = {
        "simulation_interval_s": [0.0, 60.0],
        "fixed_dt_s": 1.0,
        "dump_every_step": True,
        "rows": 61,
        "dump_indices": [1, 61],
        "times_s": [0.0, 60.0],
        "terminal_final_time_stamp": FINAL_TIME_STAMP,
        "terminal_dump_index_stamp": f"d{FINAL_DUMP_STAMP}",
        "terminal_flin_pattern": "flin_*_0000060.tif",
        "terminal_toa_pattern": "time_of_arrival_*_0000060.tif",
        "terminal_phi_pattern": "phi_*_d0000061.tif",
        "eligible_transition_target_toa_s": 0.0,
        "initial_source_toa_s": 0.0,
    }
    if dump != required_dump:
        raise ValueError("expected.json dump/time oracle is invalid")
    specs = expected.get("variants")
    if not isinstance(specs, list) or len(specs) != len(VARIANT_CONTRACT):
        raise ValueError("expected.json does not contain exactly seven variants")
    for spec, contract in zip(specs, VARIANT_CONTRACT):
        variant_id, topology, distance, requested_flin, expected_ignite = contract
        expected_target = [12, 11 + distance]
        if (
            spec.get("id") != variant_id
            or spec.get("topology") != topology
            or spec.get("distance_cells") != distance
            or not math.isclose(float(spec.get("requested_source_flin_kw_m")), requested_flin, abs_tol=1.0e-9)
            or spec.get("target_row_col") != expected_target
            or spec.get("expected_target_ignited") is not expected_ignite
            or spec.get("expected_target_toa_s") != (0.0 if expected_ignite else None)
            or spec.get("expected_source_toa_s") != 0.0
        ):
            raise ValueError(f"variant oracle metadata is invalid for {variant_id}")
        designed = float(spec.get("designed_source_flin_kw_m"))
        if abs(designed - requested_flin) > 1.0e-3:
            raise ValueError(f"binary32 design misses its requested FLIN for {variant_id}")
        if expected_ignite != (designed > 1000.0 and distance <= 1):
            raise ValueError(f"stored transition expectation is not derived from the strict oracle for {variant_id}")
    return expected, specs


def load_fingerprints() -> dict[str, object]:
    data = json.loads(FINGERPRINT_PATH.read_text(encoding="utf-8"))
    variants = data.get("variants")
    oracle = data.get("oracle_artifacts")
    if (
        data.get("schema_version") != 2
        or data.get("case_id") != CASE_ID
        or data.get("source_commit") != SOURCE_COMMIT
        or not isinstance(variants, dict)
        or set(variants) != {item[0] for item in VARIANT_CONTRACT}
        or not isinstance(oracle, dict)
        or aggregate_fingerprint(variants) != data.get("aggregate_sha256")
        or oracle_artifact_fingerprint(CASE_DIR) != oracle
    ):
        raise ValueError("generated runtime-input/oracle fingerprint manifest is invalid or stale")
    return data


def read_preflight() -> tuple[dict[str, object], bool]:
    data = json.loads(PREFLIGHT_PATH.read_text(encoding="utf-8"))
    configured = data.get("configured_selector", {})
    active = data.get("active_implementation_selector", {})
    source_review = data.get("source_review", {})
    if (
        data.get("case_id") != CASE_ID
        or data.get("source_commit") != SOURCE_COMMIT
        or data.get("runtime_source_inspection") is not False
        or configured.get("name") != "INTERFACE_MODEL_TYPE"
        or configured.get("value") != 2
        or active.get("name") != "CRITICAL_HF_WUI"
        or active.get("type") != "INTEGER"
        or active.get("required_value") != 2
        or not isinstance(source_review, dict)
        or source_review.get("reviewed_files_match_commit") is not True
        or source_review.get("runtime_source_inspection") is not False
        or data.get("status") not in {"PASS", "NOT EVALUABLE"}
        or data.get("selector_gate_passed") not in {True, False}
        or not isinstance(data.get("reason"), str)
        or not data.get("reason")
    ):
        raise ValueError("source-selector preflight does not match the version-pinned schema")
    assignment = active.get("assignment_in_reviewed_source")
    deterministic = active.get("deterministically_assigned_value")
    gate_passed = data.get("selector_gate_passed") is True
    if data["status"] == "PASS":
        if (
            not gate_passed
            or deterministic != 2
            or not isinstance(assignment, str)
            or not assignment.strip()
            or assignment.strip().lower() == "none found"
        ):
            raise ValueError("PASS selector preflight does not satisfy the strict assignment contract")
        return data, True
    if gate_passed or deterministic is not None or assignment != "none found":
        raise ValueError("NOT EVALUABLE selector preflight contains contradictory PASS evidence")
    return data, False


def load_attempt_ledger() -> dict[str, object]:
    data = json.loads(ATTEMPT_LEDGER_PATH.read_text(encoding="utf-8"))
    attempts = data.get("attempts")
    if (
        data.get("schema_version") != 2
        or data.get("case_id") != CASE_ID
        or data.get("source_commit_oracle") != SOURCE_COMMIT
        or type(data.get("run_case_invoked")) is not bool
        or not isinstance(attempts, list)
    ):
        raise ValueError("run-attempt ledger identity/schema is invalid")
    if data["run_case_invoked"] is False and (
        attempts
        or data.get("runner_started_utc") is not None
        or data.get("requested_executable") is not None
    ):
        raise ValueError("clean preprocess ledger contains attempted-run evidence")
    if data["run_case_invoked"] is True and (
        not isinstance(data.get("runner_started_utc"), str)
        or not isinstance(data.get("requested_executable"), str)
        or not data["requested_executable"]
    ):
        raise ValueError("invoked runner ledger lacks start/request evidence")
    return data


def validate_complete_attempts(
    ledger: dict[str, object], identities: list[dict[str, str]]
) -> None:
    attempts = ledger["attempts"]
    if ledger["run_case_invoked"] is not True or len(attempts) != len(VARIANT_CONTRACT):
        raise ValueError("complete evidence requires exactly seven recorded solver attempts")
    runner_start = dt.datetime.fromisoformat(str(ledger["runner_started_utc"]))
    if runner_start.tzinfo is None:
        raise ValueError("runner-start timestamp is timezone-naive")
    for ordinal, (attempt, contract, identity) in enumerate(
        zip(attempts, VARIANT_CONTRACT, identities), start=1
    ):
        variant_id = contract[0]
        if not isinstance(attempt, dict):
            raise ValueError(f"attempt {ordinal} is not an object")
        if (
            attempt.get("ordinal") != ordinal
            or attempt.get("variant_id") != variant_id
            or attempt.get("state") != "EXIT_0"
            or attempt.get("exit_code") != 0
            or attempt.get("execution_directory") != CASE_DIR.as_posix()
            or attempt.get("config") != f"variants/{variant_id}/elmfire.data"
            or attempt.get("stdout") != f"variants/{variant_id}/logs/elmfire.stdout"
            or attempt.get("stderr") != f"variants/{variant_id}/logs/elmfire.stderr"
            or attempt.get("executable_resolved_path") != identity["resolved_path"]
            or attempt.get("executable_sha256_at_start") != identity["sha256"]
        ):
            raise ValueError(f"attempt ledger does not match the receipt for {variant_id}")
        started = dt.datetime.fromisoformat(str(attempt.get("started_utc")))
        finished = dt.datetime.fromisoformat(str(attempt.get("finished_utc")))
        if (
            started.tzinfo is None
            or finished.tzinfo is None
            or started < runner_start
            or finished < started
            or finished > dt.datetime.now(dt.timezone.utc) + dt.timedelta(minutes=5)
        ):
            raise ValueError(f"attempt timestamps are invalid for {variant_id}")


def assignment(text: str, name: str) -> str:
    matches = re.findall(rf"(?im)^\s*{re.escape(name)}\s*=\s*([^!\r\n]+)", text)
    if len(matches) != 1:
        raise ValueError(f"namelist must assign {name} exactly once")
    return matches[0].strip().rstrip(",").upper()


def validate_namelist(path: Path, variant_id: str) -> None:
    text = path.read_text(encoding="utf-8")
    required = {
        "SIMULATION_TSTART": "0.0",
        "SIMULATION_TSTOP": "60.0",
        "SIMULATION_DT": "1.0",
        "SIMULATION_DTMAX": "1.0",
        "DTDUMP": "1.0",
        "DUMP_EVERY_STEP": ".TRUE.",
        "DUMP_FLIN": ".TRUE.",
        "DUMP_TIME_OF_ARRIVAL": ".TRUE.",
        "DUMP_PHI": ".TRUE.",
        "NUM_IGNITIONS": "1",
        "X_IGN(1)": "0.0",
        "Y_IGN(1)": "0.0",
        "T_IGN(1)": "61.0",
        "FEEDBACK_LEVEL": "1",
        "ENABLE_SPOTTING": ".FALSE.",
        "USE_BLDG_SPREAD_MODEL": ".TRUE.",
        "BLDG_SPREAD_MODEL_TYPE": "2",
        "INTERFACE_MODEL_TYPE": "2",
        "CRITICL_HF_WUI": "0.0",
    }
    for key, value in required.items():
        if assignment(text, key) != value:
            raise ValueError(f"namelist {key} is not the required {value}")
    for directory_key, suffix in (
        ("FUELS_AND_TOPOGRAPHY_DIRECTORY", "inputs"),
        ("OUTPUTS_DIRECTORY", "outputs"),
        ("MISCELLANEOUS_INPUTS_DIRECTORY", "misc"),
        ("SCRATCH", "scratch"),
    ):
        expected = f"'./VARIANTS/{variant_id.upper()}/{suffix.upper()}'"
        if assignment(text, directory_key) != expected:
            raise ValueError(f"namelist {directory_key} escapes the generated variant root")


def validate_runtime_rasters(inputs_dir: Path) -> None:
    expected_names = {
        "adj.tif", "asp.tif", "cbd.tif", "cbh.tif", "cc.tif", "ch.tif",
        "dem.tif", "fbfm40.tif", "m1.tif", "m10.tif", "m100.tif", "phi.tif",
        "slp.tif", "source_mask.tif", "wd.tif", "ws.tif",
    }
    paths = sorted(inputs_dir.glob("*.tif"))
    if {path.name for path in paths} != expected_names:
        raise ValueError("runtime GeoTIFF set is not the exact 16-file input contract")
    for path in paths:
        with rasterio.open(path) as source:
            if (
                source.count != 1
                or source.shape != EXPECTED_SHAPE
                or min(source.shape) < 10
                or source.crs is None
                or source.crs.to_epsg() != EXPECTED_EPSG
                or not source.transform.almost_equals(EXPECTED_TRANSFORM, precision=12)
                or source.nodata is None
                or not math.isclose(float(source.nodata), EXPECTED_NODATA, abs_tol=0.0)
            ):
                raise ValueError(f"runtime raster grid contract failed for {path.name}")


def live_output_snapshot(output_dir: Path) -> dict[str, object]:
    paths = sorted(
        path
        for path in output_dir.iterdir()
        if path.is_file() and path.name != "run_complete.json"
    )
    entries: list[dict[str, object]] = []
    digest = hashlib.sha256()
    for path in paths:
        relative = path.relative_to(CASE_DIR).as_posix()
        file_hash = sha256_file(path)
        size = path.stat().st_size
        digest.update(relative.encode("utf-8") + b"\0")
        digest.update(file_hash.encode("ascii") + b"\0")
        digest.update(str(size).encode("ascii") + b"\n")
        entries.append({"path": relative, "sha256": file_hash, "size_bytes": size})
    return {"sha256": digest.hexdigest(), "file_count": len(entries), "files": entries}


def validate_log(log: object, expected_path: Path) -> None:
    if not isinstance(log, dict):
        raise ValueError(f"receipt does not bind {expected_path.name}")
    relative = expected_path.relative_to(CASE_DIR).as_posix()
    if (
        log.get("path") != relative
        or not expected_path.is_file()
        or log.get("sha256") != sha256_file(expected_path)
        or log.get("size_bytes") != expected_path.stat().st_size
    ):
        raise ValueError(f"captured {expected_path.name} is missing, changed, or misidentified")


def require_current_success(
    root: Path,
    variant_id: str,
    expected_input: dict[str, object],
    expected_oracle: dict[str, object],
) -> dict[str, str]:
    validate_namelist(root / "elmfire.data", variant_id)
    validate_runtime_rasters(root / "inputs")
    current = variant_fingerprint(root)
    if current != expected_input:
        raise ValueError("current runtime inputs do not match the generated file manifest")
    marker_path = root / "outputs/run_complete.json"
    marker = json.loads(marker_path.read_text(encoding="utf-8"))
    stdout_path = root / "logs/elmfire.stdout"
    stderr_path = root / "logs/elmfire.stderr"
    if (
        marker.get("schema_version") != 2
        or marker.get("case_id") != CASE_ID
        or marker.get("variant_id") != variant_id
        or marker.get("source_commit_oracle") != SOURCE_COMMIT
        or marker.get("status") != "ELMFIRE_EXIT_0"
        or marker.get("successful_exit") is not True
        or marker.get("execution_directory") != CASE_DIR.as_posix()
        or marker.get("input_fingerprint_sha256") != current["sha256"]
        or marker.get("runtime_input_file_count") != current["file_count"]
        or marker.get("oracle_artifact_fingerprint_sha256") != expected_oracle.get("sha256")
        or marker.get("oracle_artifact_file_count") != expected_oracle.get("file_count")
    ):
        raise ValueError("successful-run receipt identity/input/oracle fields are stale or inconsistent")
    executable_path = Path(str(marker.get("executable_resolved_path", "")))
    if (
        not executable_path.is_absolute()
        or not executable_path.is_file()
        or not os.access(executable_path, os.X_OK)
        or marker.get("executable_sha256") != sha256_file(executable_path)
        or not isinstance(marker.get("executable_version_reported"), str)
        or not str(marker["executable_version_reported"]).startswith("ELMFIRE ")
    ):
        raise ValueError("receipt does not bind the current executable path/hash/version")
    validate_log(marker.get("stdout"), stdout_path)
    validate_log(marker.get("stderr"), stderr_path)
    stdout_text = stdout_path.read_text(encoding="utf-8", errors="replace")
    if "FIRE FRONT PROPAGATION STALLED" in stdout_text.upper():
        raise ValueError("captured stdout reports an early propagation stall")
    if marker.get("output_snapshot") != live_output_snapshot(root / "outputs"):
        raise ValueError("ELMFIRE output snapshot changed after successful exit")
    completed = dt.datetime.fromisoformat(str(marker.get("completed_utc")))
    if completed.tzinfo is None or completed > dt.datetime.now(dt.timezone.utc) + dt.timedelta(minutes=5):
        raise ValueError("completion timestamp is absent, naive, or in the future")
    return {
        "resolved_path": executable_path.as_posix(),
        "sha256": str(marker["executable_sha256"]),
        "version": str(marker["executable_version_reported"]),
    }


def exact_dump_ledger(output_dir: Path) -> tuple[Path, list[dict[str, object]], str]:
    paths = sorted(output_dir.glob("dump_times_*.csv"))
    name_match = re.fullmatch(r"dump_times_(\d{7})\.csv", paths[0].name) if len(paths) == 1 else None
    if len(paths) != 1 or name_match is None:
        raise ValueError(f"expected exactly one canonical dump-times CSV, found {len(paths)}")
    with paths[0].open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream, skipinitialspace=True)
        if reader.fieldnames != ["dump_index", "time_seconds", "is_final_dump"]:
            raise ValueError("dump-times CSV header is not exact")
        raw_rows = list(reader)
    if len(raw_rows) != FINAL_DUMP_INDEX:
        raise ValueError(f"dump ledger has {len(raw_rows)} rows instead of 61")
    rows: list[dict[str, object]] = []
    for expected_index, row in enumerate(raw_rows, start=1):
        index = int(str(row["dump_index"]).strip())
        time_s = float(str(row["time_seconds"]).strip())
        final_token = str(row["is_final_dump"]).strip().upper()
        required_final = "T" if expected_index == FINAL_DUMP_INDEX else "F"
        if index != expected_index or not math.isclose(time_s, expected_index - 1.0, rel_tol=0.0, abs_tol=1.0e-8):
            raise ValueError(f"dump row {expected_index} is not index={expected_index}, time={expected_index - 1} s")
        if final_token != required_final:
            raise ValueError(f"dump row {expected_index} has wrong final flag {final_token!r}")
        rows.append({"dump_index": index, "time_seconds": time_s, "is_final_dump": final_token == "T"})
    return paths[0], rows, name_match.group(1)


def exact_terminal_rasters(output_dir: Path, case_stamp: str) -> dict[str, Path]:
    flin_paths = sorted(output_dir.glob("flin_*.tif"))
    toa_paths = sorted(output_dir.glob("time_of_arrival_*.tif"))
    phi_paths = sorted(output_dir.glob("phi_*.tif"))
    if len(flin_paths) != 1 or flin_paths[0].name != f"flin_{case_stamp}_{FINAL_TIME_STAMP}.tif":
        raise ValueError("expected exactly one final-time FLIN raster stamped _0000060")
    if len(toa_paths) != 1 or toa_paths[0].name != f"time_of_arrival_{case_stamp}_{FINAL_TIME_STAMP}.tif":
        raise ValueError("expected exactly one final-time TOA raster stamped _0000060")
    phi_by_index: dict[int, Path] = {}
    for path in phi_paths:
        match = re.fullmatch(rf"phi_{case_stamp}_d(\d{{7}})\.tif", path.name)
        if not match:
            raise ValueError(f"noncanonical PHI filename {path.name}")
        index = int(match.group(1))
        if index in phi_by_index:
            raise ValueError(f"duplicate PHI dump index {index}")
        phi_by_index[index] = path
    if set(phi_by_index) != set(range(1, FINAL_DUMP_INDEX + 1)):
        raise ValueError("PHI rasters are not the exact d0000001..d0000061 every-step series")
    return {"flin": flin_paths[0], "toa": toa_paths[0], "phi": phi_by_index[FINAL_DUMP_INDEX]}


def read_aligned(path: Path, reference_path: Path, *, allow_mask: bool) -> np.ma.MaskedArray:
    with rasterio.open(reference_path) as reference, rasterio.open(path) as source:
        if (
            reference.count != 1
            or source.count != 1
            or reference.shape != EXPECTED_SHAPE
            or source.shape != reference.shape
            or reference.crs is None
            or reference.crs.to_epsg() != EXPECTED_EPSG
            or source.crs != reference.crs
            or not reference.transform.almost_equals(EXPECTED_TRANSFORM, precision=12)
            or not source.transform.almost_equals(reference.transform, precision=12)
            or not math.isclose(float(reference.nodata), EXPECTED_NODATA, abs_tol=0.0)
            or not math.isclose(float(source.nodata), EXPECTED_NODATA, abs_tol=0.0)
        ):
            raise ValueError(f"grid/count/CRS/transform/nodata mismatch for {path.name}")
        values = source.read(1, masked=True).astype(np.float64)
    if np.any(~np.isfinite(values.compressed())):
        raise ValueError(f"nonfinite non-nodata values in {path.name}")
    if not allow_mask and np.ma.getmaskarray(values).any():
        raise ValueError(f"unexpected nodata pixels in {path.name}")
    return values


def target_toa(values: np.ma.MaskedArray, row: int, col: int) -> float | None:
    value = values[row, col]
    if np.ma.is_masked(value):
        return None
    result = float(value)
    if not 0.0 <= result <= FINAL_TIME_S:
        raise ValueError(f"target TOA {result} lies outside 0..60 s")
    return result


def target_heat_peak(output_dir: Path, dump_rows: list[dict[str, object]], reference: Path, row: int, col: int) -> tuple[float, float]:
    peaks = {"dfc": 0.0, "rad": 0.0}
    for key, stem in (("dfc", "hf_dfc_transient"), ("rad", "hf_rad_transient")):
        for record in dump_rows:
            index = int(record["dump_index"])
            matches = sorted(output_dir.glob(f"{stem}_*_d{index:07d}.tif"))
            if len(matches) != 1:
                raise ValueError(f"expected one {stem} raster for dump {index}, found {len(matches)}")
            values = read_aligned(matches[0], reference, allow_mask=False)
            peaks[key] = max(peaks[key], float(values.data[row, col]))
    return peaks["dfc"], peaks["rad"]


def observe_variant(spec: dict[str, object], fingerprint: dict[str, object], oracle: dict[str, object]) -> tuple[dict[str, object], dict[str, str]]:
    variant_id = str(spec["id"])
    root = CASE_DIR / "variants" / variant_id
    identity = require_current_success(root, variant_id, fingerprint, oracle)
    output_dir = root / "outputs"
    dump_path, dump_rows, case_stamp = exact_dump_ledger(output_dir)
    rasters = exact_terminal_rasters(output_dir, case_stamp)
    mask_path = root / "inputs/source_mask.tif"
    with rasterio.open(mask_path) as source:
        if (
            source.count != 1
            or source.shape != EXPECTED_SHAPE
            or source.crs is None
            or source.crs.to_epsg() != EXPECTED_EPSG
            or not source.transform.almost_equals(EXPECTED_TRANSFORM, precision=12)
            or not math.isclose(float(source.nodata), EXPECTED_NODATA, abs_tol=0.0)
        ):
            raise ValueError("source_mask grid is not the generated 25x25 contract")
        source_mask = source.read(1, masked=True).filled(0) == 1
    if int(source_mask.sum()) != 7:
        raise ValueError("source_mask must contain exactly seven initial-source cells")
    flin = read_aligned(rasters["flin"], mask_path, allow_mask=True)
    toa = read_aligned(rasters["toa"], mask_path, allow_mask=True)
    phi = read_aligned(rasters["phi"], mask_path, allow_mask=False)
    flin_masked = np.ma.getmaskarray(flin)
    toa_masked = np.ma.getmaskarray(toa)
    if np.any(flin_masked[source_mask]) or np.any(toa_masked[source_mask]):
        raise ValueError("FLIN/TOA source evidence contains nodata")
    source_flin_values = np.asarray(flin.data[source_mask], dtype=float)
    source_toa_values = np.asarray(toa.data[source_mask], dtype=float)
    if np.any((source_toa_values < 0.0) | (source_toa_values > FINAL_TIME_S)):
        raise ValueError("source TOA lies outside 0..60 s")
    measured_flin = float(np.median(source_flin_values))
    source_toa = float(np.median(source_toa_values))
    source_toa_zero = bool(np.allclose(source_toa_values, 0.0, rtol=0.0, atol=1.0e-8))
    row, col = (int(value) for value in spec["target_row_col"])
    dfc_peak, rad_peak = target_heat_peak(output_dir, dump_rows, mask_path, row, col)
    urban_toa = target_toa(toa, row, col)
    urban_phi = float(phi.data[row, col])
    phi_burning = urban_phi <= 0.0
    toa_burning = urban_toa is not None
    state_consistent = phi_burning == toa_burning
    observed_ignition = phi_burning and toa_burning
    delay = None if urban_toa is None else urban_toa - source_toa
    return (
        {
            **spec,
            "measured_source_flin_kw_m": measured_flin,
            "source_flin_abs_error_kw_m": abs(measured_flin - float(spec["requested_source_flin_kw_m"])),
            "source_toa_s": source_toa,
            "source_toa_all_zero": source_toa_zero,
            "target_terminal_phi": urban_phi,
            "target_toa_s": urban_toa,
            "target_phi_burning": phi_burning,
            "target_toa_finite": toa_burning,
            "target_state_consistent": state_consistent,
            "target_ignited_observation": observed_ignition,
            "transition_ignition_delay_s": delay,
            "target_peak_dfc_kw_m2": dfc_peak,
            "target_peak_radiation_kw_m2": rad_peak,
            "target_peak_total_heat_kw_m2": dfc_peak + rad_peak,
            "dump_ledger": {
                "path": dump_path.relative_to(CASE_DIR).as_posix(),
                "row_count": len(dump_rows),
                "first": dump_rows[0],
                "last": dump_rows[-1],
            },
            "selected_evidence": {
                key: path.relative_to(CASE_DIR).as_posix() for key, path in rasters.items()
            },
            "completion_receipt": (root / "outputs/run_complete.json").relative_to(CASE_DIR).as_posix(),
            "stdout": (root / "logs/elmfire.stdout").relative_to(CASE_DIR).as_posix(),
        },
        identity,
    )


def bool_matrix(rows_by_id: dict[str, dict[str, object]], ids: list[str]) -> dict[str, bool]:
    return {variant_id: bool(rows_by_id[variant_id]["target_ignited_observation"]) for variant_id in ids}


def save_figure(rows: list[dict[str, object]], selector_passed: bool) -> None:
    """Keep primary observations separate from optional shortcut attribution."""
    labels = [str(r["id"]).replace("contiguous_", "C ").replace("isolated_", "I ").replace("_", " ") for r in rows]
    y = np.arange(len(rows))
    fig, axes = plt.subplots(3, 1, figsize=(7.2, 8.5), constrained_layout=True)
    fields = ("measured_source_flin_kw_m", "target_terminal_phi", "target_toa_s")
    titles = ("Exported initial-source intensity", "Terminal receiver state", "Receiver arrival time")
    units = ("FLIN (kW/m)", r"$\phi$ (–)", "Time (s); missing = no arrival")
    for ax, key, title, unit in zip(axes, fields, titles, units):
        values = [np.nan if r[key] is None else float(r[key]) for r in rows]
        ax.plot(values, y, "o")
        ax.set_yticks(y, labels)
        ax.invert_yaxis()
        ax.set(title=title, xlabel=unit)
        ax.grid(axis="x", alpha=0.25)
        for i, value in enumerate(values):
            if not np.isfinite(value):
                ax.annotate("no arrival", (0, i), xytext=(5, 0), textcoords="offset points", va="center")
    axes[0].axvline(1000, color="black", ls="--", label="Optional shortcut threshold")
    axes[0].legend(loc="lower right")
    axes[1].axvline(0, color="black", lw=1)
    polish_figure(fig)
    fig.savefig(CASE_DIR / "figures/transition_observations.pdf")
    plt.close(fig)



def main() -> None:
    figure_path = CASE_DIR / "figures/transition_observations.pdf"
    figure_path.unlink(missing_ok=True)
    (CASE_DIR / "report/case_report.pdf").unlink(missing_ok=True)
    (CASE_DIR / "report/metrics_macros.tex").unlink(missing_ok=True)
    write_json_atomic(
        METRICS_PATH,
        base_payload("Postprocessing started; no verdict has been accepted yet.", "INCOMPLETE"),
    )
    try:
        preflight, selector_passed = read_preflight()
        expected, specs = load_expected()
        fingerprints = load_fingerprints()
        if expected.get("runtime_input_fingerprint_sha256") != fingerprints.get("aggregate_sha256"):
            raise ValueError("expected.json and run_fingerprints.json disagree")
    except (OSError, ValueError, KeyError, TypeError, json.JSONDecodeError) as exc:
        payload = base_payload(f"case-local specification/preflight is invalid: {exc}", "INCOMPLETE")
        write_json_atomic(METRICS_PATH, payload)
        print("[NOT EVALUABLE] CASE46_WUT: invalid case-local specification")
        return

    marker_count = sum(
        (CASE_DIR / "variants" / str(spec["id"]) / "outputs/run_complete.json").is_file()
        for spec in specs
    )
    if marker_count == 0:
        payload = base_payload(
            "No current successful-run receipts are present; ELMFIRE outputs were not evaluated.",
            "NOT RUN",
        )
        payload["runtime_input_fingerprint_sha256"] = fingerprints["aggregate_sha256"]
        payload["oracle_artifact_fingerprint_sha256"] = fingerprints["oracle_artifacts"]["sha256"]
        payload["selector_preflight_status"] = preflight["status"]
        write_json_atomic(METRICS_PATH, payload)
        print("[NOT EVALUABLE] CASE46_WUT: workflow has not run")
        return

    rows: list[dict[str, object]] = []
    identities: list[dict[str, str]] = []
    errors: dict[str, str] = {}
    for spec in specs:
        variant_id = str(spec["id"])
        try:
            fingerprint = fingerprints["variants"].get(variant_id)
            if not isinstance(fingerprint, dict) or spec.get("runtime_input_fingerprint_sha256") != fingerprint.get("sha256"):
                raise ValueError("variant fingerprint metadata is missing or inconsistent")
            row, identity = observe_variant(spec, fingerprint, fingerprints["oracle_artifacts"])
            rows.append(row)
            identities.append(identity)
        except (OSError, ValueError, KeyError, TypeError, csv.Error, rasterio.errors.RasterioError) as exc:
            errors[variant_id] = str(exc)

    unique_identities = {
        (identity["resolved_path"], identity["sha256"], identity["version"])
        for identity in identities
    }
    if len(unique_identities) > 1:
        errors["executable_identity"] = "all seven variants must use one executable path/hash/version"
    if errors or len(rows) != len(specs):
        payload = base_payload(
            "Required receipts, logs, exact dump ledger, or terminal rasters are incomplete/ambiguous: "
            + json.dumps(errors, sort_keys=True),
            "INCOMPLETE",
            len(rows),
        )
        payload["runtime_input_fingerprint_sha256"] = fingerprints["aggregate_sha256"]
        payload["oracle_artifact_fingerprint_sha256"] = fingerprints["oracle_artifacts"]["sha256"]
        payload["selector_preflight_status"] = preflight["status"]
        payload["variant_errors"] = errors
        write_json_atomic(METRICS_PATH, payload)
        print("[NOT EVALUABLE] CASE46_WUT: incomplete or stale evidence")
        return

    rows_by_id = {str(row["id"]): row for row in rows}
    executable = identities[0]
    max_setup_error = max(float(row["source_flin_abs_error_kw_m"]) for row in rows)
    source_toa_all_zero = all(bool(row["source_toa_all_zero"]) for row in rows)
    setup_pass = max_setup_error <= 0.5 and source_toa_all_zero
    state_pass = all(bool(row["target_state_consistent"]) for row in rows)
    contiguous_ids = ["contiguous_below", "contiguous_equal", "contiguous_above"]
    isolated_ids = ["isolated_d1_insufficient", "isolated_d1_sufficient", "isolated_d2_sufficient"]
    contiguous_observed = bool_matrix(rows_by_id, contiguous_ids)
    isolated_observed = bool_matrix(rows_by_id, isolated_ids)
    contiguous_expected = {"contiguous_below": False, "contiguous_equal": False, "contiguous_above": True}
    isolated_expected = {
        "isolated_d1_insufficient": False,
        "isolated_d1_sufficient": True,
        "isolated_d2_sufficient": False,
    }
    contiguous_pass = contiguous_observed == contiguous_expected
    isolated_pass = isolated_observed == isolated_expected
    eligible_ids = ["contiguous_above", "isolated_d1_sufficient"]
    timing_observed = {
        variant_id: {
            "target_toa_s": rows_by_id[variant_id]["target_toa_s"],
            "target_source_delay_s": rows_by_id[variant_id]["transition_ignition_delay_s"],
        }
        for variant_id in eligible_ids
    }
    timing_pass = all(
        row["target_toa_s"] is not None
        and math.isclose(float(row["target_toa_s"]), 0.0, rel_tol=0.0, abs_tol=1.0e-8)
        and math.isclose(float(row["transition_ignition_delay_s"]), 0.0, rel_tol=0.0, abs_tol=1.0e-8)
        for row in (rows_by_id[variant_id] for variant_id in eligible_ids)
    )
    zero = rows_by_id["zero_control"]
    zero_pass = (
        zero["target_toa_s"] is None
        and float(zero["target_terminal_phi"]) > 0.0
        and not bool(zero["target_ignited_observation"])
    )
    sufficient_ids = ["contiguous_above", "isolated_d1_sufficient"]
    heat_observed = {variant_id: float(rows_by_id[variant_id]["target_peak_total_heat_kw_m2"]) for variant_id in sufficient_ids}
    intended_heat_pass = all(math.isfinite(value) and value > 0.0 for value in heat_observed.values())
    intended_delay_observed = {variant_id: rows_by_id[variant_id]["transition_ignition_delay_s"] for variant_id in sufficient_ids}
    intended_delay_pass = all(value is not None and float(value) > 0.0 for value in intended_delay_observed.values())
    conditional_status = lambda passed: ("PASS" if passed else "FAIL") if selector_passed else "NOT EVALUABLE"
    metrics = [
        metric(METRIC_NAMES[0], "61 rows: indices 1..61, times 0..60 s, only row 61 final", "7/7 exact; FLIN/TOA time stamp 0000060; PHI dump index d0000061", "--", "PASS"),
        metric(METRIC_NAMES[1], "maximum absolute error <=0.5 kW/m and source TOA=0 s", {"max_abs_error_kw_m": max_setup_error, "all_source_toa_zero": source_toa_all_zero}, "kW/m", "PASS" if setup_pass else "FAIL"),
        metric(METRIC_NAMES[2], "PHI<=0 iff TOA is finite at each designated target", {variant_id: bool(row["target_state_consistent"]) for variant_id, row in rows_by_id.items()}, "--", "PASS" if state_pass else "FAIL"),
        metric(METRIC_NAMES[3], "positive DFC+radiation at sufficient-heat receivers", heat_observed, "kW/m2", "PASS" if intended_heat_pass else "FAIL"),
        metric(METRIC_NAMES[4], "finite target TOA with delay > 0 s", intended_delay_observed, "s", "PASS" if intended_delay_pass else "FAIL"),
        metric(METRIC_NAMES[5], "below=false, equal=false, above=true", contiguous_observed, "--", conditional_status(contiguous_pass)),
        metric(METRIC_NAMES[6], "d1-insufficient=false, d1-sufficient=true, d2-sufficient=false", isolated_observed, "--", conditional_status(isolated_pass)),
        metric(METRIC_NAMES[7], "eligible target TOA=0 s and target-source delay=0 s", timing_observed, "s", conditional_status(timing_pass)),
        metric(METRIC_NAMES[8], "target remains PHI>0 with nodata TOA", {"target_phi": zero["target_terminal_phi"], "target_toa_s": zero["target_toa_s"]}, "--", "PASS" if zero_pass else "FAIL"),
    ]
    primary_indices = [0, 1, 2, 3, 4, 8]
    verification_passed = all(metrics[index]["status"] == "PASS" for index in primary_indices)
    overall_status = "PASS" if verification_passed else "FAIL"
    reason = (
        "All intended heat/FTP transition, state, control, dump, and provenance checks passed."
        if verification_passed
        else "One or more intended heat/FTP transition checks failed; optional FLIN-shortcut diagnostics do not control this decision."
    )
    payload = {
        "schema_version": 2,
        "case_id": CASE_ID,
        "source_commit": SOURCE_COMMIT,
        "overall_status": overall_status,
        "workflow_status": "COMPLETE",
        "verification_passed": verification_passed,
        "required_outputs_complete": True,
        "required_variant_count": len(specs),
        "completed_variant_count": len(rows),
        "runtime_input_fingerprint_sha256": fingerprints["aggregate_sha256"],
        "oracle_artifact_fingerprint_sha256": fingerprints["oracle_artifacts"]["sha256"],
        "preflight": "outputs/source_selector_preflight.json",
        "selector_preflight_status": preflight["status"],
        "selector_gate_passed": selector_passed,
        "reason": reason,
        "executable": executable,
        "metrics": metrics,
        "variants": rows,
        "evidence_paths": {
            "expected_matrix": "variants/expected.json",
            "oracle_and_runtime_fingerprints": "variants/run_fingerprints.json",
            "selector_preflight": "outputs/source_selector_preflight.json",
            "variant_metadata_pattern": "variants/<variant_id>/variant.json",
            "completion_receipt_pattern": "variants/<variant_id>/outputs/run_complete.json",
            "stdout_pattern": "variants/<variant_id>/logs/elmfire.stdout",
            "terminal_flin_pattern": "variants/<variant_id>/outputs/flin_*_0000060.tif",
            "terminal_toa_pattern": "variants/<variant_id>/outputs/time_of_arrival_*_0000060.tif",
            "terminal_phi_pattern": "variants/<variant_id>/outputs/phi_*_d0000061.tif",
        },
    }
    write_json_atomic(METRICS_PATH, payload)
    save_figure(rows, selector_passed)
    print(f"[{overall_status}] CASE46_WUT: workflow complete")


if __name__ == "__main__":
    main()
