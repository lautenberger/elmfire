#!/usr/bin/env python3
"""Fail-closed evaluation of CASE47's implementation characterization."""
from __future__ import annotations

from report_language import polish_figure

import csv
import hashlib
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

CASE_DIR = Path(__file__).resolve().parents[1]
CASE_ID = "CASE47_UWT"
SOURCE_COMMIT = "a2dfbcdf72209733c000e5d3431e44723e15efea"
ATTEMPT_LEDGER = CASE_DIR / "logs/run_attempts.json"
PEAKS = (100, 400)
ADJUSTMENTS = (0, 1)
CORRIDORS = ("open", "barrier")
HEAT_SOURCE = (8, 10)
PATH_SOURCE = (22, 10)
NEAR = (22, 11)
BARRIER = (22, 12)
DISTAL = (22, 13)
ISOLATED = (8, 13)
PDF_METADATA = {
    "Author": "ELMFIRE Verification and Validation Suite",
    "CreationDate": datetime(2020, 1, 1, tzinfo=timezone.utc),
    "ModDate": datetime(2020, 1, 1, tzinfo=timezone.utc),
}


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def parse_scalar(config: Path, key: str) -> float:
    text = config.read_text(encoding="utf-8")
    matches = re.findall(rf"(?mi)^\s*{re.escape(key)}\s*=\s*([0-9.eEdD+-]+)", text)
    if len(matches) != 1:
        raise ValueError(f"{key} is not unique in {config}")
    value = float(matches[0].replace("d", "e").replace("D", "E"))
    if not math.isfinite(value):
        raise ValueError(f"{key} is not finite")
    return value


def one(paths: list[Path], label: str) -> Path:
    if len(paths) != 1:
        raise ValueError(f"expected exactly one {label}; found {len(paths)}")
    return paths[0]


def validate_grid(path: Path, reference: Path) -> None:
    with rasterio.open(path) as candidate, rasterio.open(reference) as expected:
        if (
            candidate.shape != expected.shape
            or candidate.count != 1
            or candidate.transform != expected.transform
            or candidate.crs != expected.crs
            or candidate.nodata != expected.nodata
        ):
            raise ValueError(f"grid mismatch: {path}")


def array(path: Path) -> np.ndarray:
    with rasterio.open(path) as source:
        return source.read(1, masked=True).astype(float).filled(np.nan)


def cell_value(values: np.ndarray, cell: tuple[int, int]) -> float | None:
    value = float(values[cell])
    return value if math.isfinite(value) else None


def check_input_manifest(root: Path) -> dict[str, object]:
    path = root / "input_manifest.json"
    payload = json.loads(path.read_text(encoding="utf-8"))
    if payload.get("source_commit") != SOURCE_COMMIT:
        raise ValueError("source provenance mismatch")
    hashes = payload.get("artifact_sha256")
    if not isinstance(hashes, dict) or not hashes:
        raise ValueError("input artifact hashes are absent")
    for relative, expected in hashes.items():
        artifact = root / str(relative)
        if not artifact.is_file() or sha256(artifact) != expected:
            raise ValueError(f"prepared artifact changed: {artifact}")
    expected_fingerprint = hashlib.sha256(
        json.dumps(hashes, sort_keys=True).encode("utf-8")
    ).hexdigest()
    if payload.get("input_fingerprint") != expected_fingerprint:
        raise ValueError("prepared-input fingerprint is malformed or stale")
    marker = root / "logs/completed_input_fingerprint.txt"
    if not marker.is_file():
        raise ValueError("successful-run fingerprint marker is absent")
    if marker.read_text(encoding="utf-8").strip() != expected_fingerprint:
        raise ValueError("successful-run fingerprint marker is stale")
    receipt_path = root / "logs/completion_marker.json"
    receipt = json.loads(receipt_path.read_text(encoding="utf-8"))
    required_receipt = {
        "case_id": CASE_ID,
        "variant_id": str(payload["id"]),
        "status": "ELMFIRE_EXIT_0",
        "input_fingerprint": expected_fingerprint,
        "oracle_source_revision": SOURCE_COMMIT,
    }
    for key, expected in required_receipt.items():
        if receipt.get(key) != expected:
            raise ValueError(f"completion receipt {key} does not match the run contract")
    executable_digest = str(receipt.get("executable_sha256", ""))
    if (
        not receipt.get("completed_utc")
        or not receipt.get("executable_requested")
        or not receipt.get("executable_resolved")
        or len(executable_digest) != 64
        or any(character not in "0123456789abcdef" for character in executable_digest.lower())
    ):
        raise ValueError("completion receipt does not identify the executed binary")
    executable = Path(str(receipt["executable_resolved"]))
    if (not executable.is_absolute() or not executable.is_file()
            or sha256(executable) != executable_digest):
        raise ValueError("executed binary is absent or no longer matches its recorded SHA-256")
    stdout_relative = receipt.get("stdout_path")
    stdout = CASE_DIR / str(stdout_relative)
    if (stdout_relative != f"logs/{payload['id']}.stdout" or not stdout.is_file()
            or receipt.get("stdout_sha256") != sha256(stdout)):
        raise ValueError("ELMFIRE stdout is absent or differs from the completion receipt")
    stderr_relative = receipt.get("stderr_path")
    stderr = CASE_DIR / str(stderr_relative)
    if (stderr_relative != f"logs/{payload['id']}.stderr" or not stderr.is_file()
            or receipt.get("stderr_sha256") != sha256(stderr)):
        raise ValueError("ELMFIRE stderr is absent or differs from the completion receipt")
    current_outputs = {
        str(path.relative_to(root)): sha256(path)
        for path in sorted((root / "outputs").rglob("*"))
        if path.is_file()
    }
    recorded_outputs = receipt.get("output_artifact_sha256")
    if not isinstance(recorded_outputs, dict) or not recorded_outputs:
        raise ValueError("completion receipt does not bind the output artifact snapshot")
    if recorded_outputs != current_outputs:
        raise ValueError("output artifacts differ from the successful-run receipt")
    payload["completion_receipt"] = str(receipt_path.relative_to(CASE_DIR))
    payload["executable_sha256"] = executable_digest
    return payload


def select_outputs(root: Path) -> dict[str, object]:
    """Require the explicit exact-stop final dump and every output used by the case."""
    config = root / "elmfire.data"
    tstop = parse_scalar(config, "SIMULATION_TSTOP")
    dt = parse_scalar(config, "SIMULATION_DT")
    dtmax = parse_scalar(config, "SIMULATION_DTMAX")
    feedback = parse_scalar(config, "FEEDBACK_LEVEL")
    if (
        not math.isclose(tstop, 600.0, rel_tol=0.0, abs_tol=1.0e-12)
        or not math.isclose(dt, 1.0, rel_tol=0.0, abs_tol=1.0e-12)
        or not math.isclose(dtmax, dt, rel_tol=0.0, abs_tol=1.0e-12)
        or not math.isclose(feedback, 1.0, rel_tol=0.0, abs_tol=1.0e-12)
        or not math.isclose(tstop / dt, round(tstop / dt), abs_tol=1.0e-9)
    ):
        raise ValueError("configured timing/feedback differs from the 600-step case contract")
    output = root / "outputs"
    dump_manifest = one(sorted(output.glob("dump_times_*.csv")), "dump-times manifest")
    with dump_manifest.open(newline="", encoding="utf-8") as stream:
        rows = list(csv.DictReader(stream))
    final_rows = [
        row
        for row in rows
        if str(row.get("is_final_dump", "")).strip().upper() in {"T", "TRUE", "1", "Y", "YES"}
    ]
    if len(final_rows) != 1:
        raise ValueError("dump-times manifest does not contain one final record")
    final_time = float(final_rows[0]["time_seconds"])
    if not math.isfinite(final_time) or not math.isclose(final_time, tstop, rel_tol=0.0, abs_tol=1.0e-3):
        raise ValueError(f"explicit final dump is not at TSTOP ({final_time} versus {tstop})")
    stamp = int(math.floor(final_time + 0.5))
    dump_indices = [int(row["dump_index"]) for row in rows]
    if dump_indices != list(range(1, len(rows) + 1)):
        raise ValueError("dump indices are duplicated, missing, or out of order")
    dump_times = [float(row["time_seconds"]) for row in rows]
    expected_times = [60.0 * index for index in dump_indices]
    if len(dump_times) != 10 or any(
        not math.isclose(actual, expected, rel_tol=0.0, abs_tol=1.0e-3)
        for actual, expected in zip(dump_times, expected_times)
    ):
        raise ValueError("dump records are not on the exact 60 s schedule")
    suffix = f"_{stamp:07d}.tif"
    paths = {
        "toa": one(sorted(output.glob(f"time_of_arrival_*{suffix}")), "terminal TOA raster"),
        "vs": one(sorted(output.glob(f"vs_*{suffix}")), "terminal spread-rate raster"),
        "flin": one(sorted(output.glob(f"flin_*{suffix}")), "terminal fireline-intensity raster"),
        "total_dfc": one(sorted(output.glob(f"total_dfc_received_*{suffix}")), "terminal total-DFC raster"),
        "total_rad": one(sorted(output.glob(f"total_rad_received_*{suffix}")), "terminal total-radiation raster"),
    }
    transient = {
        "hrr": sorted(output.glob("hrr_transient_*.tif")),
        "dfc": sorted(output.glob("hf_dfc_transient_*.tif")),
        "rad": sorted(output.glob("hf_rad_transient_*.tif")),
    }
    for name, items in transient.items():
        parsed_indices = []
        for path in items:
            match = re.search(r"_d([0-9]{7})\.tif$", path.name)
            if match is None:
                raise ValueError(f"malformed {name} transient dump name: {path.name}")
            parsed_indices.append(int(match.group(1)))
        if parsed_indices != dump_indices:
            raise ValueError(f"{name} transient dumps do not match the dump manifest")
    reference = root / "inputs/fbfm40.tif"
    for path in [*paths.values(), *(p for values in transient.values() for p in values)]:
        validate_grid(path, reference)
    log_path = CASE_DIR / "logs" / f"{root.name}.stdout"
    log_text = log_path.read_text(encoding="utf-8", errors="replace")
    logged_times = [
        float(value)
        for value in re.findall(r"Current Timestep:\s*([0-9.+\-Ee]+)\s+of", log_text)
    ]
    expected_steps = int(round(tstop / dt))
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
            "logged solver-step timestamps differ from 0..TSTOP-DT by "
            f"{maximum_step_deviation:g} s"
        )
    return {
        "final_time_s": final_time,
        "terminal_selection": "explicit post-step exact-stop final dump",
        "logged_solver_step_count": len(logged_times),
        "maximum_logged_step_deviation_s": maximum_step_deviation,
        "dump_manifest": str(dump_manifest.relative_to(CASE_DIR)),
        "paths": paths,
        "transient": transient,
    }


def observe(root: Path, input_record: dict[str, object], selected: dict[str, object]) -> dict[str, object]:
    paths = selected["paths"]
    transient = selected["transient"]
    toa = array(paths["toa"])
    vs = array(paths["vs"])
    flin = array(paths["flin"])
    fbfm = array(root / "inputs/fbfm40.tif")
    total_dfc = array(paths["total_dfc"])
    total_rad = array(paths["total_rad"])
    heat = total_dfc + total_rad
    finite_toa = toa[np.isfinite(toa)]
    if np.any(finite_toa < 0.0) or np.any(finite_toa > float(selected["final_time_s"]) + 1.0e-3):
        raise ValueError("terminal TOA contains a finite value outside [0,TSTOP]")
    if (not np.all(np.isfinite(total_dfc)) or not np.all(np.isfinite(total_rad))
            or np.any(total_dfc < -1.0e-6) or np.any(total_rad < -1.0e-6)):
        raise ValueError("terminal received-heat fields contain nodata, nonfinite, or negative values")
    source_flin = [cell_value(flin, HEAT_SOURCE), cell_value(flin, PATH_SOURCE)]
    if any(value is None or value < 0.0 for value in source_flin):
        raise ValueError("terminal FLIN is unavailable or negative at a prepared source cell")
    hrr_peaks: list[float] = []
    transient_arrays: dict[str, list[np.ndarray]] = {name: [] for name in transient}
    for name, items in transient.items():
        for path in items:
            values = array(path)
            if not np.all(np.isfinite(values)):
                raise ValueError(f"{name} transient output contains nodata/nonfinite cells")
            if np.any(values < -1.0e-6):
                raise ValueError(f"{name} transient output contains a negative physical value")
            transient_arrays[name].append(values)
    for values in transient_arrays["hrr"]:
        samples = [cell_value(values, HEAT_SOURCE), cell_value(values, PATH_SOURCE)]
        hrr_peaks.extend(value for value in samples if value is not None)
    if not hrr_peaks:
        raise ValueError("source HRR transient values are unavailable")
    final_transient_maximum = max(
        float(np.max(np.abs(transient_arrays[name][-1])))
        for name in ("hrr", "dfc", "rad")
    )
    if final_transient_maximum > 1.0e-6:
        raise ValueError("the exact-stop transient fields are not reset-zero")
    wildland_values = vs[(fbfm == 1) & np.isfinite(vs)]
    max_wildland_vs = float(np.max(wildland_values)) if wildland_values.size else None
    record = {
        "id": input_record["id"],
        "source_hrrpua_peak_kw_m2": input_record["source_hrrpua_peak_kw_m2"],
        "wildland_adjustment": input_record["wildland_adjustment"],
        "corridor": input_record["corridor"],
        "terminal_time_s": selected["final_time_s"],
        "terminal_selection": selected["terminal_selection"],
        "source_hrrpua_observed_peak_kw_m2": max(hrr_peaks),
        "isolated_total_dfc_kj": cell_value(total_dfc, ISOLATED),
        "isolated_total_rad_kj": cell_value(total_rad, ISOLATED),
        "isolated_total_heat_kj": cell_value(heat, ISOLATED),
        "isolated_toa_s": cell_value(toa, ISOLATED),
        "adjacent_toa_s": cell_value(toa, NEAR),
        "distal_toa_s": cell_value(toa, DISTAL),
        "barrier_toa_s": cell_value(toa, BARRIER),
        "maximum_wildland_spread_rate_m_min": max_wildland_vs,
        "terminal_source_flin_kw_m": source_flin,
        "exact_stop_transient_maximum": final_transient_maximum,
        "logged_solver_step_count": selected["logged_solver_step_count"],
        "maximum_logged_step_deviation_s": selected["maximum_logged_step_deviation_s"],
        "completion_receipt": input_record["completion_receipt"],
        "executable_sha256": input_record["executable_sha256"],
        "selected_files": [
            str(path.relative_to(CASE_DIR))
            for path in [*paths.values(), *transient["hrr"], *transient["dfc"], *transient["rad"]]
        ],
    }
    if record["isolated_total_heat_kj"] is None:
        raise ValueError("isolated receiver total heat is unavailable")
    return record


def rel_error(value: float, expected: float) -> float:
    return abs(value - expected) / max(abs(expected), 1.0e-12)


def ratio_error(numerator: float, denominator: float, expected: float) -> float:
    """Return a large finite failure value when a required denominator vanishes."""
    if not math.isfinite(numerator) or not math.isfinite(denominator):
        return 1.0e30
    if abs(denominator) <= 1.0e-12:
        return 1.0e30
    return rel_error(numerator / denominator, expected)


def metric(name: str, expected: str, calculated: object, units: str, passed: bool, rationale: str) -> dict[str, object]:
    return {
        "name": name,
        "expected": expected,
        "calculated": calculated,
        "units": units,
        "status": "PASS" if passed else "FAIL",
        "layer": "current implementation characterization",
        "rationale": rationale,
    }


def assess(rows: list[dict[str, object]]) -> list[dict[str, object]]:
    by_id = {str(row["id"]): row for row in rows}
    ratio_errors = []
    heat_ratio_errors = []
    source_repeat_errors = []
    for adjustment in (0, 1):
        for corridor in ("open", "barrier"):
            low = by_id[f"hrr100_adj{adjustment}_{corridor}"]
            high = by_id[f"hrr400_adj{adjustment}_{corridor}"]
            ratio_errors.append(ratio_error(
                float(high["source_hrrpua_observed_peak_kw_m2"]),
                float(low["source_hrrpua_observed_peak_kw_m2"]),
                4.0,
            ))
            heat_ratio_errors.append(ratio_error(
                float(high["isolated_total_heat_kj"]),
                float(low["isolated_total_heat_kj"]),
                4.0,
            ))
    for peak in (100, 400):
        values = [
            float(row["source_hrrpua_observed_peak_kw_m2"])
            for row in rows
            if int(float(row["source_hrrpua_peak_kw_m2"])) == peak
        ]
        source_repeat_errors.append((max(values) - min(values)) / max(max(values), 1.0e-12))

    heat_values = [float(row["isolated_total_heat_kj"]) for row in rows]
    isolated_arrivals = sum(row["isolated_toa_s"] is not None for row in rows)
    barrier_arrivals = sum(
        row["barrier_toa_s"] is not None for row in rows if row["corridor"] == "barrier"
    )
    adj0 = [row for row in rows if row["wildland_adjustment"] == 0.0]
    adj1_open = [
        row for row in rows
        if row["wildland_adjustment"] == 1.0 and row["corridor"] == "open"
    ]
    adj1_open_rates = [
        float(row["maximum_wildland_spread_rate_m_min"])
        for row in adj1_open
        if row["maximum_wildland_spread_rate_m_min"] is not None
    ]
    adj0_path_arrivals = sum(
        row[name] is not None
        for row in adj0
        for name in ("adjacent_toa_s", "distal_toa_s")
    )
    adj1_open_path_arrivals = sum(
        row[name] is not None
        for row in adj1_open
        for name in ("adjacent_toa_s", "distal_toa_s")
    )
    minimum_adj1_open = min(adj1_open_rates, default=math.nan)
    positive_adj1_rate = bool(
        len(adj1_open_rates) == len(adj1_open) == 2
        and math.isfinite(minimum_adj1_open)
        and minimum_adj1_open > 1.0e-4
    )
    arrival_repeat_difference = math.nan
    if len(adj1_open) == 2 and all(
        row[name] is not None
        for row in adj1_open
        for name in ("adjacent_toa_s", "distal_toa_s")
    ):
        by_peak = {
            int(float(row["source_hrrpua_peak_kw_m2"])): row
            for row in adj1_open
        }
        if set(by_peak) == {100, 400}:
            arrival_repeat_difference = max(
                abs(float(by_peak[100][name]) - float(by_peak[400][name]))
                for name in ("adjacent_toa_s", "distal_toa_s")
            )
    return [
        metric(
            "source HRR fourfold response error",
            "<= 0.01",
            max(ratio_errors),
            "fraction",
            max(ratio_errors) <= 0.01,
            "The high table must multiply the urban design-fire source by four.",
        ),
        metric(
            "source HRR control-repeat error",
            "<= 0.001",
            max(source_repeat_errors),
            "fraction",
            max(source_repeat_errors) <= 0.001,
            "ADJ and the nonburnable corridor control must not alter the urban source curve.",
        ),
        metric(
            "isolated receiver heat-exposure minimum",
            "> 0",
            min(heat_values),
            "kJ",
            min(heat_values) > 0.0,
            "The disconnected receiver must be inside the WU-E source heat footprint.",
        ),
        metric(
            "isolated heat fourfold response error",
            "<= 0.02",
            max(heat_ratio_errors),
            "fraction",
            max(heat_ratio_errors) <= 0.02,
            "Integrated DFC plus radiation should retain linear HRR amplitude scaling.",
        ),
        metric(
            "isolated receiver arrivals",
            "0 of 8",
            isolated_arrivals,
            "count",
            isolated_arrivals == 0,
            "At the pinned revision, accumulated heat is not mapped to ignition for a wildland target.",
        ),
        metric(
            "nonburnable barrier arrivals",
            "0 of 4",
            barrier_arrivals,
            "count",
            barrier_arrivals == 0,
            "FBFM99 must remain a blocking, nonburnable control.",
        ),
        metric(
            "ADJ0 ordinary-path arrivals",
            "0 of 8 adjacent/distal checks",
            adj0_path_arrivals,
            "count",
            adj0_path_arrivals == 0,
            "Zero ADJ must prevent ordinary propagation without treating a missing spread-rate value as numerical zero.",
        ),
        metric(
            "ADJ1 open-path arrivals",
            "4 of 4 adjacent/distal checks",
            adj1_open_path_arrivals,
            "count",
            adj1_open_path_arrivals == 4,
            "Both open-path variants must reach both named receiver cells.",
        ),
        metric(
            "minimum ADJ1-open wildland spread rate",
            "> 1e-4 in both open variants",
            minimum_adj1_open if math.isfinite(minimum_adj1_open) else None,
            "m/min",
            positive_adj1_rate,
            "A finite positive raster value corroborates the TOA-based ordinary-spread control.",
        ),
        metric(
            "open-path TOA HRR-control difference",
            "<= 1",
            arrival_repeat_difference if math.isfinite(arrival_repeat_difference) else None,
            "s",
            math.isfinite(arrival_repeat_difference) and arrival_repeat_difference <= 1.0,
            "Ordinary path ignition time must be insensitive to the disconnected HRR-amplitude control.",
        ),
    ]


def plot_results(rows: list[dict[str, object]]) -> None:
    ordered = sorted(rows, key=lambda row: str(row["id"]))
    labels = [str(row["id"]).replace("hrr", "H").replace("_", "\n") for row in ordered]
    x = np.arange(len(rows))
    peaks = [float(row["source_hrrpua_observed_peak_kw_m2"]) for row in ordered]
    heat = [float(row["isolated_total_heat_kj"]) for row in ordered]
    adjacent_toa = [
        float(row["adjacent_toa_s"])
        if row["adjacent_toa_s"] is not None else np.nan
        for row in ordered
    ]
    distal_toa = [
        float(row["distal_toa_s"])
        if row["distal_toa_s"] is not None else np.nan
        for row in ordered
    ]
    series = ((peaks, "Source peak HRRPUA (kW/m²)"),
              (heat, "Isolated received heat (kJ)"),
              (adjacent_toa, "Adjacent receiver arrival (s)"),
              (distal_toa, "Distal receiver arrival (s)"))
    readable_labels = [str(r["id"]).replace("_", " ") for r in ordered]
    for page, filename in enumerate(("implementation_characterization", "arrival_controls")):
        fig, axes = plt.subplots(2, 1, figsize=(7.2, 7.0), constrained_layout=True)
        for ax, (values, title) in zip(axes, series[2*page:2*page+2]):
            ax.plot(values, x, "o")
            ax.set_yticks(x, readable_labels)
            ax.invert_yaxis()
            ax.set_xlabel(title)
            ax.grid(axis="x", alpha=0.25)
            for i, value in enumerate(values):
                if not np.isfinite(value):
                    ax.annotate("no arrival", (0, i), xytext=(5, 0), textcoords="offset points", va="center")
        polish_figure(fig)
        fig.savefig(CASE_DIR / f"figures/{filename}.pdf", metadata=PDF_METADATA)
        plt.close(fig)

    representative = CASE_DIR / "variants/hrr400_adj1_open"
    selected = select_outputs(representative)
    toa = array(selected["paths"]["toa"])
    with rasterio.open(representative / "inputs/fbfm40.tif") as source:
        transform = source.transform
        fbfm = source.read(1)
    extent = (
        transform.c,
        transform.c + transform.a * toa.shape[1],
        transform.f + transform.e * toa.shape[0],
        transform.f,
    )
    fig, ax = plt.subplots(figsize=(7.2, 5.8), constrained_layout=True)
    image = ax.imshow(toa, extent=extent, origin="upper", cmap="inferno")
    ax.contour(
        np.linspace(5, 305, fbfm.shape[1]),
        np.linspace(305, 5, fbfm.shape[0]),
        fbfm == 99,
        levels=[0.5],
        colors="cyan",
        linewidths=0.6,
    )
    fig.colorbar(image, ax=ax, label="time of arrival (s)")
    ax.set_title("hrr400_adj1_open: arrival at t = 600 s\nCyan: nonburnable boundary")
    ax.set_xlabel("Easting (m)")
    ax.set_ylabel("Northing (m)")
    ax.set_aspect("equal")
    polish_figure(fig)
    fig.savefig(
        CASE_DIR / "figures/whole_domain_result.pdf",
        metadata={**PDF_METADATA, "Title": "CASE47_UWT whole-domain output"},
    )
    plt.close(fig)


def incomplete(required: int, completed: int, missing: dict[str, str]) -> dict[str, object]:
    attempt_recorded = ATTEMPT_LEDGER.exists()
    not_run = not attempt_recorded and bool(missing) and completed == 0 and all(
        "successful-run fingerprint marker is absent" in reason
        for reason in missing.values()
    )
    payload = {
        "case_id": CASE_ID,
        "overall_status": "NOT EVALUABLE",
        "workflow_status": "NOT RUN" if not_run else "INCOMPLETE",
        "verification_passed": False,
        "required_outputs_complete": False,
        "required_variant_count": required,
        "completed_variant_count": completed,
        "implementation_characterization_status": "NOT EVALUABLE",
        "intended_capability_status": "NOT EVALUABLE",
        "metrics": [],
        "missing_or_invalid": missing,
        "reason": (
            "ELMFIRE has not been run for the current prepared-input fingerprints."
            if not_run
            else (
                "ELMFIRE execution was attempted, but required successful-run evidence is incomplete."
                if attempt_recorded and completed == 0
                else "Required terminal or transient evidence is missing, malformed, stale, or ambiguous."
            )
        ),
        "source_commit": SOURCE_COMMIT,
    }
    if attempt_recorded:
        payload["attempt_ledger"] = str(ATTEMPT_LEDGER.relative_to(CASE_DIR))
    return payload


def main() -> None:
    for stale in (
        CASE_DIR / "figures/implementation_characterization.pdf",
        CASE_DIR / "figures/whole_domain_result.pdf",
    ):
        stale.unlink(missing_ok=True)
    try:
        gate = json.loads((CASE_DIR / "data/misc/source_capability.json").read_text(encoding="utf-8"))
        if gate.get("source_commit") != SOURCE_COMMIT or gate.get("supported") is not False:
            raise ValueError("embedded capability gate does not describe the pinned unavailable path")
    except (OSError, ValueError, json.JSONDecodeError) as error:
        payload = incomplete(8, 0, {"capability_gate": str(error)})
        (CASE_DIR / "outputs/metrics.json").write_text(json.dumps(payload, indent=2) + "\n")
        raise SystemExit("[NOT EVALUABLE] invalid source-capability gate")
    manifest_path = CASE_DIR / "variants/manifest.json"
    try:
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
        variants = manifest["variants"]
        expected = {
            f"hrr{peak:03d}_adj{adjustment}_{corridor}": (peak, adjustment, corridor)
            for peak in PEAKS
            for adjustment in ADJUSTMENTS
            for corridor in CORRIDORS
        }
        ids = [str(record.get("id")) for record in variants]
        if (
            manifest.get("case_id") != CASE_ID
            or manifest.get("source_commit") != SOURCE_COMMIT
            or manifest.get("variant_count") != 8
            or len(variants) != 8
            or len(ids) != len(set(ids))
            or set(ids) != set(expected)
        ):
            raise ValueError("variant manifest/provenance contract failed")
        for record in variants:
            peak, adjustment, corridor = expected[str(record["id"])]
            if (
                not math.isclose(float(record["source_hrrpua_peak_kw_m2"]), peak)
                or not math.isclose(float(record["wildland_adjustment"]), adjustment)
                or record["corridor"] != corridor
            ):
                raise ValueError(f"variant factors do not match identity: {record['id']}")
    except (OSError, KeyError, TypeError, ValueError, json.JSONDecodeError) as error:
        payload = incomplete(8, 0, {"manifest": str(error)})
        (CASE_DIR / "outputs/metrics.json").write_text(json.dumps(payload, indent=2) + "\n")
        raise SystemExit("[NOT EVALUABLE] invalid generated variant manifest")

    observations: list[dict[str, object]] = []
    missing: dict[str, str] = {}
    for record in variants:
        variant_id = str(record["id"])
        root = CASE_DIR / "variants" / variant_id
        try:
            local = check_input_manifest(root)
            if local["id"] != variant_id:
                raise ValueError("variant identity mismatch")
            selected = select_outputs(root)
            observations.append(observe(root, local, selected))
        except (OSError, KeyError, TypeError, ValueError, csv.Error, json.JSONDecodeError, rasterio.errors.RasterioError) as error:
            missing[variant_id] = str(error)

    output_dir = CASE_DIR / "outputs"
    output_dir.mkdir(exist_ok=True)
    if missing:
        payload = incomplete(len(variants), len(observations), missing)
    else:
        executable_hashes = {str(row["executable_sha256"]) for row in observations}
        if len(executable_hashes) != 1:
            payload = incomplete(
                len(variants), len(observations),
                {"executable_identity": "Variants were run with more than one binary SHA-256."},
            )
            payload["workflow_status"] = "INCOMPLETE"
        else:
            common_executable_sha256 = next(iter(executable_hashes))
            try:
                metrics = assess(observations)
                plot_results(observations)
            except (ArithmeticError, KeyError, TypeError, ValueError, OSError, rasterio.errors.RasterioError) as error:
                payload = {
                    "case_id": CASE_ID,
                    "overall_status": "NOT EVALUABLE",
                    "workflow_status": "INCOMPLETE",
                    "verification_passed": False,
                    "required_outputs_complete": False,
                    "required_variant_count": len(variants),
                    "completed_variant_count": len(observations),
                    "implementation_characterization_status": "NOT EVALUABLE",
                    "intended_capability_status": "NOT EVALUABLE",
                    "metrics": [],
                    "reason": f"Complete output evidence could not be assessed: {error}",
                    "source_commit": SOURCE_COMMIT,
                    "executed_binary_sha256": common_executable_sha256,
                }
            else:
                characterization = "PASS" if all(item["status"] == "PASS" for item in metrics) else "FAIL"
                intended = {
                    "name": "intended heat-only urban-to-wildland ignition capability",
                    "expected": "heat exposure can initiate a disconnected wildland receiver",
                    "calculated": "no source-code state transition exists for FBFM != 91 target cells",
                    "units": "capability",
                    "status": "NOT EVALUABLE",
                    "layer": "published intended capability",
                    "rationale": "The experiment can characterize the current implementation but cannot verify an unimplemented target law.",
                }
                payload = {
                    "case_id": CASE_ID,
                    "overall_status": "NOT EVALUABLE",
                    "workflow_status": "COMPLETE",
                    "verification_passed": False,
                    "required_outputs_complete": True,
                    "required_variant_count": len(variants),
                    "completed_variant_count": len(observations),
                    "implementation_characterization_status": characterization,
                    "intended_capability_status": "NOT EVALUABLE",
                    "metrics": [*metrics, intended],
                    "reason": "Current behavior is characterized separately; the documented heat-only U-to-W capability has no executable oracle path at this source revision.",
                    "source_commit": SOURCE_COMMIT,
                    "executed_binary_sha256": common_executable_sha256,
                    "variants": observations,
                }
    (output_dir / "observations.json").write_text(
        json.dumps({"case_id": CASE_ID, "variants": observations}, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    (output_dir / "metrics.json").write_text(
        json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )
    print(f"[OK] {CASE_ID}: {payload['overall_status']} ({payload['workflow_status']})")


if __name__ == "__main__":
    main()
