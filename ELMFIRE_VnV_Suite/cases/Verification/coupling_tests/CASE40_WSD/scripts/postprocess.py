#!/usr/bin/env python3
"""Measure head direction, head ROS, ellipse ratio, and reaction intensity."""
from __future__ import annotations

from report_language import polish_figure

import csv
import json
import math
import re
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import rasterio
from rasterio.transform import from_origin

CASE_DIR = Path(__file__).resolve().parents[1]
EXPECTED_SHAPE = (140, 140)
EXPECTED_TRANSFORM = from_origin(-700.0, 700.0, 10.0, 10.0)
EXPECTED_CRS = "EPSG:32610"
EXPECTED_NEGATIVE_CELLS = 24
MIN_BURNED_CELLS = 100
MIN_HEAD_CELLS = 5
MIN_HEAD_CORRIDOR_CELLS = 10
MIN_IR_BURNED_COVERAGE = 0.90


def terminal_files(
    variant_root: Path, expected_tstop_s: float
) -> tuple[Path | None, Path | None, str | None]:
    """Resolve regular or safely identifiable stalled-final ELMFIRE rasters."""
    directory = variant_root / "outputs"
    try:
        config = (variant_root / "elmfire.data").read_text(encoding="utf-8")
        timestep_values = re.findall(
            r"(?mi)^\s*SIMULATION_DT\s*=\s*([0-9.eEdD+-]+)", config
        )
        met_step_values = re.findall(
            r"(?mi)^\s*DT_METEOROLOGY\s*=\s*([0-9.eEdD+-]+)", config
        )
        if len(timestep_values) != 1 or len(met_step_values) != 1:
            raise ValueError("time-control assignments are not unique")
        timestep = float(timestep_values[0].replace("D", "E").replace("d", "e"))
        met_step = float(met_step_values[0].replace("D", "E").replace("d", "e"))
    except (OSError, ValueError) as exc:
        return None, None, f"invalid time-control metadata ({exc})"
    manifests = sorted(directory.glob("dump_times_*.csv"))
    if len(manifests) != 1:
        return None, None, f"expected one dump_times CSV, found {len(manifests)}"
    try:
        with manifests[0].open(newline="", encoding="utf-8") as stream:
            rows = list(csv.DictReader(stream))
        final_rows = [
            row
            for row in rows
            if str(row.get("is_final_dump", "")).strip().upper()
            in {"T", "TRUE", "1", "Y", "YES"}
        ]
        if len(final_rows) != 1:
            return None, None, (
                f"expected one final dump record, found {len(final_rows)}"
            )
        final_time = float(final_rows[0]["time_seconds"])
    except (OSError, KeyError, TypeError, ValueError, csv.Error) as exc:
        return None, None, f"invalid dump-times metadata ({exc})"
    regular_terminal_dump = math.isclose(
        final_time, expected_tstop_s, rel_tol=0.0, abs_tol=1.0e-6
    )
    stalled_terminal_dump = (
        final_time > expected_tstop_s
        and math.isclose(
            final_time - met_step, expected_tstop_s,
            rel_tol=0.0, abs_tol=max(timestep, 1.0e-6),
        )
    )
    if not (regular_terminal_dump or stalled_terminal_dump):
        return None, None, (
            f"final dump is {final_time:g} s, expected {expected_tstop_s:g} s"
        )
    stamp = int(round(final_time))

    def one(pattern: str) -> Path | None:
        matches = sorted(
            path
            for path in directory.glob(pattern)
            if "_transient_" not in path.name
        )
        return matches[0] if len(matches) == 1 else None

    if stalled_terminal_dump:
        toa = one("time_of_arrival*_*.tif")
        intensity = one("ir_*_*.tif")
    else:
        toa = one(f"time_of_arrival*_{stamp:07d}.tif")
        intensity = one(f"ir_*_{stamp:07d}.tif")
    if toa is None or intensity is None:
        return None, None, "final TOA and reaction-intensity rasters are not unique"
    return toa, intensity, None


def circular_error(actual: float, expected: float) -> float:
    return abs((actual - expected + 180.0) % 360.0 - 180.0)


def front_contract(path: Path) -> bool:
    """Require the bounded, interior ignition produced by this case version."""
    if not path.is_file():
        return False
    try:
        with rasterio.open(path) as source:
            if (
                source.shape != EXPECTED_SHAPE
                or not source.transform.almost_equals(EXPECTED_TRANSFORM)
                or str(source.crs) != EXPECTED_CRS
            ):
                return False
            phi = source.read(1, masked=True).filled(np.nan).astype(float)
    except (OSError, rasterio.errors.RasterioError):
        return False
    border = np.concatenate((phi[0, :], phi[-1, :], phi[:, 0], phi[:, -1]))
    return bool(
        np.all(np.isfinite(phi))
        and np.min(phi) >= -1.0001
        and np.max(phi) <= 1.0001
        and np.count_nonzero(phi < 0.0) == EXPECTED_NEGATIVE_CELLS
        and np.any((phi > -0.9999) & (phi < 0.0))
        and np.any((phi > 0.0) & (phi < 0.9999))
        and not np.any(np.isclose(phi, 0.0, atol=1.0e-7))
        and np.all(np.isfinite(border))
        and np.all(border > 0.0)
    )


def write_manifest_error(reason: str, required_count: int = 0) -> None:
    """Record malformed or obsolete generated metadata as unavailable evidence."""
    result = {
        "case_id": "CASE40_WSD",
        "overall_status": "NOT EVALUABLE",
        "verification_passed": False,
        "required_outputs_complete": False,
        "required_variant_count": required_count,
        "completed_variant_count": 0,
        "missing_variants": [],
        "metrics": [],
        "variant_results": [],
        "reason": reason,
    }
    (CASE_DIR / "outputs").mkdir(exist_ok=True)
    (CASE_DIR / "outputs/metrics.json").write_text(
        json.dumps(result, indent=2, allow_nan=False) + "\n"
    )


def finite_number(value: object, label: str, *, positive: bool = False) -> float:
    """Coerce a manifest number and reject nonfinite or invalid denominators."""
    if isinstance(value, bool):
        raise TypeError(f"{label} must be numeric, not boolean")
    number = float(value)
    if not math.isfinite(number) or (positive and number <= 0.0):
        qualifier = "positive and finite" if positive else "finite"
        raise ValueError(f"{label} must be {qualifier}")
    return number


def main() -> None:
    specification: dict[str, object] = {}
    try:
        specification = json.loads(
            (CASE_DIR / "variants/expected.json").read_text(encoding="utf-8")
        )
        if not isinstance(specification, dict):
            raise ValueError("manifest root must be an object")
        raw_variants = specification["variants"]
        if not isinstance(raw_variants, list) or not raw_variants:
            raise ValueError("variants must be a nonempty list")
        tstop = finite_number(
            specification["simulation_tstop_s"],
            "simulation_tstop_s",
            positive=True,
        )
        slope_degrees = finite_number(
            specification["slope_degrees"], "slope_degrees"
        )
        if not 0.0 <= slope_degrees < 90.0:
            raise ValueError("slope_degrees must be in [0, 90)")
        raw_tolerances = specification["tolerances"]
        if not isinstance(raw_tolerances, dict):
            raise ValueError("tolerances must be an object")
        tolerances = {}
        for key in (
            "direction_degrees",
            "head_ros_relative_error",
            "length_width_relative_error",
            "ir_relative_error",
        ):
            tolerances[key] = finite_number(
                raw_tolerances[key], f"tolerances.{key}", positive=True
            )
        required_variant_keys = {
            "id",
            "relative_angle_degrees",
            "expected_direction_degrees",
            "expected_head_ros_m_min",
            "expected_length_width",
            "expected_ir_kw_m2",
        }
        variants = []
        for index, raw_variant in enumerate(raw_variants):
            if not isinstance(raw_variant, dict) or not required_variant_keys.issubset(
                raw_variant
            ):
                raise ValueError(
                    f"variant {index} lacks required identity or oracle values"
                )
            variant = dict(raw_variant)
            if not isinstance(variant["id"], str) or not variant["id"].strip():
                raise ValueError(f"variant {index} has an invalid id")
            for key in ("relative_angle_degrees", "expected_direction_degrees"):
                variant[key] = finite_number(variant[key], f"{variant['id']}.{key}")
            for key in (
                "expected_head_ros_m_min",
                "expected_length_width",
                "expected_ir_kw_m2",
            ):
                variant[key] = finite_number(
                    variant[key], f"{variant['id']}.{key}", positive=True
                )
            variants.append(variant)
        specification["variants"] = variants
        specification["simulation_tstop_s"] = tstop
        specification["slope_degrees"] = slope_degrees
        specification["tolerances"] = tolerances
    except (OSError, KeyError, TypeError, ValueError, json.JSONDecodeError) as exc:
        candidate_variants = (
            specification.get("variants", [])
            if isinstance(specification, dict)
            else []
        )
        required_count = (
            len(candidate_variants) if isinstance(candidate_variants, list) else 0
        )
        write_manifest_error(
            f"Generated variant metadata is missing, obsolete, or malformed ({exc}); "
            "rerun the complete case.",
            required_count,
        )
        print("[OK] CASE40_WSD: NOT EVALUABLE")
        return
    rows, missing, issues = [], [], []
    for variant in variants:
        variant_root = CASE_DIR / "variants" / variant["id"]
        output = variant_root / "outputs"
        front_path = variant_root / "inputs/phi.tif"
        toa_path, ir_path, terminal_issue = terminal_files(
            variant_root, tstop
        )
        front_valid = front_contract(front_path)
        if not front_valid or terminal_issue is not None:
            missing.append(variant["id"])
            if not front_valid:
                issues.append(f"{variant['id']}: invalid current PHI input")
            if terminal_issue is not None:
                issues.append(f"{variant['id']}: {terminal_issue}")
            continue
        try:
            with (
                rasterio.open(front_path) as front_source,
                rasterio.open(toa_path) as source,
                rasterio.open(ir_path) as ir_source,
            ):
                if (
                    source.shape != front_source.shape
                    or ir_source.shape != front_source.shape
                    or not source.transform.almost_equals(front_source.transform)
                    or not ir_source.transform.almost_equals(front_source.transform)
                    or source.crs != front_source.crs
                    or ir_source.crs != front_source.crs
                ):
                    raise ValueError("output grids do not match the current PHI grid")
                toa = source.read(1, masked=True).filled(np.nan).astype(float)
                rr, cc = np.indices(toa.shape)
                xs, ys = rasterio.transform.xy(
                    source.transform, rr, cc, offset="center"
                )
                ir = ir_source.read(1, masked=True).filled(np.nan).astype(float)
        except (OSError, ValueError, rasterio.errors.RasterioError) as exc:
            missing.append(variant["id"])
            issues.append(f"{variant['id']}: invalid raster evidence ({exc})")
            continue
        # Rasterio releases differ in whether ``xy`` preserves the input
        # index shape or returns flattened coordinate vectors.  Normalize the
        # coordinates before applying the two-dimensional burned-cell mask.
        x = np.asarray(xs, dtype=float).reshape(toa.shape)
        y = np.asarray(ys, dtype=float).reshape(toa.shape)
        y = y / math.cos(math.radians(slope_degrees))
        burned = np.isfinite(toa) & (toa >= 0.0)
        burned_count = int(np.count_nonzero(burned))
        if burned_count < MIN_BURNED_CELLS:
            missing.append(variant["id"])
            issues.append(f"{variant['id']}: insufficient burned-footprint coverage")
            continue
        xb, yb, tb = x[burned], y[burned], toa[burned]
        radial = np.hypot(xb, yb)
        # Select the farthest one percent explicitly, with the documented
        # five-cell floor.  A percentile-value threshold can select fewer
        # than five cells for a valid compact footprint when radii are mostly
        # unique (as in the opposed wind/slope variant).
        head_count = max(MIN_HEAD_CELLS, int(math.ceil(0.01 * radial.size)))
        head_indices = np.argpartition(radial, -head_count)[-head_count:]
        head_cells = np.zeros(radial.size, dtype=bool)
        head_cells[head_indices] = True
        measured_direction = math.degrees(
            math.atan2(float(np.mean(xb[head_cells])), float(np.mean(yb[head_cells])))
        ) % 360.0
        theta = math.radians(variant["expected_direction_degrees"])
        along = xb*math.sin(theta) + yb*math.cos(theta)
        across = xb*math.cos(theta) - yb*math.sin(theta)
        corridor = (np.abs(across) <= 25.0) & (along >= 80.0) & (tb > 0.0)
        if np.count_nonzero(corridor) < MIN_HEAD_CORRIDOR_CELLS:
            missing.append(variant["id"])
            issues.append(f"{variant['id']}: insufficient head-axis TOA coverage")
            continue
        try:
            slope, _ = np.polyfit(along[corridor], tb[corridor], 1)
        except (ValueError, np.linalg.LinAlgError, FloatingPointError) as exc:
            missing.append(variant["id"])
            issues.append(f"{variant['id']}: invalid head-axis regression ({exc})")
            continue
        measured_ros = 60.0/slope if slope > 0.0 else math.nan
        measured_low = (np.max(along)-np.min(along))/max(np.max(across)-np.min(across), 1.0)
        burned_ir = ir[burned]
        valid_ir = burned_ir[np.isfinite(burned_ir) & (burned_ir >= 0.0)]
        if valid_ir.size / burned_count < MIN_IR_BURNED_COVERAGE:
            missing.append(variant["id"])
            issues.append(f"{variant['id']}: insufficient reaction-intensity coverage")
            continue
        measured_ir = float(np.median(valid_ir))
        calculated = (
            measured_direction,
            measured_ros,
            measured_low,
            measured_ir,
        )
        if not all(math.isfinite(value) for value in calculated):
            missing.append(variant["id"])
            issues.append(f"{variant['id']}: nonfinite derived observable")
            continue
        rows.append({**variant, "measured_direction_degrees": measured_direction,
            "direction_error_degrees": circular_error(measured_direction, variant["expected_direction_degrees"]),
            "measured_head_ros_m_min": measured_ros,
            "head_ros_relative_error": abs(measured_ros-variant["expected_head_ros_m_min"])/variant["expected_head_ros_m_min"],
            "measured_length_width": measured_low,
            "length_width_relative_error": abs(measured_low-variant["expected_length_width"])/variant["expected_length_width"],
            "measured_ir_kw_m2": measured_ir,
            "ir_relative_error": abs(measured_ir-variant["expected_ir_kw_m2"])/variant["expected_ir_kw_m2"]})
    complete = len(rows) == len(specification["variants"]) and not missing
    tolerances = specification["tolerances"]
    summaries = (
        ("maximum DMS direction error", "direction_error_degrees", tolerances["direction_degrees"], "degrees"),
        ("maximum head-ROS relative error", "head_ros_relative_error", tolerances["head_ros_relative_error"], "fraction"),
        ("maximum length/width relative error", "length_width_relative_error", tolerances["length_width_relative_error"], "fraction"),
        ("maximum reaction-intensity relative error", "ir_relative_error", tolerances["ir_relative_error"], "fraction"),
    )
    metrics = []
    for name, key, limit, units in summaries:
        value = max((float(row[key]) for row in rows), default=None)
        metrics.append({"name": name, "expected": f"<= {limit}", "calculated": value,
                        "units": units,
                        "status": (
                            "NOT EVALUABLE"
                            if not complete
                            else ("PASS" if value is not None and value <= limit else "FAIL")
                        )})
    passed = complete and all(metric["status"] == "PASS" for metric in metrics)
    result = {"case_id": "CASE40_WSD", "overall_status": "PASS" if passed else ("FAIL" if complete else "NOT EVALUABLE"),
              "verification_passed": passed, "required_outputs_complete": complete,
              "required_variant_count": len(specification["variants"]), "completed_variant_count": len(rows),
              "missing_variants": missing, "metrics": metrics, "variant_results": rows}
    if not complete:
        result["reason"] = "; ".join(issues) or (
            "Required evidence is incomplete for: " + ", ".join(missing)
        )
    (CASE_DIR / "outputs").mkdir(exist_ok=True)
    (CASE_DIR / "outputs/metrics.json").write_text(
        json.dumps(result, indent=2, allow_nan=False) + "\n"
    )
    if rows:
        fig, axes = plt.subplots(1, 2, figsize=(10, 4))
        angle = [row["relative_angle_degrees"] for row in rows]
        axes[0].plot(angle, [row["expected_direction_degrees"] for row in rows], "-", label="expected")
        axes[0].plot(angle, [row["measured_direction_degrees"] for row in rows], "o", label="ELMFIRE")
        axes[1].plot(angle, [row["expected_head_ros_m_min"] for row in rows], "-", label="expected")
        axes[1].plot(angle, [row["measured_head_ros_m_min"] for row in rows], "o", label="ELMFIRE")
        axes[0].set(xlabel="wind-to-upslope angle (degrees)", ylabel="DMS azimuth (degrees)")
        axes[1].set(xlabel="wind-to-upslope angle (degrees)", ylabel="head ROS (m/min)")
        for axis in axes: axis.grid(alpha=0.25); axis.legend()
        fig.tight_layout(); (CASE_DIR/"figures").mkdir(exist_ok=True)
        polish_figure(fig)
        fig.savefig(CASE_DIR/"figures/vector_response.pdf", bbox_inches="tight"); plt.close(fig)
    print(f"[OK] CASE40_WSD: {result['overall_status']}")


if __name__ == "__main__":
    main()
