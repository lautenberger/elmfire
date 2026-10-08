#!/usr/bin/env python3
"""Read case-local ELMFIRE rasters and evaluate an analytical sweep."""
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

INITIAL_FRONT_X_M = -400.0
CELL_SIZE_M = 10.0
CORRIDOR_X_MAX_M = INITIAL_FRONT_X_M + 250.0
CORRIDOR_HALF_HEIGHT_M = 150.0


def _terminal_outputs(
    variant_root: Path, toa_required: bool
) -> tuple[Path | None, Path | None, Path | None, list[str]]:
    """Resolve unique final rasters and prove the run reached its stop time."""
    try:
        text = (variant_root / "elmfire.data").read_text(encoding="utf-8")
        matches = re.findall(
            r"(?mi)^\s*SIMULATION_TSTOP\s*=\s*([0-9.eEdD+-]+)", text
        )
        if len(matches) != 1:
            raise ValueError("SIMULATION_TSTOP is not unique")
        tstop = float(matches[0].replace("D", "E").replace("d", "e"))
        timestep_matches = re.findall(
            r"(?mi)^\s*SIMULATION_DT\s*=\s*([0-9.eEdD+-]+)", text
        )
        met_step_matches = re.findall(
            r"(?mi)^\s*DT_METEOROLOGY\s*=\s*([0-9.eEdD+-]+)", text
        )
        if len(timestep_matches) != 1 or len(met_step_matches) != 1:
            raise ValueError("time-control assignments are not unique")
        timestep = float(
            timestep_matches[0].replace("D", "E").replace("d", "e")
        )
        met_step = float(
            met_step_matches[0].replace("D", "E").replace("d", "e")
        )
        manifests = sorted((variant_root / "outputs").glob("dump_times_*.csv"))
        if len(manifests) != 1:
            raise ValueError("dump-times manifest is not unique")
        with manifests[0].open(newline="", encoding="utf-8") as stream:
            rows = list(csv.DictReader(stream))
        final_rows = [
            row
            for row in rows
            if str(row.get("is_final_dump", "")).strip().upper()
            in {"T", "TRUE", "1", "Y", "YES"}
        ]
        if len(final_rows) != 1:
            raise ValueError("final dump record is not unique")
        final_time = float(final_rows[0]["time_seconds"])
        regular_terminal_dump = math.isclose(
            final_time, tstop, rel_tol=0.0, abs_tol=1.0e-3
        )
        pre_jump_time = final_time - met_step
        stalled_terminal_dump = (
            final_time > tstop
            and math.isclose(
                pre_jump_time, tstop, rel_tol=0.0, abs_tol=max(timestep, 1.0e-3)
            )
        )
        if toa_required and not (regular_terminal_dump or stalled_terminal_dump):
            raise ValueError("final dump did not reach SIMULATION_TSTOP")
    except (OSError, KeyError, TypeError, ValueError, csv.Error):
        return None, None, None, ["terminal_dump"]

    stamp = int(math.floor(final_time + 0.5))  # ELMFIRE uses Fortran NINT.
    output = variant_root / "outputs"

    def one(pattern: str) -> Path | None:
        paths = sorted(
            path
            for path in output.glob(pattern)
            if "_transient_" not in path.name
        )
        return paths[0] if len(paths) == 1 else None

    if stalled_terminal_dump:
        # The stalled-front branch advances T by DT_METEOROLOGY before its
        # unconditional final dump; I7.7 filenames then contain asterisks.
        # Accept only one uniquely named raster of each required type.
        spread = one("vs_*_*.tif")
        intensity = one("ir_*_*.tif")
        toa = one("time_of_arrival*_*.tif")
    else:
        spread = one(f"vs_*_{stamp:07d}.tif")
        intensity = one(f"ir_*_{stamp:07d}.tif")
        toa = one(f"time_of_arrival*_{stamp:07d}.tif")
    unavailable = []
    if spread is None:
        unavailable.append("direct_ros")
    if intensity is None:
        unavailable.append("reaction_intensity")
    if toa_required and toa is None:
        unavailable.append("time_of_arrival")
    return spread, intensity, toa, unavailable


def _front_contract(path: Path) -> bool:
    """Reject stale/unbounded PHI inputs before accepting their outputs."""
    if not path.is_file():
        return False
    try:
        with rasterio.open(path) as source:
            phi = source.read(1, masked=True).filled(np.nan).astype(float)
            expected_transform = rasterio.transform.from_origin(
                -700.0, 700.0, CELL_SIZE_M, CELL_SIZE_M
            )
            grid_valid = (
                source.shape == (140, 140)
                and source.transform == expected_transform
                and source.crs is not None
                and source.crs.to_epsg() == 32610
            )
    except (OSError, ValueError, IndexError, rasterio.errors.RasterioError):
        return False
    finite = np.isfinite(phi)
    if not grid_valid or not np.all(finite):
        return False
    values = phi[finite]
    border = np.concatenate((phi[0, :], phi[-1, :], phi[:, 0], phi[:, -1]))
    return bool(
        np.min(values) >= -1.0001
        and np.max(values) <= 1.0001
        and np.count_nonzero(values < 0.0) == 300
        and np.any(values > 0.0)
        and np.any((values > -0.9999) & (values < -1.0e-6))
        and np.any((values > 1.0e-6) & (values < 0.9999))
        and not np.any(np.isclose(values, 0.0, atol=1.0e-7))
        and np.all(np.isfinite(border))
        and np.all(border > 0.0)
    )


def _direct_observables(
    spread_path: Path, intensity_path: Path, front_path: Path
) -> tuple[np.ndarray, np.ndarray]:
    """Read colocated direct fields at and downstream of the planar head."""
    with rasterio.open(spread_path) as spread_source, rasterio.open(
        intensity_path
    ) as intensity_source, rasterio.open(front_path) as front_source:
        if (
            spread_source.shape != intensity_source.shape
            or spread_source.transform != intensity_source.transform
            or spread_source.crs != intensity_source.crs
            or spread_source.shape != front_source.shape
            or spread_source.transform != front_source.transform
            or spread_source.crs != front_source.crs
        ):
            raise ValueError("Direct fields do not match the current PHI grid")
        spread = spread_source.read(1, masked=True).filled(np.nan).astype(float)
        intensity = intensity_source.read(1, masked=True).filled(np.nan).astype(float)
        rows, cols = np.indices(spread.shape)
        xs, ys = rasterio.transform.xy(
            spread_source.transform, rows, cols, offset="center"
        )
        x = np.asarray(xs, dtype=float).reshape(spread.shape)
        y = np.asarray(ys, dtype=float).reshape(spread.shape)
        cell_size = abs(float(spread_source.transform.a))
    valid = (
        np.isfinite(spread)
        & np.isfinite(intensity)
        & (spread >= 0.0)
        & (intensity >= 0.0)
        & (x >= INITIAL_FRONT_X_M - cell_size)
        & (x <= CORRIDOR_X_MAX_M)
        & (np.abs(y) <= CORRIDOR_HALF_HEIGHT_M)
    )
    return spread[valid], intensity[valid]


def _relative_error(actual: float, expected: float, floor: float = 1.0e-8) -> float:
    return abs(actual - expected) / max(abs(expected), floor)


def _metric_status(complete: bool, passed: bool) -> str:
    """Keep unavailable evidence distinct from a calculated scientific failure."""
    if not complete:
        return "NOT EVALUABLE"
    return "PASS" if passed else "FAIL"


def _toa_ros(
    path: Path,
    front_path: Path,
    initial_front_x_m: float = INITIAL_FRONT_X_M,
    slope_degrees: float = 0.0,
) -> tuple[float, float]:
    """Fit arrival time versus x in an interior corridor; return m/min and R2."""
    try:
        with rasterio.open(path) as source, rasterio.open(
            front_path
        ) as front_source:
            if (
                source.shape != front_source.shape
                or source.transform != front_source.transform
                or source.crs != front_source.crs
            ):
                return math.nan, math.nan
            toa = source.read(1, masked=True).filled(np.nan).astype(float)
            rows, cols = np.indices(toa.shape)
            xs, ys = rasterio.transform.xy(
                source.transform, rows, cols, offset="center"
            )
            cell_size = abs(float(source.transform.a))
    except (OSError, ValueError, IndexError, rasterio.errors.RasterioError):
        return math.nan, math.nan
    x = np.asarray(xs, dtype=float).reshape(toa.shape)
    y = np.asarray(ys, dtype=float).reshape(toa.shape)
    slope_cosine = math.cos(math.radians(slope_degrees))
    if not math.isfinite(slope_cosine) or slope_cosine <= 0.0:
        return math.nan, math.nan
    x_surface = initial_front_x_m + (x - initial_front_x_m) / slope_cosine
    surface_cell_size = cell_size / slope_cosine
    mask = (
        np.isfinite(toa)
        & (toa > 0.0)
        & (x_surface >= initial_front_x_m + 0.5 * surface_cell_size)
        & (x_surface <= initial_front_x_m + 250.0)
        & (np.abs(y) <= 150.0)
    )
    if np.count_nonzero(mask) < 20 or np.unique(x_surface[mask]).size < 3:
        return math.nan, math.nan
    try:
        slope, intercept = np.polyfit(x_surface[mask], toa[mask], 1)
    except (ValueError, np.linalg.LinAlgError, FloatingPointError):
        return math.nan, math.nan
    fitted = slope * x_surface[mask] + intercept
    residual = float(np.sum((toa[mask] - fitted) ** 2))
    total = float(np.sum((toa[mask] - np.mean(toa[mask])) ** 2))
    r2 = 1.0 - residual / total if total > 0.0 else math.nan
    return (60.0 / slope if slope > 0.0 else math.nan), r2


def evaluate(case_dir: Path) -> dict[str, object]:
    specification = json.loads((case_dir / "variants/expected.json").read_text())
    rows: list[dict[str, object]] = []
    missing: dict[str, list[str]] = {}
    for variant in specification["variants"]:
        variant_id = str(variant["id"])
        variant_root = case_dir / "variants" / variant_id
        front_path = variant_root / "inputs/phi.tif"
        expected_ros = float(variant["expected_ros_m_min"])
        expected_ir = float(variant["expected_ir_kw_m2"])
        toa_required = bool(variant.get("toa_required", expected_ros >= 0.10))
        spread_path, ir_path, toa_path, unavailable = _terminal_outputs(
            variant_root, toa_required
        )
        if not _front_contract(front_path):
            unavailable.append("front_initialization")
        spread_values = np.array([], dtype=float)
        ir_values = np.array([], dtype=float)
        if spread_path is not None and ir_path is not None:
            try:
                spread_values, ir_values = _direct_observables(
                    spread_path, ir_path, front_path
                )
            except (
                OSError,
                ValueError,
                IndexError,
                rasterio.errors.RasterioError,
            ):
                unavailable.append("direct_grid_alignment")
            if spread_values.size == 0 or ir_values.size == 0:
                unavailable.append("colocated_direct_observables")
        toa_ros: float | None = None
        toa_r2: float | None = None
        if toa_required:
            if toa_path is None:
                unavailable.append("time_of_arrival")
            else:
                toa_ros, toa_r2 = _toa_ros(
                    toa_path,
                    front_path,
                    slope_degrees=float(variant.get("slope_degrees", 0.0)),
                )
                if not (math.isfinite(toa_ros) and math.isfinite(toa_r2)):
                    unavailable.append("time_of_arrival_fit")
        if unavailable:
            missing[variant_id] = sorted(set(unavailable))
            continue
        # Direct fields are sparse solver diagnostics: zero or off-axis values may
        # coexist with the homogeneous direction-of-maximum-spread value. The
        # upper envelope is the configured head ROS and reaction intensity.
        measured_ros = float(np.max(spread_values))
        measured_ir = float(np.max(ir_values))
        source_files = [
            str(spread_path.relative_to(case_dir)),
            str(ir_path.relative_to(case_dir)),
        ]
        if toa_required and toa_path is not None:
            source_files.append(str(toa_path.relative_to(case_dir)))
        rows.append(
            {
                **variant,
                "toa_required": toa_required,
                "measured_ros_m_min": measured_ros,
                "measured_ir_kw_m2": measured_ir,
                "toa_ros_m_min": toa_ros,
                "toa_r2": toa_r2,
                "ros_relative_error": _relative_error(measured_ros, expected_ros),
                "ir_relative_error": _relative_error(measured_ir, expected_ir),
                "toa_ros_relative_error": (
                    _relative_error(toa_ros, expected_ros)
                    if toa_ros is not None
                    else None
                ),
                "source_files": source_files,
            }
        )

    required = len(specification["variants"])
    complete = len(rows) == required and not missing
    ros_max = max((float(row["ros_relative_error"]) for row in rows), default=None)
    ir_max = max((float(row["ir_relative_error"]) for row in rows), default=None)
    propagating = [row for row in rows if row["toa_required"]]
    toa_metrics_required = any(
        bool(
            variant.get(
                "toa_required",
                float(variant.get("expected_ros_m_min", 0.0)) >= 0.10,
            )
        )
        for variant in specification["variants"]
    )
    toa_max = max(
        (float(row["toa_ros_relative_error"]) for row in propagating), default=None
    )
    r2_min = min((float(row["toa_r2"]) for row in propagating), default=None)
    tolerances = specification["tolerances"]
    metric_rows = [
        {
            "name": "maximum direct-ROS relative error",
            "expected": f"<= {tolerances['ros_relative_error']}",
            "calculated": ros_max,
            "units": "fraction",
            "status": _metric_status(
                complete,
                ros_max is not None and ros_max <= tolerances["ros_relative_error"],
            ),
        },
        {
            "name": "maximum reaction-intensity relative error",
            "expected": f"<= {tolerances['ir_relative_error']}",
            "calculated": ir_max,
            "units": "fraction",
            "status": _metric_status(
                complete,
                ir_max is not None and ir_max <= tolerances["ir_relative_error"],
            ),
        },
    ]
    if toa_metrics_required:
        metric_rows.extend(
            [
                {
                    "name": "maximum TOA-ROS relative error",
                    "expected": f"<= {tolerances['toa_ros_relative_error']}",
                    "calculated": toa_max,
                    "units": "fraction",
                    "status": _metric_status(
                        complete,
                        toa_max is not None
                        and toa_max <= tolerances["toa_ros_relative_error"],
                    ),
                },
                {
                    "name": "minimum TOA regression R2",
                    "expected": f">= {tolerances['toa_r2_min']}",
                    "calculated": r2_min,
                    "units": "dimensionless",
                    "status": _metric_status(
                        complete,
                        r2_min is not None
                        and r2_min >= tolerances["toa_r2_min"],
                    ),
                },
            ]
        )
    passed = complete and all(item["status"] == "PASS" for item in metric_rows)
    result = {
        "case_id": specification["case_id"],
        "overall_status": "PASS" if passed else ("FAIL" if complete else "NOT EVALUABLE"),
        "verification_passed": passed,
        "required_outputs_complete": complete,
        "required_variant_count": required,
        "completed_variant_count": len(rows),
        "missing_variants": list(missing),
        "missing_evidence": missing,
        "metrics": metric_rows,
        "variant_results": rows,
    }
    if not complete:
        result["reason"] = (
            "Required evidence is unavailable: "
            + "; ".join(
                f"{variant_id} ({', '.join(evidence)})"
                for variant_id, evidence in missing.items()
            )
        )
    return result


def plot(case_dir: Path, results: dict[str, object]) -> None:
    """Plot paired reference and ELMFIRE values with unambiguous styling.

    Color identifies the fuel model in both panels. Marker shape identifies
    the source: an open diamond is the independent hand calculation and a
    filled circle is the ELMFIRE result. Each fuel-model group contains only
    one x value, so marker-only rendering avoids an invisible one-point line.
    """
    from matplotlib.lines import Line2D

    rows = results.get("variant_results", [])
    if not rows:
        return
    figure_dir = case_dir / "figures"
    figure_dir.mkdir(parents=True, exist_ok=True)
    fig, axes = plt.subplots(1, 2, figsize=(10, 4.2))
    groups = sorted({str(row.get("group", "all")) for row in rows})
    colors = plt.get_cmap("tab10").colors
    group_handles = []
    for index, group in enumerate(groups):
        color = colors[index % len(colors)]
        selected = [row for row in rows if str(row.get("group", "all")) == group]
        selected.sort(key=lambda row: float(row["x"]))
        x = [row["x"] for row in selected]
        expected_style = {
            "color": color,
            "marker": "D",
            "linestyle": "none",
            "markersize": 7,
            "markerfacecolor": "none",
            "markeredgewidth": 1.6,
            "zorder": 3,
        }
        measured_style = {
            "color": color,
            "marker": "o",
            "linestyle": "none",
            "markersize": 5,
            "zorder": 4,
        }
        axes[0].plot(x, [row["expected_ros_m_min"] for row in selected], **expected_style)
        axes[0].plot(x, [row["measured_ros_m_min"] for row in selected], **measured_style)
        axes[1].plot(x, [row["expected_ir_kw_m2"] for row in selected], **expected_style)
        axes[1].plot(x, [row["measured_ir_kw_m2"] for row in selected], **measured_style)
        group_handles.append(
            Line2D([], [], color=color, marker="s", linestyle="none", label=group)
        )
    axes[0].set(xlabel=results.get("x_label", "sweep value"), ylabel="Rate of spread (m/min)")
    axes[1].set(xlabel=results.get("x_label", "sweep value"), ylabel="Reaction intensity (kW/m$^2$)")
    method_handles = [
        Line2D([], [], color="black", marker="D", markerfacecolor="none",
               markeredgewidth=1.6, linestyle="none", label="Independent calculation"),
        Line2D([], [], color="black", marker="o", linestyle="none", label="ELMFIRE"),
    ]
    model_legend = axes[0].legend(
        handles=group_handles, title="Fuel model", fontsize=7,
        title_fontsize=8, loc="best"
    )
    axes[0].add_artist(model_legend)
    axes[1].legend(
        handles=method_handles, title="Value source", fontsize=7,
        title_fontsize=8, loc="best"
    )
    for axis in axes:
        axis.grid(alpha=0.25)
    fig.tight_layout()
    polish_figure(fig)
    fig.savefig(figure_dir / "sweep_response.pdf", bbox_inches="tight")
    plt.close(fig)


def save(case_dir: Path, results: dict[str, object]) -> None:
    expected = json.loads((case_dir / "variants/expected.json").read_text())
    results["x_label"] = expected.get("x_label", "sweep value")
    output = case_dir / "outputs"
    output.mkdir(parents=True, exist_ok=True)
    (output / "metrics.json").write_text(
        json.dumps(results, indent=2, allow_nan=False) + "\n"
    )
    plot(case_dir, results)
