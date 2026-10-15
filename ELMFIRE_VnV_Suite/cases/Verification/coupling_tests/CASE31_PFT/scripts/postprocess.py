#!/usr/bin/env python3
"""Evaluate planarity, fuel-transition response, wind response, and barriers."""

from __future__ import annotations

from report_language import polish_figure
import csv
import json
from pathlib import Path
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import rasterio
from rasterio.transform import from_origin

CASE_DIR = Path(__file__).resolve().parents[1]
NAMES = [
    x.strip()
    for x in (CASE_DIR / "scripts/variants.txt").read_text().splitlines()
    if x.strip()
]
OUT, FIG = CASE_DIR / "outputs", CASE_DIR / "figures"
UPSTREAM_STATION_X_M = -50.0
DOWNSTREAM_STATION_X_M = 100.0
BARRIER_APPROACH_STATION_X_M = -5.0
OPENING_STATION_X_M = 35.0
CENTRAL_HALF_WIDTH_M = 350.0
OPENING_HALF_WIDTH_M = 40.0
MIN_STATION_COVERAGE = 0.90
EXPECTED_TSTOP_S = 43200.0
EXPECTED_SHAPE = (300, 300)
EXPECTED_TRANSFORM = from_origin(-750.0, 750.0, 5.0, 5.0)
EXPECTED_CRS = "EPSG:32610"
EXPECTED_NEGATIVE_CELLS = {"diagonal": 1981}
OUT.mkdir(parents=True, exist_ok=True)
FIG.mkdir(parents=True, exist_ok=True)


def path_for(name: str) -> tuple[Path | None, str | None]:
    """Resolve the one TOA raster identified as final by ELMFIRE metadata."""
    output = CASE_DIR / f"variants/{name}/outputs"
    manifests = sorted(output.glob("dump_times_*.csv"))
    if len(manifests) != 1:
        return None, f"{name}: expected one dump_times CSV, found {len(manifests)}"
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
            return None, f"{name}: expected one final dump record, found {len(final_rows)}"
        final_time = float(final_rows[0]["time_seconds"])
    except (OSError, KeyError, TypeError, ValueError, csv.Error) as exc:
        return None, f"{name}: invalid dump-times metadata ({exc})"
    if name == "gapped_break" and final_time < EXPECTED_TSTOP_S - 1.0e-3:
        return None, (
            f"{name}: final dump is {final_time:g} s, expected "
            f"{EXPECTED_TSTOP_S:g} s"
        )
    stamp = int(np.floor(final_time + 0.5))
    paths = sorted(
        path
        for path in output.glob(f"time_of_arrival*_{stamp:07d}.tif")
        if "_transient_" not in path.name
    )
    if len(paths) != 1:
        return None, (
            f"{name}: final dump at {final_time:g} s maps to "
            f"{len(paths)} TOA rasters"
        )
    return paths[0], None


def read(path: Path, front_path: Path) -> tuple[np.ndarray, object]:
    with rasterio.open(front_path) as front, rasterio.open(path) as src:
        if (
            src.shape != front.shape
            or not src.transform.almost_equals(front.transform)
            or src.crs != front.crs
        ):
            raise ValueError("TOA grid does not match the current PHI grid")
        a = src.read(1, masked=True).filled(np.nan).astype(float)
        a[(a < 0) | (a > 1e8)] = np.nan
        return a, src.transform


def coordinates(shape: tuple[int, int], transform: object) -> tuple[np.ndarray, np.ndarray]:
    rows, cols = np.indices(shape)
    xs, ys = rasterio.transform.xy(transform, rows, cols, offset="center")
    return (
        np.asarray(xs, dtype=float).reshape(shape),
        np.asarray(ys, dtype=float).reshape(shape),
    )


def front_contract(path: Path, name: str) -> tuple[bool, str | None]:
    """Reject obsolete unbounded signed-distance inputs before evaluation."""
    if not path.is_file():
        return False, f"{name}: PHI input is missing"
    try:
        with rasterio.open(path) as src:
            if (
                src.shape != EXPECTED_SHAPE
                or not src.transform.almost_equals(EXPECTED_TRANSFORM)
                or str(src.crs) != EXPECTED_CRS
            ):
                return False, f"{name}: PHI grid geometry is invalid"
            phi = src.read(1, masked=True).filled(np.nan).astype(float)
    except (OSError, rasterio.errors.RasterioError) as exc:
        return False, f"{name}: PHI cannot be read ({exc})"
    border = np.concatenate((phi[0, :], phi[-1, :], phi[:, 0], phi[:, -1]))
    expected_negative = EXPECTED_NEGATIVE_CELLS.get(name, 2000)
    valid = bool(
        np.all(np.isfinite(phi))
        and np.min(phi) >= -1.0001
        and np.max(phi) <= 1.0001
        and np.count_nonzero(phi < 0.0) == expected_negative
        and np.any((phi > -0.9999) & (phi < 0.0))
        and np.any((phi > 0.0) & (phi < 0.9999))
        and not np.any(np.isclose(phi, 0.0, atol=1.0e-7))
        and np.all(np.isfinite(border))
        and np.all(border > 0.0)
    )
    return valid, None if valid else f"{name}: PHI does not satisfy the current front contract"


def normalized_cross_front_variation(
    a: np.ndarray, transform: object, diagonal: bool = False
) -> float:
    x, y = coordinates(a.shape, transform)
    samples: list[np.ndarray] = []
    if diagonal:
        # x+y is constant on np.diag(a, k) for a north-up raster.  Restrict
        # each station along the finite front's tangent to exclude its tips.
        tangent = (x - y) / np.sqrt(2.0)
        normal = (x + y) / np.sqrt(2.0) + 300.0
        for station in np.linspace(25.0, 275.0, 9):
            mask = (
                (np.abs(normal - station) <= 2.0)
                & (np.abs(tangent) <= CENTRAL_HALF_WIDTH_M)
            )
            samples.append(a[mask])
    else:
        for station in np.linspace(
            UPSTREAM_STATION_X_M, DOWNSTREAM_STATION_X_M, 6
        ):
            column = int(np.argmin(np.abs(x[0, :] - station)))
            samples.append(a[np.abs(y[:, column]) <= CENTRAL_HALF_WIDTH_M, column])
    spreads = []
    for values in samples:
        finite = values[np.isfinite(values) & (values > 0.0)]
        if values.size == 0 or finite.size / values.size < MIN_STATION_COVERAGE:
            return float("nan")
        spreads.append(np.std(finite) / max(np.mean(finite), 1.0))
    # Every predeclared station must be adequately populated; the maximum is
    # the conservative aggregate and cannot hide one poorly resolved front.
    return float(np.max(spreads)) if spreads else float("nan")


def station_time(a: np.ndarray, transform: object, x_m: float) -> float:
    x, y = coordinates(a.shape, transform)
    column = int(np.argmin(np.abs(x[0, :] - x_m)))
    values = a[np.abs(y[:, column]) <= CENTRAL_HALF_WIDTH_M, column]
    finite = values[np.isfinite(values) & (values > 0.0)]
    if values.size == 0 or finite.size / values.size < MIN_STATION_COVERAGE:
        return float("nan")
    return float(np.median(finite))


def station_burned_fraction(
    a: np.ndarray,
    transform: object,
    x_m: float,
    half_width_m: float = CENTRAL_HALF_WIDTH_M,
) -> float:
    x, y = coordinates(a.shape, transform)
    column = int(np.argmin(np.abs(x[0, :] - x_m)))
    values = a[np.abs(y[:, column]) <= half_width_m, column]
    return float(np.isfinite(values).mean()) if values.size else float("nan")


def metric(
    name: str,
    expected: str,
    value: float,
    tolerance: str,
    passed: bool,
    units: str = "",
) -> dict:
    evaluable = np.isfinite(value)
    return {
        "name": name,
        "expected": expected,
        "calculated": None if not np.isfinite(value) else value,
        "units": units,
        "tolerance": tolerance,
        "status": "NOT EVALUABLE" if not evaluable else ("PASS" if passed else "FAIL"),
    }


def main() -> None:
    resolved = {name: path_for(name) for name in NAMES}
    paths = {name: item[0] for name, item in resolved.items()}
    reasons = [item[1] for item in resolved.values() if item[1] is not None]
    for name in NAMES:
        valid, reason = front_contract(
            CASE_DIR / f"variants/{name}/inputs/phi.tif", name
        )
        if not valid and reason is not None:
            reasons.append(reason + "; rerun the complete case")
    if reasons:
        payload = {
            "case_id": "CASE31_PFT",
            "overall_status": "NOT EVALUABLE",
            "verification_passed": False,
            "required_outputs_complete": False,
            "variants": NAMES,
            "metrics": [],
            "reason": "; ".join(reasons),
        }
        (OUT / "metrics.json").write_text(
            json.dumps(payload, indent=2, allow_nan=False) + "\n"
        )
        return
    try:
        loaded = {
            name: read(
                path,
                CASE_DIR / f"variants/{name}/inputs/phi.tif",
            )
            for name, path in paths.items()
            if path is not None
        }
    except (OSError, ValueError, rasterio.errors.RasterioError) as exc:
        payload = {
            "case_id": "CASE31_PFT",
            "overall_status": "NOT EVALUABLE",
            "verification_passed": False,
            "required_outputs_complete": False,
            "variants": NAMES,
            "metrics": [],
            "reason": f"Invalid required TOA evidence: {exc}",
        }
        (OUT / "metrics.json").write_text(
            json.dumps(payload, indent=2, allow_nan=False) + "\n"
        )
        return
    arrays = {name: item[0] for name, item in loaded.items()}
    transforms = {name: item[1] for name, item in loaded.items()}
    diagonal_variation = normalized_cross_front_variation(
        arrays["diagonal"], transforms["diagonal"], diagonal=True
    )
    jump_variation = normalized_cross_front_variation(
        arrays["fuel_jump"], transforms["fuel_jump"]
    )
    t_left = station_time(
        arrays["fuel_jump"], transforms["fuel_jump"], UPSTREAM_STATION_X_M
    )
    t_right = station_time(
        arrays["fuel_jump"], transforms["fuel_jump"], DOWNSTREAM_STATION_X_M
    )
    t_right_wind = station_time(
        arrays["fuel_jump_wind"],
        transforms["fuel_jump_wind"],
        DOWNSTREAM_STATION_X_M,
    )
    wind_ratio = (
        t_right_wind / t_right if np.isfinite(t_right) and t_right > 0 else float("nan")
    )
    continuous_upstream = station_burned_fraction(
        arrays["continuous_break"],
        transforms["continuous_break"],
        UPSTREAM_STATION_X_M,
    )
    continuous_at_barrier = station_burned_fraction(
        arrays["continuous_break"],
        transforms["continuous_break"],
        BARRIER_APPROACH_STATION_X_M,
    )
    gapped_upstream = station_burned_fraction(
        arrays["gapped_break"],
        transforms["gapped_break"],
        UPSTREAM_STATION_X_M,
    )
    gapped_opening = station_burned_fraction(
        arrays["gapped_break"],
        transforms["gapped_break"],
        OPENING_STATION_X_M,
        OPENING_HALF_WIDTH_M,
    )
    continuous = station_burned_fraction(
        arrays["continuous_break"],
        transforms["continuous_break"],
        DOWNSTREAM_STATION_X_M,
    )
    gapped = station_burned_fraction(
        arrays["gapped_break"],
        transforms["gapped_break"],
        DOWNSTREAM_STATION_X_M,
    )
    gap_gain = (
        float(gapped - continuous)
        if continuous_upstream >= MIN_STATION_COVERAGE
        and continuous_at_barrier >= MIN_STATION_COVERAGE
        and gapped_upstream >= MIN_STATION_COVERAGE
        and gapped_opening >= MIN_STATION_COVERAGE
        else float("nan")
    )
    transition_delay = t_right - t_left
    metrics = [
        metric(
            "diagonal-front cross-front variation",
            "<= 0.03",
            diagonal_variation,
            "3% coefficient of variation",
            diagonal_variation <= 0.03,
        ),
        metric(
            "fuel-jump front planarity",
            "<= 0.03",
            jump_variation,
            "3% coefficient of variation",
            jump_variation <= 0.03,
        ),
        metric(
            "fuel-transition arrival-time increment",
            "> 0",
            transition_delay,
            "positive downstream travel time",
            transition_delay > 0,
            "s",
        ),
        metric(
            "eastward-wind arrival-time ratio",
            "<= 0.95",
            wind_ratio,
            "at least 5% earlier than no-wind",
            wind_ratio <= 0.95,
        ),
        metric(
            "gapped-break downstream burned-fraction gain",
            ">= 0.10",
            gap_gain,
            "10 percentage points",
            gap_gain >= 0.10,
        ),
    ]
    complete = all(row["status"] != "NOT EVALUABLE" for row in metrics)
    passed = complete and all(row["status"] == "PASS" for row in metrics)
    payload = {
        "case_id": "CASE31_PFT",
        "overall_status": "PASS" if passed else ("FAIL" if complete else "NOT EVALUABLE"),
        "verification_passed": passed,
        "required_outputs_complete": complete,
        "variants": NAMES,
        "metrics": metrics,
        "source_files": [str(p.relative_to(CASE_DIR)) for p in paths.values()],
    }
    if not complete:
        payload["reason"] = "Unavailable calculated metrics: " + ", ".join(
            row["name"] for row in metrics if row["status"] == "NOT EVALUABLE"
        )
    (OUT / "metrics.json").write_text(
        json.dumps(payload, indent=2, allow_nan=False) + "\n"
    )

    # Figures are supplementary.  Persist the authoritative decision before
    # plotting so a rendering failure cannot leave a stale prior result.
    fig, axes = plt.subplots(2, 3, figsize=(12, 7.5))
    axes.flat[-1].axis("off")
    for ax, name in zip(axes.flat, NAMES):
        im = ax.imshow(arrays[name], origin="upper", cmap="inferno")
        ax.set_title(name.replace("_", " "))
        fig.colorbar(im, ax=ax, shrink=0.72, label="arrival time (s)")
    fig.tight_layout()
    polish_figure(fig)
    fig.savefig(FIG / "verification_summary.pdf", bbox_inches="tight")
    plt.close(fig)
    with rasterio.open(
        CASE_DIR / "variants/gapped_break/inputs/fbfm40.tif"
    ) as src:
        fuels = src.read(1)
    fig, ax = plt.subplots(figsize=(6, 5))
    im = ax.imshow(fuels, cmap="tab20", origin="upper")
    ax.set_title("Gapped nonburnable fuel break")
    fig.colorbar(im, ax=ax, label="FBFM code")
    fig.tight_layout()
    polish_figure(fig)
    fig.savefig(FIG / "input_configuration.pdf", bbox_inches="tight")
    plt.close(fig)
    print(f"[OK] CASE31_PFT: {payload['overall_status']}")


if __name__ == "__main__":
    main()
