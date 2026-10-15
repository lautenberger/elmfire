#!/usr/bin/env python3
"""Independent intended-model oracle for CASE44 WU-E heat response.

The HRR timing uses the source's absolute transition-time nomenclature.  The
heat kernel implements the reviewed downscaled Section-1 model and has no
runtime dependency on an ELMFIRE checkout or another verification case.
"""
from __future__ import annotations

import math
from typing import Mapping, Sequence

import numpy as np


WIND_MPH_TO_MPS = 0.447
RADIATION_DISTANCE_M = 100.0
RADIATION_FRACTION = 0.35
DFC_FRACTION = 0.65


def combustion_efficiency(wind_speed_mph: float) -> float:
    return 3.25 / (1.0 + math.exp(-0.05 * (wind_speed_mph - 75.0))) + 0.5


def hrr_transient(time_s: float, curve: Mapping[str, float]) -> float:
    """Evaluate the implementation's absolute-time HRRPUA breakpoints."""
    early = float(curve["t_early_s"])
    full = float(curve["t_full_developed_s"])
    decay = float(curve["t_decay_s"])
    peak = float(curve["peak_hrrpua_kw_m2"])
    if time_s <= early:
        return max(0.0, peak / early * time_s)
    if time_s <= full:
        return peak
    if time_s > decay:
        return 0.0
    return max(0.0, peak / (full - decay) * (time_s - decay))


def ellipse_ucb(
    wind_speed_mph: float,
    hamada_a_m: float,
    hamada_d_m: float,
    wind_proportion: float = 1.0,
) -> dict[str, float | str]:
    """Evaluate the exact model-2 regression and ellipse construction."""
    speed_mps = wind_speed_mph * WIND_MPH_TO_MPS
    separation = min(hamada_d_m, 50.0)
    if speed_mps < 10.0:
        branch = "low"
        d1 = 1.679463256 - 0.123901243 * hamada_a_m + 0.307612446 * separation
        d2 = 78.62957398 + 1.536189561 * hamada_a_m - 0.5662073 * separation
        s1 = -2.922896622 - 0.05550541 * hamada_a_m + 0.017291361 * separation
        s2 = 39.31478699 + 0.768094781 * hamada_a_m - 0.28310365 * separation
        u1 = -6.297892493 - 0.119654483 * hamada_a_m + 0.037754535 * separation
        u2 = 78.62957398 + 1.536189561 * hamada_a_m - 0.5662073 * separation
        downwind = wind_proportion * (d1 * speed_mps + d2)
        upwind = wind_proportion * (u1 * speed_mps + u2)
        sidewind = wind_proportion * (s1 * speed_mps + s2)
    elif speed_mps > 17.3:
        branch = "high"
        d1 = -7.159031537 - 0.043555289 * hamada_a_m - 0.14894238 * separation
        d2 = 394.4930697 + 0.720929023 * hamada_a_m + 11.42149084 * separation
        s1 = -0.577270631 - 0.015285438 * hamada_a_m + 0.012786629 * separation
        s2 = 38.11784939 + 0.800599307 * hamada_a_m - 0.412476476 * separation
        u1 = -1.092711783 - 0.025390239 * hamada_a_m + 0.016740663 * separation
        u2 = 52.39584604 + 1.104793131 * hamada_a_m - 0.57241037 * separation
        downwind = wind_proportion * (d1 * speed_mps + d2)
        upwind = wind_proportion * (u1 * speed_mps + u2)
        sidewind = wind_proportion * (s1 * speed_mps + s2)
    else:
        branch = "middle"
        d1 = 4.099488028 - 0.000767118 * hamada_a_m + 0.134372426 * separation
        d2 = -94.26651508 - 0.000694022 * hamada_a_m - 3.053034015 * separation
        d3 = 615.192675 + 0.300438559 * hamada_a_m + 19.34120221 * separation
        s1 = 0.437844987 + 0.008280661 * hamada_a_m - 0.002833081 * separation
        s2 = -10.13978982 - 0.192922421 * hamada_a_m + 0.067862023 * separation
        s3 = 66.32382799 + 1.282260348 * hamada_a_m - 0.484673257 * separation
        u1 = 0.525004045 + 0.01046073 * hamada_a_m - 0.004473105 * separation
        u2 = -12.4091466 - 0.249326233 * hamada_a_m + 0.109759448 * separation
        u3 = 84.64808209 + 1.727651884 * hamada_a_m - 0.801945211 * separation
        downwind = wind_proportion * (d1 * speed_mps**2 + d2 * speed_mps + d3)
        upwind = wind_proportion * (u1 * speed_mps**2 + u2 * speed_mps + u3)
        sidewind = wind_proportion * (s1 * speed_mps**2 + s2 * speed_mps + s3)

    major = 0.5 * (downwind + upwind)
    eccentricity = min(0.5 * major, major - upwind)
    eccentricity_term = 1.0 - (eccentricity / major) ** 2 if major != 0.0 else -1.0
    minor = sidewind / math.sqrt(eccentricity_term) if eccentricity_term > 0.0 else 0.0
    return {
        "branch": branch,
        "wind_speed_mps": speed_mps,
        "effective_d_m": separation,
        "dist_downwind_m": downwind,
        "dist_upwind_m": upwind,
        "dist_sidewind_m": sidewind,
        "ellipse_major_m": major,
        "ellipse_minor_m": minor,
        "ellipse_eccentricity_m": eccentricity,
    }


def single_source_heat_maps(
    shape: tuple[int, int],
    source_row: int,
    source_col: int,
    *,
    cell_size_m: float,
    band_cells: int,
    wind_direction_deg: float,
    wind_speed_mph: float,
    hamada_a_m: float,
    hamada_d_m: float,
    hrrpua_kw_m2: float,
    target_nonburnable_fraction: np.ndarray,
    target_absorptivity: np.ndarray,
    hrr_ellipse_adj: float,
) -> tuple[np.ndarray, np.ndarray]:
    """Evaluate the complete raster-order DFC and radiation fields."""
    ny, nx = shape
    if target_nonburnable_fraction.shape != shape or target_absorptivity.shape != shape:
        raise ValueError("target coefficient arrays must match the heat-map shape")
    ellipse = ellipse_ucb(wind_speed_mph, hamada_a_m, hamada_d_m)
    major = float(ellipse["ellipse_major_m"])
    minor = float(ellipse["ellipse_minor_m"])
    eccentricity = float(ellipse["ellipse_eccentricity_m"])
    downwind = float(ellipse["dist_downwind_m"])
    if major == 0.0 or minor == 0.0 or hrr_ellipse_adj == 0.0:
        raise ValueError("ellipse axes and HRR_ELLIPSE_ADJ must be nonzero")

    dfc = np.zeros(shape, dtype=np.float64)
    radiation = np.zeros(shape, dtype=np.float64)
    half_cell = 0.5 * cell_size_m
    source_area = hamada_a_m**2
    hrr_adjuster = source_area / (
        math.pi * (hrr_ellipse_adj * major) * (hrr_ellipse_adj * minor)
    )
    wind_theta = math.radians(270.0 - wind_direction_deg)
    eta = combustion_efficiency(wind_speed_mph)
    beta_ssd = min(1.0, (hamada_d_m + hamada_a_m) / cell_size_m)
    fine_grid_correction = min((cell_size_m / hamada_a_m) ** 2, 1.0)

    row_start = max(2, source_row - band_cells)
    row_stop = min(ny - 3, source_row + band_cells)
    col_start = max(2, source_col - band_cells)
    col_stop = min(nx - 3, source_col + band_cells)
    for row in range(row_start, row_stop + 1):
        for col in range(col_start, col_stop + 1):
            delta_x = col - source_col
            delta_y = source_row - row
            radius_cells = math.hypot(delta_x, delta_y)
            if radius_cells < 1.0e-3:
                continue
            radius_m = radius_cells * cell_size_m
            target_theta = math.atan2(delta_y, delta_x)
            cosine = math.cos(target_theta - wind_theta)
            if cosine > 0.7:
                ellipse_distance = major + eccentricity
            elif cosine < -0.7:
                ellipse_distance = major - eccentricity
            else:
                ellipse_distance = 2.0 * minor
            dfc_checker = (ellipse_distance + half_cell - radius_m * beta_ssd) / cell_size_m
            dfc_factor = min(1.0, max(0.0, dfc_checker))
            combustible = 1.0 - float(target_nonburnable_fraction[row, col])
            dfc[row, col] = fine_grid_correction * eta * DFC_FRACTION * combustible * dfc_factor * hrrpua_kw_m2 * hrr_adjuster

            effective_distance = radius_m * beta_ssd * hrr_ellipse_adj
            radiation_checker = (RADIATION_DISTANCE_M + half_cell - effective_distance) / cell_size_m
            delta_radiation = min(1.0, max(0.0, radiation_checker))
            radiation_factor = delta_radiation * (1.0 - dfc_factor)
            if radiation_factor == 0.0:
                continue
            radiation[row, col] = fine_grid_correction * (
                eta
                * RADIATION_FRACTION
                * combustible
                * float(target_absorptivity[row, col])
                * radiation_factor
                * hrrpua_kw_m2
                * source_area
            ) / (4.0 * math.pi * effective_distance**2)
    return dfc, radiation


def combined_heat_maps(
    variant: Mapping[str, object],
    shape: tuple[int, int],
    time_s: float,
    curves: Mapping[str, Mapping[str, float]],
    target_nonburnable_fraction: np.ndarray,
    target_absorptivity: np.ndarray,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Sum source contributions and return HRR, DFC and radiation maps."""
    hrr = np.zeros(shape, dtype=np.float64)
    dfc = np.zeros(shape, dtype=np.float64)
    radiation = np.zeros(shape, dtype=np.float64)
    sources = variant["sources"]
    if not isinstance(sources, Sequence):
        raise TypeError("variant sources must be a sequence")
    for source in sources:
        if not isinstance(source, Mapping):
            raise TypeError("each source must be a mapping")
        row = int(source["row"])
        col = int(source["col"])
        release = hrr_transient(time_s, curves[str(source["curve"] )])
        hrr[row, col] += release
        source_dfc, source_rad = single_source_heat_maps(
            shape,
            row,
            col,
            cell_size_m=float(variant["cell_size_m"]),
            band_cells=int(variant["band_cells"]),
            wind_direction_deg=float(variant["wind_direction_deg"]),
            wind_speed_mph=float(variant["wind_speed_mph"]),
            hamada_a_m=float(variant["hamada_a_m"]),
            hamada_d_m=float(variant["hamada_d_m"]),
            hrrpua_kw_m2=release,
            target_nonburnable_fraction=target_nonburnable_fraction,
            target_absorptivity=target_absorptivity,
            hrr_ellipse_adj=float(variant["hrr_ellipse_adj"]),
        )
        dfc += source_dfc
        radiation += source_rad
    return hrr, dfc, radiation
