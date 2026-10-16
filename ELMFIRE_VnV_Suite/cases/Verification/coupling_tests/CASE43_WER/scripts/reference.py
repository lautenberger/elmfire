#!/usr/bin/env python3
"""Independent intended-model oracle for the downscaled WU-E ellipse/heat kernel.

This module contains no imports from ELMFIRE or another V&V case.  Constants,
The ellipse regression retains the source nomenclature and exact branch rules.
Heat transfer follows the intended Section-1 formulation reviewed for this case.
"""
from __future__ import annotations

import math
from typing import Mapping

import numpy as np


WIND_MPH_TO_MPS = 0.447
RADIATION_DISTANCE_M = 100.0
RADIATION_FRACTION = 0.35
DFC_FRACTION = 0.65


def combustion_efficiency(wind_speed_mph: float) -> float:
    return 3.25 / (1.0 + math.exp(-0.05 * (wind_speed_mph - 75.0))) + 0.5


def hrr_transient(time_s: float) -> float:
    """Return the CASE43 design-fire HRRPUA in kW/m2."""
    if time_s <= 1.0:
        return max(0.0, 100.0 * time_s)
    if time_s <= 100.0:
        return 100.0
    if time_s > 110.0:
        return 0.0
    return max(0.0, 100.0 / (100.0 - 110.0) * (time_s - 110.0))


def ellipse_ucb(
    wind_speed_mph: float,
    hamada_a_m: float,
    hamada_d_m: float,
    wind_proportion: float = 1.0,
) -> dict[str, float | str]:
    """Evaluate ELLIPSE_UCB, including exact 10/17.3 m/s branch rules."""
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
    eb2 = 1.0 - (eccentricity / major) ** 2 if major != 0.0 else -1.0
    minor = sidewind / math.sqrt(eb2) if eb2 > 0.0 else 0.0
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


def heat_maps(
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
    target_nonburnable_fraction: float = 0.0,
    target_absorptivity: float = 0.8,
    hrr_ellipse_adj: float = 1.0,
    source_fuel_factor: float = 1.0,
    source_is_vegetation: bool = False,
) -> tuple[np.ndarray, np.ndarray]:
    """Return complete raster-order DFC/radiation fields for one source."""
    ny, nx = shape
    ellipse = ellipse_ucb(wind_speed_mph, hamada_a_m, hamada_d_m, source_fuel_factor)
    major = float(ellipse["ellipse_major_m"])
    minor = float(ellipse["ellipse_minor_m"])
    eccentricity = float(ellipse["ellipse_eccentricity_m"])
    dist_downwind = float(ellipse["dist_downwind_m"])
    if major == 0.0 or minor == 0.0 or hrr_ellipse_adj == 0.0:
        raise ValueError("The configured ellipse must have nonzero axes and adjustment")

    dfc = np.zeros(shape, dtype=np.float64)
    radiation = np.zeros(shape, dtype=np.float64)
    half_cell = 0.5 * cell_size_m
    source_area = cell_size_m**2 if source_is_vegetation else hamada_a_m**2
    hrr_adjuster = source_area / (
        math.pi * (hrr_ellipse_adj * major) * (hrr_ellipse_adj * minor)
    )
    wind_theta = math.radians(270.0 - wind_direction_deg)
    dfc_coefficient = 1.0 - target_nonburnable_fraction
    eta = combustion_efficiency(wind_speed_mph)
    beta_ssd = 1.0 if source_is_vegetation else min(1.0, (hamada_d_m + hamada_a_m) / cell_size_m)
    fine_grid_correction = 1.0 if source_is_vegetation else min((cell_size_m / hamada_a_m) ** 2, 1.0)

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
            dfc[row, col] = (
                fine_grid_correction
                * eta
                * DFC_FRACTION
                * dfc_coefficient
                * dfc_factor
                * hrrpua_kw_m2
                * hrr_adjuster
            )

            effective_distance = radius_m * beta_ssd * hrr_ellipse_adj
            radiation_checker = (RADIATION_DISTANCE_M + half_cell - effective_distance) / cell_size_m
            delta_radiation = min(1.0, max(0.0, radiation_checker))
            radiation_factor = delta_radiation * (1.0 - dfc_factor)
            if radiation_factor == 0.0:
                continue
            radiation[row, col] = fine_grid_correction * (
                eta
                * RADIATION_FRACTION
                * dfc_coefficient
                * target_absorptivity
                * radiation_factor
                * hrrpua_kw_m2
                * source_area
            ) / (4.0 * math.pi * effective_distance**2)
    return dfc, radiation


def variant_heat_maps(variant: Mapping[str, object], shape: tuple[int, int]) -> tuple[np.ndarray, np.ndarray]:
    """Convenience wrapper for one generated CASE43 variant."""
    return heat_maps(
        shape,
        int(variant["source_row"]),
        int(variant["source_col"]),
        cell_size_m=float(variant["cell_size_m"]),
        band_cells=int(variant["band_cells"]),
        wind_direction_deg=float(variant["wind_direction_deg"]),
        wind_speed_mph=float(variant["wind_speed_mph"]),
        hamada_a_m=float(variant["hamada_a_m"]),
        hamada_d_m=float(variant["hamada_d_m"]),
        hrrpua_kw_m2=float(variant["peak_hrrpua_kw_m2"]),
        source_fuel_factor=float(variant.get("source_fuel_factor", 1.0)),
        source_is_vegetation=bool(variant.get("source_is_vegetation", False)),
    )
