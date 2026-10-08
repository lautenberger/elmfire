#!/usr/bin/env python3
"""Generate deterministic CASE45 WU-E heat-to-ROS variants.

The generator is intentionally source-checkout independent.  The reviewed
source revision and equations are recorded in the manifest, but runtime never
opens an ELMFIRE source file.  ``variants`` is wholly disposable.
"""
from __future__ import annotations

from report_language import polish_figure

import json
import math
import shutil
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
from matplotlib.colors import ListedColormap
from rasterio.transform import from_origin

from fingerprint import aggregate_fingerprint, sha256_file, variant_fingerprint

CASE_DIR = Path(__file__).resolve().parents[1]
CASE_ID = "CASE45_HRS"
REQUIRED_VARIANT_COUNT = 12
SOURCE_COMMIT = "a2dfbcdf72209733c000e5d3431e44723e15efea"
SIZE = 25
DX_M = 20.0
TRANSFORM = from_origin(-250.0, 250.0, DX_M, DX_M)
CRS = "EPSG:32610"
NODATA = -9999.0
SOURCE_RC = (12, 12)
RECEIVER_PHI = 0.001
PHI_NOISE_BOUND = 0.0005  # Source I/O adds 1e-3 * (0.5 - RX), even with wind noise off.
SOURCE_PHI = -0.45
GUARD_RCS = ((9, 9), (9, 15), (15, 9), (15, 15))
AREA_M = 20.0
SEPARATION_M = 10.0
DT_S = 1.0
TARGET_CFL = 0.10
HRR_ELLIPSE_ADJ = 0.5
EXPECTED_DUMP_RECORDS = [
    {"dump_index": index + 1, "time_seconds": float(index), "is_final_dump": index == 4}
    for index in range(5)
]
PDF_METADATA = {
    "Creator": CASE_ID,
    "Producer": "Matplotlib",
    "CreationDate": datetime(2026, 1, 1, tzinfo=timezone.utc),
    "ModDate": datetime(2026, 1, 1, tzinfo=timezone.utc),
}
SOURCE_REVIEW = {
    "checkout_state_at_design_review": "dirty outside the reviewed WU-E files",
    "reviewed_files_match_commit": True,
    "reviewed_file_sha256": {
        "build/source/elmfire_vars.f90": "5ad4a4fdd7e3faf98d839fb75af5b3c1178cadd335142ab960adec2462c7896c",
        "build/source/elmfire_init.f90": "06d2be089db961d1af387f4d1cfb60a6ab28f2daa50d7e4ada8bfda58cd87c0d",
        "build/source/elmfire_namelists.f90": "2fcd9e57a2bec5df964d96ccef0f658b703e0406abb19c13cb8080af066c78b4",
        "build/source/elmfire_spread_rate.f90": "eed515c2f44b5582c01b1573e41a7e24bf76f0a445870f27ba7d32d281e656a4",
        "build/source/elmfire_level_set.f90": "18c446fce24ac5fac8dd174e38e273bfdddecfa81c9371550b247055b6d833b3",
    },
    "known_modified_paths_outside_reviewed_files": [
        "build/source/elmfire.f90",
        "build/source/elmfire_io.f90",
        "build/source/elmfire_subs.f90",
    ],
    "untracked_entries_present_at_design_review": True,
    "untracked_paths_at_design_review": [
        ".vscode/",
        "ELMFIRE_VnV_Suite/",
        "Thomas_VAL/",
        "VERIFICATION_TROUBLESHOOT_DWI/",
        "notebooks/",
        "thomas_fire/",
        "verification/spotting-suite-legacy/",
        "verification/spotting-suite/",
    ],
    "runtime_source_inspection": False,
}


def ellipse_ucb(wind_mph: float) -> dict[str, float]:
    """Independent transcription of the reviewed UCB ellipse equations."""
    v = wind_mph * 0.447
    a0, d0 = AREA_M, min(SEPARATION_M, 50.0)
    if v < 10.0:
        d1 = 1.679463256 - 0.123901243 * a0 + 0.307612446 * d0
        d2 = 78.62957398 + 1.536189561 * a0 - 0.5662073 * d0
        s1 = -2.922896622 - 0.05550541 * a0 + 0.017291361 * d0
        s2 = 39.31478699 + 0.768094781 * a0 - 0.28310365 * d0
        u1 = -6.297892493 - 0.119654483 * a0 + 0.037754535 * d0
        u2 = d2
        down, side, up = d1 * v + d2, s1 * v + s2, u1 * v + u2
    elif v > 17.3:
        d1 = -7.159031537 - 0.043555289 * a0 - 0.14894238 * d0
        d2 = 394.4930697 + 0.720929023 * a0 + 11.42149084 * d0
        s1 = -0.577270631 - 0.015285438 * a0 + 0.012786629 * d0
        s2 = 38.11784939 + 0.800599307 * a0 - 0.412476476 * d0
        u1 = -1.092711783 - 0.025390239 * a0 + 0.016740663 * d0
        u2 = 52.39584604 + 1.104793131 * a0 - 0.57241037 * d0
        down, side, up = d1 * v + d2, s1 * v + s2, u1 * v + u2
    else:
        d1 = 4.099488028 - 0.000767118 * a0 + 0.134372426 * d0
        d2 = -94.26651508 - 0.000694022 * a0 - 3.053034015 * d0
        d3 = 615.192675 + 0.300438559 * a0 + 19.34120221 * d0
        s1 = 0.437844987 + 0.008280661 * a0 - 0.002833081 * d0
        s2 = -10.13978982 - 0.192922421 * a0 + 0.067862023 * d0
        s3 = 66.32382799 + 1.282260348 * a0 - 0.484673257 * d0
        u1 = 0.525004045 + 0.01046073 * a0 - 0.004473105 * d0
        u2 = -12.4091466 - 0.249326233 * a0 + 0.109759448 * d0
        u3 = 84.64808209 + 1.727651884 * a0 - 0.801945211 * d0
        down = d1 * v * v + d2 * v + d3
        side = s1 * v * v + s2 * v + s3
        up = u1 * v * v + u2 * v + u3
    major = 0.5 * (down + up)
    eccentricity = min(0.5 * major, major - up)
    eb2 = 1.0 - (eccentricity / major) ** 2
    minor = side / math.sqrt(eb2) if eb2 > 0.0 else 0.0
    if min(major, minor, down, up, side) <= 0.0:
        raise ValueError("selected ellipse regression produced a nonpositive distance")
    return {
        "major_m": major,
        "minor_m": minor,
        "eccentricity_m": eccentricity,
        "downwind_m": down,
        "upwind_m": up,
        "sidewind_m": side,
    }


def hrr_adjuster(wind_mph: float) -> float:
    ellipse = ellipse_ucb(wind_mph)
    return DX_M**2 / (
        math.pi
        * HRR_ELLIPSE_ADJ * ellipse["major_m"]
        * HRR_ELLIPSE_ADJ * ellipse["minor_m"]
    )


def heat_transfer_coefficient(wind_mph: float, normal: str) -> dict[str, float]:
    """Return receiver heat flux per unit source HRR for one-cell offset."""
    ellipse = ellipse_ucb(wind_mph)
    idx, idy = {"head": (1.0, 0.0), "back": (-1.0, 0.0), "side": (0.0, 1.0)}[normal]
    target_r_m = math.hypot(idx, idy) * DX_M
    target_theta = math.atan2(idy, idx)
    wind_theta = math.radians(270.0 - 270.0)
    relative_theta = target_theta - wind_theta
    major = ellipse["major_m"]
    minor = ellipse["minor_m"]
    eccentricity = ellipse["eccentricity_m"]
    max_factor = 0.3 * ellipse["downwind_m"] * (major - eccentricity) / minor**2
    ellipse_distance = max_factor * minor**2 / (major - eccentricity * math.cos(relative_theta))
    dfc_factor = min(1.0, max(0.0, (ellipse_distance + 0.5 * DX_M - target_r_m) / DX_M))
    rad_checker = (ellipse_distance + 100.0 + 0.5 * DX_M - target_r_m) / DX_M
    delta_rad = min(1.0, max(0.0, rad_checker))
    rad_factor = delta_rad * (1.0 - dfc_factor)
    rad_eff_m = (
        DX_M * (1.0 - dfc_factor)
        if 0.0 < dfc_factor < 1.0
        else target_r_m - ellipse_distance
    )
    adjuster = hrr_adjuster(wind_mph)
    dfc_per_hrr = dfc_factor * adjuster
    rad_per_hrr = (
        adjuster * 0.3 * 0.89 * rad_factor * DX_M**2 / (4.0 * math.pi * rad_eff_m**2)
        if rad_factor > 0.0 and abs(rad_eff_m) > 1.0e-12
        else 0.0
    )
    return {
        "hrr_adjuster": adjuster,
        "ellipse_distance_m": ellipse_distance,
        "dfc_factor": dfc_factor,
        "dfc_per_hrr": dfc_per_hrr,
        "radiation_per_hrr": rad_per_hrr,
        "total_heat_per_hrr": dfc_per_hrr + rad_per_hrr,
    }


def received_heat_real32(wind_mph: float, normal: str, hrr_peak: float) -> dict[str, float]:
    """Transcribe the reviewed default-REAL operation sequence in binary32."""
    f = np.float32
    v, area, separation = f(f(wind_mph) * f(0.447)), f(AREA_M), f(SEPARATION_M)
    if v < f(10.0):
        d1 = f(f(1.679463256) - f(0.123901243) * area + f(0.307612446) * separation)
        d2 = f(f(78.62957398) + f(1.536189561) * area - f(0.5662073) * separation)
        s1 = f(f(-2.922896622) - f(0.05550541) * area + f(0.017291361) * separation)
        s2 = f(f(39.31478699) + f(0.768094781) * area - f(0.28310365) * separation)
        u1 = f(f(-6.297892493) - f(0.119654483) * area + f(0.037754535) * separation)
        u2 = d2
        down = f(d1 * v + d2)
        side = f(s1 * v + s2)
        up = f(u1 * v + u2)
    elif v > f(17.3):
        d1 = f(f(-7.159031537) - f(0.043555289) * area - f(0.14894238) * separation)
        d2 = f(f(394.4930697) + f(0.720929023) * area + f(11.42149084) * separation)
        s1 = f(f(-0.577270631) - f(0.015285438) * area + f(0.012786629) * separation)
        s2 = f(f(38.11784939) + f(0.800599307) * area - f(0.412476476) * separation)
        u1 = f(f(-1.092711783) - f(0.025390239) * area + f(0.016740663) * separation)
        u2 = f(f(52.39584604) + f(1.104793131) * area - f(0.57241037) * separation)
        down = f(d1 * v + d2)
        side = f(s1 * v + s2)
        up = f(u1 * v + u2)
    else:
        d1 = f(f(4.099488028) - f(0.000767118) * area + f(0.134372426) * separation)
        d2 = f(f(-94.26651508) - f(0.000694022) * area - f(3.053034015) * separation)
        d3 = f(f(615.192675) + f(0.300438559) * area + f(19.34120221) * separation)
        s1 = f(f(0.437844987) + f(0.008280661) * area - f(0.002833081) * separation)
        s2 = f(f(-10.13978982) - f(0.192922421) * area + f(0.067862023) * separation)
        s3 = f(f(66.32382799) + f(1.282260348) * area - f(0.484673257) * separation)
        u1 = f(f(0.525004045) + f(0.01046073) * area - f(0.004473105) * separation)
        u2 = f(f(-12.4091466) - f(0.249326233) * area + f(0.109759448) * separation)
        u3 = f(f(84.64808209) + f(1.727651884) * area - f(0.801945211) * separation)
        v2 = f(v * v)
        down = f(d1 * v2 + d2 * v + d3)
        side = f(s1 * v2 + s2 * v + s3)
        up = f(u1 * v2 + u2 * v + u3)
    major = f(f(down + up) / f(2.0))
    eccentricity = min(f(major / f(2.0)), f(major - up))
    minor_squared_factor = f(f(1.0) - f(f(eccentricity / major) ** f(2.0)))
    minor = f(side / f(np.sqrt(minor_squared_factor)))

    dx, pi, eta = f(DX_M), f(math.pi), f(HRR_ELLIPSE_ADJ)
    area_cell = f(dx * dx)
    adjuster = f(area_cell / f(f(pi * f(eta * major)) * f(eta * minor)))
    idx, idy = {
        "head": (f(1.0), f(0.0)),
        "back": (f(-1.0), f(0.0)),
        "side": (f(0.0), f(1.0)),
    }[normal]
    target_r = f(f(np.sqrt(f(idx * idx + idy * idy))) * dx)
    target_theta = f(np.arctan2(idy, idx))
    wind_theta = f(f(pi / f(180.0)) * f(f(270.0) - f(270.0)))
    target_theta_f = f(target_theta - wind_theta)
    minor_squared = f(minor * minor)
    max_distance = f(f(f(f(0.3) * down) * f(major - eccentricity)) / minor_squared)
    ellipse_distance = f(
        f(max_distance * minor_squared)
        / f(major - f(eccentricity * f(np.cos(target_theta_f))))
    )
    reciprocal_dx, half_dx = f(f(1.0) / dx), f(f(0.5) * dx)
    dfc_checker = f(reciprocal_dx * f(f(ellipse_distance + half_dx) - target_r))
    dfc_factor = f(max(f(0.0), min(f(1.0), dfc_checker)))
    rad_checker = f(
        reciprocal_dx * f(f(f(ellipse_distance + f(100.0)) + half_dx) - target_r)
    )
    delta_rad = f(max(f(0.0), min(f(1.0), rad_checker)))
    rad_factor = f(delta_rad - f(delta_rad * dfc_factor))
    rad_eff = (
        f(dx - f(dfc_factor * dx))
        if f(0.0) < dfc_factor < f(1.0)
        else f(target_r - ellipse_distance)
    )
    hrr = f(hrr_peak)
    dfc_heat = f(f(f(f(1.0) * dfc_factor) * hrr) * adjuster)
    rad_numerator = f(
        f(f(f(f(f(0.3) * f(1.0)) * f(0.89)) * rad_factor) * hrr) * area_cell
    )
    rad_denominator = f(f(f(f(4.0) * pi) * rad_eff) * rad_eff)
    rad_heat = f(adjuster * f(rad_numerator / rad_denominator))
    total_heat = f(dfc_heat + rad_heat)
    energy = f(f(total_heat * f(DT_S)) * area_cell)
    return {
        "ellipse_major_m": float(major),
        "ellipse_minor_m": float(minor),
        "ellipse_eccentricity_m": float(eccentricity),
        "ellipse_downwind_m": float(down),
        "ellipse_upwind_m": float(up),
        "ellipse_sidewind_m": float(side),
        "hrr_adjuster": float(adjuster),
        "ellipse_distance_m": float(ellipse_distance),
        "dfc_factor": float(dfc_factor),
        "radiation_factor": float(rad_factor),
        "dfc_heat_kw_m2": float(dfc_heat),
        "radiation_heat_kw_m2": float(rad_heat),
        "total_heat_kw_m2": float(total_heat),
        "step_energy_kj": float(energy),
    }


def tune_peak_real32(
    target_energy_kj: float, wind_mph: float, normal: str, *, require_exact: bool = False
) -> tuple[float, dict[str, float]]:
    """Choose a binary32 table value nearest the requested realized energy."""
    coefficient = heat_transfer_coefficient(wind_mph, normal)["total_heat_per_hrr"]
    center = np.float32(target_energy_kj / (coefficient * DT_S * DX_M**2))
    candidates = {float(center)}
    lower, upper = center, center
    for _ in range(512):
        lower = np.nextafter(lower, np.float32(-np.inf))
        upper = np.nextafter(upper, np.float32(np.inf))
        candidates.add(float(lower))
        candidates.add(float(upper))
    evaluated = [
        (peak, received_heat_real32(wind_mph, normal, peak)) for peak in candidates
    ]
    exact = [
        pair for pair in evaluated if pair[1]["step_energy_kj"] == target_energy_kj
    ]
    if require_exact:
        if not exact:
            raise ValueError(
                f"no binary32 HRR amplitude realizes exactly {target_energy_kj} kJ"
            )
        return min(exact, key=lambda pair: abs(pair[0] - float(center)))
    return min(
        evaluated,
        key=lambda pair: (
            abs(pair[1]["step_energy_kj"] - target_energy_kj),
            abs(pair[0] - float(center)),
        ),
    )


def target_for(normal: str) -> tuple[int, int]:
    # Wind is from 270 degrees, so the DMS direction is grid east.
    return {
        "head": (SOURCE_RC[0], SOURCE_RC[1] + 1),
        "back": (SOURCE_RC[0], SOURCE_RC[1] - 1),
        "side": (SOURCE_RC[0] - 1, SOURCE_RC[1]),
    }[normal]


def ellipse_normal_velocity(
    energy_kj: float,
    ftp_crit_kj_m2: float,
    ellipse: dict[str, float],
    normal: str,
    *,
    normal_component: bool = False,
) -> float:
    """Predict speed magnitude or normal transport from the fixed-FTP law."""
    absolute_u = min(
        1.0e5,
        60.0 * energy_kj / (0.3048 * DT_S * DX_M * ftp_crit_kj_m2),
    )
    front = ellipse["major_m"] + ellipse["eccentricity_m"]
    side = 2.0 * ellipse["minor_m"]
    back = ellipse["major_m"] - ellipse["eccentricity_m"]
    total = max(1.0e-5, front + side + back)
    v_head = absolute_u * front / total
    v_back = absolute_u * back / total
    v_side = absolute_u * side / total
    low = min((v_head + v_back) / (2.0 * v_side), 10.0) if v_side > 1.0e-4 else 1.0
    nx, ny = {
        "head": (1.0, 0.0),
        "back": (-1.0, 0.0),
        "side": (0.0, 1.0),
    }[normal]
    cosang, sinang = nx, -ny
    aa = max(0.5 * (v_head + v_back), 1.0e-10)
    bb = 0.5 * max((v_head + v_back) / low, 1.0e-10)
    denom = max(math.sqrt(aa**2 * cosang**2 + bb**2 * sinang**2), 1.0e-10)
    dydt = aa**2 * cosang / denom + 0.5 * (v_head - v_back)
    dxdt = bb**2 * sinang / denom
    if normal_component:
        return dydt * nx - dxdt * ny
    return math.hypot(dydt, dxdt)


def prepared_limited_gradient_per_m() -> float:
    """Superbee gradient magnitude at the near-front receiver before crossing."""
    delta_up = RECEIVER_PHI - SOURCE_PHI
    delta_local = 1.0 - RECEIVER_PHI
    ratio = delta_up / delta_local
    half_superbee = max(0.0, max(min(0.5 * ratio, 1.0), min(ratio, 0.5)))
    reconstructed_ahead = RECEIVER_PHI + half_superbee * delta_local
    reconstructed_behind = SOURCE_PHI
    return (reconstructed_ahead - reconstructed_behind) / DX_M


def write_raster(path: Path, values: np.ndarray | float, dtype: str) -> None:
    data = values if isinstance(values, np.ndarray) else np.full((SIZE, SIZE), values)
    with rasterio.open(
        path,
        "w",
        driver="GTiff",
        width=SIZE,
        height=SIZE,
        count=1,
        dtype=dtype,
        crs=CRS,
        transform=TRANSFORM,
        nodata=NODATA,
        compress="deflate",
    ) as dst:
        dst.write(np.asarray(data, dtype=dtype), 1)


def variant_specs() -> list[dict[str, object]]:
    base_wind = 35.0
    base_peak, _ = tune_peak_real32(30100.0, base_wind, "head")
    raw = [
        ("energy_below", 34.0, "head", 29900.0, None, 3000.0),
        ("energy_equal", 34.0, "head", 30000.0, None, 3000.0),
        ("energy_above", 34.0, "head", 30100.0, None, 3000.0),
        ("wind_below_35", 34.9, "head", 30100.0, None, 3000.0),
        ("wind_equal_35", 35.0, "head", 30100.0, None, 3000.0),
        ("wind_above_35", 35.1, "head", 30100.0, None, 3000.0),
        ("normal_back", 35.0, "back", 30100.0, None, 3000.0),
        ("normal_side", 35.0, "side", 30100.0, None, 3000.0),
        ("hrr_half", 35.0, "head", None, 0.5 * base_peak, 3000.0),
        ("hrr_double", 35.0, "head", None, 2.0 * base_peak, 3000.0),
        ("ftp_low", 35.0, "head", 30100.0, None, 3000.0),
        ("ftp_high", 35.0, "head", 30100.0, None, 6000.0),
    ]
    result: list[dict[str, object]] = []
    for variant_id, wind, normal, requested_energy, explicit_peak, ftp in raw:
        ellipse = ellipse_ucb(wind)
        transfer = heat_transfer_coefficient(wind, normal)
        if explicit_peak is None:
            peak, realized = tune_peak_real32(
                float(requested_energy),
                wind,
                normal,
                require_exact=variant_id == "energy_equal",
            )
        else:
            peak = float(np.float32(explicit_peak))
            realized = received_heat_real32(wind, normal, peak)
        designed_energy = realized["step_energy_kj"]
        absolute_velocity_ft_min = min(
            1.0e5,
            60.0 * designed_energy / (0.3048 * DT_S * DX_M * ftp),
        )
        absolute_displacement_m = absolute_velocity_ft_min * 0.3048 / 60.0 * DT_S
        local_velocity_ft_min = ellipse_normal_velocity(
            designed_energy, ftp, ellipse, normal
        )
        local_displacement_m = local_velocity_ft_min * 0.3048 / 60.0 * DT_S
        normal_displacement_m = ellipse_normal_velocity(
            designed_energy, ftp, ellipse, normal, normal_component=True
        ) * 0.3048 / 60.0 * DT_S
        # Transport uses u dot grad(phi), not |u| |grad(phi)|. The side
        # receiver has a flat tangential stencil; allow for the I/O noise
        # in both directional gradients and in the initial receiver value.
        gradient_noise_bound = 4.0 * PHI_NOISE_BOUND / DX_M
        conservative_rk2_phi_decrement = 0.5 * (
            normal_displacement_m * (prepared_limited_gradient_per_m() - gradient_noise_bound)
            - absolute_displacement_m * gradient_noise_bound
        )
        receiver_phi_upper_bound = RECEIVER_PHI + PHI_NOISE_BOUND
        if conservative_rk2_phi_decrement <= receiver_phi_upper_bound:
            raise ValueError(
                f"{variant_id} does not guarantee first-heated-step receiver crossing: "
                f"{conservative_rk2_phi_decrement} <= {receiver_phi_upper_bound}"
            )
        if absolute_displacement_m > TARGET_CFL * DX_M + 1.0e-9:
            raise ValueError(
                f"{variant_id} violates the prepared TARGET_CFL bound: "
                f"{absolute_displacement_m} m > {TARGET_CFL * DX_M} m"
            )
        result.append(
            {
                "id": variant_id,
                "wind_mph": wind,
                "wind_from_degrees": 270.0,
                "normal_class": normal,
                "normal_xy": {"head": [1.0, 0.0], "back": [-1.0, 0.0], "side": [0.0, 1.0]}[normal],
                "source_row_col": list(SOURCE_RC),
                "receiver_row_col": list(target_for(normal)),
                "hrr_peak_kw_m2": peak,
                "requested_step_energy_kj": requested_energy,
                "designed_step_energy_kj": designed_energy,
                "binary32_input_design": realized,
                "ftp_crit_table_kj_m2": ftp,
                "ellipse": ellipse,
                "heat_transfer": transfer,
                "intended_ftp_crit_kj_m2": ftp,
                "source_deviation_diagnostics": {
                    "energy_above_30000_kj": designed_energy > 30000.0,
                    "wind_at_or_below_35_mph": wind <= 35.0,
                },
                "predicted_absolute_velocity_ft_min": absolute_velocity_ft_min,
                "predicted_absolute_displacement_per_step_m": absolute_displacement_m,
                "predicted_receiver_velocity_ft_min": local_velocity_ft_min,
                "predicted_receiver_displacement_per_step_m": local_displacement_m,
                "prepared_limited_gradient_per_m": prepared_limited_gradient_per_m(),
                "conservative_rk2_phi_decrement": conservative_rk2_phi_decrement,
                "receiver_phi_margin_factor": conservative_rk2_phi_decrement / receiver_phi_upper_bound,
                "normal_displacement_per_step_m": normal_displacement_m,
                "receiver_phi_upper_bound": receiver_phi_upper_bound,
                "target_cfl_displacement_limit_m": TARGET_CFL * DX_M,
            }
        )
    return result


def concrete_namelist(template: str, variant_id: str) -> str:
    replacements = {
        "@INPUT_DIR@": f"./variants/{variant_id}/inputs",
        "@OUTPUT_DIR@": f"./variants/{variant_id}/outputs",
        "@MISC_DIR@": f"./variants/{variant_id}/misc",
        "@SCRATCH_DIR@": f"./variants/{variant_id}/scratch",
    }
    for token, value in replacements.items():
        if template.count(token) == 0:
            raise ValueError(f"missing template token {token}")
        template = template.replace(token, value)
    if "@" in template:
        raise ValueError("unexpanded token remains in generated namelist")
    return template


def initial_metrics(count: int, aggregate: str | None) -> dict[str, object]:
    specifications = [
        ("recorded heat-to-ROS oracle relative error", "<= 0.005"),
        ("receiver LIST_BURNED observability", "TOA = 1 s and terminal VS is finite"),
        ("fixed-FTP continuity across 30000 kJ", "normalized speed is continuous within 0.5%"),
        ("wind continuity across 35 mph", "no artificial speed jump; oracle error <= 0.005"),
        ("HRR amplitude response", "double/half heat ratio = 4 +/- 0.04"),
        ("head/back/side response", "matched-wind maximum relative error <= 0.005"),
        ("FTP_CRIT inverse response", "speed ratio for 3000/6000 kJ m^-2 = 2 +/- 0.01"),
    ]
    return {
        "case_id": CASE_ID,
        "source_commit": SOURCE_COMMIT,
        "overall_status": "NOT EVALUABLE",
        "workflow_status": "NOT RUN",
        "verification_passed": False,
        "required_outputs_complete": False,
        "required_variant_count": count,
        "completed_variant_count": 0,
        "runtime_input_fingerprint_sha256": aggregate,
        "reason": "ELMFIRE was intentionally not run while the case was generated.",
        "metrics": [
            {"name": name, "expected": expected, "calculated": None, "units": "--", "status": "NOT EVALUABLE"}
            for name, expected in specifications
        ],
    }


def make_input_figure(specs: list[dict[str, object]]) -> None:
    """Separate whole-domain inputs from the twelve-variant design chart."""
    inputs = CASE_DIR / "variants/energy_equal/inputs"
    fig, axes = plt.subplots(1, 2, figsize=(7.2, 4.0), constrained_layout=True)
    for ax, name, title in zip(axes, ("fbfm40", "phi"), ("Fuel: 91 urban / 93 barrier", "Initial level set")):
        with rasterio.open(inputs / f"{name}.tif") as src:
            a = src.read(1, masked=True)
            extent = (src.bounds.left, src.bounds.right, src.bounds.bottom, src.bounds.top)
        if name == "fbfm40":
            im = ax.imshow(np.where(a == 91, 1, 0), extent=extent, origin="upper", cmap=ListedColormap(["#dadada", "#bf3f3f"]), vmin=0, vmax=1)
        else:
            im = ax.imshow(a, extent=extent, origin="upper", cmap="coolwarm", vmin=-1, vmax=1)
            fig.colorbar(im, ax=ax, label=r"$\phi$ (–)", shrink=0.75)
        ax.set(title=title, xlabel="Easting (m)", ylabel="Northing (m)")
    fig.suptitle("Prepared inputs: energy_equal")
    polish_figure(fig)
    fig.savefig(CASE_DIR / "figures/input_configuration.pdf", metadata=PDF_METADATA)
    plt.close(fig)
    fig, ax = plt.subplots(figsize=(7.2, 5.5), constrained_layout=True)
    y = np.arange(len(specs))
    ax.barh(y, [float(s["designed_step_energy_kj"]) / 1000 for s in specs])
    ax.axvline(30, ls="--", color="black", label="30 MJ (source-branch diagnostic)")
    ax.set_yticks(y, [s["id"].replace("_", " ") for s in specs])
    ax.invert_yaxis()
    ax.set(xlabel="Designed receiver energy per step (MJ)", title="Prescribed heat stimulus")
    ax.legend(loc="lower right")
    polish_figure(fig)
    fig.savefig(CASE_DIR / "figures/input_stimulus.pdf", metadata=PDF_METADATA)
    plt.close(fig)



def main() -> None:
    metrics_path = CASE_DIR / "outputs/metrics.json"
    metrics_path.parent.mkdir(parents=True, exist_ok=True)
    metrics_path.write_text(
        json.dumps(initial_metrics(REQUIRED_VARIANT_COUNT, None), indent=2) + "\n",
        encoding="utf-8",
    )
    for generated in (
        CASE_DIR / "figures/input_configuration.pdf",
        CASE_DIR / "figures/response_summary.pdf",
        CASE_DIR / "report/case_report.pdf",
        CASE_DIR / "report/metrics_macros.tex",
    ):
        generated.unlink(missing_ok=True)
    for pattern in ("*.stdout", "*.stderr"):
        for log_path in (CASE_DIR / "logs").glob(pattern):
            log_path.unlink()

    variants_dir = CASE_DIR / "variants"
    if variants_dir.exists():
        shutil.rmtree(variants_dir)
    variants_dir.mkdir(parents=True)
    template = (CASE_DIR / "elmfire.data.in").read_text(encoding="utf-8")
    fuel_table = (CASE_DIR / "data/misc/fuel_models.csv").read_text(encoding="utf-8")
    building_template = (
        CASE_DIR / "data/misc/building_fuel_models.csv"
    ).read_text(encoding="utf-8").strip().split(",")
    if len(building_template) != 16:
        raise ValueError("case-local building fuel table must contain exactly 16 fields")
    specs = variant_specs()
    if len(specs) != REQUIRED_VARIANT_COUNT or len({str(item["id"]) for item in specs}) != len(specs):
        raise ValueError("CASE45 variant list must contain 12 unique identifiers")
    fingerprints: dict[str, dict[str, object]] = {}
    for spec in specs:
        root = variants_dir / str(spec["id"])
        inputs, misc = root / "inputs", root / "misc"
        for directory in (inputs, misc, root / "outputs", root / "scratch"):
            directory.mkdir(parents=True, exist_ok=True)
        fbfm = np.full((SIZE, SIZE), 93, dtype=np.int16)
        phi = np.ones((SIZE, SIZE), dtype=np.float32)
        source = tuple(spec["source_row_col"])
        receiver = tuple(spec["receiver_row_col"])
        fbfm[source] = 91
        fbfm[receiver] = 91
        for guard in GUARD_RCS:
            fbfm[guard] = 91
        phi[source] = SOURCE_PHI
        phi[receiver] = RECEIVER_PHI
        if spec["normal_class"] == "side":
            # Suppress the artificial tangential local minimum whose upwind
            # gradient otherwise opposes the desired normal crossing.
            phi[receiver[0], receiver[1]-2:receiver[1]+3] = RECEIVER_PHI
        fields: dict[str, tuple[np.ndarray | float, str]] = {
            "fbfm40": (fbfm, "int16"),
            "phi": (phi, "float32"),
            "adj": (0.0, "float32"),
            "slp": (0.0, "float32"),
            "asp": (0.0, "float32"),
            "dem": (0.0, "float32"),
            "cc": (0.0, "float32"),
            "ch": (0.0, "float32"),
            "cbh": (0.0, "float32"),
            "cbd": (0.0, "float32"),
            "ws": (float(spec["wind_mph"]), "float32"),
            "wd": (270.0, "float32"),
            "m1": (5.0, "float32"),
            "m10": (7.0, "float32"),
            "m100": (9.0, "float32"),
        }
        for name, (values, dtype) in fields.items():
            write_raster(inputs / f"{name}.tif", values, dtype)
        (misc / "fuel_models.csv").write_text(fuel_table, encoding="utf-8")
        building_fields = building_template.copy()
        building_fields[7] = f"{float(spec['hrr_peak_kw_m2']):.12g}"
        building_fields[8] = f"{float(spec['ftp_crit_table_kj_m2']):.12g}"
        building_row = ",".join(building_fields) + "\n"
        (misc / "building_fuel_models.csv").write_text(building_row, encoding="utf-8")
        (root / "elmfire.data").write_text(concrete_namelist(template, str(spec["id"])), encoding="utf-8")
        fingerprint = variant_fingerprint(root)
        spec["runtime_input_fingerprint_sha256"] = fingerprint["sha256"]
        fingerprints[str(spec["id"])] = fingerprint
        (root / "variant.json").write_text(json.dumps(spec, indent=2) + "\n", encoding="utf-8")
    aggregate = aggregate_fingerprint(fingerprints)
    manifest = {
        "schema_version": 2,
        "case_id": CASE_ID,
        "source_commit": SOURCE_COMMIT,
        "source_review": SOURCE_REVIEW,
        "source_checkout_required_at_runtime": False,
        "runtime_input_fingerprint_sha256": aggregate,
        "grid": {
            "shape": [SIZE, SIZE],
            "cell_size_m": DX_M,
            "crs": CRS,
            "origin_upper_left_m": [-250.0, 250.0],
            "transform_gdal": list(TRANSFORM.to_gdal()),
            "row_direction": "north to south",
            "nodata": NODATA,
        },
        "oracle": "recorded transient DFC+radiation is the stimulus for an independent downstream heat-to-ROS calculation",
        "oracle_selection": "use the receiver DFC+radiation sample at its terminal-TOA time; compare it with the receiver VS captured when that cell entered LIST_BURNED",
        "input_design_arithmetic": "explicit IEEE-754 binary32 transcription of the reviewed default-REAL operation sequence",
        "design_fire_times_s": {"early": 1.0, "full_development_end": 100.0, "decay_end": 120.0},
        "time_control": {
            "simulation_tstart_s": 0.0,
            "simulation_tstop_s": 4.0,
            "simulation_dt_s": DT_S,
            "simulation_dtmax_s": DT_S,
            "expected_dump_records": EXPECTED_DUMP_RECORDS,
            "dormant_ignition_time_s": 5.0,
            "target_cfl": TARGET_CFL,
            "maximum_predicted_absolute_displacement_per_step_m": max(
                float(spec["predicted_absolute_displacement_per_step_m"]) for spec in specs
            ),
            "target_cfl_displacement_limit_m": TARGET_CFL * DX_M,
        },
        "receiver_observability": {
            "source_phi": SOURCE_PHI,
            "receiver_phi": RECEIVER_PHI,
            "guard_row_col": [list(item) for item in GUARD_RCS],
            "prepared_limited_gradient_per_m": prepared_limited_gradient_per_m(),
            "required_receiver_toa_s": 1.0,
            "derivation": "the first nonzero-HRR step has a conservative half-RK2 normal-transport decrement larger than receiver_phi plus the source I/O noise bound for every variant; the side stencil is flat tangentially; four urban guard cells keep LIST_TAGGED above the two-node early-stop threshold; a post-stop ignition prevents the zero-HRR first step from satisfying the stall predicate",
        },
        "internal_only": [
            "TOTAL_TRANSIENT_DFC",
            "TOTAL_TRANSIENT_RADIATION",
            "RAD_PER_SQCELL",
            "FTP_PA after overwrite",
            "ABSOLUTE_U",
            "ellipse head/back/side component velocities",
            "LOW",
        ],
        "variants": specs,
    }
    expected_path = variants_dir / "expected.json"
    expected_path.write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    fingerprint_document = {
        "schema_version": 2,
        "case_id": CASE_ID,
        "source_commit": SOURCE_COMMIT,
        "aggregate_sha256": aggregate,
        "oracle_manifest_sha256": sha256_file(expected_path),
        "oracle_evaluator_sha256": sha256_file(CASE_DIR / "scripts/postprocess.py"),
        "source_review": SOURCE_REVIEW,
        "variants": fingerprints,
    }
    (variants_dir / "run_fingerprints.json").write_text(
        json.dumps(fingerprint_document, indent=2) + "\n", encoding="utf-8"
    )
    (variants_dir / "variant_ids.txt").write_text(
        "\n".join(str(s["id"]) for s in specs) + "\n", encoding="utf-8"
    )
    metrics_path.write_text(
        json.dumps(initial_metrics(len(specs), aggregate), indent=2) + "\n", encoding="utf-8"
    )
    make_input_figure(specs)
    print(f"[OK] {CASE_ID}: generated {len(specs)} deterministic variants")


if __name__ == "__main__":
    main()
