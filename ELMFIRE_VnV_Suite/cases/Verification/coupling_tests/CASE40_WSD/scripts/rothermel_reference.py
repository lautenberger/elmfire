#!/usr/bin/env python3
"""Independent Rothermel surface-spread reference used only by this case.

The equations follow Andrews (2018) and retain ELMFIRE's currently selected
original wind limit and its associated slope-factor cap.  Inputs use the
native Rothermel units stored in ELMFIRE's fuel-model table: feet, pounds,
minutes, Btu, and moisture fractions.  Returned reaction intensity is SI
(kW/m2), matching ELMFIRE's reaction-intensity raster.
"""
from __future__ import annotations

import csv
import math
from dataclasses import dataclass
from pathlib import Path

BTU_FT2_MIN_TO_KW_M2 = 1.055 / (60.0 * 0.3048 * 0.3048)


@dataclass(frozen=True)
class FuelModel:
    number: int
    name: str
    dynamic: bool
    loads: tuple[float, float, float, float, float]
    sav: tuple[float, float, float]
    depth_ft: float
    dead_extinction: float
    heat_content: float


def read_fuel_models(path: Path) -> dict[int, FuelModel]:
    """Read the public ELMFIRE fuel-table columns without source dependencies."""
    result: dict[int, FuelModel] = {}
    with path.open(newline="", encoding="utf-8") as stream:
        for row in csv.reader(stream):
            if not row or row[0].lstrip().startswith("#"):
                continue
            number = int(row[0])
            result[number] = FuelModel(
                number=number,
                name=row[1],
                dynamic=row[2].strip().upper() in {".TRUE.", "TRUE", "T"},
                loads=tuple(float(value) for value in row[3:8]),
                sav=tuple(float(value) for value in row[8:11]),
                depth_ft=float(row[11]),
                dead_extinction=float(row[12]) / 100.0,
                heat_content=float(row[13]),
            )
    return result


def wind_adjustment_factor(
    canopy_cover: float, canopy_height_m: float, fuel_bed_depth_ft: float
) -> float:
    """Return ELMFIRE's 20-ft-to-midflame wind adjustment factor."""
    if canopy_cover < 0.0:
        return 0.0
    if canopy_cover > 1.0e-4 and canopy_height_m > 1.0e-4:
        height_ft = canopy_height_m / 0.3048
        u_h_over_u_20 = 1.0 / math.log(
            (20.0 + 0.36 * height_ft) / (0.13 * height_ft)
        )
        shelter = 0.3333 * canopy_cover
        return u_h_over_u_20 * 0.555 / math.sqrt(shelter * height_ft)
    if fuel_bed_depth_ft <= 1.0e-4:
        return 0.0
    term1 = 1.36 / math.log(
        (20.0 + 0.36 * fuel_bed_depth_ft) / (0.13 * fuel_bed_depth_ft)
    )
    term2 = math.log(1.36 / 0.13) - 1.0
    return term1 * term2


def blended_waf(
    canopy_cover: float, canopy_height_m: float, fuel_bed_depth_ft: float
) -> float:
    """Apply ELMFIRE's sheltered/unsheltered transition for Rothermel fuels."""
    unsheltered = wind_adjustment_factor(0.0, 0.0, fuel_bed_depth_ft)
    sheltered = min(
        wind_adjustment_factor(canopy_cover, canopy_height_m, 0.0), unsheltered
    )
    shelter = 0.3333 * canopy_cover
    if shelter >= 0.05:
        return sheltered
    unsheltered_fraction = 1.0 - 20.0 * shelter
    return unsheltered_fraction * unsheltered + (1.0 - unsheltered_fraction) * sheltered


def _fuel_state(fuel: FuelModel, live_herb_moisture: float) -> dict[str, object]:
    """Build the six-class weighted fuel state, including dynamic curing."""
    w0 = [fuel.loads[0], fuel.loads[1], fuel.loads[2], 0.0, fuel.loads[3], fuel.loads[4]]
    sigma = [fuel.sav[0], 109.0, 30.0, 9999.0, fuel.sav[1], fuel.sav[2]]
    ilh = max(30, min(120, int(math.floor(100.0 * live_herb_moisture + 0.5))))
    cured_fraction = 0.0
    if fuel.dynamic:
        live_fraction = max(0.0, min(1.0, (ilh - 30.0) / 90.0))
        cured_fraction = 1.0 - live_fraction
        transferred = cured_fraction * w0[4]
        sigma4 = sigma[4]
        denominator = sigma[0] * w0[0] + sigma4 * transferred
        if denominator > 0.0:
            sigma[0] = (
                sigma[0] ** 2 * w0[0] + sigma4**2 * transferred
            ) / denominator
        w0[0] += transferred
        w0[4] *= live_fraction

    rho_p, mineral_total, mineral_effective = 32.0, 0.055, 0.01
    area = [sigma[i] * w0[i] / rho_p for i in range(6)]
    area_dead = max(sum(area[:4]), 1.0e-9)
    area_live = max(sum(area[4:]), 1.0e-9)
    area_total = area_dead + area_live
    f = [area[i] / (area_dead if i < 4 else area_live) for i in range(6)]
    f_dead, f_live = area_dead / area_total, area_live / area_total
    f_w0 = [f[i] * w0[i] for i in range(6)]
    f_sigma = [f[i] * sigma[i] for i in range(6)]
    epsilon = [math.exp(-138.0 / value) for value in sigma]
    f_epsilon = [f[i] * epsilon[i] for i in range(6)]
    sigma_dead = sum(f_sigma[:4])
    sigma_live = sum(f_sigma[4:])
    sigma_overall = f_dead * sigma_dead + f_live * sigma_live
    beta = sum(w0) / (fuel.depth_ft * rho_p)
    beta_optimum = 3.348 / sigma_overall**0.8189
    bulk_density = sum(w0) / fuel.depth_ft
    xi = math.exp((0.792 + 0.681 * math.sqrt(sigma_overall)) * (0.1 + beta)) / (
        192.0 + 0.2595 * sigma_overall
    )
    a_coefficient = 133.0 / sigma_overall**0.7913
    b_coefficient = 0.02526 * sigma_overall**0.54
    c_coefficient = 7.47 * math.exp(-0.133 * sigma_overall**0.55)
    e_coefficient = 0.715 * math.exp(-0.000359 * sigma_overall)
    gamma_peak = sigma_overall**1.5 / (495.0 + 0.0594 * sigma_overall**1.5)
    gamma = gamma_peak * (beta / beta_optimum) ** a_coefficient * math.exp(
        a_coefficient * (1.0 - beta / beta_optimum)
    )
    eta_s = 0.174 / mineral_effective**0.19
    net_dead = sum(f_w0[:4]) * (1.0 - mineral_total)
    net_live = sum(f_w0[4:]) * (1.0 - mineral_total)
    wprime_dead = [w0[i] * epsilon[i] for i in range(4)]
    wprime_live_denominator = sum(
        w0[i] * math.exp(-500.0 / sigma[i]) for i in (4, 5)
    )
    live_extinction_base = (
        2.9 * sum(wprime_dead) / wprime_live_denominator
        if wprime_live_denominator > 1.0e-6
        else 100.0
    )
    return {
        "w0": w0,
        "sigma": sigma,
        "f": f,
        "epsilon": epsilon,
        "f_epsilon": f_epsilon,
        "f_dead": f_dead,
        "f_live": f_live,
        "beta": beta,
        "beta_optimum": beta_optimum,
        "bulk_density": bulk_density,
        "xi": xi,
        "b_coefficient": b_coefficient,
        "phiw_term": c_coefficient * (beta / beta_optimum) ** (-e_coefficient),
        "phis_term": 5.275 * beta ** (-0.3),
        "ir_dead_prefactor": gamma * net_dead * eta_s * fuel.heat_content,
        "ir_live_prefactor": gamma * net_live * eta_s * fuel.heat_content,
        "live_extinction_base": live_extinction_base,
        "wprime_dead": wprime_dead,
        "cured_fraction": cured_fraction,
        "ilh_percent": ilh,
    }


def _moisture_damping(ratio: float) -> float:
    value = 1.0 - 2.59 * ratio + 5.11 * ratio**2 - 3.52 * ratio**3
    return max(0.0, min(1.0, value))


def calculate(
    fuel: FuelModel,
    *,
    m1: float,
    m10: float,
    m100: float,
    mlh: float,
    mlw: float,
    midflame_wind_ft_min: float = 0.0,
    slope_degrees: float = 0.0,
) -> dict[str, float]:
    """Calculate ELMFIRE's Rothermel surface quantities for one homogeneous cell."""
    state = _fuel_state(fuel, mlh)
    moisture = [m1, m10, m100, m1, mlh, mlw]
    f = state["f"]
    weighted_moisture = [f[i] * moisture[i] for i in range(6)]
    dead_moisture = sum(weighted_moisture[:4])

    wprime_dead = state["wprime_dead"]
    dead_weighted_sum = sum(wprime_dead[i] * moisture[i] for i in range(4))
    denominator = max(sum(wprime_dead) * fuel.dead_extinction, 1.0e-12)
    live_extinction = state["live_extinction_base"] * (
        1.0 - dead_weighted_sum / denominator
    ) - 0.226
    live_extinction = max(live_extinction, fuel.dead_extinction)
    live_moisture = sum(weighted_moisture[4:])

    eta_dead = _moisture_damping(dead_moisture / fuel.dead_extinction)
    eta_live = _moisture_damping(live_moisture / live_extinction)
    ir_native = (
        state["ir_dead_prefactor"] * eta_dead
        + state["ir_live_prefactor"] * eta_live
    )
    qig = [250.0 + 1116.0 * value for value in moisture]
    f_epsilon = state["f_epsilon"]
    dead_sink = state["bulk_density"] * sum(
        f_epsilon[i] * qig[i] for i in range(4)
    )
    live_sink = state["bulk_density"] * sum(
        f_epsilon[i] * qig[i] for i in range(4, 6)
    )
    heat_sink = state["f_dead"] * dead_sink + state["f_live"] * live_sink
    ros0_ft_min = ir_native * state["xi"] / heat_sink if heat_sink > 0.0 else 0.0

    wind_limit_ft_min = 0.9 * ir_native
    limited_wind = min(max(midflame_wind_ft_min, 0.0), wind_limit_ft_min)
    phiw = state["phiw_term"] * limited_wind ** state["b_coefficient"]
    phis_unlimited = state["phis_term"] * math.tan(math.radians(slope_degrees)) ** 2
    phis_limit = state["phiw_term"] * wind_limit_ft_min ** state["b_coefficient"]
    phis = min(phis_unlimited, phis_limit)
    ros_ft_min = ros0_ft_min * (1.0 + phiw + phis)
    return {
        "ros0_ft_min": ros0_ft_min,
        "ros_ft_min": ros_ft_min,
        "ros_m_min": ros_ft_min * 0.3048,
        "reaction_intensity_kw_m2": ir_native * BTU_FT2_MIN_TO_KW_M2,
        "reaction_intensity_native": ir_native,
        "wind_factor": phiw,
        "slope_factor": phis,
        "unlimited_slope_factor": phis_unlimited,
        "wind_limit_ft_min": wind_limit_ft_min,
        "wind_exponent": state["b_coefficient"],
        "wind_term": state["phiw_term"],
        "live_extinction_fraction": live_extinction,
        "cured_fraction": state["cured_fraction"],
        "effective_live_herb_load": state["w0"][4],
        "effective_dead_1h_load": state["w0"][0],
        "ilh_percent": float(state["ilh_percent"]),
    }
