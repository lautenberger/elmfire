#!/usr/bin/env python3
"""Generate point-fire variants for five wind-to-upslope angles."""
from __future__ import annotations

import math
from pathlib import Path

import numpy as np

from case_support import (
    CELL_SIZE_M, HALF_WIDTH_M, SIZE, begin_run, bounded_level_set,
    replace_assignment, reset_generated_directory, timestep_for_ros,
    write_expected, write_raster,
)
from rothermel_reference import calculate, read_fuel_models, wind_adjustment_factor

CASE_DIR = Path(__file__).resolve().parents[1]
FUEL_CODE = 2
SLOPE_PERCENT = 50.0
# ELMFIRE uses NINT(SLP) to index its slope-factor and surface-projection
# lookup tables. Generate that effective integer-degree condition explicitly.
REQUESTED_SLOPE_DEGREES = math.degrees(math.atan(SLOPE_PERCENT / 100.0))
SLOPE_DEGREES = float(math.floor(REQUESTED_SLOPE_DEGREES + 0.5))
TARGET_MIDFLAME_MPH = 4.0
RELATIVE_ANGLES = (0.0, 45.0, 90.0, 135.0, 180.0)
INITIAL_RADIUS_M = 30.0


def main() -> None:
    begin_run(CASE_DIR, "CASE40_WSD", len(RELATIVE_ANGLES))
    base = (CASE_DIR / "elmfire.data.in").read_text(encoding="utf-8")
    fuel = read_fuel_models(CASE_DIR / "data/misc/fuel_models.csv")[FUEL_CODE]
    waf = wind_adjustment_factor(0.0, 0.0, fuel.depth_ft)
    no_wind = calculate(fuel, m1=0.05, m10=0.07, m100=0.09, mlh=0.60,
                        mlw=0.90, slope_degrees=SLOPE_DEGREES)
    wind_only = calculate(
        fuel, m1=0.05, m10=0.07, m100=0.09, mlh=0.60, mlw=0.90,
        midflame_wind_ft_min=TARGET_MIDFLAME_MPH*5280.0/60.0,
    )
    phi_s, phi_w = no_wind["slope_factor"], wind_only["wind_factor"]
    x = -HALF_WIDTH_M + (np.arange(SIZE) + 0.5) * CELL_SIZE_M
    y = HALF_WIDTH_M - (np.arange(SIZE) + 0.5) * CELL_SIZE_M
    xx, yy = np.meshgrid(x, y)
    slope_cosine = math.cos(math.radians(SLOPE_DEGREES))
    surface_y = yy / slope_cosine
    ignition_phi = bounded_level_set(
        np.hypot(xx, surface_y) - INITIAL_RADIUS_M
    )
    stale_figure = CASE_DIR / "figures/vector_response.pdf"
    if stale_figure.exists():
        stale_figure.unlink()
    variants = []
    for angle in RELATIVE_ANGLES:
        radians = math.radians(angle)
        component_x = phi_w * math.sin(radians)
        component_y = phi_s + phi_w * math.cos(radians)
        phi_magnitude = math.hypot(component_x, component_y)
        direction = math.degrees(math.atan2(component_x, component_y)) % 360.0
        head_ros_ft_min = no_wind["ros0_ft_min"] * (1.0 + phi_magnitude)
        head_ros_m_min = head_ros_ft_min * 0.3048
        effective_wind = (phi_magnitude / wind_only["wind_term"]) ** (1.0 / wind_only["wind_exponent"])
        low = min(
            0.936*math.exp(0.1147*effective_wind*(60.0/5280.0))
            + 0.461*math.exp(-0.0692*effective_wind*(60.0/5280.0)) - 0.397,
            8.0,
        )
        variant_id = f"angle_{int(angle):03d}"
        root = CASE_DIR / "variants" / variant_id
        inputs = root / "inputs"
        inputs.mkdir(parents=True, exist_ok=True)
        reset_generated_directory(root / "outputs")
        reset_generated_directory(root / "scratch")
        fields = {"slp": SLOPE_DEGREES, "asp": 180.0,
                  "ws": TARGET_MIDFLAME_MPH/waf,
                  "wd": (angle + 180.0) % 360.0,
                  "m1": 5.0, "m10": 7.0, "m100": 9.0, "adj": 1.0,
                  # Define a circular ignition in slope-surface coordinates,
                  # then clip PHI to ELMFIRE's supported [-1, 1] interval.
                  "phi": ignition_phi,
                  "dem": 0.0, "cc": 0.0, "ch": 0.0, "cbh": 0.0, "cbd": 0.0}
        for name, value in fields.items():
            write_raster(inputs / f"{name}.tif", value, "float32")
        write_raster(inputs / "fbfm40.tif", FUEL_CODE, "int16")
        config = base
        timestep_s, timestep_max_s = timestep_for_ros(head_ros_m_min)
        # Keep the fixed 1800 s experiment duration while choosing the largest
        # conservative whole-second step that divides it exactly. This avoids
        # ELMFIRE.s shortened final-step stalled-front timestamp defect.
        timestep_s = max(1.0, float(math.floor(timestep_s)))
        while 1800 % int(timestep_s) != 0:
            timestep_s -= 1.0
        timestep_max_s = timestep_s
        replacements = {
            "FUELS_AND_TOPOGRAPHY_DIRECTORY": f"'./variants/{variant_id}/inputs'",
            "WEATHER_DIRECTORY": f"'./variants/{variant_id}/inputs'",
            "OUTPUTS_DIRECTORY": f"'./variants/{variant_id}/outputs'",
            "SCRATCH": f"'./variants/{variant_id}/scratch'",
            "LH_MOISTURE_CONTENT": 60.0,
            "LW_MOISTURE_CONTENT": 90.0,
            "SIMULATION_DT": round(timestep_s, 6),
            "SIMULATION_DTMAX": round(timestep_max_s, 6),
            "SIMULATION_TSTOP": 1800.0,
        }
        for key, value in replacements.items():
            config = replace_assignment(config, key, value)
        (root / "elmfire.data").write_text(config, encoding="utf-8")
        variants.append({"id": variant_id, "group": "wind-slope angle", "x": angle,
            "relative_angle_degrees": angle, "expected_direction_degrees": direction,
            "expected_head_ros_m_min": head_ros_m_min,
            "expected_length_width": low,
            "expected_ir_kw_m2": no_wind["reaction_intensity_kw_m2"],
            "expected_ros_m_min": head_ros_m_min,
            "reference": {"phi_s": phi_s, "phi_w": phi_w, "phi_magnitude": phi_magnitude}})
    write_expected(CASE_DIR, {"case_id": "CASE40_WSD", "x_label": "wind-to-upslope angle (degrees)",
        "initial_radius_m": INITIAL_RADIUS_M,
        "requested_slope_percent": SLOPE_PERCENT,
        "requested_slope_degrees": REQUESTED_SLOPE_DEGREES,
        "effective_slope_percent": 100.0 * math.tan(math.radians(SLOPE_DEGREES)),
        "slope_degrees": SLOPE_DEGREES,
        "simulation_tstop_s": 1800.0,
        "tolerances": {"direction_degrees": 5.0, "head_ros_relative_error": 0.05,
                       "length_width_relative_error": 0.10, "ir_relative_error": 0.01},
        "variants": variants})
    print(f"[OK] CASE40_WSD generated {len(variants)} variants")


if __name__ == "__main__":
    main()
