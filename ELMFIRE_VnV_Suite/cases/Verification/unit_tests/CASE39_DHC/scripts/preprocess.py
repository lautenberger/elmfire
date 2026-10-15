#!/usr/bin/env python3
"""Generate dynamic-herbaceous curing sweeps for GR2 and GS3."""
from pathlib import Path

from case_support import begin_run, duration_for_ros, make_planar_variant, write_expected
from rothermel_reference import calculate, read_fuel_models, wind_adjustment_factor

CASE_DIR = Path(__file__).resolve().parents[1]
FUEL_CODES = (102, 123)
LIVE_HERB_PERCENT = (30.0, 45.0, 60.0, 75.0, 90.0, 100.0, 110.0, 120.0, 135.0, 200.0)
TARGET_MIDFLAME_MPH = 6.0


def main() -> None:
    begin_run(CASE_DIR, "CASE39_DHC", len(FUEL_CODES) * len(LIVE_HERB_PERCENT))
    base = (CASE_DIR / "elmfire.data.in").read_text(encoding="utf-8")
    fuels = read_fuel_models(CASE_DIR / "data/misc/fuel_models.csv")
    variants = []
    for code in FUEL_CODES:
        fuel = fuels[code]
        if not fuel.dynamic:
            raise ValueError(f"CASE39 requires dynamic fuel models; {code} is static")
        waf = wind_adjustment_factor(0.0, 0.0, fuel.depth_ft)
        for percent in LIVE_HERB_PERCENT:
            expected = calculate(
                fuel, m1=0.05, m10=0.05, m100=0.05, mlh=percent/100.0,
                mlw=0.90, midflame_wind_ft_min=TARGET_MIDFLAME_MPH*5280.0/60.0,
            )
            variant_id = f"fuel_{code:03d}_lh_{int(percent):03d}"
            make_planar_variant(
                CASE_DIR, base, variant_id=variant_id, fuel_model=code,
                slope_degrees=0.0, aspect_degrees=0.0,
                wind_20ft_mph=TARGET_MIDFLAME_MPH/waf,
                wind_from_degrees=270.0, m1_percent=5.0, m10_percent=5.0,
                m100_percent=5.0, live_herb_percent=percent,
                live_woody_percent=90.0,
                expected_ros_m_min=expected["ros_m_min"],
                tstop_seconds=duration_for_ros(expected["ros_m_min"]),
            )
            variants.append({"id": variant_id, "group": fuel.name, "x": percent,
                "fuel_model": code, "live_herb_percent": percent,
                "expected_cured_fraction": expected["cured_fraction"],
                "expected_ros_m_min": expected["ros_m_min"],
                "expected_ir_kw_m2": expected["reaction_intensity_kw_m2"],
                "reference": expected})
    write_expected(CASE_DIR, {"case_id": "CASE39_DHC", "x_label": "live-herb moisture (percent)",
        "tolerances": {"ros_relative_error": 0.02, "ir_relative_error": 0.01,
                       "toa_ros_relative_error": 0.05, "toa_r2_min": 0.995},
        "variants": variants})
    print(f"[OK] CASE39_DHC generated {len(variants)} variants")


if __name__ == "__main__":
    main()
