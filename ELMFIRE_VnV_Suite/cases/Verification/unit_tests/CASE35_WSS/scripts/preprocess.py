#!/usr/bin/env python3
"""Generate target-midflame wind sweeps for five representative fuels."""
from pathlib import Path

from case_support import begin_run, duration_for_ros, make_planar_variant, write_expected
from rothermel_reference import calculate, read_fuel_models, wind_adjustment_factor

CASE_DIR = Path(__file__).resolve().parents[1]
FUEL_CODES = (2, 9, 102, 165, 188)
MIDFLAME_MPH = (0.0, 1.0, 2.0, 4.0, 6.0, 8.0, 12.0)


def main() -> None:
    begin_run(CASE_DIR, "CASE35_WSS", len(FUEL_CODES) * len(MIDFLAME_MPH))
    base = (CASE_DIR / "elmfire.data.in").read_text(encoding="utf-8")
    fuels = read_fuel_models(CASE_DIR / "data/misc/fuel_models.csv")
    variants = []
    for code in FUEL_CODES:
        fuel = fuels[code]
        waf = wind_adjustment_factor(0.0, 0.0, fuel.depth_ft)
        for target_mph in MIDFLAME_MPH:
            midflame_ft_min = target_mph * 5280.0 / 60.0
            expected = calculate(fuel, m1=0.05, m10=0.07, m100=0.09,
                                 mlh=0.90, mlw=0.90,
                                 midflame_wind_ft_min=midflame_ft_min)
            token = str(target_mph).replace(".", "p")
            variant_id = f"fuel_{code:03d}_u_{token}"
            make_planar_variant(
                CASE_DIR, base, variant_id=variant_id, fuel_model=code,
                slope_degrees=0.0, aspect_degrees=0.0,
                wind_20ft_mph=target_mph / waf if waf > 0.0 else 0.0,
                wind_from_degrees=270.0, m1_percent=5.0, m10_percent=7.0,
                m100_percent=9.0, live_herb_percent=90.0,
            live_woody_percent=90.0,
            expected_ros_m_min=expected["ros_m_min"],
            tstop_seconds=duration_for_ros(expected["ros_m_min"]),
            )
            variants.append({"id": variant_id, "group": fuel.name, "x": target_mph,
                "fuel_model": code, "target_midflame_mph": target_mph,
                "input_20ft_mph": target_mph / waf if waf > 0.0 else 0.0,
                "expected_ros_m_min": expected["ros_m_min"],
                "expected_ir_kw_m2": expected["reaction_intensity_kw_m2"],
                "reference": expected})
    write_expected(CASE_DIR, {"case_id": "CASE35_WSS", "x_label": "midflame wind (mph)",
        "tolerances": {"ros_relative_error": 0.02, "ir_relative_error": 0.01,
                       "toa_ros_relative_error": 0.05, "toa_r2_min": 0.995},
        "variants": variants})
    print(f"[OK] CASE35_WSS generated {len(variants)} variants")


if __name__ == "__main__":
    main()
