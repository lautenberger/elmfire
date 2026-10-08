#!/usr/bin/env python3
"""Generate dead-moisture-to-extinction sweeps for two static fuels."""
from pathlib import Path

from case_support import begin_run, duration_for_ros, make_planar_variant, write_expected
from rothermel_reference import calculate, read_fuel_models

CASE_DIR = Path(__file__).resolve().parents[1]
FUEL_CODES = (3, 186)
EXTINCTION_RATIOS = (0.10, 0.25, 0.50, 0.70, 0.85, 0.95, 1.00, 1.10)


def main() -> None:
    begin_run(CASE_DIR, "CASE37_DMS", len(FUEL_CODES) * len(EXTINCTION_RATIOS))
    base = (CASE_DIR / "elmfire.data.in").read_text(encoding="utf-8")
    fuels = read_fuel_models(CASE_DIR / "data/misc/fuel_models.csv")
    variants = []
    for code in FUEL_CODES:
        fuel = fuels[code]
        for ratio in EXTINCTION_RATIOS:
            moisture = ratio * fuel.dead_extinction
            expected = calculate(fuel, m1=moisture, m10=moisture, m100=moisture,
                                 mlh=0.90, mlw=0.90)
            variant_id = f"fuel_{code:03d}_r_{int(round(100*ratio)):03d}"
            make_planar_variant(
                CASE_DIR, base, variant_id=variant_id, fuel_model=code,
                slope_degrees=0.0, aspect_degrees=0.0, wind_20ft_mph=0.0,
                wind_from_degrees=270.0, m1_percent=100.0*moisture,
                m10_percent=100.0*moisture, m100_percent=100.0*moisture,
                live_herb_percent=90.0, live_woody_percent=90.0,
                expected_ros_m_min=expected["ros_m_min"],
                tstop_seconds=duration_for_ros(expected["ros_m_min"]),
            )
            variants.append({"id": variant_id, "group": fuel.name, "x": ratio,
                "fuel_model": code, "moisture_extinction_ratio": ratio,
                "expected_ros_m_min": expected["ros_m_min"],
                "expected_ir_kw_m2": expected["reaction_intensity_kw_m2"],
                "reference": expected})
    write_expected(CASE_DIR, {"case_id": "CASE37_DMS", "x_label": "dead moisture / extinction moisture",
        "tolerances": {"ros_relative_error": 0.02, "ir_relative_error": 0.01,
                       "toa_ros_relative_error": 0.05, "toa_r2_min": 0.995},
        "variants": variants})
    print(f"[OK] CASE37_DMS generated {len(variants)} variants")


if __name__ == "__main__":
    main()
