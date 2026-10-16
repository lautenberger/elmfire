#!/usr/bin/env python3
"""Generate static live-fuel moisture sweeps with unchanged fuel loads."""
from pathlib import Path

from case_support import begin_run, duration_for_ros, make_planar_variant, write_expected
from rothermel_reference import calculate, read_fuel_models

CASE_DIR = Path(__file__).resolve().parents[1]
FUEL_CODES = (142, 146, 148)
# These shrub models carry live woody, rather than live herbaceous, fuel.
# ELMFIRE applies a 60% lower bound to live-woody moisture during weather
# interpolation, so values below 60% would not represent their configured
# input and cannot be used as distinct verification points.
LIVE_MOISTURE_PERCENT = (60.0, 70.0, 90.0, 120.0, 150.0, 180.0, 200.0)


def main() -> None:
    begin_run(CASE_DIR, "CASE38_LMS", len(FUEL_CODES) * len(LIVE_MOISTURE_PERCENT))
    base = (CASE_DIR / "elmfire.data.in").read_text(encoding="utf-8")
    fuels = read_fuel_models(CASE_DIR / "data/misc/fuel_models.csv")
    variants = []
    for code in FUEL_CODES:
        fuel = fuels[code]
        if fuel.dynamic:
            raise ValueError(f"CASE38 requires static fuel models; {code} is dynamic")
        for percent in LIVE_MOISTURE_PERCENT:
            fraction = percent / 100.0
            expected = calculate(fuel, m1=0.08, m10=0.08, m100=0.08,
                                 mlh=fraction, mlw=fraction)
            variant_id = f"fuel_{code:03d}_m_{int(percent):03d}"
            make_planar_variant(
                CASE_DIR, base, variant_id=variant_id, fuel_model=code,
                slope_degrees=0.0, aspect_degrees=0.0, wind_20ft_mph=0.0,
                wind_from_degrees=270.0, m1_percent=8.0, m10_percent=8.0,
                m100_percent=8.0, live_herb_percent=percent,
                live_woody_percent=percent,
                expected_ros_m_min=expected["ros_m_min"],
                tstop_seconds=duration_for_ros(expected["ros_m_min"]),
            )
            variants.append({"id": variant_id, "group": fuel.name, "x": percent,
                "fuel_model": code, "live_moisture_percent": percent,
                "expected_ros_m_min": expected["ros_m_min"],
                "expected_ir_kw_m2": expected["reaction_intensity_kw_m2"],
                "reference": expected})
    write_expected(CASE_DIR, {"case_id": "CASE38_LMS", "x_label": "live fuel moisture (percent)",
        "tolerances": {"ros_relative_error": 0.02, "ir_relative_error": 0.01,
                       "toa_ros_relative_error": 0.05, "toa_r2_min": 0.995},
        "variants": variants})
    print(f"[OK] CASE38_LMS generated {len(variants)} variants")


if __name__ == "__main__":
    main()
