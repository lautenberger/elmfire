#!/usr/bin/env python3
"""Generate canopy variants that exercise ELMFIRE's wind-adjustment coupling."""
from pathlib import Path

from case_support import begin_run, duration_for_ros, make_planar_variant, write_expected
from rothermel_reference import blended_waf, calculate, read_fuel_models

CASE_DIR = Path(__file__).resolve().parents[1]
FUEL_CODE = 2
WIND_20FT_MPH = 10.0
CANOPY_VARIANTS = ((0.0, 0.0), (0.10, 5.0), (0.10, 15.0), (0.10, 30.0),
                   (0.30, 5.0), (0.30, 15.0), (0.30, 30.0),
                   (0.60, 5.0), (0.60, 15.0), (0.60, 30.0))


def main() -> None:
    begin_run(CASE_DIR, "CASE42_WAF", len(CANOPY_VARIANTS))
    base = (CASE_DIR / "elmfire.data.in").read_text(encoding="utf-8")
    fuel = read_fuel_models(CASE_DIR / "data/misc/fuel_models.csv")[FUEL_CODE]
    variants = []
    for cover, height in CANOPY_VARIANTS:
        waf = blended_waf(cover, height, fuel.depth_ft)
        expected = calculate(
            fuel, m1=0.05, m10=0.07, m100=0.09, mlh=0.90, mlw=0.90,
            midflame_wind_ft_min=WIND_20FT_MPH*waf*5280.0/60.0,
        )
        variant_id = f"cc_{int(cover*100):03d}_ch_{int(height):03d}"
        make_planar_variant(
            CASE_DIR, base, variant_id=variant_id, fuel_model=FUEL_CODE,
            slope_degrees=0.0, aspect_degrees=0.0, wind_20ft_mph=WIND_20FT_MPH,
            wind_from_degrees=270.0, m1_percent=5.0, m10_percent=7.0,
            m100_percent=9.0, live_herb_percent=90.0, live_woody_percent=90.0,
            expected_ros_m_min=expected["ros_m_min"],
            tstop_seconds=duration_for_ros(expected["ros_m_min"]),
            canopy_cover_percent=100.0*cover, canopy_height_m=height,
        )
        variants.append({"id": variant_id, "group": f"height {height:g} m", "x": cover,
            "canopy_cover_fraction": cover, "canopy_height_m": height,
            "expected_waf": waf, "expected_midflame_mph": WIND_20FT_MPH*waf,
            "expected_ros_m_min": expected["ros_m_min"],
            "expected_ir_kw_m2": expected["reaction_intensity_kw_m2"],
            "reference": expected})
    write_expected(CASE_DIR, {"case_id": "CASE42_WAF", "x_label": "canopy-cover fraction",
        "tolerances": {"ros_relative_error": 0.03, "ir_relative_error": 0.01,
                       "toa_ros_relative_error": 0.06, "toa_r2_min": 0.995},
        "variants": variants})
    print(f"[OK] CASE42_WAF generated {len(variants)} variants")


if __name__ == "__main__":
    main()
