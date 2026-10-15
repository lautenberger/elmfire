#!/usr/bin/env python3
"""Generate one-at-a-time custom fuel-table parameter variants."""
from pathlib import Path

from case_support import begin_run, duration_for_ros, make_planar_variant, write_expected
from rothermel_reference import calculate, read_fuel_models

CASE_DIR = Path(__file__).resolve().parents[1]
BASE_ROW = [250, "CFP", ".FALSE.", 0.10, 0.05, 0.02, 0.00, 0.08,
            2000.0, 9999.0, 1600.0, 1.0, 25.0, 8000.0]
VARIANTS = (
    ("baseline", "baseline", 0, None),
    ("load_low", "1-h load", 3, 0.05), ("load_high", "1-h load", 3, 0.15),
    ("sav_low", "1-h SAV", 8, 1000.0), ("sav_high", "1-h SAV", 8, 3000.0),
    ("depth_low", "fuel-bed depth", 11, 0.5), ("depth_high", "fuel-bed depth", 11, 2.0),
    ("mex_low", "dead extinction", 12, 15.0), ("mex_high", "dead extinction", 12, 35.0),
    ("heat_low", "heat content", 13, 6000.0), ("heat_high", "heat content", 13, 10000.0),
)


def main() -> None:
    begin_run(CASE_DIR, "CASE41_CFP", len(VARIANTS))
    base = (CASE_DIR / "elmfire.data.in").read_text(encoding="utf-8")
    standard_rows = (CASE_DIR / "data/misc/fuel_models.csv").read_text(encoding="utf-8").rstrip()
    expected_variants = []
    for index, (variant_id, group, column, value) in enumerate(VARIANTS):
        row = list(BASE_ROW)
        if value is not None:
            row[column] = value
        misc = CASE_DIR / "variants" / variant_id / "misc"
        misc.mkdir(parents=True, exist_ok=True)
        table = misc / "fuel_models.csv"
        table.write_text(standard_rows + "\n" + ",".join(str(item) for item in row) + "\n", encoding="utf-8")
        fuel = read_fuel_models(table)[250]
        expected = calculate(fuel, m1=0.05, m10=0.07, m100=0.09, mlh=0.90, mlw=0.90)
        make_planar_variant(
            CASE_DIR, base, variant_id=variant_id, fuel_model=250,
            slope_degrees=0.0, aspect_degrees=0.0, wind_20ft_mph=0.0,
            wind_from_degrees=270.0, m1_percent=5.0, m10_percent=7.0,
            m100_percent=9.0, live_herb_percent=90.0, live_woody_percent=90.0,
            expected_ros_m_min=expected["ros_m_min"],
            tstop_seconds=duration_for_ros(expected["ros_m_min"]),
            fuel_table_directory=f"./variants/{variant_id}/misc/",
        )
        expected_variants.append({"id": variant_id, "group": group, "x": index,
            "changed_column": column, "configured_value": value,
            "expected_ros_m_min": expected["ros_m_min"],
            "expected_ir_kw_m2": expected["reaction_intensity_kw_m2"],
            "reference": expected})
    write_expected(CASE_DIR, {"case_id": "CASE41_CFP", "x_label": "one-at-a-time variant index",
        "tolerances": {"ros_relative_error": 0.02, "ir_relative_error": 0.01,
                       "toa_ros_relative_error": 0.05, "toa_r2_min": 0.995},
        "variants": expected_variants})
    print(f"[OK] CASE41_CFP generated {len(expected_variants)} variants")


if __name__ == "__main__":
    main()
