"""Regression tests for the Rothermel sweep runtime and evidence contracts."""

from __future__ import annotations

import hashlib
import importlib.util
import json
import math
import os
import tempfile
import unittest
from pathlib import Path

os.environ.setdefault("MPLCONFIGDIR", "/tmp/elmfire-vnv-test-matplotlib")

import numpy as np
import rasterio
from rasterio.transform import from_origin


ROOT = Path(__file__).resolve().parents[2]
CANONICAL = (
    ROOT / "cases/Verification/unit_tests/CASE34_FMS/scripts"
)


def load_module(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(module)
    return module


SUPPORT = load_module("case34_support_test", CANONICAL / "case_support.py")
EVALUATOR = load_module("case34_evaluator_test", CANONICAL / "evaluate_outputs.py")
CASE31 = load_module(
    "case31_postprocess_test",
    ROOT
    / "cases/Verification/coupling_tests/CASE31_PFT/scripts/postprocess.py",
)
CASE40 = load_module(
    "case40_postprocess_test",
    ROOT
    / "cases/Verification/coupling_tests/CASE40_WSD/scripts/postprocess.py",
)


class SweepRuntimeContractTests(unittest.TestCase):
    transform = from_origin(-700.0, 700.0, 10.0, 10.0)

    def write_raster(
        self,
        path: Path,
        values: np.ndarray,
        *,
        transform=None,
    ) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        with rasterio.open(
            path,
            "w",
            driver="GTiff",
            height=values.shape[0],
            width=values.shape[1],
            count=1,
            dtype="float32",
            crs="EPSG:32610",
            transform=transform or self.transform,
            nodata=-9999.0,
        ) as dst:
            dst.write(values.astype(np.float32), 1)

    def make_variant(
        self,
        case: Path,
        *,
        expected_ros: float,
        expected_ir: float,
        toa_required: bool,
        toa: str = "missing",
    ) -> None:
        variant = case / "variants/example"
        (variant / "outputs").mkdir(parents=True, exist_ok=True)
        (variant / "elmfire.data").write_text(
            "&TIME_CONTROL\nSIMULATION_TSTOP = 100.0\n/\n",
            encoding="utf-8",
        )
        (variant / "outputs/dump_times_0000001.csv").write_text(
            "dump_index,time_seconds,is_final_dump\n1,100.0,T\n",
            encoding="utf-8",
        )
        x = -700.0 + (np.arange(140) + 0.5) * 10.0
        y = 700.0 - (np.arange(140) + 0.5) * 10.0
        xx, yy = np.meshgrid(x, y)
        phi = SUPPORT.finite_strip_level_set(xx, yy)
        self.write_raster(variant / "inputs/phi.tif", phi)
        self.write_raster(
            variant / "outputs/vs_0000001_0000100.tif",
            np.full((140, 140), expected_ros),
        )
        self.write_raster(
            variant / "outputs/ir_0000001_0000100.tif",
            np.full((140, 140), expected_ir),
        )
        if toa != "missing":
            arrival = np.full((140, 140), -9999.0)
            if toa == "valid":
                mask = (xx > -400.0) & (np.abs(yy) <= 150.0)
                arrival[mask] = (xx[mask] + 400.0) / (expected_ros / 60.0)
            elif toa == "insufficient":
                mask = (np.abs(xx + 395.0) < 1.0) & (np.abs(yy) <= 150.0)
                arrival[mask] = 10.0
            else:
                raise ValueError(toa)
            self.write_raster(
                variant / "outputs/time_of_arrival_0000001_0000100.tif",
                arrival,
            )
        document = {
            "case_id": "CASE_TEST",
            "tolerances": {
                "ros_relative_error": 0.02,
                "ir_relative_error": 0.01,
                "toa_ros_relative_error": 0.05,
                "toa_r2_min": 0.995,
            },
            "variants": [
                {
                    "id": "example",
                    "group": "test",
                    "x": 0.0,
                    "expected_ros_m_min": expected_ros,
                    "expected_ir_kw_m2": expected_ir,
                    "toa_required": toa_required,
                }
            ],
        }
        (case / "variants/expected.json").write_text(
            json.dumps(document), encoding="utf-8"
        )

    def test_positive_ros_timestep_is_above_stall_scale_and_below_cfl(self) -> None:
        for ros in (0.026, 0.05, 0.1, 0.23, 1.0, 10.0, 61.0):
            timestep, timestep_max = SUPPORT.timestep_for_ros(ros)
            normalized_advance = (ros / 60.0) * timestep / SUPPORT.CELL_SIZE_M
            self.assertGreater(normalized_advance, 0.001)
            self.assertLessEqual(normalized_advance, SUPPORT.TARGET_CFL)
            self.assertAlmostEqual(
                normalized_advance,
                SUPPORT.TARGET_FRONT_ADVANCE_FRACTION,
                places=12,
            )
            self.assertEqual(timestep, timestep_max)

    def test_zero_ros_uses_finite_step(self) -> None:
        self.assertEqual(SUPPORT.timestep_for_ros(0.0), (5.0, 5.0))

    def test_begin_run_invalidates_stale_decision_and_report(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            case = Path(directory)
            (case / "outputs").mkdir()
            (case / "figures").mkdir()
            (case / "report").mkdir()
            (case / "outputs/metrics.json").write_text(
                '{"overall_status":"PASS"}\n', encoding="utf-8"
            )
            (case / "figures/sweep_response.pdf").touch()
            (case / "report/case_report.pdf").touch()
            SUPPORT.begin_run(case, "CASE_TEST", 3)
            result = json.loads((case / "outputs/metrics.json").read_text())
            self.assertEqual(result["overall_status"], "NOT EVALUABLE")
            self.assertEqual(result["workflow_status"], "INCOMPLETE")
            self.assertFalse((case / "figures/sweep_response.pdf").exists())
            self.assertFalse((case / "report/case_report.pdf").exists())

    def test_optional_toa_is_not_required_or_fabricated(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            case = Path(directory)
            self.make_variant(
                case,
                expected_ros=0.05,
                expected_ir=100.0,
                toa_required=False,
            )
            result = EVALUATOR.evaluate(case)
            self.assertTrue(result["required_outputs_complete"])
            self.assertEqual(result["overall_status"], "PASS")
            row = result["variant_results"][0]
            self.assertIsNone(row["toa_ros_m_min"])
            self.assertIsNone(row["toa_r2"])
            self.assertEqual(len(row["source_files"]), 2)
            self.assertEqual(len(result["metrics"]), 2)
            self.assertNotIn(
                "TOA", " ".join(metric["name"] for metric in result["metrics"])
            )

    def test_zero_direct_fields_remain_valid_without_toa(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            case = Path(directory)
            self.make_variant(
                case,
                expected_ros=0.0,
                expected_ir=0.0,
                toa_required=False,
            )
            result = EVALUATOR.evaluate(case)
            self.assertEqual(result["overall_status"], "PASS")
            self.assertEqual(result["variant_results"][0]["measured_ros_m_min"], 0.0)

    def test_required_missing_toa_is_not_evaluable(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            case = Path(directory)
            self.make_variant(
                case,
                expected_ros=1.0,
                expected_ir=100.0,
                toa_required=True,
            )
            result = EVALUATOR.evaluate(case)
            self.assertEqual(result["overall_status"], "NOT EVALUABLE")
            self.assertIn("time_of_arrival", result["missing_evidence"]["example"])
            self.assertEqual(len(result["metrics"]), 4)
            self.assertTrue(
                all(metric["status"] == "NOT EVALUABLE" for metric in result["metrics"])
            )

    def test_required_unfit_toa_is_not_evaluable(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            case = Path(directory)
            self.make_variant(
                case,
                expected_ros=1.0,
                expected_ir=100.0,
                toa_required=True,
                toa="insufficient",
            )
            result = EVALUATOR.evaluate(case)
            self.assertEqual(result["overall_status"], "NOT EVALUABLE")
            self.assertIn(
                "time_of_arrival_fit", result["missing_evidence"]["example"]
            )

    def test_required_valid_toa_is_evaluable(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            case = Path(directory)
            self.make_variant(
                case,
                expected_ros=1.0,
                expected_ir=100.0,
                toa_required=True,
                toa="valid",
            )
            result = EVALUATOR.evaluate(case)
            self.assertEqual(result["overall_status"], "PASS")
            self.assertTrue(math.isfinite(result["variant_results"][0]["toa_r2"]))

    def test_early_final_dump_is_not_evaluable(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            case = Path(directory)
            self.make_variant(
                case,
                expected_ros=1.0,
                expected_ir=100.0,
                toa_required=True,
                toa="valid",
            )
            (case / "variants/example/outputs/dump_times_0000001.csv").write_text(
                "dump_index,time_seconds,is_final_dump\n1,99.0,T\n",
                encoding="utf-8",
            )
            result = EVALUATOR.evaluate(case)
            self.assertEqual(result["overall_status"], "NOT EVALUABLE")
            self.assertIn("terminal_dump", result["missing_evidence"]["example"])

    def test_direct_only_variant_may_terminate_before_tstop(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            case = Path(directory)
            self.make_variant(
                case,
                expected_ros=0.0,
                expected_ir=0.0,
                toa_required=False,
            )
            variant = case / "variants/example"
            (variant / "outputs/dump_times_0000001.csv").write_text(
                "dump_index,time_seconds,is_final_dump\n1,99.0,T\n",
                encoding="utf-8",
            )
            self.write_raster(
                variant / "outputs/vs_0000001_0000099.tif",
                np.zeros((140, 140)),
            )
            self.write_raster(
                variant / "outputs/ir_0000001_0000099.tif",
                np.zeros((140, 140)),
            )
            result = EVALUATOR.evaluate(case)
            self.assertEqual(result["overall_status"], "PASS")

    def test_toa_ros_uses_slope_surface_distance(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            case = Path(directory)
            expected_surface_ros = 1.0
            self.make_variant(
                case,
                expected_ros=expected_surface_ros,
                expected_ir=100.0,
                toa_required=True,
                toa="valid",
            )
            variant = case / "variants/example"
            with rasterio.open(variant / "inputs/phi.tif") as source:
                rows, cols = np.indices(source.shape)
                xs, ys = rasterio.transform.xy(
                    source.transform, rows, cols, offset="center"
                )
            x = np.asarray(xs).reshape((140, 140))
            y = np.asarray(ys).reshape((140, 140))
            angle = 45.0
            surface_x = -400.0 + (x + 400.0) / math.cos(math.radians(angle))
            arrival = np.full((140, 140), -9999.0)
            mask = (surface_x > -400.0) & (surface_x <= -150.0) & (np.abs(y) <= 150.0)
            arrival[mask] = (surface_x[mask] + 400.0) / (expected_surface_ros / 60.0)
            self.write_raster(
                variant / "outputs/time_of_arrival_0000001_0000100.tif",
                arrival,
            )
            measured, r2 = EVALUATOR._toa_ros(
                variant / "outputs/time_of_arrival_0000001_0000100.tif",
                variant / "inputs/phi.tif",
                slope_degrees=angle,
            )
            self.assertAlmostEqual(measured, expected_surface_ros, places=6)
            self.assertGreaterEqual(r2, 0.999999)

    def test_binary_front_is_rejected_as_unresolved(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            case = Path(directory)
            self.make_variant(
                case,
                expected_ros=0.05,
                expected_ir=100.0,
                toa_required=False,
            )
            phi = np.ones((140, 140))
            phi[40:100, 25:30] = -1.0
            self.write_raster(case / "variants/example/inputs/phi.tif", phi)
            result = EVALUATOR.evaluate(case)
            self.assertEqual(result["overall_status"], "NOT EVALUABLE")
            self.assertIn(
                "front_initialization", result["missing_evidence"]["example"]
            )

    def test_direct_field_grid_must_match_current_front(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            case = Path(directory)
            self.make_variant(
                case,
                expected_ros=0.05,
                expected_ir=100.0,
                toa_required=False,
            )
            shifted = from_origin(-690.0, 700.0, 10.0, 10.0)
            self.write_raster(
                case / "variants/example/outputs/vs_0000001_0000100.tif",
                np.full((140, 140), 0.05),
                transform=shifted,
            )
            result = EVALUATOR.evaluate(case)
            self.assertEqual(result["overall_status"], "NOT EVALUABLE")
            self.assertIn(
                "direct_grid_alignment", result["missing_evidence"]["example"]
            )

    def test_toa_grid_must_match_current_front(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            case = Path(directory)
            self.make_variant(
                case,
                expected_ros=1.0,
                expected_ir=100.0,
                toa_required=True,
                toa="valid",
            )
            shifted = from_origin(-690.0, 700.0, 10.0, 10.0)
            self.write_raster(
                case
                / "variants/example/outputs/time_of_arrival_0000001_0000100.tif",
                np.ones((140, 140)),
                transform=shifted,
            )
            result = EVALUATOR.evaluate(case)
            self.assertEqual(result["overall_status"], "NOT EVALUABLE")
            self.assertIn(
                "time_of_arrival_fit", result["missing_evidence"]["example"]
            )

    def test_case_local_helper_and_evaluator_copies_are_synchronized(self) -> None:
        evaluator_cases = (
            "unit_tests/CASE34_FMS",
            "unit_tests/CASE35_WSS",
            "unit_tests/CASE36_SLS",
            "unit_tests/CASE37_DMS",
            "unit_tests/CASE38_LMS",
            "unit_tests/CASE39_DHC",
            "unit_tests/CASE41_CFP",
            "coupling_tests/CASE42_WAF",
        )
        helper_cases = evaluator_cases + ("coupling_tests/CASE40_WSD",)
        verification = ROOT / "cases/Verification"
        for filename, cases in (
            ("case_support.py", helper_cases),
            ("evaluate_outputs.py", evaluator_cases),
        ):
            digests = {
                hashlib.sha256(
                    (verification / case / "scripts" / filename).read_bytes()
                ).hexdigest()
                for case in cases
            }
            self.assertEqual(len(digests), 1, filename)


class CoupledFrontEvidenceTests(unittest.TestCase):
    def test_case31_requires_coverage_at_every_planarity_station(self) -> None:
        arrival = np.full((300, 300), 100.0)
        transform = from_origin(-750.0, 750.0, 5.0, 5.0)
        # Remove more than ten percent of the central corridor at only one of
        # the six declared vertical stations.  A median-over-stations
        # evaluator used to hide this incomplete front.
        x = -750.0 + (np.arange(300) + 0.5) * 5.0
        column = int(np.argmin(np.abs(x - CASE31.UPSTREAM_STATION_X_M)))
        arrival[80:101, column] = np.nan
        self.assertTrue(
            math.isnan(
                CASE31.normalized_cross_front_variation(
                    arrival, transform, diagonal=False
                )
            )
        )

    def test_case31_resolves_toa_from_the_final_dump_record(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            old_case_dir = CASE31.CASE_DIR
            CASE31.CASE_DIR = Path(directory)
            try:
                output = Path(directory) / "variants/example/outputs"
                output.mkdir(parents=True)
                (output / "dump_times_0000001.csv").write_text(
                    "dump_index,time_seconds,is_final_dump\n"
                    "1,1200,F\n2,43200,T\n",
                    encoding="utf-8",
                )
                expected = output / "time_of_arrival_0000001_0043200.tif"
                expected.touch()
                (output / "time_of_arrival_0000001_0001200.tif").touch()
                path, reason = CASE31.path_for("example")
                self.assertEqual(path, expected)
                self.assertIsNone(reason)
            finally:
                CASE31.CASE_DIR = old_case_dir

    def test_case31_gapped_variant_must_reach_planned_stop(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            old_case_dir = CASE31.CASE_DIR
            CASE31.CASE_DIR = Path(directory)
            try:
                output = Path(directory) / "variants/gapped_break/outputs"
                output.mkdir(parents=True)
                (output / "dump_times_0000001.csv").write_text(
                    "dump_index,time_seconds,is_final_dump\n1,18000,T\n",
                    encoding="utf-8",
                )
                (output / "time_of_arrival_0000001_0018000.tif").touch()
                path, reason = CASE31.path_for("gapped_break")
                self.assertIsNone(path)
                self.assertIn("expected 43200", reason)
            finally:
                CASE31.CASE_DIR = old_case_dir

    def test_case31_gapped_variant_accepts_terminal_overshoot(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            old_case_dir = CASE31.CASE_DIR
            CASE31.CASE_DIR = Path(directory)
            try:
                output = Path(directory) / "variants/gapped_break/outputs"
                output.mkdir(parents=True)
                (output / "dump_times_0000001.csv").write_text(
                    "dump_index,time_seconds,is_final_dump\n1,43205,T\n",
                    encoding="utf-8",
                )
                expected = output / "time_of_arrival_0000001_0043205.tif"
                expected.touch()
                path, reason = CASE31.path_for("gapped_break")
                self.assertEqual(path, expected)
                self.assertIsNone(reason)
            finally:
                CASE31.CASE_DIR = old_case_dir

    def test_case40_requires_the_configured_terminal_time(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory)
            (output / "dump_times_0000001.csv").write_text(
                "dump_index,time_seconds,is_final_dump\n1,1700,T\n",
                encoding="utf-8",
            )
            (output / "time_of_arrival_0000001_0001700.tif").touch()
            (output / "ir_0000001_0001700.tif").touch()
            toa, intensity, reason = CASE40.terminal_files(output, 1800.0)
            self.assertIsNone(toa)
            self.assertIsNone(intensity)
            self.assertIn("expected 1800", reason)

    def test_case40_obsolete_manifest_becomes_not_evaluable(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            old_case_dir = CASE40.CASE_DIR
            CASE40.CASE_DIR = Path(directory)
            try:
                (Path(directory) / "variants").mkdir()
                (Path(directory) / "variants/expected.json").write_text(
                    '{"variants": []}\n', encoding="utf-8"
                )
                CASE40.main()
                result = json.loads(
                    (Path(directory) / "outputs/metrics.json").read_text()
                )
                self.assertEqual(result["overall_status"], "NOT EVALUABLE")
                self.assertIn("metadata", result["reason"])
            finally:
                CASE40.CASE_DIR = old_case_dir

    def test_case40_invalid_numeric_manifest_becomes_not_evaluable(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            old_case_dir = CASE40.CASE_DIR
            CASE40.CASE_DIR = Path(directory)
            try:
                (Path(directory) / "variants").mkdir()
                manifest = {
                    "simulation_tstop_s": 1800.0,
                    "slope_degrees": 26.565,
                    "tolerances": {
                        "direction_degrees": "not-a-number",
                        "head_ros_relative_error": 0.05,
                        "length_width_relative_error": 0.10,
                        "ir_relative_error": 0.01,
                    },
                    "variants": [
                        {
                            "id": "angle_000",
                            "relative_angle_degrees": 0.0,
                            "expected_direction_degrees": 0.0,
                            "expected_head_ros_m_min": 1.0,
                            "expected_length_width": 2.0,
                            "expected_ir_kw_m2": 100.0,
                        }
                    ],
                }
                (Path(directory) / "variants/expected.json").write_text(
                    json.dumps(manifest), encoding="utf-8"
                )
                CASE40.main()
                result = json.loads(
                    (Path(directory) / "outputs/metrics.json").read_text()
                )
                self.assertEqual(result["overall_status"], "NOT EVALUABLE")
                self.assertIn("malformed", result["reason"])
            finally:
                CASE40.CASE_DIR = old_case_dir


if __name__ == "__main__":
    unittest.main()
