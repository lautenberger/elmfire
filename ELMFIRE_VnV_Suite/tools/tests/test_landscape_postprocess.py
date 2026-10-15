"""Tests for stochastic landscape-ensemble TOA handling."""

from __future__ import annotations

import importlib.util
import json
import shutil
import subprocess
import sys
import tempfile
import unittest
import warnings
from pathlib import Path

import numpy as np
import rasterio
from rasterio.transform import from_origin


MODULE_PATH = (
    Path(__file__).resolve().parents[2]
    / "cases/Validation/landscape_scale/tubbs_fire/scripts/postprocess.py"
)
SPEC = importlib.util.spec_from_file_location("landscape_postprocess", MODULE_PATH)
POSTPROCESS = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(POSTPROCESS)


class LandscapePostprocessTests(unittest.TestCase):
    def write_raster(self, path: Path, values: np.ndarray) -> None:
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
            transform=from_origin(0.0, 2.0, 1.0, 1.0),
            nodata=-9999.0,
        ) as dst:
            dst.write(values.astype(np.float32), 1)

    def test_member_specific_nodata_is_an_unburned_outcome(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            case = Path(directory)
            (case / "elmfire.data.in").write_text(
                "&INPUTS\n"
                "FUELS_AND_TOPOGRAPHY_DIRECTORY='./data/inputs'\n"
                "ASP_FILENAME='asp'\n/\n",
                encoding="utf-8",
            )
            self.write_raster(
                case / "data/inputs/asp.tif",
                np.array([[-9999.0, 0.0], [0.0, 0.0]]),
            )
            first = case / "outputs/time_of_arrival_0000001_0000100.tif"
            second = case / "outputs/time_of_arrival_0000002_0000100.tif"
            self.write_raster(first, np.array([[-9999.0, 10.0], [20.0, -9999.0]]))
            self.write_raster(second, np.array([[-9999.0, -9999.0], [25.0, 30.0]]))

            previous = POSTPROCESS.CASE_DIR
            POSTPROCESS.CASE_DIR = case
            try:
                arrival, valid, metadata = POSTPROCESS.read_ensemble([first, second])
            finally:
                POSTPROCESS.CASE_DIR = previous

            np.testing.assert_array_equal(
                valid, np.array([[False, True], [True, True]])
            )
            self.assertTrue(np.isnan(arrival[1, 0, 1]))
            self.assertEqual(arrival[0, 0, 1], 10.0)
            self.assertEqual(arrival[1, 1, 1], 30.0)
            self.assertEqual(metadata["analysis_domain_valid_cells"], 3)

    def test_report_macro_converter_accepts_corrected_schema(self) -> None:
        source = MODULE_PATH.with_name("metrics_to_macro.py")
        with tempfile.TemporaryDirectory() as directory:
            case = Path(directory)
            (case / "scripts").mkdir()
            (case / "outputs").mkdir()
            (case / "report").mkdir()
            shutil.copy2(source, case / "scripts/metrics_to_macro.py")
            (case / "outputs/metrics.json").write_text(
                json.dumps(
                    {
                        "method_version": "landscape_validation_v3",
                        "status": "CHARACTERIZED",
                        "jaccard": 0.25,
                    }
                ),
                encoding="utf-8",
            )
            subprocess.run(
                [sys.executable, str(case / "scripts/metrics_to_macro.py")],
                check=True,
                env={"PYTHONDONTWRITEBYTECODE": "1"},
            )
            macros = (case / "report/metrics_macros.tex").read_text(encoding="utf-8")
            self.assertIn(r"\DefineMetric{status}{CHARACTERIZED}", macros)
            self.assertIn(r"\DefineMetric{jaccard}{0.25}", macros)

    def test_arrival_plot_handles_never_burned_cells_without_warning(self) -> None:
        arrival = np.array(
            [
                [[np.nan, 3600.0], [np.nan, 7200.0]],
                [[np.nan, np.nan], [np.nan, 10800.0]],
            ]
        )
        valid = np.ones((2, 2), dtype=bool)

        with tempfile.TemporaryDirectory() as directory:
            previous = POSTPROCESS.FIGURE_DIR
            POSTPROCESS.FIGURE_DIR = Path(directory)
            try:
                with warnings.catch_warnings():
                    warnings.simplefilter("error", RuntimeWarning)
                    POSTPROCESS.plot_arrival(arrival, valid)
            finally:
                POSTPROCESS.FIGURE_DIR = previous

            self.assertTrue((Path(directory) / "arrival_time_summary.pdf").is_file())


if __name__ == "__main__":
    unittest.main()
