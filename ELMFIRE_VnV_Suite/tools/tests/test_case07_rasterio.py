"""Regression tests for CASE07's Rasterio-only raster access."""

from __future__ import annotations

import csv
import importlib.util
from pathlib import Path
import sys
import tempfile
import types
import unittest

import numpy as np
import rasterio
from rasterio.transform import from_origin


REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_DIR = REPO_ROOT / "cases/Verification/coupling_tests/CASE07_IPC/scripts"


def load_module(name: str, filename: str, stub_spatial_evidence: bool = False):
    """Load one case-local script without relying on the caller's directory."""
    previous = sys.modules.get("spatial_evidence")
    if stub_spatial_evidence:
        helper = types.ModuleType("spatial_evidence")
        helper.generate_spatial_evidence = lambda *args, **kwargs: None
        sys.modules["spatial_evidence"] = helper
    try:
        spec = importlib.util.spec_from_file_location(name, SCRIPT_DIR / filename)
        module = importlib.util.module_from_spec(spec)
        assert spec.loader is not None
        spec.loader.exec_module(module)
        return module
    finally:
        if stub_spatial_evidence:
            if previous is None:
                del sys.modules["spatial_evidence"]
            else:
                sys.modules["spatial_evidence"] = previous


PREPROCESS = load_module("case07_preprocess_test", "preprocess.py")
POSTPROCESS = load_module(
    "case07_postprocess_test", "postprocess.py", stub_spatial_evidence=True
)


def write_test_raster(path: Path, values: np.ndarray, transform) -> None:
    """Write one small GeoTIFF used by the postprocessor tests."""
    with rasterio.open(
        path,
        "w",
        driver="GTiff",
        height=values.shape[0],
        width=values.shape[1],
        count=1,
        dtype="float32",
        crs="EPSG:32610",
        transform=transform,
        nodata=-9999.0,
    ) as dataset:
        dataset.write(values.astype(np.float32), 1)


class Case07RasterioTests(unittest.TestCase):
    def test_preprocessor_writer_preserves_grid_contract(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            path = Path(temporary_directory) / "input.tif"
            values = np.arange(24, dtype=np.float32).reshape(4, 6)
            PREPROCESS.write_tif(path, values, 10.0, np.float32)

            with rasterio.open(path) as dataset:
                self.assertEqual((dataset.height, dataset.width), values.shape)
                self.assertEqual(dataset.crs.to_epsg(), 32610)
                self.assertEqual(
                    dataset.transform.to_gdal(), (-20.0, 10.0, 0.0, 20.0, 0.0, -10.0)
                )
                self.assertEqual(dataset.nodata, -9999.0)
                np.testing.assert_array_equal(dataset.read(1), values)

    def test_member_ros_reads_rasterio_georeferencing(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            output = root / "variant/outputs"
            output.mkdir(parents=True)
            transform = from_origin(0.0, 80.0, 10.0, 10.0)
            x = 5.0 + np.arange(40) * 10.0
            toa = np.tile(x / 2.0, (8, 1))
            write_test_raster(output / "time_of_arrival_0000001_0000240.tif", toa, transform)

            original_case_dir = POSTPROCESS.CASE_DIR
            POSTPROCESS.CASE_DIR = root
            try:
                normalized_ros, filename, fit_cells = POSTPROCESS.read_member_ros(
                    {
                        "working_directory": "variant",
                        "pign": 0.5,
                        "wind_speed_mps": 2.0,
                    }
                )
            finally:
                POSTPROCESS.CASE_DIR = original_case_dir

            self.assertAlmostEqual(normalized_ros, 1.0)
            self.assertEqual(filename, "time_of_arrival_0000001_0000240.tif")
            self.assertGreaterEqual(fit_cells, POSTPROCESS.MIN_FIT_CELLS)

    def test_sft_delay_reads_final_and_transient_rasters(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            output = root / "variant/outputs"
            output.mkdir(parents=True)
            transform = from_origin(0.0, 80.0, 10.0, 10.0)
            shape = (8, 40)
            toa = np.full(shape, -9999.0, dtype=np.float32)
            ignition = np.zeros(shape, dtype=np.float32)
            deposited = np.zeros(shape, dtype=np.float32)
            row = 4
            toa[row, 16:20] = 6.0
            ignition[row, 16:20] = 1.0
            deposited[row, 16:20] = 1.0
            write_test_raster(output / "time_of_arrival_0000001_0000240.tif", toa, transform)
            write_test_raster(output / "ember_ignition_0000001_0000240.tif", ignition, transform)
            write_test_raster(
                output / "ember_flux_transient_0000001_d0000001.tif",
                deposited,
                transform,
            )
            with (output / "dump_times_0000001.csv").open(
                    "w", newline="", encoding="utf-8") as stream:
                writer = csv.DictWriter(
                    stream, fieldnames=("dump_index", "time_seconds", "is_final_dump")
                )
                writer.writeheader()
                writer.writerow(
                    {"dump_index": 1, "time_seconds": 2.0, "is_final_dump": "F"}
                )

            original_case_dir = POSTPROCESS.CASE_DIR
            POSTPROCESS.CASE_DIR = root
            try:
                delays, provenance = POSTPROCESS.read_sft_delays(
                    {"working_directory": "variant", "ny": 8, "nx": 40}
                )
            finally:
                POSTPROCESS.CASE_DIR = original_case_dir

            np.testing.assert_array_equal(delays, np.full(4, 5.0))
            self.assertEqual(provenance["sample_count"], 4)


if __name__ == "__main__":
    unittest.main()
