"""Regression tests for CASE09's Rasterio-only raster access."""

from __future__ import annotations

import importlib.util
from pathlib import Path
import sys
import tempfile
import types
import unittest

import numpy as np
import rasterio


REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_DIR = REPO_ROOT / "cases/Verification/coupling_tests/CASE09_MMD/scripts"


def load_module(name: str, filename: str, stub_spatial_evidence: bool = False):
    """Load one case-local script without depending on the caller's directory."""
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


PREPROCESS = load_module("case09_preprocess_test", "preprocess.py")
POSTPROCESS = load_module(
    "case09_postprocess_test", "postprocess.py", stub_spatial_evidence=True
)


class Case09RasterioTests(unittest.TestCase):
    def test_case_scripts_do_not_import_osgeo(self) -> None:
        for filename in ("preprocess.py", "postprocess.py", "spatial_evidence.py"):
            source = (SCRIPT_DIR / filename).read_text(encoding="utf-8")
            self.assertNotIn("from osgeo", source, filename)
            self.assertNotIn("import osgeo", source, filename)

    def test_preprocessor_writer_preserves_grid_contract(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            path = Path(temporary_directory) / "input.tif"
            values = np.arange(24, dtype=np.float32).reshape(4, 6)
            PREPROCESS.write_tif(path, values, 10.0, 2, np.float32)

            with rasterio.open(path) as dataset:
                self.assertEqual((dataset.height, dataset.width), values.shape)
                self.assertEqual(dataset.crs.to_epsg(), 32610)
                self.assertEqual(
                    dataset.transform.to_gdal(),
                    (-20.0, 10.0, 0.0, 20.0, 0.0, -10.0),
                )
                self.assertEqual(dataset.nodata, -9999.0)
                np.testing.assert_array_equal(dataset.read(1), values)

    def test_postprocessor_reader_preserves_values_and_georeferencing(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            path = Path(temporary_directory) / "output.tif"
            values = np.arange(24, dtype=np.float32).reshape(4, 6)
            PREPROCESS.write_tif(path, values, 10.0, 2, np.float32)

            observed, transform, nodata = POSTPROCESS.read_raster(path)

            np.testing.assert_array_equal(observed, values)
            self.assertEqual(transform, (-20.0, 10.0, 0.0, 20.0, 0.0, -10.0))
            self.assertEqual(nodata, -9999.0)


if __name__ == "__main__":
    unittest.main()
