"""Regression tests for CASE04 accumulation-reference handling."""

from __future__ import annotations

import importlib.util
from pathlib import Path
import sys
import unittest

import numpy as np


REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_DIR = REPO_ROOT / "cases/Verification/coupling_tests/CASE04_STI/scripts"
sys.path.insert(0, str(SCRIPT_DIR))

SPEC = importlib.util.spec_from_file_location(
    "case04_postprocess_test", SCRIPT_DIR / "postprocess.py"
)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)


class Case04AccumulationTests(unittest.TestCase):
    def test_zero_reference_is_not_evaluable(self) -> None:
        result, reference = MODULE.evaluate_accumulation_plateau(
            np.array([100.0, 100.0, 100.0]),
            np.array([0.0, 0.0, np.nan]),
            np.array([True, True, True]),
            minimum_cells=2,
            relative_error_limit=0.1,
        )

        self.assertEqual(result["accumulation_passed"], "NOT EVALUABLE")
        self.assertEqual(result["accumulation_reference_cells"], 0)
        self.assertTrue(np.isnan(reference))

    def test_only_positive_colocated_reference_cells_are_compared(self) -> None:
        result, reference = MODULE.evaluate_accumulation_plateau(
            np.array([999.0, 120.0, 180.0, 999.0]),
            np.array([0.0, 120.0, 180.0, np.nan]),
            np.array([True, True, True, True]),
            minimum_cells=2,
            relative_error_limit=0.1,
        )

        self.assertTrue(result["accumulation_passed"])
        self.assertEqual(result["accumulation_reference_cells"], 2)
        self.assertEqual(reference, 150.0)
        self.assertEqual(result["accumulation_plateau_density_pcs_m2"], 150.0)


if __name__ == "__main__":
    unittest.main()
