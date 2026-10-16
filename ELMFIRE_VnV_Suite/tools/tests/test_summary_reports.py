import importlib.util
import sys
import tempfile
import unittest
from pathlib import Path


MODULE_PATH = Path(__file__).resolve().parents[1] / "generate_summary_reports.py"
sys.path.insert(0, str(MODULE_PATH.parent))
SPEC = importlib.util.spec_from_file_location("generate_summary_reports", MODULE_PATH)
REPORTS = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
sys.modules[SPEC.name] = REPORTS
SPEC.loader.exec_module(REPORTS)


class DecisionNormalizationTests(unittest.TestCase):
    def test_supported_decisions(self) -> None:
        examples = {
            "pass": "PASS",
            "FAIL": "FAIL",
            "NOT EVALUATED (capability missing)": "NOT EVALUABLE",
            "NOT RUN": "NOT EVALUABLE",
            "CHARACTERIZED": "CHARACTERIZED",
            True: "PASS",
            False: "FAIL",
        }
        for raw, expected in examples.items():
            with self.subTest(raw=raw):
                self.assertEqual(REPORTS.normalize_decision(raw), expected)

    def test_overall_status_has_priority(self) -> None:
        decision, raw, detail = REPORTS.extract_decision(
            {"overall_status": "FAIL", "status": "pass", "verification_passed": True}
        )
        self.assertEqual(decision, "FAIL")
        self.assertEqual(raw, "FAIL")
        self.assertIn("overall_status", detail)

    def test_unknown_or_missing_status_is_not_evaluable(self) -> None:
        self.assertIsNone(REPORTS.normalize_decision("looks reasonable"))
        decision, raw, _ = REPORTS.extract_decision({"metric": 1.0})
        self.assertEqual(decision, "NOT EVALUABLE")
        self.assertEqual(raw, "missing")


class LinkedSummaryTests(unittest.TestCase):
    def make_summary(self) -> object:
        return REPORTS.CaseSummary(
            case_id="CASE15_CRO",
            title="Constant rate of spread",
            category="unit_tests",
            elmfire_command="${ELMFIRE_BIN:-elmfire}",
            elmfire_config="data/inputs/elmfire.data",
            mpi_ranks="1",
            namelist_schema="1",
            relative_directory="cases/Verification/unit_tests/CASE15_CRO",
            report_pdf=(
                "cases/Verification/unit_tests/CASE15_CRO/report/case_report.pdf"
            ),
            report_available=True,
            metrics_file=(
                "cases/Verification/unit_tests/CASE15_CRO/outputs/metrics.json"
            ),
            metrics_available=True,
            raw_status="pass",
            decision="PASS",
            detail="test",
        )

    def test_case_include_and_summary_share_a_hyperlink_target(self) -> None:
        summary = self.make_summary()
        environment = {"Python runtime": "CPython test"}
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            include_path = root / "cases.tex"
            summary_path = root / "summary.tex"
            REPORTS.write_case_includes(include_path, [summary])
            REPORTS.write_summary_table(
                summary_path, "verification", [summary], environment
            )

            label = REPORTS.case_label(summary)
            self.assertIn(f",{label}}}]", include_path.read_text(encoding="utf-8"))
            summary_text = summary_path.read_text(encoding="utf-8")
            self.assertIn(f"\\hyperref[{label}]", summary_text)
            self.assertIn("Computational environment recorded during report preparation", summary_text)
            self.assertIn("Comparison type and declared parallel allocation", summary_text)
            self.assertNotIn("Passed?", summary_text)
            self.assertIn("Isolated-process verification", summary_text)
            for hidden in ("case.yaml", "metrics.json", "elmfire.data", "Schema",
                           "ELMFIRE command", "unit\\_tests"):
                self.assertNotIn(hidden, summary_text)

    def test_environment_excludes_installation_paths(self) -> None:
        environment = {
            "Repository revision": "suite-not-model",
            "ELMFIRE command": "/private/model/bin/elmfire",
            "Python runtime": "CPython (/private/env/bin/python)",
            "ELMFIRE version declaration": "2025.1002",
            "MPI launcher": "Open MPI 4.1.6",
            "Python version": "CPython 3.11.9",
        }
        shown = REPORTS.report_environment(environment)
        self.assertEqual(shown["Declared ELMFIRE release"], "2025.1002")
        self.assertNotIn("/private", str(shown))
        self.assertNotIn("suite-not-model", str(shown))

    def test_release_path_is_not_misrepresented_as_a_version(self) -> None:
        shown = REPORTS.report_environment({"ELMFIRE version declaration": "/build/elmfire"})
        self.assertEqual(shown["Declared ELMFIRE release"],
                         "not documented in the available case materials")


if __name__ == "__main__":
    unittest.main()
