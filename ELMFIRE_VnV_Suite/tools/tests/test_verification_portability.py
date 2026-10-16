"""Regression checks for running verification cases outside an ELMFIRE source tree."""

from __future__ import annotations

import re
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
VERIFICATION = ROOT / "cases" / "Verification"
SPOTTING = VERIFICATION / "coupling_tests"


class VerificationPortabilityTests(unittest.TestCase):
    def spotting_cases(self) -> list[Path]:
        return sorted(SPOTTING.glob("CASE[01][0-9]_*"))[:14]

    def active_cases(self) -> list[Path]:
        return sorted(
            path.parent
            for category in ("unit_tests", "coupling_tests")
            for path in (VERIFICATION / category).glob("CASE*/run_case.sh")
        )

    def test_all_fourteen_spotting_cases_are_present(self) -> None:
        cases = self.spotting_cases()
        self.assertEqual(len(cases), 14)
        self.assertEqual(
            [case.name[:6] for case in cases],
            [f"CASE{number:02d}" for number in range(1, 15)],
        )

    def test_spotting_preprocessors_do_not_search_elmfire_source_tree(self) -> None:
        forbidden = ("build/source", "repository_root", "feature_found_in_source")
        for case in self.spotting_cases():
            for script in (case / "scripts").glob("*.py"):
                text = script.read_text(encoding="utf-8")
                for token in forbidden:
                    self.assertNotIn(token, text, f"{script} contains {token!r}")

    def test_spotting_templates_use_only_reader_supported_domain_metadata(self) -> None:
        for case in self.spotting_cases():
            namelist = case / "elmfire.data.in"
            if namelist.is_file():
                self.assertNotIn("&COMPUTATIONAL_DOMAIN",
                                 namelist.read_text(encoding="utf-8"), case.name)

    def test_runnable_spotting_cases_own_required_model_tables(self) -> None:
        for case in self.spotting_cases():
            if case.name.startswith("CASE13_"):
                continue  # Capability-only design; it does not launch ELMFIRE.
            for name in ("fuel_models.csv", "building_fuel_models.csv"):
                self.assertTrue((case / "data" / "misc" / name).is_file(),
                                f"{case.name} is missing data/misc/{name}")

    def test_case_runners_are_location_independent(self) -> None:
        absolute_path = re.compile(r"/(?:Users|home|global)/")
        for case in self.active_cases():
            runner = (case / "run_case.sh").read_text(encoding="utf-8")
            self.assertIn("BASH_SOURCE[0]", runner, case.name)
            self.assertIsNone(absolute_path.search(runner), case.name)

    def test_case_runners_define_the_elmfire_executable_they_use(self) -> None:
        default = re.compile(
            r"ELMFIRE_BIN\s*=\s*[\"']?\$\{ELMFIRE_BIN:-elmfire\}"
        )
        for case in self.active_cases():
            runner = (case / "run_case.sh").read_text(encoding="utf-8")
            if "$ELMFIRE_BIN" in runner or "${ELMFIRE_BIN}" in runner:
                self.assertRegex(runner, default, case.name)

    def test_case_python_does_not_require_gdal_bindings(self) -> None:
        """Keep case scripts independent of the ABI-sensitive osgeo package."""
        forbidden = re.compile(r"(?:from\s+osgeo\b|import\s+osgeo\b)")
        for case in self.active_cases():
            for script in (case / "scripts").glob("*.py"):
                source = script.read_text(encoding="utf-8")
                self.assertIsNone(
                    forbidden.search(source),
                    f"{script} imports osgeo; use Rasterio for Python raster I/O",
                )

    def test_case_runners_ignore_user_site_packages(self) -> None:
        """Prevent ~/.local packages from shadowing the selected HPC environment."""
        for case in self.active_cases():
            runner = (case / "run_case.sh").read_text(encoding="utf-8")
            self.assertRegex(
                runner,
                r"(?m)^export\s+PYTHONNOUSERSITE=1\s*$",
                case.name,
            )

    def test_case_namelists_do_not_hard_code_system_gdal(self) -> None:
        """Let ELMFIRE resolve the GDAL utilities from the active environment."""
        for case in self.active_cases():
            for namelist in (case / "elmfire.data.in", case / "elmfire.data"):
                if not namelist.is_file():
                    continue
                source = namelist.read_text(encoding="utf-8")
                self.assertNotRegex(
                    source,
                    r"(?mi)^\s*PATH_TO_GDAL\s*=\s*['\"]?/usr/bin/?['\"]?",
                    str(namelist),
                )

    def test_rothermel_sweeps_generate_bounded_phi_inputs(self) -> None:
        common_cases = (
            "unit_tests/CASE34_FMS",
            "unit_tests/CASE35_WSS",
            "unit_tests/CASE36_SLS",
            "unit_tests/CASE37_DMS",
            "unit_tests/CASE38_LMS",
            "unit_tests/CASE39_DHC",
            "unit_tests/CASE41_CFP",
            "coupling_tests/CASE42_WAF",
        )
        bounded_expression = (
            "np.clip(signed_distance_m / CELL_SIZE_M, -1.0, 1.0)"
        )
        for relative in common_cases:
            helper = VERIFICATION / relative / "scripts/case_support.py"
            text = helper.read_text(encoding="utf-8")
            self.assertIn(bounded_expression, text)
            self.assertIn("TARGET_FRONT_ADVANCE_FRACTION = 0.05", text)
            self.assertIn('"SIMULATION_DTMAX": round(timestep_max_s, 6)', text)

        wind_slope = (
            VERIFICATION
            / "coupling_tests/CASE40_WSD/scripts/preprocess.py"
        ).read_text(encoding="utf-8")
        self.assertIn("np.hypot(xx, surface_y) - INITIAL_RADIUS_M", wind_slope)
        self.assertIn("timestep_for_ros(head_ros_m_min)", wind_slope)

        planar_front = (
            VERIFICATION
            / "coupling_tests/CASE31_PFT/scripts/preprocess.py"
        ).read_text(encoding="utf-8")
        self.assertIn(
            "np.clip(signed_distance / CELL, -1.0, 1.0)", planar_front
        )

    def test_case_variants_are_not_nested_under_data(self) -> None:
        for case in self.active_cases():
            self.assertFalse(
                (case / "data/variants").exists(),
                f"{case.name} stores generated variants below data/",
            )

    def test_case19_accepts_indexed_dump_names_and_physical_times(self) -> None:
        helper = (
            VERIFICATION
            / "coupling_tests/CASE19_WTH/scripts/raster_functions.py"
        ).read_text(encoding="utf-8")
        self.assertIn(r'_d?(\d{7})\.tif$', helper)
        self.assertIn('row["time_seconds"]', helper)

    def test_guide_adapter_notes_match_the_active_contracts(self) -> None:
        for number in range(20, 30):
            matches = list(SPOTTING.glob(f"CASE{number:02d}_*/scripts/guide_verification.py"))
            self.assertEqual(len(matches), 1, f"CASE{number:02d}")
            text = matches[0].read_text(encoding="utf-8")
            self.assertNotIn("ember count vs 1036", text)
            self.assertNotIn("analytically ~10800 s", text)

    def test_guide_variant_namelists_have_sources_outside_variants(self) -> None:
        for number in range(20, 30):
            matches = list(SPOTTING.glob(f"CASE{number:02d}_*"))
            self.assertEqual(len(matches), 1, f"CASE{number:02d}")
            case = matches[0]
            adapter = (case / "scripts/case_adapter.py").read_text(encoding="utf-8")
            self.assertIn("materialize_variant_namelist", adapter)
            for raw in (case / "scripts/variants.tsv").read_text(
                encoding="utf-8"
            ).splitlines():
                if not raw.strip() or raw.lstrip().startswith("#"):
                    continue
                label, config_rel, _ = raw.split("\t")
                if not config_rel.startswith("variants/"):
                    continue
                special = case / "scripts/namelists" / f"{label}.in"
                self.assertTrue(
                    special.is_file() or (case / "elmfire.data.in").is_file(),
                    f"{case.name}/{label} has no source namelist",
                )

    def test_adapter_modules_are_case_local(self) -> None:
        for case in self.active_cases():
            scripts = case / "scripts"
            combined = "\n".join(
                path.read_text(encoding="utf-8") for path in scripts.glob("*.py")
            )
            for module in ("case_adapter", "guide_verification"):
                if re.search(rf"(?:from|import)\s+{module}\b", combined):
                    self.assertTrue((scripts / f"{module}.py").is_file(),
                                    f"{case.name} imports non-local {module}")


if __name__ == "__main__":
    unittest.main()
