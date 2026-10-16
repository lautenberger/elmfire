import importlib.util
import tempfile
import unittest
from pathlib import Path


MODULE_PATH = Path(__file__).resolve().parents[1] / "run_all.py"
SPEC = importlib.util.spec_from_file_location("run_all", MODULE_PATH)
RUN_ALL = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(RUN_ALL)


class DiscoverCasesTests(unittest.TestCase):
    def make_case(self, root: Path, relative: str) -> None:
        case = root / relative
        case.mkdir(parents=True)
        (case / "run_case.sh").write_text("#!/usr/bin/env bash\n", encoding="utf-8")

    def test_suite_filtering_happens_before_sharding(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            cases = Path(directory) / "cases"
            self.make_case(cases, "Verification/unit_tests/CASE01_ONE")
            self.make_case(cases, "Verification/coupling_tests/CASE02_TWO")
            self.make_case(cases, "Validation/landscape_scale/fire_one")

            all_cases = RUN_ALL.discover_cases(str(cases), "all")
            verification = RUN_ALL.discover_cases(str(cases), "verification")
            validation = RUN_ALL.discover_cases(str(cases), "validation")

            self.assertEqual(len(all_cases), 3)
            self.assertEqual(len(verification), 2)
            self.assertEqual(len(validation), 1)
            start, end = RUN_ALL.shard_slice(len(verification), 2, 1)
            self.assertEqual(verification[start:end], [verification[1]])

    def test_templates_and_legacy_material_are_excluded(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            cases = Path(directory) / "cases"
            self.make_case(cases, "case_template")
            self.make_case(cases, "Verification/coupling_tests/__legacy__/old_case")
            self.make_case(cases, "Validation/structure_scale/current_case")

            discovered = RUN_ALL.discover_cases(str(cases), "all")
            self.assertEqual(len(discovered), 1)
            self.assertIn("current_case", discovered[0])

    def test_unknown_scope_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            with self.assertRaises(ValueError):
                RUN_ALL.discover_cases(str(Path(directory) / "cases"), "other")

    def test_slurm_wrapper_executes_original_runner_without_embedding_it(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            case = root / "case with spaces"
            case.mkdir()
            marker = "RUN_CASE_BODY_MUST_NOT_BE_EMBEDDED"
            (case / "run_case.sh").write_text(
                f"#!/usr/bin/env bash\necho {marker}\n", encoding="utf-8"
            )
            header = root / "slurm_head.txt"
            header.write_text(
                "#SBATCH --job-name=elmfire-generic\n"
                "#SBATCH --ntasks=1\n"
                "module purge\n",
                encoding="utf-8",
            )

            wrapper = Path(RUN_ALL.make_slurm_wrapper(str(case), str(header)))
            text = wrapper.read_text(encoding="utf-8")

            self.assertNotIn(marker, text)
            self.assertNotIn("elmfire-generic", text)
            self.assertEqual(text.count("#SBATCH --job-name="), 1)
            self.assertIn("#SBATCH --job-name=elmfire-case-with-spaces", text)
            self.assertIn(f"cd -- '{case}'", text)
            self.assertIn(f"exec bash '{case / 'run_case.sh'}'", text)
            self.assertTrue(wrapper.stat().st_mode & 0o100)

    def test_suite_specific_slurm_headers(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            cases = root / "cases"
            common = root / "common"
            verification = cases / "Verification/unit_tests/CASE01_ONE/run_case.sh"
            validation = cases / "Validation/landscape_scale/fire/run_case.sh"

            self.assertEqual(
                Path(RUN_ALL.slurm_header_for_case(verification, cases, common)).name,
                "slurm_verification_head.txt",
            )
            self.assertEqual(
                Path(RUN_ALL.slurm_header_for_case(validation, cases, common)).name,
                "slurm_validation_head.txt",
            )

    def test_slurm_job_name_preserves_canonical_case_number(self) -> None:
        self.assertEqual(
            RUN_ALL.slurm_job_name("/suite/cases/CASE08_FBC"),
            "elmfire-CASE08_FBC",
        )
        self.assertEqual(
            RUN_ALL.slurm_job_name("/suite/cases/camp_fire"),
            "elmfire-camp_fire",
        )

    def test_validation_slurm_header_uses_one_rank_per_member(self) -> None:
        root = MODULE_PATH.parents[1]
        header = (root / "common/slurm_validation_head.txt").read_text(
            encoding="utf-8"
        )

        self.assertIn("#SBATCH --nodes=1", header)
        self.assertIn("#SBATCH --ntasks=50", header)
        self.assertIn("#SBATCH --ntasks-per-node=50", header)
        self.assertIn("export PROJ_NETWORK=OFF", header)
        self.assertIn('export ELMFIRE_MPI_RANKS="${SLURM_NTASKS:-50}"', header)
        self.assertNotIn('ELMFIRE_MPI_RANKS:-${SLURM_NTASKS', header)
        self.assertNotIn("#SBATCH --ntasks=51", header)

    def test_landscape_runners_do_not_require_compile_script_execute_bit(self) -> None:
        root = MODULE_PATH.parents[1]
        cases = root / "cases/Validation/landscape_scale"

        for case_id in ("camp_fire", "thomas_fire", "tubbs_fire"):
            runner = (cases / case_id / "run_case.sh").read_text(encoding="utf-8")
            with self.subTest(case_id=case_id):
                self.assertIn('if [[ -n "${SLURM_NTASKS:-}" ]]', runner)
                self.assertIn("ELMFIRE_MPI_RANKS=$SLURM_NTASKS", runner)
                self.assertIn('ELMFIRE_MPI_RANKS=${ELMFIRE_MPI_RANKS:-50}', runner)
                self.assertIn('bash "$CASE_DIR/compile_case.sh"', runner)
                self.assertIn("export PROJ_NETWORK=OFF", runner)


if __name__ == "__main__":
    unittest.main()
