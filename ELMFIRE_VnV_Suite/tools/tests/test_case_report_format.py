"""Guard the agreed presentation shared by independently compiled reports."""
import re
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / 'tools'))
from generate_summary_reports import discover_case_summaries


class CaseReportFormatTests(unittest.TestCase):
    def test_all_cases_use_local_canonical_style_and_common_page_geometry(self):
        canonical = (ROOT / 'main_report/report_style.tex').read_bytes()
        for suite in ('verification', 'validation'):
            for case in discover_case_summaries(suite):
                with self.subTest(case=case.case_id):
                    report = ROOT / case.relative_directory / 'report'
                    wrapper = (report / 'case_report.tex').read_text()
                    self.assertEqual((report / 'report_style.tex').read_bytes(), canonical)
                    self.assertIn(r'\documentclass[12pt]{article}', wrapper)
                    self.assertIn(r'\usepackage[letterpaper,margin=0.85in]{geometry}', wrapper)
                    self.assertIn(r'\input{report_style.tex}', wrapper)
                    self.assertLess(wrapper.index(r'\input{report_style.tex}'), wrapper.index(r'\begin{document}'))
                    self.assertNotRegex(wrapper, r'\\setlength\{\\par(?:indent|skip)\}')

    def test_purpose_has_no_identity_label_or_detached_glossary(self):
        for suite in ('verification', 'validation'):
            headings = set()
            for case in discover_case_summaries(suite):
                with self.subTest(case=case.case_id):
                    body = (ROOT / case.relative_directory / 'report/case_body.tex').read_text()
                    headings.add(tuple(re.findall(r'\\section\{([^}]+)\}', body)))
                    self.assertNotRegex(body, r'\\(?:paragraph|subsection|section)\{(?:Terminology|Reading the notation|Notation|Acronyms)')
                    first_section = body.split(r'\section{', 2)[1]
                    self.assertNotIn(r'\CaseID', first_section)
                    self.assertNotIn('Case ID:', first_section)
                    self.assertNotRegex(body, r'\(([A-Z]+)\)\s*\(\1\)')
            self.assertEqual(len(headings), 1, suite)
            self.assertEqual(len(next(iter(headings))), 9 if suite == 'verification' else 10)
