#!/usr/bin/env python3
"""Audit compiled reports and included figure PDFs without changing evidence.

Findings are review candidates, not automatic edits: published titles and the
scientific verb 'contract' may be legitimate. PDF text extraction cannot prove
visual readability or detect text embedded in raster images.
"""
from __future__ import annotations

import argparse
import json
import re
import subprocess
from pathlib import Path

from generate_summary_reports import ROOT_DIR, discover_case_summaries

PATTERNS = {
    "terminology": re.compile(
        r"\b(?:oracle|test harness|fixture|payload|artifact|contract|schema|registry|"
        r"adapter|pipeline|workflow|runtime dependency|machine-readable|source tree|"
        r"repository|implementation path|code path|stub|mock|backend|framework|variant|manifest)s?\b", re.I),
    "filename": re.compile(r"\b[^\s]+\.(?:py|sh|yaml|yml|json|csv|tsv|tiff?|tex|in|f90|rar|txt|pdf|md|gz)\b", re.I),
    "internal_path": re.compile(r"(?:/Users/|/global/|(?:cases|scripts|outputs|variants|data|build/source)/)"),
    "missing_reference": re.compile(r"(?:\?\?|MISSING:)")
}


def audit_pdf(path: Path) -> dict[str, object]:
    result = subprocess.run(["pdftotext", "-layout", str(path), "-"],
                            capture_output=True, text=True, check=False)
    record = {"path": str(path.relative_to(ROOT_DIR)), "extraction_exit_code": result.returncode,
              "pages": 0, "findings": []}
    if result.returncode:
        record["error"] = result.stderr.strip()
        return record
    pages = result.stdout.split("\f")
    if not pages[-1].strip():
        pages.pop()
    record["pages"] = len(pages)
    for page_number, page in enumerate(pages, 1):
        for line_number, line in enumerate(page.splitlines(), 1):
            for kind, pattern in PATTERNS.items():
                if pattern.search(line):
                    record["findings"].append({"page": page_number, "line": line_number,
                                               "kind": kind, "text": line.strip()})
    return record


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, help="Optional audit JSON; never a case metric record")
    parser.add_argument("--figures", action="store_true", help="Also inspect standalone figure PDFs")
    args = parser.parse_args()
    paths = []
    for suite in ("verification", "validation"):
        for case in discover_case_summaries(suite):
            paths.append(ROOT_DIR / case.report_pdf)
            if args.figures:
                paths.extend(sorted((ROOT_DIR / case.relative_directory / "figures").glob("*.pdf")))
        paths.append(ROOT_DIR / "main_report" / f"{suite}_report.pdf")
    records = [audit_pdf(p) if p.is_file() else {"path":str(p.relative_to(ROOT_DIR)),"error":"missing PDF"}
               for p in dict.fromkeys(paths)]
    payload = {"reports_and_figures": records,
               "visual_inspection_required": True,
               "note": "Candidates require contextual review; no scientific decisions were recalculated."}
    if args.output:
        # Prevent a typo from replacing a scientific data record.
        if args.output.resolve().is_relative_to((ROOT_DIR / "cases").resolve()):
            parser.error("Audit output must be outside the case directories")
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
    else:
        print(json.dumps(payload, indent=2))
    findings = sum(len(r.get("findings", [])) for r in records)
    print(f"Audited {len(records)} PDFs; {findings} contextual-review candidates.")


if __name__ == "__main__":
    main()
