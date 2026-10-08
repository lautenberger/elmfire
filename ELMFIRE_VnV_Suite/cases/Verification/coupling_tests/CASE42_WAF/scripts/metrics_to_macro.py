#!/usr/bin/env python3
"""Convert the authoritative metrics JSON into report-safe LaTeX rows."""

from report_language import report_text
import json
from pathlib import Path

CASE_DIR = Path(__file__).resolve().parents[1]
LATEX_BREAK = " " + chr(92) * 2


def tex(value):
    if value is None:
        return "--"
    text = f"{value:.6g}" if isinstance(value, float) else str(value)
    replacements = {
        "_": r"\_",
        "%": r"\%",
        "&": r"\&",
        "<=": r"$\leq$",
        ">=": r"$\geq$",
        "<": r"$<$",
        ">": r"$>$",
    }
    for source, replacement in replacements.items():
        text = text.replace(source, replacement)
    return text


def main() -> None:
    path = CASE_DIR / "outputs/metrics.json"
    payload = json.loads(path.read_text()) if path.exists() else {
        "overall_status": "NOT EVALUABLE", "metrics": [],
        "required_variant_count": 0, "completed_variant_count": 0,
    }
    metrics = payload.get("metrics", [])
    rows = [
        f"{tex(item.get('name'))} & {tex(item.get('expected'))} & "
        f"{tex(item.get('calculated'))} {tex(item.get('units', ''))} & "
        f"{tex(item.get('status'))}{LATEX_BREAK}"
        for item in metrics
    ] or ["No calculated metrics & -- & -- & NOT EVALUABLE" + LATEX_BREAK]
    lines = [
        "% Generated; do not edit.",
        f"\\newcommand{{\\VerificationStatus}}{{{tex(payload.get('overall_status', 'NOT EVALUABLE'))}}}",
        f"\\newcommand{{\\RequiredVariantCount}}{{{payload.get('required_variant_count', 0)}}}",
        f"\\newcommand{{\\CompletedVariantCount}}{{{payload.get('completed_variant_count', 0)}}}",
        f"\\newcommand{{\\MetricCount}}{{{len(metrics)}}}",
        f"\\newcommand{{\\PassingMetricCount}}{{{sum(item.get('status') == 'PASS' for item in metrics)}}}",
        "\\newcommand{\\MetricRows}{%", *rows, "}", "",
    ]
    (CASE_DIR / "report/metrics_macros.tex").write_text(report_text("\n".join(lines)), encoding="utf-8")


if __name__ == "__main__":
    main()
