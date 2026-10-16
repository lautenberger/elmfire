#!/usr/bin/env python3
"""Convert case-local metrics into report-safe LaTeX macros."""
from __future__ import annotations

from report_language import report_text
import json
from pathlib import Path

CASE_DIR = Path(__file__).resolve().parents[1]
LATEX_BREAK = " " + chr(92) * 2


def tex(value: object) -> str:
    if value is None:
        return "--"
    text = f"{value:.6g}" if isinstance(value, float) else str(value)
    character_replacements = {"&": r"\&", "%": r"\%", "_": r"\_", "#": r"\#"}
    text = "".join(character_replacements.get(char, char) for char in text)
    operator_replacements = {
        "<=": r"$\leq$",
        ">=": r"$\geq$",
        "<": r"$<$",
        ">": r"$>$",
    }
    for source, replacement in operator_replacements.items():
        text = text.replace(source, replacement)
    return text


def main() -> None:
    path = CASE_DIR / "outputs/metrics.json"
    payload = (
        json.loads(path.read_text())
        if path.exists()
        else {"overall_status": "NOT EVALUABLE", "metrics": [], "variants": []}
    )
    metrics = payload.get("metrics", [])
    rows = [
        f"{tex(m.get('name'))} & {tex(m.get('expected'))} ({tex(m.get('tolerance'))}) & {tex(m.get('calculated'))} {tex(m.get('units',''))} & {tex(m.get('status'))}"
        + LATEX_BREAK
        for m in metrics
    ]
    if not rows:
        rows = ["No calculated metrics & -- & -- & NOT EVALUABLE" + LATEX_BREAK]
    lines = [
        "% Generated; do not edit.",
        f"\\newcommand{{\\VerificationStatus}}{{{tex(payload.get('overall_status'))}}}",
        f"\\newcommand{{\\VerificationReason}}{{{tex(payload.get('reason', '--'))}}}",
        f"\\newcommand{{\\VariantCount}}{{{len(payload.get('variants',[]))}}}",
        f"\\newcommand{{\\MetricCount}}{{{len(metrics)}}}",
        f"\\newcommand{{\\PassingMetricCount}}{{{sum(m.get('status')=='PASS' for m in metrics)}}}",
        "\\newcommand{\\MetricRows}{%",
        *rows,
        "}",
        "",
    ]
    (CASE_DIR / "report/metrics_macros.tex").write_text(report_text("\n".join(lines)))


if __name__ == "__main__":
    main()
