#!/usr/bin/env python3
"""Convert case-local metrics into report-safe LaTeX macros."""

from report_language import report_text
import json
from pathlib import Path

CASE_DIR = Path(__file__).resolve().parents[1]
LATEX_BREAK = " " + chr(92) * 2


def tex(value):
    if value is None:
        return "--"
    text = f"{value:.6g}" if isinstance(value, float) else str(value)
    return text.replace("_", r"\_").replace("%", r"\%").replace("&", r"\&")


def main():
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
