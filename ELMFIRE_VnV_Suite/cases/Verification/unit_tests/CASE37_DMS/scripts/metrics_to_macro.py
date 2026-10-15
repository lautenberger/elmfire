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
    escaped = text.replace("_", r"\_").replace("%", r"\%").replace("&", r"\&")
    return escaped.replace("<=", r"\ensuremath{\le}").replace(">=", r"\ensuremath{\ge}")


def worst_variant(rows, key):
    """Return the row with the largest finite calculated error."""
    candidates = [
        row
        for row in rows
        if isinstance(row.get(key), (int, float))
    ]
    return max(candidates, key=lambda row: row[key]) if candidates else {}


def main() -> None:
    path = CASE_DIR / "outputs/metrics.json"
    payload = json.loads(path.read_text()) if path.exists() else {
        "overall_status": "NOT EVALUABLE", "metrics": [],
        "required_variant_count": 0, "completed_variant_count": 0,
    }
    metrics = payload.get("metrics", [])
    variant_results = payload.get("variant_results", [])
    worst_ros = worst_variant(variant_results, "ros_relative_error")
    worst_ir = worst_variant(variant_results, "ir_relative_error")
    rows = [
        f"{tex(item.get('name'))} & {tex(item.get('expected'))} & "
        f"{'MISSING' if item.get('status') == 'NOT EVALUABLE' else tex(item.get('calculated')) + ' ' + tex(item.get('units', ''))} & "
        f"{tex(item.get('status'))}{LATEX_BREAK}"
        for item in metrics
    ] or ["No calculated metrics & -- & -- & NOT EVALUABLE" + LATEX_BREAK]
    lines = [
        "% Generated; do not edit.",
        f"\\newcommand{{\\VerificationStatus}}{{{tex(payload.get('overall_status', 'NOT EVALUABLE'))}}}",
        f"\\newcommand{{\\RequiredVariantCount}}{{{payload.get('required_variant_count', 0)}}}",
        f"\\newcommand{{\\CompletedVariantCount}}{{{payload.get('completed_variant_count', 0)}}}",
        f"\\newcommand{{\\WorstRosVariant}}{{{tex(worst_ros.get('id'))}}}",
        f"\\newcommand{{\\WorstRosExpected}}{{{tex(worst_ros.get('expected_ros_m_min'))}}}",
        f"\\newcommand{{\\WorstRosMeasured}}{{{tex(worst_ros.get('measured_ros_m_min'))}}}",
        f"\\newcommand{{\\WorstRosRelativeError}}{{{tex(worst_ros.get('ros_relative_error'))}}}",
        f"\\newcommand{{\\WorstIrVariant}}{{{tex(worst_ir.get('id'))}}}",
        f"\\newcommand{{\\WorstIrExpected}}{{{tex(worst_ir.get('expected_ir_kw_m2'))}}}",
        f"\\newcommand{{\\WorstIrMeasured}}{{{tex(worst_ir.get('measured_ir_kw_m2'))}}}",
        f"\\newcommand{{\\WorstIrRelativeError}}{{{tex(worst_ir.get('ir_relative_error'))}}}",
        f"\\newcommand{{\\MetricCount}}{{{len(metrics)}}}",
        f"\\newcommand{{\\PassingMetricCount}}{{{sum(item.get('status') == 'PASS' for item in metrics)}}}",
        "\\newcommand{\\MetricRows}{%", *rows, "}", "",
    ]
    (CASE_DIR / "report/metrics_macros.tex").write_text(report_text("\n".join(lines)), encoding="utf-8")


if __name__ == "__main__":
    main()
