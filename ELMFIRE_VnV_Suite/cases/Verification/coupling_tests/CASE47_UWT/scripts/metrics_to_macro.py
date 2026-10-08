#!/usr/bin/env python3
"""Render authoritative CASE47 JSON results as report-safe LaTeX macros."""
from __future__ import annotations

from report_language import report_text

import json
import math
from pathlib import Path

CASE_DIR = Path(__file__).resolve().parents[1]
LATEX_BREAK = " " + chr(92) * 2
CASE_ID = "CASE47_UWT"
SOURCE_COMMIT = "a2dfbcdf72209733c000e5d3431e44723e15efea"
EXPECTED_METRICS = {
    "source HRR fourfold response error",
    "source HRR control-repeat error",
    "isolated receiver heat-exposure minimum",
    "isolated heat fourfold response error",
    "isolated receiver arrivals",
    "nonburnable barrier arrivals",
    "ADJ0 ordinary-path arrivals",
    "ADJ1 open-path arrivals",
    "minimum ADJ1-open wildland spread rate",
    "open-path TOA HRR-control difference",
    "intended heat-only urban-to-wildland ignition capability",
}


def tex(value: object) -> str:
    if value is None:
        return "--"
    if isinstance(value, float):
        text = f"{value:.6g}"
    else:
        text = str(value)
    replacements = (
        ("\\", r"\textbackslash{}"),
        ("&", r"\&"),
        ("%", r"\%"),
        ("_", r"\_"),
        ("#", r"\#"),
        ("<=", r"$\leq$"),
        (">=", r"$\geq$"),
        ("<", r"$<$"),
        (">", r"$>$"),
    )
    for source, target in replacements:
        text = text.replace(source, target)
    return text


def main() -> None:
    source = CASE_DIR / "outputs/metrics.json"
    if source.is_file():
        try:
            payload = json.loads(source.read_text(encoding="utf-8"))
        except json.JSONDecodeError:
            payload = {}
    else:
        payload = {}
    if not isinstance(payload, dict):
        payload = {}
    required_keys = (
        "case_id",
        "overall_status",
        "workflow_status",
        "verification_passed",
        "required_outputs_complete",
        "implementation_characterization_status",
        "intended_capability_status",
        "required_variant_count",
        "completed_variant_count",
        "reason",
        "metrics",
    )
    missing = [key for key in required_keys if key not in payload]
    metrics = payload.get("metrics")
    def valid_calculated(value: object) -> bool:
        if value is None:
            return False
        if isinstance(value, bool):
            return True
        if isinstance(value, (int, float)):
            return math.isfinite(float(value))
        return isinstance(value, str) and bool(value.strip())

    rows_valid = (
        isinstance(metrics, list)
        and all(
            isinstance(item, dict)
            and {"name", "expected", "calculated", "units", "status"}.issubset(item)
            and isinstance(item.get("name"), str) and bool(item["name"])
            and isinstance(item.get("units"), str)
            and item.get("status") in {"PASS", "FAIL", "NOT EVALUABLE"}
            and (item.get("status") == "NOT EVALUABLE"
                 or valid_calculated(item.get("calculated")))
            for item in metrics
        )
    )
    required_count = payload.get("required_variant_count")
    completed_count = payload.get("completed_variant_count")
    basic_valid = (
        payload.get("case_id") == CASE_ID
        and payload.get("overall_status") == "NOT EVALUABLE"
        and payload.get("workflow_status") in {"NOT RUN", "INCOMPLETE", "COMPLETE"}
        and payload.get("verification_passed") is False
        and type(payload.get("required_outputs_complete")) is bool
        and required_count == 8
        and isinstance(completed_count, int) and 0 <= completed_count <= 8
        and payload.get("implementation_characterization_status") in {"PASS", "FAIL", "NOT EVALUABLE"}
        and payload.get("intended_capability_status") == "NOT EVALUABLE"
        and isinstance(payload.get("reason"), str) and bool(payload.get("reason"))
        and rows_valid
    )
    complete_valid = payload.get("workflow_status") != "COMPLETE" or (
        payload.get("required_outputs_complete") is True
        and completed_count == 8
        and payload.get("source_commit") == SOURCE_COMMIT
        and isinstance(payload.get("variants"), list) and len(payload["variants"]) == 8
        and len(metrics) == 11
        and {item.get("name") for item in metrics} == EXPECTED_METRICS
        and len(str(payload.get("executed_binary_sha256", ""))) == 64
        and all(character in "0123456789abcdef"
                for character in str(payload.get("executed_binary_sha256", "")).lower())
    )
    if payload.get("workflow_status") == "COMPLETE" and rows_valid:
        by_name = {str(item.get("name")): item for item in metrics}
        intended_row = by_name.get(
            "intended heat-only urban-to-wildland ignition capability", {}
        )
        current_rows = [
            item for item in metrics
            if item.get("name") != "intended heat-only urban-to-wildland ignition capability"
        ]
        component_status = payload.get("implementation_characterization_status")
        layer_valid = (
            intended_row.get("status") == "NOT EVALUABLE"
            and len(current_rows) == 10
            and all(item.get("status") in {"PASS", "FAIL"} for item in current_rows)
            and (
                (component_status == "PASS" and all(item.get("status") == "PASS" for item in current_rows))
                or (component_status == "FAIL" and any(item.get("status") == "FAIL" for item in current_rows))
            )
        )
    else:
        layer_valid = (
            payload.get("implementation_characterization_status") == "NOT EVALUABLE"
            and payload.get("required_outputs_complete") is False
            and not metrics
        )
    not_run_valid = payload.get("workflow_status") != "NOT RUN" or (
        completed_count == 0 and payload.get("required_outputs_complete") is False
    )
    if missing or not basic_valid or not complete_valid or not not_run_valid or not layer_valid:
        payload = {
            "case_id": CASE_ID,
            "overall_status": "NOT EVALUABLE",
            "workflow_status": "INCOMPLETE",
            "verification_passed": False,
            "required_outputs_complete": False,
            "implementation_characterization_status": "NOT EVALUABLE",
            "intended_capability_status": "NOT EVALUABLE",
            "required_variant_count": 8,
            "completed_variant_count": 0,
            "reason": "Metrics payload failed the report consistency contract"
                      + ((": missing " + ", ".join(missing)) if missing else ""),
            "metrics": [],
        }
    metrics = payload["metrics"]
    rows = []
    for item in metrics:
        rows.append(
            f"{tex(item.get('name', 'MISSING'))} & {tex(item.get('expected', 'MISSING'))} & "
            f"{tex(item.get('calculated', 'MISSING'))} {tex(item.get('units', ''))} & "
            f"{tex(item.get('status', 'MISSING'))}{LATEX_BREAK}"
        )
    if not rows:
        rows = [f"{tex(payload['reason'])} & -- & -- & NOT EVALUABLE{LATEX_BREAK}"]
    lines = [
        "% Generated from outputs/metrics.json; do not edit.",
        f"\\newcommand{{\\VerificationStatus}}{{{tex(payload['overall_status'])}}}",
        f"\\newcommand{{\\WorkflowStatus}}{{{tex(payload['workflow_status'])}}}",
        f"\\newcommand{{\\ImplementationStatus}}{{{tex(payload['implementation_characterization_status'])}}}",
        f"\\newcommand{{\\IntendedCapabilityStatus}}{{{tex(payload['intended_capability_status'])}}}",
        f"\\newcommand{{\\RequiredVariantCount}}{{{int(payload['required_variant_count'])}}}",
        f"\\newcommand{{\\CompletedVariantCount}}{{{int(payload['completed_variant_count'])}}}",
        f"\\newcommand{{\\VerificationReason}}{{{tex(payload['reason'])}}}",
        f"\\newcommand{{\\ExecutedBinarySHA}}{{{tex(payload.get('executed_binary_sha256', '--'))}}}",
        f"\\newcommand{{\\MetricCount}}{{{len(metrics)}}}",
        f"\\newcommand{{\\PassingMetricCount}}{{{sum(item.get('status') == 'PASS' for item in metrics)}}}",
        f"\\newcommand{{\\ResultFiguresAvailable}}{{{int(payload['workflow_status'] == 'COMPLETE' and (CASE_DIR / 'figures/implementation_characterization.pdf').is_file() and (CASE_DIR / 'figures/whole_domain_result.pdf').is_file())}}}",
        "\\newcommand{\\MetricRows}{%",
        *rows,
        "}",
        "",
    ]
    (CASE_DIR / "report/metrics_macros.tex").write_text(report_text("\n".join(lines)), encoding="utf-8")


if __name__ == "__main__":
    main()
