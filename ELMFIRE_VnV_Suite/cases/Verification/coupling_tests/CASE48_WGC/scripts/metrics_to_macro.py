#!/usr/bin/env python3
"""Convert CASE48_WGC metrics JSON into fail-visible, report-safe macros."""

from report_language import report_text

import json
import math
from pathlib import Path


CASE_DIR = Path(__file__).resolve().parents[1]
LATEX_BREAK = " " + chr(92) * 2
CASE_ID = "CASE48_WGC"
DEFAULT_REQUIRED = 4
EXPECTED_METRICS = {
    "current-fingerprint output completeness",
    "finest-pair landmark TOA relative L2 change",
    "finest-pair community ROS relative change",
    "finest-pair total-heat relative change",
    "landmark TOA observed spatial order",
    "community ROS observed spatial order",
    "total-heat observed spatial order",
    "minimum TOA-fit coefficient of determination",
    "maximum logged solver-step deviation",
}


def tex(value):
    if value is None:
        return "--"
    text = f"{value:.6g}" if isinstance(value, float) else str(value)
    replacements = {
        "\\": r"\textbackslash{}", "_": r"\_", "%": r"\%", "&": r"\&",
        "#": r"\#", "{": r"\{", "}": r"\}", "^": r"\textasciicircum{}",
        "~": r"\textasciitilde{}",
    }
    text = "".join(replacements.get(character, character) for character in text)
    return text.replace("<=", r"\ensuremath{\le}").replace(">=", r"\ensuremath{\ge}")


def load_payload() -> dict:
    path = CASE_DIR / "outputs" / "metrics.json"
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        payload = {}
    if not isinstance(payload, dict):
        payload = {}
    required = {
        "overall_status", "workflow_status", "verification_passed",
        "required_outputs_complete", "required_variant_count",
        "completed_variant_count", "reason", "metrics",
    }
    missing = sorted(required - set(payload))
    metrics = payload.get("metrics")
    def valid_calculated(value) -> bool:
        if value is None:
            return False
        if isinstance(value, bool):
            return True
        if isinstance(value, (int, float)):
            return math.isfinite(float(value))
        return isinstance(value, str) and bool(value.strip())

    valid_rows = (
        isinstance(metrics, list)
        and all(
            isinstance(row, dict)
            and {"name", "expected", "calculated", "units", "status"}.issubset(row)
            and isinstance(row.get("name"), str)
            and isinstance(row.get("units"), str)
            and row.get("status") in {"PASS", "FAIL", "NOT EVALUABLE"}
            and (row.get("status") == "NOT EVALUABLE" or valid_calculated(row.get("calculated")))
            for row in metrics
        )
    )
    required_count = payload.get("required_variant_count")
    completed_count = payload.get("completed_variant_count")
    scalar_contract = (
        payload.get("case_id") == CASE_ID
        and payload.get("overall_status") in {"PASS", "FAIL", "NOT EVALUABLE"}
        and payload.get("workflow_status") in {"COMPLETE", "INCOMPLETE", "NOT RUN"}
        and type(payload.get("verification_passed")) is bool
        and type(payload.get("required_outputs_complete")) is bool
        and isinstance(required_count, int) and required_count == DEFAULT_REQUIRED
        and isinstance(completed_count, int) and 0 <= completed_count <= required_count
        and isinstance(payload.get("reason"), str) and bool(payload.get("reason"))
    )
    pass_contract = payload.get("overall_status") != "PASS" or (
        payload.get("workflow_status") == "COMPLETE"
        and payload.get("verification_passed") is True
        and payload.get("required_outputs_complete") is True
        and completed_count == required_count
        and valid_rows and bool(metrics)
        and all(row.get("status") == "PASS" for row in metrics)
    )
    nonpass_contract = payload.get("overall_status") == "PASS" or payload.get("verification_passed") is False
    complete_contract = payload.get("required_outputs_complete") is not True or completed_count == required_count
    fail_contract = payload.get("overall_status") != "FAIL" or (
        payload.get("workflow_status") == "COMPLETE"
        and payload.get("required_outputs_complete") is True
        and completed_count == required_count
        and valid_rows and bool(metrics)
        and any(row.get("status") == "FAIL" for row in metrics)
        and all(row.get("status") in {"PASS", "FAIL"} for row in metrics)
    )
    workflow_contract = payload.get("workflow_status") != "COMPLETE" or payload.get("required_outputs_complete") is True
    complete_decision = payload.get("overall_status") in {"PASS", "FAIL"}
    metric_inventory_contract = not complete_decision or (
        isinstance(metrics, list)
        and valid_rows
        and len(metrics) == len(EXPECTED_METRICS)
        and {row.get("name") for row in metrics} == EXPECTED_METRICS
    )
    source_revision = str(payload.get("source_revision", ""))
    executable_digest = str(payload.get("executed_binary_sha256", ""))
    provenance_contract = not complete_decision or (
        len(source_revision) == 40
        and all(character in "0123456789abcdef" for character in source_revision.lower())
        and len(executable_digest) == 64
        and all(character in "0123456789abcdef" for character in executable_digest.lower())
        and isinstance(payload.get("variants"), list)
        and len(payload["variants"]) == DEFAULT_REQUIRED
        and all(isinstance(row, dict) for row in payload["variants"])
    )
    not_run_contract = payload.get("workflow_status") != "NOT RUN" or (
        completed_count == 0 and payload.get("required_outputs_complete") is False
    )
    if (missing or not valid_rows or not scalar_contract or not pass_contract
            or not nonpass_contract or not complete_contract or not fail_contract
            or not workflow_contract or not metric_inventory_contract
            or not provenance_contract or not not_run_contract):
        return {
            "case_id": CASE_ID,
            "overall_status": "NOT EVALUABLE", "workflow_status": "INCOMPLETE",
            "verification_passed": False, "required_outputs_complete": False,
            "required_variant_count": DEFAULT_REQUIRED, "completed_variant_count": 0,
            "reason": "Metrics payload failed the report consistency contract"
                      + ((": missing " + ", ".join(missing)) if missing else ""),
            "metrics": [],
        }
    return payload


def main() -> None:
    payload = load_payload()
    metrics = payload["metrics"]
    rows = []
    for item in metrics:
        calculated = (
            f"{tex(item.get('calculated'))} {tex(item.get('units', ''))}"
            if "calculated" in item else "MISSING"
        )
        rows.append(
            f"{tex(item.get('name', 'MISSING'))} & {tex(item.get('expected', 'MISSING'))} & "
            f"{calculated} & {tex(item.get('status', 'MISSING'))}{LATEX_BREAK}"
        )
    if not rows:
        rows = [f"{tex(payload['reason'])} & -- & -- & NOT EVALUABLE{LATEX_BREAK}"]
    lines = [
        "% Generated; do not edit.",
        f"\\newcommand{{\\VerificationStatus}}{{{tex(payload['overall_status'])}}}",
        f"\\newcommand{{\\WorkflowStatus}}{{{tex(payload['workflow_status'])}}}",
        f"\\newcommand{{\\VerificationReason}}{{{tex(payload['reason'])}}}",
        f"\\newcommand{{\\ExecutedBinarySHA}}{{{tex(payload.get('executed_binary_sha256', '--'))}}}",
        f"\\newcommand{{\\RequiredVariantCount}}{{{payload['required_variant_count']}}}",
        f"\\newcommand{{\\CompletedVariantCount}}{{{payload['completed_variant_count']}}}",
        f"\\newcommand{{\\MetricCount}}{{{len(metrics)}}}",
        f"\\newcommand{{\\PassingMetricCount}}{{{sum(row.get('status') == 'PASS' for row in metrics)}}}",
        "\\newcommand{\\MetricRows}{%", *rows, "}", "",
    ]
    (CASE_DIR / "report" / "metrics_macros.tex").write_text(
        report_text("\n".join(lines)), encoding="utf-8"
    )


if __name__ == "__main__":
    main()
