#!/usr/bin/env python3
"""Convert CASE43 metrics JSON into report-safe LaTeX macros."""
from __future__ import annotations

from report_language import report_text

import json
import math
from pathlib import Path


CASE_DIR = Path(__file__).resolve().parents[1]
LATEX_BREAK = " " + chr(92) * 2
EXPECTED_NAMES = {
    "maximum HRRPUA normalized L1 error",
    "maximum DFC full-map normalized L1 error",
    "maximum radiation full-map normalized L1 error",
    "D greater than 50 m clamp pair error",
    "constant/raster parameter equivalence error",
    "maximum 90-degree covariance error",
    "vegetation-source fuel-factor heat error",
    "maximum arrival-mask mismatch count",
    "maximum unexpected HRR source count",
}
EXPECTED_VARIANTS = {
    "branch_below_10", "branch_at_10", "branch_above_10",
    "branch_below_17p3", "branch_at_17p3", "branch_above_17p3",
    "area_05", "area_10", "area_20", "area_40",
    "distance_00", "distance_10", "distance_25", "distance_50", "distance_75",
    "rotation_wd000", "rotation_wd090", "rotation_wd180", "rotation_wd270",
    "equiv_constant", "equiv_raster",
    "fuel_urban", "fuel_forest",
}


def tex(value: object) -> str:
    if value is None:
        return "--"
    text = f"{value:.6g}" if isinstance(value, float) else str(value)
    replacements = {"\\": r"\textbackslash{}", "&": r"\&", "%": r"\%", "_": r"\_", "#": r"\#", "{": r"\{", "}": r"\}"}
    return "".join(replacements.get(character, character) for character in text)


def finite_number(value: object) -> bool:
    return type(value) in {int, float} and math.isfinite(float(value))


def sha256_value(value: object) -> bool:
    return (
        isinstance(value, str)
        and len(value) == 64
        and all(character in "0123456789abcdef" for character in value.lower())
    )


def evidence_row_valid(row: object) -> bool:
    if not isinstance(row, dict):
        return False
    required_hashes = (
        "input_fingerprint_sha256", "executable_sha256",
        "stdout_sha256", "stderr_sha256",
    )
    outputs = row.get("output_artifact_sha256")
    evaluator = row.get("evaluator_artifact_sha256")
    return (
        row.get("id") in EXPECTED_VARIANTS
        and isinstance(row.get("completion_receipt"), str)
        and all(sha256_value(row.get(key)) for key in required_hashes)
        and isinstance(outputs, dict) and bool(outputs)
        and all(isinstance(key, str) and sha256_value(value) for key, value in outputs.items())
        and isinstance(evaluator, dict) and bool(evaluator)
        and all(isinstance(key, str) and sha256_value(value) for key, value in evaluator.items())
    )


def load_payload() -> dict:
    path = CASE_DIR / "outputs" / "metrics.json"
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        payload = {}
    if not isinstance(payload, dict):
        payload = {}
    metrics = payload.get("metrics")
    rows_valid = isinstance(metrics, list) and all(
        isinstance(item, dict)
        and {"name", "expected", "calculated", "units", "status"}.issubset(item)
        and item.get("status") in {"PASS", "FAIL", "NOT EVALUABLE"}
        and (
            item.get("status") == "NOT EVALUABLE"
            or finite_number(item.get("calculated"))
        )
        for item in metrics
    )
    names_valid = rows_valid and (
        not metrics
        or len(metrics) == len(EXPECTED_NAMES)
        and {item["name"] for item in metrics} == EXPECTED_NAMES
    )
    complete_decision = payload.get("overall_status") in {"PASS", "FAIL"}
    variant_results = payload.get("variant_results")
    evidence_valid = (
        isinstance(variant_results, list)
        and len(variant_results) == payload.get("completed_variant_count")
        and all(evidence_row_valid(row) for row in variant_results)
        and len({row["id"] for row in variant_results}) == len(variant_results)
    )
    base_valid = (
        payload.get("case_id") == "CASE43_WER"
        and payload.get("source_commit") == "a2dfbcdf72209733c000e5d3431e44723e15efea"
        and payload.get("overall_status") in {"PASS", "FAIL", "NOT EVALUABLE"}
        and payload.get("workflow_status") in {"NOT RUN", "INCOMPLETE", "BLOCKED", "COMPLETE"}
        and type(payload.get("verification_passed")) is bool
        and type(payload.get("required_outputs_complete")) is bool
        and payload.get("required_variant_count") == 23
        and isinstance(payload.get("completed_variant_count"), int)
        and 0 <= payload["completed_variant_count"] <= 23
        and isinstance(payload.get("reason"), str)
        and names_valid
        and evidence_valid
    )
    complete_valid = not complete_decision or (
        payload.get("workflow_status") == "COMPLETE"
        and payload.get("required_outputs_complete") is True
        and payload.get("completed_variant_count") == 23
        and {row["id"] for row in variant_results} == EXPECTED_VARIANTS
        and sha256_value(payload.get("executed_binary_sha256"))
        and len(metrics) == len(EXPECTED_NAMES)
        and {item["name"] for item in metrics} == EXPECTED_NAMES
        and (
            payload.get("overall_status") == "PASS"
            and payload.get("verification_passed") is True
            and all(item["status"] == "PASS" for item in metrics)
            or payload.get("overall_status") == "FAIL"
            and payload.get("verification_passed") is False
            and any(item["status"] == "FAIL" for item in metrics)
            and all(item["status"] in {"PASS", "FAIL"} for item in metrics)
        )
    )
    noncomplete_valid = complete_decision or (
        payload.get("overall_status") == "NOT EVALUABLE"
        and payload.get("workflow_status") != "COMPLETE"
        and payload.get("verification_passed") is False
        and payload.get("required_outputs_complete") is False
        and all(item["status"] == "NOT EVALUABLE" for item in metrics)
    )
    if not (base_valid and complete_valid and noncomplete_valid):
        return {
            "case_id": "CASE43_WER", "overall_status": "NOT EVALUABLE",
            "workflow_status": "INCOMPLETE", "verification_passed": False,
            "required_outputs_complete": False, "required_variant_count": 23,
            "completed_variant_count": 0,
            "reason": "Metrics payload failed the report consistency contract.",
            "metrics": [],
        }
    return payload


def main() -> None:
    payload = load_payload()
    metrics = payload.get("metrics", [])
    rows = [
        f"{tex(item.get('name'))} & {tex(item.get('expected'))} & {tex(item.get('calculated'))} {tex(item.get('units', ''))} & {tex(item.get('status', 'NOT EVALUABLE'))}{LATEX_BREAK}"
        for item in metrics
    ]
    if not rows:
        rows = [f"{tex(payload.get('reason', 'No calculated metrics are available'))} & -- & -- & NOT EVALUABLE{LATEX_BREAK}"]
    lines = [
        "% Generated by scripts/metrics_to_macro.py; do not edit.",
        f"\\newcommand{{\\VerificationStatus}}{{{tex(payload['overall_status'])}}}",
        f"\\newcommand{{\\WorkflowStatus}}{{{tex(payload['workflow_status'])}}}",
        f"\\newcommand{{\\VerificationReason}}{{{tex(payload.get('reason', '--'))}}}",
        f"\\newcommand{{\\ExecutedBinarySHA}}{{{tex(payload.get('executed_binary_sha256') or '--')}}}",
        f"\\newcommand{{\\RequiredVariantCount}}{{{int(payload.get('required_variant_count', 0))}}}",
        f"\\newcommand{{\\CompletedVariantCount}}{{{int(payload.get('completed_variant_count', 0))}}}",
        f"\\newcommand{{\\MetricCount}}{{{len(metrics)}}}",
        f"\\newcommand{{\\PassingMetricCount}}{{{sum(item.get('status') == 'PASS' for item in metrics)}}}",
        "\\newcommand{\\MetricRows}{%",
        *rows,
        "}",
        "",
    ]
    (CASE_DIR / "report" / "metrics_macros.tex").write_text(report_text("\n".join(lines)), encoding="utf-8")
    print("[OK] wrote report/metrics_macros.tex")


if __name__ == "__main__":
    main()
