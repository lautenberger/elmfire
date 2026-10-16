#!/usr/bin/env python3
"""Validate CASE45 metrics and convert them into fail-visible report macros."""
from __future__ import annotations

from report_language import report_text

import json
from pathlib import Path

CASE_DIR = Path(__file__).resolve().parents[1]
SOURCE = CASE_DIR / "outputs/metrics.json"
DESTINATION = CASE_DIR / "report/metrics_macros.tex"
METRIC_NAMES = [
    "recorded heat-to-ROS oracle relative error",
    "receiver LIST_BURNED observability",
    "fixed-FTP continuity across 30000 kJ",
    "wind continuity across 35 mph",
    "HRR amplitude response",
    "head/back/side response",
    "FTP_CRIT inverse response",
]
SCIENTIFIC_STATUSES = {"PASS", "FAIL", "NOT EVALUABLE"}
WORKFLOW_STATUSES = {"NOT RUN", "INCOMPLETE", "BLOCKED", "COMPLETE"}


def tex(value: object) -> str:
    if value is None:
        return "--"
    value = f"{value:.6g}" if isinstance(value, float) else str(value)
    table = {
        "\\": r"\textbackslash{}",
        "&": r"\&",
        "%": r"\%",
        "_": r"\_",
        "#": r"\#",
        "$": r"\$",
        "{": r"\{",
        "}": r"\}",
        "^": r"\textasciicircum{}",
        "~": r"\textasciitilde{}",
    }
    escaped = "".join(table.get(char, char) for char in value)
    return (
        escaped.replace("<=", r"\ensuremath{\leq}")
        .replace(">=", r"\ensuremath{\geq}")
        .replace(">", r"\ensuremath{>}")
        .replace("+/-", r"\ensuremath{\pm}")
    )


def require_int(value: object, name: str) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value < 0:
        raise ValueError(f"{name} must be a nonnegative integer")
    return value


def validate(payload: object) -> dict[str, object]:
    if not isinstance(payload, dict) or payload.get("case_id") != "CASE45_HRS":
        raise ValueError("metrics payload has the wrong case identity")
    overall = payload.get("overall_status")
    workflow = payload.get("workflow_status")
    if overall not in SCIENTIFIC_STATUSES or workflow not in WORKFLOW_STATUSES:
        raise ValueError("metrics payload has an invalid status vocabulary")
    required = require_int(payload.get("required_variant_count"), "required_variant_count")
    completed = require_int(payload.get("completed_variant_count"), "completed_variant_count")
    if required != 12 or completed > required or not isinstance(payload.get("reason"), str):
        raise ValueError("metrics payload has invalid completion accounting")
    metrics = payload.get("metrics")
    if not isinstance(metrics, list) or [item.get("name") for item in metrics if isinstance(item, dict)] != METRIC_NAMES:
        raise ValueError("metrics payload does not contain the seven required rows in order")
    for item in metrics:
        if (
            not isinstance(item, dict)
            or item.get("status") not in SCIENTIFIC_STATUSES
            or not isinstance(item.get("expected"), str)
            or not isinstance(item.get("units"), str)
            or not isinstance(item.get("calculated"), (str, int, float, bool, type(None)))
        ):
            raise ValueError("a metric row is malformed")
    passed = payload.get("verification_passed")
    complete = payload.get("required_outputs_complete")
    if not isinstance(passed, bool) or not isinstance(complete, bool):
        raise ValueError("metrics decision flags must be Boolean")
    if overall == "PASS" and not (
        workflow == "COMPLETE"
        and passed
        and complete
        and completed == required
        and all(item["status"] == "PASS" for item in metrics)
    ):
        raise ValueError("PASS is inconsistent with completion or component rows")
    return payload


def missing_payload(reason: str) -> dict[str, object]:
    return {
        "case_id": "CASE45_HRS",
        "overall_status": "NOT EVALUABLE",
        "workflow_status": "INCOMPLETE",
        "required_variant_count": 12,
        "completed_variant_count": 0,
        "reason": reason,
        "metrics": [
            {
                "name": name,
                "expected": "case criterion",
                "calculated": None,
                "units": "--",
                "status": "NOT EVALUABLE",
            }
            for name in METRIC_NAMES
        ],
    }


def main() -> None:
    try:
        payload = validate(json.loads(SOURCE.read_text(encoding="utf-8")))
    except (OSError, TypeError, ValueError, json.JSONDecodeError) as exc:
        payload = missing_payload(f"MISSING or invalid metrics.json: {exc}")
    rows = []
    for item in payload.get("metrics", []):
        calculated = tex(item.get("calculated"))
        units = tex(item.get("units", "--"))
        displayed = calculated if units == "--" else f"{calculated} {units}"
        rows.append(
            f"{tex(item.get('name'))} & {tex(item.get('expected'))} & {displayed} & {tex(item.get('status', 'MISSING'))} \\\\"
        )
    executable = payload.get("executed_binary")
    executable_sha = executable.get("sha256", "--") if isinstance(executable, dict) else "--"
    content = [
        "% Generated mechanically; do not edit.",
        rf"\newcommand{{\VerificationStatus}}{{{tex(payload.get('overall_status', 'MISSING'))}}}",
        rf"\newcommand{{\WorkflowStatus}}{{{tex(payload.get('workflow_status', 'MISSING'))}}}",
        rf"\newcommand{{\StatusReason}}{{{tex(payload.get('reason', 'MISSING'))}}}",
        rf"\newcommand{{\ExecutedBinarySHA}}{{{tex(executable_sha)}}}",
        rf"\newcommand{{\RequiredVariants}}{{{int(payload.get('required_variant_count', 0))}}}",
        rf"\newcommand{{\CompletedVariants}}{{{int(payload.get('completed_variant_count', 0))}}}",
        r"\newcommand{\MetricRows}{%",
        *rows,
        "}",
        "",
    ]
    DESTINATION.write_text(report_text("\n".join(content)), encoding="utf-8")


if __name__ == "__main__":
    main()
