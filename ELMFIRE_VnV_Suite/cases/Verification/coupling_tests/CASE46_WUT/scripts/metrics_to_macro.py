#!/usr/bin/env python3
"""Strictly validate CASE46 metrics before rendering report macros."""
from __future__ import annotations

from report_language import report_text

import json
import re
from pathlib import Path

CASE_DIR = Path(__file__).resolve().parents[1]
SOURCE = CASE_DIR / "outputs/metrics.json"
DESTINATION = CASE_DIR / "report/metrics_macros.tex"
EXPECTED_NAMES = [
    "evidence and exact dump ledger",
    "source fireline-intensity setup",
    "terminal PHI/TOA state consistency",
    "intended W-to-U receiver heat exposure",
    "intended W-to-U finite ignition delay",
    "contiguous strict-threshold matrix",
    "isolated distance/threshold matrix",
    "first-update transition timing",
    "zero-source control",
]


def tex(value: object) -> str:
    if value is None:
        return "--"
    if isinstance(value, (dict, list)):
        value = json.dumps(value, sort_keys=True, separators=(",", ":"))
    elif isinstance(value, float):
        value = f"{value:.6g}"
    else:
        value = str(value)
    table = {
        "\\": r"\textbackslash{}",
        "&": r"\&",
        "%": r"\%",
        "_": r"\_",
        "#": r"\#",
        "$": r"\$",
        "{": r"\{",
        "}": r"\}",
        "~": r"\textasciitilde{}",
        "^": r"\textasciicircum{}",
    }
    escaped = "".join(table.get(character, character) for character in value)
    return escaped.replace("<=", r"\ensuremath{\leq}").replace(">", r"\ensuremath{>}")


def is_sha256(value: object) -> bool:
    return isinstance(value, str) and re.fullmatch(r"[0-9a-f]{64}", value) is not None


def breakable_tex(value: object, interval: int = 8) -> str:
    if value is None:
        return "--"
    pieces: list[str] = []
    since_break = 0
    for character in str(value):
        pieces.append(tex(character))
        since_break += 1
        if character == "/" or since_break >= interval:
            pieces.append(r"\allowbreak{}")
            since_break = 0
    return "".join(pieces)


def validate(payload: object) -> dict[str, object]:
    if not isinstance(payload, dict):
        raise ValueError("metrics root is not an object")
    metrics = payload.get("metrics")
    if (
        payload.get("schema_version") != 2
        or payload.get("case_id") != "CASE46_WUT"
        or payload.get("source_commit") != "a2dfbcdf72209733c000e5d3431e44723e15efea"
        or payload.get("overall_status") not in {"PASS", "FAIL", "NOT EVALUABLE"}
        or payload.get("workflow_status") not in {"NOT RUN", "INCOMPLETE", "COMPLETE"}
        or type(payload.get("verification_passed")) is not bool
        or type(payload.get("required_outputs_complete")) is not bool
        or payload.get("required_variant_count") != 7
        or not isinstance(payload.get("completed_variant_count"), int)
        or not 0 <= payload["completed_variant_count"] <= 7
        or not isinstance(payload.get("reason"), str)
        or not payload["reason"].strip()
        or not is_sha256(payload.get("runtime_input_fingerprint_sha256"))
        or not is_sha256(payload.get("oracle_artifact_fingerprint_sha256"))
        or not isinstance(metrics, list)
        or len(metrics) != len(EXPECTED_NAMES)
        or [item.get("name") for item in metrics if isinstance(item, dict)] != EXPECTED_NAMES
    ):
        raise ValueError("metrics identity, fingerprint, count, or status schema is invalid")
    statuses: list[str] = []
    for item in metrics:
        if (
            not isinstance(item, dict)
            or not isinstance(item.get("expected"), str)
            or not item["expected"]
            or not isinstance(item.get("units"), str)
            or item.get("status") not in {"PASS", "FAIL", "NOT EVALUABLE"}
        ):
            raise ValueError("metric row is malformed")
        statuses.append(str(item["status"]))
    overall = payload["overall_status"]
    workflow = payload["workflow_status"]
    passed = payload["verification_passed"]
    complete = payload["required_outputs_complete"]
    completed = payload["completed_variant_count"]
    executable = payload.get("executable")
    primary_indices = (0, 1, 2, 3, 4, 8)
    if overall == "PASS":
        if not (
            workflow == "COMPLETE"
            and passed is True
            and complete is True
            and completed == 7
            and all(statuses[index] == "PASS" for index in primary_indices)
            and all(status in {"PASS", "FAIL", "NOT EVALUABLE"} for status in statuses[5:8])
            and isinstance(executable, dict)
            and isinstance(executable.get("resolved_path"), str)
            and is_sha256(executable.get("sha256"))
            and str(executable.get("version", "")).startswith("ELMFIRE ")
        ):
            raise ValueError("PASS payload violates the strict acceptance contract")
    elif overall == "FAIL":
        if not (
            workflow == "COMPLETE"
            and passed is False
            and complete is True
            and completed == 7
            and any(statuses[index] == "FAIL" for index in primary_indices)
            and all(statuses[index] in {"PASS", "FAIL"} for index in primary_indices)
            and isinstance(executable, dict)
        ):
            raise ValueError("FAIL payload violates complete selector-enabled semantics")
    else:
        if passed is not False:
            raise ValueError("NOT EVALUABLE payload cannot set verification_passed")
        if workflow == "COMPLETE":
            raise ValueError("complete evidence must decide the primary heat/FTP capability")
        elif not (
            complete is False
            and completed < 7
            and statuses == ["NOT EVALUABLE"] * len(EXPECTED_NAMES)
            and executable is None
        ):
            raise ValueError("incomplete/not-run NOT EVALUABLE payload has contradictory evidence semantics")
    return payload


def render(payload: dict[str, object]) -> str:
    executable = payload.get("executable") if isinstance(payload.get("executable"), dict) else {}
    rows: list[str] = []
    for item in payload["metrics"]:
        calculated = tex(item.get("calculated"))
        units = tex(item["units"])
        displayed = calculated if units == "--" else f"{calculated} {units}"
        rows.append(
            f"{tex(item['name'])} & {tex(item['expected'])} & {displayed} & {tex(item['status'])} \\\\"
        )
    content = [
        "% Generated mechanically after strict semantic validation; do not edit.",
        rf"\newcommand{{\VerificationStatus}}{{{tex(payload['overall_status'])}}}",
        rf"\newcommand{{\WorkflowStatus}}{{{tex(payload['workflow_status'])}}}",
        rf"\newcommand{{\VerificationReason}}{{{tex(payload['reason'])}}}",
        rf"\newcommand{{\SelectorStatus}}{{{tex(payload.get('selector_preflight_status', 'NOT EVALUABLE'))}}}",
        rf"\newcommand{{\RequiredVariants}}{{{payload['required_variant_count']}}}",
        rf"\newcommand{{\CompletedVariants}}{{{payload['completed_variant_count']}}}",
        rf"\newcommand{{\RuntimeFingerprint}}{{{breakable_tex(payload['runtime_input_fingerprint_sha256'])}}}",
        rf"\newcommand{{\OracleFingerprint}}{{{breakable_tex(payload['oracle_artifact_fingerprint_sha256'])}}}",
        rf"\newcommand{{\ExecutablePath}}{{{breakable_tex(executable.get('resolved_path'))}}}",
        rf"\newcommand{{\ExecutableHash}}{{{breakable_tex(executable.get('sha256'))}}}",
        rf"\newcommand{{\ExecutableVersion}}{{{tex(executable.get('version'))}}}",
        r"\newcommand{\MetricRows}{%",
        *rows,
        "}",
        "",
    ]
    return "\n".join(content)


def main() -> None:
    try:
        payload = validate(json.loads(SOURCE.read_text(encoding="utf-8")))
    except (OSError, ValueError, KeyError, TypeError, json.JSONDecodeError) as exc:
        fallback = [
            "% INVALID metrics.json; report compilation intentionally blocked.",
            r"\newcommand{\VerificationStatus}{INVALID}",
            r"\newcommand{\WorkflowStatus}{INCOMPLETE}",
            rf"\newcommand{{\VerificationReason}}{{{tex(exc)}}}",
            r"\newcommand{\SelectorStatus}{INVALID}",
            r"\newcommand{\RequiredVariants}{7}",
            r"\newcommand{\CompletedVariants}{0}",
            r"\newcommand{\RuntimeFingerprint}{--}",
            r"\newcommand{\OracleFingerprint}{--}",
            r"\newcommand{\ExecutablePath}{--}",
            r"\newcommand{\ExecutableHash}{--}",
            r"\newcommand{\ExecutableVersion}{--}",
            r"\newcommand{\MetricRows}{INVALID metrics payload & -- & -- & INVALID \\}",
            "",
        ]
        DESTINATION.write_text(report_text("\n".join(fallback)), encoding="utf-8")
        raise SystemExit(f"CASE46 metrics rejected: {exc}")
    DESTINATION.write_text(report_text(render(payload)), encoding="utf-8")


if __name__ == "__main__":
    main()
