#!/usr/bin/env python3
"""Convert case-local JSON results and input statistics to LaTeX macros."""

from __future__ import annotations

from report_language import report_text

import json
from pathlib import Path

CASE_DIR = Path(__file__).resolve().parents[1]
OUTPUT_PATH = CASE_DIR / "report" / "metrics_macros.tex"
MISSING = r"\textbf{MISSING}"


def latex(value: object) -> str:
    if value is None:
        return MISSING
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, float):
        return f"{value:.4g}"
    text = str(value)
    for old, new in (
        ("\\", r"\textbackslash{}"),
        ("_", r"\_"),
        ("%", r"\%"),
        ("&", r"\&"),
        ("#", r"\#"),
    ):
        text = text.replace(old, new)
    return text


def read_json(name: str) -> dict[str, object]:
    path = CASE_DIR / "outputs" / name
    return json.loads(path.read_text(encoding="utf-8")) if path.is_file() else {}


def scalar_metrics(data: dict[str, object]) -> dict[str, object]:
    return {
        key: value
        for key, value in data.items()
        if isinstance(value, (str, int, float, bool)) or value is None
    }


def input_metrics(data: dict[str, object]) -> dict[str, object]:
    result: dict[str, object] = {}
    fields = data.get("fields", {})
    for field in ("dem", "slp", "asp", "ws", "wd", "m1", "m10", "m100"):
        values = fields.get(field, {})
        for statistic in (
            "minimum",
            "maximum",
            "mean",
            "standard_deviation",
            "sample_median",
            "sample_p05",
            "sample_p95",
            "circular_mean_degrees",
        ):
            result[f"{field}_{statistic}"] = values.get(statistic)
    observation = data.get("observation", {})
    for key in (
        "total_detection_count",
        "retained_detection_count",
        "first_detection_utc",
        "last_detection_utc",
        "frp_minimum_mw",
        "frp_mean_mw",
        "frp_maximum_mw",
    ):
        result[f"observation_{key}"] = observation.get(key)
    return result


metrics = read_json("metrics.json")
if metrics.get("method_version") != "landscape_validation_v3":
    metrics = {
        "status": "NOT EVALUABLE",
        "reason": "No result from the current landscape validation workflow",
        "ensemble_members_found": 0,
    }
inputs = read_json("input_statistics.json")
if inputs.get("method_version") != "landscape_inputs_v2":
    inputs = {}

lines = [
    "% Generated from outputs/*.json; do not edit by hand.",
    r"\makeatletter",
    r"\newcommand{\DefineMetric}[2]{\expandafter\def\csname metric@#1\endcsname{#2}}",
    r"\newcommand{\Metric}[1]{\ifcsname metric@#1\endcsname\csname metric@#1\endcsname\else\textbf{MISSING}\fi}",
    r"\newcommand{\DefineInputMetric}[2]{\expandafter\def\csname inputmetric@#1\endcsname{#2}}",
    r"\newcommand{\InputMetric}[1]{\ifcsname inputmetric@#1\endcsname\csname inputmetric@#1\endcsname\else\textbf{MISSING}\fi}",
    r"\makeatother",
    "",
]
for key, value in scalar_metrics(metrics).items():
    lines.append(rf"\DefineMetric{{{key}}}{{{latex(value)}}}")
for key, value in input_metrics(inputs).items():
    lines.append(rf"\DefineInputMetric{{{key}}}{{{latex(value)}}}")
OUTPUT_PATH.write_text(report_text("\n".join(lines) + "\n"), encoding="utf-8")
