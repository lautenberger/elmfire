#!/usr/bin/env python3
"""Format existing verified metrics for readable report tables; do not evaluate physics."""

from report_language import report_text
from pathlib import Path
import json
import re

CASE_DIR = Path(__file__).resolve().parents[1]


def tex(value):
    text = str(value)
    table = {"\\": r"\textbackslash{}", "_": r"\_", "%": r"\%", "&": r"\&",
             "#": r"\#", "$": r"\$", "{": r"\{", "}": r"\}",
             "<": r"\ensuremath{<}", ">": r"\ensuremath{>}", "~": r"\textasciitilde{}", "^": r"\textasciicircum{}"}
    return "".join(r"\ensuremath{\leq}" if token == "<=" else r"\ensuremath{\geq}" if token == ">=" else table.get(token, token) for token in re.findall(r"<=|>=|.", text, re.S))


def display(value):
    if value is None:
        return "Not available"
    if isinstance(value, bool):
        return "Yes" if value else "No"
    if isinstance(value, float):
        return tex(f"{value:.6g}")
    if isinstance(value, dict):
        return r"\newline ".join(tex(k.replace("_", " ")) + ": " + display(v) for k, v in value.items())
    if isinstance(value, list):
        return "; ".join(display(v) for v in value)
    return tex(value)


def main():
    payload = json.loads((CASE_DIR / "outputs/metrics.json").read_text())
    rows = payload.get("metrics", [])
    lines = [r"\begin{longtable}{@{}P{0.19\textwidth}P{0.76\textwidth}@{}}", r"\toprule"]
    for row in rows:
        name = tex(row["name"][0].upper() + row["name"][1:])
        status = tex(row["status"])
        units = row.get("units", "")
        observed = display(row.get("calculated"))
        if units not in ("", "-", "--") and row.get("calculated") is not None:
            observed = r"\textit{Units: " + tex(units) + r".}\newline " + observed
        lines += [r"\multicolumn{2}{@{}P{0.97\textwidth}@{}}{\textbf{" + name + "} --- " + r"\textbf{" + status + r"}}\\*",
                  r"Criterion & " + display(row.get("expected", "Not specified")) + r" \\*",
                  r"Observed & " + observed + r" \\[0.6em]", r"\midrule"]
    if not rows:
        lines.append(r"\multicolumn{2}{P{0.97\textwidth}}{No evaluable metric rows are available.}\\")
    lines += [r"\bottomrule", r"\end{longtable}"]
    (CASE_DIR / "report/readable_results.tex").write_text(report_text("\n".join(lines) + "\n"))
    summary = (r"\newcommand{\ReportOverview}{\textbf{Current result: " + tex(payload["overall_status"]) +
               ".} " + str(payload.get("completed_variant_count", 0)) + " of " +
               str(payload.get("required_variant_count", 0)) + " required variants have evaluable evidence. " +
               r"Workflow: \textbf{" + tex(payload.get("workflow_status", "UNKNOWN")) + ".}}\n")
    (CASE_DIR / "report/readability_macros.tex").write_text(report_text(summary))


if __name__ == "__main__":
    main()
