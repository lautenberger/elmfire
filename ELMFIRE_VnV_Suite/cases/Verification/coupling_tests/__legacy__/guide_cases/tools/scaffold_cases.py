#!/usr/bin/env python3
"""Create native case wrappers around the original ELMFIRE Guide scenarios."""

from __future__ import annotations

import json
from pathlib import Path
import shutil


SUITE_DIR = Path(__file__).resolve().parents[1]
REFERENCE = SUITE_DIR / "reference" / "original_guide"
TEMPLATE = REFERENCE / "TEMPLATE"


CASES = {
    "point_ignition": {
        "title": "Guide Point Ignition Verification",
        "section": "3.2.1",
        "purpose": "Verify baseline isotropic surface-fire spread without wind or slope.",
        "setup": "Uniform Fuel Model 3, flat terrain, zero wind, and a central point ignition.",
        "expected": "Circular isochrones and a head rate of spread of 1.51 m/min.",
        "acceptance": "The simulated head rate of spread must be within 10 percent of 1.51 m/min.",
        "variants": [("main", "elmfire.data.in", "Point", "Point/point.data")],
    },
    "windy_ellipse": {
        "title": "Guide Wind-Driven Ellipse Verification",
        "section": "3.2.2",
        "purpose": "Verify wind-driven elliptical spread and the Huygens wavelet construction.",
        "setup": "Uniform Fuel Model 5, flat terrain, constant northerly wind, and an upwind point ignition.",
        "expected": "Head ROS 3.635 m/min and an ellipse length-to-width ratio of 1.24.",
        "acceptance": "Both head ROS and length-to-width ratio must be within 10 percent of their targets.",
        "variants": [("main", "elmfire.data.in", "Windy", "Windy/windy.data")],
    },
    "valley_slopes": {
        "title": "Guide Valley Slope Verification",
        "section": "3.2.3",
        "purpose": "Verify slope-driven spread across two opposing landscape slopes.",
        "setup": "Uniform Fuel Model 3, zero wind, a 5-degree eastern slope, a 15-degree western slope, and central ignition.",
        "expected": "Head ROS 1.92 m/min on the shallow side and 5.37 m/min on the steep side.",
        "acceptance": "Each regional head ROS must be within 10 percent of its BEHAVE target.",
        "variants": [("main", "elmfire.data.in", "Valley", "Valley/valley.data")],
    },
    "fuel_quadrants": {
        "title": "Guide Fuel-Quadrant Verification",
        "section": "3.2.4",
        "purpose": "Verify spatially varying fuel-model inputs and spread across fuel boundaries.",
        "setup": "Fuel Models 8, 7, 4, and 2 occupy separate quadrants on flat terrain with no wind.",
        "expected": "Regional head ROS values are 0.080, 0.472, 1.492, and 0.871 m/min.",
        "acceptance": "Every regional head ROS must be within 10 percent of its BEHAVE target.",
        "variants": [("main", "elmfire.data.in", "Quadrant", "Quadrant/quadrant.data")],
    },
    "complex_interactions": {
        "title": "Guide Complex-Interaction Verification",
        "section": "3.2.5",
        "purpose": "Review the interaction of spatial fuels, opposing slopes, and wind in one scenario.",
        "setup": "Four fuel quadrants and two slopes are combined with constant wind and central ignition.",
        "expected": "A physically coherent perimeter with fast southward spread and enhanced spread toward steep and fast-fuel regions.",
        "acceptance": "This case is qualitative and remains NOT EVALUATED until explicit numerical criteria are defined.",
        "variants": [("main", "elmfire.data.in", "Complex", "Complex/complex.data")],
    },
    "moisture_quadrants": {
        "title": "Guide Moisture-Quadrant Verification",
        "section": "3.2.6",
        "purpose": "Verify spatially varying fine-fuel moisture inputs and moisture-of-extinction behavior.",
        "setup": "Uniform Fuel Model 3 with 1-hour moisture values of 6, 12, 18, and 25 percent by quadrant.",
        "expected": "Regional ROS values are 1.511, 1.089, 0.801, and 0.0 m/min.",
        "acceptance": "Nonzero targets use a 10 percent tolerance; the 25 percent quadrant uses an absolute 0.05 m/min tolerance.",
        "variants": [("main", "elmfire.data.in", "MoistureQuad", "Moisture Quad/moisture_quad.data")],
    },
    "canopy_fire": {
        "title": "Guide Canopy-Fire Verification",
        "section": "3.2.7",
        "purpose": "Verify surface, passive-crown, and active-crown classification and spread calculations.",
        "setup": "Uniform canopy and Fuel Model 3 are exercised at 1, 5, and 10 mph wind speeds.",
        "expected": "Crown classes 0, 1, and 2 with maximum ROS 1.809, 10.06, and 35.3 m/min.",
        "acceptance": "Crown class must be within 0.5 and maximum ROS within 10 percent for every variant.",
        "variants": [
            ("1mph", "variants/1mph/elmfire.data.in", "Canopy-1mph", "Canopy/1/canopy_1mph.data"),
            ("5mph", "elmfire.data.in", "Canopy-5mph", "Canopy/5/canopy_5mph.data"),
            ("10mph", "variants/10mph/elmfire.data.in", "Canopy-10mph", "Canopy/10/canopy_10mph.data"),
        ],
    },
    "firebrand_transport": {
        "title": "Guide Firebrand-Transport Verification",
        "section": "3.2.8",
        "purpose": "Verify firebrand generation magnitude and lognormal Lagrangian transport sampling.",
        "setup": "Fuel Model 3, flat terrain, 5 mph wind, PER-MW generation, empirical transport, Lagrangian accumulation, and direct ignition.",
        "expected": "A dominant-cell count of 8450 and log-distance parameters mu 2.42 and sigma 1.31.",
        "acceptance": "Count uses 15 percent tolerance; mu and sigma use absolute tolerances of 0.15.",
        "variants": [("main", "elmfire.data.in", "Firebrands", "Firebrands/firebrands.data")],
    },
    "overnight_adjustment": {
        "title": "Guide Overnight-Adjustment Verification",
        "section": "3.2.9",
        "purpose": "Verify solar timing and application of the overnight rate-of-spread reduction.",
        "setup": "The Point geometry runs across multiple days with the diurnal adjustment enabled.",
        "expected": "Spread reduction begins at 17.5 hours and ends at 7.5 hours local-model clock time.",
        "acceptance": "Each detected transition must be within one hour of its expected time.",
        "variants": [("main", "elmfire.data.in", "Overnight", "Overnight/overnight.data")],
    },
    "suppression": {
        "title": "Guide Suppression Verification",
        "section": "3.2.10",
        "purpose": "Verify stochastic initial attack and deterministic area-growth extended attack containment.",
        "setup": "Point-fire geometry is exercised with initial-attack and extended-attack configurations.",
        "expected": "Initial-attack containment fraction 0.54 and extended-attack simulation end time 88200 s.",
        "acceptance": "Initial attack uses absolute tolerance 0.05; extended attack uses 15 percent tolerance.",
        "variants": [
            ("extended_attack", "elmfire.data.in", "Suppression-extended", "Suppression/extended_attack/extended_attack.data"),
            ("initial_attack", "variants/initial_attack/elmfire.data.in", "Suppression-initial", "Suppression/initial_attack/initial_attack.data"),
        ],
    },
}


RUNNER = r'''#!/usr/bin/env bash
set -euo pipefail

CASE_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)
PYTHON_BIN=${PYTHON_BIN:-python3}
ELMFIRE_BIN=${ELMFIRE_BIN:-elmfire}

mkdir -p "$CASE_DIR/logs"
export MPLCONFIGDIR="$CASE_DIR/logs/matplotlib"

"$PYTHON_BIN" "$CASE_DIR/scripts/generate_inputs.py"

while IFS=$'\t' read -r label config_rel guide_name; do
  [[ -z "$label" || "$label" == \#* ]] && continue
  config_path="$CASE_DIR/$config_rel"
  variant_dir=$(dirname "$config_path")
  mkdir -p "$variant_dir/outputs" "$variant_dir/scratch"
  echo "[INFO] Running $guide_name ($label)"
  (
    cd "$variant_dir"
    "$ELMFIRE_BIN" "$(basename "$config_path")"
  ) >"$CASE_DIR/logs/${label}.stdout" 2>"$CASE_DIR/logs/${label}.stderr"
done < "$CASE_DIR/scripts/variants.tsv"

"$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
"$CASE_DIR/compile_case.sh"
'''


COMPILE = r'''#!/usr/bin/env bash
set -euo pipefail

CASE_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)
(
  cd "$CASE_DIR/report"
  latexmk -pdf -interaction=nonstopmode -halt-on-error -file-line-error case_report.tex
)
echo "[OK] Built $CASE_DIR/report/case_report.pdf"
'''


GENERATE = r'''#!/usr/bin/env python3
"""Generate deterministic rasters for this ELMFIRE Guide verification case."""

import os
from pathlib import Path
import sys

CASE_DIR = Path(__file__).resolve().parents[1]
SUITE_DIR = CASE_DIR.parent
os.environ.setdefault("MPLCONFIGDIR", str(CASE_DIR / "logs" / "matplotlib"))
sys.path.insert(0, str(SUITE_DIR / "common"))

from case_adapter import generate_inputs

generate_inputs(CASE_DIR)
'''


POSTPROCESS = r'''#!/usr/bin/env python3
"""Extract GUIDE metrics and generate this case's report artifacts."""

import os
from pathlib import Path
import sys

CASE_DIR = Path(__file__).resolve().parents[1]
SUITE_DIR = CASE_DIR.parent
os.environ.setdefault("MPLCONFIGDIR", str(CASE_DIR / "logs" / "matplotlib"))
sys.path.insert(0, str(SUITE_DIR / "common"))

from case_adapter import postprocess

payload = postprocess(CASE_DIR, CASE_ID, CASE_TITLE)
print(f"[OK] {CASE_ID}: {payload['overall_status']}")
'''


REPORT = r'''\documentclass[11pt]{article}
\usepackage[margin=1in]{geometry}
\usepackage{booktabs}
\usepackage{float}
\usepackage{graphicx}
\usepackage{hyperref}
\usepackage{subfiles}

\begin{document}
\subfile{case_body.tex}
\end{document}
'''


BODY = r'''\input{case_macros.tex}
\input{metrics_macros.tex}
\documentclass[../report/case_report.tex]{subfiles}
\begin{document}

\begin{center}
  {\Large\bfseries \CaseTitle}\par
  \vspace{0.5em}
  ELMFIRE Verification and Validation Suite
\end{center}

\subsection*{Case summary}
\begin{tabular}{@{}ll@{}}
\toprule
Case ID & \texttt{\CaseID} \\
Source & ELMFIRE Guide, Section \GuideSection \\
Category & Coupling verification \\
Current status & \textbf{\VerificationStatus} \\
\bottomrule
\end{tabular}

\subsection{Verification purpose}
\CasePurpose

\subsection{Simulation setup}
\CaseSetup

The input-generation script creates a deterministic 126 by 126 raster domain
at 30 m resolution using the parameters encoded by the original GUIDE harness.
The case-local \texttt{elmfire.data.in} file, plus any declared variants,
contains the ELMFIRE namelist configuration.

\subsection{Expected behavior and acceptance criteria}
\textbf{Expected behavior.} \CaseExpected

\textbf{Acceptance criteria.} \CaseAcceptance

\subsection{Workflow}
\begin{enumerate}
  \item Run \texttt{scripts/generate\_inputs.py} to generate required rasters.
  \item Run ELMFIRE through \texttt{run\_case.sh} using \texttt{ELMFIRE\_BIN}.
  \item Run \texttt{scripts/postprocess.py} to calculate metrics and the decision.
  \item Compile this report with \texttt{compile\_case.sh}.
\end{enumerate}

\subsection{Results}
The postprocessor found \MetricCount{} metric rows across \VariantCount{}
variant(s), with \PassingMetricCount{} passing rows. The overall result is
\textbf{\VerificationStatus}.

\begin{figure}[H]
  \centering
  \includegraphics[width=\textwidth,height=0.62\textheight,keepaspectratio]{../figures/verification_summary.pdf}
  \caption{Expected and measured GUIDE verification metrics. A dash indicates
  that an ELMFIRE output was not available when postprocessing ran.}
\end{figure}

\subsection{Scope and provenance}
This case is a repository-native reformulation of the corresponding scenario
in the ELMFIRE Guide verification section. The original PDF, harness, and
template inputs are retained under the suite-level \texttt{reference/}
directory. Scientific targets and tolerances are inherited from that material.

\end{document}
'''


def yaml_text(case_id: str, spec: dict) -> str:
    lines = [
        f'case_id: "{case_id}"',
        f'case_title: "{spec["title"]}"',
        'category: "Verification/coupling_tests"',
        f'guide_section: "{spec["section"]}"',
        "",
        "elmfire:",
        '  bin: "${ELMFIRE_BIN:-elmfire}"',
        f'  config: "{spec["variants"][0][1]}"',
        "",
        "variants:",
    ]
    for label, config, guide_name, _ in spec["variants"]:
        lines.extend(
            [
                f'  - id: "{label}"',
                f'    guide_case: "{guide_name}"',
                f'    config: "{config}"',
            ]
        )
    lines.extend(["", "postprocess:", '  metrics: "outputs/metrics.json"', '  figure: "figures/verification_summary.pdf"', ""])
    return "\n".join(lines)


def latex_escape(text: str) -> str:
    replacements = {"\\": r"\textbackslash{}", "&": r"\&", "%": r"\%", "$": r"\$", "#": r"\#", "_": r"\_"}
    return "".join(replacements.get(char, char) for char in text)


def write_case(case_id: str, spec: dict) -> None:
    case_dir = SUITE_DIR / case_id
    for rel in ("data", "figures", "logs", "outputs", "report", "scripts", "variants"):
        (case_dir / rel).mkdir(parents=True, exist_ok=True)

    (case_dir / "case.yaml").write_text(yaml_text(case_id, spec), encoding="utf-8")
    (case_dir / "run_case.sh").write_text(RUNNER, encoding="utf-8")
    (case_dir / "compile_case.sh").write_text(COMPILE, encoding="utf-8")
    (case_dir / "scripts" / "generate_inputs.py").write_text(GENERATE, encoding="utf-8")
    post = POSTPROCESS.replace("CASE_ID", repr(case_id)).replace("CASE_TITLE", repr(spec["title"]))
    (case_dir / "scripts" / "postprocess.py").write_text(post, encoding="utf-8")
    variants = ["# label\tconfig_path\toriginal_guide_case"]
    for label, config, guide_name, source in spec["variants"]:
        variants.append(f"{label}\t{config}\t{guide_name}")
        target = case_dir / config
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(TEMPLATE / source, target)
        namelist = target.read_text(encoding="utf-8")
        namelist = namelist.replace("'./inputs'", "'./data/inputs'")
        target.write_text(namelist, encoding="utf-8")
    (case_dir / "scripts" / "variants.tsv").write_text("\n".join(variants) + "\n", encoding="utf-8")

    macros = "\n".join(
        [
            rf"\newcommand{{\CaseID}}{{{latex_escape(case_id)}}}",
            rf"\newcommand{{\CaseTitle}}{{{latex_escape(spec['title'])}}}",
            rf"\newcommand{{\GuideSection}}{{{spec['section']}}}",
            rf"\newcommand{{\CasePurpose}}{{{latex_escape(spec['purpose'])}}}",
            rf"\newcommand{{\CaseSetup}}{{{latex_escape(spec['setup'])}}}",
            rf"\newcommand{{\CaseExpected}}{{{latex_escape(spec['expected'])}}}",
            rf"\newcommand{{\CaseAcceptance}}{{{latex_escape(spec['acceptance'])}}}",
            "",
        ]
    )
    (case_dir / "report" / "case_macros.tex").write_text(macros, encoding="utf-8")
    (case_dir / "report" / "metrics_macros.tex").write_text(
        "\\newcommand{\\VerificationStatus}{NOT RUN}\n"
        "\\newcommand{\\VariantCount}{0}\n"
        "\\newcommand{\\MetricCount}{0}\n"
        "\\newcommand{\\PassingMetricCount}{0}\n",
        encoding="utf-8",
    )
    (case_dir / "report" / "case_report.tex").write_text(REPORT, encoding="utf-8")
    (case_dir / "report" / "case_body.tex").write_text(BODY, encoding="utf-8")
    (case_dir / "README.md").write_text(
        f"# {spec['title']}\n\n"
        f"This coupling-verification case reformulates ELMFIRE Guide Section {spec['section']}.\n\n"
        "Run `./run_case.sh` with `ELMFIRE_BIN` configured.\n",
        encoding="utf-8",
    )
    for script in (case_dir / "run_case.sh", case_dir / "compile_case.sh", case_dir / "scripts" / "generate_inputs.py", case_dir / "scripts" / "postprocess.py"):
        script.chmod(0o755)


def main() -> None:
    for case_id, spec in CASES.items():
        write_case(case_id, spec)
        print(f"[OK] scaffolded {case_id}")


if __name__ == "__main__":
    main()
