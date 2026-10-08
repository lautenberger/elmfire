import json, sys, pathlib

CASE_DIR = pathlib.Path(__file__).resolve().parents[1]
metrics_path = CASE_DIR / "outputs" / "metrics.json"
out_path = CASE_DIR / "report" / "metrics_macros.tex"
metrics = json.loads(metrics_path.read_text())

# Always write the general, robust macros first
lines = [
    "% Auto-generated metrics_macros.tex",
    "\\makeatletter",
    "\\newcommand{\\DefineMetric}[2]{\\expandafter\\def\\csname metric@#1\\endcsname{#2}}",
    "\\newcommand{\\Metric}[1]{%",
    "  \\ifcsname metric@#1\\endcsname",
    "    \\csname metric@#1\\endcsname",
    "  \\else",
    "    \\textbf{??}%",
    "  \\fi}",
    "\\newcommand{\\MetricOr}[2]{%",
    "  \\ifcsname metric@#1\\endcsname",
    "    \\csname metric@#1\\endcsname",
    "  \\else",
    "    #2%",
    "  \\fi}",
    "\\newcommand{\\IfMetricTF}[3]{%",
    "  \\ifcsname metric@#1\\endcsname",
    "    #2%",
    "  \\else",
    "    #3%",
    "  \\fi}",
    "\\makeatother",
    "",
]

# If JSON exists and is valid, append concrete definitions
metrics = {}
if metrics_path.exists():
    try:
        metrics = json.loads(metrics_path.read_text())
    except Exception:
        # If it's unreadable, we just keep the general macros and no defs.
        pass

for k, v in metrics.items():
    # Plain formatting; keep LaTeX-friendly scalars
    sval = f"{v:.6g}" if isinstance(v, float) else str(v)
    lines.append(f"\\DefineMetric{{{k}}}{{{sval}}}")


out_path.write_text("\n".join(lines), encoding="utf-8")