#!/usr/bin/env python3
"""Generate linked case indexes and summary tables for the two suite reports.

Scientific status is read from each case's outputs/metrics.json.  A successful
shell command is deliberately not interpreted as a scientific PASS.  The
aggregate-build environment is recorded separately from case-declared runtime
configuration so the report does not imply unsupported execution provenance.
"""

from __future__ import annotations

import argparse
import hashlib
import importlib.metadata
import json
import os
import platform
import re
import shutil
import subprocess
import sys
from collections import Counter
from dataclasses import asdict, dataclass
from datetime import datetime, timezone
from pathlib import Path

import yaml

from report_language import scientific_text

ROOT_DIR = Path(__file__).resolve().parents[1]
CASES_DIR = ROOT_DIR / "cases"
DEFAULT_OUTPUT_DIR = ROOT_DIR / "main_report" / "generated"
SUITE_DIRECTORIES = {
    "verification": CASES_DIR / "Verification",
    "validation": CASES_DIR / "Validation",
}


@dataclass(frozen=True)
class CaseSummary:
    case_id: str
    title: str
    category: str
    elmfire_command: str
    elmfire_config: str
    mpi_ranks: str
    namelist_schema: str
    relative_directory: str
    report_pdf: str
    report_available: bool
    metrics_file: str
    metrics_available: bool
    raw_status: str
    decision: str
    detail: str


def latex_escape(value: object) -> str:
    text = str(value)
    replacements = {
        "\\": r"\textbackslash{}",
        "&": r"\&",
        "%": r"\%",
        "$": r"\$",
        "#": r"\#",
        "_": r"\_",
        "{": r"\{",
        "}": r"\}",
        "~": r"\textasciitilde{}",
        "^": r"\textasciicircum{}",
    }
    return "".join(replacements.get(character, character) for character in text)


def latex_escape_breakable(value: object) -> str:
    """Escape table text and add safe break opportunities to long values."""
    escaped = latex_escape(value)
    for separator in ("/", ", ", "; "):
        escaped = escaped.replace(separator, separator + r"\allowbreak{}")
    if re.fullmatch(r"[0-9a-f]{64}", escaped):
        escaped = r"\allowbreak{}".join(
            escaped[index : index + 16] for index in range(0, len(escaped), 16)
        )
    return escaped


def case_label(summary: CaseSummary) -> str:
    """Return a stable, LaTeX-safe destination label for a case detail."""
    slug = re.sub(r"[^a-z0-9]+", "-", summary.case_id.casefold()).strip("-")
    return f"case-detail:{slug or 'unnamed'}"


def command_output(command: list[str]) -> str:
    """Return the first non-empty line from a read-only version command."""
    try:
        completed = subprocess.run(
            command,
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            timeout=5,
        )
    except (OSError, subprocess.SubprocessError):
        return "not available"
    if completed.returncode != 0:
        return "not available"
    return next(
        (line.strip() for line in completed.stdout.splitlines() if line.strip()),
        "not available",
    )


def git_output(*arguments: str) -> str:
    try:
        completed = subprocess.run(
            ["git", *arguments],
            cwd=ROOT_DIR,
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
            timeout=5,
        )
    except (OSError, subprocess.SubprocessError):
        return "not available"
    return completed.stdout.strip()


def resolve_executable(command: str) -> str | None:
    expanded = Path(command).expanduser()
    if expanded.is_file():
        return str(expanded.resolve())
    return shutil.which(command)


def sha256_file(path: str | None) -> str:
    if path is None:
        return "not available"
    digest = hashlib.sha256()
    try:
        with Path(path).open("rb") as stream:
            for chunk in iter(lambda: stream.read(1024 * 1024), b""):
                digest.update(chunk)
    except OSError:
        return "not available"
    return digest.hexdigest()


def capture_environment() -> dict[str, str]:
    """Capture the aggregate-build environment without executing ELMFIRE."""
    elmfire_command = os.environ.get("ELMFIRE_BIN", "elmfire")
    elmfire_path = resolve_executable(elmfire_command)
    conda_name = os.environ.get("CONDA_DEFAULT_ENV")
    conda_prefix = os.environ.get("CONDA_PREFIX")
    conda = "not active"
    if conda_name or conda_prefix:
        conda = " / ".join(value for value in (conda_name, conda_prefix) if value)

    slurm_parts = []
    for label, variable in (
        ("job", "SLURM_JOB_ID"),
        ("cluster", "SLURM_CLUSTER_NAME"),
        ("tasks", "SLURM_NTASKS"),
        ("nodes", "SLURM_JOB_NUM_NODES"),
    ):
        value = os.environ.get(variable)
        if value:
            slurm_parts.append(f"{label}={value}")

    package_versions = []
    for display_name, distribution_name in (
        ("NumPy", "numpy"),
        ("Matplotlib", "matplotlib"),
        ("PyYAML", "PyYAML"),
        ("Rasterio", "rasterio"),
        ("GDAL", "GDAL"),
        ("Pandas", "pandas"),
        ("GeoPandas", "geopandas"),
        ("Shapely", "shapely"),
    ):
        try:
            version = importlib.metadata.version(distribution_name)
        except importlib.metadata.PackageNotFoundError:
            version = "not installed"
        package_versions.append(f"{display_name} {version}")

    status = git_output("status", "--porcelain")
    repository_state = (
        "unknown" if status == "not available" else ("clean" if not status else "dirty")
    )
    return {
        "Captured at (UTC)": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "Repository revision": git_output("rev-parse", "--short=12", "HEAD"),
        "Repository state": repository_state,
        "Host platform": platform.platform(),
        "Processor architecture": platform.machine(),
        "Fortran compiler": command_output(["gfortran", "--version"]),
        "Python version": f"{platform.python_implementation()} {platform.python_version()}",
        "Python runtime": (
            f"{platform.python_implementation()} {platform.python_version()} "
            f"({sys.executable})"
        ),
        "Conda environment": conda,
        "ELMFIRE command": elmfire_command,
        "ELMFIRE resolved path": elmfire_path or "not found on PATH",
        "ELMFIRE version declaration": (
            os.environ.get("ELMFIRE_VERSION")
            or os.environ.get("ELMFIRE_VER")
            or "not declared"
        ),
        "ELMFIRE executable SHA-256": sha256_file(elmfire_path),
        "MPI launcher": command_output(["mpirun", "--version"]),
        "Slurm allocation": (
            ", ".join(slurm_parts) if slurm_parts else "not a Slurm build job"
        ),
        "Loaded modules": os.environ.get("LOADEDMODULES", "not recorded"),
        "Python packages": "; ".join(package_versions),
    }


def normalize_decision(value: object) -> str | None:
    """Map heterogeneous case result vocabulary to report-level decisions."""
    if isinstance(value, bool):
        return "PASS" if value else "FAIL"
    if value is None:
        return None
    normalized = re.sub(r"\s+", " ", str(value).strip().upper().replace("_", " "))
    if normalized == "PASS" or normalized.startswith("PASS "):
        return "PASS"
    if normalized == "FAIL" or normalized.startswith("FAIL "):
        return "FAIL"
    if normalized == "CHARACTERIZED" or normalized.startswith("CHARACTERIZED "):
        return "CHARACTERIZED"
    non_decisions = (
        "NOT EVALUABLE",
        "NOT EVALUATED",
        "NOT RUN",
        "INCOMPLETE",
        "BLOCKED",
        "MISSING",
    )
    if any(normalized.startswith(prefix) for prefix in non_decisions):
        return "NOT EVALUABLE"
    return None


def extract_decision(payload: dict[str, object]) -> tuple[str, str, str]:
    """Return normalized decision, raw source value, and explanatory detail."""
    raw_value: object = None
    source_key = ""
    for key in ("overall_status", "status", "verification_status", "decision"):
        if key in payload:
            raw_value = payload[key]
            source_key = key
            break
    if raw_value is None and "verification_passed" in payload:
        raw_value = payload["verification_passed"]
        source_key = "verification_passed"

    decision = normalize_decision(raw_value)
    if decision is None:
        decision = "NOT EVALUABLE"
        detail = "No recognized scientific decision is present in metrics.json."
    else:
        detail = f"Decision normalized from metrics.json field '{source_key}'."

    reason = payload.get("reason")
    workflow = payload.get("workflow_status")
    if reason:
        detail = str(reason)
    elif workflow and decision == "NOT EVALUABLE":
        detail = f"Workflow status: {workflow}."
    return decision, str(raw_value) if raw_value is not None else "missing", detail


def read_case_metadata(case_dir: Path) -> dict[str, object]:
    metadata_path = case_dir / "case.yaml"
    if not metadata_path.is_file():
        return {}
    payload = yaml.safe_load(metadata_path.read_text(encoding="utf-8"))
    return payload if isinstance(payload, dict) else {}


def case_sort_key(summary: CaseSummary) -> tuple[object, ...]:
    match = re.match(r"CASE(\d+)", summary.case_id, re.IGNORECASE)
    if match:
        return (0, int(match.group(1)), summary.case_id)
    return (1, summary.case_id.casefold())


def discover_case_summaries(suite: str) -> list[CaseSummary]:
    suite_dir = SUITE_DIRECTORIES[suite]
    summaries: list[CaseSummary] = []
    for runner in sorted(suite_dir.glob("**/run_case.sh")):
        case_dir = runner.parent
        relative_parts = case_dir.relative_to(suite_dir).parts
        if "__legacy__" in relative_parts or "case_template" in relative_parts:
            continue
        metadata = read_case_metadata(case_dir)
        case_id = str(metadata.get("case_id") or case_dir.name)
        title = str(metadata.get("case_title") or metadata.get("title") or case_id)
        elmfire = metadata.get("elmfire")
        elmfire = elmfire if isinstance(elmfire, dict) else {}
        namelist_contract = metadata.get("namelist_contract")
        namelist_contract = (
            namelist_contract if isinstance(namelist_contract, dict) else {}
        )
        category_parts = relative_parts[:-1]
        category = "/".join(category_parts) if category_parts else suite
        metrics_path = case_dir / "outputs" / "metrics.json"
        report_path = case_dir / "report" / "case_report.pdf"
        if metrics_path.is_file():
            try:
                payload = json.loads(metrics_path.read_text(encoding="utf-8"))
                if not isinstance(payload, dict):
                    raise ValueError("top-level JSON value is not an object")
                decision, raw_status, detail = extract_decision(payload)
            except (json.JSONDecodeError, OSError, ValueError) as error:
                decision = "NOT EVALUABLE"
                raw_status = "invalid metrics.json"
                detail = f"metrics.json could not be read: {error}"
        else:
            decision = "NOT EVALUABLE"
            raw_status = "missing metrics.json"
            detail = "No case-local outputs/metrics.json is available."
        summaries.append(
            CaseSummary(
                case_id=case_id,
                title=title,
                category=category,
                elmfire_command=str(elmfire.get("bin", "not declared")),
                elmfire_config=str(elmfire.get("config", "not declared")),
                mpi_ranks=str(elmfire.get("mpi_ranks", "not declared")),
                namelist_schema=str(
                    namelist_contract.get("schema_version", "not declared")
                ),
                relative_directory=str(case_dir.relative_to(ROOT_DIR)),
                report_pdf=str(report_path.relative_to(ROOT_DIR)),
                report_available=report_path.is_file(),
                metrics_file=str(metrics_path.relative_to(ROOT_DIR)),
                metrics_available=metrics_path.is_file(),
                raw_status=raw_status,
                decision=decision,
                detail=detail,
            )
        )
    return sorted(summaries, key=case_sort_key)


def decision_tex(decision: str) -> str:
    styles = {
        "PASS": r"\mbox{\textcolor{green!45!black}{\textbf{PASS}}}",
        "FAIL": r"\mbox{\textcolor{red!75!black}{\textbf{FAIL}}}",
        "CHARACTERIZED": r"\mbox{\textcolor{blue!70!black}{\textbf{CHARACTERIZED}}}",
        "NOT EVALUABLE": r"\mbox{\textcolor{orange!75!black}{\textbf{NOT EVALUABLE}}}",
    }
    return styles.get(decision, latex_escape(decision))


def category_description(category: str) -> str:
    """Describe the scientific comparison without displaying directory names."""
    return {
        "unit_tests": "Isolated-process verification",
        "coupling_tests": "Coupled-process verification",
        "structure_scale": "Structure-scale validation",
        "landscape_scale": "Landscape-scale validation",
    }.get(category, "Scientific comparison")


def report_environment(environment: dict[str, str]) -> dict[str, str]:
    """Keep operational provenance in JSON, not in the reader-facing table.

    These are report-preparation observations, not retrospective claims about
    the environment used for an earlier simulation. Never infer a model release
    from its installation directory or from the suite's own revision.
    """
    fields = (
        ("Captured at (UTC)", "Recorded at (Coordinated Universal Time)"),
        ("ELMFIRE version declaration", "Declared ELMFIRE release"),
        ("Host platform", "Operating system"),
        ("Processor architecture", "Processor architecture"),
        ("MPI launcher", "Message Passing Interface (MPI) implementation"),
        ("Fortran compiler", "Available Fortran compiler"),
        ("Python version", "Python version"),
        ("Python packages", "Scientific analysis libraries"),
    )
    result = {}
    for key, label in fields:
        value = environment.get(key, "not documented in the available case materials")
        # Version commands and declarations can contain local paths. Do not
        # guess a release identifier from such a value.
        if re.search(r"(?:^|[\s(])(?:/|~/|[A-Za-z]:[\\/])", value):
            value = "not documented in the available case materials"
        result[label] = value
    return result


def write_case_includes(path: Path, summaries: list[CaseSummary]) -> None:
    lines = [
        "% Generated by tools/generate_summary_reports.py; do not edit by hand.",
    ]
    for summary in summaries:
        heading = f"{latex_escape(summary.case_id)} -- {latex_escape(scientific_text(summary.title))}"
        report_from_main = "../" + summary.report_pdf
        label = case_label(summary)
        lines.extend(
            [
                r"\clearpage",
                rf"\IfFileExists{{{report_from_main}}}{{%",
                rf"  \includepdf[pages=-,pagecommand={{\thispagestyle{{empty}}}},addtotoc={{1,section,1,{{{heading}}},{label}}}]{{{report_from_main}}}%",
                r"}{%",
                r"  \phantomsection",
                rf"  \label{{{label}}}",
                rf"  \addcontentsline{{toc}}{{section}}{{{heading}}}",
                rf"  \section*{{{heading}}}",
                r"  \textbf{NOT EVALUABLE:} the detailed case description is unavailable.",
                r"}",
                "",
            ]
        )
    path.write_text("\n".join(lines), encoding="utf-8")


def write_summary_table(
    path: Path,
    suite: str,
    summaries: list[CaseSummary],
    environment: dict[str, str],
) -> None:
    counts = Counter(summary.decision for summary in summaries)
    title = suite.capitalize()
    lines = [
        "% Generated by tools/generate_summary_reports.py; do not edit by hand.",
        r"\clearpage",
        rf"\section{{{title} summary}}",
        rf"\label{{sec:{suite}-summary}}",
        r"\ReportBodyText",
        r"\subsection{Computational environment recorded during report preparation}",
        "The following information describes the environment available during "
        "preparation of this guide. It does not establish the environment used "
        "for earlier simulations. An available compiler is not necessarily the "
        "compiler used to build the model. ELMFIRE was not executed to obtain "
        "these observations; an undeclared model release remains unknown.",
        "",
        r"\begingroup\small",
        r"\begin{longtable}{@{}>{\raggedright\arraybackslash}p{0.27\textwidth}>{\raggedright\arraybackslash}p{0.68\textwidth}@{}}",
        r"\toprule",
        r"Configuration & Recorded value \\",
        r"\midrule",
        r"\endfirsthead",
        r"\toprule",
        r"Configuration & Recorded value \\",
        r"\midrule",
        r"\endhead",
        r"\bottomrule",
        r"\endlastfoot",
    ]
    for key, value in report_environment(environment).items():
        lines.append(f"{latex_escape(key)} & {latex_escape_breakable(value)} " + r"\\")
    lines.extend(
        [
            r"\end{longtable}",
            r"\endgroup\ReportBodyText",
            "",
            r"\subsection{Comparison type and declared parallel allocation}",
            "The comparison type distinguishes isolated and coupled processes "
            "or the spatial scale of validation. The process count is the "
            "allocation declared for each case, not a measurement of resources "
            "used in an earlier simulation. Missing declarations are not inferred.",
            "",
            r"\begingroup\scriptsize",
            r"\begin{longtable}{@{}>{\raggedright\arraybackslash}p{0.22\textwidth}>{\raggedright\arraybackslash}p{0.48\textwidth}>{\raggedright\arraybackslash}p{0.20\textwidth}@{}}",
            r"\toprule",
            r"Case & Comparison type & MPI processes \\",
            r"\midrule",
            r"\endfirsthead",
            r"\toprule",
            r"Case & Comparison type & MPI processes \\",
            r"\midrule",
            r"\endhead",
            r"\midrule",
            r"\multicolumn{3}{r}{Continued on next page} \\",
            r"\endfoot",
            r"\bottomrule",
            r"\endlastfoot",
        ]
    )
    for summary in summaries:
        label = case_label(summary)
        case_link = rf"\hyperref[{label}]{{{latex_escape(summary.case_id)}}}"
        lines.append(
            " & ".join(
                (
                    case_link,
                    latex_escape(category_description(summary.category)),
                    latex_escape(summary.mpi_ranks),
                )
            )
            + r" \\"
        )
    lines.extend(
        [
            r"\end{longtable}",
            r"\endgroup\ReportBodyText",
            "",
            r"\subsection{Scientific decision summary}",
            "The decisions below follow the established calculated metrics and "
            "case-specific acceptance criteria. Completing a simulation or "
            "preparing a report does not establish PASS. Missing or insufficient "
            "scientific evidence is reported as NOT EVALUABLE.",
            "",
            rf"\textbf{{Total cases:}} {len(summaries)}\quad",
            rf"\textbf{{PASS:}} {counts.get('PASS', 0)}\quad",
            rf"\textbf{{FAIL:}} {counts.get('FAIL', 0)}\quad",
            rf"\textbf{{CHARACTERIZED:}} {counts.get('CHARACTERIZED', 0)}\quad",
            rf"\textbf{{NOT EVALUABLE:}} {counts.get('NOT EVALUABLE', 0)}",
            "",
            r"\begingroup\small",
            r"\setlength{\LTpre}{0.8em}",
            r"\setlength{\LTpost}{0pt}",
            r"\begin{longtable}{@{}>{\raggedright\arraybackslash}p{0.15\textwidth}>{\raggedright\arraybackslash}p{0.16\textwidth}>{\raggedright\arraybackslash}p{0.38\textwidth}>{\raggedright\arraybackslash}p{0.22\textwidth}@{}}",
            r"\toprule",
            r"Case & Category & Title & Decision \\",
            r"\midrule",
            r"\endfirsthead",
            r"\toprule",
            r"Case & Category & Title & Decision \\",
            r"\midrule",
            r"\endhead",
            r"\midrule",
            r"\multicolumn{4}{r}{Continued on next page} \\",
            r"\endfoot",
            r"\bottomrule",
            r"\endlastfoot",
        ]
    )
    for summary in summaries:
        label = case_label(summary)
        case_link = rf"\hyperref[{label}]{{{latex_escape(summary.case_id)}}}"
        title_link = rf"\hyperref[{label}]{{{latex_escape(scientific_text(summary.title))}}}"
        lines.append(
            " & ".join(
                (
                    case_link,
                    latex_escape(category_description(summary.category)),
                    title_link,
                    decision_tex(summary.decision),
                )
            )
            + r" \\"
        )
    lines.extend(
        [
            r"\end{longtable}",
            r"\endgroup\ReportBodyText",
            "",
            r"\begin{samepage}",
            r"\paragraph{Decision vocabulary.}",
            "PASS and FAIL are case-defined scientific decisions. CHARACTERIZED "
            "denotes descriptive validation metrics without a justified acceptance "
            "threshold. NOT EVALUABLE denotes missing, unrun, incomplete, unsupported, "
            "or otherwise non-decisional evidence.",
            r"\end{samepage}",
            "",
        ]
    )
    path.write_text("\n".join(lines), encoding="utf-8")


def write_json(
    path: Path,
    suite: str,
    summaries: list[CaseSummary],
    environment: dict[str, str],
) -> None:
    counts = Counter(summary.decision for summary in summaries)
    payload = {
        "schema_version": 3,
        "suite": suite,
        "aggregate_environment": environment,
        "case_count": len(summaries),
        "decision_counts": dict(sorted(counts.items())),
        "cases": [asdict(summary) for summary in summaries],
    }
    path.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")


def write_cover_metadata(path: Path, environment: dict[str, str]) -> None:
    """Write cover-page metadata captured with the aggregate environment."""
    version = report_environment(environment)["Declared ELMFIRE release"]
    lines = [
        "% Generated by tools/generate_summary_reports.py; do not edit by hand.",
        rf"\newcommand{{\AggregateELMFIREVersion}}{{{latex_escape(version)}}}",
        "",
    ]
    path.write_text("\n".join(lines), encoding="utf-8")


def generate(suite: str, output_dir: Path) -> list[CaseSummary]:
    summaries = discover_case_summaries(suite)
    environment = capture_environment()
    output_dir.mkdir(parents=True, exist_ok=True)
    write_case_includes(output_dir / f"{suite}_cases.tex", summaries)
    write_summary_table(
        output_dir / f"{suite}_summary.tex", suite, summaries, environment
    )
    write_cover_metadata(
        output_dir / f"{suite}_cover_metadata.tex", environment
    )
    write_json(
        output_dir / f"{suite}_summary.json", suite, summaries, environment
    )
    return summaries


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--suite", choices=("all", "verification", "validation"), default="all"
    )
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR)
    args = parser.parse_args()
    suites = ("verification", "validation") if args.suite == "all" else (args.suite,)
    for suite in suites:
        summaries = generate(suite, args.output_dir)
        counts = Counter(summary.decision for summary in summaries)
        print(f"[OK] Generated {suite} report inputs for {len(summaries)} cases: {dict(counts)}")


if __name__ == "__main__":
    main()
