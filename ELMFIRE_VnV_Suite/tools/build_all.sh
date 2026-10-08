#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
MAIN_DIR="$ROOT_DIR/main_report"
PYTHON_BIN=${PYTHON_BIN:-python3}

compile_missing_case_reports() {
  local case_file case_dir report_dir compile_script
  local primary_succeeded
  local failures=0

  while IFS= read -r -d '' case_file; do
    case_dir=${case_file%/case.yaml}
    report_dir="$case_dir/report"
    compile_script="$case_dir/compile_case.sh"

    if [[ -f "$report_dir/case_report.pdf" ]]; then
      continue
    fi

    if [[ ! -f "$report_dir/case_report.tex" ]]; then
      echo "[WARN] Cannot build case report; missing $report_dir/case_report.tex" >&2
      failures=1
      continue
    fi

    echo "[INFO] Building missing case report: $report_dir/case_report.pdf"
    primary_succeeded=0
    if [[ -f "$compile_script" ]]; then
      # Invoke through Bash so a transferred repository does not require the
      # executable bit to be restored before reports can be assembled.
      if PYTHON_BIN="$PYTHON_BIN" bash "$compile_script"; then
        primary_succeeded=1
      fi
    elif (
      cd "$report_dir"
      latexmk -lualatex -silent -interaction=nonstopmode -halt-on-error case_report.tex
    ); then
      primary_succeeded=1
    fi

    if [[ "$primary_succeeded" -ne 1 || ! -f "$report_dir/case_report.pdf" ]]; then
      echo "[WARN] Normal case report build failed; retrying with explicit missing-figure notices: $case_dir" >&2
      # Draft graphics expose filenames and suppress available evidence too.
      # Keep existing figures and replace only missing ones with a scientific
      # evidence notice. This presentation fallback never changes metrics.
      if ! (
        cd "$report_dir"
        latexmk -g -lualatex -silent -interaction=nonstopmode -halt-on-error \
          -usepretex='\AtBeginDocument{\let\OriginalIncludeGraphics\includegraphics\renewcommand{\includegraphics}[2][]{\IfFileExists{#2}{\OriginalIncludeGraphics[#1]{#2}}{\IfFileExists{../figures/#2}{\OriginalIncludeGraphics[#1]{#2}}{\fbox{\parbox{0.85\linewidth}{Required figure unavailable. The narrative does not replace missing scientific evidence.}}}}}}' \
          case_report.tex
      ); then
        echo "[WARN] Case report build failed with missing-figure notices: $case_dir" >&2
        failures=1
        continue
      fi
    fi

    if [[ ! -f "$report_dir/case_report.pdf" ]]; then
      echo "[WARN] Case report build did not create $report_dir/case_report.pdf" >&2
      failures=1
    fi
  done < <(
    find "$ROOT_DIR/cases/Verification" "$ROOT_DIR/cases/Validation" \
      -type f -name case.yaml -print0
  )

  return "$failures"
}

case_report_failures=0
compile_missing_case_reports || case_report_failures=1

# Generate the include lists only after attempting the standalone reports so
# newly created case_report.pdf files are included in this aggregate build.
# Scientific decisions remain derived solely from outputs/metrics.json.
"$PYTHON_BIN" "$ROOT_DIR/tools/generate_summary_reports.py"

(cd "$MAIN_DIR" && latexmk -lualatex -silent verification_report.tex)
echo "[OK] Built $MAIN_DIR/verification_report.pdf"

(cd "$MAIN_DIR" && latexmk -lualatex -silent validation_report.tex)
echo "[OK] Built $MAIN_DIR/validation_report.pdf"

if [[ "$case_report_failures" -ne 0 ]]; then
  echo "[ERROR] One or more standalone case reports could not be built." >&2
  echo "[ERROR] Aggregate reports use explicit placeholders for those cases." >&2
  exit 1
fi
