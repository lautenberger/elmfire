#!/usr/bin/env python3
"""Create a content-bound receipt after run_case observes ELMFIRE exit zero."""
from __future__ import annotations

import argparse
import datetime as dt
import hashlib
import json
import os
import re
import shutil
from pathlib import Path

from fingerprint import (
    aggregate_fingerprint,
    oracle_artifact_fingerprint,
    sha256_file,
    variant_fingerprint,
)

CASE_DIR = Path(__file__).resolve().parents[1]
CASE_ID = "CASE46_WUT"
SOURCE_COMMIT = "a2dfbcdf72209733c000e5d3431e44723e15efea"


def resolve_executable(value: str) -> Path:
    located = shutil.which(value) if "/" not in value else value
    if not located:
        raise ValueError(f"ELMFIRE executable cannot be resolved: {value}")
    path = Path(located).expanduser().resolve(strict=True)
    if not path.is_file() or not os.access(path, os.X_OK):
        raise ValueError(f"ELMFIRE executable is not an executable file: {path}")
    return path


def executable_version(stdout_path: Path) -> str:
    text = stdout_path.read_text(encoding="utf-8", errors="replace")
    match = re.search(r"(?im)^\s*(ELMFIRE\s+[^\r\n]+?)\s*$", text)
    if not match:
        raise ValueError("ELMFIRE version line is absent from captured stdout")
    return " ".join(match.group(1).split())


def output_snapshot(output_dir: Path) -> dict[str, object]:
    paths = sorted(
        path
        for path in output_dir.iterdir()
        if path.is_file() and path.name != "run_complete.json"
    )
    if not paths:
        raise ValueError("ELMFIRE produced no output files")
    entries: list[dict[str, object]] = []
    digest = hashlib.sha256()
    for path in paths:
        relative = path.relative_to(CASE_DIR).as_posix()
        file_hash = sha256_file(path)
        size = path.stat().st_size
        digest.update(relative.encode("utf-8") + b"\0")
        digest.update(file_hash.encode("ascii") + b"\0")
        digest.update(str(size).encode("ascii") + b"\n")
        entries.append({"path": relative, "sha256": file_hash, "size_bytes": size})
    return {"sha256": digest.hexdigest(), "file_count": len(entries), "files": entries}


def write_json_atomic(path: Path, payload: object) -> None:
    temporary = path.with_name(f".{path.name}.tmp")
    temporary.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
    temporary.replace(path)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--variant", required=True)
    parser.add_argument("--executable", required=True)
    parser.add_argument("--execution-directory", required=True)
    parser.add_argument("--stdout", required=True)
    parser.add_argument("--stderr", required=True)
    args = parser.parse_args()

    execution_directory = Path(args.execution_directory).resolve(strict=True)
    if execution_directory != CASE_DIR:
        raise SystemExit(f"execution directory must be the CASE46 root: {CASE_DIR}")
    executable = resolve_executable(args.executable)
    stdout_path = Path(args.stdout).resolve(strict=True)
    stderr_path = Path(args.stderr).resolve(strict=True)
    for log_path in (stdout_path, stderr_path):
        if not log_path.is_file() or CASE_DIR not in log_path.parents:
            raise SystemExit(f"captured log is outside CASE46 or not a file: {log_path}")

    manifest = json.loads(
        (CASE_DIR / "variants/run_fingerprints.json").read_text(encoding="utf-8")
    )
    variants = manifest.get("variants")
    oracle_expected = manifest.get("oracle_artifacts")
    if (
        manifest.get("schema_version") != 2
        or manifest.get("case_id") != CASE_ID
        or manifest.get("source_commit") != SOURCE_COMMIT
        or not isinstance(variants, dict)
        or not isinstance(oracle_expected, dict)
        or aggregate_fingerprint(variants) != manifest.get("aggregate_sha256")
    ):
        raise SystemExit("runtime-input/oracle fingerprint manifest does not match CASE46_WUT")
    expected = variants.get(args.variant)
    if not isinstance(expected, dict):
        raise SystemExit(f"unknown variant {args.variant}")
    actual = variant_fingerprint(CASE_DIR / "variants" / args.variant)
    oracle_actual = oracle_artifact_fingerprint(CASE_DIR)
    if actual != expected:
        raise SystemExit("runtime inputs changed after preprocessing; refusing completion receipt")
    if oracle_actual != oracle_expected:
        raise SystemExit("expected/preflight oracle artifacts changed; refusing completion receipt")

    output_dir = CASE_DIR / "variants" / args.variant / "outputs"
    snapshot = output_snapshot(output_dir)
    marker = {
        "schema_version": 2,
        "case_id": CASE_ID,
        "variant_id": args.variant,
        "source_commit_oracle": SOURCE_COMMIT,
        "status": "ELMFIRE_EXIT_0",
        "successful_exit": True,
        "execution_directory": CASE_DIR.as_posix(),
        "executable_requested": args.executable,
        "executable_resolved_path": executable.as_posix(),
        "executable_sha256": sha256_file(executable),
        "executable_version_reported": executable_version(stdout_path),
        "stdout": {
            "path": stdout_path.relative_to(CASE_DIR).as_posix(),
            "sha256": sha256_file(stdout_path),
            "size_bytes": stdout_path.stat().st_size,
        },
        "stderr": {
            "path": stderr_path.relative_to(CASE_DIR).as_posix(),
            "sha256": sha256_file(stderr_path),
            "size_bytes": stderr_path.stat().st_size,
        },
        "input_fingerprint_sha256": actual["sha256"],
        "runtime_input_file_count": actual["file_count"],
        "oracle_artifact_fingerprint_sha256": oracle_actual["sha256"],
        "oracle_artifact_file_count": oracle_actual["file_count"],
        "output_snapshot": snapshot,
        "completed_utc": dt.datetime.now(dt.timezone.utc).isoformat(),
    }
    write_json_atomic(output_dir / "run_complete.json", marker)


if __name__ == "__main__":
    main()
