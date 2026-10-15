#!/usr/bin/env python3
"""Fingerprint CASE44 runtime inputs and write post-run completion evidence."""
from __future__ import annotations

import argparse
import hashlib
import json
import shutil
from datetime import datetime, timezone
from pathlib import Path


CASE_ID = "CASE44_WHP"
SOURCE_COMMIT = "a2dfbcdf72209733c000e5d3431e44723e15efea"


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def output_artifact_hashes(variant_root: Path) -> dict[str, str]:
    """Hash the exact run-output snapshot, excluding the receipt itself."""
    output_dir = variant_root / "outputs"
    return {
        path.relative_to(variant_root).as_posix(): sha256_file(path)
        for path in sorted(output_dir.rglob("*"))
        if path.is_file() and path.name != "completion_marker.json"
    }


def evaluator_artifact_hashes(case_dir: Path) -> dict[str, str]:
    """Bind the receipt to the case contract and independent evaluator."""
    relative_paths = (
        "case.yaml",
        "elmfire.data.in",
        "scripts/input_fingerprint.py",
        "scripts/postprocess.py",
        "scripts/reference.py",
    )
    return {name: sha256_file(case_dir / name) for name in relative_paths}


def runtime_files(case_dir: Path, variant_id: str) -> list[Path]:
    variant = case_dir / "variants" / variant_id
    files = [variant / "elmfire.data"]
    files.extend(sorted(path for path in (variant / "inputs").rglob("*") if path.is_file()))
    files.extend(sorted(path for path in (case_dir / "data" / "misc").rglob("*") if path.is_file()))
    missing = [str(path) for path in files if not path.is_file()]
    if missing:
        raise FileNotFoundError("missing fingerprint input: " + ", ".join(missing))
    if len(files) < 4:
        raise ValueError("runtime fingerprint set is unexpectedly small")
    return sorted(files, key=lambda path: path.relative_to(case_dir).as_posix())


def build_fingerprint(case_dir: Path, variant_id: str) -> dict[str, object]:
    aggregate = hashlib.sha256()
    entries: list[dict[str, object]] = []
    for path in runtime_files(case_dir, variant_id):
        relative = path.relative_to(case_dir).as_posix()
        file_digest = sha256_file(path)
        size = path.stat().st_size
        aggregate.update(relative.encode("utf-8"))
        aggregate.update(b"\0")
        aggregate.update(file_digest.encode("ascii"))
        aggregate.update(b"\0")
        aggregate.update(str(size).encode("ascii"))
        aggregate.update(b"\n")
        entries.append({"path": relative, "size_bytes": size, "sha256": file_digest})
    return {
        "algorithm": "sha256(path\\0sha256\\0size\\n)",
        "case_id": CASE_ID,
        "variant_id": variant_id,
        "source_commit": SOURCE_COMMIT,
        "input_fingerprint_sha256": aggregate.hexdigest(),
        "files": entries,
    }


def write_expected_record(case_dir: Path, variant_id: str) -> dict[str, object]:
    record = build_fingerprint(case_dir, variant_id)
    path = case_dir / "variants" / variant_id / "input_fingerprint.json"
    path.write_text(json.dumps(record, indent=2) + "\n", encoding="utf-8")
    return record


def record_attempt(case_dir: Path, variant_id: str, status: str) -> Path:
    """Record that execution was attempted, even if no receipt can be made."""
    path = case_dir / "outputs" / "run_attempt.json"
    payload = {
        "case_id": CASE_ID,
        "variant_id": variant_id,
        "status": status,
        "updated_utc": datetime.now(timezone.utc).isoformat(),
    }
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(".json.tmp")
    temporary.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
    temporary.replace(path)
    return path


def write_completion_marker(case_dir: Path, variant_id: str, executable: str) -> Path:
    record = build_fingerprint(case_dir, variant_id)
    candidate = Path(executable).expanduser()
    if candidate.is_absolute():
        resolved = str(candidate.resolve()) if candidate.is_file() else None
    elif "/" in executable:
        candidate = (case_dir / candidate).resolve()
        resolved = str(candidate) if candidate.is_file() else None
    else:
        resolved = shutil.which(executable)
    if resolved is None:
        raise FileNotFoundError(f"cannot resolve executed ELMFIRE binary: {executable}")
    executable_path = Path(resolved).resolve()
    variant_root = case_dir / "variants" / variant_id
    stdout_path = case_dir / "logs" / f"{variant_id}.stdout"
    stderr_path = case_dir / "logs" / f"{variant_id}.stderr"
    if not stdout_path.is_file():
        raise FileNotFoundError(f"ELMFIRE stdout is unavailable: {stdout_path}")
    if not stderr_path.is_file():
        raise FileNotFoundError(f"ELMFIRE stderr is unavailable: {stderr_path}")
    artifacts = output_artifact_hashes(variant_root)
    if not artifacts:
        raise ValueError("successful ELMFIRE exit produced no output artifacts")
    marker = {
        "case_id": CASE_ID,
        "variant_id": variant_id,
        "source_commit": SOURCE_COMMIT,
        "status": "ELMFIRE_EXIT_0",
        "input_fingerprint_sha256": record["input_fingerprint_sha256"],
        "executable_requested": executable,
        "executable_resolved": str(executable_path),
        "executable_sha256": sha256_file(executable_path),
        "stdout_path": stdout_path.relative_to(case_dir).as_posix(),
        "stdout_sha256": sha256_file(stdout_path),
        "stderr_path": stderr_path.relative_to(case_dir).as_posix(),
        "stderr_sha256": sha256_file(stderr_path),
        "evaluator_artifact_sha256": evaluator_artifact_hashes(case_dir),
        "output_artifact_sha256": artifacts,
        "completed_utc": datetime.now(timezone.utc).isoformat(),
    }
    path = variant_root / "outputs" / "completion_marker.json"
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(".json.tmp")
    temporary.write_text(json.dumps(marker, indent=2) + "\n", encoding="utf-8")
    temporary.replace(path)
    return path


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--variant", required=True)
    actions = parser.add_mutually_exclusive_group()
    actions.add_argument("--write-completion-marker", action="store_true")
    actions.add_argument("--write-attempt", action="store_true")
    parser.add_argument("--executable")
    parser.add_argument("--status", choices=("STARTED", "FAILED", "RECEIPT_FAILED"))
    args = parser.parse_args()
    case_dir = Path(__file__).resolve().parents[1]
    if args.write_attempt:
        if not args.status:
            parser.error("--status is required with --write-attempt")
        print(record_attempt(case_dir, args.variant, args.status))
    elif args.write_completion_marker:
        if not args.executable:
            parser.error("--executable is required with --write-completion-marker")
        print(write_completion_marker(case_dir, args.variant, args.executable))
    else:
        print(build_fingerprint(case_dir, args.variant)["input_fingerprint_sha256"])


if __name__ == "__main__":
    main()
