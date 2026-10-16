#!/usr/bin/env python3
"""Record current-input and executable identity after a successful ELMFIRE exit."""
from __future__ import annotations

import argparse
import hashlib
import json
import shutil
from datetime import datetime, timezone
from pathlib import Path

CASE_DIR = Path(__file__).resolve().parents[1]
CASE_ID = "CASE49_WTC"
SOURCE_REVISION = "a2dfbcdf72209733c000e5d3431e44723e15efea"


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def artifact_hashes(root: Path) -> dict[str, str]:
    """Bind the receipt to the exact output snapshot produced by this run."""
    return {
        str(path.relative_to(root)): sha256(path)
        for path in sorted((root / "outputs").rglob("*"))
        if path.is_file()
    }


def resolve_executable(value: str, execution_directory: Path) -> Path:
    candidate = Path(value).expanduser()
    if candidate.is_absolute():
        resolved = candidate.resolve()
    elif "/" in value:
        resolved = (execution_directory / candidate).resolve()
    else:
        found = shutil.which(value)
        if found is None:
            raise FileNotFoundError(f"cannot resolve executed ELMFIRE binary: {value}")
        resolved = Path(found).resolve()
    if not resolved.is_file():
        raise FileNotFoundError(f"executed ELMFIRE binary is unavailable: {resolved}")
    return resolved


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--variant", required=True)
    parser.add_argument("--executable", required=True)
    parser.add_argument("--execution-directory", required=True, type=Path)
    args = parser.parse_args()
    root = CASE_DIR / "variants" / args.variant
    fingerprint = (root / "input_fingerprint.txt").read_text(encoding="utf-8").strip()
    executable = resolve_executable(args.executable, args.execution_directory.resolve())
    payload = {
        "case_id": CASE_ID,
        "variant_id": args.variant,
        "status": "ELMFIRE_EXIT_0",
        "input_fingerprint": fingerprint,
        "oracle_source_revision": SOURCE_REVISION,
        "executable_requested": args.executable,
        "executable_resolved": str(executable),
        "executable_sha256": sha256(executable),
        "stdout_sha256": sha256(root / "logs" / "elmfire.stdout"),
        "stderr_sha256": sha256(root / "logs" / "elmfire.stderr"),
        "output_artifact_sha256": artifact_hashes(root),
        "completed_utc": datetime.now(timezone.utc).isoformat(),
    }
    logs = root / "logs"
    logs.mkdir(parents=True, exist_ok=True)
    receipt_tmp = logs / "completion_marker.json.tmp"
    receipt_tmp.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
    receipt_tmp.replace(logs / "completion_marker.json")
    fingerprint_tmp = logs / "completed_input_fingerprint.txt.tmp"
    fingerprint_tmp.write_text(fingerprint + "\n", encoding="utf-8")
    fingerprint_tmp.replace(logs / "completed_input_fingerprint.txt")


if __name__ == "__main__":
    main()
