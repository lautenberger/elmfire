#!/usr/bin/env python3
"""Bind a successful CASE45 run to its binary, inputs, oracle, logs, and outputs."""
from __future__ import annotations

import argparse
import datetime as dt
import json
from pathlib import Path

from fingerprint import (
    aggregate_fingerprint,
    file_snapshot,
    output_snapshot,
    sha256_file,
    variant_fingerprint,
)

CASE_DIR = Path(__file__).resolve().parents[1]
CASE_ID = "CASE45_HRS"
SOURCE_COMMIT = "a2dfbcdf72209733c000e5d3431e44723e15efea"


def relative_to_case(path: Path) -> str:
    try:
        return path.relative_to(CASE_DIR).as_posix()
    except ValueError:
        return str(path)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("variant")
    parser.add_argument("--executable", required=True)
    parser.add_argument("--stdout", required=True, type=Path)
    parser.add_argument("--stderr", required=True, type=Path)
    args = parser.parse_args()

    executable_requested = Path(args.executable).expanduser()
    if not executable_requested.is_absolute():
        executable_requested = CASE_DIR / executable_requested
    executable = executable_requested.resolve(strict=True)
    if not executable.is_file():
        raise SystemExit(f"executed binary is not a regular file: {executable}")

    manifest_path = CASE_DIR / "variants/run_fingerprints.json"
    oracle_path = CASE_DIR / "variants/expected.json"
    evaluator_path = CASE_DIR / "scripts/postprocess.py"
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    variants = manifest.get("variants")
    if (
        manifest.get("case_id") != CASE_ID
        or manifest.get("source_commit") != SOURCE_COMMIT
        or not isinstance(variants, dict)
        or aggregate_fingerprint(variants) != manifest.get("aggregate_sha256")
        or manifest.get("oracle_manifest_sha256") != sha256_file(oracle_path)
        or manifest.get("oracle_evaluator_sha256") != sha256_file(evaluator_path)
    ):
        raise SystemExit("runtime-input/oracle manifest does not match CASE45_HRS")

    expected = variants.get(args.variant)
    if not isinstance(expected, dict):
        raise SystemExit(f"unknown variant {args.variant}")
    root = CASE_DIR / "variants" / args.variant
    actual = variant_fingerprint(root)
    if actual["sha256"] != expected.get("sha256"):
        raise SystemExit("runtime inputs changed after preprocessing; refusing completion marker")

    variant_metadata_path = root / "variant.json"
    oracle = json.loads(oracle_path.read_text(encoding="utf-8"))
    expected_specs = {
        str(item.get("id")): item
        for item in oracle.get("variants", [])
        if isinstance(item, dict)
    }
    variant_metadata = json.loads(variant_metadata_path.read_text(encoding="utf-8"))
    if expected_specs.get(args.variant) != variant_metadata:
        raise SystemExit("variant metadata is not identical to the hashed oracle manifest")

    stdout = args.stdout.resolve(strict=True)
    stderr = args.stderr.resolve(strict=True)
    log_snapshot = file_snapshot([stdout, stderr], CASE_DIR)
    outputs = output_snapshot(root)
    marker = {
        "schema_version": 2,
        "case_id": CASE_ID,
        "variant_id": args.variant,
        "source_commit": SOURCE_COMMIT,
        "status": "ELMFIRE_EXIT_0",
        "successful_exit": True,
        "input_fingerprint_sha256": actual["sha256"],
        "runtime_input_file_count": actual["file_count"],
        "oracle_manifest_sha256": sha256_file(oracle_path),
        "oracle_evaluator_sha256": sha256_file(evaluator_path),
        "variant_metadata_sha256": sha256_file(variant_metadata_path),
        "executable_requested": args.executable,
        "executable_resolved": str(executable),
        "executable_sha256": sha256_file(executable),
        "executable_size_bytes": executable.stat().st_size,
        "log_snapshot": log_snapshot,
        "output_snapshot": outputs,
        "completed_utc": dt.datetime.now(dt.timezone.utc).isoformat(),
    }
    marker_path = root / "outputs/run_complete.json"
    marker_path.write_text(json.dumps(marker, indent=2) + "\n", encoding="utf-8")
    print(
        f"[OK] {CASE_ID}/{args.variant}: bound exit zero to "
        f"{relative_to_case(executable)} ({marker['executable_sha256']})"
    )


if __name__ == "__main__":
    main()
