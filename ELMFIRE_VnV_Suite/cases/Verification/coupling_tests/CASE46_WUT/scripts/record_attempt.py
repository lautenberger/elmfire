#!/usr/bin/env python3
"""Maintain CASE46's fail-closed runner/solver-attempt ledger."""
from __future__ import annotations

import argparse
import datetime as dt
import json
import os
import shutil
from pathlib import Path

from fingerprint import sha256_file

CASE_DIR = Path(__file__).resolve().parents[1]
CASE_ID = "CASE46_WUT"
SOURCE_COMMIT = "a2dfbcdf72209733c000e5d3431e44723e15efea"
LEDGER_PATH = CASE_DIR / "outputs/run_attempts.json"


def timestamp() -> str:
    return dt.datetime.now(dt.timezone.utc).isoformat()


def write(payload: object) -> None:
    temporary = LEDGER_PATH.with_name(f".{LEDGER_PATH.name}.tmp")
    temporary.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
    temporary.replace(LEDGER_PATH)


def load() -> dict[str, object]:
    payload = json.loads(LEDGER_PATH.read_text(encoding="utf-8"))
    if (
        payload.get("schema_version") != 2
        or payload.get("case_id") != CASE_ID
        or payload.get("source_commit_oracle") != SOURCE_COMMIT
        or type(payload.get("run_case_invoked")) is not bool
        or not isinstance(payload.get("attempts"), list)
    ):
        raise ValueError("run-attempt ledger identity/schema is invalid")
    return payload


def resolve_executable(value: str) -> Path:
    located = shutil.which(value) if "/" not in value else value
    if not located:
        raise ValueError(f"ELMFIRE executable cannot be resolved: {value}")
    path = Path(located).expanduser().resolve(strict=True)
    if not path.is_file() or not os.access(path, os.X_OK):
        raise ValueError(f"ELMFIRE executable is not an executable file: {path}")
    return path


def main() -> None:
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="action", required=True)
    runner = subparsers.add_parser("runner-start")
    runner.add_argument("--requested-executable", required=True)
    start = subparsers.add_parser("start")
    start.add_argument("--variant", required=True)
    start.add_argument("--executable", required=True)
    start.add_argument("--stdout", required=True)
    start.add_argument("--stderr", required=True)
    finish = subparsers.add_parser("finish")
    finish.add_argument("--variant", required=True)
    finish.add_argument("--exit-code", required=True, type=int)
    args = parser.parse_args()
    payload = load()

    if args.action == "runner-start":
        if payload["run_case_invoked"] is not False or payload["attempts"]:
            raise SystemExit("run_case invocation is not starting from a clean preprocess ledger")
        payload["run_case_invoked"] = True
        payload["runner_started_utc"] = timestamp()
        payload["requested_executable"] = args.requested_executable
        write(payload)
        return

    if payload["run_case_invoked"] is not True:
        raise SystemExit("solver attempt cannot be recorded before runner-start")
    attempts = payload["attempts"]
    expected_ids = (CASE_DIR / "variants/variant_ids.txt").read_text(encoding="utf-8").splitlines()
    if args.action == "start":
        ordinal = len(attempts) + 1
        if ordinal > len(expected_ids) or args.variant != expected_ids[ordinal - 1]:
            raise SystemExit("solver attempts must follow the exact generated variant order once each")
        if attempts and attempts[-1].get("state") == "STARTED":
            raise SystemExit("previous solver attempt has no recorded exit")
        executable = resolve_executable(args.executable)
        stdout_path = Path(args.stdout).resolve()
        stderr_path = Path(args.stderr).resolve()
        variant_root = CASE_DIR / "variants" / args.variant
        if stdout_path != variant_root / "logs/elmfire.stdout" or stderr_path != variant_root / "logs/elmfire.stderr":
            raise SystemExit("solver logs are not the exact case-local paths")
        attempts.append(
            {
                "ordinal": ordinal,
                "variant_id": args.variant,
                "state": "STARTED",
                "started_utc": timestamp(),
                "execution_directory": CASE_DIR.as_posix(),
                "config": (variant_root / "elmfire.data").relative_to(CASE_DIR).as_posix(),
                "executable_resolved_path": executable.as_posix(),
                "executable_sha256_at_start": sha256_file(executable),
                "stdout": stdout_path.relative_to(CASE_DIR).as_posix(),
                "stderr": stderr_path.relative_to(CASE_DIR).as_posix(),
            }
        )
        write(payload)
        return

    if not attempts or attempts[-1].get("variant_id") != args.variant or attempts[-1].get("state") != "STARTED":
        raise SystemExit("no matching open solver attempt to finish")
    attempts[-1]["exit_code"] = args.exit_code
    attempts[-1]["finished_utc"] = timestamp()
    attempts[-1]["state"] = "EXIT_0" if args.exit_code == 0 else "EXIT_NONZERO"
    write(payload)


if __name__ == "__main__":
    main()
