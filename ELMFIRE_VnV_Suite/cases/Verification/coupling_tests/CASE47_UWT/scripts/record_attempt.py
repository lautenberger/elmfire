#!/usr/bin/env python3
"""Atomically record runtime attempts against current prepared inputs."""
from __future__ import annotations

import argparse
import json
from datetime import datetime, timezone
from pathlib import Path

CASE_DIR = Path(__file__).resolve().parents[1]
CASE_ID = "CASE47_UWT"
LEDGER_PATH = CASE_DIR / "logs" / "run_attempts.json"


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--variant", required=True)
    parser.add_argument("--executable", required=True)
    parser.add_argument("--status", choices=("STARTED", "FAILED", "COMPLETED"), required=True)
    parser.add_argument("--exit-code", type=int)
    parser.add_argument(
        "--failure-stage",
        choices=("ELMFIRE_EXECUTION", "COMPLETION_RECEIPT"),
    )
    args = parser.parse_args()

    expected = {
        line.strip()
        for line in (CASE_DIR / "variants" / "variant_ids.txt").read_text(
            encoding="utf-8"
        ).splitlines()
        if line.strip()
    }
    if args.variant not in expected:
        raise ValueError(f"variant is not in the current run plan: {args.variant}")
    if args.status == "FAILED":
        if args.exit_code is None or args.exit_code == 0 or args.failure_stage is None:
            raise ValueError("FAILED attempts require a nonzero exit code and failure stage")
    elif args.exit_code not in (None, 0) or args.failure_stage is not None:
        raise ValueError("only FAILED attempts may carry failure details")

    fingerprint = (
        CASE_DIR / "variants" / args.variant / "input_fingerprint.txt"
    ).read_text(encoding="utf-8").strip()
    if len(fingerprint) != 64 or any(
        character not in "0123456789abcdef" for character in fingerprint.lower()
    ):
        raise ValueError("current input fingerprint is malformed")

    if LEDGER_PATH.exists():
        ledger = json.loads(LEDGER_PATH.read_text(encoding="utf-8"))
        if ledger.get("case_id") != CASE_ID or not isinstance(ledger.get("attempts"), list):
            raise ValueError("existing attempt ledger is malformed")
    else:
        ledger = {"case_id": CASE_ID, "attempts": []}

    attempts = ledger["attempts"]
    matches = [item for item in attempts if item.get("variant_id") == args.variant]
    if len(matches) > 1:
        raise ValueError("attempt ledger contains duplicate variant records")
    now = datetime.now(timezone.utc).isoformat()
    if matches:
        record = matches[0]
        if record.get("input_fingerprint") != fingerprint:
            raise ValueError("attempt ledger fingerprint is stale")
        if args.status == "STARTED":
            raise ValueError("variant already has an attempt record")
    else:
        if args.status != "STARTED":
            raise ValueError("an attempt must be STARTED before it can be updated")
        record = {
            "variant_id": args.variant,
            "input_fingerprint": fingerprint,
            "attempted_utc": now,
        }
        attempts.append(record)

    record.update({
        "status": args.status,
        "executable_requested": args.executable,
        "updated_utc": now,
    })
    record.pop("exit_code", None)
    record.pop("failure_stage", None)
    if args.status == "FAILED":
        record["exit_code"] = args.exit_code
        record["failure_stage"] = args.failure_stage
    elif args.status == "COMPLETED":
        record["exit_code"] = 0

    attempts.sort(key=lambda item: str(item["variant_id"]))
    LEDGER_PATH.parent.mkdir(parents=True, exist_ok=True)
    temporary = LEDGER_PATH.with_suffix(".json.tmp")
    temporary.write_text(json.dumps(ledger, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    temporary.replace(LEDGER_PATH)


if __name__ == "__main__":
    main()
