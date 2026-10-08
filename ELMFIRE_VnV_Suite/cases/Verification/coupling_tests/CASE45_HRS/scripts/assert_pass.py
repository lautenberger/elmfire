#!/usr/bin/env python3
"""Return success only for a complete, scientifically passing CASE45 result."""
from __future__ import annotations

import json
from pathlib import Path

CASE_DIR = Path(__file__).resolve().parents[1]
payload = json.loads((CASE_DIR / "outputs/metrics.json").read_text(encoding="utf-8"))
if not (
    payload.get("case_id") == "CASE45_HRS"
    and payload.get("overall_status") == "PASS"
    and payload.get("workflow_status") == "COMPLETE"
    and payload.get("verification_passed") is True
    and payload.get("required_outputs_complete") is True
    and payload.get("required_variant_count") == 12
    and payload.get("completed_variant_count") == 12
    and isinstance(payload.get("metrics"), list)
    and len(payload["metrics"]) == 7
    and all(item.get("status") == "PASS" for item in payload["metrics"])
):
    raise SystemExit("CASE45_HRS did not produce a complete all-PASS decision")
print("[PASS] CASE45_HRS complete scientific gate")
