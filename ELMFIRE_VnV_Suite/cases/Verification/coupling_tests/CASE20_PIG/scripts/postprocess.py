#!/usr/bin/env python3
"""Extract GUIDE metrics and generate this case's report artifacts."""

from pathlib import Path

CASE_DIR = Path(__file__).resolve().parents[1]

from case_adapter import postprocess

payload = postprocess(CASE_DIR, 'CASE20_PIG', 'Case 20: Point ignition and isotropic spread')
print(f"[OK] {'CASE20_PIG'}: {payload['overall_status']}")
