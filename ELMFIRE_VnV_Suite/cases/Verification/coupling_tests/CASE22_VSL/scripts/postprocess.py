#!/usr/bin/env python3
"""Extract GUIDE metrics and generate this case's report artifacts."""

from pathlib import Path

CASE_DIR = Path(__file__).resolve().parents[1]

from case_adapter import postprocess

payload = postprocess(CASE_DIR, 'CASE22_VSL', 'Case 22: Spread across opposing valley slopes')
print(f"[OK] {'CASE22_VSL'}: {payload['overall_status']}")
