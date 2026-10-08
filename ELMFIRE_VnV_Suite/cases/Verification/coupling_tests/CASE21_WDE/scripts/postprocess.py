#!/usr/bin/env python3
"""Extract GUIDE metrics and generate this case's report artifacts."""

from pathlib import Path

CASE_DIR = Path(__file__).resolve().parents[1]

from case_adapter import postprocess

payload = postprocess(CASE_DIR, 'CASE21_WDE', 'Case 21: Wind-driven elliptical spread')
print(f"[OK] {'CASE21_WDE'}: {payload['overall_status']}")
