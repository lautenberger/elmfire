#!/usr/bin/env python3
"""Extract GUIDE metrics and generate this case's report artifacts."""

from pathlib import Path

CASE_DIR = Path(__file__).resolve().parents[1]

from case_adapter import postprocess

payload = postprocess(CASE_DIR, 'CASE27_FBT', 'Case 27: Lagrangian firebrand generation and transport')
print(f"[OK] {'CASE27_FBT'}: {payload['overall_status']}")
