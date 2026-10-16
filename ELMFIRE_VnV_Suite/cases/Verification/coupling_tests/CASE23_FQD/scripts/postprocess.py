#!/usr/bin/env python3
"""Extract GUIDE metrics and generate this case's report artifacts."""

from pathlib import Path

CASE_DIR = Path(__file__).resolve().parents[1]

from case_adapter import postprocess

payload = postprocess(CASE_DIR, 'CASE23_FQD', 'Case 23: Spatially varying fuel quadrants')
print(f"[OK] {'CASE23_FQD'}: {payload['overall_status']}")
