#!/usr/bin/env python3
"""Extract GUIDE metrics and generate this case's report artifacts."""

from pathlib import Path

CASE_DIR = Path(__file__).resolve().parents[1]

from case_adapter import postprocess

payload = postprocess(CASE_DIR, 'CASE25_MQD', 'Case 25: Spatially varying moisture quadrants')
print(f"[OK] {'CASE25_MQD'}: {payload['overall_status']}")
