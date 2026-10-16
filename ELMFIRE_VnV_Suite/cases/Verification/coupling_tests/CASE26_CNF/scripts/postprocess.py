#!/usr/bin/env python3
"""Extract GUIDE metrics and generate this case's report artifacts."""

from pathlib import Path

CASE_DIR = Path(__file__).resolve().parents[1]

from case_adapter import postprocess

payload = postprocess(CASE_DIR, 'CASE26_CNF', 'Case 26: Surface, passive-crown, and active-crown fire')
print(f"[OK] {'CASE26_CNF'}: {payload['overall_status']}")
