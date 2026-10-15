#!/usr/bin/env python3
"""Extract GUIDE metrics and generate this case's report artifacts."""

from pathlib import Path

CASE_DIR = Path(__file__).resolve().parents[1]

from case_adapter import postprocess

payload = postprocess(CASE_DIR, 'CASE28_OVA', 'Case 28: Overnight spread-rate adjustment')
print(f"[OK] {'CASE28_OVA'}: {payload['overall_status']}")
