#!/usr/bin/env python3
"""Extract GUIDE metrics and generate this case's report artifacts."""

from pathlib import Path

CASE_DIR = Path(__file__).resolve().parents[1]

from case_adapter import postprocess

payload = postprocess(CASE_DIR, 'CASE29_SUP', 'Case 29: Initial- and extended-attack suppression')
print(f"[OK] {'CASE29_SUP'}: {payload['overall_status']}")
