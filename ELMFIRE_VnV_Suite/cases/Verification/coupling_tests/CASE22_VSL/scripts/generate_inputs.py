#!/usr/bin/env python3
"""Generate deterministic rasters for this ELMFIRE Guide verification case."""

from pathlib import Path

CASE_DIR = Path(__file__).resolve().parents[1]

from case_adapter import generate_inputs

generate_inputs(CASE_DIR)
