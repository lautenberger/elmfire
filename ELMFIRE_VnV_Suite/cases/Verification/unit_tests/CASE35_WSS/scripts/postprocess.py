#!/usr/bin/env python3
"""Evaluate this case's required spread-rate, intensity, and TOA rasters."""
from pathlib import Path

from evaluate_outputs import evaluate, save

CASE_DIR = Path(__file__).resolve().parents[1]


def main() -> None:
    results = evaluate(CASE_DIR)
    save(CASE_DIR, results)
    print(f"[OK] {results['case_id']}: {results['overall_status']}")


if __name__ == "__main__":
    main()
