#!/usr/bin/env python3
"""Portable target comparison for verification/FBP_tests.

Examples:
  python compare.py --root . --output-kind levelset
  python compare.py --root ./generated --output-kind headfire

Output-kind mapping is explicit to avoid the mode-label inversion in the older
script:
  levelset -> vs_*.tif and flin_*.tif
  headfire -> head_fire_spread_rate_*.tif and head_flin_*.tif
"""

from __future__ import annotations

import argparse
from pathlib import Path

import numpy as np
import pandas as pd
import rasterio


def parse_args() -> argparse.Namespace:
    here = Path(__file__).resolve().parent
    p = argparse.ArgumentParser()
    p.add_argument("--root", type=Path, default=here)
    p.add_argument("--targets", type=Path, default=None)
    p.add_argument("--output-kind", choices=("auto", "levelset", "headfire"), default="auto")
    p.add_argument("--out", type=Path, default=None)
    p.add_argument("--tolerance-pct", type=float, default=10.0)
    return p.parse_args()


def valid_values(path: Path) -> np.ndarray:
    with rasterio.open(path) as src:
        arr = src.read(1, masked=True)
    vals = arr.compressed().astype(np.float64)
    return vals[np.isfinite(vals)]


def max_from_glob(case_dir: Path, pattern: str):
    best = None
    best_file = None
    for path in sorted((case_dir / "outputs").glob(pattern)):
        vals = valid_values(path)
        if vals.size == 0:
            continue
        m = float(vals.max())
        if best is None or m > best:
            best = m
            best_file = path
    return best, best_file


def detect_kind(root: Path) -> str:
    for case in sorted(p for p in root.iterdir() if p.is_dir() and p.name.isdigit()):
        out = case / "outputs"
        if list(out.glob("head_fire_spread_rate_*.tif")):
            return "headfire"
        if list(out.glob("vs_*.tif")):
            return "levelset"
    raise RuntimeError("Could not detect outputs; specify --output-kind")


def pct_error(actual, target):
    if actual is None or pd.isna(target) or target == 0:
        return np.nan
    return 100.0 * (actual - target) / target


def main() -> None:
    args = parse_args()
    root = args.root.resolve()
    targets_path = (args.targets or (root / "targets.csv")).resolve()
    if not targets_path.is_file():
        raise FileNotFoundError(targets_path)

    kind = detect_kind(root) if args.output_kind == "auto" else args.output_kind
    ros_pattern = "head_fire_spread_rate_*.tif" if kind == "headfire" else "vs_*.tif"
    fi_pattern = "head_flin_*.tif" if kind == "headfire" else "flin_*.tif"

    targets = pd.read_csv(targets_path, sep=None, engine="python")
    targets.columns = targets.columns.str.strip()
    for col in ("ROS", "FI"):
        if col not in targets.columns:
            raise ValueError(f"{targets_path} lacks {col}")

    cases = sorted(p for p in root.iterdir() if p.is_dir() and len(p.name) == 2 and p.name.isdigit())
    rows = []
    for idx, case in enumerate(cases):
        if idx >= len(targets):
            break
        ros, ros_file = max_from_glob(case, ros_pattern)
        fi, fi_file = max_from_glob(case, fi_pattern)
        tgt_ros = float(targets.iloc[idx]["ROS"])
        tgt_fi = float(targets.iloc[idx]["FI"])
        ros_err = pct_error(ros, tgt_ros)
        fi_err = pct_error(fi, tgt_fi)
        rows.append({
            "case": case.name,
            "output_kind": kind,
            "ROS_target": tgt_ros,
            "ROS_max": ros,
            "ROS_pct_error": ros_err,
            "ROS_match": False if np.isnan(ros_err) else abs(ros_err) <= args.tolerance_pct,
            "ROS_source_file": ros_file.name if ros_file else "",
            "FI_target": tgt_fi,
            "FI_max": fi,
            "FI_pct_error": fi_err,
            "FI_match": False if np.isnan(fi_err) else abs(fi_err) <= args.tolerance_pct,
            "FI_source_file": fi_file.name if fi_file else "",
        })

    summary = pd.DataFrame(rows)
    out = args.out or (root / f"verification_summary_{kind}.csv")
    summary.to_csv(out, index=False)
    pd.set_option("display.max_columns", None)
    pd.set_option("display.width", 180)
    print(summary.to_string(index=False, float_format=lambda x: f"{x:.4f}"))
    print(f"\nWrote {out}")


if __name__ == "__main__":
    main()
