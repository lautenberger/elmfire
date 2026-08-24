# -*- coding: utf-8 -*-
"""
Verify ELMFIRE outputs by comparing max values in:
  outputs/vs_*.tif   -> target ROS
  outputs/flin_*.tif -> target FI

Requirements:
  pip install rasterio pandas numpy
"""

import os
import glob
import numpy as np
import pandas as pd
import rasterio
import argparse


# -----------------------------
# User settings
# -----------------------------
ROOT_DIR = os.path.expanduser("~/elmfire/elmfire/verification/FBP_tests")  # <-- change if needed
TARGETS_CSV = os.path.join(ROOT_DIR, "targets.csv")
OUT_CSV = os.path.join(ROOT_DIR, "verification_summary.csv")


# -----------------------------
# Helpers
# -----------------------------
def safe_read_csv(path: str) -> pd.DataFrame:
    """Robust CSV/TSV reader; strips header whitespace."""
    df = pd.read_csv(path, sep=None, engine="python")
    df.columns = df.columns.str.strip()
    return df


def max_of_geotiffs(filepaths):
    """
    Returns (max_value, which_file).
    Ignores nodata, NaNs, and +/-inf.
    """
    best_val = None
    best_file = None

    for fp in filepaths:
        with rasterio.open(fp) as src:
            arr = src.read(1, masked=True)  # masked handles nodata automatically if set
            data = np.asarray(arr).astype(np.float64)

            # Masked array: compress to valid values
            if np.ma.isMaskedArray(arr):
                vals = data[~arr.mask]
            else:
                vals = data

            # Filter NaN/inf
            vals = vals[np.isfinite(vals)]

            if vals.size == 0:
                continue

            m = float(vals.max())
            if (best_val is None) or (m > best_val):
                best_val = m
                best_file = fp

    return best_val, best_file


def list_run_folders(root_dir: str):
    """Return sorted list of run folders like 01,02,... that are directories."""
    runs = []
    for name in os.listdir(root_dir):
        if len(name) == 2 and name.isdigit():
            path = os.path.join(root_dir, name)
            if os.path.isdir(path):
                runs.append(name)
    return sorted(runs)


def find_run_id_column(df: pd.DataFrame):
    """Try common run id column names; returns column name or None."""
    candidates = ["run", "case", "id", "folder", "run_id", "scenario"]
    lower_map = {c.lower(): c for c in df.columns}
    for cand in candidates:
        if cand in lower_map:
            return lower_map[cand]
    return None


# -----------------------------
# Main
# -----------------------------
def main():
    parser = argparse.ArgumentParser(description="Verify ELMFIRE outputs")
    parser.add_argument("--mode", type=int, choices=[1, 2], required=True,
                        help="1 = head_fire_* files, 2 = vs_/flin_ files")
    args = parser.parse_args()

    mode = args.mode
    if not os.path.exists(TARGETS_CSV):
        raise FileNotFoundError(f"Missing targets.csv: {TARGETS_CSV}")

    targets = safe_read_csv(TARGETS_CSV)
    pd.set_option("display.float_format", "{:,.1f}".format)

    # Required target columns
    for col in ["ROS", "FI"]:
        if col not in targets.columns:
            raise ValueError(f"targets.csv must contain a '{col}' column. Found: {list(targets.columns)}")

    run_folders = list_run_folders(ROOT_DIR)
    if not run_folders:
        raise RuntimeError(f"No run folders (e.g., '01', '02') found in: {ROOT_DIR}")

    run_id_col = find_run_id_column(targets)

    # Build a lookup for targets
    if run_id_col:
        # Normalize run ids like 1 -> "01"
        def norm_run(x):
            s = str(x).strip()
            if s.isdigit() and len(s) < 2:
                return f"{int(s):02d}"
            return s.zfill(2) if s.isdigit() and len(s) == 1 else s

        targets = targets.copy()
        targets["_run_norm"] = targets[run_id_col].apply(norm_run)
        target_map = targets.set_index("_run_norm")[["ROS", "FI"]].to_dict(orient="index")
        mapping_mode = f"Matched by targets column '{run_id_col}'"
    else:
        # Row order mapping
        if len(targets) < len(run_folders):
            raise ValueError(
                f"targets.csv has {len(targets)} rows but there are {len(run_folders)} run folders. "
                "Add rows or add a run-id column (run/case/id/folder)."
            )
        mapping_mode = "Matched by row order of targets.csv"
        target_map = {run_folders[i]: {"ROS": float(targets.iloc[i]["ROS"]), "FI": float(targets.iloc[i]["FI"])}
                      for i in range(len(run_folders))}

    rows = []
    for run in run_folders:
        out_dir = os.path.join(ROOT_DIR, run, "outputs")
        if mode == 2:
            vs_files = sorted(glob.glob(os.path.join(out_dir, "head_fire_spread_rate_*.tif")))
            flin_files = sorted(glob.glob(os.path.join(out_dir, "head_flin_*.tif")))
        elif mode == 1:
            vs_files = sorted(glob.glob(os.path.join(out_dir, "vs_*.tif")))
            flin_files = sorted(glob.glob(os.path.join(out_dir, "flin_*.tif")))

        ros_max, ros_file = max_of_geotiffs(vs_files) if vs_files else (None, None)
        fi_max, fi_file = max_of_geotiffs(flin_files) if flin_files else (None, None)

        tgt = target_map.get(run, None)
        tgt_ros = tgt["ROS"] if tgt else None
        tgt_fi = tgt["FI"] if tgt else None

        def pct_err(actual, target):
            if actual is None or target is None:
                return None
            if pd.isna(actual) or pd.isna(target):
                return None
            if target == 0:
                return None  # avoid divide-by-zero; define policy if needed
            return 100.0 * (actual - target) / target

        ros_err = pct_err(ros_max, tgt_ros)
        fi_err  = pct_err(fi_max, tgt_fi)


        rows.append({
            "run": run,
            "vs_files_found": len(vs_files),
            "flin_files_found": len(flin_files),
            "ROS_target": tgt_ros,
            "ROS_max": ros_max,
            "ROS_error": ros_err,
            "ROS_source_file": os.path.basename(ros_file) if ros_file else None,
            "FI_target": tgt_fi,
            "FI_max": fi_max,
            "FI_error": fi_err,
            "FI_source_file": os.path.basename(fi_file) if fi_file else None,
        })

    summary = pd.DataFrame(rows)

# --- Percent error calculation ---

    def percent_error(actual, target):
        if pd.isna(actual) or pd.isna(target):
            return np.nan
        if target == 0:
            return np.nan  # avoid divide-by-zero; define policy if needed
        return 100.0 * (actual - target) / target


    summary["ROS_pct_error"] = summary.apply(
        lambda r: percent_error(r["ROS_max"], r["ROS_target"]), axis=1
    )

    summary["FI_pct_error"] = summary.apply(
        lambda r: percent_error(r["FI_max"], r["FI_target"]), axis=1
    )

    # --- Pass/Fail thresholds (in percent) ---
    ROS_TOL_PCT = 10.0   # allow ±5%
    FI_TOL_PCT  = 10.0   # allow ±5%

    summary["ROS_match"] = summary["ROS_pct_error"].apply(
        lambda x: None if pd.isna(x) else abs(x) <= ROS_TOL_PCT
    )

    summary["FI_match"] = summary["FI_pct_error"].apply(
        lambda x: None if pd.isna(x) else abs(x) <= FI_TOL_PCT
    )

    # Write output
    summary.to_csv(OUT_CSV, index=False)

    print("Target mapping:", mapping_mode)
    print(f"Wrote: {OUT_CSV}")
    print()
    # Print a compact view
    cols = ["run", "ROS_target", "ROS_max", "ROS_error", "FI_target", "FI_max", "FI_error", "ROS_match", "FI_match"]
    print(summary[cols].to_string(index=False))


if __name__ == "__main__":
    main()
