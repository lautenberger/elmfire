#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Mar 23 13:28:02 2026

@author: nick
"""

# compare_ros_by_fuel_model.py

from pathlib import Path
import numpy as np
import pandas as pd
import rasterio


def read_raster(path):
    """Read first band from a raster and return array, profile, nodata."""
    with rasterio.open(path) as src:
        arr = src.read(1)
        profile = src.profile.copy()
        nodata = src.nodata
    return arr, profile, nodata


def rasters_match(profile_a, profile_b, name_a="A", name_b="B"):
    """Check that two rasters are aligned."""
    keys = ["width", "height", "transform", "crs"]
    mismatches = []

    for key in keys:
        if profile_a[key] != profile_b[key]:
            mismatches.append(f"{key}: {name_a}={profile_a[key]} vs {name_b}={profile_b[key]}")

    return mismatches


def safe_r2(y_true, y_pred):
    """R^2 without sklearn. Returns NaN if undefined."""
    if y_true.size < 2:
        return np.nan

    y_mean = np.mean(y_true)
    ss_tot = np.sum((y_true - y_mean) ** 2)
    ss_res = np.sum((y_true - y_pred) ** 2)

    if np.isclose(ss_tot, 0.0):
        return np.nan

    return 1.0 - (ss_res / ss_tot)


def compute_metrics(y_true, y_pred):
    """
    Compute error metrics between truth and simulation arrays.
    Percent metrics that divide by truth only use pixels where truth != 0.
    """
    diff = y_pred - y_true
    abs_diff = np.abs(diff)

    mean_truth = np.mean(y_true)
    mean_sim = np.mean(y_pred)
    bias = np.mean(diff)
    mae = np.mean(abs_diff)
    rmse = np.sqrt(np.mean(diff ** 2))
    r2 = safe_r2(y_true, y_pred)

    # Percent bias based on mean truth
    if np.isclose(mean_truth, 0.0):
        percent_bias = np.nan
    else:
        percent_bias = 100.0 * bias / mean_truth

    # MAPE only where truth != 0
    nonzero_truth = ~np.isclose(y_true, 0.0)
    if np.any(nonzero_truth):
        mape = 100.0 * np.mean(
            np.abs((y_pred[nonzero_truth] - y_true[nonzero_truth]) / y_true[nonzero_truth])
        )
    else:
        mape = np.nan

    # sMAPE is safer when zeros are present
    denom = np.abs(y_true) + np.abs(y_pred)
    valid_smape = ~np.isclose(denom, 0.0)
    if np.any(valid_smape):
        smape = 100.0 * np.mean(
            2.0 * np.abs(y_pred[valid_smape] - y_true[valid_smape]) / denom[valid_smape]
        )
    else:
        smape = np.nan

    return {
        "n_pixels": int(y_true.size),
        "truth_mean": mean_truth,
        "sim_mean": mean_sim,
        "bias": bias,
        "percent_bias": percent_bias,
        "mae": mae,
        "rmse": rmse,
        "mape": mape,
        "smape": smape,
        "r2": r2,
    }


def build_valid_mask(fuel, truth, sim, fuel_nodata=None, truth_nodata=None, sim_nodata=None):
    """
    Build a mask of pixels valid in all three rasters.
    """
    mask = np.ones(fuel.shape, dtype=bool)

    if fuel_nodata is not None:
        mask &= fuel != fuel_nodata

    if truth_nodata is not None:
        mask &= truth != truth_nodata

    if sim_nodata is not None:
        mask &= sim != sim_nodata

    # Remove NaN/inf
    mask &= np.isfinite(fuel)
    mask &= np.isfinite(truth)
    mask &= np.isfinite(sim)

    return mask


def summarize_by_fuel_model(
    fuel_raster,
    truth_raster,
    sim_raster,
    output_csv=None,
    fuel_code_map=None,
):
    """
    Compare truth vs simulation per fuel model.
    """
    fuel, fuel_profile, fuel_nodata = read_raster(fuel_raster)
    truth, truth_profile, truth_nodata = read_raster(truth_raster)
    sim, sim_profile, sim_nodata = read_raster(sim_raster)

    # Alignment checks
    mismatches = []
    mismatches.extend(rasters_match(fuel_profile, truth_profile, "fuel", "truth"))
    mismatches.extend(rasters_match(fuel_profile, sim_profile, "fuel", "sim"))

    if mismatches:
        raise ValueError("Raster alignment mismatch:\n" + "\n".join(mismatches))

    # Cast fuel model to integer for grouping
    fuel = fuel.astype(np.int32)

    valid_mask = build_valid_mask(
        fuel=fuel,
        truth=truth,
        sim=sim,
        fuel_nodata=fuel_nodata,
        truth_nodata=truth_nodata,
        sim_nodata=sim_nodata,
    )

    fuel_valid = fuel[valid_mask]
    truth_valid = truth[valid_mask].astype(np.float64)
    sim_valid = sim[valid_mask].astype(np.float64)

    unique_fuels = np.unique(fuel_valid)

    rows = []
    for fuel_code in unique_fuels:
        fm_mask = fuel_valid == fuel_code
        y_true = truth_valid[fm_mask]
        y_pred = sim_valid[fm_mask]

        if y_true.size == 0:
            continue

        metrics = compute_metrics(y_true, y_pred)
        row = {
            "fuel_model_code": int(fuel_code),
            "fuel_model_name": fuel_code_map.get(int(fuel_code), str(int(fuel_code)))
            if fuel_code_map is not None else str(int(fuel_code)),
        }
        row.update(metrics)
        rows.append(row)

    df = pd.DataFrame(rows)

    if not df.empty:
        df = df.sort_values(["fuel_model_code"]).reset_index(drop=True)

    # Add global row across all valid pixels
    overall = compute_metrics(truth_valid, sim_valid)
    overall_row = {
        "fuel_model_code": -1,
        "fuel_model_name": "ALL_VALID_PIXELS",
    }
    overall_row.update(overall)

    df = pd.concat([df, pd.DataFrame([overall_row])], ignore_index=True)

    if output_csv is not None:
        df.to_csv(output_csv, index=False)

    return df


if __name__ == "__main__":
    # Update these paths if needed
    base_dir = Path("~/elmfire/elmfire/validation/dogrib").expanduser()
    fuel_raster = base_dir / "inputs/fbp16.tif"
    truth_raster = base_dir / "prometheus/hros.tif"
    sim_raster = base_dir / "outputs/head_fire_spread_rate_001.tif"
    output_csv = base_dir / "prometheus/ros_comparison_by_fuel_model.csv"

    # Optional: provide readable names for your FBP16 codes here
    # Edit this mapping to match your convention.
    fuel_code_map = {
        1: "1",
        2: "2",
        3: "3",
        4: "4",
        7: "7",
        13: "13",
        33: "33",
        101: "101",
        102: "102",
        640: "640",
        650: "650",
        660: "660",
    }

    df = summarize_by_fuel_model(
        fuel_raster=fuel_raster,
        truth_raster=truth_raster,
        sim_raster=sim_raster,
        output_csv=output_csv,
        fuel_code_map=fuel_code_map,
    )

    pd.set_option("display.width", 200)
    pd.set_option("display.max_columns", None)
    print(df.to_string(index=False))
    print(f"\nSaved results to: {output_csv.resolve()}")