#!/usr/bin/env python3
"""Post-process old/new CFFDRS/FBP verification suites.

Expected suite layout under --work-root:
  old_legacy_mode2/
  new_legacy_mode2/
  new_dynamic_mode2/
  old_legacy_mode3/
  new_legacy_mode3/
  new_dynamic_mode3/
Optional:
  new_dynamic_mode2_debug/
  mpi_smoke/np1, np2, ...

Writes scalar target comparisons, pairwise raster-difference metrics, timing
summaries, CFFDRS diagnostic summaries, MPI reproducibility metrics, plots, and
a Markdown report.
"""

from __future__ import annotations

import argparse
import math
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import rasterio

SUITES = {
    "old_legacy_mode2": ("old_legacy", 2),
    "new_legacy_mode2": ("new_legacy", 2),
    "new_dynamic_mode2": ("new_dynamic", 2),
    "old_legacy_mode3": ("old_legacy", 3),
    "new_legacy_mode3": ("new_legacy", 3),
    "new_dynamic_mode3": ("new_dynamic", 3),
}

PAIRINGS = [
    ("old_legacy", "new_legacy", "prior_legacy_vs_new_legacy_physics"),
    ("new_legacy", "new_dynamic", "new_legacy_vs_new_dynamic_input_route"),
    ("old_legacy", "new_dynamic", "prior_legacy_vs_new_dynamic_end_to_end"),
]

DIAGNOSTICS = ["wsv", "isi", "rsi", "be", "sfc", "rso", "cfb", "cfc", "tfc", "sros", "bui", "gcf"]


def args() -> argparse.Namespace:
    p = argparse.ArgumentParser()
    p.add_argument("--work-root", type=Path, required=True)
    p.add_argument("--out-dir", type=Path, default=None)
    return p.parse_args()


def case_dirs(root: Path):
    return sorted(p for p in root.iterdir() if p.is_dir() and len(p.name) == 2 and p.name.isdigit())


def read_masked(path: Path):
    with rasterio.open(path) as src:
        a = src.read(1, masked=True).astype(np.float64)
        meta = (src.width, src.height, src.transform, src.crs)
    return a, meta


def values(path: Path) -> np.ndarray:
    a, _ = read_masked(path)
    v = a.compressed()
    return v[np.isfinite(v)]


def max_glob(out: Path, pattern: str):
    best = None
    best_path = None
    for p in sorted(out.glob(pattern)):
        v = values(p)
        if not len(v):
            continue
        m = float(v.max())
        if best is None or m > best:
            best, best_path = m, p
    return best, best_path


def latest(out: Path, pattern: str) -> Path | None:
    matches = sorted(out.glob(pattern))
    return matches[-1] if matches else None


def patterns(mode: int):
    if mode == 2:
        return "head_fire_spread_rate_*.tif", "head_flin_*.tif"
    return "vs_*.tif", "flin_*.tif"


def raster_metrics(a_path: Path | None, b_path: Path | None):
    if a_path is None or b_path is None:
        return None
    a, ma = read_masked(a_path)
    b, mb = read_masked(b_path)
    if ma != mb:
        return {"aligned": False}
    mask = np.ma.getmaskarray(a) | np.ma.getmaskarray(b) | ~np.isfinite(a.data) | ~np.isfinite(b.data)
    av = a.data[~mask]
    bv = b.data[~mask]
    if av.size == 0:
        return {"aligned": True, "n": 0}
    d = bv - av
    corr = np.nan
    if av.size > 1 and np.std(av) > 0 and np.std(bv) > 0:
        corr = float(np.corrcoef(av, bv)[0, 1])
    return {
        "aligned": True,
        "n": int(av.size),
        "bias": float(np.mean(d)),
        "mae": float(np.mean(np.abs(d))),
        "rmse": float(np.sqrt(np.mean(d * d))),
        "max_abs": float(np.max(np.abs(d))),
        "corr": corr,
    }


def load_suite(suite_root: Path, label: str, mode: int):
    targets_path = suite_root / "targets.csv"
    targets = pd.read_csv(targets_path, sep=None, engine="python") if targets_path.is_file() else pd.DataFrame()
    if not targets.empty:
        targets.columns = targets.columns.str.strip()
    timing_path = suite_root / "timings_external.csv"
    timing = pd.read_csv(timing_path, dtype={"case": str}) if timing_path.is_file() else pd.DataFrame()
    if not timing.empty:
        timing["case"] = timing["case"].astype(str).str.zfill(2)
    timing_map = timing.set_index("case").to_dict("index") if not timing.empty else {}

    ros_pat, fi_pat = patterns(mode)
    rows = []
    file_map = {}
    for idx, case in enumerate(case_dirs(suite_root)):
        ros, ros_path = max_glob(case / "outputs", ros_pat)
        fi, fi_path = max_glob(case / "outputs", fi_pat)
        tgt_ros = float(targets.iloc[idx]["ROS"]) if idx < len(targets) and "ROS" in targets else np.nan
        tgt_fi = float(targets.iloc[idx]["FI"]) if idx < len(targets) and "FI" in targets else np.nan
        t = timing_map.get(case.name, {})
        rows.append({
            "suite": suite_root.name,
            "implementation": label,
            "mode": mode,
            "case": case.name,
            "ROS_target": tgt_ros,
            "ROS_max": ros,
            "ROS_pct_error": np.nan if ros is None or not np.isfinite(tgt_ros) or tgt_ros == 0 else 100.0 * (ros - tgt_ros) / tgt_ros,
            "FI_target": tgt_fi,
            "FI_max": fi,
            "FI_pct_error": np.nan if fi is None or not np.isfinite(tgt_fi) or tgt_fi == 0 else 100.0 * (fi - tgt_fi) / tgt_fi,
            "wall_s": pd.to_numeric(t.get("wall_s", np.nan), errors="coerce"),
            "user_s": pd.to_numeric(t.get("user_s", np.nan), errors="coerce"),
            "sys_s": pd.to_numeric(t.get("sys_s", np.nan), errors="coerce"),
            "max_rss_kb": pd.to_numeric(t.get("max_rss_kb", np.nan), errors="coerce"),
            "exit_code": pd.to_numeric(t.get("exit_code", np.nan), errors="coerce"),
        })
        file_map[case.name] = {"ros": ros_path, "fi": fi_path}
    return pd.DataFrame(rows), file_map


def make_line_plot(df: pd.DataFrame, mode: int, metric: str, out: Path):
    sub = df[df["mode"] == mode].copy()
    if sub.empty:
        return
    target_col = f"{metric}_target"
    model_col = f"{metric}_max"
    pivot = sub.pivot(index="case", columns="implementation", values=model_col)
    targets = sub.drop_duplicates("case").set_index("case")[target_col].reindex(pivot.index)
    x = np.arange(len(pivot.index))
    plt.figure(figsize=(11, 6))
    plt.plot(x, targets.values, marker="o", label=f"{metric} target")
    for col in ("old_legacy", "new_legacy", "new_dynamic"):
        if col in pivot:
            plt.plot(x, pivot[col].values, marker="o", label=col)
    plt.xticks(x, pivot.index, rotation=90)
    plt.xlabel("FBP verification case")
    plt.ylabel(metric)
    plt.title(f"CFFDRS/FBP {metric}: targets and model outputs, MODE={mode}")
    plt.legend()
    plt.tight_layout()
    plt.savefig(out, dpi=180)
    plt.close()


def make_runtime_plot(df: pd.DataFrame, mode: int, out: Path):
    sub = df[df["mode"] == mode].copy()
    if sub.empty or sub["wall_s"].isna().all():
        return
    pivot = sub.pivot(index="case", columns="implementation", values="wall_s")
    x = np.arange(len(pivot.index))
    plt.figure(figsize=(11, 6))
    for col in ("old_legacy", "new_legacy", "new_dynamic"):
        if col in pivot:
            plt.plot(x, pivot[col].values, marker="o", label=col)
    plt.xticks(x, pivot.index, rotation=90)
    plt.xlabel("FBP verification case")
    plt.ylabel("Wall time (s)")
    plt.title(f"ELMFIRE wall time by FBP case, MODE={mode}")
    plt.legend()
    plt.tight_layout()
    plt.savefig(out, dpi=180)
    plt.close()


def summarize_diagnostics(debug_root: Path) -> pd.DataFrame:
    rows = []
    if not debug_root.is_dir():
        return pd.DataFrame()
    for case in case_dirs(debug_root):
        for q in DIAGNOSTICS:
            p = latest(case / "outputs", f"cffdrs_{q}_*.tif")
            if p is None and q == "wsv":
                p = latest(case / "outputs", "cffdrs_debug_*.tif")
            if p is None:
                continue
            v = values(p)
            if not len(v):
                continue
            rows.append({
                "case": case.name,
                "quantity": q,
                "min": float(v.min()),
                "mean": float(v.mean()),
                "max": float(v.max()),
                "std": float(v.std()),
                "file": p.name,
            })
    return pd.DataFrame(rows)


def summarize_mpi(mpi_root: Path) -> pd.DataFrame:
    if not mpi_root.is_dir():
        return pd.DataFrame()
    roots = sorted((p for p in mpi_root.glob("np*") if p.is_dir()), key=lambda p: int(p.name[2:]))
    if not roots:
        return pd.DataFrame()
    base = roots[0]
    rows = []
    for other in roots[1:]:
        for metric, pattern in [("ROS", "head_fire_spread_rate_*.tif"), ("FI", "head_flin_*.tif")]:
            a = latest(base / "01" / "outputs", pattern)
            b = latest(other / "01" / "outputs", pattern)
            m = raster_metrics(a, b)
            if m is None:
                continue
            row = {"baseline": base.name, "comparison": other.name, "metric": metric}
            row.update(m)
            rows.append(row)
    return pd.DataFrame(rows)


def main() -> None:
    a = args()
    work = a.work_root.resolve()
    out = (a.out_dir or (work / "comparison")).resolve()
    out.mkdir(parents=True, exist_ok=True)

    all_rows = []
    maps = {}
    for suite, (label, mode) in SUITES.items():
        root = work / suite
        if not root.is_dir():
            continue
        df, fmap = load_suite(root, label, mode)
        all_rows.append(df)
        maps[(label, mode)] = fmap

    if not all_rows:
        raise RuntimeError(f"No expected suites found under {work}")
    summary = pd.concat(all_rows, ignore_index=True)
    summary.to_csv(out / "model_case_summary.csv", index=False)

    pair_rows = []
    for mode in (2, 3):
        ros_pat, fi_pat = patterns(mode)
        for left, right, pair_name in PAIRINGS:
            lm = maps.get((left, mode), {})
            rm = maps.get((right, mode), {})
            for case in sorted(set(lm).intersection(rm)):
                for metric in ("ros", "fi"):
                    m = raster_metrics(lm[case].get(metric), rm[case].get(metric))
                    if m is None:
                        continue
                    row = {"mode": mode, "pair": pair_name, "left": left, "right": right, "case": case, "metric": metric.upper()}
                    row.update(m)
                    pair_rows.append(row)
    pair_df = pd.DataFrame(pair_rows)
    pair_df.to_csv(out / "pairwise_raster_metrics.csv", index=False)

    diag = summarize_diagnostics(work / "new_dynamic_mode2_debug")
    if not diag.empty:
        diag.to_csv(out / "new_dynamic_cffdrs_diagnostics.csv", index=False)

    mpi = summarize_mpi(work / "mpi_smoke")
    if not mpi.empty:
        mpi.to_csv(out / "mpi_reproducibility.csv", index=False)

    runtime = summary.groupby(["mode", "implementation"], as_index=False).agg(
        total_wall_s=("wall_s", "sum"),
        mean_wall_s=("wall_s", "mean"),
        median_wall_s=("wall_s", "median"),
        max_rss_kb=("max_rss_kb", "max"),
        mean_abs_ROS_pct_error=("ROS_pct_error", lambda s: float(np.nanmean(np.abs(s)))),
        mean_abs_FI_pct_error=("FI_pct_error", lambda s: float(np.nanmean(np.abs(s)))),
    )
    runtime.to_csv(out / "runtime_and_accuracy_summary.csv", index=False)

    for mode in (2, 3):
        make_line_plot(summary, mode, "ROS", out / f"ros_target_comparison_mode{mode}.png")
        make_line_plot(summary, mode, "FI", out / f"fi_target_comparison_mode{mode}.png")
        make_runtime_plot(summary, mode, out / f"runtime_by_case_mode{mode}.png")

    report = [
        "# CFFDRS/FBP old vs new comparison",
        "",
        "## Runtime and target-error summary",
        "",
        runtime.to_markdown(index=False, floatfmt=".4g"),
        "",
        "## Interpretation",
        "",
        "- `old_legacy` and `new_legacy` use the same physical legacy-style inputs. Their differences isolate implementation changes rather than the new BUI/GCF/m/s input route.",
        "- `new_legacy` versus `new_dynamic` tests the new dynamic-input plumbing. Large differences here indicate an input-unit, BUI, GCF, raster-I/O, or MPI/state-handling issue rather than an FBP-equation change.",
        "- MODE=2 is the direct head-fire calculation test. MODE=3 includes level-set propagation and is therefore a broader integration test.",
        "- Runtime comparisons use runs with CFFDRS diagnostic dumping disabled so the new implementation is not penalized for writing its additional diagnostic rasters.",
    ]
    if not pair_df.empty:
        agg = pair_df.groupby(["mode", "pair", "metric"], as_index=False).agg(mean_rmse=("rmse", "mean"), max_abs=("max_abs", "max"))
        report += ["", "## Pairwise raster differences", "", agg.to_markdown(index=False, floatfmt=".4g")]
    if not mpi.empty:
        report += ["", "## MPI reproducibility smoke test", "", mpi.to_markdown(index=False, floatfmt=".4g")]
    (out / "comparison_report.md").write_text("\n".join(report) + "\n")

    print(runtime.to_string(index=False))
    print(f"\nWrote comparison products to {out}")


if __name__ == "__main__":
    main()
