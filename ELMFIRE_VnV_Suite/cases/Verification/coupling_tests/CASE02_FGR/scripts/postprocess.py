#!/usr/bin/env python3
"""Evaluate timestep-independent firebrand generation over a finite duration."""
from __future__ import annotations

from report_language import polish_figure, report_text

import json
import math
import re
from pathlib import Path

import matplotlib
import numpy as np
import rasterio
from spatial_evidence import generate_spatial_evidence

matplotlib.use("Agg")
import matplotlib.pyplot as plt

CASE_DIR = Path(__file__).resolve().parents[1]
VARIANTS_DIR = CASE_DIR / "variants"
OUTPUT_DIR = CASE_DIR / "outputs"
FIGURE_DIR = CASE_DIR / "figures"
REPORT_DIR = CASE_DIR / "report"
TOTAL_RELATIVE_TOLERANCE = 0.005
PROFILE_L1_TOLERANCE = 0.01


def latest_raster(directory: Path, prefix: str) -> Path | None:
    """Select the newest raster for a requested output prefix and fail clearly when none exists."""
    files = [p for p in directory.glob(f"{prefix}_*.tif") if "transient" not in p.name]
    return max(files, key=lambda p: (p.stat().st_mtime_ns, p.name)) if files else None


def lognormal_cdf(distance: float, mu: float, sigma: float) -> float:
    """Evaluate the configured lognormal cumulative distribution used by the analytical transport reference."""
    argument = (math.log(max(distance, 1.0e-6)) - mu) / (math.sqrt(2.0) * sigma)
    return 0.5 * (1.0 + math.erf(argument))


def analytical_profile(variant: dict) -> tuple[np.ndarray, np.ndarray, int]:
    """Integrate the analytical transport distribution over computational-cell intervals."""
    dx = float(variant["dx_m"])
    mu = float(variant["mu_downwind"])
    sigma = float(variant["sigma_downwind"])
    p_eps = float(variant["p_eps"])
    quantile = 2.3263478740408408  # standard-normal 99th percentile for p_eps=0.01
    if not math.isclose(p_eps, 0.01, abs_tol=1.0e-12):
        raise ValueError(
            "This verification currently defines the 99th-percentile truncation")
    x_quantile = math.exp(mu + sigma * quantile)
    kmax = math.floor(x_quantile / dx + 0.5)  # Fortran NINT for a positive value
    normalization = lognormal_cdf(kmax * dx, mu, sigma) - \
        lognormal_cdf(1.0e-6, mu, sigma)
    offsets = np.arange(1, kmax + 1, dtype=float)
    probability = np.array([
        (lognormal_cdf(i * dx, mu, sigma) - lognormal_cdf((i - 1) * dx, mu, sigma))
        / normalization
        for i in range(1, kmax + 1)
    ])
    return offsets * dx, probability * float(variant["expected_total_firebrands"]), kmax


def read_ember_profile(
        path: Path, variant: dict) -> tuple[np.ndarray, np.ndarray, float]:
    """Extract the physical-domain ember profile and exclude the numerical buffer cells."""
    with rasterio.open(path) as dataset:
        array = dataset.read(1).astype(float)
        nodata = dataset.nodata
        transform = dataset.transform.to_gdal()
    expected_shape = (int(variant["ny"]), int(variant["nx"]))
    if array.shape != expected_shape:
        raise ValueError(f"{path} shape {array.shape}; expected {expected_shape}")
    if not math.isclose(abs(transform[1]), float(variant["dx_m"]), abs_tol=1.0e-6):
        raise ValueError(f"{path} cell size does not match the manifest")
    if nodata is not None:
        array[array == nodata] = np.nan
    array[array < 0.0] = np.nan
    row = int(variant["ignition_row"])
    buffer_cells = int(variant["buffer_cells"])
    columns = np.arange(buffer_cells, array.shape[1] - buffer_cells)
    distances = (columns - int(variant["ignition_column"])) * float(variant["dx_m"])
    profile = array[row, columns]
    total = float(np.nansum(array))
    return distances, profile, total


def write_pdf(results: list[dict]) -> None:
    """Generate the report-ready vector PDF with Matplotlib."""
    xmax = 200.0
    shown = [float(v) for r in results for v in r["reference"] if np.isfinite(v)]
    shown += [float(v)
              for r in results for v in r.get("observed", []) if np.isfinite(v)]
    ymax = max(10.0, math.ceil(1.15 * max(shown, default=10.0) / 5.0) * 5.0)
    figure, axes = plt.subplots(
        1, len(results), figsize=(5.5 * len(results), 5.5), sharex=True, sharey=True,
        constrained_layout=True,
    )
    axes = np.atleast_1d(axes)
    for axis, result in zip(axes, results):
        observed = result.get("observed")
        if observed is not None:
            distance = np.asarray(result["distance"], dtype=float)
            observed = np.asarray(observed, dtype=float)
            valid = np.isfinite(observed) & (distance > 0.0) & (distance <= xmax)
            axis.bar(
                distance[valid], observed[valid], width=0.9 * result["dx_m"],
                color="#4c78a8", label="ELMFIRE",
            )
        reference_distance = np.asarray(result["reference_distance"], dtype=float)
        reference = np.asarray(result["reference"], dtype=float)
        valid_reference = reference_distance <= xmax
        axis.plot(
            reference_distance[valid_reference], reference[valid_reference],
            color="#d62728", linewidth=1.8, label="Analytical",
        )
        axis.set_title(
            rf"$\Delta t = {result['dt_s']:g}\ \mathrm{{s}}$ "
            f"({result['status']})", loc="left",
        )
        axis.set_xlim(0.0, xmax)
        axis.set_ylim(0.0, ymax)
        axis.grid(axis="y", color="0.85", linewidth=0.7)
        axis.set_axisbelow(True)
        axis.legend(loc="upper right")
    axes[-1].set_xticks(np.arange(0.0, xmax + 1.0, 50.0))
    axes[-1].set_xlabel("Downwind distance [m]")
    figure.supylabel("Accumulated firebrand count [pcs/cell]")
    polish_figure(figure)
    figure.savefig(FIGURE_DIR / "generation_residence_time.pdf", format="pdf")
    plt.close(figure)


# def write_error_figure(results: list[dict]) -> None:
#     """Compare count and spatial-profile errors across the timestep sweep."""
#     computed = [result for result in results if "total_relative_error" in result]
#     figure, axis = plt.subplots(figsize=(6.4, 3.8), constrained_layout=True)
#     if computed:
#         x = np.arange(len(computed), dtype=float)
#         width = 0.36
#         axis.bar(x - width / 2,
#                  [result["total_relative_error"] for result in computed],
#                  width, color="#4c78a8", label="integrated-count error")
#         axis.bar(x + width / 2,
#                  [result["profile_l1_relative_error"] for result in computed],
#                  width, color="#f58518", label=r"profile normalized $L_1$ error")
#         axis.set_xticks(x, [rf"$\Delta t={result['dt_s']:g}$ s"
#                            for result in computed])
#     else:
#         axis.text(0.5, 0.5, "Current matching outputs not available",
#                   ha="center", va="center", transform=axis.transAxes)
#     axis.axhline(TOTAL_RELATIVE_TOLERANCE, color="#4c78a8", linestyle=":",
#                  label="count-error limit")
#     axis.axhline(PROFILE_L1_TOLERANCE, color="#f58518", linestyle="--",
#                  label=r"profile-$L_1$ limit")
#     axis.set(ylabel="Relative error [-]", ylim=(0.0, None))
#     axis.grid(axis="y", alpha=0.25)
#     axis.legend(fontsize=8)
#     figure.savefig(FIGURE_DIR / "generation_residence_time_errors.pdf",
#                    format="pdf")
#     plt.close(figure)


def latex_escape(value: object) -> str:
    """Escape generated text before inserting it into a LaTeX macro or table cell."""
    return str(value).replace("_", "\\_")


def main() -> None:
    """Run postprocessing from case inputs through final generated artifacts."""
    for directory in (OUTPUT_DIR, FIGURE_DIR, REPORT_DIR):
        directory.mkdir(exist_ok=True)
    manifest_path = VARIANTS_DIR / "manifest.json"
    if not manifest_path.is_file():
        raise FileNotFoundError(f"Run preprocess.py first; missing {manifest_path}")
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    results = []
    for variant in manifest:
        ref_x, reference, kmax = analytical_profile(variant)
        output_dir = CASE_DIR / variant["directory"] / "outputs"
        raster = latest_raster(output_dir, "ember_flux")
        result = {
            "name": variant["name"], "dt_s": float(variant["dt_s"]),
            "dx_m": float(variant["dx_m"]), "status": "not_run",
            "reference_distance": ref_x, "reference": reference,
            "expected_total_firebrands": float(variant["expected_total_firebrands"]),
            "kmax": kmax,
        }
        if raster is not None:
            distance, observed, total = read_ember_profile(raster, variant)
            observed_at_ref = np.array([
                observed[np.flatnonzero(np.isclose(distance, x, atol=1.0e-6))[0]]
                for x in ref_x
            ])
            total_error = abs(
                total - result["expected_total_firebrands"]) / result["expected_total_firebrands"]
            profile_l1 = float(
                np.nansum(
                    np.abs(
                        observed_at_ref -
                        reference)) /
                result["expected_total_firebrands"])
            passed = total_error <= TOTAL_RELATIVE_TOLERANCE and profile_l1 <= PROFILE_L1_TOLERANCE
            result.update(
                status="pass" if passed else "fail",
                output_raster=str(raster.relative_to(CASE_DIR)),
                distance=distance, observed=observed,
                observed_total_firebrands=total,
                total_relative_error=total_error,
                profile_l1_relative_error=profile_l1,
            )
        results.append(result)

    write_pdf(results)
    # write_error_figure(results)
    serializable = [{k: v for k,
                     v in result.items() if k not in {"distance",
                                                      "observed",
                                                      "reference_distance",
                                                      "reference"}} for result in results]
    computed = [
        result for result in serializable if result["status"] in {
            "pass", "fail"}]
    status = "not_run" if not computed else (
        "pass" if len(computed) == len(serializable) and all(
            r["status"] == "pass" for r in computed) else "fail")
    metrics = {
        "case_id": "CASE02_FGR",
        "objective": "Verify finite-duration generation is independent of simulation timestep",
        "generation_rate_pcs_s": 10.0,
        "residence_time_s": 10.0,
        "expected_total_firebrands": 100.0,
        "analytical_profile": "100 times the cell-integrated lognormal PDF truncated and normalized at P=0.99",
        "total_relative_tolerance": TOTAL_RELATIVE_TOLERANCE,
        "profile_l1_tolerance": PROFILE_L1_TOLERANCE,
        "status": status,
        "num_variants": len(serializable),
        "num_variants_computed": len(computed),
        "variants": serializable,
    }
    metrics["verification_passed"] = status == "pass" if status in {
        "pass", "fail"} else "not_evaluated"
    (OUTPUT_DIR / "metrics.json").write_text(json.dumps(metrics, indent=2) + "\n", encoding="utf-8")

    by_name = {result["name"]: result for result in serializable}

    def value(name: str, key: str, fmt: str = ".6g") -> str:
        """Perform the case-local value operation."""
        item = by_name.get(name, {})
        return format(item[key], fmt) if key in item else "not computed"
    macros = {
        "status": status,
        "generationratepcspers": "10.0",
        "residencetimes": "10.0",
        "expectedtotal": "100.0",
        "fineobservedtotal": value("dt0p13", "observed_total_firebrands"),
        "finetotalrelativeerror": value("dt0p13", "total_relative_error", ".3e"),
        "fineprofileloneerror": value("dt0p13", "profile_l1_relative_error", ".3e"),
        "coarseobservedtotal": value("dt12p7", "observed_total_firebrands"),
        "coarsetotalrelativeerror": value("dt12p7", "total_relative_error", ".3e"),
        "coarseprofileloneerror": value("dt12p7", "profile_l1_relative_error", ".3e"),
        "numvariants": len(serializable),
        "numvariantscomputed": len(computed),
    }
    lines = [
        f"\\expandafter\\def\\csname metric@{re.sub(r'[^A-Za-z0-9]+', '', key)}\\endcsname{{{latex_escape(val)}}}" for key,
        val in macros.items()]
    (REPORT_DIR / "metrics_macros.tex").write_text(report_text("\n".join(lines) + "\n"), encoding="utf-8")
    print(f"[OK] evaluated {len(computed)}/{len(serializable)} variants: {status}")


if __name__ == "__main__":
    main()
    generate_spatial_evidence(
        CASE_DIR, output_preference=("ember_flux", "time_of_arrival"),
        preferred_variant="dt0p13", strip_width_m=1.0,
    )
