#!/usr/bin/env python3
"""Postprocess the unit-width biomass-emission verification."""
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
FIGURE_DIR = CASE_DIR / "figures"
OUTPUT_DIR = CASE_DIR / "outputs"
REPORT_DIR = CASE_DIR / "report"
TRUNCATION_DISTANCE_M = 150.0
COMPARISON_END_M = 200.0
RELATIVE_ERROR_TOLERANCE = 0.05
UNIT_WIDTH_EMBER_GR_PER_MW_S = 33.3
FT_PER_MIN_TO_M_PER_S = 0.3048 / 60.0


def latest_raster(directory: Path, prefix: str) -> Path | None:
    """Select the newest raster for a requested output prefix and fail clearly when none exists."""
    files = [p for p in directory.glob(f"{prefix}_*.tif") if "transient" not in p.name]
    return max(files, key=lambda p: (p.stat().st_mtime_ns, p.name)) if files else None


def read_raster(path: Path, dx: float, nx: int, ny: int):
    """Read one GDAL raster into a floating-point array while preserving nodata handling at the caller."""
    with rasterio.open(path) as dataset:
        array = dataset.read(1).astype(float)
        nodata = dataset.nodata
        transform = dataset.transform.to_gdal()
    if array.shape != (ny, nx):
        raise ValueError(
            f"{path} has shape {array.shape}; expected {(ny, nx)}. "
            "Remove stale outputs and rerun ELMFIRE."
        )
    if not math.isclose(abs(transform[1]), dx, abs_tol=1e-6):
        raise ValueError(f"{path} cell size does not match manifest dx={dx:g}")
    if nodata is not None:
        array[array == nodata] = np.nan
    array[array < -1000.0] = np.nan
    return array, transform


def deposition_profile(embers, transform, dx, buffer_cells):
    """Convert pixel counts to pcs/m^2 for the represented 1 m-wide strip."""
    columns = np.arange(buffer_cells, embers.shape[1] - buffer_cells)
    density = embers[embers.shape[0] // 2, columns] / dx
    density[(density < 0.0) | ~np.isfinite(density)] = np.nan
    x = transform[0] + (columns + 0.5) * transform[1]
    return x, density


def simulation_reference(flin, spread_rate, buffer_cells):
    """Calculate the unit-width reference using steady simulated FLIN and ROS.

    FLIN output is kW/m. Since SPREAD_RATE_IN_M is false, VS output is ft/min.
    The maximum-FLIN valid interior cell represents the fully developed head fire.
    """
    interior = np.s_[buffer_cells:-buffer_cells, buffer_cells:-buffer_cells]
    f, v = flin[interior], spread_rate[interior]
    valid = np.isfinite(f) & np.isfinite(v) & (f > 0.0) & (v > 0.0)
    if not valid.any():
        raise ValueError("FLIN and VS have no common positive interior cells")
    ranked = np.where(valid, f, -np.inf)
    local = np.unravel_index(int(np.argmax(ranked)), ranked.shape)
    full = (local[0] + buffer_cells, local[1] + buffer_cells)
    flin_mw_m = float(f[local]) / 1000.0
    ros_m_s = float(v[local]) * FT_PER_MIN_TO_M_PER_S
    density = UNIT_WIDTH_EMBER_GR_PER_MW_S * flin_mw_m / ros_m_s
    return density, flin_mw_m, ros_m_s, full


def write_pdf_figure(results):
    """Generate the report-ready vector PDF with Matplotlib."""
    xmax = 220.0
    shown = [
        float(value)
        for result in results
        for x, value in zip(result.get("x", []), result.get("profile", []))
        if np.isfinite(value) and 0.0 <= x <= xmax
    ]
    shown.extend(
        result["reference_density_pcs_m2"]
        for result in results
        if "reference_density_pcs_m2" in result
    )
    ymax = max(150.0, math.ceil(1.1 * max(shown, default=150.0) / 25.0) * 25.0)
    figure, axes = plt.subplots(
        1, len(results), figsize=(8.0, 3.0), sharex=True, sharey=True,
        constrained_layout=True,
    )
    axes = np.atleast_1d(axes)
    for axis, result in zip(axes, results):
        x = np.asarray(result.get("x", []), dtype=float)
        profile = np.asarray(result.get("profile", []), dtype=float)
        valid = np.isfinite(profile) & (x >= 0.0) & (x <= xmax)
        axis.bar(
            x[valid], profile[valid], width=0.9 * result["dx_m"],
            color="#4c78a8", label="ELMFIRE",
        )
        reference = result.get("reference_density_pcs_m2")
        if reference is not None:
            axis.axhline(
                reference, color="#d62728", linewidth=1.6, linestyle="--",
                label=f"Reference: {reference:.2f}",
            )
        axis.set_title(
            rf"$\Delta x = {result['dx_m']:g}\ \mathrm{{m}}$ "
            f"({result['status']})", loc="left",
        )
        axis.set_xlim(0.0, xmax)
        axis.set_ylim(0.0, ymax)
        axis.grid(axis="y", color="0.85", linewidth=0.7)
        axis.set_axisbelow(True)
        axis.legend(loc="upper right")
    axes[-1].set_xticks(np.arange(0.0, xmax + 1.0, 50.0))
    axes[-2].set_xlabel("Downwind distance [m]")
    figure.supylabel(r"Accumulated firebrands [pcs/m$^2$]")
    polish_figure(figure)
    figure.savefig(
        FIGURE_DIR / "emission_time_resolution.pdf", format="pdf"
    )
    plt.close(figure)


# def write_error_figure(results):
#     """Plot the resolution trend of the declared far-field error metric."""
#     computed = [
#         result for result in results
#         if "far_field_mean_relative_error" in result
#     ]
#     figure, axis = plt.subplots(figsize=(6.4, 3.8), constrained_layout=True)
#     if computed:
#         dx = np.asarray([result["dx_m"] for result in computed], dtype=float)
#         mean_error = np.asarray([
#             result["far_field_mean_relative_error"] for result in computed
#         ])
#         max_error = np.asarray([
#             result["far_field_max_relative_error"] for result in computed
#         ])
#         order = np.argsort(dx)
#         axis.plot(dx[order], mean_error[order], "o-", color="#1f77b4",
#                   label="far-field mean relative error")
#         axis.plot(dx[order], max_error[order], "s--", color="#ff7f0e",
#                   label="far-field maximum relative error")
#     else:
#         axis.text(0.5, 0.5, "Current matching outputs not available",
#                   ha="center", va="center", transform=axis.transAxes)
#     axis.axhline(RELATIVE_ERROR_TOLERANCE, color="#d62728", linestyle=":",
#                  linewidth=1.6, label="mean-error acceptance limit")
#     axis.set(xlabel=r"Grid spacing, $\Delta x$ [m]",
#              ylabel="Relative error [-]", ylim=(0.0, None))
#     axis.grid(alpha=0.25)
#     axis.legend(fontsize=8)
#     figure.savefig(FIGURE_DIR / "emission_time_error.pdf", format="pdf")
#     plt.close(figure)


def main():
    """Run postprocessing from case inputs through final generated artifacts."""
    for directory in (FIGURE_DIR, OUTPUT_DIR, REPORT_DIR):
        directory.mkdir(exist_ok=True)
    manifest_path = VARIANTS_DIR / "manifest.json"
    if not manifest_path.is_file():
        raise FileNotFoundError(f"Run preprocess.py first; missing {manifest_path}")
    manifest = json.loads(manifest_path.read_text())
    results = []
    for variant in manifest:
        dx, nx, ny = float(variant["dx_m"]), int(variant["nx"]), int(variant["ny"])
        buffer_cells = int(variant["buffer_cells"])
        out = CASE_DIR / variant["directory"] / "outputs"
        paths = {
            name: latest_raster(
                out,
                name) for name in (
                "ember_flux",
                "flin",
                "vs")}
        configured = float(variant["configured_ember_gr_per_mw_s"])
        effective = configured * dx
        if not math.isclose(effective, UNIT_WIDTH_EMBER_GR_PER_MW_S, rel_tol=1e-12):
            raise ValueError(f"{variant['name']}: {configured:g} * {dx:g} != 33.3")
        result = {
            "name": variant["name"], "dx_m": dx,
            "configured_ember_gr_per_mw_s": configured,
            "effective_unit_width_ember_gr_per_mw_s": effective,
            "deposition_normalization": "ember_flux / dx for 1 m strip = pcs/m^2",
        }
        missing = [name for name, path in paths.items() if path is None]
        if missing:
            result.update(
                status="not_run",
                notes=f"Missing raster(s): {', '.join(missing)}")
            results.append(result)
            continue
        ember, transform = read_raster(paths["ember_flux"], dx, nx, ny)
        flin, _ = read_raster(paths["flin"], dx, nx, ny)
        spread, _ = read_raster(paths["vs"], dx, nx, ny)
        x, profile = deposition_profile(ember, transform, dx, buffer_cells)
        reference, flin_mw_m, ros_m_s, cell = simulation_reference(
            flin, spread, buffer_cells)
        mask = (x > TRUNCATION_DISTANCE_M) & (
            x <= COMPARISON_END_M) & np.isfinite(profile)
        result.update(
            output_raster=str(paths["ember_flux"].relative_to(CASE_DIR)),
            flin_raster=str(paths["flin"].relative_to(CASE_DIR)),
            spread_rate_raster=str(paths["vs"].relative_to(CASE_DIR)),
            reference_density_pcs_m2=reference,
            reference_fireline_intensity_mw_per_m=flin_mw_m,
            reference_ros_m_per_s=ros_m_s,
            reference_cell_row=int(cell[0]),
            reference_cell_column=int(cell[1]),
        )
        if not mask.any():
            result.update(
                status="insufficient_output",
                notes="No valid cells in 150-200 m interval")
        else:
            errors = np.abs(profile[mask] - reference)
            mean_error = float(np.mean(errors) / reference)
            result.update(
                status="pass" if mean_error <= RELATIVE_ERROR_TOLERANCE else "fail",
                far_field_mean_density_pcs_m2=float(np.mean(profile[mask])),
                far_field_mean_relative_error=mean_error,
                far_field_max_relative_error=float(np.max(errors) / reference),
                num_cells_compared=int(mask.sum()),
            )
        result["x"], result["profile"] = x, profile
        results.append(result)

    write_pdf_figure(results)
    # write_error_figure(results)
    clean = [{k: v for k, v in r.items() if k not in {"x", "profile"}} for r in results]
    computed = [r for r in clean if r["status"] in {"pass", "fail"}]
    status = "not_run" if not computed else (
        "pass" if len(computed) == len(clean) and all(
            r["status"] == "pass" for r in computed) else "fail")
    references = [r["reference_density_pcs_m2"] for r in computed]
    metrics = {
        "case_id": "CASE01_BET",
        "matlab_unit_width_ember_gr_per_mw_s": UNIT_WIDTH_EMBER_GR_PER_MW_S,
        "reference_formula": "GR_1m * (FLIN[kW/m]/1000) / (VS[ft/min]*0.3048/60)",
        "generation_scaling": "EMBER_GR_PER_MW_VEGE = 33.3 / dx",
        "deposition_scaling": "density[pcs/m^2] = ember_flux / dx for a 1 m strip",
        "relative_error_tolerance": RELATIVE_ERROR_TOLERANCE,
        "status": status,
        "num_variants": len(clean),
        "num_variants_computed": len(computed),
        "mean_simulation_reference_density_pcs_m2": float(
            np.mean(references)) if references else "not_computed",
        "mean_reference_fireline_intensity_mw_per_m": float(
            np.mean(
                [
                    r["reference_fireline_intensity_mw_per_m"] for r in computed])) if computed else "not_computed",
        "mean_reference_ros_m_per_s": float(
            np.mean(
                [
                    r["reference_ros_m_per_s"] for r in computed])) if computed else "not_computed",
        "variants": clean,
    }
    metrics["verification_passed"] = status == "pass" if status in {
        "pass", "fail"} else "not_evaluated"
    (OUTPUT_DIR / "metrics.json").write_text(json.dumps(metrics, indent=2) + "\n")
    macro_values = {
        "caseid": metrics["case_id"],
        "status": status,
        "meansimulationreferencedensitypcsm2": "{:.3f}".format(
            metrics["mean_simulation_reference_density_pcs_m2"]) if references else "not_computed",
        "meanreferenceflinmwperm": "{:.6f}".format(
            metrics["mean_reference_fireline_intensity_mw_per_m"]) if computed else "not_computed",
        "meanreferencerosmpers": "{:.6f}".format(
            metrics["mean_reference_ros_m_per_s"]) if computed else "not_computed",
        "numvariants": len(clean),
        "numvariantscomputed": len(computed),
    }
    lines = [
        f"\\expandafter\\def\\csname metric@{re.sub(r'[^A-Za-z0-9]+','',key)}\\endcsname"
        f"{{{str(value).replace('_',chr(92)+'_')}}}" for key,
        value in macro_values.items()]
    (REPORT_DIR / "metrics_macros.tex").write_text(report_text("\n".join(lines) + "\n"))
    print(f"[OK] evaluated {len(computed)}/{len(clean)} variants: {status}")


if __name__ == "__main__":
    main()
    generate_spatial_evidence(
        CASE_DIR, output_preference=("ember_flux", "time_of_arrival"),
        preferred_variant="dx10", strip_width_m=1.0,
    )
