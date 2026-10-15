#!/usr/bin/env python3
"""Generate the CASE46 wildland-to-urban transition matrix and preflight.

No ELMFIRE source checkout is inspected at runtime.  The selector finding is a
reviewed, version-pinned design fact carried as a machine-readable artifact.
"""
from __future__ import annotations

from report_language import polish_figure

import json
import math
import shutil
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

# Fonts are sized for a 7-inch figure printed at full report width.
plt.rcParams.update({
    "font.size": 13, "axes.labelsize": 13, "axes.titlesize": 14,
    "xtick.labelsize": 12, "ytick.labelsize": 12, "legend.fontsize": 12,
    "figure.titlesize": 14, "lines.linewidth": 1.8,
    "pdf.fonttype": 42, "savefig.pad_inches": 0.12,
})

import numpy as np
import rasterio
from matplotlib.colors import BoundaryNorm, ListedColormap
from rasterio.transform import from_origin

from fingerprint import (
    aggregate_fingerprint,
    oracle_artifact_fingerprint,
    sha256_file,
    variant_fingerprint,
)

CASE_DIR = Path(__file__).resolve().parents[1]
CASE_ID = "CASE46_WUT"
SOURCE_COMMIT = "a2dfbcdf72209733c000e5d3431e44723e15efea"
SIZE = 25
DX_M = 10.0
TRANSFORM = from_origin(-125.0, 125.0, DX_M, DX_M)
CRS = "EPSG:32610"
NODATA = -9999.0
SOURCE_ROW = 12
SOURCE_COL = 11
SOURCE_REVIEW = {
    "checkout_state_at_design_review": "dirty outside the reviewed WU-E files",
    "reviewed_files_match_commit": True,
    "reviewed_file_sha256": {
        "build/source/elmfire_vars.f90": "5ad4a4fdd7e3faf98d839fb75af5b3c1178cadd335142ab960adec2462c7896c",
        "build/source/elmfire_init.f90": "06d2be089db961d1af387f4d1cfb60a6ab28f2daa50d7e4ada8bfda58cd87c0d",
        "build/source/elmfire_namelists.f90": "2fcd9e57a2bec5df964d96ccef0f658b703e0406abb19c13cb8080af066c78b4",
        "build/source/elmfire_spread_rate.f90": "eed515c2f44b5582c01b1573e41a7e24bf76f0a445870f27ba7d32d281e656a4",
        "build/source/elmfire_level_set.f90": "18c446fce24ac5fac8dd174e38e273bfdddecfa81c9371550b247055b6d833b3",
    },
    "known_modified_paths_outside_reviewed_files": [
        "build/source/elmfire.f90",
        "build/source/elmfire_io.f90",
        "build/source/elmfire_subs.f90",
    ],
    "untracked_entries_present_at_design_review": True,
    "untracked_paths_at_design_review": [
        ".vscode/",
        "ELMFIRE_VnV_Suite/",
        "Thomas_VAL/",
        "VERIFICATION_TROUBLESHOOT_DWI/",
        "notebooks/",
        "thomas_fire/",
        "verification/spotting-suite-legacy/",
        "verification/spotting-suite/",
    ],
    "runtime_source_inspection": False,
    "additional_pinned_blob_review": {
        "build/source/elmfire_io.f90": {
            "sha256": "33831af878791501cf8ad32ba28d5128bb4463f7a8fdef51ab20b907a5620732",
            "note": "git-show blob at the pinned commit; the working checkout copy was modified outside the cited dump paths",
            "reviewed_locations": [
                "build/source/elmfire_io.f90:2451-2508",
                "build/source/elmfire_io.f90:2620-2628",
                "build/source/elmfire_io.f90:2693-2703",
                "build/source/elmfire_io.f90:2761-2764",
            ],
        }
    },
}
TECHNICAL_REFERENCES = [
    {
        "path": "Thesis(PubLetterIncluded)_YirenQin_20250623.pdf",
        "sha256": "09ba61fb6b5f038b4d9613332f7a9b0b99626fc8afb0526eec44d43cb8a9aefa",
        "pdf_pages": [53, 64],
        "printed_pages": [31, 42],
        "equations": ["(2.42)-(2.43)", "(2.81)-(2.83)"],
        "use": "fireline-intensity/residence-time scaling and vegetation heat-release context; the primary transition oracle remains accumulated heat over fixed FTP_CRIT",
    },
]


def write_text_atomic(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f".{path.name}.tmp")
    temporary.write_text(text, encoding="utf-8")
    temporary.replace(path)


def write_json_atomic(path: Path, payload: object) -> None:
    write_text_atomic(path, json.dumps(payload, indent=2) + "\n")


def fuel1_reference() -> dict[str, float]:
    """Closed Rothermel reference for the one-class FBFM01 source at 5% M1."""
    load, sigma, depth, extinction, heat = 0.034, 3500.0, 1.0, 0.12, 8000.0
    particle_density, total_mineral, effective_mineral = 32.0, 0.055, 0.01
    beta = load / (depth * particle_density)
    beta_opt = 3.348 / sigma**0.8189
    xi = math.exp((0.792 + 0.681 * math.sqrt(sigma)) * (0.1 + beta)) / (192.0 + 0.2595 * sigma)
    exponent = 133.0 / sigma**0.7913
    gamma_peak = sigma**1.5 / (495.0 + 0.0594 * sigma**1.5)
    gamma = gamma_peak * (beta / beta_opt) ** exponent * math.exp(exponent * (1.0 - beta / beta_opt))
    eta_s = 0.174 / effective_mineral**0.19
    ratio = 0.05 / extinction
    eta_m = max(0.0, min(1.0, 1.0 - 2.59 * ratio + 5.11 * ratio**2 - 3.52 * ratio**3))
    ir_native = gamma * load * (1.0 - total_mineral) * eta_s * heat * eta_m
    epsilon = math.exp(-138.0 / sigma)
    heat_sink = (load / depth) * epsilon * (250.0 + 1116.0 * 0.05)
    ros_ft_min = ir_native * xi / heat_sink
    ir_kw_m2 = ir_native * 1.055 / (60.0 * 0.3048**2)
    residence_min = 384.0 / sigma
    flin_kw_m = residence_min * ir_kw_m2 * ros_ft_min * 0.3048
    return {
        "unadjusted_ros_ft_min": ros_ft_min,
        "reaction_intensity_kw_m2": ir_kw_m2,
        "residence_time_min": residence_min,
        "unadjusted_flin_kw_m": flin_kw_m,
    }


def fuel1_real32_state() -> dict[str, np.float32]:
    """Transcribe the one-dead-class source calculations in default REAL."""
    f = np.float32
    load, sigma, depth = f(0.034), f(3500.0), f(1.0)
    particle_density, total_mineral = f(32.0), f(0.055)
    effective_mineral, heat, extinction = f(0.01), f(8000.0), f(0.12)
    mineral_damping = f(f(0.174) / f(effective_mineral ** f(0.19)))

    area_dead = max(f(f(sigma * load) / particle_density), f(1.0e-9))
    area_live = f(1.0e-9)
    area_overall = f(area_dead + area_live)
    dead_fraction = f(area_dead / area_overall)
    live_fraction = f(area_live / area_overall)
    class_fraction = f(f(f(sigma * load) / particle_density) / area_dead)
    epsilon = f(np.exp(f(f(-138.0) / sigma)))
    net_dead_load = f(f(class_fraction * load) * f(f(1.0) - total_mineral))
    sigma_dead = f(class_fraction * sigma)
    sigma_overall = f(dead_fraction * sigma_dead + live_fraction * f(0.0))
    beta = f(load / f(depth * particle_density))
    beta_opt = f(f(3.348) / f(sigma_overall ** f(0.8189)))
    propagating_flux = f(
        np.exp(
            f(
                f(f(0.792) + f(f(0.681) * f(np.sqrt(sigma_overall))))
                * f(f(0.1) + beta)
            )
        )
        / f(f(192.0) + f(f(0.2595) * sigma_overall))
    )
    exponent = f(f(133.0) / f(sigma_overall ** f(0.7913)))
    sigma_power = f(sigma_overall ** f(1.5))
    gamma_peak = f(sigma_power / f(f(495.0) + f(f(0.0594) * sigma_power)))
    beta_ratio = f(beta / beta_opt)
    gamma = f(
        f(gamma_peak * f(beta_ratio ** exponent))
        * f(np.exp(f(exponent * f(f(1.0) - beta_ratio))))
    )
    residence = f(f(384.0) / sigma_overall)
    reaction_prefactor = f(
        f(f(f(gamma * net_dead_load) * mineral_damping) * heat)
    )

    moisture = f(0.05)
    qig = f(f(250.0) + f(f(1116.0) * moisture))
    heat_sink = f(f(load / depth) * f(epsilon * qig))
    moisture_ratio = f(moisture / extinction)
    moisture_ratio2 = f(moisture_ratio * moisture_ratio)
    moisture_damping = f(
        f(
            f(f(1.0) - f(f(2.59) * moisture_ratio))
            + f(f(5.11) * moisture_ratio2)
        )
        - f(f(3.52) * f(moisture_ratio2 * moisture_ratio))
    )
    reaction_native = f(reaction_prefactor * moisture_damping)
    conversion = f(f(1.055) / f(f(f(60.0) * f(0.3048)) * f(0.3048)))
    reaction_si = f(reaction_native * conversion)
    return {
        "propagating_flux_ratio": propagating_flux,
        "residence_time_min": residence,
        "reaction_intensity_native": reaction_native,
        "reaction_intensity_kw_m2": reaction_si,
        "heat_sink": heat_sink,
    }


def source_flin_real32(
    adjustment: float, state: dict[str, np.float32]
) -> dict[str, float]:
    """Evaluate the zero-wind/slope FBFM01 response in binary32."""
    f = np.float32
    adj = f(adjustment)
    ros = f(
        f(
            f(
                f(f(adj + f(0.0)) * f(1.0))
                * state["reaction_intensity_native"]
            )
            * state["propagating_flux_ratio"]
        )
        / state["heat_sink"]
    )
    flin = f(
        f(f(state["residence_time_min"] * state["reaction_intensity_kw_m2"]) * ros)
        * f(0.3048)
    )
    return {
        "adjustment_float32": float(adj),
        "surface_ros_ft_min": float(ros),
        "source_flin_kw_m": float(flin),
    }


def tune_adjustment_real32(
    target_flin_kw_m: float,
    state: dict[str, np.float32],
    *,
    require_exact: bool = False,
) -> tuple[float, dict[str, float]]:
    """Select a nonnegative binary32 ADJ value nearest the requested FLIN."""
    base = source_flin_real32(1.0, state)["source_flin_kw_m"]
    center = np.float32(0.0 if target_flin_kw_m == 0.0 else target_flin_kw_m / base)
    candidates = {float(center)}
    lower, upper = center, center
    for _ in range(512):
        lower = np.nextafter(lower, np.float32(-np.inf))
        upper = np.nextafter(upper, np.float32(np.inf))
        if lower >= np.float32(0.0):
            candidates.add(float(lower))
        candidates.add(float(upper))
    evaluated = [
        (adj, source_flin_real32(adj, state)) for adj in candidates
    ]
    exact = [
        pair
        for pair in evaluated
        if pair[1]["source_flin_kw_m"] == target_flin_kw_m
    ]
    if require_exact:
        if not exact:
            raise ValueError(
                f"no binary32 ADJ value realizes exactly {target_flin_kw_m} kW/m"
            )
        return min(exact, key=lambda pair: abs(pair[0] - float(center)))
    return min(
        evaluated,
        key=lambda pair: (
            abs(pair[1]["source_flin_kw_m"] - target_flin_kw_m),
            abs(pair[0] - float(center)),
        ),
    )


def write_raster(path: Path, values: np.ndarray | float, dtype: str) -> None:
    data = values if isinstance(values, np.ndarray) else np.full((SIZE, SIZE), values)
    with rasterio.open(
        path,
        "w",
        driver="GTiff",
        width=SIZE,
        height=SIZE,
        count=1,
        dtype=dtype,
        crs=CRS,
        transform=TRANSFORM,
        nodata=NODATA,
        compress="deflate",
    ) as dst:
        dst.write(np.asarray(data, dtype=dtype), 1)


def concrete_namelist(template: str, variant_id: str) -> str:
    replacements = {
        "@INPUT_DIR@": f"./variants/{variant_id}/inputs",
        "@OUTPUT_DIR@": f"./variants/{variant_id}/outputs",
        "@MISC_DIR@": f"./variants/{variant_id}/misc",
        "@SCRATCH_DIR@": f"./variants/{variant_id}/scratch",
    }
    for token, value in replacements.items():
        if template.count(token) == 0:
            raise ValueError(f"missing template token {token}")
        template = template.replace(token, value)
    if "@" in template:
        raise ValueError("unexpanded template token")
    return template


def variant_specs(real32_state: dict[str, np.float32]) -> list[dict[str, object]]:
    raw = [
        ("contiguous_below", "contiguous_interface", 1, 999.0),
        ("contiguous_equal", "contiguous_interface", 1, 1000.0),
        ("contiguous_above", "contiguous_interface", 1, 1001.0),
        ("isolated_d1_insufficient", "isolated_pixel", 1, 999.0),
        ("isolated_d1_sufficient", "isolated_pixel", 1, 1001.0),
        ("isolated_d2_sufficient", "isolated_pixel", 2, 1001.0),
        ("zero_control", "isolated_pixel", 1, 0.0),
    ]
    result: list[dict[str, object]] = []
    for variant_id, topology, distance, target_flin in raw:
        adj, realized = tune_adjustment_real32(
            target_flin,
            real32_state,
            require_exact=target_flin == 1000.0,
        )
        designed_flin = realized["source_flin_kw_m"]
        expected_ignite = bool(designed_flin > 1000.0 and distance <= 1)
        result.append(
            {
                "id": variant_id,
                "topology": topology,
                "distance_cells": distance,
                "target_row_col": [SOURCE_ROW, SOURCE_COL + distance],
                "requested_source_flin_kw_m": target_flin,
                "adj_float32": adj,
                "designed_source_flin_kw_m": designed_flin,
                "binary32_input_design": realized,
                "equality_realization": "exact binary32 realization" if target_flin == 1000.0 else "not applicable",
                "intended_threshold_result": "ignite" if expected_ignite else "do_not_ignite",
                "intended_criterion": "vegetative source, urban target, Euclidean distance <= 1 cell, FLIN_SURFACE > 1000 kW/m",
                "expected_target_ignited": expected_ignite,
                "expected_target_toa_s": 0.0 if expected_ignite else None,
                "expected_target_terminal_phi": "<= 0" if expected_ignite else "> 0",
                "expected_source_toa_s": 0.0,
            }
        )
    return result


def source_selector_preflight() -> dict[str, object]:
    return {
        "case_id": CASE_ID,
        "artifact_kind": "version-pinned source-selector preflight",
        "source_commit": SOURCE_COMMIT,
        "runtime_source_inspection": False,
        "source_review": SOURCE_REVIEW,
        "technical_references": TECHNICAL_REFERENCES,
        "configured_selector": {
            "namelist_group": "WUI",
            "name": "INTERFACE_MODEL_TYPE",
            "value": 2,
            "reviewed_location": "build/source/elmfire_namelists.f90:676-710",
        },
        "active_implementation_selector": {
            "name": "CRITICAL_HF_WUI",
            "type": "INTEGER",
            "condition": "CRITICAL_HF_WUI == 2",
            "required_value": 2,
            "deterministically_assigned_value": None,
            "reviewed_locations": [
                "build/source/elmfire_vars.f90:112",
                "build/source/elmfire_spread_rate.f90:665-678",
                "build/source/elmfire_level_set.f90:2414-2420",
            ],
            "assignment_in_reviewed_source": "none found",
        },
        "similarly_named_namelist_value": {
            "name": "CRITICL_HF_WUI",
            "type": "REAL",
            "configured_value": 0.0,
            "note": "This misspelled namelist variable is distinct from CRITICAL_HF_WUI and does not select the transition branch.",
        },
        "selector_gate_passed": False,
        "status": "NOT EVALUABLE",
        "reason": "The reviewed executable interface cannot deterministically assign the integer selector tested by the transition branch.",
        "remediation_gate": "Expose and assign the active selector through a reviewed namelist/API, then rerun all threshold variants before any PASS/FAIL transition verdict.",
        "pass_contract": "status PASS, selector_gate_passed true, CRITICAL_HF_WUI required and deterministically assigned value 2, and a nonempty reviewed assignment location",
    }


def initial_metrics(
    count: int, aggregate: str | None, oracle_sha256: str | None
) -> dict[str, object]:
    names = [
        ("evidence and exact dump ledger", "61 rows: indices 1..61, times 0..60 s, only row 61 final", "--"),
        ("source fireline-intensity setup", "maximum absolute error <=0.5 kW/m and source TOA=0 s", "kW/m"),
        ("terminal PHI/TOA state consistency", "PHI<=0 iff TOA is finite at each designated target", "--"),
        ("intended W-to-U receiver heat exposure", "positive DFC+radiation at sufficient-heat receivers", "kW/m2"),
        ("intended W-to-U finite ignition delay", "finite target TOA with delay > 0 s", "s"),
        ("contiguous strict-threshold matrix", "below=false, equal=false, above=true", "--"),
        ("isolated distance/threshold matrix", "d1-insufficient=false, d1-sufficient=true, d2-sufficient=false", "--"),
        ("first-update transition timing", "eligible target TOA=0 s and target-source delay=0 s", "s"),
        ("zero-source control", "target remains PHI>0 with nodata TOA", "--"),
    ]
    return {
        "schema_version": 2,
        "case_id": CASE_ID,
        "source_commit": SOURCE_COMMIT,
        "overall_status": "NOT EVALUABLE",
        "workflow_status": "NOT RUN",
        "verification_passed": False,
        "required_outputs_complete": False,
        "required_variant_count": count,
        "completed_variant_count": 0,
        "runtime_input_fingerprint_sha256": aggregate,
        "oracle_artifact_fingerprint_sha256": oracle_sha256,
        "preflight": "outputs/source_selector_preflight.json",
        "reason": "No current successful-run receipts are present; only optional shortcut attribution depends on the selector preflight.",
        "metrics": [
            {"name": name, "expected": expected, "calculated": None, "units": units, "status": "NOT EVALUABLE"}
            for name, expected, units in names
        ],
    }


def make_figure(specs: list[dict[str, object]]) -> None:
    chosen = ["contiguous_equal", "isolated_d1_sufficient", "isolated_d2_sufficient"]
    fig, axes = plt.subplots(2, 2, figsize=(7.2, 7.5), constrained_layout=True)
    axes = axes.ravel()
    cmap = ListedColormap(["#8c8c8c", "#4daf4a", "#e41a1c"])
    norm = BoundaryNorm([0, 1, 2, 3], cmap.N)
    for ax, variant_id in zip(axes[:3], chosen):
        path = CASE_DIR / "variants" / variant_id / "inputs/fbfm40.tif"
        with rasterio.open(path) as src:
            fuel = src.read(1)
            extent = (src.bounds.left, src.bounds.right, src.bounds.bottom, src.bounds.top)
        with rasterio.open(CASE_DIR / "variants" / variant_id / "inputs/source_mask.tif") as src:
            ignition = src.read(1)
        display = np.where(fuel == 1, 1, np.where(fuel == 91, 2, 0))
        ax.imshow(display, origin="upper", extent=extent, cmap=cmap, norm=norm)
        ax.contour(ignition, levels=[0.5], colors="black", linewidths=1.0, origin="upper", extent=extent)
        ax.set_title(variant_id.replace("_", "\n"), fontsize=12)
        ax.set_xlabel("Easting (m)")
        ax.set_ylabel("Northing (m)")
    x = np.arange(len(specs))
    axes[3].bar(x, [float(s["designed_source_flin_kw_m"]) for s in specs], color="#4c78a8")
    axes[3].axhline(1000.0, color="black", linestyle="--")
    axes[3].set_xticks(
        x,
        ["C <", "C =", "C >", "I1 <", "I1 >", "I2 >", "zero"],
        rotation=45,
        fontsize=12,
    )
    axes[3].set_ylabel("Designed FLIN\n(kW m$^{-1}$)")
    axes[3].set_title("Input design; not output")
    fig.suptitle("Prepared fuels and ignition\nGreen: wildland; red: urban; gray: nonburnable")
    out = CASE_DIR / "figures/input_configuration.pdf"
    out.parent.mkdir(parents=True, exist_ok=True)
    polish_figure(fig)
    fig.savefig(out, metadata={"CreationDate": None, "ModDate": None})
    plt.close(fig)


def main() -> None:
    output_dir = CASE_DIR / "outputs"
    output_dir.mkdir(parents=True, exist_ok=True)
    write_json_atomic(output_dir / "source_selector_preflight.json", source_selector_preflight())
    write_json_atomic(output_dir / "metrics.json", initial_metrics(7, None, None))
    write_json_atomic(
        output_dir / "run_attempts.json",
        {
            "schema_version": 2,
            "case_id": CASE_ID,
            "source_commit_oracle": SOURCE_COMMIT,
            "run_case_invoked": False,
            "runner_started_utc": None,
            "requested_executable": None,
            "attempts": [],
        },
    )
    for stale in (
        CASE_DIR / "figures/input_configuration.pdf",
        CASE_DIR / "figures/transition_observations.pdf",
        CASE_DIR / "report/case_report.pdf",
        CASE_DIR / "report/metrics_macros.tex",
    ):
        stale.unlink(missing_ok=True)
    # Reference hashes record the design review; all runtime equations and
    # inputs are case-local, so source publications are not runtime inputs.

    variants_dir = CASE_DIR / "variants"
    if variants_dir.exists():
        shutil.rmtree(variants_dir)
    variants_dir.mkdir(parents=True)
    template = (CASE_DIR / "elmfire.data.in").read_text(encoding="utf-8")
    fuel_table = (CASE_DIR / "data/misc/fuel_models.csv").read_text(encoding="utf-8")
    building_table = (
        CASE_DIR / "data/misc/building_fuel_models.csv"
    ).read_text(encoding="utf-8")
    reference = fuel1_reference()
    real32_state = fuel1_real32_state()
    real32_reference = {
        **{name: float(value) for name, value in real32_state.items()},
        **source_flin_real32(1.0, real32_state),
    }
    specs = variant_specs(real32_state)
    fingerprints: dict[str, dict[str, object]] = {}
    source_mask = np.zeros((SIZE, SIZE), dtype=np.int16)
    source_mask[9:16, SOURCE_COL] = 1
    for spec in specs:
        variant_id = str(spec["id"])
        root = variants_dir / variant_id
        inputs, misc = root / "inputs", root / "misc"
        for directory in (inputs, misc, root / "outputs", root / "scratch"):
            directory.mkdir(parents=True, exist_ok=True)
        fbfm = np.full((SIZE, SIZE), 93, dtype=np.int16)
        fbfm[8:17, 8:12] = 1
        target_row, target_col = (int(v) for v in spec["target_row_col"])
        if spec["topology"] == "contiguous_interface":
            fbfm[8:17, 12:16] = 91
        else:
            fbfm[target_row, target_col] = 91
        phi = np.ones((SIZE, SIZE), dtype=np.float32)
        phi[8:17, SOURCE_COL - 1 : SOURCE_COL + 2] = 0.55
        phi[source_mask == 1] = -0.45
        adj = np.zeros((SIZE, SIZE), dtype=np.float32)
        adj[fbfm == 1] = np.float32(spec["adj_float32"])
        fields: dict[str, tuple[np.ndarray | float, str]] = {
            "fbfm40": (fbfm, "int16"),
            "source_mask": (source_mask, "int16"),
            "phi": (phi, "float32"),
            "adj": (adj, "float32"),
            "slp": (0.0, "float32"),
            "asp": (0.0, "float32"),
            "dem": (0.0, "float32"),
            "cc": (0.0, "float32"),
            "ch": (0.0, "float32"),
            "cbh": (0.0, "float32"),
            "cbd": (0.0, "float32"),
            "ws": (0.0, "float32"),
            "wd": (270.0, "float32"),
            "m1": (0.05, "float32"),
            "m10": (0.07, "float32"),
            "m100": (0.09, "float32"),
        }
        for name, (values, dtype) in fields.items():
            write_raster(inputs / f"{name}.tif", values, dtype)
        (misc / "fuel_models.csv").write_text(fuel_table, encoding="utf-8")
        (misc / "building_fuel_models.csv").write_text(building_table, encoding="utf-8")
        write_text_atomic(root / "elmfire.data", concrete_namelist(template, variant_id))
        fingerprint = variant_fingerprint(root)
        spec["runtime_input_fingerprint_sha256"] = fingerprint["sha256"]
        fingerprints[variant_id] = fingerprint
        write_json_atomic(root / "variant.json", spec)
    aggregate = aggregate_fingerprint(fingerprints)
    manifest = {
        "schema_version": 2,
        "case_id": CASE_ID,
        "source_commit": SOURCE_COMMIT,
        "source_review": SOURCE_REVIEW,
        "technical_references": TECHNICAL_REFERENCES,
        "source_checkout_required_at_runtime": False,
        "runtime_input_fingerprint_sha256": aggregate,
        "grid": {
            "shape": [SIZE, SIZE],
            "cell_size_m": DX_M,
            "crs": CRS,
            "origin_upper_left_m": [-125.0, 125.0],
            "row_direction": "north to south",
            "nodata": NODATA,
        },
        "wildland_reference": reference,
        "wildland_reference_binary32": real32_reference,
        "input_design_arithmetic": "explicit IEEE-754 binary32 transcription of the reviewed default-REAL operation sequence",
        "intended_threshold": {"fireline_intensity_kw_m": 1000.0, "comparison": ">", "distance_cells": 1.0, "distance_comparison": "<="},
        "time_and_dump_oracle": {
            "simulation_interval_s": [0.0, 60.0],
            "fixed_dt_s": 1.0,
            "dump_every_step": True,
            "rows": 61,
            "dump_indices": [1, 61],
            "times_s": [0.0, 60.0],
            "terminal_final_time_stamp": "0000060",
            "terminal_dump_index_stamp": "d0000061",
            "terminal_flin_pattern": "flin_*_0000060.tif",
            "terminal_toa_pattern": "time_of_arrival_*_0000060.tif",
            "terminal_phi_pattern": "phi_*_d0000061.tif",
            "eligible_transition_target_toa_s": 0.0,
            "initial_source_toa_s": 0.0,
        },
        "stall_prevention": {
            "num_ignitions": 1,
            "dormant_ignition_time_s": 61.0,
            "rationale": "a post-stop ignition keeps ALREADY_IGNITED false and prevents the solver's early-stall shortcut without affecting the 0..60 s solution",
        },
        "variants": specs,
    }
    write_json_atomic(variants_dir / "expected.json", manifest)
    write_text_atomic(
        variants_dir / "variant_ids.txt",
        "\n".join(str(spec["id"]) for spec in specs) + "\n",
    )
    oracle_fingerprint = oracle_artifact_fingerprint(CASE_DIR)
    fingerprint_document = {
        "schema_version": 2,
        "case_id": CASE_ID,
        "source_commit": SOURCE_COMMIT,
        "aggregate_sha256": aggregate,
        "oracle_artifacts": oracle_fingerprint,
        "source_review": SOURCE_REVIEW,
        "variants": fingerprints,
    }
    write_json_atomic(variants_dir / "run_fingerprints.json", fingerprint_document)
    write_json_atomic(
        output_dir / "metrics.json",
        initial_metrics(len(specs), aggregate, str(oracle_fingerprint["sha256"])),
    )
    make_figure(specs)
    print(f"[OK] {CASE_ID}: generated {len(specs)} variants and pinned selector preflight")


if __name__ == "__main__":
    main()
