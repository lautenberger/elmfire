#!/usr/bin/env python3
"""Create report-ready input evidence from generated CASE47 rasters."""
from __future__ import annotations

from report_language import polish_figure

from datetime import datetime, timezone
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

CASE_DIR = Path(__file__).resolve().parents[1]
HEAT_SOURCE = (8, 10)
ISOLATED = (8, 13)
PATH_SOURCE = (22, 10)
NEAR = (22, 11)
BARRIER = (22, 12)
DISTAL = (22, 13)


def read(variant: str, field: str) -> tuple[np.ndarray, rasterio.Affine]:
    path = CASE_DIR / "variants" / variant / "inputs" / f"{field}.tif"
    with rasterio.open(path) as source:
        return source.read(1), source.transform


def extent(transform: rasterio.Affine, array: np.ndarray) -> tuple[float, float, float, float]:
    left = transform.c
    top = transform.f
    right = left + transform.a * array.shape[1]
    bottom = top + transform.e * array.shape[0]
    return left, right, bottom, top


def xy(cell: tuple[int, int]) -> tuple[float, float]:
    row, col = cell
    return 10.0 * (col + 0.5), 310.0 - 10.0 * (row + 0.5)


def annotate(ax: plt.Axes, show_labels: bool) -> None:
    labels = (
        (HEAT_SOURCE, "U heat source", (-54, 12)),
        (ISOLATED, "W isolated", (10, 12)),
        (PATH_SOURCE, "U path source", (-55, -18)),
        (NEAR, "W near", (-12, 13)),
        (DISTAL, "W distal", (10, -18)),
    )
    for cell, label, offset in labels:
        px, py = xy(cell)
        source_cell = cell in {HEAT_SOURCE, PATH_SOURCE}
        ax.plot(px, py, marker="o", ms=5, mec="black", mfc="#ffeda0" if source_cell else "white")
        if show_labels:
            ax.annotate(
                label,
                (px, py),
                xytext=offset,
                textcoords="offset points",
                fontsize=12,
                arrowprops={"arrowstyle": "-", "lw": 0.5, "color": "black"},
                bbox={"boxstyle": "round,pad=0.12", "fc": "white", "ec": "none", "alpha": 0.8},
            )


def main() -> None:
    open_id = "hrr400_adj1_open"
    barrier_id = "hrr400_adj1_barrier"
    open_fuel, transform = read(open_id, "fbfm40")
    barrier_fuel, _ = read(barrier_id, "fbfm40")
    adj0, _ = read("hrr400_adj0_open", "adj")
    adj1, _ = read(open_id, "adj")
    phi, _ = read(open_id, "phi")
    image_extent = extent(transform, open_fuel)

    cmap = ListedColormap(["#dddddd", "#4daf4a", "#d73027"])
    norm = BoundaryNorm([-0.5, 0.5, 1.5, 2.5], cmap.N)
    fig, axes = plt.subplots(2, 2, figsize=(7.2, 7.6), constrained_layout=True)
    panels = (
        (axes[0, 0], np.where(open_fuel == 1, 1, np.where(open_fuel == 91, 2, 0)), "Open corridor", cmap, norm),
        (axes[0, 1], np.where(barrier_fuel == 1, 1, np.where(barrier_fuel == 91, 2, 0)), "Barrier corridor", cmap, norm),
        (axes[1, 0], adj0, "Wildland ADJ = 0", "viridis", None),
        (axes[1, 1], adj1, "Wildland ADJ = 1", "viridis", None),
    )
    for index, (ax, array, title, color_map, color_norm) in enumerate(panels):
        shown = ax.imshow(array, origin="upper", extent=image_extent, interpolation="nearest", cmap=color_map, norm=color_norm, vmin=None if color_norm else 0, vmax=None if color_norm else 1)
        ax.contour(
            np.linspace(5, 305, phi.shape[1]),
            np.linspace(305, 5, phi.shape[0]),
            phi,
            levels=[0.0],
            colors="black",
            linewidths=1.0,
        )
        annotate(ax, show_labels=False)
        ax.set_title(title)
        ax.set_xlabel("Easting (m)")
        ax.set_ylabel("Northing (m)")
        ax.set_aspect("equal")
        if color_norm is None:
            fig.colorbar(shown, ax=ax, orientation="horizontal", shrink=0.8, pad=0.03, ticks=[0, 1], label="ADJ multiplier")
    axes[0, 1].plot(*xy(BARRIER), marker="s", ms=8, mfc="none", mec="black", mew=1.5)
    fig.suptitle(
        "CASE47 prepared inputs: 10 m grid\n"
        "Gray: nonburnable; green: wildland; red: urban",
        fontsize=12,
    )
    output = CASE_DIR / "figures/input_configuration.pdf"
    output.parent.mkdir(exist_ok=True)
    polish_figure(fig)
    fig.savefig(
        output,
        metadata={
            "Title": "CASE47_UWT prepared input configuration",
            "Author": "ELMFIRE Verification and Validation Suite",
            "CreationDate": datetime(2020, 1, 1, tzinfo=timezone.utc),
            "ModDate": datetime(2020, 1, 1, tzinfo=timezone.utc),
        },
    )
    plt.close(fig)
    print(f"[OK] wrote {output}")


if __name__ == "__main__":
    main()
