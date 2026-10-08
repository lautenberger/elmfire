"""Create auditable whole-domain configuration and result figures.

The parent postprocessor calls generate_spatial_evidence after it has calculated
the case metrics. This module reads prepared GeoTIFF inputs and actual ELMFIRE
outputs; it never synthesizes a replacement result.
"""

from report_language import polish_figure

from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap
from mpl_toolkits.axes_grid1 import make_axes_locatable
import numpy as np
import rasterio


BUFFER_CELLS = 2
FIGURE_DPI = 180


def _read_raster(path):
    """Return the first raster band, nodata value, and GDAL geotransform."""
    with rasterio.open(path) as dataset:
        return (
            dataset.read(1).astype(float),
            dataset.nodata,
            dataset.transform.to_gdal(),
        )


def _physical_view(array, geotransform, buffer_cells=BUFFER_CELLS):
    """Remove the numerical halo and return data plus map-coordinate extent."""
    ny, nx = array.shape
    buffer = buffer_cells if min(nx, ny) > 2 * buffer_cells else 0
    view = array[buffer:ny - buffer or None, buffer:nx - buffer or None]
    x_left = geotransform[0] + buffer * geotransform[1]
    x_right = geotransform[0] + (nx - buffer) * geotransform[1]
    y_top = geotransform[3] + buffer * geotransform[5]
    y_bottom = geotransform[3] + (ny - buffer) * geotransform[5]
    extent = (
        min(x_left, x_right), max(x_left, x_right),
        min(y_bottom, y_top), max(y_bottom, y_top),
    )
    return view, extent


def _valid(array, nodata):
    """Mask nodata and non-finite values without changing valid zeros."""
    mask = ~np.isfinite(array)
    if nodata is not None:
        mask |= np.isclose(array, nodata)
    return np.ma.array(array, mask=mask)


def _map_aspect(extent):
    """Preserve map scale unless a one-cell strip would become illegible."""
    width = max(extent[1] - extent[0], np.finfo(float).eps)
    height = max(extent[3] - extent[2], np.finfo(float).eps)
    ratio = width / height
    return "auto" if ratio > 10.0 or ratio < 0.1 else "equal"


def _compact_figure_height(extent, panel_width, extra_height, limits):
    """Fit the canvas to the map while retaining room for labels and titles."""
    width = max(extent[1] - extent[0], np.finfo(float).eps)
    height = max(extent[3] - extent[2], np.finfo(float).eps)
    required = panel_width * height / width + extra_height
    return float(np.clip(required, limits[0], limits[1]))


def add_aligned_colorbar(figure, axis, mappable, orientation="auto", **kwargs):
    """Attach a readable colorbar matched to the rendered map dimensions."""
    if orientation == "auto":
        orientation = "horizontal" if axis.get_data_ratio() < 0.42 else "vertical"
    if orientation == "horizontal":
        fraction, pad = 0.08, 0.24
    else:
        fraction, pad = 0.046, 0.04
    colorbar = figure.colorbar(
        mappable, ax=axis, orientation=orientation,
        fraction=fraction, pad=pad, **kwargs)
    colorbar.ax.tick_params(labelsize=8)
    colorbar.ax.xaxis.label.set_size(9)
    colorbar.ax.yaxis.label.set_size(9)
    return colorbar


def _find_output(case_dir, field, preferred_variant):
    """Choose the final real ELMFIRE raster for the representative variant."""
    candidates = []
    for path in case_dir.rglob(f"{field}_*.tif"):
        if path.parent.name != "outputs":
            continue
        if field == "ember_flux" and "transient" in path.name:
            continue
        candidates.append(path)
    if preferred_variant:
        preferred = [p for p in candidates if preferred_variant in p.parts]
        if preferred:
            candidates = preferred
    return sorted(candidates)[-1] if candidates else None


def _find_input(input_dir, names):
    """Return the first available prepared input from a relevance-ordered list."""
    for name in names:
        path = input_dir / name
        if path.exists():
            return path
    return None


def _plot_configuration(case_dir, run_root, preferred_variant):
    """Plot whole-domain fuel/structure layout and initial ignition geometry."""
    input_dir = run_root / "data" / "inputs"
    has_plot_inputs = (
        (input_dir / "new_phi.tif").is_file()
        and any(
            (input_dir / name).is_file()
            for name in ("structure_fraction.tif", "structure_id.tif", "new_fbfm40.tif")
        )
    )
    if not has_plot_inputs:
        candidates = sorted(case_dir.rglob("data/inputs/new_phi.tif"))
        if preferred_variant:
            preferred = [p for p in candidates if preferred_variant in p.parts]
            candidates = preferred or candidates
        if not candidates:
            return None
        input_dir = candidates[0].parent

    layout_path = _find_input(
        input_dir,
        ("structure_fraction.tif", "structure_id.tif", "new_fbfm40.tif"),
    )
    ignition_path = _find_input(input_dir, ("new_phi.tif",))
    if layout_path is None or ignition_path is None:
        return None

    layout, layout_nodata, transform = _read_raster(layout_path)
    ignition, ignition_nodata, ignition_transform = _read_raster(ignition_path)
    layout, extent = _physical_view(layout, transform)
    ignition, ignition_extent = _physical_view(ignition, ignition_transform)
    layout = _valid(layout, layout_nodata)
    ignition = _valid(ignition, ignition_nodata)
    ignition_mask = np.ma.array(
        np.where(ignition <= 0.0, 1.0, 0.0), mask=np.ma.getmaskarray(ignition)
    )

    figure_height = _compact_figure_height(
        extent, panel_width=4.35, extra_height=1.70, limits=(2.55, 5.20))
    figure, axes = plt.subplots(
        1, 2, figsize=(10.2, figure_height), constrained_layout=True)
    figure.set_constrained_layout_pads(
        w_pad=0.06, h_pad=0.04, wspace=0.12, hspace=0.04)
    layout_image = axes[0].imshow(
        layout, origin="upper", extent=extent, interpolation="nearest",
        cmap="viridis", aspect=_map_aspect(extent),
    )
    layout_label = (
        "Structure fraction (-)"
        if layout_path.name == "structure_fraction.tif"
        else "Structure identifier (-)"
        if layout_path.name == "structure_id.tif"
        else "Fuel model code (-)"
    )
    add_aligned_colorbar(figure, axes[0], layout_image, label=layout_label)
    axes[0].set_title(f"Prescribed {layout_label}")

    ignition_cmap = ListedColormap(["#f7f7f7", "#d73027"])
    ignition_image = axes[1].imshow(
        ignition_mask, origin="upper", extent=ignition_extent,
        interpolation="nearest", cmap=ignition_cmap, vmin=0.0, vmax=1.0,
        aspect=_map_aspect(ignition_extent),
    )
    add_aligned_colorbar(
        figure, axes[1], ignition_image, ticks=[0, 1],
        label="Initially ignited cell (-)",
    )
    ignition_cells = int(np.ma.sum(ignition_mask))
    cell_word = "cell" if ignition_cells == 1 else "cells"
    axes[1].set_title(
        f"Initial ignition/source geometry ({ignition_cells} {cell_word})"
    )
    for axis in axes:
        axis.set_xlabel("Easting (m)")
        axis.set_ylabel("Northing (m)")

    weather = []
    for name, label, unit in (("ws.tif", "Wind speed", "mph"), ("wd.tif", "Wind direction", "degrees")):
        weather_path = input_dir / name
        if weather_path.exists():
            values, nodata, weather_transform = _read_raster(weather_path)
            values, _ = _physical_view(values, weather_transform)
            values = _valid(values, nodata)
            if values.count():
                weather.append(f"{label}={float(np.ma.median(values)):.3g} {unit}")
    variant_label = str(run_root.relative_to(case_dir)) if run_root != case_dir else "base case"
    suffix = f"; {', '.join(weather)}" if weather else ""
    figure.suptitle(f"{case_dir.name}: whole-domain inputs\nGrid spacing {abs(transform[1]):g} by {abs(transform[5]):g} m{suffix}")

    output = case_dir / "figures" / "input_configuration.pdf"
    output.parent.mkdir(parents=True, exist_ok=True)
    polish_figure(figure)
    figure.savefig(
        output, format="pdf", dpi=FIGURE_DPI,
        bbox_inches="tight", pad_inches=0.06,
    )
    plt.close(figure)
    return output


def _plot_domain_result(case_dir, raster_path, field, strip_width_m):
    """Plot one representative whole-domain field from actual model output."""
    array, nodata, transform = _read_raster(raster_path)
    array, extent = _physical_view(array, transform)
    values = _valid(array, nodata)
    dx = abs(transform[1])
    dy = abs(transform[5])

    if field == "time_of_arrival":
        values = np.ma.masked_where(values <= 0.0, values)
        label, title, cmap = "Time of arrival (s)", "Time-of-arrival field", "turbo"
    elif field == "ember_flux":
        area = dx * (strip_width_m if strip_width_m is not None else dy)
        values = np.ma.masked_where(values < 0.0, values) / area
        label = r"Accumulated firebrand density (pcs m$^{-2}$)"
        title, cmap = "Accumulated firebrand distribution", "magma"
    elif field == "ember_ignition":
        values = np.ma.masked_where(values < 0.0, values)
        label, title, cmap = "Ember-ignition state (-)", "Ember-ignition field", "magma"
    else:
        label, title, cmap = r"Level-set field $\phi$ (m)", "Transported level-set field", "RdBu_r"

    figure_height = _compact_figure_height(
        extent, panel_width=6.7, extra_height=1.85, limits=(2.75, 6.20))
    figure, axis = plt.subplots(
        figsize=(8.2, figure_height), constrained_layout=True)
    figure.set_constrained_layout_pads(
        w_pad=0.06, h_pad=0.04, wspace=0.04, hspace=0.04)
    image = axis.imshow(
        values, origin="upper", extent=extent, interpolation="nearest",
        cmap=cmap, aspect=_map_aspect(extent),
    )
    if field == "phi" and values.count() and np.ma.min(values) <= 0 <= np.ma.max(values):
        axis.contour(
            np.asarray(values.filled(np.nan)), levels=[0.0], colors="black",
            linewidths=1.0, origin="upper", extent=extent,
        )
    add_aligned_colorbar(figure, axis, image, label=label)
    axis.set_xlabel("Easting (m)")
    axis.set_ylabel("Northing (m)")
    run_root = raster_path.parent.parent
    variant = "base case" if run_root == case_dir else str(run_root.relative_to(case_dir))
    axis.set_title(f"{title} — {case_dir.name}\nGrid spacing {dx:g} by {dy:g} m")

    output = case_dir / "figures" / "domain_result.pdf"
    output.parent.mkdir(parents=True, exist_ok=True)
    polish_figure(figure)
    figure.savefig(
        output, format="pdf", dpi=FIGURE_DPI,
        bbox_inches="tight", pad_inches=0.06,
    )
    plt.close(figure)
    return output


def generate_spatial_evidence(
    case_dir,
    output_preference=("time_of_arrival", "ember_flux", "ember_ignition", "phi"),
    preferred_variant=None,
    strip_width_m=None,
):
    """Generate required configuration and whole-domain result PDF figures.

    output_preference is ordered by scientific relevance for the case.
    strip_width_m converts accumulated firebrand counts to areal density for
    one-dimensional tests that represent a prescribed cross-stream strip.
    """
    case_dir = Path(case_dir)
    raster_path = None
    field = None
    for candidate_field in output_preference:
        raster_path = _find_output(case_dir, candidate_field, preferred_variant)
        if raster_path is not None:
            field = candidate_field
            break

    run_root = raster_path.parent.parent if raster_path is not None else case_dir
    configuration = _plot_configuration(case_dir, run_root, preferred_variant)
    result = (
        _plot_domain_result(case_dir, raster_path, field, strip_width_m)
        if raster_path is not None else None
    )
    if configuration is None:
        print("[WARN] no prepared spatial configuration was available to plot")
    if result is None:
        print("[WARN] no actual ELMFIRE domain raster was available to plot")
    return {"configuration": configuration, "result": result}
