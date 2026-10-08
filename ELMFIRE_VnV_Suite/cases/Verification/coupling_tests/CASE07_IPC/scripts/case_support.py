#!/usr/bin/env python3
"""Prepare inputs or analyze outputs for one independent spotting case.

The case-local entry point at the bottom selects preprocessing or postprocessing.
Preprocessing reads case.json and writes deterministic GeoTIFF inputs in SI units.
Postprocessing reads ELMFIRE GeoTIFF outputs, computes reference comparisons,
and writes metrics.json, LaTeX macros, and vector PDF figures. It never runs ELMFIRE.
"""

from report_language import polish_figure, report_text
from pathlib import Path
import json, re

import matplotlib
matplotlib.use("Agg")  # Headless backend for batch verification and CI.
import matplotlib.pyplot as plt
import numpy as np
import rasterio
from rasterio.transform import from_origin
# A single explicit sentinel lets every generated raster use the same mask.
NODATA = -9999.0
BUFFER_CELLS = 2  # ELMFIRE numerical halo on each raster boundary.

def load_case(case_dir):
    """Load the human-auditable case contract from case.json."""
    return json.loads((Path(case_dir) / "case.json").read_text(encoding="utf-8"))

def write_tif(path, arr, dx, dtype=np.float32):
    """Write one north-up raster; dx is the square cell size in metres."""
    path = Path(path)
    # The upper-left origin includes the numerical halo; usable cells begin at
    # BUFFER_CELLS in both array dimensions. Rows increase southward.
    transform = from_origin(
        -BUFFER_CELLS * dx,
        (arr.shape[0] - BUFFER_CELLS) * dx,
        dx,
        dx,
    )
    with rasterio.open(
        path,
        "w",
        driver="GTiff",
        height=arr.shape[0],
        width=arr.shape[1],
        count=1,
        dtype=np.dtype(dtype).name,
        crs="EPSG:32610",
        transform=transform,
        nodata=NODATA,
    ) as dataset:
        dataset.write(np.asarray(arr, dtype=dtype), 1)

def preprocess(case_dir):
    """Generate deterministic, co-registered inputs without running ELMFIRE."""
    case_dir = Path(case_dir); c = load_case(case_dir)
    inp = case_dir / "data" / "inputs"; misc = case_dir / "data" / "misc"
    inp.mkdir(parents=True, exist_ok=True); misc.mkdir(parents=True, exist_ok=True)
    nx, ny, dx = int(c["nx"]), int(c["ny"]), float(c["dx"])
    zeros = np.zeros((ny, nx), dtype=np.float32); ones = np.ones((ny, nx), dtype=np.float32)
    fbfm = np.full((ny, nx), 102, dtype=np.int16); phi = np.ones((ny, nx), dtype=np.float32)
    # Keep the source on the physical centerline and outside the two-cell halo.
    row = min(max(ny // 2, BUFFER_CELLS), ny - BUFFER_CELLS - 1)
    if c["wui"]:
        fbfm[:] = 93
        for j in range(nx):
            fbfm[row, j] = 91 if ((j * dx) % 20.0) < 10.0 else 102
    phi[row, BUFFER_CELLS] = -1.0  # First usable cell, never a boundary cell.
    rasters = [("asp", zeros, np.float32), ("cbd", zeros, np.float32), ("cbh", zeros, np.float32), ("cc", zeros, np.float32), ("ch", zeros, np.float32), ("dem", zeros, np.float32), ("slp", zeros, np.float32), ("adj", ones, np.float32), ("new_phi", phi, np.float32), ("new_fbfm40", fbfm, np.int16), ("ws", np.full((ny,nx), 40.0 if c["wui"] else 15.0, dtype=np.float32), np.float32), ("wd", np.full((ny,nx), 270.0, dtype=np.float32), np.float32), ("m1", zeros, np.float32), ("m10", zeros, np.float32), ("m100", zeros, np.float32)]
    for name, arr, dtype in rasters:
        write_tif(inp / (name + ".tif"), arr, dx, dtype)
    if c["wui"]:
        isb = fbfm == 91
        write_tif(inp / "bldg_area.tif", np.where(isb, 10.0, NODATA).astype(np.float32), dx)
        write_tif(inp / "bldg_sep.tif", np.where(isb, 10.0, NODATA).astype(np.float32), dx)
        write_tif(inp / "bldg_nonburnable.tif", np.where(isb, 0.0, NODATA).astype(np.float32), dx)
        write_tif(inp / "bldg_footprint_frac.tif", np.where(isb, 1.0, NODATA).astype(np.float32), dx)
        write_tif(inp / "bldg_fuel_model.tif", np.where(isb, 14, -9999).astype(np.int16), dx, np.int16)
    for fn in ["fuel_models.csv", "building_fuel_models.csv"]:
        src = case_dir / "data" / "misc" / fn
        if not src.is_file():
            raise FileNotFoundError(f"Required case-local table is missing: {src}")
    for d in ["outputs", "figures", "logs/scratch"]:
        (case_dir / d).mkdir(parents=True, exist_ok=True)
    print("[OK] wrote inputs for " + c["id"])

def read_raster(path):
    """Return the first raster band as floating-point values."""
    try:
        with rasterio.open(path) as dataset:
            return dataset.read(1).astype(float)
    except rasterio.errors.RasterioIOError:
        return None

def pdf_line(path, xs, ys, label):
    """Write a compact vector-PDF profile with labelled SI-unit axes."""
    xs = np.asarray(xs, dtype=float)
    ys = np.asarray(ys, dtype=float)
    mask = np.isfinite(xs) & np.isfinite(ys)
    fig, ax = plt.subplots(figsize=(7.2, 3.6), constrained_layout=True)
    ax.plot(xs[mask], ys[mask], color="#1f77b4", linewidth=2, label=label)
    ax.set_xlabel("Downwind distance (m)")
    ax.set_ylabel(label)
    ax.grid(True, alpha=0.25)
    ax.legend()
    polish_figure(fig)
    fig.savefig(path, format="pdf")
    plt.close(fig)

def reference(c, x, ignition_x):
    """Construct the SI reference on georeferenced, non-buffer cell centres."""
    dx=float(c["dx"]); uw=17.88 if c["wui"] else 6.71
    travel_x=np.maximum(x-ignition_x,0.0)
    if c["id"] == "CASE07_IPC":
        return x, np.where(x>150,149.54,149.54*np.clip(x/150,0,1)), "expected accumulated firebrands"
    if "5_3" in c["id"] or c["consumption"]:
        return x, travel_x/(0.34 if c["wui"] else 0.71), "expected surface-fire TOA"
    if c["wui"]:
        return x, np.where(travel_x<80,travel_x/uw+10,np.nan), "expected first-firebrand TOA"
    return x, travel_x/uw, "expected wind TOA"

def postprocess(case_dir):
    """Compare available ELMFIRE output with the reference and emit artifacts."""
    case_dir=Path(case_dir); c=load_case(case_dir); out=case_dir/"outputs"; fig=case_dir/"figures"; rep=case_dir/"report"
    out.mkdir(exist_ok=True); fig.mkdir(exist_ok=True); rep.mkdir(exist_ok=True)
    with rasterio.open(case_dir / "data/inputs/new_phi.tif") as input_dataset:
        gt = input_dataset.transform.to_gdal()
        nx = input_dataset.width
    x_all=gt[0]+(np.arange(nx)+0.5)*gt[1]
    x=x_all[BUFFER_CELLS:nx-BUFFER_CELLS]
    ignition_x=x_all[BUFFER_CELLS]
    x,y,label=reference(c,x,ignition_x); pdf_line(fig/"reference_profile.pdf", x, y, label)
    metrics={"case_id":c["id"],"source_references":c.get("source_references", []),"status":"not_run","notes":"No comparable ELMFIRE GeoTIFF output found; reference-only figure generated."}
    files=sorted(out.glob("time_of_arrival*.tif"))
    if files:
        arr=read_raster(files[-1]); row=arr.shape[0]//2
        sim=arr[row,BUFFER_CELLS:arr.shape[1]-BUFFER_CELLS]
        sim=sim[:min(len(sim),len(y))]; ref=y[:len(sim)]
        m=np.isfinite(sim)&np.isfinite(ref)&(sim>-1000)&(ref>0)
        if m.any():
            rel=np.abs(sim[m]-ref[m])/np.maximum(np.abs(ref[m]),1e-9)
            metrics.update(status="computed", toa_mean_relative_error=float(rel.mean()), toa_max_relative_error=float(rel.max()), num_toa_cells_compared=int(m.sum()))
            pdf_line(fig/"toa_comparison.pdf", x[:len(sim)], sim, "simulated TOA (s)")
    (out/"metrics.json").write_text(json.dumps(metrics,indent=2),encoding="utf-8")
    lines=[]
    for k,v in metrics.items():
        safe=re.sub(r'[^A-Za-z0-9]+','',k)
        latex_value = str(v).replace("_", "\\_")
        lines.append(f"\\expandafter\\def\\csname metric@{safe}\\endcsname{{{latex_value}}}")
    (rep/"metrics_macros.tex").write_text(report_text("\n".join(lines)+"\n"),encoding="utf-8")
    print("[OK] postprocessed " + c["id"])
