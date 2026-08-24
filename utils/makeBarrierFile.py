#!/usr/bin/env python3
"""
Standalone barrier-file generator.

Given a single GeoTIFF defining the area of interest (its grid, CRS and extent),
rasterise road and waterway vectors as barrier widths (m) onto a matching grid.

Two data sources are supported (``--source``):

* ``osm``   – fetch roads and waterways on demand from the OpenStreetMap
              Overpass API.  Works anywhere on Earth with no local data; this
              is the default.
* ``local`` – clip from local national datasets (the original behaviour).  Uses
              a primary waterway dataset plus a backup river dataset where the
              primary has no coverage.  Suited to offline / HPC runs.

Unlike pipeline/getBarrierFile.py, this script is not tied to the validation
pipeline's per-case folder layout: point it at any raster and it writes a
barrier.tif aligned to that raster.

Output
------
A float32 GeoTIFF on the template grid; pixel value = effective barrier width
in metres (0 where no barrier feature is present).

Usage
-----
    # global, API-driven (default)
    python makeBarrierFile.py AREA.tif [-o barrier.tif]

    # local national datasets
    python makeBarrierFile.py AREA.tif --source local \
        [--roads roads.gpkg] [--roads-layer lines] \
        [--water water.gpkg] [--water-layer lines] \
        [--backup-water rivers.gpkg]

Data-source and width defaults are read from pipelineConfig.py when it is
importable (i.e. when run from the repo); otherwise pass them explicitly.
"""

from __future__ import annotations

import argparse
import os
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import geopandas as gpd
import numpy as np
import rasterio
from rasterio.features import rasterize
from rasterio.warp import transform_bounds


# ---------------------------------------------------------------------------
# Defaults (overridable on the command line). Pulled from pipelineConfig when
# available so the script "just works" inside the repo.
# ---------------------------------------------------------------------------

def _load_config_defaults() -> dict:
    """Best-effort import of pipelineConfig for sensible defaults."""
    defaults = {
        "roads_gpkg": None,
        "roads_layer": "lines",
        "water_gpkg": None,
        "water_layer": "lines",
        "backup_water_gpkg": None,
        "road_class_field": "highway",
        "water_class_field": "waterway",
        "all_touched": True,
        "dtype": "float32",
        "nodata": -9999.0,
        "backup_width_m": 5.0,
        "backup_match_tol_m": 1.0,
        "road_widths_m": {
            "motorway": 30.0, "trunk": 25.0, "primary": 16.0, "secondary": 12.0,
            "tertiary": 10.0, "residential": 8.0, "service": 6.0, "track": 4.0,
            "path": 2.0,
        },
        "water_widths_m": {
            "river": 30.0, "stream": 6.0, "canal": 10.0, "ditch": 3.0, "drain": 2.0,
        },
        "overpass_url": "https://overpass-api.de/api/interpreter",
        "overpass_timeout": 180,
    }
    # Allow `import pipelineConfig` to resolve relative to the repo root (parent
    # of this script's directory).
    repo_root = Path(__file__).resolve().parent.parent
    if str(repo_root) not in sys.path:
        sys.path.insert(0, str(repo_root))
    try:
        import pipelineConfig as cfg  # type: ignore
    except Exception:
        return defaults

    def g(name, key):
        if hasattr(cfg, name):
            defaults[key] = getattr(cfg, name)

    g("ROADS_GPKG", "roads_gpkg")
    g("ROADS_LAYER", "roads_layer")
    g("WATER_GPKG", "water_gpkg")
    g("WATER_LAYER", "water_layer")
    g("BACKUP_WATER_GPKG", "backup_water_gpkg")
    g("ROAD_CLASS_FIELD", "road_class_field")
    g("WATER_CLASS_FIELD", "water_class_field")
    g("ALL_TOUCHED", "all_touched")
    g("RASTER_DTYPE", "dtype")
    g("RASTER_NODATA", "nodata")
    g("BARRIER_BACKUP_WATER_WIDTH_M", "backup_width_m")
    g("BARRIER_BACKUP_MATCH_TOL_M", "backup_match_tol_m")
    g("ROAD_WIDTHS_M", "road_widths_m")
    g("WATER_WIDTHS_M", "water_widths_m")
    g("OVERPASS_URL", "overpass_url")
    g("OVERPASS_TIMEOUT_S", "overpass_timeout")
    return defaults


# ---------------------------------------------------------------------------
# ogr2ogr clipping (fast spatial pre-filter of the big national datasets)
# ---------------------------------------------------------------------------

def _ogr2ogr_exists() -> bool:
    try:
        subprocess.run(["ogr2ogr", "--version"], check=True, capture_output=True)
        return True
    except Exception:
        return False


def _clip_with_ogr2ogr(
    template_bounds,
    template_crs,
    in_path: str,
    in_layer: Optional[str],
    out_path: str,
    where: Optional[str] = None,
) -> None:
    if not _ogr2ogr_exists():
        raise RuntimeError(
            "ogr2ogr not found on PATH. Install GDAL (conda gdal) or run from OSGeo4W shell."
        )

    epsg = template_crs.to_epsg()
    spat_srs = ["-spat_srs", f"EPSG:{epsg}"] if epsg else ["-spat_srs", template_crs.to_wkt()]
    spat = [
        "-spat",
        str(template_bounds.left), str(template_bounds.bottom),
        str(template_bounds.right), str(template_bounds.top),
    ]

    cmd = ["ogr2ogr", "-f", "GPKG", out_path, in_path]
    if in_layer:
        cmd.append(in_layer)
    cmd += spat + spat_srs
    if where:
        cmd += ["-where", where]

    if os.path.exists(out_path):
        os.remove(out_path)
    subprocess.run(cmd, check=True)


def _read_and_clip(
    path: str,
    layer: Optional[str],
    crs,
    bounds,
    tmp_dir: Optional[Path] = None,
    where: Optional[str] = None,
) -> gpd.GeoDataFrame:
    """Spatially load *path* restricted to *bounds*.

    The default barrier sources are nationwide datasets (the OSM roads gpkg is
    ~20 GB), so reading the whole file is not viable.  When ogr2ogr is
    available we clip to the template extent first; otherwise we fall back to a
    full read + .cx slice (fine for small, pre-clipped inputs).
    """
    if tmp_dir is not None and _ogr2ogr_exists():
        safe = "".join(c if c.isalnum() else "_" for c in Path(path).stem)
        clip = tmp_dir / f"clip_{safe}.gpkg"
        _clip_with_ogr2ogr(bounds, crs, path, layer, str(clip), where=where)
        if not clip.exists():
            return gpd.GeoDataFrame(geometry=[], crs=crs)
        gdf = gpd.read_file(clip)
    else:
        gdf = gpd.read_file(path, layer=layer) if layer else gpd.read_file(path)
    if gdf.empty:
        return gdf
    gdf = gdf.to_crs(crs)
    return gdf.cx[bounds.left:bounds.right, bounds.bottom:bounds.top]


# ---------------------------------------------------------------------------
# OpenStreetMap Overpass API (global, no local data)
# ---------------------------------------------------------------------------

def _fetch_osm_overpass(
    crs,
    bounds,
    road_class_field: str,
    water_class_field: str,
    overpass_url: str,
    overpass_timeout: int,
) -> Tuple[gpd.GeoDataFrame, gpd.GeoDataFrame]:
    """Fetch highway + waterway ways from Overpass for the template extent.

    Returns ``(roads, water)`` GeoDataFrames in the template CRS.  The class tag
    is stored under *road_class_field* / *water_class_field* so the downstream
    dissolve/buffer logic is identical to the local path.
    """
    import requests  # local import: only needed for the OSM source
    from shapely.geometry import LineString

    # Overpass works in lon/lat (EPSG:4326); densified reprojection of the
    # template bounds avoids clipping the query box for skewed projections.
    west, south, east, north = transform_bounds(
        crs, "EPSG:4326", bounds.left, bounds.bottom, bounds.right, bounds.top,
        densify_pts=21,
    )
    bbox = f"{south},{west},{north},{east}"
    query = (
        f"[out:json][timeout:{int(overpass_timeout)}];"
        "("
        f'way["{road_class_field}"]({bbox});'
        f'way["{water_class_field}"]({bbox});'
        ");"
        "out geom;"
    )

    resp = requests.post(
        overpass_url, data={"data": query}, timeout=overpass_timeout + 30,
        headers={"User-Agent": "elmfire-makeBarrierFile/1.0 (barrier raster generator)"},
    )
    resp.raise_for_status()
    elements = resp.json().get("elements", [])

    road_geoms, road_cls = [], []
    water_geoms, water_cls = [], []
    for el in elements:
        geom = el.get("geometry")
        if not geom or len(geom) < 2:
            continue
        line = LineString([(pt["lon"], pt["lat"]) for pt in geom])
        tags = el.get("tags", {})
        if tags.get(road_class_field):
            road_geoms.append(line)
            road_cls.append(tags[road_class_field])
        elif tags.get(water_class_field):
            water_geoms.append(line)
            water_cls.append(tags[water_class_field])

    def _to_template_crs(geoms, classes, field):
        if not geoms:
            return gpd.GeoDataFrame({field: [], "geometry": []}, geometry="geometry", crs=crs)
        gdf = gpd.GeoDataFrame(
            {field: classes, "geometry": geoms}, geometry="geometry", crs="EPSG:4326",
        )
        return gdf.to_crs(crs)

    roads = _to_template_crs(road_geoms, road_cls, road_class_field)
    water = _to_template_crs(water_geoms, water_cls, water_class_field)
    print(f"  Overpass: {len(roads)} road, {len(water)} waterway features.")
    return roads, water


# ---------------------------------------------------------------------------
# Geometry helpers
# ---------------------------------------------------------------------------

def _dissolve_buffer_by_class(
    gdf: gpd.GeoDataFrame,
    class_field: str,
    width_map: Dict[str, float],
) -> List[Tuple[dict, float]]:
    """Dissolve lines by class and buffer by half-width; return rasterize shapes."""
    shapes: List[Tuple[dict, float]] = []
    if gdf.empty or class_field not in gdf.columns:
        return shapes

    gdf = gdf[[class_field, "geometry"]].copy()
    gdf = gdf[gdf.geometry.notnull() & ~gdf.geometry.is_empty]
    if gdf.empty:
        return shapes

    gdf[class_field] = gdf[class_field].astype(str)
    gdf = gdf[gdf[class_field].isin(width_map)]
    if gdf.empty:
        return shapes

    for cls, w in width_map.items():
        sub = gdf[gdf[class_field] == cls]
        if sub.empty:
            continue
        try:
            union = sub.geometry.union_all()
        except Exception:
            union = sub.unary_union
        if union is None or union.is_empty:
            continue
        half = float(w) / 2.0
        if half <= 0:
            continue
        try:
            buf = union.buffer(half)
        except Exception:
            continue
        if buf is None or buf.is_empty:
            continue
        shapes.append((buf.__geo_interface__, float(w)))

    return shapes


def _buffer_fixed_width(
    gdf: gpd.GeoDataFrame,
    width_m: float,
) -> List[Tuple[dict, float]]:
    """Buffer all geometries by half of *width_m*; return rasterize shapes."""
    if gdf is None or gdf.empty:
        return []
    gdf = gdf[["geometry"]].copy()
    gdf = gdf[gdf.geometry.notnull() & ~gdf.geometry.is_empty]
    if gdf.empty:
        return []
    half = float(width_m) / 2.0
    if half <= 0:
        return []
    try:
        union = gdf.geometry.union_all()
    except Exception:
        union = gdf.unary_union
    if union is None or union.is_empty:
        return []
    try:
        buf = union.buffer(half)
    except Exception:
        return []
    if buf is None or buf.is_empty:
        return []
    return [(buf.__geo_interface__, float(width_m))]


def _backup_rivers_not_in_primary(
    primary: gpd.GeoDataFrame,
    backup: gpd.GeoDataFrame,
    tol_m: float,
) -> gpd.GeoDataFrame:
    """Return backup features that do not intersect primary features."""
    if backup is None or backup.empty:
        return backup
    backup = backup[backup.geometry.notnull() & ~backup.geometry.is_empty].copy()
    if backup.empty:
        return backup
    if primary is None or primary.empty:
        return backup
    primary = primary[primary.geometry.notnull() & ~primary.geometry.is_empty].copy()
    if primary.empty:
        return backup
    try:
        prim_union = primary.geometry.union_all()
    except Exception:
        prim_union = primary.unary_union
    if prim_union is None or prim_union.is_empty:
        return backup
    if tol_m > 0:
        prim_union = prim_union.buffer(float(tol_m))
    return backup.loc[~backup.geometry.intersects(prim_union)].copy()


# ---------------------------------------------------------------------------
# Core
# ---------------------------------------------------------------------------

def make_barrier_file(
    template_tif: Path,
    output_tif: Path,
    *,
    source: str = "osm",
    roads_gpkg: Optional[str] = None,
    roads_layer: Optional[str] = None,
    water_gpkg: Optional[str] = None,
    water_layer: Optional[str] = None,
    backup_water_gpkg: Optional[str] = None,
    overpass_url: str = "https://overpass-api.de/api/interpreter",
    overpass_timeout: int = 180,
    road_class_field: str = "highway",
    water_class_field: str = "waterway",
    road_widths_m: Optional[Dict[str, float]] = None,
    water_widths_m: Optional[Dict[str, float]] = None,
    backup_width_m: float = 5.0,
    backup_match_tol_m: float = 1.0,
    all_touched: bool = True,
    dtype: str = "float32",
    nodata: float = -9999.0,
    overwrite: bool = False,
) -> Path:
    road_widths_m = road_widths_m or {}
    water_widths_m = water_widths_m or {}
    if source == "local" and not roads_gpkg and not water_gpkg:
        raise RuntimeError("source=local requires at least one of --roads / --water.")

    if output_tif.exists() and not overwrite:
        print(f"  Skipped — {output_tif} already exists (use --overwrite to replace).")
        return output_tif

    with rasterio.open(template_tif) as src:
        meta = src.meta.copy()
        transform = src.transform
        crs = src.crs
        shape = (src.height, src.width)
        bounds = src.bounds

    if crs is None:
        raise ValueError(f"Template raster has no CRS: {template_tif}")
    if crs.is_geographic:
        raise ValueError(
            f"Template CRS is geographic (degrees) for {template_tif}. "
            "Barrier buffering requires a projected CRS (metres)."
        )

    roads = gpd.GeoDataFrame(geometry=[], crs=crs)
    water = gpd.GeoDataFrame(geometry=[], crs=crs)
    backup = gpd.GeoDataFrame(geometry=[], crs=crs)
    has_backup = False

    if source == "osm":
        roads, water = _fetch_osm_overpass(
            crs, bounds, road_class_field, water_class_field,
            overpass_url, overpass_timeout,
        )
        # OSM waterways already cover the backup-river classes globally, so no
        # separate backup dataset is needed in this mode.
    elif source == "local":
        backup_path = Path(backup_water_gpkg) if backup_water_gpkg else None
        has_backup = backup_path is not None and backup_path.exists()
        with tempfile.TemporaryDirectory(prefix="barrier_clip_") as td:
            tmp_dir = Path(td)
            if roads_gpkg:
                roads = _read_and_clip(
                    str(roads_gpkg), roads_layer, crs, bounds, tmp_dir,
                    where=f"{road_class_field} IS NOT NULL",
                )
            if water_gpkg:
                water = _read_and_clip(
                    str(water_gpkg), water_layer, crs, bounds, tmp_dir,
                    where=f"{water_class_field} IS NOT NULL",
                )
            if has_backup:
                backup = _read_and_clip(str(backup_path), None, crs, bounds, tmp_dir)
    else:
        raise ValueError(f"Unknown source {source!r} (expected 'osm' or 'local').")

    road_shapes = _dissolve_buffer_by_class(roads, road_class_field, road_widths_m)
    water_shapes = _dissolve_buffer_by_class(water, water_class_field, water_widths_m)

    def _rasterize(shapes):
        if not shapes:
            return np.zeros(shape, dtype=dtype)
        return rasterize(
            shapes=shapes, out_shape=shape, transform=transform,
            fill=0, dtype=dtype, all_touched=all_touched,
        )

    arr = np.maximum(_rasterize(road_shapes), _rasterize(water_shapes))

    if has_backup:
        missing_backup = _backup_rivers_not_in_primary(water, backup, backup_match_tol_m)
        backup_shapes = _buffer_fixed_width(missing_backup, backup_width_m)
        if backup_shapes:
            arr = np.maximum(arr, _rasterize(backup_shapes))

    output_tif.parent.mkdir(parents=True, exist_ok=True)
    meta.update(count=1, dtype=dtype, nodata=nodata, compress="deflate", tiled=True)
    with rasterio.open(output_tif, "w", **meta) as dst:
        dst.write(arr.astype(dtype), 1)

    n_barrier = int(np.count_nonzero(arr))
    print(f"  Wrote {output_tif}  ({n_barrier} barrier pixels of {arr.size})")
    return output_tif


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def _parse_args(argv=None) -> argparse.Namespace:
    d = _load_config_defaults()
    p = argparse.ArgumentParser(
        description="Generate a barrier-width raster aligned to a template GeoTIFF.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    p.add_argument("template", type=Path,
                   help="Template GeoTIFF defining the area, grid and projected CRS.")
    p.add_argument("-o", "--output", type=Path, default=None,
                   help="Output barrier GeoTIFF (default: barrier.tif next to template).")

    p.add_argument("--source", choices=("osm", "local"), default="osm",
                   help="Vector source: 'osm' = global Overpass API; "
                        "'local' = clip from local national datasets.")

    osm = p.add_argument_group("osm source")
    osm.add_argument("--overpass-url", default=d["overpass_url"],
                     help="Overpass API endpoint.")
    osm.add_argument("--overpass-timeout", type=int, default=d["overpass_timeout"],
                     help="Overpass server-side timeout (s).")

    loc = p.add_argument_group("local source")
    loc.add_argument("--roads", default=d["roads_gpkg"],
                     help="Roads vector dataset (OSM lines).")
    loc.add_argument("--roads-layer", default=d["roads_layer"],
                     help="Layer name within the roads dataset.")
    loc.add_argument("--water", default=d["water_gpkg"],
                     help="Primary waterway vector dataset.")
    loc.add_argument("--water-layer", default=d["water_layer"],
                     help="Layer name within the water dataset.")
    loc.add_argument("--backup-water", default=d["backup_water_gpkg"],
                     help="Backup river dataset, used where primary water has no coverage.")

    p.add_argument("--road-class-field", default=d["road_class_field"],
                   help="Attribute field holding the road class.")
    p.add_argument("--water-class-field", default=d["water_class_field"],
                   help="Attribute field holding the waterway class.")
    p.add_argument("--backup-width", type=float, default=d["backup_width_m"],
                   help="Fixed width (m) applied to backup river features.")
    p.add_argument("--backup-tol", type=float, default=d["backup_match_tol_m"],
                   help="Tolerance (m) for treating primary/backup features as overlapping.")
    p.add_argument("--no-all-touched", dest="all_touched", action="store_false",
                   default=d["all_touched"],
                   help="Disable rasterio all_touched rasterisation.")
    p.add_argument("--dtype", default=d["dtype"], help="Output raster dtype.")
    p.add_argument("--nodata", type=float, default=d["nodata"], help="Output nodata value.")
    p.add_argument("--overwrite", action="store_true",
                   help="Overwrite the output if it already exists.")

    args = p.parse_args(argv)
    args._road_widths_m = d["road_widths_m"]
    args._water_widths_m = d["water_widths_m"]
    return args


def main(argv=None) -> int:
    args = _parse_args(argv)

    template = args.template
    if not template.exists():
        print(f"ERROR: template raster not found: {template}", file=sys.stderr)
        return 1

    output = args.output or (template.parent / "barrier.tif")

    print(f"Template: {template}  (source: {args.source})")
    make_barrier_file(
        template, output,
        source=args.source,
        roads_gpkg=args.roads,
        roads_layer=args.roads_layer,
        water_gpkg=args.water,
        water_layer=args.water_layer,
        backup_water_gpkg=args.backup_water,
        overpass_url=args.overpass_url,
        overpass_timeout=args.overpass_timeout,
        road_class_field=args.road_class_field,
        water_class_field=args.water_class_field,
        road_widths_m=args._road_widths_m,
        water_widths_m=args._water_widths_m,
        backup_width_m=args.backup_width,
        backup_match_tol_m=args.backup_tol,
        all_touched=args.all_touched,
        dtype=args.dtype,
        nodata=args.nodata,
        overwrite=args.overwrite,
    )
    print("[barrier] Done.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
