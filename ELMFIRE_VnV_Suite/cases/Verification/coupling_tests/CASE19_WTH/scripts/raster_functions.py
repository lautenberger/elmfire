#!/usr/bin/env python3
"""
Stack transient heat-flux rasters and plot maps/time series.

Requirements:
  pip install rasterio matplotlib numpy
"""

import glob
import csv
import re
from pathlib import Path
from typing import List
import numpy as np
import rasterio

FNAME_INDEX_RE = re.compile(r"_d?(\d{7})\.tif$", re.IGNORECASE)

def find_rasters(pattern: str) -> List[str]:
    files = glob.glob(f"{pattern}")

    if not files:
        raise FileNotFoundError("No files matched the pattern")

    # Newer ELMFIRE versions mark a sequential dump index with ``d`` while
    # older versions omit that marker.  In both forms, sort by the index.
    def dump_index(p: str) -> int:
        m = FNAME_INDEX_RE.search(p)
        if not m:
            raise ValueError(
                f"Filename does not end with _[d]XXXXXXX.tif: {p}"
            )
        return int(m.group(1))
    files.sort(key=dump_index)
    return files


def read_dump_times(output_directory: Path) -> dict[int, float] | None:
    """Return ELMFIRE's dump-index to physical-time mapping when available."""
    paths = sorted(output_directory.glob("dump_times_*.csv"))
    if not paths:
        return None
    if len(paths) != 1:
        raise ValueError(
            f"Expected one dump_times CSV in {output_directory}, found {len(paths)}"
        )
    with paths[0].open(newline="", encoding="utf-8") as stream:
        rows = csv.DictReader(stream, skipinitialspace=True)
        mapping = {
            int(row["dump_index"]): float(row["time_seconds"])
            for row in rows
        }
    if not mapping:
        raise ValueError(f"No dump timestamps found in {paths[0]}")
    return mapping

def load_stack(pattern: str):
    files = find_rasters(pattern)
    dump_times = read_dump_times(Path(files[0]).parent)
    with rasterio.open(files[0]) as src0:
        height, width = src0.height, src0.width
        transform = src0.transform
        crs = src0.crs
        nodata = src0.nodata
        bounds = src0.bounds

    t_list = []
    arr_stack = np.ma.empty((len(files), height, width), dtype=np.float32)

    for i, f in enumerate(files):
        with rasterio.open(f) as src:
            data = src.read(1).astype(np.float32)
            if nodata is not None:
                data = np.ma.masked_equal(data, nodata)
            else:
                data = np.ma.masked_invalid(data)
        match = FNAME_INDEX_RE.search(f)
        if match is None:
            raise ValueError(f"Cannot parse dump index from {f}")
        dump_index = int(match.group(1))
        if dump_times is not None:
            if dump_index not in dump_times:
                raise ValueError(
                    f"Dump index {dump_index} from {f} is absent from dump_times CSV"
                )
            t_list.append(dump_times[dump_index])
        else:
            # Compatibility with legacy outputs whose suffix was physical
            # seconds and which did not include a dump-times sidecar.
            t_list.append(float(dump_index))
        arr_stack[i] = data

    times = np.array(t_list, dtype=float)
    return arr_stack, times, transform, crs, bounds
