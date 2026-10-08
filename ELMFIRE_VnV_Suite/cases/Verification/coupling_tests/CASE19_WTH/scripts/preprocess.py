#!/usr/bin/env python3
"""Generate the low-propagation adjustment raster used to isolate one WUI source.

The source adj.tif supplies the authoritative grid, georeferencing, nodata,
and profile. The generated field is uniformly zero, preventing any secondary
level-set source during the 5000 s verification interval. A dormant point
ignition after the stop time keeps the generic stalled-front shortcut inactive.
"""

from pathlib import Path

import numpy as np
import rasterio


CASE_DIR = Path(__file__).resolve().parents[1]
INPUT_DIR = CASE_DIR / "data" / "inputs"
SOURCE_PATH = INPUT_DIR / "adj.tif"
OUTPUT_PATH = INPUT_DIR / "isolated_adj.tif"
ISOLATION_ADJUSTMENT = 0.0


def main() -> None:
    with rasterio.open(SOURCE_PATH) as source:
        profile = source.profile.copy()
        source_values = source.read(1, masked=True)

    values = np.full(source_values.shape, ISOLATION_ADJUSTMENT, dtype=np.float32)
    if np.ma.is_masked(source_values):
        nodata = profile.get("nodata")
        if nodata is None:
            raise ValueError("adj.tif has masked cells but no nodata value")
        values[np.ma.getmaskarray(source_values)] = nodata

    profile.update(dtype="float32", count=1)
    with rasterio.open(OUTPUT_PATH, "w", **profile) as destination:
        destination.write(values, 1)
    print(f"[OK] wrote {OUTPUT_PATH} with adjustment {ISOLATION_ADJUSTMENT:g}")


if __name__ == "__main__":
    main()
