# -*- coding: utf-8 -*-
"""
Reproject + align degree_of_curing.tif to fbp.tif grid, apply (1.33 - X) / 1.11,
and save to lh.tif.

Assumptions:
- Single-band rasters
- fbp.tif is the reference grid (CRS, transform, width/height)
- Uses nearest resampling (change to bilinear if degree_of_curing is continuous)
"""

import numpy as np
import rasterio
from rasterio.warp import reproject, Resampling

IN_RASTER = r"/home/nick/elmfire/elmfire/validation/dogrib/raw/degree_of_curing_100.tif"
REF_RASTER = r"/home/nick/elmfire/elmfire/validation/dogrib/inputs/fbp_zip__clip.tif"
OUT_RASTER = r"/home/nick/elmfire/elmfire/validation/dogrib/inputs/lh.tif"

# Choose nodata for output
OUT_NODATA = -9999.0  # float nodata (since formula produces floats)

# Resampling choice:
# - nearest: categorical / integer inputs
# - bilinear: continuous inputs (often appropriate for "degree_of_curing")
RESAMPLING = Resampling.nearest


def main():
    with rasterio.open(REF_RASTER) as ref, rasterio.open(IN_RASTER) as src:
        # Prepare destination array on the reference grid
        dst = np.full((ref.height, ref.width), OUT_NODATA, dtype=np.float32)

        # Reproject+align src -> ref grid
        reproject(
            source=rasterio.band(src, 1),
            destination=dst,
            src_transform=src.transform,
            src_crs=src.crs,
            src_nodata=src.nodata,
            dst_transform=ref.transform,
            dst_crs=ref.crs,
            dst_nodata=OUT_NODATA,
            resampling=RESAMPLING,
        )

        # Apply formula safely (preserve nodata)
        mask = dst == OUT_NODATA
        out = 100 * ((1.33 - dst/100) / 1.11)
        out = out.astype(np.float32)
        out[mask] = OUT_NODATA

        # Write output with the reference grid metadata
        profile = ref.profile.copy()
        profile.update(
            driver="GTiff",
            dtype="float32",
            count=1,
            nodata=OUT_NODATA,
            compress="deflate",
            predictor=2,
            tiled=True,
            BIGTIFF="IF_SAFER",
        )

        with rasterio.open(OUT_RASTER, "w", **profile) as out_ds:
            out_ds.write(out, 1)

    print(f"Wrote: {OUT_RASTER}")


if __name__ == "__main__":
    main()