import numpy as np
import rasterio

template = "inputs/ws.tif"   # existing raster to copy properties from
output   = "inputs/m1.tif"

ffmc = np.array([89.1, 89.9, 90.4, 90.8, 90.9, 90.9, 90.7, 90.6])
values = 147.2*(101-ffmc)/(59.5+ffmc)

with rasterio.open(template) as src:
    profile = src.profile
    height = src.height
    width = src.width

# update profile for multiband output
profile.update(
    count=len(values),
    dtype="float32"
)

with rasterio.open(output, "w", **profile) as dst:
    for i, val in enumerate(values, start=1):
        band = np.full((height, width), val, dtype=np.float32)
        dst.write(band, i)