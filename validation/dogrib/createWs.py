import numpy as np
import rasterio

template = "inputs/wd.tif"   # existing raster to copy properties from
output   = "inputs/ws.tif"

kph = np.array([21, 25, 27, 37, 43, 45, 46, 18])
values = kph/1.6

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