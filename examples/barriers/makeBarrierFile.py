import numpy as np
import rasterio

# ---- USER INPUTS ----
input_tif = r"\\wsl.localhost\Ubuntu-24.04\home\nick\elmfire_examples\barriers\inputs\adj.tif"
output_tif = r"\\wsl.localhost\Ubuntu-24.04\home\nick\elmfire_examples\barriers\inputs\barrier.tif"
values = [2, 4, 11, 12, 20]
dtype = "float32"
# ---------------------

def compute_row_indices(height, n, margin_from_bottom=15):
    """
    n stripes:
      - first stripe at halfway down the raster
      - last stripe margin_from_bottom cells above the last row
      - others equally spaced between
    """
    if n <= 0:
        return []

    start = height / 2.0
    end = (height - 1) - margin_from_bottom

    # Safety: if margin is too big or raster is small, keep end within bounds
    end = max(0, min(height - 1, end))

    if n == 1:
        rows = [int(round(start))]
    else:
        # If end is above start (tiny raster / big margin), step becomes <= 0
        step = (end - start) / (n - 1)
        rows = [int(round(start + i * step)) for i in range(n)]

    # Clip to valid range
    rows = [max(0, min(height - 1, r)) for r in rows]

    # If rounding caused duplicates, nudge to keep unique where possible
    used = set()
    out = []
    for r in rows:
        rr = r
        while rr in used and rr < height - 1:
            rr += 1
        while rr in used and rr > 0:
            rr -= 1
        used.add(rr)
        out.append(rr)

    return out


with rasterio.open(input_tif) as src:
    height, width = src.height, src.width
    transform = src.transform
    crs = src.crs

    arr = np.zeros((height, width), dtype=dtype)

    stripe_rows = compute_row_indices(height, len(values))
    for r, v in zip(stripe_rows, values):
        arr[r, :] = v

    profile = src.profile.copy()
    profile.update(
        count=1,
        dtype=dtype,
        nodata=-9999
    )

with rasterio.open(output_tif, "w", **profile) as dst:
    dst.write(arr, 1)

print("Done. Stripe rows:", stripe_rows)
