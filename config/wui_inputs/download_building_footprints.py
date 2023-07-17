# +
# adapted with modifications from: https://github.com/microsoft/GlobalMLBuildingFootprints/blob/main/examples/example_building_footprints.ipynb
# -

import pandas as pd
import geopandas as gpd
import rasterio
from rasterio.warp import transform_bounds
import mercantile
import shapely.geometry
from tqdm import tqdm
import os
import tempfile
import fiona


def download_building_footprints(raster_file_AOI, output_file):

  ###############################################################
  ###### STEP 0: Load a project/fire model raster file & reproject  
  ###### to get desired extents for extracting building footprints
  ###############################################################

  # Define the source and target coordinate reference systems
  with rasterio.open(raster_file_AOI) as src:   # Read raster file and get its properties
    bounds = src.bounds
    src_crs = src.crs
  dst_crs = 'EPSG:4326'

  minx = bounds.left
  miny = bounds.bottom
  maxx = bounds.right
  maxy = bounds.top

  # Transform the extents coordinates
  minx, miny, maxx, maxy = transform_bounds(src_crs, dst_crs, minx, miny, maxx, maxy)

  ###############################################################
  ###### STEP 1: Define our area of interest (AOI).
  ###### Note: the coordinate reference system for the GeoJSON  
  ###### should be "EPSG:4326", i.e. in global lat/lon format.
  ###############################################################
    
  # Geometry copied from https://geojson.io
  aoi_geom = {
      "coordinates": [
          [   [minx, maxy],
              [minx, miny],
              [maxx, miny],
              [maxx, maxy],
              [minx, maxy],
          ]
      ],
      "type": "Polygon",
  }
  aoi_shape = shapely.geometry.shape(aoi_geom)
  minx, miny, maxx, maxy = aoi_shape.bounds

  ###############################################################
  ###### STEP 2: Determine which tiles intersect our AOI
  ###############################################################

  quad_keys = set()
  for tile in list(mercantile.tiles(minx, miny, maxx, maxy, zooms=9)):
      quad_keys.add(int(mercantile.quadkey(tile)))
  quad_keys = list(quad_keys)
  print(f"The input area spans {len(quad_keys)} tiles: {quad_keys}")

  ###############################################################
  ###### STEP 3: Download the building footprints for
  ###### each tile that intersects our AOI and crop the results
  ###############################################################

  df = pd.read_csv(
    "https://minedbuildings.blob.core.windows.net/global-buildings/dataset-links.csv"
  )

  idx = 0
  combined_rows = []

  with tempfile.TemporaryDirectory() as tmpdir:
     # Download the GeoJSON files for each tile that intersects the input geometry
     tmp_fns = []
     for quad_key in tqdm(quad_keys):
      rows = df[df["QuadKey"] == quad_key]
      if rows.shape[0] == 1:
          url = rows.iloc[0]["Url"]

          df2 = pd.read_json(url, lines=True)
          df2["geometry"] = df2["geometry"].apply(shapely.geometry.shape)

          gdf = gpd.GeoDataFrame(df2, crs=4326)
          fn = os.path.join(tmpdir, f"{quad_key}.geojson")
          tmp_fns.append(fn)
          if not os.path.exists(fn):
              gdf.to_file(fn, driver="GeoJSON")
      elif rows.shape[0] > 1:
          raise ValueError(f"Multiple rows found for QuadKey: {quad_key}")
      else:
          raise ValueError(f"QuadKey not found in dataset: {quad_key}")

     # Merge the GeoJSON files into a single file
     for fn in tmp_fns:
          with fiona.open(fn, "r") as f:
              for row in tqdm(f):
                  row = dict(row)
                  shape = shapely.geometry.shape(row["geometry"])
                  height = row['properties']['properties']
                  height_val = float(str(height)[10:-1])

                  if aoi_shape.contains(shape):
                      if "id" in row:
                          del row["id"]
                      row["properties"] = {"id": idx, 'height': height_val}
                      idx += 1
                      combined_rows.append(row)
                      

  ###############################################################
  ###### STEP 4: Save the resulting footprints to file
  ###############################################################

  schema = {"geometry": "Polygon", "properties": {"id": "int", "height": "float"}}

  with fiona.open(output_file, "w", driver="GeoJSON", crs="EPSG:4326", schema=schema) as f:
    f.writerecords(combined_rows)

  bldg_footprints_gdf = gpd.read_file(output_file)

  return bldg_footprints_gdf


