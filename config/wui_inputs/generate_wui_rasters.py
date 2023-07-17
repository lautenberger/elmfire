"""generate_wui_raster.py

This script performs the following:
*   get fire model raster extents and properties from elmfire input file - this is area of interest (AOI)
*   download and save raw geojson of building footprints and heights for AOI, called from Microsoft API
*   with building footprints polygons calculate building area, minimum separation distance, 
    and determine building fuel model (PLACE HOLDER)
*   create geodataframe with polygons representing raster grid and filter to keep only grid cells 
    intersecting building footprints polygons 
*   calculate necessary attributes per grid cell/raster pixel: building area average, minimum separation
    distance, building footprint fraction (PLACE HOLDER), and building nonburnable fraction (PLACE HOLDER)
*   burn grid cell attribute values to raster pixels and save each as a separate raster file.

Notes:
*   building height of -1.0 is a no data value
*   building fuel model, footprint fraction, and nonburnable fraction are not ready, 
    however rasters are created with place holder values

REQUIREMENTS:
*   any representative elmfire input raster file is required to run this script. Set file name and path
*   install necessary python packages listed below
  
"""

# install necessary python packages
#!pip install numpy pandas rasterio shapely geopandas mercantile tqdm fiona python-dateutil

import os

dir_name = './wui_files/'  # path to data file directory
raster_file_src = os.path.join(dir_name, 'asp.tif') # filename of elmfire model raster input for grabbing geospatial properties
bldg_footprints_fn = os.path.join(dir_name, 'bldg_footprints_raw.geojson') # set building footprints filename for downloading from microsoft API


#########################################
#### CAUTION EDITING BELOW THIS LINE ####
#########################################


from datetime import datetime
from dateutil.relativedelta import relativedelta

start_time = datetime.now()


import numpy as np
import geopandas as gpd
import rasterio
from rasterio.features import rasterize
from rasterio.mask import mask
from shapely.geometry import box

from download_building_footprints import *

# this function will get building footprint polygons for the spatial domain of interest, save the polygons in a geojson to 'bldg_footprints_fn', and return a GeoDataFrame
print('Downloading and saving building footprints and heights as a GEOJSON file.')
bldg_polys_src = download_building_footprints(raster_file_src, bldg_footprints_fn)

#bldg_polys_src = gpd.read_file('')
print('\nThere are ', len(bldg_polys_src), ' building footprints in the raw downloaded file.')
#print(bldg_polys_src.head())
print('Clipping out of bounds footprints...')
with rasterio.open(raster_file_src) as src:   # Get raster file properties
  raster_bounds = src.bounds
  raster_crs = src.crs
  raster_transform = src.transform
  raster_width = src.width
  raster_height = src.height
  raster_cell_size = src.res[0]

#reproject bldg footprints back to raster CRS and clip
bldgs_reproj = bldg_polys_src.to_crs(raster_crs)
bldg_polys_gdf = bldgs_reproj.cx[raster_bounds[0]:raster_bounds[2], raster_bounds[1]:raster_bounds[3]]

bldg_polys_gdf.reset_index(inplace = True, drop=True)

print('There are ', len(bldg_polys_gdf), ' building footprints in the clipped file.')
del bldgs_reproj


print('Performing polygon operations...')

bldg_polys_gdf.columns = ['BLDG_ID','BLDG_HEIGHT', 'geometry']

# CALCULATE BLDG_AREA IN METERS SQUARED
bldg_polys_gdf.insert(2, 'BLDG_AREA', -9999.99)

if bldg_polys_gdf.crs.axis_info[0].unit_name == 'metre':
    print('CRS has units of meters. Calculating building area in m\u00B2.')
    bldg_polys_gdf.loc[:,'BLDG_AREA'] = bldg_polys_gdf.geometry.area
else:
    print('CRS does not have units of meters. Building area not calculated.')


# DETERMINE AND ASSIGN BLDG_FUEL_MODEL
bldg_polys_gdf.insert(3, 'BLDG_FUEL_MODEL', 0)   # place holder
bldg_polys_gdf['BLDG_FUEL_MODEL'] = bldg_polys_gdf['BLDG_FUEL_MODEL'].astype(int)

# DETERMINE AND ASSIGN BLDG_SEPARATION_DIST_MIN
bldg_polys_gdf.insert(4, 'BLDG_SEPARATION_DIST_MIN', -9999.99)

spatial_index = bldg_polys_gdf.sindex
polygons = bldg_polys_gdf['geometry'].values

print('Calculating the minimum separation distance between buildings.')
for index, row in tqdm(bldg_polys_gdf.iterrows(), total=bldg_polys_gdf.shape[0]):
    polygon = row['geometry']
    buffer_polygon = polygon.buffer(100)  # Create a buffer of 100 meters around the polygon
    neighbors_idx = spatial_index.intersection(buffer_polygon.bounds)

    min_distance = float(999)  # Initialize with a large value - any polygons with > ~150ft separation distance will have this set value
    for neighbor_idx in neighbors_idx:
        if neighbor_idx != index:
            neighbor_polygon = polygons[neighbor_idx]
            distance = polygon.distance(neighbor_polygon)
            min_distance = min(min_distance, distance)

    bldg_polys_gdf.at[index, 'BLDG_SEPARATION_DIST_MIN'] = min_distance

#print((bldg_polys_gdf['BLDG_SEPARATION_DIST_MIN'] == 999).sum())  # check of how many bldgs have >~150ft separation distance

# Save the geodataframe as geojson with added attributes

print('\nBuilding polygon calculations are done.\nSaving the geodataframe as a GEOJSON file with the following attributes:\n',bldg_polys_gdf.dtypes)
bldg_polys_gdf.to_file(os.path.join(dir_name, 'bldg_footprints_attributes.geojson'), driver='GeoJSON') #,crs = 32610)

#bldg_polys_gdf = gpd.read_file('./raster_files/bldg_footprints_attributes.geojson')

"""Create grid cell geodataframe representing raster pixels and keep only the intersecting ones for building footprint polygon operations"""
print('\nPreparing the building footprints data for rasterization...')

grid_cells = []

for rows in range(raster_height):
  for columns in range(raster_width):
    left = raster_bounds.left + columns * raster_cell_size
    bottom = raster_bounds.bottom + rows * raster_cell_size
    right = left + raster_cell_size
    top = bottom + raster_cell_size
    one_cell = box(left, bottom, right, top)
    grid_cells.append(one_cell)

grid = gpd.GeoDataFrame(geometry=grid_cells).reset_index(drop=True)
grid.crs = src.crs

grid_filtered = (
    grid
    .sjoin(bldg_polys_gdf[['BLDG_ID', 'geometry']], how='inner', predicate='intersects')
    .groupby('geometry')['BLDG_ID']
    .agg(list)
    .reset_index()
    .dropna(subset=['BLDG_ID'])
)

del grid

separation_dist_dict = dict(bldg_polys_gdf[['BLDG_ID', 'BLDG_SEPARATION_DIST_MIN']].values)
def get_min_separation_dist(ids):
    return min(separation_dist_dict[id] for id in ids)

bldg_area_dict = dict(bldg_polys_gdf[['BLDG_ID', 'BLDG_AREA']].values)
def get_avg_bldg_area(ids):
    areas = [bldg_area_dict[id] for id in ids if id in bldg_area_dict]
    return sum(areas) / len(areas) if areas else 0

grid_filtered['BLDG_SEPARATION_DIST_MIN'] = grid_filtered['BLDG_ID'].apply(get_min_separation_dist)
grid_filtered['BLDG_AREA_AVG'] = grid_filtered['BLDG_ID'].apply(get_avg_bldg_area)

# place holders
grid_filtered['BLDG_FUEL_MODEL'] = 0 #bldg_polys_gdf['BLDG_FUEL_MODEL']
grid_filtered['BLDG_FOOTPRINT_FRAC'] = 0.5
grid_filtered['BLDG_NONBURNABLE_FRAC'] = 0.1

#print('Burn values for rasterization are ready:\n',grid_filtered.head())


"""Create rasters"""

empty_raster = np.empty((raster_height, raster_width))
raster_profile = {
    'driver': 'GTiff',
    'height': raster_height,
    'width': raster_width,
    'count': 1,
    'crs': raster_crs,
    'transform': raster_transform
}

raster_data = [
    ('bldg_separation_distance.tif', 'BLDG_SEPARATION_DIST_MIN', empty_raster, -9999.99, 'float32'),
    ('bldg_area_avg.tif', 'BLDG_AREA_AVG', empty_raster, -9999.99, 'float32'),
    ('bldg_fuel_model.tif', 'BLDG_FUEL_MODEL', empty_raster, -9999, np.int16),
    ('bldg_footprint_frac.tif', 'BLDG_FOOTPRINT_FRAC', empty_raster, -9999.99, 'float32'),
    ('bldg_nonburnable_frac.tif', 'BLDG_NONBURNABLE_FRAC', empty_raster, -9999.99, 'float32')
]


for fn, column_name, raster, fill_value, d_types in raster_data:
    values = grid_filtered[column_name].values

    mask = rasterize(
        [(geom, 1) for geom in grid_filtered.geometry],
        out=raster,
        transform=raster_transform,
        dtype=d_types
    )

    raster[~mask.astype(bool)] = fill_value

    output_fn = os.path.join(dir_name, fn)

    with rasterio.open(output_fn, 'w', **raster_profile, dtype=d_types) as dst:
        dst.write(raster, 1)

    print(f"Raster '{os.path.basename(output_fn)}' created.")

print("Rasterization complete.")

end_time = datetime.now()
execution_time = relativedelta(end_time, start_time)
print(f"Execution time: {execution_time.minutes} minutes {execution_time.seconds} seconds")
