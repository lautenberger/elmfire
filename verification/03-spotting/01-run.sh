#!/bin/bash

# Begin specifying inputs

CELLSIZE=30.0 # Grid size in meters

# End inputs specification

# ELMFIRE_VER defaults to the repo-root VERSION file; export ELMFIRE_VER to override.
. ../../tutorials/functions/functions.sh
set_elmfire_version

SCRATCH=./scratch
INPUTS=./inputs
OUTPUTS=./outputs

rm -f -r $SCRATCH $INPUTS $OUTPUTS
mkdir $INPUTS $SCRATCH $OUTPUTS

echo $CELLSIZE | python input_generator.py
cp elmfire.data.in $INPUTS/elmfire.data

# Combine the topography/fuel layers into a single multiband landscape file
create_landscape $INPUTS fbfm40
cp $ELMFIRE_BASE_DIR/build/source/fuel_models.csv
A_SRS="EPSG: 32610" # Spatial reference system - UTM Zone 10

# Execute ELMFIRE
elmfire_$ELMFIRE_VER ./inputs/elmfire.data

# Postprocess
for f in ./outputs/*.bil; do
   gdal_translate -a_srs "$A_SRS" -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" $f ./outputs/`basename $f | cut -d. -f1`.tif
done
gdal_contour -i 3600 `ls ./outputs/time_of_arrival*.tif` ./outputs/hourly_isochrones.shp

# Clean up and exit:
rm -f -r ./outputs/*.bil ./outputs/*.hdr $SCRATCH

exit 0
