#!/bin/bash

# Begin specifying inputs

CELLSIZE=30.0 # Grid size in meters

# End inputs specification

ELMFIRE_VER=${ELMFIRE_VER:-2025.0212}

SCRATCH=./scratch
INPUTS=./inputs
OUTPUTS=./outputs

rm -f -r $SCRATCH $INPUTS $OUTPUTS
mkdir $INPUTS $SCRATCH $OUTPUTS

echo $CELLSIZE | python3 input_generator.py
cp elmfire.data.in $INPUTS/elmfire.data
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
