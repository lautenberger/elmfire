#!/bin/bash

# Begin specifying inputs

CELLSIZE=30.0 # Grid size in meters

# End inputs specification

ELMFIRE_VER=${ELMFIRE_VER:-2023.1015}

SCRATCH=./scratch
INPUTS=./inputs
OUTPUTS=./outputs
MISC=./misc

rm -f -r $SCRATCH $INPUTS $OUTPUTS $MISC
mkdir $INPUTS $SCRATCH $OUTPUTS $MISC

cp $ELMFIRE_BASE_DIR/build/source/fuel_models.csv $MISC

echo $CELLSIZE | python3 input_generator.py
cp elmfire.data.in $INPUTS/elmfire.data
cp $ELMFIRE_BASE_DIR/build/source/fuel_models.csv $MISC
cp $ELMFIRE_BASE_DIR/build/source/building_fuel_models.csv $MISC
A_SRS="EPSG: 32610" # Spatial reference system - UTM Zone 10

# Execute ELMFIRE
mpirun -np 1 elmfire_debug ./inputs/elmfire.data
# elmfire_debug ./inputs/elmfire.data

# Postprocess
#for outputs/*.bil; do
#   gdal_translate -a_srs "$A_SRS" -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" $f ./outputs/`basename $f | cut -d. -f1`.tif
#done
#gdal_contour -i 3600 `ls ./outputs/time_of_arrival*.tif` ./outputs/hourly_isochrones.shp

# Clean up and exit:
#rm -f -r ./outputs/*.bil ./outputs/*.hdr $SCRATCH $MISC

exit 0
