#!/bin/bash

# Spread rate is calculated from Cruz et al crown fire model for:
# 20-ft wind speed:  12 mph
# 1-hr dead fuel moisture: 4%
# Canopy bulk density: 0.18 kg/m3

LOW=8.0      # Ellipse length over width
VHEAD=3938.  # Head fire spread rate (65.63 m/min = 3938 m/hr)
DT=1         # Time step at which ellipse is written (h)
TSTOP=1      # Time at which last  ellipse is written (h)
YIGN=-3000.0 # y-coordinate of ignition location (m)
OUTDIR=./exact_ellipses # Directory to put shapefiles

rm -f -r $OUTDIR
mkdir $OUTDIR

# Set up csv files containing linestring of ellipse surface
./02-ellipse-shapefiles.py $LOW $VHEAD $DT $TSTOP $YIGN $OUTDIR

# Convert those csv's to shapefiles
for (( i = $DT; i<=$TSTOP; i=i+$DT )); do
   ogr2ogr -f "ESRI Shapefile" -a_srs "EPSG:32610" $OUTDIR/ellipse_$i.shp -dialect sqlite \
   -sql "SELECT id, GeomFromText(gm) FROM ellipse_$i" $OUTDIR/ellipse_$i.csv &
done
wait

./03-run-elmfire.sh

exit 0
