#!/bin/bash

# Spread rate is calculated from Behave for:
# Fuel model: GR2 (102)
# 1-h fuel moisture: 3%
# Live herbaceous fuel moisture: 30%
# 20-ft wind speed:  10 mph
# Slope steepness: 0%
# Mid flame wind speed, from Behave, is 3.62 mph which
# Gives L/W = 2.236 from Equation 5.

LOW=2.236    # Ellipse length over width
VHEAD=843.99 # Head fire spread rate (46.15 ft/min = 843.99 m/hr)
DT=1         # Time step at which ellipse is written (h)
TSTOP=6      # Time at which last  ellipse is written (h)
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
