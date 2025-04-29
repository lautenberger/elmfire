#!/bin/bash

# Begin specifying inputs

CELLSIZE=30.0 # Grid size in meters
DOMAINSIZE=12000.0 # Height and width of domain in meters
SIMULATION_TSTOP=22200.0 # Simulation stop time (seconds)

NUM_FLOAT_RASTERS=2
FLOAT_RASTER[1]=adj  ; FLOAT_VAL[1]=1.0  # Spread rate adjustment factor (-)
FLOAT_RASTER[2]=phi  ; FLOAT_VAL[2]=1.0  # Initial value of phi field

NUM_INT_RASTERS=8
INT_RASTER[1]=slp     ; INT_VAL[1]=0   # Topographical slope (deg)
INT_RASTER[2]=asp     ; INT_VAL[2]=0   # Topographical aspect (deg)
INT_RASTER[3]=dem     ; INT_VAL[3]=0   # Elevation (m)
INT_RASTER[4]=fbfm40  ; INT_VAL[4]=102 # Fire behavior fuel model code (-)
INT_RASTER[5]=cc      ; INT_VAL[5]=0   # Canopy cover (percent)
INT_RASTER[6]=ch      ; INT_VAL[6]=0   # Canopy height (10*meters)
INT_RASTER[7]=cbh     ; INT_VAL[7]=0   # Canopy base height (10*meters)
INT_RASTER[8]=cbd     ; INT_VAL[8]=0   # Canopy bulk density (100*kg/m3)

A_SRS="EPSG: 32610" # Spatial reference system - UTM Zone 10

WX_INPUTS_FILE=wx.csv

# End inputs specification

ELMFIRE_VER=${ELMFIRE_VER:-2025.0429}

. ../functions/functions.sh

XMIN=`echo "0.0 - 0.5 * $DOMAINSIZE" | bc -l`
XMAX=`echo "0.0 + 0.5 * $DOMAINSIZE" | bc -l`
YMIN=$XMIN
YMAX=$XMAX

TR="$CELLSIZE $CELLSIZE"
TE="$XMIN $YMIN $XMAX $YMAX"

SCRATCH=./scratch
INPUTS=./inputs
OUTPUTS=./outputs

rm -f -r $SCRATCH $INPUTS $OUTPUTS
mkdir $SCRATCH $INPUTS $OUTPUTS

cp elmfire.data.in $INPUTS/elmfire.data

printf "x,y,z\n-100000,-100000,0\n100000,-100000,0\n-100000,100000,0\n100000,100000,0\n" > $SCRATCH/dummy.xyz

gdalwarp -tr 200000 200000 -te -100000 -100000 100000 100000 -s_srs "$A_SRS" -t_srs "$A_SRS" $SCRATCH/dummy.xyz $SCRATCH/dummy.tif
gdalwarp -dstnodata -9999 -ot Float32 -tr $TR -te $TE $SCRATCH/dummy.tif $SCRATCH/float.tif
gdalwarp -dstnodata -9999 -ot Int16   -tr $TR -te $TE $SCRATCH/dummy.tif $SCRATCH/int.tif

# Create static float input rasters
for i in $(eval echo "{1..$NUM_FLOAT_RASTERS}"); do
   gdal_calc.py -A $SCRATCH/float.tif --co="COMPRESS=DEFLATE" --co="ZLEVEL=9" --NoDataValue=-9999 --outfile="$INPUTS/${FLOAT_RASTER[i]}.tif" --calc="A + ${FLOAT_VAL[i]}"
done

# Create integer input rasters
for i in $(eval echo "{1..$NUM_INT_RASTERS}"); do
   gdal_calc.py -A $SCRATCH/int.tif --co="COMPRESS=DEFLATE" --co="ZLEVEL=9" --NoDataValue=-9999 --outfile="$INPUTS/${INT_RASTER[i]}.tif" --calc="A + ${INT_VAL[i]}"
done

# Create transient float input rasters
COLS=`head -n 1 $WX_INPUTS_FILE | tr ',' ' '`
tail -n +2 $WX_INPUTS_FILE > $SCRATCH/wx.csv
NUM_TIMES=`cat $SCRATCH/wx.csv | wc -l`

ICOL=0
for QUANTITY in $COLS; do
   let "ICOL = ICOL + 1"
   TIMESTEP=0
   FNLIST=''
   while read LINE; do
      VAL=`echo $LINE | cut -d, -f$ICOL`
      FNOUT=$SCRATCH/${QUANTITY}_$TIMESTEP.tif
      FNLIST="$FNLIST $FNOUT"
      gdal_calc.py -A $SCRATCH/float.tif --NoDataValue=-9999 --type=Float32 --outfile="$FNOUT" --calc="A + $VAL"
      let "TIMESTEP=TIMESTEP+1"
   done < $SCRATCH/wx.csv
   gdal_merge.py -separate -n -9999 -init -9999 -a_nodata -9999 -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -o $INPUTS/$QUANTITY.tif $FNLIST
done

# Set inputs in elmfire.data
replace_line COMPUTATIONAL_DOMAIN_XLLCORNER $XMIN no
replace_line COMPUTATIONAL_DOMAIN_YLLCORNER $YMIN no
replace_line COMPUTATIONAL_DOMAIN_CELLSIZE $CELLSIZE no
replace_line SIMULATION_TSTOP $SIMULATION_TSTOP no
replace_line DTDUMP $SIMULATION_TSTOP no
replace_line A_SRS "$A_SRS" yes

# Execute ELMFIRE
elmfire_$ELMFIRE_VER ./inputs/elmfire.data

# Postprocess
for f in ./outputs/*.bil; do
   gdal_translate -a_srs "$A_SRS" -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" $f ./outputs/`basename $f | cut -d. -f1`.tif
done
gdal_contour -i 3600 `ls ./outputs/time_of_arrival*.tif` ./outputs/hourly_isochrones.shp

# Clean up and exit:
#rm -f -r ./outputs/*.csv ./outputs/*.bil ./outputs/*.hdr $SCRATCH

exit 0
