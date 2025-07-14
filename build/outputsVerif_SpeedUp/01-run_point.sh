#!/bin/bash
#SBATCH -n 1
#SBATCH -N 1
#SBATCH -t 3600
#SBATCH --mem-per-cpu=50000
#SBATCH --time=1-02:00:00
#SBATCH --account atrouve-prj-eng

function replace_line {
   MATCH_PATTERN=$1
   NEW_VALUE="$2"
   IS_STRING=$3

   LINE=`grep -n "$MATCH_PATTERN" ./inputs/elmfire.data | cut -d: -f1`
   sed -i "$LINE d" ./inputs/elmfire.data
   if [ "$IS_STRING" = "yes" ]; then
      sed -i "$LINE i $MATCH_PATTERN = '$NEW_VALUE'" ./inputs/elmfire.data
   else
      sed -i "$LINE i $MATCH_PATTERN = $NEW_VALUE" ./inputs/elmfire.data
   fi
}


# Begin specifying inputs

CELLSIZE=20.0 # Grid size in meters
DOMAINSIZE=12000.0 # Height and width of domain in meters
SIMULATION_TSTOP=21600.0 # Simulation stop time (seconds)

NUM_FLOAT_RASTERS=7
# FLOAT_RASTER[1]=ws   ; FLOAT_VAL[1]=15.0 # Wind speed, mph
FLOAT_RASTER[2]=wd   ; FLOAT_VAL[2]=0.0  # Wind direction, deg
FLOAT_RASTER[3]=m1   ; FLOAT_VAL[3]=3.0  # 1-hr   dead moisture content, %
FLOAT_RASTER[4]=m10  ; FLOAT_VAL[4]=4.0  # 10-hr  dead moisture content, %
FLOAT_RASTER[5]=m100 ; FLOAT_VAL[5]=5.0  # 100-hr dead moisture content, %
FLOAT_RASTER[6]=adj  ; FLOAT_VAL[6]=1.0  # Spread rate adjustment factor (-)
FLOAT_RASTER[7]=phi  ; FLOAT_VAL[7]=1.0  # Initial value of phi field

NUM_INT_RASTERS=8
INT_RASTER[1]=slp     ; INT_VAL[1]=0   # Topographical slope (deg)
INT_RASTER[2]=asp     ; INT_VAL[2]=0   # Topographical aspect (deg)
INT_RASTER[3]=dem     ; INT_VAL[3]=0   # Elevation (m)
INT_RASTER[4]=fbfm40  ; INT_VAL[4]=91 # Fire behavior fuel model code (-)
INT_RASTER[5]=cc      ; INT_VAL[5]=0   # Canopy cover (percent)
INT_RASTER[6]=ch      ; INT_VAL[6]=0   # Canopy height (10*meters)
INT_RASTER[7]=cbh     ; INT_VAL[7]=0   # Canopy base height (10*meters)
INT_RASTER[8]=cbd     ; INT_VAL[8]=0   # Canopy bulk density (100*kg/m3)

LH_MOISTURE_CONTENT=30.0 # Live herbaceous moisture content, percent
LW_MOISTURE_CONTENT=60.0 # Live woody moisture content, percent
A_SRS="EPSG: 32610" # Spatial reference system - UTM Zone 10

# End inputs specification

ELMFIRE_VER=2025.0302

#./functions/functions.sh



XMIN=`echo "0.0 - 0.5 * $DOMAINSIZE" | bc -l`
XMAX=`echo "0.0 + 0.5 * $DOMAINSIZE" | bc -l`
YMIN=$XMIN
YMAX=$XMAX

TR="$CELLSIZE $CELLSIZE"
TE="$XMIN $YMIN $XMAX $YMAX"

SCRATCH=./scratch
INPUTS=./inputs
# OUTPUTS=./outputs
MISC=./misc

rm -f -r $SCRATCH $INPUTS #$MISC # $OUTPUTS 
mkdir $INPUTS $SCRATCH #$MISC # $OUTPUTS 

cp elmfire.data.in $INPUTS/elmfire.data
# cp ./miscSOTA/fuel_models.csv $MISC
# cp ./miscSOTA/building_fuel_models.csv $MISC

printf "x,y,z\n-100000,-100000,0\n100000,-100000,0\n-100000,100000,0\n100000,100000,0\n" > $SCRATCH/dummy.xyz

gdalwarp -tr 200000 200000 -te -100000 -100000 100000 100000 -s_srs "$A_SRS" -t_srs "$A_SRS" $SCRATCH/dummy.xyz $SCRATCH/dummy.tif
gdalwarp -dstnodata -9999 -ot Float32 -tr $TR -te $TE $SCRATCH/dummy.tif $SCRATCH/float.tif
gdalwarp -dstnodata -9999 -ot Int16   -tr $TR -te $TE $SCRATCH/dummy.tif $SCRATCH/int.tif



# Create float input rasters
for i in $(eval echo "{2..$NUM_FLOAT_RASTERS}"); do
   gdal_calc.py -A $SCRATCH/float.tif --co="COMPRESS=DEFLATE" --co="ZLEVEL=9" --NoDataValue=-9999 --outfile="$INPUTS/${FLOAT_RASTER[i]}.tif" --calc="A + ${FLOAT_VAL[i]}"
done

# Create integer input rasters
for i in $(eval echo "{1..$NUM_INT_RASTERS}"); do
   gdal_calc.py -A $SCRATCH/int.tif --co="COMPRESS=DEFLATE" --co="ZLEVEL=9" --NoDataValue=-9999 --outfile="$INPUTS/${INT_RASTER[i]}.tif" --calc="A + ${INT_VAL[i]}"
done



# Set inputs in elmfire.data
replace_line COMPUTATIONAL_DOMAIN_XLLCORNER $XMIN no
replace_line COMPUTATIONAL_DOMAIN_YLLCORNER $YMIN no
replace_line COMPUTATIONAL_DOMAIN_CELLSIZE $CELLSIZE no
replace_line SIMULATION_TSTOP $SIMULATION_TSTOP no
replace_line LH_MOISTURE_CONTENT $LH_MOISTURE_CONTENT no
replace_line LW_MOISTURE_CONTENT $LW_MOISTURE_CONTENT no
replace_line A_SRS "$A_SRS" yes


for WIND_SPEED in 0.0 15.0 30.0; do
   
   OUTPUTS_DIR="./outputs_C${CELLSIZE}_W${WIND_SPEED}"
   replace_line "OUTPUTS_DIRECTORY" "$OUTPUTS_DIR" yes

   if [ -d "$OUTPUTS_DIR" ]; then
     rm -r $OUTPUTS_DIR
   fi

   mkdir $OUTPUTS_DIR

   
   gdal_calc.py -A $SCRATCH/float.tif --co="COMPRESS=DEFLATE" --co="ZLEVEL=9" --NoDataValue=-9999 --outfile="$INPUTS/ws.tif" --calc="A + $WIND_SPEED"

   # Execute ELMFIRE

   #/home/dwip/binYiren_SU/elmfire_2024.0916 ./inputs/elmfire.data
   elmfire ./inputs/elmfire.data


   # Postprocess
   for f in $OUTPUTS_DIR/*.bil; do
      gdal_translate -a_srs "$A_SRS" -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" $f $OUTPUTS_DIR/`basename $f | cut -d. -f1`.tif
   done
   # gdal_contour -i 3600 `ls $OUTPUTS_DIR/time_of_arrival*.tif` ./outputs/hourly_isochrones.shp

   # Clean up and exit:
   rm -f -r $OUTPUTS_DIR/*.bil $OUTPUTS_DIR/*.hdr ./inputs/ws.tif #$OUTPUTS_DIR/*.csv 

done

WIND_SPEED=15.0

gdal_calc.py -A $SCRATCH/float.tif --co="COMPRESS=DEFLATE" --co="ZLEVEL=9" --NoDataValue=-9999 --outfile="$INPUTS/ws.tif" --calc="A + $WIND_SPEED"


for FUEL in $(seq 1 7); do
   
   OUTPUTS_DIR="./outputs_C${CELLSIZE}_B${FUEL}"

   replace_line "BLDG_FUEL_MODEL_CONSTANT" "$FUEL" no
   replace_line "OUTPUTS_DIRECTORY" "$OUTPUTS_DIR" yes

   if [ -d "$OUTPUTS_DIR" ]; then
     rm -r $OUTPUTS_DIR
   fi

   mkdir $OUTPUTS_DIR

   #/home/dwip/binYiren_SU/elmfire_2024.0916 ./inputs/elmfire.data
   elmfire ./inputs/elmfire.data


   # Postprocess
   for f in $OUTPUTS_DIR/*.bil; do
      gdal_translate -a_srs "$A_SRS" -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" $f $OUTPUTS_DIR/`basename $f | cut -d. -f1`.tif
   done
   # gdal_contour -i 3600 `ls $OUTPUTS_DIR/time_of_arrival*.tif` ./outputs/hourly_isochrones.shp

   # Clean up and exit:
   rm -f -r $OUTPUTS_DIR/*.bil $OUTPUTS_DIR/*.hdr #$OUTPUTS_DIR/*.csv 


done

if [ -d "outputsVerif" ]; then
   rm -r outputsVerif
fi

mkdir outputsVerif
mv outputs_C* outputsVerif

exit 0
