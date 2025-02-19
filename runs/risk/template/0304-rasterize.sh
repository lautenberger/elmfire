#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1

function process_forecast_hour {

   local FH=$1
   local N=$2 # Thread number - used to lock processes to a cpu core
   local fh=$((10#$FH))
   local SCRATCH=$SCRATCHBASE/$FH
   mkdir -p $SCRATCH
   cd $SCRATCH

   echo "FH: $FH"

   TIMESTAMP=`date -u -d "$TIMESTAMP_START UTC + $fh hours" +"%Y%m%d_%H%M00"`

   taskset -a -c $N awk -v fh=$fh -F, '$2 == fh' < $SCRATCHBASE/fire_size_stats_all.csv > fire_size_stats.csv

   taskset -a -c $N cat fire_size_stats.csv | cut -d, -f3  > x.txt
   taskset -a -c $N cat fire_size_stats.csv | cut -d, -f4  > y.txt
   taskset -a -c $N cat fire_size_stats.csv | cut -d, -f7  > area.txt
   taskset -a -c $N cat fire_size_stats.csv | cut -d, -f8  > crown.txt
   taskset -a -c $N cat fire_size_stats.csv | cut -d, -f9  > vol.txt
   taskset -a -c $N cat fire_size_stats.csv | cut -d, -f10 > pop.txt

   taskset -a -c $N paste -d, x.txt y.txt area.txt  > fire-area.csv
   taskset -a -c $N paste -d, x.txt y.txt crown.txt > crown-fire-area.csv
   taskset -a -c $N paste -d, x.txt y.txt vol.txt   > fire-volume.csv
   taskset -a -c $N paste -d, x.txt y.txt pop.txt   > impacted-structures.csv

   for QUANTITY in fire-area crown-fire-area fire-volume impacted-structures; do
      echo $QUANTITY $FH

      INPUT_FILE=$QUANTITY-$FH.data
      OUTPUT_FILE=${QUANTITY}_$TIMESTAMP

      echo '&IDG_INPUTS'                              > $INPUT_FILE
      echo 'USE_KERNEL_DENSITY = .FALSE.'            >> $INPUT_FILE
      echo 'STRAIGHT_AVERAGE   = .TRUE.'             >> $INPUT_FILE
      echo 'READ_ZVAL          = .TRUE.'             >> $INPUT_FILE
      echo 'USE_ZMAX           = .FALSE.'            >> $INPUT_FILE
      echo 'SCALE_FACTOR       = 1.0'                >> $INPUT_FILE
      echo 'SEARCH_RADIUS      = 0.0'                >> $INPUT_FILE
      echo "INPUT_DIRECTORY    = './'"               >> $INPUT_FILE
      echo "OUTPUT_DIRECTORY   = './'"               >> $INPUT_FILE
      echo "OUTPUT_FILENAME    = '"$OUTPUT_FILE"'"   >> $INPUT_FILE
      echo "A_SRS              = 'EPSG:5070'"        >> $INPUT_FILE
      echo "CELLSIZE           = $CELLSIZE"          >> $INPUT_FILE
      echo "XLLCORNER          = $XLLCORNER"         >> $INPUT_FILE
      echo "YLLCORNER          = $YLLCORNER"         >> $INPUT_FILE
      echo "NCOLS              = $NCOLS"             >> $INPUT_FILE
      echo "NROWS              = $NROWS"             >> $INPUT_FILE
      echo "IGNITIONS_FILE_CSV = '"$QUANTITY.csv"'"  >> $INPUT_FILE
      echo 'CONVERT_TO_GEOTIFF = .FALSE.'            >> $INPUT_FILE
      echo 'COMPRESS           = .FALSE.'            >> $INPUT_FILE
      echo "PATH_TO_GDAL       = '"$PATH_TO_GDAL"/'" >> $INPUT_FILE
      echo '/'                                       >> $INPUT_FILE
      taskset -a -c $N $IDG $INPUT_FILE

      echo "Converting .bil to .tif (unsmoothed-) files"
      for f in $SCRATCH/*.bil; do
         taskset -a -c $N gdal_translate -a_srs "$SRS" -co "COMPRESS=DEFLATE" -co "ZLEVEL=3" $f $SCRATCH/unsmoothed-`basename -s .bil $f`.tif >& /dev/null
      done
      rm -f $SCRATCH/*.bil $SCRATCH/*.hdr

      echo "Running gdal_fillnodata"
      for f in $SCRATCH/unsmoothed-$QUANTITY*.tif; do
         FNINT=$SCRATCH/intermediate-`basename $f | cut -d- -f2-`
         FNOUT=$SCRATCH/smoothed-`basename $f | cut -d- -f2-`
         taskset -a -c $N gdal_fillnodata.py -md 100 -b 1 $f $FNINT >& /dev/null
         taskset -a -c $N gdalwarp -multi -co "COMPRESS=DEFLATE" -co "ZLEVEL=3" $FNINT $FNOUT >& /dev/null
      done

      rm -f $SCRATCH/unsmoothed*.tif

      echo "Warping to CA Albers"
      for f in $SCRATCH/smoothed-$QUANTITY*.tif; do
         gdalwarp -multi -r bilinear -t_srs "EPSG:3310" -tr 150 150 -te $TE_3310 -co "COMPRESS=DEFLATE" -co "ZLEVEL=5" $f $SCRATCH/3310-`basename $f | cut -d- -f2-` >& /dev/null
      done
      rm -f $SCRATCH/smoothed*.tif

      echo "Burning in nonburnable mask"
      for f in $SCRATCH/3310-$QUANTITY*.tif; do
         gdal_calc.py -A $f -B $SCRATCHBASE/california.tif --type=UInt16 --NoDataValue=0 --co="COMPRESS=DEFLATE" --co="ZLEVEL=9" \
         --co="TILED=yes" --calc="( 0 + (A>0)*A*B)" --outfile=$SCRATCH/`basename $f | cut -d- -f2-` >& /dev/null
      done
      rm -f $SCRATCH/3310*.tif

      cp -f $SCRATCH/$QUANTITY*.tif $OUTDIR/

   done #QUANTITY
}

# Start computations

date
START_SEC=`date +%s`

DATADIR=${DATADIR:-$1}
NPARALLEL=${NPARALLEL:-$2}
FHS=${FHS:-"$3"}

# Echo command line arguments
echo "DATADIR  : $DATADIR"
echo "NPARALLEL: $NPARALLEL"

# Temporal info
FORECAST_CYCLE=`basename $DATADIR | cut -d- -f1`
PATTERN=`basename $DATADIR | cut -d- -f2`
FUELS_INPUTS=`basename $DATADIR | cut -d- -f3`

YYYYMMDD_START=`echo $FORECAST_CYCLE | cut -d_ -f1`
HH_START=`echo $FORECAST_CYCLE | cut -d_ -f2 | cut -d- -f1`
TIMESTAMP_START=`date -u -d "$YYYYMMDD_START $HH_START:00" +"%Y-%m-%d %H:%M"`

# Paths and files
OUTDIR=$DATADIR/post
CLOUDFIRE_VER=${CLOUDFIRE_VER:-2025.0209}
IDG=$CLOUDFIRE_BASE_DIR/code/linux/bin/ignition_density_grid_$CLOUDFIRE_VER
SCRATCHBASE=$ELMFIRE_SCRATCH_BASE/0304_rasterize_${FORECAST_CYCLE}_${FUELS_INPUTS}_${PATTERN}_$RANDOM

# Parameters for CA Albers
NCOLS=5120
NROWS=8640
CELLSIZE=150.0
XLLCORNER=-2362425.0
YLLCORNER=1185435.0
SRS="EPSG:5070"
TE_3310='-400000 -700000 600050 500000'

PATH_TO_GDAL=`which gdal_translate | rev | cut -d'/' -f2- | rev`

mkdir -p $OUTDIR $SCRATCHBASE 2> /dev/null

cp -f $ELMFIRE_BASE_DIR/etc/california.tif $SCRATCHBASE/

for DIR in `ls -d $DATADIR/*/ | grep -v inputs | grep -v log | grep -v post`; do
   TILE=`basename $DIR`
   echo $TILE
   tail -n +2 $DATADIR/$TILE/fire_size_stats.csv >> $SCRATCHBASE/fire_size_stats_all.csv
done

N=0
for FH in $FHS; do
   process_forecast_hour $FH $N &
   let "N=N+1"
   if [ "$N" = "$NPARALLEL" ]; then
      N=0
      wait
   fi
done
wait

rm -f -r $SCRATCHBASE

exit 0
