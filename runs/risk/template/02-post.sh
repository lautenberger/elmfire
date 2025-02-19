#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1

function merge_forecast_hour {

   local FH=$1
   local N=$2 # Thread number - used to lock processes to a cpu core
   local SCRATCH=$SCRATCHBASE/$FH

   mkdir -p $SCRATCH
   fh=$((10#$FH))
   TIMESTAMP=`date -u -d "$DATE_START UTC + $fh hours" +"%Y%m%d_%H%M00"`

   unset FNLIST

   for TILE in $TILES; do

      unset INPUTS
      unset CALC

# Get tile location
      LINE=`cat $TILE_FILE | grep $TILE`
      XMIN=`echo $LINE | cut -d, -f5`
      YMIN=`echo $LINE | cut -d, -f6`
      XMAX=`echo $LINE | cut -d, -f7`
      YMAX=`echo $LINE | cut -d, -f8`
      TE="$XMIN $YMIN $XMAX $YMAX"

      I=`echo $TILE | cut -d_ -f1`
      J=`echo $TILE | cut -d_ -f2`
      i=$((10#$I))
      j=$((10#$J))
      let "im1 = i - 1"
      let "ip1 = i + 1"
      let "jm1 = j - 1"
      let "jp1 = j + 1"

# Sum times burned and build FNLIST[k]
      for k in $KS; do

         mkdir $SCRATCH/$k 2> /dev/null
         COUNT=0
         CALC[k]='0'
         INPUTS[k]=''
         for i in $(eval echo "{$im1..$ip1}"); do
            I=`printf %03d $i`
            for j in $(eval echo "{$jm1..$jp1}"); do
               J=`printf %03d $j`
               FN=$DATADIR/${I}_${J}/${QUANTITY_IN[k]}_$FH.tif

               if [ -e $FN ]; then
                  let "COUNT = COUNT + 1"
                  LETTER=`echo $LETTERS | cut -d' ' -f$COUNT`
                  INPUTS[k]="${INPUTS[k]} -$LETTER $SCRATCH/$k/$LETTER.tif"
                  CALC[k]="${CALC[k]} + $LETTER"
                  rm -f $SCRATCH/$k/$LETTER.tif
                  taskset -a -c $N gdalwarp -r bilinear -dstnodata 0 -te $TE -tr ${TR[k]} $FN $SCRATCH/$k/$LETTER.tif >& /dev/null
                  taskset -a -c $N gdal_edit.py -unsetnodata $SCRATCH/$k/$LETTER.tif >& /dev/null
               fi
            done #j
         done #i

# FNLIST[k] is a list of all files to mosaic
         FNOUT=$DATADIR/$TILE/${QUANTITY_OUT[k]}_$TIMESTAMP.tif
         rm -f $FNOUT
         FNLIST[k]="${FNLIST[k]} $FNOUT"
         if [ "${CALC_TYPE[k]}" = "sum" ]; then
            taskset -a -c $N gdal_calc.py ${INPUTS[k]} --NoDataValue=-9999 --calc="${CALC[k]}" --co="COMPRESS=DEFLATE" --co="ZLEVEL=9" --outfile=$FNOUT >& /dev/null
         else
            FN=$DATADIR/$TILE/${QUANTITY_IN[k]}_$FH.tif
            taskset -a -c $N gdalwarp -r bilinear -dstnodata -9999 -tr ${TR[k]} -te $TE -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" $FN $FNOUT >& /dev/null
         fi
      done

   done #TILE

# Build mosaics from FNLIST[k]
   for k in $KS; do
      taskset -a -c $N gdal_merge.py -init -9999 -n -9999 -a_nodata -9999 -o $SCRATCH/$k/${QUANTITY_OUT[k]}_$TIMESTAMP.tif ${FNLIST[k]} >& /dev/null
      taskset -a -c $N gdalwarp -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" $SCRATCH/$k/${QUANTITY_OUT[k]}_$TIMESTAMP.tif $OUTDIR/${QUANTITY_OUT[k]}_$TIMESTAMP.tif >& /dev/null
      rm -f $SCRATCH/$k/${QUANTITY_OUT[k]}_$TIMESTAMP.tif
   done

# Merge vector files for powerline fires
   if [ "$PATTERN" != "all" ]; then
      unset FNLIST
      for TILE in $TILES; do
         FNLIST="$FNLIST $DATADIR/$TILE/zonal_stats_$FH.shp"
      done
      taskset -a -c $N ogrmerge.py -progress -skipfailures -single -t_srs "EPSG:3857" -o $OUTDIR/zonal_stats_$FH.shp $FNLIST 1> /dev/null
      taskset -a -c $N ogr2ogr $OUTDIR/fire-area_$TIMESTAMP.shp           $OUTDIR/zonal_stats_$FH.shp  -sql "SELECT area as val from zonal_stats_$FH" 1> /dev/null
      taskset -a -c $N ogr2ogr $OUTDIR/fire-volume_$TIMESTAMP.shp         $OUTDIR/zonal_stats_$FH.shp  -sql "SELECT volume as val from zonal_stats_$FH" 1> /dev/null
      taskset -a -c $N ogr2ogr $OUTDIR/impacted-structures_$TIMESTAMP.shp $OUTDIR/zonal_stats_$FH.shp  -sql "SELECT structures as val from zonal_stats_$FH" 1> /dev/null
#      taskset -a -c $N ogr2ogr $OUTDIR/plignrate_$TIMESTAMP.shp           $OUTDIR/zonal_stats_$FH.shp  -sql "SELECT plignrate as val from zonal_stats_$FH" 1> /dev/null
      rm -f $OUTDIR/zonal_stats_$FH.*
   fi
}

date
START_SEC=`date +%s`

DATADIR=${DATADIR:-$1}
NPARALLEL=${NPARALLEL:-$2}
FHS=${FHS:-"$3"}

# Echo command line arguments
echo "DATADIR  : $DATADIR"
echo "NPARALLEL: $NPARALLEL"
echo "FHS      : $FHS" # Forecast hours to process

# Get run parameters from $DATADIR
FORECAST_CYCLE=`basename $DATADIR | cut -d- -f1`
PATTERN=`basename $DATADIR | cut -d- -f2`
FUELS_INPUTS=`basename $DATADIR | cut -d- -f3`
YYYYMMDD_START=`echo $FORECAST_CYCLE | cut -d_ -f1`
HH_START=`echo $FORECAST_CYCLE | cut -d_ -f2`
DATE_START=`date -u -d "$YYYYMMDD_START $HH_START:00" +"%Y-%m-%d %H:%M"`

# Directories
TILE_FILE=$ELMFIRE_BASE_DIR/config/tiles_conus_aea.csv
SCRATCHBASE=$ELMFIRE_SCRATCH_BASE/post_${FORECAST_CYCLE}_${PATTERN}_${FUELS_INPUTS}_$RANDOM
OUTDIR=$DATADIR/post

# Input and output quantities
QUANTITY_IN[1]=affected_population ; QUANTITY_OUT[1]=impacted-structures ; CALC_TYPE[1]=none ; TR[1]='150 150'
QUANTITY_IN[2]=fire_volume         ; QUANTITY_OUT[2]=fire-volume         ; CALC_TYPE[2]=none ; TR[2]='150 150'
QUANTITY_IN[3]=surface_fire_area   ; QUANTITY_OUT[3]=fire-area           ; CALC_TYPE[3]=none ; TR[3]='150 150'
QUANTITY_IN[4]=times_burned        ; QUANTITY_OUT[4]=times-burned        ; CALC_TYPE[4]=sum  ; TR[4]='150 150'
#QUANTITY_IN[5]=plignrate           ; QUANTITY_OUT[5]=plignrate           ; CALC_TYPE[5]=none ; TR[5]='150 150'

# k's specify which quantities to post-process
KS='1 2 3 4'
if [ "$PATTERN" = "all" ]; then
   KS='4'
fi

# Misc stuff
TILES=`ls -d ./0*/ | tr -d '/' | tr -d .`
LETTERS='A B C D E F G H I'

mkdir -p $SCRATCHBASE $OUTDIR 2> /dev/null

N=0
for FH in $FHS; do
   merge_forecast_hour $FH $N &
   let "N=N+1"
   if [ "$N" = "$NPARALLEL" ]; then
      N=0
      wait
   fi
done
wait

rm -f -r $SCRATCHBASE

date
END_SEC=`date +%s`
let "ELAPSED_TIME = END_SEC - START_SEC"
echo "Elapsed time:  $ELAPSED_TIME seconds"

exit 0
