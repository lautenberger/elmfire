#!/bin/bash

date
START_SEC=`date +%s`

TILE_FILE=$ELMFIRE_BASE_DIR/config/tiles_conus_aea.csv
PATTERN=`basename $(pwd) | cut -d- -f2`
if [ "$PATTERN" = "all" ]; then
   NPARALLEL=9
elif [ "$PATTERN" = "tlines" ]; then
   NPARALLEL=3
else
#   NPARALLEL=18
   NPARALLEL=6
fi

SCRATCH=$ELMFIRE_SCRATCH_BASE/risk_post_$RANDOM
OUTDIR=$(pwd)/out
TE_3310='-400000 -700000 600050 500000'

KS='1 2 3 4'
#if [ "$PATTERN" = "$UTILITY01" ] || [ "$PATTERN" = "$UTILITY02" ]; then
#   KS='1 2 3 4 5'
#fi

if [ "$PATTERN" = "all" ]; then
   KS='4'
fi

QUANTITY_IN[1]=affected_population
QUANTITY_IN[2]=fire_volume
QUANTITY_IN[3]=surface_fire_area
QUANTITY_IN[4]=times_burned
QUANTITY_IN[5]=plignrate

QUANTITY_OUT[1]=impacted-structures
QUANTITY_OUT[2]=fire-volume
QUANTITY_OUT[3]=fire-area
QUANTITY_OUT[4]=times-burned
QUANTITY_OUT[5]=plignrate

CALC_TYPE[1]=none
CALC_TYPE[2]=none
CALC_TYPE[3]=none
CALC_TYPE[4]=sum
CALC_TYPE[5]=none

FHS=`seq -f %04g 7 114`
TILES=`ls -d ./0*/ | tr -d '/' | tr -d .`
LETTERS='A B C D E F G H I'

FORECAST_CYCLE=`basename $(pwd) | head -c 11`
YYYYMMDD_START=`echo $FORECAST_CYCLE | cut -d_ -f1`
HH_START=`echo $FORECAST_CYCLE | cut -d_ -f2`
DATE_START=`date -u -d "$YYYYMMDD_START $HH_START:00" +"%Y-%m-%d %H:%M"`

rm -f -r $SCRATCH $OUTDIR
mkdir $SCRATCH $OUTDIR

cp -f $ELMFIRE_BASE_DIR/etc/california.tif $SCRATCH

function merge {

   local FH=$1
   mkdir $SCRATCH/$FH
   fh=$((10#$FH))
   TIMESTAMP=`date -u -d "$DATE_START UTC + $fh hours" +"%Y%m%d_%H%M00"`

   unset FNLIST

   for TILE in $TILES; do

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

      unset INPUTS
      unset CALC

      for k in $KS; do
         mkdir $SCRATCH/$FH/$k 2> /dev/null
         if [ "${CALC_TYPE[k]}" != "sum" ]; then
            continue
         fi
         COUNT=0
         CALC[k]='0'
         INPUTS[k]=''
         for i in $(eval echo "{$im1..$ip1}"); do
            I=`printf %03d $i`
            for j in $(eval echo "{$jm1..$jp1}"); do
               J=`printf %03d $j`
               FN=$(pwd)/${I}_${J}/${QUANTITY_IN[k]}_$FH.tif

               if [ -e $FN ]; then
                  let "COUNT = COUNT + 1"
                  LETTER=`echo $LETTERS | cut -d' ' -f$COUNT`
                  INPUTS[k]="${INPUTS[k]} -$LETTER $SCRATCH/$FH/$k/$LETTER.tif"
                  CALC[k]="${CALC[k]} + $LETTER"
                  rm -f $SCRATCH/$FH/$k/$LETTER.tif
                  gdalwarp -multi -dstnodata 0 -te $TE $FN $SCRATCH/$FH/$k/$LETTER.tif && \
                  gdal_edit.py -unsetnodata $SCRATCH/$FH/$k/$LETTER.tif &
               fi
            done
         done

      done #QUANTITY
      wait

      for k in $KS; do
         FNOUT=$(pwd)/$TILE/${QUANTITY_OUT[k]}_$TIMESTAMP.tif
         FNLIST[k]="${FNLIST[k]} $FNOUT"
         rm -f $FNOUT
         if [ "${CALC_TYPE[k]}" = "sum" ]; then
            gdal_calc.py ${INPUTS[k]} --NoDataValue=-9999 --calc="${CALC[k]}" --co="COMPRESS=DEFLATE" --co="ZLEVEL=9" --outfile=$FNOUT &
         else
            FN=$(pwd)/$TILE/${QUANTITY_IN[k]}_$FH.tif
            gdalwarp -multi -dstnodata -9999 -te $TE -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" $FN $FNOUT &
         fi
      done
      wait

   done #TILE

   if [ "$PATTERN" = "all" ]; then
      k=4
      gdal_merge.py -init -9999 -n -9999 -a_nodata -9999 -o $SCRATCH/$FH/$k/${QUANTITY_OUT[k]}_$TIMESTAMP.tif ${FNLIST[k]} &&
      gdalwarp -multi -r bilinear -t_srs "EPSG:3310" -tr 150 150 -te $TE_3310 -co "COMPRESS=DEFLATE" -co "ZLEVEL=5" -wo "NUM_THREADS=2" $SCRATCH/$FH/$k/${QUANTITY_OUT[k]}_$TIMESTAMP.tif \
      $SCRATCH/$FH/$k/3310-${QUANTITY_OUT[k]}_$TIMESTAMP.tif &&
      gdal_calc.py -A $SCRATCH/$FH/$k/3310-${QUANTITY_OUT[k]}_$TIMESTAMP.tif -B $SCRATCH/california.tif --type=UInt16 --NoDataValue=0 --co="COMPRESS=DEFLATE" --co="ZLEVEL=9" \
      --co="TILED=yes" --calc="( 0 + (A>0)*A*B)" --outfile=$OUTDIR/${QUANTITY_OUT[k]}_$TIMESTAMP.tif &&
      rm -f $SCRATCH/$FH/$k/${QUANTITY_OUT[k]}_$TIMESTAMP.tif $SCRATCH/$FH/$k/3310-${QUANTITY_OUT[k]}_$TIMESTAMP.tif &
   else
      for k in $KS; do
         gdal_merge.py -init -9999 -n -9999 -a_nodata -9999 -o $SCRATCH/$FH/$k/${QUANTITY_OUT[k]}_$TIMESTAMP.tif ${FNLIST[k]} &&
         gdalwarp -multi -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -wo "NUM_THREADS=4" $SCRATCH/$FH/$k/${QUANTITY_OUT[k]}_$TIMESTAMP.tif $OUTDIR/${QUANTITY_OUT[k]}_$TIMESTAMP.tif &&
         rm -f $SCRATCH/$FH/$k/${QUANTITY_OUT[k]}_$TIMESTAMP.tif &
      done
#      k=4
#      gdal_merge.py -init -9999 -n -9999 -a_nodata -9999 -o $SCRATCH/$FH/$k/${QUANTITY_OUT[k]}_$TIMESTAMP.tif ${FNLIST[k]} &&
#      gdalwarp -multi -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -co "TILED=yes" $SCRATCH/$FH/$k/${QUANTITY_OUT[k]}_$TIMESTAMP.tif $OUTDIR/${QUANTITY_OUT[k]}_$TIMESTAMP.tif &&
#      rm -f $SCRATCH/$FH/$k/${QUANTITY_OUT[k]}_$TIMESTAMP.tif &
   fi
   wait

   if [ "$PATTERN" != "all" ]; then
      unset FNLIST
      for TILE in $TILES; do
         FNLIST="$FNLIST $(pwd)/$TILE/zonal_stats_$FH.shp"
      done
      ogrmerge.py -progress -skipfailures -single -t_srs "EPSG:3857" -o $OUTDIR/zonal_stats_$FH.shp $FNLIST
      ogr2ogr $OUTDIR/fire-area_$TIMESTAMP.shp           $OUTDIR/zonal_stats_$FH.shp  -sql "SELECT area as val from zonal_stats_$FH" &
      ogr2ogr $OUTDIR/fire-volume_$TIMESTAMP.shp         $OUTDIR/zonal_stats_$FH.shp  -sql "SELECT volume as val from zonal_stats_$FH" &
      ogr2ogr $OUTDIR/impacted-structures_$TIMESTAMP.shp $OUTDIR/zonal_stats_$FH.shp  -sql "SELECT structures as val from zonal_stats_$FH" &
      ogr2ogr $OUTDIR/plignrate_$TIMESTAMP.shp           $OUTDIR/zonal_stats_$FH.shp  -sql "SELECT plignrate as val from zonal_stats_$FH" &
      wait
      rm -f $OUTDIR/zonal_stats_$FH.*
   fi
}

N=0
for FH in $FHS; do
   merge $FH &
   let "N=N+1"
   if [ "$N" = "$NPARALLEL" ]; then
      wait
      N=0
   fi
done

wait
rm -f -r $SCRATCH

date
END_SEC=`date +%s`
let "ELAPSED_TIME = END_SEC - START_SEC"
echo "Elapsed time:  $ELAPSED_TIME"

exit 0
