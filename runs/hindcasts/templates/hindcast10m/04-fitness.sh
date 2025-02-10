#!/bin/bash

CWD=$(pwd)
DATADIR=$CWD/
ONLY_USE_LAST_CALIBRATION_TIME=yes
SCRATCH=$ELMFIRE_SCRATCH_BASE/04-fitness
SEC_BETWEEN_CALIBRATION=10800
NCASES=`tail -n +2 fire_size_stats.csv | wc -l`
NPARALLEL=$NCASES
declare -a SUMFIT

AVAILABLE_POLYGONS_CLI=$ELMFIRE_BASE_DIR/cloudfire/available_polygons.py
GET_POLYGON_CLI=$ELMFIRE_BASE_DIR/cloudfire/get_polygon.py
BUFFERSCRIPT=$ELMFIRE_BASE_DIR/etc/buffer.py

rm -f -r $SCRATCH
mkdir $SCRATCH

function sec_from_timestamp {
   local TIMESTAMP=$1
   YYYYMMDD=`echo $TIMESTAMP | cut -d_ -f1`
   HHMMSS=`echo $TIMESTAMP | cut -d_ -f2`
   HH=${HHMMSS:0:2}
   MM=${HHMMSS:2:2}
   date -u -d "$YYYYMMDD $HH:$MM UTC" +%s
}

function calc_fitness {
   i=$1
   local COUNT=0
   local FITNESS

   THREE=`printf %03d $i`
   f=`ls $DATADIR/time-of-arrival_$THREE*.tif`
   gdal_calc.py -A $f --co="COMPRESS=DEFLATE" --co="ZLEVEL=9" --outfile="$SCRATCH/toa_$THREE.tif" --calc="(A>0)*60.0"
   gdal_edit.py -unsetnodata $SCRATCH/toa_$THREE.tif

   for TIMESTAMP in $CALIBRATION_TIMESTAMPS; do
      echo "$i, $TIMESTAMP"
      TIMESTAMP_SEC=`sec_from_timestamp $TIMESTAMP`
      let "ELAPSED = TIMESTAMP_SEC - FORECAST_START_SEC_FLOOR"
      gdal_calc.py -A $SCRATCH/toa_$THREE.tif -B $SCRATCH/burning.tif --NoDataValue=-9999 \
                   --co="COMPRESS=DEFLATE" --co="ZLEVEL=9" \
                   --calc="0.0 + B + 1.0*(A>0)*(A<$ELAPSED)" --outfile=$SCRATCH/model_${THREE}_$ELAPSED.tif 1>& /dev/null
# Intersection over union fitness function:
      gdal_calc.py -A $SCRATCH/target_$ELAPSED.tif -B $SCRATCH/model_${THREE}_$ELAPSED.tif -C $SCRATCH/burning.tif \
                   --co="COMPRESS=DEFLATE" --co="ZLEVEL=9" \
                   --NoDataValue=-9999 --outfile=$SCRATCH/union_${THREE}_$ELAPSED.tif \
                   --calc="numpy.absolute(C-1) * numpy.minimum( 1, (A==1) + (B==1) ) " 1>& /dev/null

      gdal_calc.py -A $SCRATCH/target_$ELAPSED.tif -B $SCRATCH/model_${THREE}_$ELAPSED.tif -C $SCRATCH/burning.tif \
                   --co="COMPRESS=DEFLATE" --co="ZLEVEL=9" \
                   --NoDataValue=-9999 --outfile=$SCRATCH/intersection_${THREE}_$ELAPSED.tif \
                   --calc="numpy.absolute(C-1) * (A==1) * (B==1) " 1>& /dev/null

      INTERSECTION_AVG=`gdalinfo -stats $SCRATCH/intersection_${THREE}_$ELAPSED.tif | grep STATISTICS_MEAN | cut -d= -f2`
      UNION_AVG=`       gdalinfo -stats $SCRATCH/union_${THREE}_$ELAPSED.tif        | grep STATISTICS_MEAN | cut -d= -f2`
      let "COUNT = COUNT + 1"
      FITNESS[COUNT]=`echo "$INTERSECTION_AVG / $UNION_AVG" | bc -l`
      SUMFIT[i]=`echo "${SUMFIT[i]} + ${FITNESS[COUNT]}" | bc -l`
   done
   SUMFIT[i]=`echo "${SUMFIT[i]} / $NUM_CALIBRATION_TIMES" | bc -l`
   echo ${SUMFIT[i]} > $SCRATCH/$i.txt
}

CWD=$(pwd)
FORECAST=`basename $CWD`
FIRENAME=`echo $FORECAST | cut -d_ -f1`
FORECAST_TIMESTAMP=`echo $FORECAST | cut -d_ -f2-3`
YEAR=${FORECAST_TIMESTAMP:0:4}
FORECAST_START_SEC=`sec_from_timestamp $FORECAST_TIMESTAMP`
SIMULATION_TSTOP=`cat elmfire.data | grep SIMULATION_TSTOP | cut -d= -f2 | xargs`
let "FORECAST_END_SEC=FORECAST_START_SEC + SIMULATION_TSTOP"

FORECAST_TIMESTAMP_FLOOR="${FORECAST_TIMESTAMP:0:11}0000"
FORECAST_START_SEC_FLOOR=`sec_from_timestamp $FORECAST_TIMESTAMP_FLOOR`

AVAILABLE_TIMESTAMPS=`$AVAILABLE_POLYGONS_CLI --active=False --year=$YEAR --list='timestamps' --firename="$FIRENAME"`

for TIMESTAMP in $AVAILABLE_TIMESTAMPS; do
   TIMESTAMP_SEC=`sec_from_timestamp $TIMESTAMP`
   echo "TIMESTAMP_SEC: $TIMESTAMP_SEC"
   if [ "$TIMESTAMP_SEC" -gt "$FORECAST_START_SEC" ] && [ "$TIMESTAMP_SEC" -lt "$FORECAST_END_SEC" ]; then
      POSSIBLE_CALIBRATION_TIMESTAMPS="$POSSIBLE_CALIBRATION_TIMESTAMPS $TIMESTAMP"
   fi
done

# Eliminate timestamps too close to themselves
CURRENT_SEC=''
CALIBRATION_TIMESTAMPS=''
NUM_CALIBRATION_TIMES=0

for TIMESTAMP in $POSSIBLE_CALIBRATION_TIMESTAMPS; do

   if [ -z "$CURRENT_SEC" ]; then
      CALIBRATION_TIMESTAMPS=$TIMESTAMP
      CURRENT_SEC=`sec_from_timestamp $TIMESTAMP`
      continue
   fi

   TIMESTAMP_SEC=`sec_from_timestamp $TIMESTAMP`
   DIFF=`echo $(($TIMESTAMP_SEC-$CURRENT_SEC)) | sed 's/-//'`
   if [ "$DIFF" -ge "$SEC_BETWEEN_CALIBRATION" ]; then
      CURRENT_SEC=$TIMESTAMP_SEC
      CALIBRATION_TIMESTAMPS=$CALIBRATION_TIMESTAMPS" $TIMESTAMP"
      let "NUM_CALIBRATION_TIMES = NUM_CALIBRATION_TIMES + 1"
   fi
done

if [ "$ONLY_USE_LAST_CALIBRATION_TIME" = "yes" ]; then
   NUM_CALIBRATION_TIMES=1
   CALIBRATION_TIMESTAMPS=`echo $CALIBRATION_TIMESTAMPS | rev | cut -d' ' -f1 | rev`
fi

f=`ls $DATADIR/time-of-arrival_001.tif | head -n 1`
SRS=`gdalsrsinfo $f | grep PROJ.4 | cut -d: -f2 | xargs`
cp -f $f $SCRATCH/dummy.tif
cp -f $DATADIR/burning.tif $SCRATCH/
gdal_edit.py -unsetnodata $SCRATCH/dummy.tif
gdal_calc.py -A $SCRATCH/dummy.tif --NoDataValue=-9999 --calc="0.0+(A*0.0)" --outfile=$SCRATCH/template.tif
rm -f $SCRATCH/dummy.tif

for TIMESTAMP in $CALIBRATION_TIMESTAMPS; do
   TIMESTAMP_SEC=`sec_from_timestamp $TIMESTAMP`
   let "ELAPSED = TIMESTAMP_SEC - FORECAST_START_SEC_FLOOR"
   $GET_POLYGON_CLI --firename="$FIRENAME" --timestamp="$TIMESTAMP" --transfer_mode='wget' --outdir="$SCRATCH"
   ogr2ogr -t_srs "$SRS" $SCRATCH/intermediate.shp $SCRATCH/${FIRENAME}_$TIMESTAMP.shp

   $BUFFERSCRIPT $SCRATCH/intermediate.shp $SCRATCH/intermediate_posbuff.shp 500.0
   cp -f $SCRATCH/intermediate.prj $SCRATCH/intermediate_posbuff.prj

   $BUFFERSCRIPT $SCRATCH/intermediate_posbuff.shp $SCRATCH/target_$ELAPSED.shp -500.0
   cp -f $SCRATCH/intermediate_posbuff.prj $SCRATCH/target_$ELAPSED.prj

   cp -f $SCRATCH/template.tif $SCRATCH/target_$ELAPSED.tif
   gdal_rasterize -burn 1 $SCRATCH/target_$ELAPSED.shp $SCRATCH/target_$ELAPSED.tif
   rm -f $SCRATCH/intermediate*
done
rm -f $SCRATCH/$FIRENAME*

N=0
for (( i=1; i<=$NCASES; i++ )); do
   SUMFIT[i]=0.
   calc_fitness $i &
   let "N = N + 1"
   if [ "$N" = "$NPARALLEL" ]; then
      wait
      N=0
   fi
done
wait

echo "All done! now summarizing fitness"

echo "icase,fitness" > $SCRATCH/fitness.csv
for (( i=1; i<=$NCASES; i++ )); do
   FIT=`cat $SCRATCH/$i.txt`
   echo "$i,$FIT" >> $SCRATCH/fitness.csv
done

cat $SCRATCH/fitness.csv | cut -d, -f2 > $SCRATCH/fitness.txt
cat $DATADIR/coeffs.csv | tr -d [:blank:] | sed 's/.$//' > $SCRATCH/coeffs.csv
paste -d, $SCRATCH/coeffs.csv $SCRATCH/fitness.txt > $DATADIR/coeffs_w_fitness.csv
mkdir $DATADIR/toa $DATADIR/targ $DATADIR/model $DATADIR/intersection $DATADIR/union 2> /dev/null
mv $SCRATCH/toa*.tif $DATADIR/toa
mv $SCRATCH/targ*.tif $DATADIR/targ
mv $SCRATCH/model*.tif $DATADIR/model
mv $SCRATCH/intersection*.tif $DATADIR/intersection
mv $SCRATCH/union*.tif $DATADIR/union

rm -f -r $SCRATCH

exit 0
