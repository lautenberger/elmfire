progress_message () {
   NOW=`date -u +"%Y-%m-%d %H:%M:%S"`
   echo "$NOW - $1"
}

rename () {
   local frame=$1
   local FRAME=`printf %03d $frame`
   local TIMESTAMP=${TIMESTAMP_BY_FRAME[frame]}
   local CWD=$(pwd)

   for RASTER in $RASTERS; do
      for PERC in $PERCS; do
         PERC_TWO=`printf %02d $PERC`
         f=$CWD/${RASTER}_${PERC_TWO}_$FRAME.tif
         mv $f ./geoserver/$FIRE_NAME/${START_DATE}_${START_TIME}/elmfire/landfire/$PERC_TWO/$RASTER/${RASTER}_$TIMESTAMP.tif 2> /dev/null
      done
   done
}

timestamp_by_frame () {
   local frame=$1
   local MINUTES
   if [ "$frame" = "1" ]; then
      local TIMESTAMP=`date -u -d "$START_DATE $START_HOUR:$START_MIN UTC" +"%Y%m%d_%H%M00"`
   else
      let "MINUTES = (frame - 1) * 60 - start_min"
      local TIMESTAMP=`date -u -d "$START_DATE $START_HOUR:$START_MIN UTC + $MINUTES minutes" +"%Y%m%d_%H%M00"`
   fi
   TIMESTAMP_BY_FRAME[$frame]="$TIMESTAMP"
}

set_percentile () {
   local FN=$1
   local PERCENTILE=$2
   BASENAME=`basename $FN`
   STUB=`echo $BASENAME | cut -d. -f1`
   ogrinfo $FN -sql "ALTER TABLE $STUB DROP COLUMN FID"
   ogrinfo $FN -sql "ALTER TABLE $STUB ADD  COLUMN PERCENTILE Integer"
   ogrinfo $FN -dialect SQLite -sql "UPDATE $STUB SET PERCENTILE = $PERCENTILE"
}

posnegbuffer () {
   local FNIN=$1
   local FNOUT_NOPATH=$2
   local PATHNAME=$3
   local FID=$4
   local BUFFERDIST=$5

   UNBUFFERED=./scratch/unbuffered_isochrones_${FID}_`basename $FNIN`
   POSBUFFERED=./scratch/posbuffered_isochrones_${FID}_`basename $FNIN`
   FNOUT_LOCAL=./scratch/isochrones_${FID}_`basename $FNIN`
   PRJIN=./scratch/unbuffered_isochrones_${FID}_`basename -s .shp $FNIN`.prj
   PRJOUT=./scratch/`basename -s .shp $FNOUT_LOCAL`.prj

   ogr2ogr -fid $FID $UNBUFFERED $FNIN

   $BUFFER_SCRIPT $UNBUFFERED  $POSBUFFERED  $BUFFERDIST
   $BUFFER_SCRIPT $POSBUFFERED $FNOUT_LOCAL -$BUFFERDIST
   cp -f $PRJIN $PRJOUT
   ogr2ogr $PATHNAME/$FNOUT_NOPATH $FNOUT_LOCAL
}

add_acreage () {
   local PERC_TWO=$1
   local DAYS=$2
   local INTERMEDIATE=$SCRATCH/intermediate_${DAYS}_$PERC_TWO.shp
   local BASENAME=`basename -s .shp $INTERMEDIATE`
   ogr2ogr $INTERMEDIATE $SCRATCH/${FIRE_NAME}_${MDT}_${DAYS}_elmfire_$PERC_TWO.shp
   ACRES=`ogrinfo -sql "SELECT SUM(OGR_GEOM_AREA) AS TOTAL_AREA FROM $BASENAME" $INTERMEDIATE | grep 'TOTAL_AREA (Real)' | cut -d= -f2 | xargs`
   ACRES=`echo "$ACRES / 4046.86" | bc -l | cut -d. -f1`
   ACRES=`printf %06d $ACRES`
#   ogr2ogr $SCRATCH/elm_${FIRE_NAME}_${MDT}_${DAYS}_${ACRES}_$PERC_TWO.shp   $SCRATCH/${FIRE_NAME}_${MDT}_${DAYS}_elmfire_$PERC_TWO.shp
   ogr2ogr $SCRATCH/${FIRE_NAME}_${MDT}_${DAYS}_${ACRES}_${PERC_TWO}_elm.shp $SCRATCH/${FIRE_NAME}_${MDT}_${DAYS}_elmfire_$PERC_TWO.shp
   rm -f $SCRATCH/${FIRE_NAME}_${MDT}_${DAYS}_elmfire_$PERC_TWO.*
}
