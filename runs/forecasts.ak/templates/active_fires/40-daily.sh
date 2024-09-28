#!/bin/bash

PERCS='10 30 50 70 90'
EOD_UTC=07
MURRELET=10.1.30.133

function attribute_table {
   f=$1
   ID=$2

   BASENAME=`basename -s .gpkg $f`
   FIRE_NAME=`echo $BASENAME | cut -d_ -f1`
   DATE=`echo $BASENAME | cut -d_ -f2`
   TIME=`echo $BASENAME | cut -d_ -f3`
   TIMESTAMP=${DATE}_$TIME
   PERC=`echo $BASENAME | cut -d_ -f4`
   TABLE=`ogrinfo $f | grep '1:' | cut -d' ' -f2`

   ogrinfo $f -sql "ALTER TABLE $TABLE ADD COLUMN id integer"
   echo "ogrinfo $f -sql ALTER TABLE $TABLE ADD COLUMN 'fire-name' text"
   ogrinfo $f -sql "ALTER TABLE $TABLE ADD COLUMN 'fire-name' text"
   ogrinfo $f -sql "ALTER TABLE $TABLE ADD COLUMN timestamp text"
   ogrinfo $f -sql "ALTER TABLE $TABLE ADD COLUMN percentile integer"

   ogrinfo $f -dialect SQLite -sql "UPDATE $TABLE SET id = '$ID'"
   ogrinfo $f -dialect SQLite -sql "UPDATE $TABLE SET 'fire-name' = '$FIRENAME'"
   ogrinfo $f -dialect SQLite -sql "UPDATE $TABLE SET timestamp = '$TIMESTAMP'"
   ogrinfo $f -dialect SQLite -sql "UPDATE $TABLE SET percentile = '$PERC'"
}

eod_utc=$((10#$EOD_UTC))

WX_START_TIME=`cat metadata.out | grep WX_BEGIN | cut -d= -f2 | xargs`
STARTING_DATE=`cat metadata.in  | grep NAME | head -n 1 | cut -d_ -f2`
STARTING_TIME=`cat metadata.in  | grep NAME | head -n 1 | cut -d_ -f3`
STARTING_TIMESTAMP="${STARTING_DATE}_$STARTING_TIME"

FIRENAME=`cat metadata.in | grep FIRENAME | cut -d= -f2 | xargs`

SCRATCH=$CLOUDFIRE_SCRATCH_BASE/gsr
OUTDIR=./daily_`cat metadata.in | grep NAME | head -n 1 | cut -d= -f2 | xargs`

rm -f -r $SCRATCH $OUTDIR
mkdir $SCRATCH $OUTDIR

HH_START=`echo $WX_START_TIME | cut -d' ' -f2 | cut -d: -f1`
hh_start=$((10#$HH_START))

if [ "$hh_start" -le "$eod_utc" ]; then
   let "hour = eod_utc - hh_start"
else
   let "hour = 24 - (hh_start - eod_utc)"
fi

while [ "$hour" -le "336" ]; do
   hours_to_process="$hours_to_process $hour"
   let "hour = hour + 24"
done

for hour in $hours_to_process; do
   for PERC in $PERCS; do
      gdal_calc.py -A ./toa_$PERC.tif --type=Byte --NoDataValue=0 --outfile=$SCRATCH/fire_${hour}_$PERC.tif --calc="0+(A>0)*(A<=$hour)" && \
      gdal_polygonize.py $SCRATCH/fire_${hour}_$PERC.tif $SCRATCH/fire_${hour}_${PERC}_multifeature.shp && \
      ogr2ogr $SCRATCH/fire_${hour}_$PERC.shp $SCRATCH/fire_${hour}_${PERC}_multifeature.shp -dialect sqlite -sql "SELECT ST_Union(geometry) FROM fire_${hour}_${PERC}_multifeature" &
   done
done
wait

for hour in $hours_to_process; do
   TIMESTAMP=`date -u -d "$WX_START_TIME UTC + $hour hours" +"%Y%m%d_%H%M00"`
   for PERC in $PERCS; do
      ogr2ogr -f GPKG $OUTDIR/${FIRENAME}_${TIMESTAMP}_$PERC.gpkg $SCRATCH/fire_${hour}_$PERC.shp &
   done
   wait
done

# New part to set attribute table:
ID=0
FNLIST=''
for f in $OUTDIR/*.gpkg; do
   FNLIST="$FNLIST $f"
   let "ID = ID + 1"
   attribute_table $f $ID &
done
wait

# New part to merge into one geopackage
rm -f $SCRATCH/merged.gpkg
for f in $FNLIST; do
   if [ -e $SCRATCH/merged.gpkg ]; then
      ogr2ogr -update -append $SCRATCH/merged.gpkg $f -nln merged
   else
      ogr2ogr $SCRATCH/merged.gpkg $f -nln merged
   fi
done
ogr2ogr $OUTDIR/${FIRENAME}_$STARTING_TIMESTAMP.gpkg $SCRATCH/merged.gpkg
ogr2ogr -f CSV $OUTDIR/${FIRENAME}_$STARTING_TIMESTAMP.csv $OUTDIR/${FIRENAME}_$STARTING_TIMESTAMP.gpkg

tar -cvzf $OUTDIR.tgz $OUTDIR
cp -f $OUTDIR.tgz $ELMFIRE_BASE_DIR/runs/forecasts/rsync/
scp $OUTDIR/${FIRENAME}_$STARTING_TIMESTAMP.gpkg $MURRELET:/srv/gis/fire_spread_polygons/
ssh $MURRELET "sudo chmod 775 /srv/gis/fire_spread_polygons/${FIRENAME}_$STARTING_TIMESTAMP.gpkg"

aws s3 cp $OUTDIR.tgz s3://aws.c-gsr-pg-pyregence-bucket/daily_perimeters/ --profile gsr

rm -f -r $OUTDIR $SCRATCH

exit 0
