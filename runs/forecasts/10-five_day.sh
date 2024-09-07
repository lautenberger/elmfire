#!/bin/bash

DATADIR=$(pwd)/runs
SCRATCH=$ELMFIRE_SCRATCH_BASE/fiveday_collate
OUTDIR=$(pwd)/five_day
PERCENTILE_TO_EXTRACT=90
T_SRS='EPSG:5070'
ACTIVE_FIRES_PYREGENCE=$CLOUDFIRE_BASE_DIR/inputs/fire/active_fire_polygons/active_fires_list/active_fires_pyregence.csv

rm -f -r $SCRATCH
mkdir -p $SCRATCH/native $SCRATCH/epsg5070 $OUTDIR 2> /dev/null

END_SEC=$(date -u +%s)
START_SEC=$(date -u -d "24 hours ago" +%s)

TIMESTAMP_TO_EXTRACT=`date -u -d "today + 5 days" +%Y%m%d`
TIMESTAMP_TO_EXTRACT=${TIMESTAMP_TO_EXTRACT}_070000

echo $TIMESTAMP_TO_EXTRACT

FNLIST=''
for DIR in `ls -d $DATADIR/*/`; do
   FIRE=`basename $DIR`
   LATEST=$(ls -rt $(find $DATADIR/$FIRE/ -name daily*.tgz) | tail -n 1)
   BASENAME=`basename $LATEST`
   if [ ${BASENAME:0:5} != "daily" ]; then
      continue
   fi

   FILE_SEC=`date -r $LATEST +%s`
   if [ "$FILE_SEC" -gt "$START_SEC" ] && [ "$FILE_SEC" -le "$END_SEC" ]; then
      FNLIST="$FNLIST $LATEST"
   fi
done

for f in $FNLIST; do
   rm -f -r $SCRATCH/native
   mkdir $SCRATCH/native

   tar -xvzf $f -C $SCRATCH/native --strip-components 2 1> /dev/null

   GPKG=`ls -rt $SCRATCH/native/*_${TIMESTAMP_TO_EXTRACT}_$PERCENTILE_TO_EXTRACT.gpkg | tail -n 1`
   BASENAME=`basename $GPKG`
   FIRE=`echo $BASENAME | cut -d_ -f1`
   ACRES=`cat $ACTIVE_FIRES_PYREGENCE | grep $FIRE | cut -d, -f6 | tr -d '"'`
   CONTAINMENT=`cat $ACTIVE_FIRES_PYREGENCE | grep $FIRE | cut -d, -f7 | tr -d '"'`

   if [ -z "$ACRES" ]; then
      ACRES=0.0
   fi

   if [ -z "$CONTAINMENT" ]; then
      CONTAINMENT=0.0
   fi

   ogr2ogr -progress -skipfailures -t_srs "$T_SRS" $SCRATCH/epsg5070/$BASENAME $GPKG -nln $TIMESTAMP_TO_EXTRACT
   ogrinfo $SCRATCH/epsg5070/$BASENAME -sql "alter table '$TIMESTAMP_TO_EXTRACT' add column acres double"
   ogrinfo $SCRATCH/epsg5070/$BASENAME -sql "alter table '$TIMESTAMP_TO_EXTRACT' add column contain double"

   ogrinfo $SCRATCH/epsg5070/$BASENAME -dialect SQLite -sql "update '$TIMESTAMP_TO_EXTRACT' set acres=$ACRES"
   ogrinfo $SCRATCH/epsg5070/$BASENAME -dialect SQLite -sql "update '$TIMESTAMP_TO_EXTRACT' set contain=$CONTAINMENT"

done

for f in $SCRATCH/epsg5070/*.gpkg; do
   if [ ! -e $SCRATCH/$TIMESTAMP_TO_EXTRACT.gpkg ]; then
      ogr2ogr $SCRATCH/$TIMESTAMP_TO_EXTRACT.gpkg $f -nln $TIMESTAMP_TO_EXTRACT
   else
      ogr2ogr -update -append $SCRATCH/$TIMESTAMP_TO_EXTRACT.gpkg $f -nln $TIMESTAMP_TO_EXTRACT
   fi
done

ogrinfo $SCRATCH/$TIMESTAMP_TO_EXTRACT.gpkg -sql "ALTER TABLE $TIMESTAMP_TO_EXTRACT DROP COLUMN id"
cp -f $SCRATCH/$TIMESTAMP_TO_EXTRACT.gpkg $OUTDIR

aws s3 cp $OUTDIR/$TIMESTAMP_TO_EXTRACT.gpkg s3://aws.c-gsr-pg-pyregence-bucket/daily_perimeters_collated/ --profile gsr

rm -f -r $SCRATCH

exit 0
