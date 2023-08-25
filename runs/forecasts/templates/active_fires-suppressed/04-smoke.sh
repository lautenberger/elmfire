#!/bin/bash

#GEOSERVER_HOSTNAME="${GEOSERVER_HOSTNAME:-shasta}"
#GEOSERVER_USERNAME="${GEOSERVER_USERNAME:-elmfire}"
#GEOSERVER_BASEDIR="${GEOSERVER_BASEDIR:-/srv/gis}"

SCRATCH=./smokescratch
PERCS='10 20 30 40 50 60 70 80 90'

# Figure out if we have smoke outputs
N=`ls smoke*.csv | wc -l`
if [ "$N" = "0" ]; then
   exit 1
fi
rm -f -r $SCRATCH
mkdir $SCRATCH

ICASES=`cat log_postprocess.txt | grep ICASE | cut -d: -f2 | xargs`

SRS=`gdalsrsinfo ./slp.tif | grep PROJ.4 | cut -d: -f2 | xargs`

FIRE_NAME=`basename $(pwd) | cut -d_ -f1`
DATE_START=`basename $(pwd) | cut -d_ -f2`
TIME_START=`basename $(pwd) | cut -d_ -f3`

WX_TIME_START=${TIME_START:0:2}:00

COUNT=0
for PERC in $PERCS; do
   let "COUNT = COUNT + 1"
   ICASE=`echo $ICASES | cut -d' ' -f$COUNT`
   SEVEN=`printf %07d $ICASE`

   FNIN=./smoke_$SEVEN.csv
   FNOUT=./hysplit_${FIRE_NAME}_${DATE_START}_${TIME_START}_$PERC.csv

   echo 'timestamp (UTC),lon,lat,area (m2),HRR (W),mdotsmoke (ug/h)' > $FNOUT

   tail -n +2 $FNIN > $SCRATCH/smoke.csv
   while read LINE; do
      SECONDS=`echo $LINE | cut -d, -f1 | cut -d. -f1`
      X=`echo $LINE | cut -d, -f2`
      Y=`echo $LINE | cut -d, -f3`
      D=`echo $LINE | cut -d, -f4`
      Q=`echo $LINE | cut -d, -f5`
      M=`echo $LINE | cut -d, -f6`

      LATLON=`echo $X $Y | gdaltransform -s_srs "$SRS" -t_srs "EPSG:4326" -output_xy`
      LON=`echo $LATLON | cut -d' ' -f1`
      LAT=`echo $LATLON | cut -d' ' -f2`
      TIMESTAMP=`date -u -d "$DATE_START $WX_TIME_START UTC + $SECONDS seconds" +"%Y-%m-%d %H:%M:%S"`
      echo "$TIMESTAMP,$LON,$LAT,$D,$Q,$M" >> $FNOUT
   done < $SCRATCH/smoke.csv
done

UPLOAD_TO_PYRECAST=no
if [ "$UPLOAD_TO_PYRECAST" = "yes" ]; then
   for f in ./hysplit*.csv ; do
      ISBAD=`cat $f | egrep '[*]' | wc -l`
      if [ "$ISBAD" = "0" ]; then
         scp $f $GEOSERVER_USERNAME@$GEOSERVER_HOSTNAME:$GEOSERVER_BASEDIR/hysplit/
      fi
   done
fi

tar -czf smoke.tgz smoke*.csv && rm -f smoke*.csv

rm -f -r $SCRATCH

exit 0
