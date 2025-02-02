#!/bin/bash

date
START_SEC=`date +%s`

if [ -z "$1" ] || [ -z "$2" ]; then
   echo "Specify pattern and tile id as command line arguments"
   exit 1
fi
PATTERN=$1
TILE=$2
NP=$3

DO_ZONAL_STATS=yes
HAVE_PLIGNRATE=no
#if [ "$PATTERN" = "$UTILITY01" ] || [ "$PATTERN" = "$UTILITY02" ] ; then
#  HAVE_PLIGNRATE=yes
#fi

if [ "$HAVE_PLIGNRATE" = "yes" ]; then
   QUANTITIES='surface_fire_area fire_volume affected_population plignrate'
else
   QUANTITIES='surface_fire_area fire_volume affected_population'
fi

HOURS=`seq -f %04g 7 114`
VECTOR_LINES=$ELMFIRE_BASE_DIR/config/ignition/patterns/$PATTERN/tiles/$TILE/lines_split.shp
VECTOR_POLYGONS=$ELMFIRE_BASE_DIR/config/ignition/patterns/$PATTERN/tiles/$TILE/polygons_split.shp
ZONAL_STATS=$ELMFIRE_BASE_DIR/build/source/zonal_stats.py
SCRATCH=$(pwd)/scratch
if [ -z "$NP" ]; then
   SOCKETS=`lscpu | grep 'Socket(s)' | cut -d: -f2 | xargs`
   CORES_PER_SOCKET=`lscpu | grep 'Core(s) per socket' | cut -d: -f2 | xargs`
   let "NP = SOCKETS * CORES_PER_SOCKET"
fi

rm -f -r $SCRATCH
mkdir $SCRATCH

function parallel_zonal_stats() {
   local HOUR=$1
   local N=$2
   local FNLIST=''

   BASE=zonal_stats_$HOUR
   taskset -a -c $N ogr2ogr $BASE.shp $VECTOR_LINES

   for QUANTITY in $QUANTITIES; do
      FNLIST="$FNLIST ./${QUANTITY}_$HOUR.txt"
      taskset -a -c $N $ZONAL_STATS $VECTOR_POLYGONS ./${QUANTITY}_$HOUR.tif ./${QUANTITY}_$HOUR.csv
      taskset -a -c $N sed -i 's/,,,/-1.0,-1.0,-1.0,-1.0/g' ./${QUANTITY}_$HOUR.csv
      taskset -a -c $N cat ${QUANTITY}_$HOUR.csv | cut -d, -f2 > ./${QUANTITY}_$HOUR.txt
   done

   if [ "$HAVE_PLIGNRATE" = "yes" ]; then
      echo "area,volume,structures,plignrate" > $BASE.csv
      paste -d, $FNLIST >> $BASE.csv
      echo '"Real","Real","Real","Real"' > $BASE.csvt
   else
      echo "area,volume,structures" > $BASE.csv
      paste -d, $FNLIST >> $BASE.csv
      echo '"Real","Real","Real"' > $BASE.csvt
   fi

   taskset -a -c $N ogr2ogr $SCRATCH/$BASE.shp $BASE.csv
   mv -f $SCRATCH/$BASE.dbf $BASE.dbf
   rm -f $FNLIST $BASE.csvt $SCRATCH/$BASE.*
}

echo "Running zonal stats"
N=0
if [ "$DO_ZONAL_STATS" = "yes" ] ; then
   for HOUR in $HOURS; do
      parallel_zonal_stats $HOUR $N &
      let "N = N + 1"
      if [ "$N" = "$NP" ]; then
         wait
         N=0
      fi
   done
fi
wait

rm -f -r $SCRATCH

date
END_SEC=`date +%s`

let "ELAPSED_TIME = END_SEC - START_SEC"
echo "Elapsed time:  $ELAPSED_TIME s"

exit 0
