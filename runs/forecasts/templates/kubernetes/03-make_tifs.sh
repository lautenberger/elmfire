#!/bin/bash

SRS=`gdalsrsinfo ./asp.tif  | grep "PROJ.4" | cut -d':' -f2 | xargs`
NPARALLEL=16

compress () {
   local f=$1
   local OT=$2
   local NODATA=$3
   local STUB=`echo $f | cut -d. -f1`

   gdal_translate -a_srs "$SRS" -a_nodata $NODATA -ot $OT -co "TILED=yes" -co "COMPRESS=DEFLATE" -co "ZLEVEL=9"  \
                   $STUB.bil $STUB.tif && rm -f $STUB.bil $STUB.hdr
}

for QUANTITY in crown-fire flame-length hours-since-burned spread-rate; do
   for PERC_TWO in 01 05 20 40 60 80 85 95 99; do
      rm -f $QUANTITY*_${PERC_TWO}_*
   done
done

N=0
for f in time*.bil; do
   compress "$f" Float32 0 &
   let "N=N+1"
   if [ "$N" = "$NPARALLEL" ]; then
     N=0
     wait
   fi
done
wait

N=0
for f in crown-fire*.bil; do
   compress "$f" Byte  255 &
   let "N=N+1"
   if [ "$N" = "$NPARALLEL" ]; then
     N=0
     wait
   fi
done
wait

N=0
for f in flame-length*.bil; do
   compress "$f" Byte    0 &
   let "N=N+1"
   if [ "$N" = "$NPARALLEL" ]; then
     N=0
     wait
   fi
done
wait

N=0
for f in hours-since-burned*.bil; do
   compress "$f" Int16   0 &
   let "N=N+1"
   if [ "$N" = "$NPARALLEL" ]; then
     N=0
     wait
   fi
done
wait

N=0
for f in spread-rate*.bil; do
   compress "$f" Byte    0 &
   let "N=N+1"
   if [ "$N" = "$NPARALLEL" ]; then
     N=0
     wait
   fi
done
wait

exit 0
