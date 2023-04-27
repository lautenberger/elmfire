#!/bin/bash

SRS=`gdalsrsinfo ./asp.tif  | grep "PROJ.4" | cut -d':' -f2 | xargs`
#SOCKETS=`lscpu | grep 'Socket(s)' | cut -d: -f2 | xargs`
#CORES_PER_SOCKET=`lscpu | grep 'Core(s) per socket' | cut -d: -f2 | xargs`
#let "N = SOCKETS * CORES_PER_SOCKET"

compress () {
   local f=$1
   local OT=$2
   local STUB=`echo $f | cut -d. -f1`

   gdal_translate -a_srs "$SRS" -a_nodata 0 -ot $OT -co "TILED=yes" -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -co "NUM_THREADS=2" $STUB.bil $STUB.tif && \
   rm -f $STUB.bil $STUB.hdr
}

#(
#for f in *.bil; do
#   ((i=i%N)); ((i++==0)) && wait
#   compress "$f" &
#done
#)

#QUANTITIES='crown-fire flame-length hours-since-burned spread-rate'
for f in time*.bil              ; do compress "$f" Float32 & done
for f in crown-fire*.bil        ; do compress "$f" Byte    & done
wait
for f in flame-length*.bil      ; do compress "$f" Byte    & done
wait
for f in hours-since-burned*.bil; do compress "$f" Byte    & done
wait
for f in spread-rate*.bil       ; do compress "$f" Byte    & done
wait

wait

exit 0
