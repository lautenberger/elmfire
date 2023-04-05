#!/bin/bash

SRS=`gdalsrsinfo ./asp.tif  | grep "PROJ.4" | cut -d':' -f2 | xargs`

compress () {
   local f=$1
   local OT=$2
   local STUB=`echo $f | cut -d. -f1`
   gdal_translate -a_srs "$SRS" -a_nodata 0 -ot $OT -co "TILED=yes" -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -co "NUM_THREADS=2" $STUB.bil $STUB.tif && \
   rm -f $STUB.bil $STUB.hdr
}

for f in time*.bil              ; do compress "$f" Float32 & done
wait
for f in crown-fire*.bil        ; do compress "$f" Byte    & done
wait
for f in flame-length*.bil      ; do compress "$f" Byte    & done
wait
for f in hours-since-burned*.bil; do compress "$f" Byte    & done
wait
for f in spread-rate*.bil       ; do compress "$f" Byte    & done
wait

exit 0
