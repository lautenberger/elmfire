#!/bin/bash

SRS=`gdalsrsinfo ./asp.tif  | grep "PROJ.4" | cut -d':' -f2 | xargs`

compress () {
   local f=$1
   local OT=$2
   local NODATA=$3
   local STUB=`echo $f | cut -d. -f1`

   gdal_translate -a_srs "$SRS" -a_nodata $NODATA -ot $OT -co "TILED=yes" -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -co "NUM_THREADS=2" \
                   $STUB.bil $STUB.tif && rm -f $STUB.bil $STUB.hdr
}

for f in time*.bil              ; do compress "$f" Float32 0 & done
for f in crown-fire*.bil        ; do compress "$f" Byte  255 & done
wait
for f in flame-length*.bil      ; do compress "$f" Byte    0 & done
wait
for f in hours-since-burned*.bil; do compress "$f" Int16   0 & done
wait
for f in spread-rate*.bil       ; do compress "$f" Byte    0 & done
wait

exit 0
