#!/bin/bash

OT=Float32
A_NODATA=-9999
NPARALLEL=`cat /proc/cpuinfo | grep "cpu cores" | cut -d: -f2 | tail -n 1 | xargs`
SRS=`gdalsrsinfo ./asp.tif  | grep "PROJ.4" | cut -d':' -f2 | xargs`

PART=$1

compress () {
   local FNIN=$1
   local STUB=`basename $FNIN | cut -d. -f1`
   local QUANTITY=${STUB::-4}
   local HHH=`echo $STUB | rev | cut -d_ -f1 | rev`
   local h=`echo $HHH | sed 's/^0*//'`
   local fh=0
   if [ "$PART" = "1" ]; then
      let "fh = h-1"
   else
      let "fh = h*3 + 120"
   fi
   local FH=$(printf "%03d" $fh)
   gdal_translate -a_srs "$SRS" -a_nodata $A_NODATA -ot $OT -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" $FNIN ./${QUANTITY}_$FH.tif
}

FNLIST=`ls ./*.bil`
N=0
for f in $FNLIST; do
   compress "$f" &
   let "N=N+1"
   if [ "$N" = "$NPARALLEL" ]; then
      wait
      N=0
   fi
done
wait

exit 0
