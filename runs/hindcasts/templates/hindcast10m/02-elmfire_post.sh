#!/bin/bash

CWD=$(pwd)

ELMFIRE_VER=${ELMFIRE_VER:-2025.0212}
ELMFIRE_POST=$ELMFIRE_BASE_DIR/build/linux/bin/elmfire_post_$ELMFIRE_VER
NPARALLEL=32
QUANTITIES='crown-fire flame-length spread-rate time-of-arrival'

NX=`gdalinfo asp.tif | grep "Size is" | cut -d' ' -f3 | cut -d, -f1 | xargs`
NY=`gdalinfo asp.tif | grep "Size is" | cut -d, -f2 | xargs`

SRS=`cat elmfire.data | grep A_SRS | cut -d= -f2- | xargs`
sed -i 's/\*\*\*\*\*\*\*\*\*/999999999/g' fire_size_stats.csv

echo "Setting up elmfire_post.data"
CELLSIZE=`gdalinfo asp.tif | grep "Pixel Size" | cut -d'(' -f2 | cut -d, -f1`
XLLCORNER=`gdalinfo asp.tif | grep "Lower Left" | cut -d'(' -f2 | cut -d, -f1 | xargs`
YLLCORNER=`gdalinfo asp.tif | grep "Lower Left" | cut -d',' -f2 | cut -d')' -f1 | xargs`

sed -i "s/DUMMYNX/$NX/g"               elmfire_post.data
sed -i "s/DUMMYNY/$NY/g"               elmfire_post.data
sed -i "s/DUMMYCELLSIZE/$CELLSIZE/g"   elmfire_post.data
sed -i "s/DUMMYXLLCORNER/$XLLCORNER/g" elmfire_post.data
sed -i "s/DUMMYYLLCORNER/$YLLCORNER/g" elmfire_post.data

$ELMFIRE_POST elmfire_post.data

tar -cvjf binary_outputs.tar.bz2 *.bin && rm *.bin &

N=0
for f in ./*.bil; do
   let "N=N+1"
   echo $f
   QUANTITY=`basename $f | cut -d_ -f1`

   if [ "$QUANTITY" = "time-of-arrival" ]; then
      NUM=`basename $f | cut -d_ -f2 | cut -d. -f1`
   else
      NUM=`basename $f | cut -d_ -f2`
   fi
   NUM=${NUM:4:3}
   gdalwarp -multi -dstnodata -9999 -s_srs "$SRS" -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -wo "NUM_THREADS=2" $f ./${QUANTITY}_$NUM.tif && \
   rm -f ./`basename -s .bil $f`.bil ./`basename -s .bil $f`.hdr &

   if [ "$N" = "$NPARALLEL" ]; then
      N=0
      wait
   fi
done
wait

for QUANTITY in $QUANTITIES; do
   rm -f ./$QUANTITY.tif

   for f in ./$QUANTITY*.tif; do
      echo $f
      rm -f ./intermediate.tif ./sum.tif

      gdalwarp -dstnodata 0 $f ./intermediate.tif
      gdal_edit.py -unsetnodata ./intermediate.tif

      if [ "$QUANTITY" = "time-of-arrival" ]; then
         if [ ! -e ./times-burned.tif ]; then
            gdal_calc.py -A ./intermediate.tif --calc="A>0" --outfile=./times-burned.tif
         else
            gdal_calc.py -A ./intermediate.tif -B ./times-burned.tif --calc="(A>0)+B" --outfile=./sum.tif
            mv ./sum.tif ./times-burned.tif
         fi
      else
         if [ ! -e ./$QUANTITY.tif ]; then
            mv ./intermediate.tif ./$QUANTITY.tif
         else
            gdal_calc.py -A ./$QUANTITY.tif -B ./intermediate.tif --calc="A+B" --outfile=./sum.tif
            mv ./sum.tif ./$QUANTITY.tif
         fi
      fi
   done
done

for QUANTITY in $QUANTITIES; do
   if [ "$QUANTITY" = "time-of-arrival" ]; then
      continue
   fi
   rm -f ./$QUANTITY.tif
   gdal_calc.py -A ./$QUANTITY.tif -B ./times-burned.tif --co="COMPRESS=DEFLATE" --co="ZLEVEL=9" --outfile=./$QUANTITY.tif --calc="A/maximum(B,1)"
done
cp -f times-burned.tif burn_probability.tif
gdal_edit.py -a_nodata 0 burn_probability.tif

wait

exit 0
