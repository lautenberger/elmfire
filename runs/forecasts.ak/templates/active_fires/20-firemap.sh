#!/bin/bash

function compress {
   local f=$1
   local OT=$2
   local NODATA=$3
   local STUB=`echo $f | cut -d. -f1`

   gdal_translate -a_srs "$SRS" -a_nodata $NODATA -ot $OT -co "TILED=yes" -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -co "NUM_THREADS=2" \
                   $STUB.bil $STUB.tif && rm -f $STUB.bil $STUB.hdr
}

function process_hour {
   HOUR=$1
   TIMESTAMP=$2
   for NUM in `seq -f %07g 1 100`; do
      echo "Hour $HOUR, ensemble member $NUM"

      gdal_calc.py -A ./time-of-arrival_$NUM.tif --NoDataValue=-9999 \
                   --calc="0+(A>0)*(A<$HOUR)" \
                   --outfile=./${NUM}_$HOUR.tif 1> /dev/null
      if [ ! -e ./sum_$HOUR.tif ]; then
         cp -f ./${NUM}_$HOUR.tif ./sum_$HOUR.tif
      else
         gdal_calc.py -A ./${NUM}_$HOUR.tif -B ./sum_$HOUR.tif \
                      --NoDataValue=-9999 --calc="A+B" \
                      --outfile=./intermediate_$HOUR.tif 1> /dev/null
         mv ./intermediate_$HOUR.tif ./sum_$HOUR.tif
      fi
   done

   BURNIN=101
   gdal_edit.py -unsetnodata ./sum_$HOUR.tif
   for A in ./already_burned_float.tif ./burning.tif; do
      if [ -e $A ]; then
         gdal_calc.py -A $A -B ./sum_$HOUR.tif --calc="(A>=0.01)*$BURNIN + (A>-0.01)*(A<0.01)*B" \
            --outfile=./intermediate_$HOUR.tif 1> /dev/null
         mv ./intermediate_$HOUR.tif ./sum_$HOUR.tif
      fi
      let "BURNIN=BURNIN+1"
   done
   gdal_edit.py -a_nodata -9999 ./sum_$HOUR.tif

   gdal_translate -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" ./sum_$HOUR.tif \
                  $OUTDIR/burn-probability_$TIMESTAMP.tif
}

function unsetnodata {
   f=$1
   basename $f
   NUM=`basename $f | cut -d_ -f2 | cut -d. -f1`
   cp $f ./intermediate_$NUM.tif
   gdal_edit.py -unsetnodata ./intermediate_$NUM.tif
   gdal_calc.py -A ./intermediate_$NUM.tif --calc="numpy.maximum(A,0.0)" \
   --outfile=./`basename $f` 1> /dev/null
   gdal_edit.py -a_nodata -9999 ./`basename $f`
}

CWD=$(pwd)
DATADIR=$CWD
NPARALLEL=128
ELMFIRE_VER=${ELMFIRE_VER:-2025.0429}
ELMFIRE_POST=$ELMFIRE_BASE_DIR/build/linux/bin/elmfire_post_$ELMFIRE_VER
HOURS=`seq 1 120`
SCRATCH=$CLOUDFIRE_SCRATCH_BASE/wifire_probabilistic
OUTDIR=$CWD/burn_probability_`basename $DATADIR`

rm -f -r $SCRATCH $OUTDIR
mkdir $SCRATCH $OUTDIR

cp -f -r $DATADIR $SCRATCH/`basename $DATADIR`
cp -f -r ./elmfire_post_firemap.data $SCRATCH/`basename $DATADIR`
cd $SCRATCH/`basename $DATADIR`

NX=`cat elmfire_post.data | grep NX | cut -d= -f2 | xargs`
NY=`cat elmfire_post.data | grep NY | cut -d= -f2 | xargs`
XLLCORNER=`cat elmfire_post.data | grep XLLCORNER | cut -d= -f2 | xargs`
YLLCORNER=`cat elmfire_post.data | grep YLLCORNER | cut -d= -f2 | xargs`

sed -i "s/DUMMY_NX/$NX/g" elmfire_post_firemap.data
sed -i "s/DUMMY_NY/$NY/g" elmfire_post_firemap.data
sed -i "s/DUMMY_XLLCORNER/$XLLCORNER/g" elmfire_post_firemap.data
sed -i "s/DUMMY_YLLCORNER/$YLLCORNER/g" elmfire_post_firemap.data

#tar -xvzf binary_outputs.tar.gz uncomment for testing

$ELMFIRE_POST elmfire_post_firemap.data

SRS=`gdalsrsinfo ./asp.tif  | grep "PROJ.4" | cut -d':' -f2 | xargs`

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
for f in ./time-of-arrival*.tif; do
   unsetnodata $f &
   let "N=N+1"
   if [ "$N" = "$NPARALLEL" ]; then
      N=0
      wait
   fi
done
wait

DATE_START=`basename $(pwd) | cut -d_ -f2`
TIME_START=`basename $(pwd) | cut -d_ -f3`

YEAR=${DATE_START:0:4}
MONTH=${DATE_START:4:2}
DAY=${DATE_START:6:2}

HH=${TIME_START:0:2}
MM=${TIME_START:2:2}

TIME_ZERO=`date -u -d "$YEAR-$MONTH-$DAY $HH:00" +"%Y-%m-%d %H:00"`

N=0
for HOUR in $HOURS; do
   TIMESTAMP=`date -u -d "$TIME_ZERO UTC + $HOUR hours" +"%Y%m%d_%H0000"`
   process_hour $HOUR $TIMESTAMP &
   let "N=N+1"
   if [ "$N" = "$NPARALLEL" ]; then
      N=0
      wait
   fi
done
wait

cd $CWD

tar -cvzf ./burn_probability_`basename $DATADIR`.tgz ./burn_probability_`basename $DATADIR`
cp -f ./burn_probability_`basename $DATADIR`.tgz $ELMFIRE_BASE_DIR/runs/forecasts/rsync

rm -f -r $SCRATCH ./burn_probability_`basename $DATADIR`

exit 0
