#!/bin/bash

date

OUTDIR=out
CLOUDFIRE_VER=${CLOUDFIRE_VER:-2023.0515}
IDG=$CLOUDFIRE_BASE_DIR/code/linux/bin/ignition_density_grid_$CLOUDFIRE_VER
SCRATCH=$ELMFIRE_SCRATCH_BASE/rasterize_all
NPARALLEL=32

NCOLS=5120
NROWS=8640
CELLSIZE=150.0
XLLCORNER=-2362425.0
YLLCORNER=1185435.0
SRS="EPSG:5070"

TE_3310='-400000 -700000 600050 500000'

YYYYMMDD_START=`basename $(pwd) | cut -d_ -f1`
HH_START=`basename $(pwd) | cut -d_ -f2 | cut -d- -f1`
TIMESTAMP_START=`date -u -d "$YYYYMMDD_START $HH_START:00" +"%Y-%m-%d %H:%M"`
CWD=$(pwd)

rm -f -r $SCRATCH #$OUTDIR
mkdir $OUTDIR $SCRATCH 2> /dev/null

cp -f $ELMFIRE_BASE_DIR/etc/california.tif $SCRATCH

for DIR in `ls -d ./0*/`; do
   echo $DIR
   TILE=`basename $DIR`
   tail -n +2 ./$TILE/fire_size_stats.csv >> $SCRATCH/fire_size_stats_all.csv
done

for fh in {7..114}; do
   FH=`printf %03d $fh`
   awk -v fh=$fh -F, '$2 == fh' < $SCRATCH/fire_size_stats_all.csv > $SCRATCH/fire_size_stats_$FH.csv &
done
wait

for fh in {7..114}; do
   FH=`printf %03d $fh`
   echo $FH
   cat $SCRATCH/fire_size_stats_$FH.csv | cut -d, -f3  > $SCRATCH/x_$FH.txt &
   cat $SCRATCH/fire_size_stats_$FH.csv | cut -d, -f4  > $SCRATCH/y_$FH.txt &
   cat $SCRATCH/fire_size_stats_$FH.csv | cut -d, -f7  > $SCRATCH/area_$FH.txt &
   cat $SCRATCH/fire_size_stats_$FH.csv | cut -d, -f8  > $SCRATCH/crown_$FH.txt &
   cat $SCRATCH/fire_size_stats_$FH.csv | cut -d, -f9  > $SCRATCH/vol_$FH.txt &
   cat $SCRATCH/fire_size_stats_$FH.csv | cut -d, -f10 > $SCRATCH/pop_$FH.txt &
done
wait

for fh in {7..114}; do
   FH=`printf %03d $fh`
   echo $FH
   paste -d, $SCRATCH/x_$FH.txt $SCRATCH/y_$FH.txt $SCRATCH/area_$FH.txt  > $SCRATCH/fire-area_$FH.csv &
   paste -d, $SCRATCH/x_$FH.txt $SCRATCH/y_$FH.txt $SCRATCH/crown_$FH.txt > $SCRATCH/crown-fire-area_$FH.csv &
   paste -d, $SCRATCH/x_$FH.txt $SCRATCH/y_$FH.txt $SCRATCH/vol_$FH.txt   > $SCRATCH/fire-volume_$FH.csv &
   paste -d, $SCRATCH/x_$FH.txt $SCRATCH/y_$FH.txt $SCRATCH/pop_$FH.txt   > $SCRATCH/impacted-structures_$FH.csv &
done
wait

for QUANTITY in fire-area crown-fire-area fire-volume impacted-structures; do
#for QUANTITY in fire-area; do
   for fh in {7..114}; do
      echo $QUANTITY $FH

      FH=`printf %03d $fh`
      TIMESTAMP=`date -u -d "$TIMESTAMP_START UTC + $fh hours" +"%Y%m%d_%H%M00"`

      INPUT_FILE=$SCRATCH/$QUANTITY-$FH.data
      OUTPUT_FILE=${QUANTITY}_$TIMESTAMP

      echo '&IDG_INPUTS'                                   > $INPUT_FILE
      echo 'USE_KERNEL_DENSITY = .FALSE.'                 >> $INPUT_FILE
      echo 'STRAIGHT_AVERAGE   = .TRUE.'                  >> $INPUT_FILE
      echo 'READ_ZVAL          = .TRUE.'                  >> $INPUT_FILE
      echo 'USE_ZMAX           = .FALSE.'                 >> $INPUT_FILE
      echo 'SCALE_FACTOR       = 1.0'                     >> $INPUT_FILE
      echo 'SEARCH_RADIUS      = 0.0'                     >> $INPUT_FILE
      echo "INPUT_DIRECTORY    = '"$SCRATCH"/'"           >> $INPUT_FILE
      echo "OUTPUT_DIRECTORY   = '"$SCRATCH"/'"           >> $INPUT_FILE
      echo "OUTPUT_FILENAME    = '"$OUTPUT_FILE"'"        >> $INPUT_FILE
      echo "A_SRS              = 'EPSG:5070'"             >> $INPUT_FILE
      echo "CELLSIZE           = $CELLSIZE"               >> $INPUT_FILE
      echo "XLLCORNER          = $XLLCORNER"              >> $INPUT_FILE
      echo "YLLCORNER          = $YLLCORNER"              >> $INPUT_FILE
      echo "NCOLS              = $NCOLS"                  >> $INPUT_FILE
      echo "NROWS              = $NROWS"                  >> $INPUT_FILE
      echo "IGNITIONS_FILE_CSV = '"${QUANTITY}_$FH.csv"'" >> $INPUT_FILE
      echo 'CONVERT_TO_GEOTIFF = .FALSE.'                 >> $INPUT_FILE
      echo 'COMPRESS           = .FALSE.'                 >> $INPUT_FILE
      echo "PATH_TO_GDAL       = '/usr/bin/'"             >> $INPUT_FILE
      echo '/'                                            >> $INPUT_FILE
      $IDG $INPUT_FILE &

   done #fh
   wait

   date
   echo "Converting .bil to .tif (unsmoothed-) files"
   for f in $SCRATCH/*.bil; do
      gdal_translate -a_srs "$SRS" -co "COMPRESS=DEFLATE" -co "ZLEVEL=3" $f $SCRATCH/unsmoothed-`basename -s .bil $f`.tif >& /dev/null &
   done
   wait
   rm -f $SCRATCH/*.bil $SCRATCH/*.hdr

   date
   echo "Running gdal_fillnodata"
   cd $SCRATCH
   N=0
   for f in unsmoothed-$QUANTITY*.tif; do
      FNINT=intermediate-`basename $f | cut -d- -f2-`
      FNOUT=smoothed-`basename $f | cut -d- -f2-`
# gdal_fillnodata.py does not work for -si > 0
      gdal_fillnodata.py -md 100 -b 1 $f $FNINT && \
      gdalwarp -multi -co "COMPRESS=DEFLATE" -co "ZLEVEL=3" $FNINT $FNOUT &
      let "N=N+1"
      if [ "$N" = "$NPARALLEL" ]; then
         wait
         N=0
      fi
   done
   wait

   cd $CWD
   rm -f $SCRATCH/unsmoothed*.tif

   date
   echo "Warping to CA Albers"
   for f in $SCRATCH/smoothed-$QUANTITY*.tif; do
      gdalwarp -multi -r bilinear -t_srs "EPSG:3310" -tr 150 150 -te $TE_3310 -co "COMPRESS=DEFLATE" -co "ZLEVEL=5" $f $SCRATCH/3310-`basename $f | cut -d- -f2-` &
   done
   wait
   rm -f $SCRATCH/smoothed*.tif

   date
   echo "Burning in nonburnable mask"
   for f in $SCRATCH/3310-$QUANTITY*.tif; do
      gdal_calc.py -A $f -B $SCRATCH/california.tif --type=UInt16 --NoDataValue=0 --co="COMPRESS=DEFLATE" --co="ZLEVEL=9" \
      --co="TILED=yes" --calc="( 0 + (A>0)*A*B)" --outfile=$SCRATCH/`basename $f | cut -d- -f2-` &
   done
   wait
   rm -f $SCRATCH/3310*.tif

   cp -f $SCRATCH/$QUANTITY*.tif $OUTDIR/ &

done #QUANTITY

wait

date

rm -f -r $SCRATCH

exit 0
