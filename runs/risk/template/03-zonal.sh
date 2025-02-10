#!/bin/bash

CLOUDFIRE_VER=${CLOUDFIRE_VER:-2025.0209}
ZONAL_STATS=$CLOUDFIRE_BASE_DIR/code/linux/bin/zonal_stats_$CLOUDFIRE_VER

PATTERN=`basename $(pwd) | cut -d- -f2`
DATADIR=$(pwd)/post
OUTDIR=$(pwd)/polygon_zonal_stats
SCRATCH=$ELMFIRE_SCRATCH_BASE/scratch_elmfire_zonal_$PATTERN
POLYGONS=$ELMFIRE_BASE_DIR/config/polygons/${PATTERN}_polygons.shp
SRS="EPSG:5070"
TR_NATIVE='150 150'
TR_RESAMPLED='150 150'
NUM_FORECAST_HOURS_TO_PROCESS=108

f=`ls ./post/times-burned*.tif | head -n1`
XMIN=`gdalinfo $f | grep "Lower Left" | cut -d'(' -f2 | cut -d')' -f1 | cut -d, -f1 | xargs`
YMIN=`gdalinfo $f | grep "Lower Left" | cut -d'(' -f2 | cut -d')' -f1 | cut -d, -f2 | xargs`
XMAX=`gdalinfo $f | grep "Upper Right" | cut -d'(' -f2 | cut -d')' -f1 | cut -d, -f1 | xargs`
YMAX=`gdalinfo $f | grep "Upper Right" | cut -d'(' -f2 | cut -d')' -f1 | cut -d, -f2 | xargs`
TE="$XMIN $YMIN $XMAX $YMAX"

HAVE_PLIGNRATE=no
#if [ "$PATTERN" = "$UTILITY01" ] || [ "$PATTERN" = "$UTILITY02" ]; then
#   HAVE_PLIGNRATE=yes
#fi

if [ "$HAVE_PLIGNRATE" = "yes" ]; then
   NUM_QUANTITIES=4
   QUANTITIES='fire-area fire-volume impacted-structures plignrate'
   QUANTITIES_FOR_ZONAL_STATS='"fire-area", "fire-volume", "impacted-structures", "plignrate"'
else
   NUM_QUANTITIES=3
   QUANTITIES='fire-area fire-volume impacted-structures'
   QUANTITIES_FOR_ZONAL_STATS='"fire-area", "fire-volume", "impacted-structures"'
fi

fhs=`seq 7 114`
for fh in $fhs; do
   if [ -z "$FORECAST_HOURS_TO_PROCESS" ] ; then
      FORECAST_HOURS_TO_PROCESS=$fh
   else
      FORECAST_HOURS_TO_PROCESS="$FORECAST_HOURS_TO_PROCESS,$fh"
   fi
done

rm -f -r $SCRATCH
mkdir $SCRATCH $OUTDIR 2> /dev/null

NF=`ogrinfo -al -so $POLYGONS | grep Feature | cut -d: -f2 | xargs`
let "NFM1 = NF - 1"

# Begin by figuring out if we have to create raster polygons
if [ ! -e ./zones/zones_$PATTERN.tif ]; then
   mkdir ./zones 2> /dev/null
   ogr2ogr -t_srs "$SRS" $SCRATCH/polygons.shp $POLYGONS
   for FID in $(eval echo "{0..$NFM1}"); do
      ogr2ogr -t_srs "$SRS" -fid $FID $SCRATCH/polygon_$FID.shp $POLYGONS
      rm -f $SCRATCH/intermediate.tif
      gdal_rasterize -init 0 -a_nodata -9999 -ot Int16 -te $TE -tr $TR_RESAMPLED -burn 1 -at $SCRATCH/polygon_$FID.shp $SCRATCH/intermediate.tif
      gdalwarp -multi -r average -tr $TR_NATIVE -ot Float32 $SCRATCH/intermediate.tif $SCRATCH/zones_${PATTERN}_$FID.tif
      FNLIST="$FNLIST $SCRATCH/zones_${PATTERN}_$FID.tif"
   done
   gdal_merge.py -separate -a_nodata -9999 -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -o ./zones/zones_$PATTERN.tif $FNLIST
   rm -f $FNLIST
fi

# Write timestamps for later use:
START_YYYYMMDD=`basename $(pwd) | cut -d- -f1 | cut -d_ -f1`
START_HH=`basename $(pwd) | cut -d- -f1 | cut -d_ -f2`
FORECAST_CYCLE=${START_YYYYMMDD}_$START_HH

echo "Processing $FORECAST_CYCLE"
for fh in $fhs; do
   FH=`printf %03d $fh`
   date -u -d "$START_YYYYMMDD $START_HH:00 UTC + $fh hours" +"%Y-%m-%d %H:%M" >> $SCRATCH/timestamps.txt
   TIMESTAMP=`date -u -d "$START_YYYYMMDD $START_HH:00 UTC + $fh hours" +"%Y%m%d_%H%M00"`
   for QUANTITY in $QUANTITIES; do
      gdal_translate -ot Float32 $DATADIR/${QUANTITY}_$TIMESTAMP.tif $SCRATCH/${FORECAST_CYCLE}_${FH}_$QUANTITY.tif &
   done
done
wait

echo "&ZONAL_STATS_INPUTS"                                             > $SCRATCH/zonal_stats_inputs.data
echo "RASTER_DIRECTORY = '"$SCRATCH"/'"                               >> $SCRATCH/zonal_stats_inputs.data
echo "NUM_QUANTITIES = $NUM_QUANTITIES"                               >> $SCRATCH/zonal_stats_inputs.data
echo "QUANTITIES = $QUANTITIES_FOR_ZONAL_STATS"                       >> $SCRATCH/zonal_stats_inputs.data
echo "NUM_FORECAST_HOURS_TO_PROCESS = $NUM_FORECAST_HOURS_TO_PROCESS" >> $SCRATCH/zonal_stats_inputs.data
echo "FORECAST_HOURS_TO_PROCESS = $FORECAST_HOURS_TO_PROCESS"         >> $SCRATCH/zonal_stats_inputs.data
echo "ZONES_FN = './zones/zones_"$PATTERN"'"                          >> $SCRATCH/zonal_stats_inputs.data
echo "OUTPUTS_DIRECTORY = '"$SCRATCH"/'"                              >> $SCRATCH/zonal_stats_inputs.data
echo "FORECAST_CYCLE = '"${FORECAST_CYCLE}"'"                         >> $SCRATCH/zonal_stats_inputs.data
echo "PATH_TO_GDAL = '/usr/bin/'"                                     >> $SCRATCH/zonal_stats_inputs.data
echo "SCRATCH = '"$SCRATCH"/'"                                        >> $SCRATCH/zonal_stats_inputs.data
echo "/"                                                              >> $SCRATCH/zonal_stats_inputs.data
$ZONAL_STATS $SCRATCH/zonal_stats_inputs.data

#cp $SCRATCH/zonal_stats_inputs.data ./

for QUANTITY in $QUANTITIES; do
   for FID in $(eval echo "{0..$NFM1}"); do
      paste -d, $SCRATCH/timestamps.txt $SCRATCH/${QUANTITY}_${FORECAST_CYCLE}0000_$FID.csv > $OUTDIR/${QUANTITY}_${FORECAST_CYCLE}0000_$FID.csv
   done
done

rm -f -r $SCRATCH

exit 0
