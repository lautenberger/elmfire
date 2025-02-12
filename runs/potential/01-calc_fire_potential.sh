#!/bin/bash

PART=$1
CWD=$(pwd)
LOCKFILE=$CWD/log/01-calc_fire_potential_part$PART.lock
if [ -e $LOCKFILE ]; then
  exit 1
fi
touch $LOCKFILE

warp_and_clip() {
   WX_RASTER=$1
   rm -f ./$WX_RASTER.tif
   gdalwarp -multi -r near -tr $WX_RES $WX_RES -t_srs "$T_SRS" -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -wo "NUM_THREADS=4" -te $TE $WX_ROOT_DIR/$FORECAST_CYCLE/${FORECAST_CYCLE}_$WX_RASTER.tif ./$WX_RASTER.tif
}

USE_SNODAS=yes
FUELS_AND_TOPOGRAPHY_ROOT_DIR=$CLOUDFIRE_BASE_DIR/inputs/fuel/2023/landfire-2.2.0_and_ca_2022_fuelscape/tiles_custom
LIVE_FUEL_MOISTURE_DIR=$CLOUDFIRE_BASE_DIR/inputs/wx/lfm/live_fuel_moisture_rasters
WX_ROOT_DIR=$CLOUDFIRE_BASE_DIR/inputs/wx/hybrid/stacked_tifs
SNODAS_DIR=$CLOUDFIRE_BASE_DIR/inputs/wx/snodas/tifs
SCRATCH=$ELMFIRE_SCRATCH_BASE/fire_potential_part$PART

WX_RASTERS='m1 m10 m100 ws wd'
WX_RES=600
FUELS_AND_TOPOGRAPHY_RASTERS='asp slp dem ch cbd cbh cc fbfm40'
OUTDIR=$CWD/part$PART
COMPLETE_LOG_PART1=$CWD/log/complete_part1.txt
COMPLETE_LOG_PART2=$CWD/log/complete_part2.txt
COMPLETE_LOG=$CWD/log/complete_part$PART.txt

ELMFIRE_VER=${ELMFIRE_VER:-2025.0212}
ELMFIRE_INSTALL_DIR=${ELMFIRE_INSTALL_DIR:-$ELMFIRE_BASE_DIR/build/linux/bin}
ELMFIRE=$ELMFIRE_INSTALL_DIR/elmfire_$ELMFIRE_VER

NP=`cat /proc/cpuinfo | grep "cpu cores" | cut -d: -f2 | tail -n 1 | xargs`
let "NP = NP / 3"

# Set TILELIST
. ./config/aoi_and_tile.sh
for (( i=1; i<=$NUM_AOI_AND_TILE; i++ )); do
   ISTHERE=`echo $TILELIST | grep ${TILE[i]} | wc -l`
   if [ "$ISTHERE" = "0" ]; then
      TILELIST="$TILELIST ${TILE[i]}"
   fi
done

# Now figure out which cycles to process:
TODAY=`date -u +%Y%m%d`
YESTERDAY=`date -u -d "$TODAY -1 day" +%Y%m%d`

# Start by figuring out which cycle to run:
if [ -z $2 ]; then
   HH=`date -u +%H`
   if [ "$HH" = "00" ]; then FORECAST_CYCLE=${YESTERDAY}_18; fi
   if [ "$HH" = "01" ]; then FORECAST_CYCLE=${YESTERDAY}_18; fi
   if [ "$HH" = "02" ]; then FORECAST_CYCLE=${YESTERDAY}_18; fi
   if [ "$HH" = "03" ]; then FORECAST_CYCLE=${YESTERDAY}_18; fi
   if [ "$HH" = "04" ]; then FORECAST_CYCLE=${YESTERDAY}_18; fi
   if [ "$HH" = "05" ]; then FORECAST_CYCLE=${TODAY}_00; fi
   if [ "$HH" = "06" ]; then FORECAST_CYCLE=${TODAY}_00; fi
   if [ "$HH" = "07" ]; then FORECAST_CYCLE=${TODAY}_00; fi
   if [ "$HH" = "08" ]; then FORECAST_CYCLE=${TODAY}_00; fi
   if [ "$HH" = "09" ]; then FORECAST_CYCLE=${TODAY}_00; fi
   if [ "$HH" = "10" ]; then FORECAST_CYCLE=${TODAY}_00; fi
   if [ "$HH" = "11" ]; then FORECAST_CYCLE=${TODAY}_06; fi
   if [ "$HH" = "12" ]; then FORECAST_CYCLE=${TODAY}_06; fi
   if [ "$HH" = "13" ]; then FORECAST_CYCLE=${TODAY}_06; fi
   if [ "$HH" = "14" ]; then FORECAST_CYCLE=${TODAY}_06; fi
   if [ "$HH" = "15" ]; then FORECAST_CYCLE=${TODAY}_06; fi
   if [ "$HH" = "16" ]; then FORECAST_CYCLE=${TODAY}_06; fi
   if [ "$HH" = "17" ]; then FORECAST_CYCLE=${TODAY}_12; fi
   if [ "$HH" = "18" ]; then FORECAST_CYCLE=${TODAY}_12; fi
   if [ "$HH" = "19" ]; then FORECAST_CYCLE=${TODAY}_12; fi
   if [ "$HH" = "20" ]; then FORECAST_CYCLE=${TODAY}_12; fi
   if [ "$HH" = "21" ]; then FORECAST_CYCLE=${TODAY}_12; fi
   if [ "$HH" = "22" ]; then FORECAST_CYCLE=${TODAY}_12; fi
   if [ "$HH" = "23" ]; then FORECAST_CYCLE=${TODAY}_18; fi
else
  FORECAST_CYCLE=$2
fi

GO=yes
ISDONE=`grep $FORECAST_CYCLE $COMPLETE_LOG 2> /dev/null | wc -l`
if [ "$ISDONE" != "0" ]; then
   GO=no
fi

if [ "$PART" = "2" ]; then
   ISDONE=`grep $FORECAST_CYCLE $COMPLETE_LOG_PART1 2> /dev/null | wc -l`
   if [ "$ISDONE" -lt "1" ]; then
      GO=no
   fi
fi

# Check if weather data is there
if [ "$GO" = "yes" ]; then
   for WX_RASTER in $WX_RASTERS; do
      IN=$WX_ROOT_DIR/$FORECAST_CYCLE/${FORECAST_CYCLE}_$WX_RASTER.tif
      if [ -e $IN ]; then
         AGE=$(( `date +%s` - `stat -L --format %Y $IN` ))
         ISOPEN=`lsof | grep $IN | wc -l`
         if [ "$AGE" -lt "5" ] || [ "$ISOPEN" != "0" ]; then
            GO=no
         fi
      else
         echo "$WX_ROOT_DIR/$FORECAST_CYCLE/${FORECAST_CYCLE}_$WX_RASTER.tif doesn't exist"
         GO=no
      fi
   done
fi

if [ "$GO" = "no" ]; then
   echo "exiting because GO=no"
   rm -f $LOCKFILE
   exit 1
fi

mkdir $OUTDIR $OUTDIR/$FORECAST_CYCLE 2> /dev/null

for TILE in $TILELIST; do

   rm -f -r $SCRATCH
   mkdir $SCRATCH $OUTDIR/$FORECAST_CYCLE/$TILE 2> /dev/null

   cp -f $CWD/empty_run/* $SCRATCH
   cp -f $ELMFIRE_BASE_DIR/build/source/fuel_models.csv $SCRATCH

   cd $SCRATCH

   for FUELS_AND_TOPOGRAPHY_RASTER in $FUELS_AND_TOPOGRAPHY_RASTERS; do
      ln -fs $FUELS_AND_TOPOGRAPHY_ROOT_DIR/$TILE/$FUELS_AND_TOPOGRAPHY_RASTER.tif ./$FUELS_AND_TOPOGRAPHY_RASTER.tif
   done

   T_SRS=`gdalsrsinfo ./$FUELS_AND_TOPOGRAPHY_RASTER.tif | grep PROJ.4 | cut -d: -f2 | xargs`
   XMIN=`gdalinfo ./$FUELS_AND_TOPOGRAPHY_RASTER.tif | grep "Lower Left" | cut -d'(' -f2 | cut -d, -f1`
   YMIN=`gdalinfo ./$FUELS_AND_TOPOGRAPHY_RASTER.tif | grep "Lower Left" | cut -d'(' -f2  | cut -d',' -f2 | cut -d')' -f1 | xargs`
   XMAX=`gdalinfo ./$FUELS_AND_TOPOGRAPHY_RASTER.tif | grep "Upper Right" | cut -d'(' -f2 | cut -d, -f1`
   YMAX=`gdalinfo ./$FUELS_AND_TOPOGRAPHY_RASTER.tif | grep "Upper Right" | cut -d'(' -f2  | cut -d',' -f2 | cut -d')' -f1 | xargs`
   TE="$XMIN $YMIN $XMAX $YMAX"

   if [ "$USE_SNODAS" = "yes" ]; then
      SNODAS=`ls $SNODAS_DIR/sd*.tif | tail -n 1`
      ln -fs $FUELS_AND_TOPOGRAPHY_ROOT_DIR/$TILE/adj.tif ./adj_nosnow.tif
      gdalwarp -multi -r bilinear -tr 30 30 -t_srs "$T_SRS" -te $TE $SNODAS ./snodas.tif
      gdal_calc.py -A ./adj_nosnow.tif -B ./snodas.tif --type=Float32 \
                   --NoDataValue=-9999 --co="COMPRESS=DEFLATE" --co="ZLEVEL=9" --outfile=./adj.tif \
                   --calc="0.0 + (B>=1.0)*0.0 + (B<1.0)*1.0"
      rm -f ./snodas.tif
   else
      ln -fs $FUELS_AND_TOPOGRAPHY_ROOT_DIR/$TILE/adj.tif ./adj.tif
   fi

   for WX_RASTER in $WX_RASTERS; do
      ((k=k%NP)); ((k++==0)) && wait
      warp_and_clip "$WX_RASTER" &
   done

   cp -f $LIVE_FUEL_MOISTURE_DIR/$TILE/part$PART/*.tif ./

# Set XLLCORNER
   LINENUM=`grep -n "COMPUTATIONAL_DOMAIN_XLLCORNER" elmfire.data | cut -d: -f1`
   sed -i "$LINENUM d" elmfire.data
   sed -i "$LINENUM i COMPUTATIONAL_DOMAIN_XLLCORNER = $XMIN" elmfire.data

# Set YLLCORNER
   LINENUM=`grep -n "COMPUTATIONAL_DOMAIN_YLLCORNER" elmfire.data | cut -d: -f1`
   sed -i "$LINENUM d" elmfire.data
   sed -i "$LINENUM i COMPUTATIONAL_DOMAIN_YLLCORNER = $YMIN" elmfire.data

# Set METEOROLOGY_BAND_STOP
   LINENUM=`grep -n "METEOROLOGY_BAND_STOP " elmfire.data | cut -d: -f1`
   sed -i "$LINENUM d" elmfire.data
   if [ "$PART" = "1" ]; then
      sed -i "$LINENUM i METEOROLOGY_BAND_STOP  = 121" elmfire.data
   else
      sed -i "$LINENUM i METEOROLOGY_BAND_STOP  = 24" elmfire.data
   fi

   wait

   mpirun -np $NP nice -n 19 $ELMFIRE elmfire.data
   ./make_tifs.sh $PART

   for QUANTITY in head_fire_flame_length ws wd m1; do
      cp -f $QUANTITY*.tif $OUTDIR/$FORECAST_CYCLE/$TILE/
   done
   cd $CWD
done

echo $FORECAST_CYCLE >> $COMPLETE_LOG

rm -f -r $SCRATCH $LOCKFILE

exit 0
