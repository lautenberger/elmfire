# Sets ELMFIRE_VER from the repo-root VERSION file (the single source of truth),
# unless the caller already exported ELMFIRE_VER. Locates VERSION by walking up
# from this file's own location, so it works regardless of the caller's depth.
function set_elmfire_version {
   if [ -z "$ELMFIRE_VER" ]; then
      local d
      d=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
      while [ ! -f "$d/VERSION" ] && [ "$d" != "/" ]; do
         d=$(dirname "$d")
      done
      if [ -f "$d/VERSION" ]; then
         ELMFIRE_VER=$(tr -d '[:space:]' < "$d/VERSION")
      fi
   fi
   export ELMFIRE_VER
}

# Stacks the eight topography/fuel layers into a single multiband "landscape"
# GeoTIFF in ELMFIRE band order (1=DEM, 2=SLP, 3=ASP, 4=FBFM, 5=CC, 6=CH,
# 7=CBH, 8=CBD), as an Int16 raster, then removes the individual layers. ELMFIRE
# reads this file when LANDSCAPE_FILENAME is set and ignores the per-layer
# filenames. Weather, adjustment-factor and phi rasters are NOT landscape bands
# and are left untouched.
# Args: <inputs_dir> [fuel_basename=fbfm40] [landscape_basename=landscape]
function create_landscape {
   local DIR=${1:-./inputs}
   local FUEL=${2:-fbfm40}
   local OUT=${3:-landscape}
   local LAYERS="$DIR/dem.tif $DIR/slp.tif $DIR/asp.tif $DIR/$FUEL.tif $DIR/cc.tif $DIR/ch.tif $DIR/cbh.tif $DIR/cbd.tif"
   gdal_merge.py -separate -ot Int16 -n -9999 -init -9999 -a_nodata -9999 \
      -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -o "$DIR/$OUT.tif" $LAYERS
   rm -f $LAYERS
}

function replace_line {
   MATCH_PATTERN=$1
   NEW_VALUE="$2"
   IS_STRING=$3

   LINE=`grep -n "$MATCH_PATTERN" ./inputs/elmfire.data | cut -d: -f1`
   sed -i "$LINE d" ./inputs/elmfire.data
   if [ "$IS_STRING" = "yes" ]; then
      sed -i "$LINE i $MATCH_PATTERN = '$NEW_VALUE'" ./inputs/elmfire.data
   else
      sed -i "$LINE i $MATCH_PATTERN = $NEW_VALUE" ./inputs/elmfire.data
   fi
}

function create_transient_inputs {
   local WX_INPUTS_FILE=$1

   COLS=`head -n 1 $WX_INPUTS_FILE | tr ',' ' '`
   tail -n +2 $WX_INPUTS_FILE > $SCRATCH/wx.csv
   NUM_TIMES=`cat $SCRATCH/wx.csv | wc -l`

   ICOL=0
   for QUANTITY in $COLS; do
      let "ICOL = ICOL + 1"
      TIMESTEP=0
      FNLIST=''
      while read LINE; do
         VAL=`echo $LINE | cut -d, -f$ICOL`
         FNOUT=$SCRATCH/${QUANTITY}_$TIMESTEP.tif
         FNLIST="$FNLIST $FNOUT"
         gdal_calc.py -A $SCRATCH/float.tif --NoDataValue=-9999 --type=Float32 --outfile="$FNOUT" --calc="A + $VAL"
         let "TIMESTEP=TIMESTEP+1"
      done < $SCRATCH/wx.csv
      gdal_merge.py -separate -n -9999 -init -9999 -a_nodata -9999 -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -o $INPUTS/$QUANTITY.tif $FNLIST
   done

}
