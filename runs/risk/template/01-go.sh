#!/bin/bash
#SBATCH --nodes=1               # node count
#SBATCH --ntasks=1              # total number of tasks across all nodes
#SBATCH --cpus-per-task=64      # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --priority=4000000000
#SBATCH --exclusive
#SBATCH --exclude=

function link_tiles {
   local QUANTITY=$1
   local DATADIR=$2
   local I=$3
   local J=$4
   local SUBDIR=$5

   i=$((10#$I)) ; let "im1 = i - 1"; let "ip1 = i + 1"
   j=$((10#$J)) ; let "jm1 = j - 1"; let "jp1 = j + 1"

   for i in $(eval echo "{$im1..$ip1}"); do
      I=`printf "%03d" $i`
      let "ilocal = i + 1 - im1"
      for j in $(eval echo "{$jm1..$jp1}"); do
         J=`printf "%03d" $j`
         let "jlocal = j + 1 - jm1"
         gdal_translate -of ENVI -co "INTERLEAVE=BSQ" $DATADIR/${I}_${J}/$QUANTITY.tif $SCRATCH/${QUANTITY}_${ilocal}_${jlocal}.bsq
      done
   done
}

echo "PATTERN $PATTERN"
echo "FUELS_INPUTS $FUELS_INPUTS"
echo "FORECAST_CYCLE $FORECAST_CYCLE"
echo "CLOUDFIRE_SERVER $CLOUDFIRE_SERVER"
echo "ELMFIRE_SCRATCH_BASE $ELMFIRE_SCRATCH_BASE"

TILEFILE=$ELMFIRE_BASE_DIR/config/tiles_conus_aea.csv

DATA_COPY_METHOD=cp # cp, scp, wget
TILES_FCST_CLI=$ELMFIRE_BASE_DIR/cloudfire/tiles_fcst.py
SCRATCH=$ELMFIRE_SCRATCH_BASE/risk_run

FORECAST_CYCLE=${FORECAST_CYCLE:-$1}
PATTERN=${PATTERN:-$1}
FUELS_INPUTS=${FUELS_INPUTS:-$1}

TODAY=`echo $FORECAST_CYCLE | cut -d_ -f1`
LFMDATE=`date -u -d "$TODAY - 1 day" +%Y%m%d`
CWD=$(pwd)

# Check that we have 0 or 3 command line arguments:
if [ "$#" != "0" ] && [ "$#" != "3" ]; then
   echo "Specify zero or three command line arguments"
   exit 1
fi

# First command line argument is FORECAST_CYCLE, or infer it from directory name:
if [ -z "$FORECAST_CYCLE" ]; then
   FORECAST_CYCLE=`basename $(pwd) | cut -d- -f1`
fi

if [ ${#FORECAST_CYCLE} -ne "11" ]; then
   echo "Specify date and cycle in form YYYYMMDD_CC or run this script from a directory named"
   echo "YYYYMMDD_CC-pattern"
   exit 1
fi

# Second command line argument is PATTERN, or infer it from directory name:
if [ -z "$PATTERN" ]; then
   PATTERN=`basename $(pwd) | cut -d- -f2`
fi

# Third command line argument is FUELS_INPUTS, or infer it from directory name:
if [ -z "$FUELS_INPUTS" ]; then
   FUELS_INPUTS=`basename $(pwd) | cut -d- -f3`
fi

if [ "$FUELS_INPUTS" != "landfire" ] ; then
   echo 'Set third command line argument to "landfire"'
   exit 1
fi

TILELIST=$ELMFIRE_BASE_DIR/config/ignition/patterns/$PATTERN/tiles/tiles.txt
RUNDIR=$ELMFIRE_BASE_DIR/runs/risk/runs/$FORECAST_CYCLE-$PATTERN-$FUELS_INPUTS
INTERMEDIATE_OUTPUTS_BASE_DIR=$ELMFIRE_SCRATCH_BASE/$FORECAST_CYCLE-$PATTERN-$FUELS_INPUTS

INPUTS_DIR=$RUNDIR/inputs

rm -f -r $INPUTS_DIR
mkdir -p $SCRATCH $INTERMEDIATE_OUTPUTS_BASE_DIR $RUNDIR $INPUTS_DIR 2> /dev/null

# Start by getting data from cloudfire

while read TILE ; do
   TILES="$TILES $TILE"
done < $TILELIST

echo "TILES: $TILES"

for TILE in $TILES; do
   I=`echo $TILE | cut -d_ -f1`
   J=`echo $TILE | cut -d_ -f2`

   i=$((10#$I)) ; let "im1 = i - 1"; let "ip1 = i + 1"
   j=$((10#$J)) ; let "jm1 = j - 1"; let "jp1 = j + 1"

   for i in $(eval echo "{$im1..$ip1}"); do
      I=`printf "%03d" $i`
      for j in $(eval echo "{$jm1..$jp1}"); do
         J=`printf "%03d" $j`
         echo ${I}_${J} >> $SCRATCH/tiles_with_halo_and_duplicates.txt
      done
   done
done
cat $SCRATCH/tiles_with_halo_and_duplicates.txt | sort | uniq > $SCRATCH/tiles_with_halo.txt
rm $SCRATCH/tiles_with_halo_and_duplicates.txt

while read TILE; do
   mkdir $RUNDIR/inputs/$TILE
   echo "$TILES_FCST_CLI $PATTERN $FORECAST_CYCLE $LFMDATE $TILE $DATA_COPY_METHOD"
   read -d '' WXLOC FUELLOC TOPOLOC STRUCTDENSLOC IGNLOC LFMLOC <<<$($TILES_FCST_CLI $PATTERN $FORECAST_CYCLE $LFMDATE $TILE $DATA_COPY_METHOD)

   case $DATA_COPY_METHOD in

      'cp')
         cp -f $WXLOC/*         $RUNDIR/inputs/$TILE/
         cp -f $FUELLOC/*       $RUNDIR/inputs/$TILE/
         cp -f $TOPOLOC/*       $RUNDIR/inputs/$TILE/
         cp -f $STRUCTDENSLOC/* $RUNDIR/inputs/$TILE/
         cp -f $IGNLOC/*        $RUNDIR/inputs/$TILE/
         cp -f $LFMLOC/*        $RUNDIR/inputs/$TILE/
         ;;

      'scp')
         scp $WXLOC/*         $RUNDIR/inputs/$TILE/
         scp $FUELLOC/*       $RUNDIR/inputs/$TILE/
         scp $TOPOLOC/*       $RUNDIR/inputs/$TILE/
         scp $STRUCTDENSLOC/* $RUNDIR/inputs/$TILE/
         scp $IGNLOC/*        $RUNDIR/inputs/$TILE/
         scp $LFMLOC/*        $RUNDIR/inputs/$TILE/
         ;;

      'wget')
         wget -r -np -nH --cut-dirs=2 -R "index.html*" -P $RUNDIR/inputs/$TILE/ $WXLOC/
         wget -r -np -nH --cut-dirs=2 -R "index.html*" -P $RUNDIR/inputs/$TILE/ $FUELLOC/
         wget -r -np -nH --cut-dirs=2 -R "index.html*" -P $RUNDIR/inputs/$TILE/ $TOPOLOC/
         wget -r -np -nH --cut-dirs=2 -R "index.html*" -P $RUNDIR/inputs/$TILE/ $STRUCTDENSLOC/
         wget -r -np -nH --cut-dirs=3 -R "index.html*" -P $RUNDIR/inputs/$TILE/ $IGNLOC/
         wget -r -np -nH --cut-dirs=2 -R "index.html*" -P $RUNDIR/inputs/$TILE/ $LFMLOC/
         ;;

      *)
         echo -n "Bad data copy method"
         exit 1
         ;;

   esac

   if [ "$PATTERN" = "$UTILITY02" ]; then
      PATTERN_SHORT=`echo $PATTERN | cut -d_ -f1`
      mv $RUNDIR/inputs/$TILE/plignrate_$PATTERN_SHORT.tif $RUNDIR/inputs/$TILE/plignrate_$PATTERN.tif
   fi

done < $SCRATCH/tiles_with_halo.txt
rm -f $SCRATCH/tiles_with_halo.txt

# Done getting data from cloudfire - now loop over tiles

for TILE in $TILES; do

   echo $TILE
   rm -f -r $RUNDIR/$TILE $SCRATCH
   mkdir $RUNDIR/$TILE $SCRATCH

   if [ "$PATTERN" = "all" ]; then
      cp -f $CWD/empty_run/elmfire.data.all $SCRATCH/elmfire.data
   else
      cp -f $CWD/empty_run/elmfire.data.powerlines $SCRATCH/elmfire.data
   fi
   cp -f $CWD/empty_run/*.sh $SCRATCH
   cp -f $CWD/empty_run/kernel_density.data $SCRATCH
   cp -f $ELMFIRE_BASE_DIR/build/source/fuel_models.csv $SCRATCH

   cd $SCRATCH

   I=`echo $TILE | cut -d_ -f1`
   J=`echo $TILE | cut -d_ -f2`

   for QUANTITY in asp dem slp cbd cbh cc ch fbfm40; do
      link_tiles $QUANTITY $INPUTS_DIR $I $J fuels_and_topography &
   done

   link_tiles ignition_mask              $INPUTS_DIR $I $J fuels_and_topography &
   link_tiles structure_density_per_acre $INPUTS_DIR $I $J fuels_and_topography &

   for QUANTITY in erc m100 m10 m1 wd ws lh lw plignrate_$PATTERN; do
      link_tiles $QUANTITY $INPUTS_DIR $I $J weather &
   done

   wait

   for A in ./fbfm40*.bsq; do
      ROW=`basename $A | cut -d_ -f2`
      COL=`basename $A | cut -d_ -f3 | cut -d. -f1`
      gdal_calc.py -A $A --type="Float32" --NoDataValue=-9999 --format="ENVI" --co="INTERLEAVE=BSQ" --calc="A*0.0 + 1.0" --outfile="./adj_${ROW}_${COL}.bsq" &
      gdal_calc.py -A $A --type="Float32" --NoDataValue=-9999 --format="ENVI" --co="INTERLEAVE=BSQ" --calc="A*0.0 + 1.0" --outfile="./phi_${ROW}_${COL}.bsq" &
   done
   wait

# The .bsq.aux.xml file created by gdal_calc doesn't work
   for i_j in 1_1 1_2 1_3 2_1 2_2 2_3 3_1 3_2 3_3; do
      for QUANTITY in adj phi; do
         rm -f ${QUANTITY}_$i_j.bsq.aux.xml
         cp -f ignition_mask_$i_j.bsq.aux.xml ${QUANTITY}_$i_j.bsq.aux.xml
      done
   done

# Set XLLCORNER and YLLCORNER:
   XLLCORNER=`gdalinfo ./slp_1_1.bsq | grep 'Lower Left' | cut -d'(' -f2 | cut -d')' -f1 | cut -d, -f1 | xargs`
   YLLCORNER=`gdalinfo ./slp_1_1.bsq | grep 'Lower Left' | cut -d'(' -f2 | cut -d')' -f1 | cut -d, -f2 | xargs`

   LINENUM=`grep -n "COMPUTATIONAL_DOMAIN_XLLCORNER" ./elmfire.data | cut -d: -f1`
   sed -i "$LINENUM d" ./elmfire.data
   sed -i "$LINENUM i COMPUTATIONAL_DOMAIN_XLLCORNER = $XLLCORNER" ./elmfire.data

   LINENUM=`grep -n "COMPUTATIONAL_DOMAIN_YLLCORNER" ./elmfire.data | cut -d: -f1`
   sed -i "$LINENUM d" ./elmfire.data
   sed -i "$LINENUM i COMPUTATIONAL_DOMAIN_YLLCORNER = $YLLCORNER" ./elmfire.data

# Grid declination
   LINE=`cat $TILEFILE | grep $TILE`
   GRID_DECL=`echo $LINE | cut -d, -f 12`

# Set GRID_DECLINATION
   LINENUM=`grep -n "GRID_DECLINATION" elmfire.data | cut -d: -f1`
   sed -i "$LINENUM d" elmfire.data
   sed -i "$LINENUM i GRID_DECLINATION = $GRID_DECL" elmfire.data

   wait

   ./01-run.sh $PATTERN $FUELS_INPUTS $FORECAST_CYCLE $TILE >& run.log
   cd $CWD

done

cp -f $CWD/02-post.sh $INTERMEDIATE_OUTPUTS_BASE_DIR
if [ "$PATTERN" = "all" ]; then
   cp -f $CWD/0304-rasterize.sh $INTERMEDIATE_OUTPUTS_BASE_DIR
else
   cp -f $CWD/03-zonal.sh           $INTERMEDIATE_OUTPUTS_BASE_DIR
   cp -f $CWD/04-attribute_table.sh $INTERMEDIATE_OUTPUTS_BASE_DIR
fi
cp -f $CWD/05-upload.sh $INTERMEDIATE_OUTPUTS_BASE_DIR

cd $INTERMEDIATE_OUTPUTS_BASE_DIR

./02-post.sh >& log_02-post.txt

if [ "$PATTERN" = "all" ]; then
   ./0304-rasterize.sh >& log_0304-rasterize.txt
else
   ./03-zonal.sh           >& log_03-zonal.txt
   ./04-attribute_table.sh >& log_04-attribute_table.txt
fi

./05-upload.sh >& log_05-upload.txt

cp -f -r * $RUNDIR

exit 0
