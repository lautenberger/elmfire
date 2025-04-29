#!/bin/bash

#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=28
#SBATCH --mem=60G
#SBATCH -t 1:00:00

# Functions:
compress () {
   local f=$1
   local CLIP=$2
   local N=$3
   local STUB=`echo $f | cut -d. -f1`
   if [ "$CLIP" = "yes" ]; then
      taskset -a -c $N gdal_translate -a_srs "$SRS" -r average -tr $TR -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -srcwin $SRCWIN $STUB.bil $STUB.tif >& /dev/null
   else
      taskset -a -c $N gdal_translate -a_srs "$SRS" -r average -tr $TR -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" $STUB.bil $STUB.tif >& /dev/null
   fi
   rm -f $STUB.bil $STUB.hdr
}

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
         if [ "$QUANTITY" = "adj" ] && [ "$USE_SNODAS" = "yes" ]; then
            gdal_calc.py --format='ENVI' --co="INTERLEAVE=BSQ" --NoDataValue=-9999 -A $DATADIR/${I}_${J}/sd.tif --calc="1.0 - (A>1.0)" \
                         --outfile=$SCRATCH/${QUANTITY}_${ilocal}_${jlocal}.bsq >& /dev/null
         else
            gdal_translate -of ENVI -co "INTERLEAVE=BSQ" $DATADIR/${I}_${J}/$QUANTITY.tif $SCRATCH/${QUANTITY}_${ilocal}_${jlocal}.bsq >& /dev/null
         fi
      done
   done
}

date
START_SEC=`date +%s`

# These variables will be passed by slurm or as command-line arguments
# for non-slurm runs
PATTERN=${PATTERN:-$1}
FUELS_INPUTS=${FUELS_INPUTS:-$2}
FORECAST_CYCLE=${FORECAST_CYCLE:-$3}
ELMFIRE_SCRATCH_BASE=${ELMFIRE_SCRATCH_BASE:-$4}
TILE=${TILE:-$5}
NUM_CORES_PER_TASK=${NUM_CORES_PER_TASK:-$6}
PARALLELIZATION_STRATEGY=${PARALLELIZATION_STRATEGY:-$7}
USE_SNODAS=${USE_SNODAS:-$8}

echo "PATTERN:                  $PATTERN"
echo "FUELS_INPUTS:             $FUELS_INPUTS"
echo "FORECAST_CYCLE:           $FORECAST_CYCLE"
echo "ELMFIRE_SCRATCH_BASE:     $ELMFIRE_SCRATCH_BASE"
echo "TILE:                     $TILE"
echo "NUM_CORES_PER_TASK:       $NUM_CORES_PER_TASK"
echo "PARALLELIZATION_STRATEGY: $PARALLELIZATION_STRATEGY"
echo "USE_SNODAS:               $USE_SNODAS"
echo "CWD:                      $(pwd)"
echo "host:                     $(hostname)"

# Directories
cd $ELMFIRE_SCRATCH_BASE # Unnecessary if using --chdir with sbatch
CWD=$(pwd)
OUTDIR=$ELMFIRE_BASE_DIR/runs/risk/runs/$FORECAST_CYCLE-$PATTERN-$FUELS_INPUTS/$TILE
INPUTS_DIR=$ELMFIRE_BASE_DIR/runs/risk/runs/$FORECAST_CYCLE-$PATTERN-$FUELS_INPUTS/inputs
SCRATCH=$ELMFIRE_SCRATCH_BASE/$FORECAST_CYCLE-$PATTERN-$FUELS_INPUTS
ELMFIRE_INSTALL_DIR=${ELMFIRE_INSTALL_DIR:-$ELMFIRE_BASE_DIR/build/linux/bin}

# Configuration
ELMFIRE_VER=${ELMFIRE_VER:-2025.0429}
TIMEOUT=5400
SRS='EPSG:5070'
TR='150 150'
SRCWIN='1600 1600 1600 1600'
MPIRUN=`which mpirun`
if [ -z "$NUM_CORES_PER_TASK" ]; then
   SOCKETS=`lscpu | grep 'Socket(s)' | cut -d: -f2 | xargs`
   CORES_PER_SOCKET=`lscpu | grep 'Core(s) per socket' | cut -d: -f2 | xargs`
   let "NP = SOCKETS * CORES_PER_SOCKET"
else
   NP=$NUM_CORES_PER_TASK
fi
ELMFIRE=$ELMFIRE_INSTALL_DIR/elmfire_$ELMFIRE_VER

# Begin run setup
mkdir -p $SCRATCH
cd $SCRATCH

cp -f $OUTDIR/*.sh   ./
cp -f $OUTDIR/*.data ./
cp -f $OUTDIR/*.csv  ./

echo "Linking inputs"
I=`echo $TILE | cut -d_ -f1`
J=`echo $TILE | cut -d_ -f2`

for QUANTITY in asp dem slp cbd cbh cc ch fbfm40; do
   link_tiles $QUANTITY $INPUTS_DIR $I $J fuels_and_topography &
done

link_tiles ignition_mask              $INPUTS_DIR $I $J fuels_and_topography &
link_tiles structure_density_per_acre $INPUTS_DIR $I $J fuels_and_topography &

if [ "$USE_SNODAS" = "yes" ]; then
   link_tiles adj $INPUTS_DIR $I $J fuels_and_topography &
fi

for QUANTITY in m100 m10 m1 wd ws lh lw; do
   link_tiles $QUANTITY $INPUTS_DIR $I $J weather &
done

wait
echo "Done linking inputs"

for A in ./fbfm40*.bsq; do
   ROW=`basename $A | cut -d_ -f2`
   COL=`basename $A | cut -d_ -f3 | cut -d. -f1`
   if [ "$USE_SNODAS" != "yes" ]; then
      gdal_calc.py -A $A --type="Float32" --NoDataValue=-9999 --format="ENVI" --co="INTERLEAVE=BSQ" --calc="A*0.0 + 1.0" --outfile="./adj_${ROW}_${COL}.bsq" >& /dev/null &
   fi
   gdal_calc.py -A $A --type="Float32" --NoDataValue=-9999 --format="ENVI" --co="INTERLEAVE=BSQ" --calc="A*0.0 + 1.0" --outfile="./phi_${ROW}_${COL}.bsq" >& /dev/null &
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
LINENUM=`grep -n "GRID_DECLINATION" elmfire.data | cut -d: -f1`
sed -i "$LINENUM d" elmfire.data
sed -i "$LINENUM i GRID_DECLINATION = $GRID_DECL" elmfire.data

# Run elmfire
echo "Running elmfire"
echo "localhost slots=$NP" > ./hosts
timeout ${TIMEOUT}s $MPIRUN --mca btl tcp,self --bind-to core --oversubscribe -np $NP -machinefile hosts $ELMFIRE elmfire.data >& elmfire.log

echo "Done running elmfire"
if [ "$PATTERN" != "all" ]; then
   ./02-rasterize.sh $PATTERN $NP >& rasterize.log
fi

N=0
for f in times_burned*.bil; do
   compress "$f" no $N &
   let "N=N+1"
   if [ "$N" = "$NP" ]; then
      N=0
      wait
   fi
done
wait

if [ "$PATTERN" != "all" ]; then
   ./03-shapefiles.sh $PATTERN $TILE $NP >& shapefiles.log
fi

rm -f *.bsq *.hdr *.xml

cp -f -r * $OUTDIR/

cd $OUTDIR
rm -f -r $SCRATCH

END_SEC=`date +%s`
let "ELAPSED = END_SEC - START_SEC"
echo "Elapsed time:  $ELAPSED s"

exit 0
