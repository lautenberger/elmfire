#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --exclusive
#SBATCH --exclude=
#SBATCH --mem=250G
#SBATCH -t 30:00:00

function wait_for_slurm_jobs {
   local JOB_IDS="$1"
   ANYRUNNING=yes
   while [ "$ANYRUNNING" = "yes" ]; do
      NOW=`date -u +"%Y-%m-%d %H:%M:%S"`
      echo "$NOW Checking if slurm jobs are finished"
      ANYRUNNING=no
      for JOB_ID in $JOB_IDS; do
         ISRUNNING=`squeue -j $JOB_ID 2> /dev/null | wc -l`
         if [ "$ISRUNNING" != "0" ]; then
            ANYRUNNING=yes
         fi
      done
      sleep 30
   done
}

PATTERN=${PATTERN:-$1}
FUELS_INPUTS=${FUELS_INPUTS:-$2}
FORECAST_CYCLE=${FORECAST_CYCLE:-$3}
NUM_CORES_PER_TASK=${NUM_CORES_PER_TASK:-$4}
PARALLELIZATION_STRATEGY=${PARALLELIZATION_STRATEGY:-$5}

echo "PATTERN:                  $PATTERN"
echo "FUELS_INPUTS:             $FUELS_INPUTS"
echo "FORECAST_CYCLE:           $FORECAST_CYCLE"
echo "NUM_CORES_PER_TASK:       $NUM_CORES_PER_TASK"
echo "PARALLELIZATION_STRATEGY: $PARALLELIZATION_STRATEGY"
echo "CLOUDFIRE_SERVER:         $CLOUDFIRE_SERVER"
echo "ELMFIRE_SCRATCH_BASE:     $ELMFIRE_SCRATCH_BASE"

# Configuration parameters
DATA_COPY_METHOD=cp # cp, scp, wget
USE_SNODAS=yes
CP_TIMEOUT=10

# Directories and files
CWD=$(pwd)
TILEFILE=$ELMFIRE_BASE_DIR/config/tiles_conus_aea.csv
TILES_FCST_CLI=$ELMFIRE_BASE_DIR/cloudfire/tiles_fcst.py
SCRATCH=$ELMFIRE_SCRATCH_BASE/setup_risk_run_${PATTERN}_${FUELS_INPUTS}_$FORECAST_CYCLE
RUNDIR=$ELMFIRE_BASE_DIR/runs/risk/runs/$FORECAST_CYCLE-$PATTERN-$FUELS_INPUTS
TILELIST=$ELMFIRE_BASE_DIR/config/ignition/patterns/$PATTERN/tiles/tiles.txt

## Check that we have 0 or 3 command line arguments:
#if [ "$#" != "0" ] && [ "$#" != "3" ]; then
#   echo "Specify zero or three command line arguments"
#   exit 1
#fi

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

#TODO:  Add error checking for command line arguments 4 & 5

# Start with clean scratch and inputs directory
rm -f -r $SCRATCH $RUNDIR/inputs
mkdir -p $SCRATCH $RUNDIR/inputs

# Build list of tiles to run
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

# Get input data from cloudfire microservice

TODAY=`echo $FORECAST_CYCLE | cut -d_ -f1`
LFMDATE=`date -u -d "$TODAY - 1 day" +%Y%m%d`

while read TILE; do
   mkdir -p $RUNDIR/inputs/$TILE
   echo "$TILES_FCST_CLI $PATTERN $FORECAST_CYCLE $LFMDATE $TILE $DATA_COPY_METHOD"
   read -d '' WXLOC FUELLOC TOPOLOC STRUCTDENSLOC IGNLOC LFMLOC SNODASLOC <<<$($TILES_FCST_CLI $PATTERN $FORECAST_CYCLE $LFMDATE $TILE $DATA_COPY_METHOD)

#echo "WXLOC        : $WXLOC"
#echo "FUELLOC      : $FUELLOC"
#echo "TOPOLOC      : $TOPOLOC"
#echo "STRUCTDENSLOC: $STRUCTDENSLOC"
#echo "IGNLOC       : $IGNLOC"
#echo "LFMLOC       : $LFMLOC"
#echo "SNODASLOC    : $SNODASLOC"

   case $DATA_COPY_METHOD in

      'cp')
         DONE=no
         while [ "$DONE" = "no" ]; do
            DONE=yes
            for LOC in $WXLOC $FUELLOC $TOPOLOC $STRUCTDENSLOC $IGNLOC $LFMLOC $SNODASLOC; do
               timeout $CP_TIMEOUT cp -f $LOC/* $RUNDIR/inputs/$TILE/
               if [ "$?" != "0" ]; then
                  DONE=no
               fi
            done
         done
#         cp -f $WXLOC/*         $RUNDIR/inputs/$TILE/
#         cp -f $FUELLOC/*       $RUNDIR/inputs/$TILE/
#         cp -f $TOPOLOC/*       $RUNDIR/inputs/$TILE/
#         cp -f $STRUCTDENSLOC/* $RUNDIR/inputs/$TILE/
#         cp -f $IGNLOC/*        $RUNDIR/inputs/$TILE/
#         cp -f $LFMLOC/*        $RUNDIR/inputs/$TILE/
#         cp -f $SNODASLOC/*     $RUNDIR/inputs/$TILE/
         ;;

      'scp')
         scp $WXLOC/*         $RUNDIR/inputs/$TILE/ &
         scp $FUELLOC/*       $RUNDIR/inputs/$TILE/ &
         scp $TOPOLOC/*       $RUNDIR/inputs/$TILE/ &
         scp $STRUCTDENSLOC/* $RUNDIR/inputs/$TILE/ &
         scp $IGNLOC/*        $RUNDIR/inputs/$TILE/ &
         scp $LFMLOC/*        $RUNDIR/inputs/$TILE/ &
         scp $SNODASLOC/*     $RUNDIR/inputs/$TILE/ &
         wait
         ;;

      'wget')
         wget -r -np -nH --cut-dirs=2 -R "index.html*" -P $RUNDIR/inputs/$TILE/ $WXLOC/
         wget -r -np -nH --cut-dirs=2 -R "index.html*" -P $RUNDIR/inputs/$TILE/ $FUELLOC/
         wget -r -np -nH --cut-dirs=2 -R "index.html*" -P $RUNDIR/inputs/$TILE/ $TOPOLOC/
         wget -r -np -nH --cut-dirs=2 -R "index.html*" -P $RUNDIR/inputs/$TILE/ $STRUCTDENSLOC/
         wget -r -np -nH --cut-dirs=3 -R "index.html*" -P $RUNDIR/inputs/$TILE/ $IGNLOC/
         wget -r -np -nH --cut-dirs=2 -R "index.html*" -P $RUNDIR/inputs/$TILE/ $LFMLOC/
         wget -r -np -nH --cut-dirs=2 -R "index.html*" -P $RUNDIR/inputs/$TILE/ $SNODASLOC/
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

# Done getting data from cloudfire microservice - now loop over tiles
for TILE in $TILES; do
   echo $TILE
   rm -f -r $RUNDIR/$TILE
   mkdir -p $RUNDIR/$TILE
   cd $RUNDIR/$TILE

   if [ "$PATTERN" = "all" ]; then
      cp -f $CWD/empty_run/elmfire.data.all        ./elmfire.data
   else
      cp -f $CWD/empty_run/elmfire.data.powerlines ./elmfire.data
   fi
   cp -f $CWD/empty_run/*.sh                            ./
   cp -f $CWD/empty_run/kernel_density.data             ./
   cp -f $ELMFIRE_BASE_DIR/build/source/fuel_models.csv ./

   if [ "$PARALLELIZATION_STRATEGY" = "multinode" ]; then
      JOB_ID=$(sbatch --job-name=${FORECAST_CYCLE}_${PATTERN}_$TILE --chdir=$ELMFIRE_SCRATCH_BASE \
             --output=$RUNDIR/log/01-run_${TILE}_%j.log \
             --export=ALL,PATTERN=$PATTERN,FUELS_INPUTS=$FUELS_INPUTS,FORECAST_CYCLE=$FORECAST_CYCLE,ELMFIRE_SCRATCH_BASE=$ELMFIRE_SCRATCH_BASE,TILE=$TILE,NUM_CORES_PER_TASK=$NUM_CORES_PER_TASK,PARALLELIZATION_STRATEGY=$PARALLELIZATION_STRATEGY,USE_SNODAS=$USE_SNODAS \
             01-run.sh | awk '{print $4}')
      JOB_IDS="$JOB_IDS $JOB_ID"
   else
      ./01-run.sh $PATTERN $FUELS_INPUTS $FORECAST_CYCLE $ELMFIRE_SCRATCH_BASE $TILE $NUM_CORES_PER_TASK $PARALLELIZATION_STRATEGY $USE_SNODAS >& $RUNDIR/log/01-run_$TILE.log
   fi
done

# Wait for jobs to finish up
if [ "$PARALLELIZATION_STRATEGY" = "multinode" ]; then
   wait_for_slurm_jobs "$JOB_IDS"
fi

# Run post-processing
cd $ELMFIRE_BASE_DIR/runs/risk
./02-post.sh $RUNDIR >& $RUNDIR/log/02-post.sh

# Clean up
rm -f -r $SCRATCH

exit 0
