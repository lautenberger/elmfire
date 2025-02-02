#!/bin/bash

PARALLELIZATION_STRATEGY=multinode
NUM_CORES_PER_TASK=28
NUM_CYCLES_PER_DAY=4

function wait_for_slurm {
   local JOB_ID=$1
   ISRUNNING=1
   while [ "$ISRUNNING" != "0" ]; do
      NOW=`date -u +"%Y-%m-%d %H:%M:%S"`
      echo "$NOW waiting for slurm job $JOB_ID to finish"
      sleep 30
      ISRUNNING=`squeue -j $JOB_ID 2> /dev/null | wc -l`
   done
}

if [ -z "$1" ] || [ -z "$2" ]; then
   echo "Specify pattern and fuel as command line argument"
   exit 1
fi

CWD=$(pwd)
mkdir -p $CWD/runs 2> /dev/null

# Link polygons
echo "Linking polygons"
POLYGON_DIR_SRC=$CLOUDFIRE_BASE_DIR/config/polygons
POLYGON_DIR_TRG=$ELMFIRE_BASE_DIR/config/polygons
if [ -d $POLYGON_DIR_SRC ]; then
   for f in $POLYGON_DIR_SRC/*; do
      BASENAME=`basename $f`
      ln -fs $POLYGON_DIR_SRC/$BASENAME $POLYGON_DIR_TRG/$BASENAME
   done
fi

# Link ignition patterns
echo "Linking ignition patterns"
PATTERN_DIR_SRC=$CLOUDFIRE_BASE_DIR/inputs/ignition/config
PATTERN_DIR_TRG=$ELMFIRE_BASE_DIR/config/ignition
if [ -d "$PATTERN_DIR_SRC" ]; then
   ln -fs $PATTERN_DIR_SRC/pattern_and_fuel.txt $PATTERN_DIR_TRG/pattern_and_fuel.txt
   for DIR in `ls -d $PATTERN_DIR_SRC/patterns/*/`; do
      SUBDIR=`basename $DIR`
      if [ -e "$PATTERN_DIR_TRG/patterns/$SUBDIR/" ] || [ -d "$PATTERN_DIR_TRG/patterns/$SUBDIR/" ]; then
         continue
      else
         ln -fs $PATTERN_DIR_SRC/patterns/$SUBDIR $PATTERN_DIR_TRG/patterns/$SUBDIR
      fi
   done
else
   ln -fs $PATTERN_DIR_TRG/pattern_and_fuel.txt.elmfire $PATTERN_DIR_TRG/pattern_and_fuel.txt
fi

PATTERN_AND_FUEL=$ELMFIRE_BASE_DIR/config/ignition/pattern_and_fuel.txt
PATTERN=$1
VALID_PATTERNS=`cat $PATTERN_AND_FUEL | cut -d- -f1 | sort | uniq`
ISGOOD=`echo "$VALID_PATTERNS" | grep "$PATTERN" | wc -l`
if [ "$ISGOOD" = "0" ]; then
   echo "Invalid ignition pattern. Check $PATTERN_AND_FUEL"
   exit 1
fi

FUELS=$2
VALID_FUELS=`cat $PATTERN_AND_FUEL | cut -d- -f2 | sort | uniq`
ISGOOD=`echo "$VALID_FUELS" | grep "$FUELS" | wc -l`
if [ "$ISGOOD" = "0" ]; then
   echo 'Invalid fuel input. Check $PATTERN_AND_FUEL'
   exit 1
fi

. ./99-get_cycle.sh --source-only

if [ -z $3 ]; then
   FORECAST_CYCLE=`get_cycle $NUM_CYCLES_PER_DAY`
else
   FORECAST_CYCLE=$3
fi

DATADIR=$CWD/runs/$FORECAST_CYCLE-$PATTERN-$FUELS
rm -f -r $DATADIR
mkdir -p $DATADIR/log

echo "PATTERN:        $PATTERN"
echo "FUELS:          $FUELS"
echo "FORECAST_CYCLE: $FORECAST_CYCLE"

cd $CWD/template

if [ "$PARALLELIZATION_STRATEGY" = "multinode" ]; then
   ./01-go.sh $PATTERN $FUELS $FORECAST_CYCLE $NUM_CORES_PER_TASK $PARALLELIZATION_STRATEGY >& $DATADIR/log/01-go.log
else
   sbatch --job-name=$PATTERN$FUELS$FORECAST_CYCLE \
          --chdir=$(pwd) \
          --output=$DATADIR/log/01-go_%j.log \
          --export=ALL,PATTERN=$PATTERN,FUELS_INPUTS=$FUELS,FORECAST_CYCLE=$FORECAST_CYCLE,CLOUDFIRE_SERVER=$CLOUDFIRE_SERVER,ELMFIRE_SCRATCH_BASE=$ELMFIRE_SCRATCH_BASE,NUM_CORES_PER_TASK=$NUM_CORES_PER_TASK,PARALLELIZATION_STRATEGY=$PARALLELIZATION_STRATEGY \
          01-go.sh
fi

exit 0
