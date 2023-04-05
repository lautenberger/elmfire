#!/bin/bash

NUM_CYCLES_PER_DAY=4
USE_SLURM=yes

if [ -z "$1" ] || [ -z "$2" ]; then
   echo "Specify pattern and fuel as command line argument"
   exit 1
fi

mkdir runs 2> /dev/null

# Link polygons
POLYGON_DIR_SRC=$CLOUDFIRE_BASE_DIR/config/polygons
POLYGON_DIR_TRG=$ELMFIRE_BASE_DIR/config/polygons
if [ -d $POLYGON_DIR_SRC ]; then
   for f in $POLYGON_DIR_SRC/*; do
      BASENAME=`basename $f`
      ln -fs $POLYGON_DIR_SRC/$BASENAME $POLYGON_DIR_TRG/$BASENAME
   done
fi

# Link ignition patterns
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

rm -f -r ./runs/$FORECAST_CYCLE-$PATTERN-$FUELS
mkdir ./runs/$FORECAST_CYCLE-$PATTERN-$FUELS

echo $PATTERN
echo $FUELS
echo $FORECAST_CYCLE

cd template
if [ "$USE_SLURM" = "yes" ]; then
   sbatch --priority=4000000000 --job-name=$PATTERN$FUELS$FORECAST_CYCLE \
          --chdir=$(pwd) \
          --output=$ELMFIRE_BASE_DIR/runs/risk/runs/$FORECAST_CYCLE-$PATTERN-$FUELS/log_01-go_%j.txt \
          --export=ALL,PATTERN=$PATTERN,FUELS_INPUTS=$FUELS,FORECAST_CYCLE=$FORECAST_CYCLE,CLOUDFIRE_SERVER=$CLOUDFIRE_SERVER,ELMFIRE_SCRATCH_BASE=$ELMFIRE_SCRATCH_BASE 01-go.sh
else
   ./01-go.sh $FORECAST_CYCLE $PATTERN $FUELS >& $ELMFIRE_BASE_DIR/runs/risk/runs/$FORECAST_CYCLE-$PATTERN-$FUELS/log_01-go.txt
fi

exit 0
