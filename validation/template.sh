#!/bin/bash

YEAR=$1
FIRENAME=$2
ACTIVE_FIRE_TIMESTAMP_NUM=$3
ALREADY_BURNED_TIMESTAMP_NUM=$4
WEST_BUFFER=$5
SOUTH_BUFFER=$6
EAST_BUFFER=$7
NORTH_BUFFER=$8
NUM_ENSEMBLE_MEMBERS=$9
RUN_HOURS=${10}
FUEL_SOURCE=${11}
FUEL_VERSION=${12}
RUN_TEMPLATE=${13}
DO_WUI=${14}

export USE_SLURM=${USE_SLURM:-'no'}

CWD=$(pwd)

if [ -z "$FIRENAME" ]; then
   echo "Error: specify fire name"
   exit 1
fi

AVAILABLE_POLYGONS_CLI=$ELMFIRE_BASE_DIR/cloudfire/available_polygons.py

ACTIVE_FIRE_TIMESTAMP=`$AVAILABLE_POLYGONS_CLI --list 'timestamps' \
                      --active=False --year=$YEAR --firename="$FIRENAME" | \
                      head -n $ACTIVE_FIRE_TIMESTAMP_NUM`

if [ -z "$ALREADY_BURNED_TIMESTAMP_NUM" ] || [ "$ALREADY_BURNED_TIMESTAMP_NUM" = "null" ]; then
   ALREADY_BURNED_TIMESTAMP=null
else
   ALREADY_BURNED_TIMESTAMP=`$AVAILABLE_POLYGONS_CLI --list 'timestamps' \
                            --active=False --year=$YEAR --firename="$FIRENAME" | \
                            head -n $ALREADY_BURNED_TIMESTAMP_NUM`
fi

YYYYMMDD=`echo $ACTIVE_FIRE_TIMESTAMP | cut -d_ -f1`
HHMMSS=`echo $ACTIVE_FIRE_TIMESTAMP | cut -d_ -f2`
HH=${HHMMSS:0:2}
MM=${HHMMSS:2:2}
IGNITION_TIME="$YYYYMMDD $HH:$MM UTC"

ARGS='{
"fireName": "'$FIRENAME'",
"initializationType": "'active_fire_polygon'",
"activeFireTimestamp": "'$ACTIVE_FIRE_TIMESTAMP'",
"alreadyBurnedTimestamp": "'$ALREADY_BURNED_TIMESTAMP'",
"ignitionTime": "'$IGNITION_TIME'",
"westBuffer": '$WEST_BUFFER',
"southBuffer": '$SOUTH_BUFFER',
"eastBuffer": '$EAST_BUFFER',
"northBuffer": '$NORTH_BUFFER',
"numEnsembleMembers": '$NUM_ENSEMBLE_MEMBERS',
"addToActiveFires": "'no'",
"runHours": '$RUN_HOURS',
"fuelSource": "'$FUEL_SOURCE'",
"fuelVersion": "'$FUEL_VERSION'",
"scpInputDeck": "'none'",
"returnAfterQueue": "'yes'",
"runTemplate": "'$RUN_TEMPLATE'",
"doWUI": "'$DO_WUI'"
}'

rm -f -r ${FIRENAME}_$ACTIVE_FIRE_TIMESTAMP

./01-crs.sh "$ARGS"

exit 0
