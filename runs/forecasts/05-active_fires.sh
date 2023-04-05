#!/bin/bash

if [ -e ./log/05-active_fires.lock ]; then
  exit 1
fi
date
touch ./log/05-active_fires.lock

AVAILABLE_POLYGONS_CLI=$ELMFIRE_BASE_DIR/cloudfire/available_polygons.py
SEC_BETWEEN_FIRES=10800
SEC_TO_BLACK=129600
WEST_BUFFER=24
SOUTH_BUFFER=24
EAST_BUFFER=24
NORTH_BUFFER=24
NUM_ENSEMBLE_MEMBERS=200
RUN_HOURS=336
FUEL_SOURCE=landfire
FUEL_VERSION=2.2.0
RUN_FIRES_NEWER_THAN=48
RUN_TEMPLATE=active_fires

function sec_from_timestamp {
   local TIMESTAMP=$1
   YYYYMMDD=`echo $TIMESTAMP | cut -d_ -f1`
   HHMMSS=`echo $TIMESTAMP | cut -d_ -f2`
   HH=${HHMMSS:0:2}
   MM=${HHMMSS:2:2}
   date -u -d "$YYYYMMDD $HH:$MM UTC" +%s
}

CWD=$(pwd)
mkdir $CWD/input_decks 2> /dev/null

let "RUN_FIRES_NEWER_THAN = RUN_FIRES_NEWER_THAN * 3600"

ACTIVE_FIRES=`$AVAILABLE_POLYGONS_CLI --active=True --list='fires'`
for ACTIVE_FIRE in $ACTIVE_FIRES; do
   echo "Processing $ACTIVE_FIRE"
   NEWISH_AVAILABLE_TIMESTAMPS=''
   NEWISH_ALREADY_RUN_TIMESTAMPS=''
   PERHAPS_RUN_THESE_TIMESTAMPS=''
   RUN_THESE_TIMESTAMPS=''

   NOW=`date -u +%s`
   AVAILABLE_TIMESTAMPS=`$AVAILABLE_POLYGONS_CLI --active=True --list='timestamps' --firename="$ACTIVE_FIRE"`

   if [ -z "$AVAILABLE_TIMESTAMPS" ]; then
      continue
   fi
   echo "available: $AVAILABLE_TIMESTAMPS"

# Make list of already run timestamps from input_decks directory
   ALREADY_RUN_TIMESTAMPS=`cd ./input_decks && ls ${ACTIVE_FIRE}_*.tar 2> /dev/null | cut -d_ -f2-3 | cut -d. -f1 && cd ..`

# Start by eliminating timestamps that are too old:
   for TIMESTAMP in $AVAILABLE_TIMESTAMPS; do
      TIMESTAMP_SEC=`sec_from_timestamp $TIMESTAMP`
      let "AGE = NOW - TIMESTAMP_SEC"
      if [ "$AGE" -le "$RUN_FIRES_NEWER_THAN" ]; then
         NEWISH_AVAILABLE_TIMESTAMPS=$NEWISH_AVAILABLE_TIMESTAMPS" $TIMESTAMP"
      fi
   done

   for TIMESTAMP in $ALREADY_RUN_TIMESTAMPS; do
      TIMESTAMP_SEC=`sec_from_timestamp $TIMESTAMP`
      let "AGE = NOW - TIMESTAMP_SEC"
      if [ "$AGE" -le "$RUN_FIRES_NEWER_THAN" ]; then
         NEWISH_ALREADY_RUN_TIMESTAMPS=$NEWISH_ALREADY_RUN_TIMESTAMPS" $TIMESTAMP"
      fi
   done

# Now eliminate timestamps too close in time to an already run fire:
   for AVAILABLE_TIMESTAMP in $NEWISH_AVAILABLE_TIMESTAMPS; do
      AVAILABLE_TIMESTAMP_SEC=`sec_from_timestamp $AVAILABLE_TIMESTAMP`
      GO=yes
      for ALREADY_RUN_TIMESTAMP in $ALREADY_RUN_TIMESTAMPS; do
         if [ "$GO" = "no" ]; then
            continue
         fi
         ALREADY_RUN_TIMESTAMP_SEC=`sec_from_timestamp $ALREADY_RUN_TIMESTAMP`
         DIFF=`echo $(($ALREADY_RUN_TIMESTAMP_SEC-$AVAILABLE_TIMESTAMP_SEC)) | sed 's/-//'`
         if [ "$DIFF" -lt "$SEC_BETWEEN_FIRES" ]; then
            GO=no
         fi
      done
      if [ "$GO" = "yes" ]; then
         PERHAPS_RUN_THESE_TIMESTAMPS=$PERHAPS_RUN_THESE_TIMESTAMPS" $AVAILABLE_TIMESTAMP"
      fi
   done

# Now eliminate timestamps too close to themselves
   CURRENT_SEC=''
   for TIMESTAMP in $PERHAPS_RUN_THESE_TIMESTAMPS; do
      if [ -z "$CURRENT_SEC" ]; then
         RUN_THESE_TIMESTAMPS=$TIMESTAMP
         CURRENT_SEC=`sec_from_timestamp $TIMESTAMP`
         continue
      fi

      TIMESTAMP_SEC=`sec_from_timestamp $TIMESTAMP`
      DIFF=`echo $(($TIMESTAMP_SEC-$CURRENT_SEC)) | sed 's/-//'`
      if [ "$DIFF" -ge "$SEC_BETWEEN_FIRES" ]; then
         CURRENT_SEC=$TIMESTAMP_SEC
         RUN_THESE_TIMESTAMPS=$RUN_THESE_TIMESTAMPS" $TIMESTAMP"
      fi
   done

   echo "running: $ACTIVE_FIRE $RUN_THESE_TIMESTAMPS"

   for ACTIVE_FIRE_TIMESTAMP in $RUN_THESE_TIMESTAMPS; do
      CURRENT_SEC=`sec_from_timestamp $ACTIVE_FIRE_TIMESTAMP`

      ALREADY_BURNED_TIMESTAMP='null'

      for BACKWARD_TIMESTAMP in `echo $AVAILABLE_TIMESTAMPS | rev`; do
         if [ "$ALREADY_BURNED_TIMESTAMP" != "null" ]; then
            continue
         fi
         TIMESTAMP=`echo $BACKWARD_TIMESTAMP | rev`
         TIMESTAMP_SEC=`sec_from_timestamp $TIMESTAMP`
         DIFF=`echo $(($CURRENT_SEC-$TIMESTAMP_SEC))`
         if [ "$DIFF" -ge "$SEC_TO_BLACK" ]; then
            ALREADY_BURNED_TIMESTAMP=$TIMESTAMP
         fi
      done

      IGNITION_TIME=`date -d @$CURRENT_SEC +"%Y-%m-%d %H:%M UTC"`

      STRING='{
      "fireName": "'$ACTIVE_FIRE'",
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
      "scpInputDeck": "'elmfire'",
      "returnAfterQueue": "'yes'",
      "runTemplate": "'$RUN_TEMPLATE'"
      }'

      echo $STRING

      touch ./input_decks/${ACTIVE_FIRE}_$ACTIVE_FIRE_TIMESTAMP.tar

      cd $ELMFIRE_BASE_DIR/runs/forecasts
      ./01-crs.sh "$STRING"
      cd $CWD

   done #ACTIVE_FIRE_TIMESTAMP

done #ACTIVE_FIRE

rm -f ./log/05-active_fires.lock

exit 0
