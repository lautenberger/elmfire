#!/bin/bash

if [ -e ./log/05-active_fires.lock ]; then
  exit 1
fi
touch ./log/05-active_fires.lock

RUN_SUPPRESSED_CASE=no
ONLY_RUN_NEWEST=yes
QUEUE_ONE_SIMULATION_PER_FIRE=yes
EXCLUDE_ALASKA=yes

AVAILABLE_POLYGONS_CLI=$ELMFIRE_BASE_DIR/cloudfire/available_polygons.py
SEC_BETWEEN_FIRES=10800
SEC_TO_BLACK_DEFAULT=129600
SEC_TO_BLACK=$SEC_TO_BLACK_DEFAULT
WEST_BUFFER=24
SOUTH_BUFFER=24
EAST_BUFFER=24
NORTH_BUFFER=24
NUM_ENSEMBLE_MEMBERS=100
RUN_HOURS=336
FUEL_SOURCE=landfire
FUEL_VERSION=2.4.0_2.3.0_2.1.0_nbflip
RUN_FIRES_NEWER_THAN=96
RUN_TEMPLATE=active_fires

ACTIVE_FIRES_PYREGENCE=$CLOUDFIRE_BASE_DIR/inputs/fire/active_fire_polygons/active_fires_list/active_fires_pyregence.csv

SCRATCH=$ELMFIRE_SCRATCH_BASE/05-active_fires.sh

function diagnostic_msg {
   local NOW=`date -u +"%Y-%m-%d %H:%M:%S.%3N"`
   printf "$NOW $1\n"
}

function sec_from_timestamp {
   local TIMESTAMP=$1
   YYYYMMDD=`echo $TIMESTAMP | cut -d_ -f1`
   HHMMSS=`echo $TIMESTAMP | cut -d_ -f2`
   HH=${HHMMSS:0:2}
   MM=${HHMMSS:2:2}
   date -u -d "$YYYYMMDD $HH:$MM UTC" +%s
}

function launch_run {
   local ACTIVE_FIRE=$1
   local RUN_TEMPLATE=$2
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

   diagnostic_msg $STRING

   touch ./input_decks/${ACTIVE_FIRE}_$ACTIVE_FIRE_TIMESTAMP.tar

   cd $ELMFIRE_BASE_DIR/runs/forecasts
   ./01-crs.sh "$STRING"
   cd $CWD
}

CWD=$(pwd)
mkdir $CWD/input_decks 2> /dev/null

let "RUN_FIRES_NEWER_THAN = RUN_FIRES_NEWER_THAN * 3600"

ACTIVE_FIRES=`$AVAILABLE_POLYGONS_CLI --active=True --list='fires'`
for ACTIVE_FIRE in $ACTIVE_FIRES; do

   STATE=`echo $ACTIVE_FIRE | cut -d'-' -f1`
   if [ "$EXCLUDE_ALASKA" = "yes" ] && [ "$STATE" = "ak" ]; then
      continue
   fi

   IS_SUPPRESSED=`echo $ACTIVE_FIRE | grep suppressed | wc -l`
   if [ "$IS_SUPPRESSED" != "0" ]; then
      continue
   fi

   echo ""
   echo ""
   diagnostic_msg "Processing $ACTIVE_FIRE\n"

   HOURS_TO_BLACK=`csvgrep -c 1 $ACTIVE_FIRES_PYREGENCE -r $ACTIVE_FIRE | cut -d, -f13 | tail -n 1`
   if [ -z "$HOURS_TO_BLACK" ]; then
      SEC_TO_BLACK=$SEC_TO_BLACK_DEFAULT
   else
      let "SEC_TO_BLACK = HOURS_TO_BLACK*3600"
   fi
   diagnostic_msg "SEC_TO_BLACK: $SEC_TO_BLACK\n"

   NEWISH_AVAILABLE_TIMESTAMPS=''
   NEWISH_ALREADY_RUN_TIMESTAMPS=''
   PERHAPS_RUN_THESE_TIMESTAMPS=''
   RUN_THESE_TIMESTAMPS=''

   NOW=`date -u +%s`
   AVAILABLE_TIMESTAMPS=`$AVAILABLE_POLYGONS_CLI --active=True --list='timestamps' --firename="$ACTIVE_FIRE"`

   if [ -z "$AVAILABLE_TIMESTAMPS" ]; then
      diagnostic_msg "$ACTIVE_FIRE no AVAILABLE_TIMESTAMPS"
      continue
   fi

#   diagnostic_msg "$ACTIVE_FIRE AVAILABLE_TIMESTAMPS:\n$AVAILABLE_TIMESTAMPS"

   NEWEST_AVAILABLE_TIMESTAMP=`echo $AVAILABLE_TIMESTAMPS | rev | cut -d' ' -f1 | rev`
   diagnostic_msg "$ACTIVE_FIRE NEWEST_AVAILABLE_TIMESTAMP: $NEWEST_AVAILABLE_TIMESTAMP"

# Make list of already run timestamps from input_decks directory
   ALREADY_RUN_TIMESTAMPS=`cd ./input_decks && ls ${ACTIVE_FIRE}_*.tar 2> /dev/null | cut -d_ -f2-3 | cut -d. -f1 && cd ..`
#   diagnostic_msg "ALREADY_RUN_TIMESTAMPS:\n$ALREADY_RUN_TIMESTAMPS"

# Start by eliminating timestamps that are too old:
   for TIMESTAMP in $AVAILABLE_TIMESTAMPS; do
      TIMESTAMP_SEC=`sec_from_timestamp $TIMESTAMP`
      let "AGE = NOW - TIMESTAMP_SEC"
      if [ "$AGE" -le "$RUN_FIRES_NEWER_THAN" ]; then
         if [ -z "$NEWISH_AVAILABLE_TIMESTAMPS" ]; then
            NEWISH_AVAILABLE_TIMESTAMPS=$TIMESTAMP
         else
            NEWISH_AVAILABLE_TIMESTAMPS=$NEWISH_AVAILABLE_TIMESTAMPS" $TIMESTAMP"
         fi
      fi
   done
   if [ -z "$NEWISH_AVAILABLE_TIMESTAMPS" ]; then
      diagnostic_msg "$ACTIVE_FIRE no NEWISH_AVAILABLE_TIMESTAMPS"
      continue
   fi

#   diagnostic_msg "$ACTIVE_FIRE NEWISH_AVAILABLE_TIMESTAMPS:\n$NEWISH_AVAILABLE_TIMESTAMPS"

# Build list of already run timestamps
   for TIMESTAMP in $ALREADY_RUN_TIMESTAMPS; do
      TIMESTAMP_SEC=`sec_from_timestamp $TIMESTAMP`
      let "AGE = NOW - TIMESTAMP_SEC"
      if [ "$AGE" -le "$RUN_FIRES_NEWER_THAN" ]; then
         if [ -z "$NEWISH_ALREADY_RUN_TIMESTAMPS" ]; then
            NEWISH_ALREADY_RUN_TIMESTAMPS=$TIMESTAMP
         else
            NEWISH_ALREADY_RUN_TIMESTAMPS=$NEWISH_ALREADY_RUN_TIMESTAMPS" $TIMESTAMP"
         fi
      fi
   done

#   diagnostic_msg "$ACTIVE_FIRE NEWISH_ALREADY_RUN_TIMESTAMPS:\n$NEWISH_ALREADY_RUN_TIMESTAMPS"

# Now eliminate timestamps too close in time to an already run fire:
   for AVAILABLE_TIMESTAMP in $NEWISH_AVAILABLE_TIMESTAMPS; do
      AVAILABLE_TIMESTAMP_SEC=`sec_from_timestamp $AVAILABLE_TIMESTAMP`
      GO=yes
      for ALREADY_RUN_TIMESTAMP in $ALREADY_RUN_TIMESTAMPS; do
         if [ "$GO" = "no" ]; then
            continue
         fi
         if [ "$AVAILABLE_TIMESTAMP" = "$ALREADY_RUN_TIMESTAMP" ]; then
            GO=no
         fi
         ALREADY_RUN_TIMESTAMP_SEC=`sec_from_timestamp $ALREADY_RUN_TIMESTAMP`
         DIFF=`echo $(($ALREADY_RUN_TIMESTAMP_SEC-$AVAILABLE_TIMESTAMP_SEC)) | sed 's/-//'`
         if [ "$DIFF" -lt "$SEC_BETWEEN_FIRES" ] && [ "$AVAILABLE_TIMESTAMP" != "$NEWEST_AVAILABLE_TIMESTAMP" ]; then
            GO=no
         fi
      done
      if [ "$GO" = "yes" ]; then
         PERHAPS_RUN_THESE_TIMESTAMPS=$PERHAPS_RUN_THESE_TIMESTAMPS" $AVAILABLE_TIMESTAMP"
      fi
   done
   if [ -z "$PERHAPS_RUN_THESE_TIMESTAMPS" ]; then
      diagnostic_msg "$ACTIVE_FIRE no PERHAPS_RUN_THESE_TIMESTAMPS"
      continue
   fi

#   diagnostic_msg "$ACTIVE_FIRE PERHAPS_RUN_THESE_TIMESTAMPS:\n$PERHAPS_RUN_THESE_TIMESTAMPS"

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
      if [ "$DIFF" -ge "$SEC_BETWEEN_FIRES" ] || [ "$TIMESTAMP" = "$NEWEST_AVAILABLE_TIMESTAMP" ]; then
         CURRENT_SEC=$TIMESTAMP_SEC
         RUN_THESE_TIMESTAMPS=$RUN_THESE_TIMESTAMPS" $TIMESTAMP"
      fi
   done

   if [ -z "$RUN_THESE_TIMESTAMPS" ]; then
      diagnostic_msg "$ACTIVE_FIRE no RUN_THESE_TIMESTAMPS"
      continue
   fi

   diagnostic_msg "$ACTIVE_FIRE definitely RUN_THESE_TIMESTAMPS: $RUN_THESE_TIMESTAMPS"

   if [ "$ONLY_RUN_NEWEST" = "yes" ]; then
      RUN_THESE_TIMESTAMPS=$NEWEST_AVAILABLE_TIMESTAMP
      diagnostic_msg "$ACTIVE_FIRE only running newest timestamp: $RUN_THESE_TIMESTAMPS"
   fi

   for ACTIVE_FIRE_TIMESTAMP in $RUN_THESE_TIMESTAMPS; do
      ISTHERE=`echo $ALREADY_RUN_TIMESTAMPS | grep $ACTIVE_FIRE_TIMESTAMP | wc -l`
      if [ "$ISTHERE" != "0" ]; then
         diagnostic_msg "$ACTIVE_FIRE not running $ACTIVE_FIRE_TIMESTAMP because it's already run"
         continue
      fi

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

      DONTRUN=no
      if [ "$QUEUE_ONE_SIMULATION_PER_FIRE" = "yes" ]; then
         mkdir $SCRATCH 2> /dev/null
         FIRE_NOW=`echo $ACTIVE_FIRE | cut -d_ -f1`
         FIRE_NOW_SUPPRESSED=$FIRE_NOW-suppressed
         squeue --format="%6i,%8u,%3t,%60j" | tail -n +2 | tr -d ' ' | grep elmfire > $SCRATCH/squeue.txt
         while read LINE; do
            QUEUE_STATE=`echo $LINE | cut -d, -f3`

            if [ "$QUEUE_STATE" = "PD" ]; then
               ACTIVE_FIRE_QUEUE=`echo $LINE | cut -d, -f4`
               FIRE_QUEUE=`echo $ACTIVE_FIRE_QUEUE | cut -d_ -f1`
               if [ "$FIRE_QUEUE" = "$FIRE_NOW" ]; then
                 echo "Removing $FIRE_QUEUE from slurm queue"
                 SLURMID=`echo $LINE | cut -d, -f1`
                 scancel $SLURMID
               fi
               if [ "$FIRE_QUEUE" = "$FIRE_NOW_SUPPRESSED" ]; then
                  echo "Removing $FIRE_QUEUE from slurm queue"
                  SLURMID=`echo $LINE | cut -d, -f1`
                  scancel $SLURMID
               fi

               if [ "$QUEUE_STATE" = "R" ]; then
                  DONTRUN=yes
               fi

            fi

         done < $SCRATCH/squeue.txt
      fi

      if [ "$DONTRUN" = "yes" ]; then
         diagnostic_msg "Not running $ACTIVE_FIRE because $FIRE_NOW is already running"
         continue
      fi

      launch_run $ACTIVE_FIRE $RUN_TEMPLATE
      if [ "$RUN_SUPPRESSED_CASE" = "yes" ]; then
         sleep 5
         launch_run $ACTIVE_FIRE-suppressed $RUN_TEMPLATE-suppressed
      fi

   done #ACTIVE_FIRE_TIMESTAMP

done #ACTIVE_FIRE

rm -f -r ./log/05-active_fires.lock $SCRATCH

exit 0
