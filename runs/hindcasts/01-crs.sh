#!/bin/bash

echo "In 01-crs.sh"

if [ -z "$1" ]; then
   echo "Error: Specify job launch JSON strong"
   exit 1
fi

STRING="$1"
SCRATCH=$ELMFIRE_SCRATCH_BASE/crs$RANDOM

rm -f -r $SCRATCH
mkdir -p $SCRATCH

. ./98-check_args.sh --source-only

TIMESTAMP=`date +"%Y-%m-%d %H:%M:%S"`

echo "Processing data request at $TIMESTAMP UTC"

FIRENAME=`echo $STRING                 | jq -r '.fireName'`
IGNITION_TIME=`echo $STRING            | jq -r '.ignitionTime'`
IGNITION_LON=`echo $STRING             | jq -r '.ignitionLon'`
IGNITION_LAT=`echo $STRING             | jq -r '.ignitionLat'`
CENTER_LON=`echo $STRING               | jq -r '.centerLon'`
CENTER_LAT=`echo $STRING               | jq -r '.centerLat'`
WEST_BUFFER_KM=`echo $STRING           | jq -r '.westBuffer'`
SOUTH_BUFFER_KM=`echo $STRING          | jq -r '.southBuffer'`
EAST_BUFFER_KM=`echo $STRING           | jq -r '.eastBuffer'`
NORTH_BUFFER_KM=`echo $STRING          | jq -r '.northBuffer'`
NUM_ENSEMBLE_MEMBERS=`echo $STRING     | jq -r '.numEnsembleMembers'`
SCP_INPUT_DECK=`echo $STRING           | jq -r '.scpInputDeck'`
INITIALIZATION_TYPE=`echo $STRING      | jq -r '.initializationType'`
ACTIVE_FIRE_TIMESTAMP=`echo $STRING    | jq -r '.activeFireTimestamp'`
ALREADY_BURNED_TIMESTAMP=`echo $STRING | jq -r '.alreadyBurnedTimestamp'`
ADD_TO_ACTIVE_FIRES=`echo $STRING      | jq -r '.addToActiveFires'`
RUN_HOURS=`echo $STRING                | jq -r '.runHours'`
IGNITION_RADIUS=`echo $STRING          | jq -r '.ignitionRadius'`
FUEL_SOURCE=`echo $STRING              | jq -r '.fuelSource'`
FUEL_VERSION=`echo $STRING             | jq -r '.fuelVersion'`
RETURN_AFTER_QUEUE=`echo $STRING       | jq -r '.returnAfterQueue'`
RUN_TEMPLATE=`echo $STRING             | jq -r '.runTemplate'`
DO_WUI=`echo $STRING                   | jq -r '.doWUI'`

INITIALIZATION_TIME=`date -u -d "$IGNITION_TIME" '+%Y-%m-%d %H:%M UTC'`

echo "FIRENAME: $FIRENAME"
echo "IGNITION_TIME: $IGNITION_TIME"
echo "INITIALIZATION_TIME: $INITIALIZATION_TIME"
echo "IGNITION_LON: $IGNITION_LON"
echo "IGNITION_LAT: $IGNITION_LAT"
echo "CENTER_LON: $CENTER_LON"
echo "CENTER_LAT: $CENTER_LAT"
echo "WEST_BUFFER_KM: $WEST_BUFFER_KM"
echo "SOUTH_BUFFER_KM: $SOUTH_BUFFER_KM"
echo "EAST_BUFFER_KM: $EAST_BUFFER_KM"
echo "NORTH_BUFFER_KM: $NORTH_BUFFER_KM"
echo "NUM_ENSEMBLE_MEMBERS: $NUM_ENSEMBLE_MEMBERS"
echo "SCP_INPUT_DECK: $SCP_INPUT_DECK"
echo "INITIALIZATION_TYPE: $INITIALIZATION_TYPE"
echo "ACTIVE_FIRE_TIMESTAMP: $ACTIVE_FIRE_TIMESTAMP"
echo "ALREADY_BURNED_TIMESTAMP: $ALREADY_BURNED_TIMESTAMP"
echo "ADD_TO_ACTIVE_FIRES: $ADD_TO_ACTIVE_FIRES"
echo "RUN_HOURS: $RUN_HOURS"
echo "IGNITION_RADIUS: $IGNITION_RADIUS"
echo "FUEL_SOURCE: $FUEL_SOURCE"
echo "FUEL_VERSION: $FUEL_VERSION"
echo "RETURN_AFTER_QUEUE: $RETURN_AFTER_QUEUE"
echo "RUN_TEMPLATE: $RUN_TEMPLATE"
echo "DO_WUI: $DO_WUI"

STATUS=0

check_args
echo "Done checking arguments. STATUS: $STATUS"
echo "Message: $MESSAGE"

if [ "$STATUS" != "0" ]; then
   echo "ELMFIRE run is complete"
   exit 1
fi

BUFFER_SIZE="$WEST_BUFFER_KM,$SOUTH_BUFFER_KM,$EAST_BUFFER_KM,$NORTH_BUFFER_KM"
FIRE_INFO="$FIRENAME,$IGNITION_TIME,$IGNITION_LON,$IGNITION_LAT,$INITIALIZATION_TYPE,$ADD_TO_ACTIVE_FIRES,$RUN_HOURS,$IGNITION_RADIUS,$NUM_ENSEMBLE_MEMBERS,$ACTIVE_FIRE_TIMESTAMP,$ALREADY_BURNED_TIMESTAMP,$FUEL_SOURCE,$FUEL_VERSION,$RUN_TEMPLATE,$DO_WUI"
./02-write_ctl_file.sh "$BUFFER_SIZE" "$FIRE_INFO"

START_TIME_FOR_FILENAME=`date -u -d "$INITIALIZATION_TIME" '+%Y%m%d_%H%M00'`
CONTROL_FILE=./control_files/$FIRENAME/${FIRENAME}_$START_TIME_FOR_FILENAME.ctl
QUEUE_MSG=`./03-queue_run.sh $CONTROL_FILE`
echo $QUEUE_MSG

sleep 1

if [ "$RETURN_AFTER_QUEUE" != "yes" ]; then

   LINE_START=1

   if [ "$USE_SLURM" = "yes" ]; then
      JOBID=`echo $QUEUE_MSG | cut -d' ' -f4 | xargs`

      STDOUT=`scontrol show job $JOBID | grep StdOut | cut -d= -f2`
      STDERR=`scontrol show job $JOBID | grep StdErr | cut -d= -f2`

      sleep 1

      JOBSTATE=`scontrol show job $JOBID | grep JobState | cut -d= -f2 | cut -d' ' -f1`

      while [ "$JOBSTATE" != "COMPLETED" ]; do
         JOBSTATE=`scontrol show job $JOBID | grep JobState | cut -d= -f2 | cut -d' ' -f1`
         NLINES=`cat $STDOUT | wc -l`
         if [ "$NLINES" -lt "$LINE_START" ]; then
            continue
         fi
         let "NLINESP1 = NLINES + 1"

         sed -n "$LINE_START,${NLINES}p;${NLINESP1}q" $STDOUT > $SCRATCH/readme.txt
         while read LINE; do
            echo "$LINE"
         done < $SCRATCH/readme.txt

         LINE_START=$NLINESP1
         sleep 1
      done

      echo "Not active anymore"

   else
      JOB_NAME=`basename -s .ctl $CONTROL_FILE`
      LOGFILE=./log/$JOB_NAME.txt
      echo "JOB_NAME: $JOB_NAME"
      echo "LOGFILE: $LOGFILE"

      ISCOMPLETE=0
      while [ "$ISCOMPLETE" = "0" ]; do
         ISCOMPLETE=`cat $LOGFILE | grep "ELMFIRE run is complete" | wc -l`

         NLINES=`cat $LOGFILE | wc -l`
         if [ "$NLINES" -lt "$LINE_START" ]; then
            continue
         fi
         let "NLINESP1 = NLINES + 1"

         sed -n "$LINE_START,${NLINES}p;${NLINESP1}q" $LOGFILE > $SCRATCH/readme.txt
         while read LINE; do
            echo "$LINE"
         done < $SCRATCH/readme.txt

         LINE_START=$NLINESP1
         sleep 1

      done

   fi
fi

rm -f -r $SCRATCH

exit 0
