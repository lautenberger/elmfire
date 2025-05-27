#!/bin/bash

echo "In 02-write_ctl_file.sh"

WEST_BUFFER_KM=`echo $1 | cut -d, -f1`
SOUTH_BUFFER_KM=`echo $1 | cut -d, -f2`
EAST_BUFFER_KM=`echo $1 | cut -d, -f3`
NORTH_BUFFER_KM=`echo $1 | cut -d, -f4`

FIRENAME=`echo $2 | cut -d, -f1`
INITIALIZATION_TIME_FROM_FILE=`echo $2 | cut -d, -f2`
INITIALIZATION_TIME=`date -u -d "$INITIALIZATION_TIME_FROM_FILE" '+%Y-%m-%d %H:%M UTC'`
START_TIME_FOR_FILENAME=`date -u -d "$INITIALIZATION_TIME" '+%Y%m%d_%H%M00'`
IGNITION_LON=`echo $2 | cut -d, -f3`
IGNITION_LAT=`echo $2 | cut -d, -f4`
INITIALIZATION_TYPE=`echo $2 | cut -d, -f5`
ADD_TO_ACTIVE_FIRES=`echo $2 | cut -d, -f6`
RUN_HOURS=`echo $2 | cut -d, -f7`
IGNITION_RADIUS=`echo $2 | cut -d, -f8`
NUM_ENSEMBLE_MEMBERS=`echo $2 | cut -d, -f9`
ACTIVE_FIRE_TIMESTAMP=`echo $2 | cut -d, -f10`
ALREADY_BURNED_TIMESTAMP=`echo $2 | cut -d, -f11`
FUEL_SOURCE=`echo $2 | cut -d, -f12`
FUEL_VERSION=`echo $2 | cut -d, -f13`
RUN_TEMPLATE=`echo $2 | cut -d, -f14`
DO_WUI=`echo $2 | cut -d, -f15`

mkdir ./control_files ./control_files/$FIRENAME 2> /dev/null
CONTROL_FILE=./control_files/$FIRENAME/${FIRENAME}_$START_TIME_FOR_FILENAME.ctl
rm -f $CONTROL_FILE

echo "FIRENAME                   = $FIRENAME"                 >> $CONTROL_FILE
echo "INITIALIZATION_TIME        = $INITIALIZATION_TIME"      >> $CONTROL_FILE
echo "IGNITION_LON               = $IGNITION_LON"             >> $CONTROL_FILE
echo "IGNITION_LAT               = $IGNITION_LAT"             >> $CONTROL_FILE
echo "IGNITION_RADIUS            = $IGNITION_RADIUS"          >> $CONTROL_FILE
echo "INITIALIZATION_TYPE        = $INITIALIZATION_TYPE"      >> $CONTROL_FILE
echo "RUN_HOURS                  = $RUN_HOURS"                >> $CONTROL_FILE
echo "NORTH_BUFFER_KM            = $NORTH_BUFFER_KM"          >> $CONTROL_FILE
echo "EAST_BUFFER_KM             = $EAST_BUFFER_KM"           >> $CONTROL_FILE
echo "WEST_BUFFER_KM             = $WEST_BUFFER_KM"           >> $CONTROL_FILE
echo "SOUTH_BUFFER_KM            = $SOUTH_BUFFER_KM"          >> $CONTROL_FILE
echo "NUM_ENSEMBLE_MEMBERS       = $NUM_ENSEMBLE_MEMBERS"     >> $CONTROL_FILE
echo "USE_SPECIFIC_WX_CYCLE      = no"                        >> $CONTROL_FILE
echo "SETUP_CALIBRATION_POLYGONS = no"                        >> $CONTROL_FILE
echo "SETUP_BARRIER_POLYGONS     = no"                        >> $CONTROL_FILE
echo "ADD_TO_ACTIVE_FIRES        = $ADD_TO_ACTIVE_FIRES"      >> $CONTROL_FILE
echo "ACTIVE_FIRE_TIMESTAMP      = $ACTIVE_FIRE_TIMESTAMP"    >> $CONTROL_FILE
echo "ALREADY_BURNED_TIMESTAMP   = $ALREADY_BURNED_TIMESTAMP" >> $CONTROL_FILE
echo "FUEL_SOURCE                = $FUEL_SOURCE"              >> $CONTROL_FILE
echo "FUEL_VERSION               = $FUEL_VERSION"             >> $CONTROL_FILE
echo "RUN_TEMPLATE               = $RUN_TEMPLATE"             >> $CONTROL_FILE
echo "DO_WUI                     = $DO_WUI"                   >> $CONTROL_FILE

exit 0
