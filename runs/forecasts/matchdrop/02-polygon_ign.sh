#!/bin/bash

FIRENAME=${1:-'null'}
ACTIVE_FIRE_TIMESTAMP=${2:-'null'}
ALREADY_BURNED_TIMESTAMP=${3:-'null'}
FUEL_SOURCE=${4:-'null'}
FUEL_VERSION=${5:-'null'}
WEST_BUFFER=${6:-30}
EAST_BUFFER=${7:-30}
SOUTH_BUFFER=${8:-30}
NORTH_BUFFER=${9:-30}

. ./common.sh

set_defaults

cp -f ./polygonign.txt ./matchdrop.sh

sed -i "s^dummy_active_fire_timestamp^$ACTIVE_FIRE_TIMESTAMP^g" matchdrop.sh
sed -i "s^dummy_already_burned_timestamp^$ALREADY_BURNED_TIMESTAMP^g" matchdrop.sh

# Now set IGNITION_TIME:
YYYYMMDD=`echo $ACTIVE_FIRE_TIMESTAMP | cut -d_ -f1`
HHMMSS=`echo $ACTIVE_FIRE_TIMESTAMP | cut -d_ -f2`
HH=${HHMMSS:0:2}
MM=${HHMMSS:2:2}
IGNITION_TIME=`date -u -d "$YYYYMMDD $HH:$MM" +"%Y-%m-%d %H:%M UTC"`

echo $IGNITION_TIME

set_matchdrop_script

chmod +x matchdrop.sh

exit 0
