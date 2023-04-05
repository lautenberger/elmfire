#!/bin/bash

FIRENAME=${1:-'nm-mar13test'}
IGNITION_LON=${2:-'-105.77'}
IGNITION_LAT=${3:-'33.32'}
IGNITION_TIME=${4:-'2023-03-13 12:00 UTC'}
FUEL_SOURCE=${5:-'landfire'}
FUEL_VERSION=${6:-'2.2.0'}

CENTER_LON=$IGNITION_LON
CENTER_LAT=$IGNITION_LAT

. ./common.sh

set_defaults

cp -f ./pointign.txt ./matchdrop.sh

sed -i "s/dummy_ignition_lon/$IGNITION_LON/g" matchdrop.sh
sed -i "s/dummy_ignition_lat/$IGNITION_LAT/g" matchdrop.sh

set_matchdrop_script

chmod +x matchdrop.sh

exit 0
