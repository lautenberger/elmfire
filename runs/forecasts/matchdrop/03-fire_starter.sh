#!/bin/bash

cd $ELMFIRE_BASE_DIR/runs/forecasts/matchdrop

export USE_SLURM=no
IGNITION_LON=${1:-'-120.0'}
IGNITION_LAT=${2:-'40.0'}
FUEL_SOURCE='landfire'
FUEL_VERSION='2.3.0_2.2.0'

FIRENAME='firestarter'
IGNITION_TIME=`date -u +"%Y-%m-%d %H:%M UTC"`

CENTER_LON=$IGNITION_LON
CENTER_LAT=$IGNITION_LAT

echo "In 03-fire_starter.sh"

. ./common.sh

set_defaults

cp -f ./pointign.txt.firestarter ./matchdrop.sh

sed -i "s/dummy_ignition_lon/$IGNITION_LON/g" matchdrop.sh
sed -i "s/dummy_ignition_lat/$IGNITION_LAT/g" matchdrop.sh

set_matchdrop_script

chmod +x matchdrop.sh

echo "Running ./matchdrop.sh"

./matchdrop.sh

exit 0
