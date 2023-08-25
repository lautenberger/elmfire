#!/bin/bash

rm -f ./matchdrop.sh

function set_defaults {
   IGNITION_TIME=${IGNITION_TIME:-'null'}
   WEST_BUFFER=${WEST_BUFFER:-'12'}
   SOUTH_BUFFER=${SOUTH_BUFFER:-'12'}
   EAST_BUFFER=${EAST_BUFFER:-'12'}
   NORTH_BUFFER=${NORTH_BUFFER:-'12'}
   NUM_ENSEMBLE_MEMBERS=${NUM_ENSEMBLE_MEMBERS:-'100'}
   ADD_TO_ACTIVE_FIRES=${ADD_TO_ACTIVE_FIRES:-'no'}
   RUN_HOURS=${RUN_HOURS:-'24'}
   IGNITION_RADIUS=${IGNITION_RADIUS:-'300'}
   FUEL_SOURCE=${FUEL_SOURCE:-'landfire'}
   FUEL_VERSION=${FUEL_VERSION:-'2.3.0_2.2.0'}
}

function set_matchdrop_script {
   sed -i "s/dummy_firename/$FIRENAME/g" matchdrop.sh
   sed -i "s/dummy_ignition_time/$IGNITION_TIME/g" matchdrop.sh
   sed -i "s/dummy_ignition_lon/$IGNITION_LON/g" matchdrop.sh
   sed -i "s/dummy_ignition_lat/$IGNITION_LAT/g" matchdrop.sh
   sed -i "s/dummy_center_lon/$CENTER_LON/g" matchdrop.sh
   sed -i "s/dummy_center_lat/$CENTER_LAT/g" matchdrop.sh
   sed -i "s/dummy_west_buffer/$WEST_BUFFER/g" matchdrop.sh
   sed -i "s/dummy_south_buffer/$SOUTH_BUFFER/g" matchdrop.sh
   sed -i "s/dummy_east_buffer/$EAST_BUFFER/g" matchdrop.sh
   sed -i "s/dummy_north_buffer/$NORTH_BUFFER/g" matchdrop.sh
   sed -i "s/dummy_num_ensemble_members/$NUM_ENSEMBLE_MEMBERS/g" matchdrop.sh
   sed -i "s/dummy_add_to_active_fires/$ADD_TO_ACTIVE_FIRES/g" matchdrop.sh
   sed -i "s/dummy_run_hours/$RUN_HOURS/g" matchdrop.sh
   sed -i "s/dummy_ignition_radius/$IGNITION_RADIUS/g" matchdrop.sh
   sed -i "s/dummy_fuel_source/$FUEL_SOURCE/g" matchdrop.sh
   sed -i "s/dummy_fuel_version/$FUEL_VERSION/g" matchdrop.sh
}
