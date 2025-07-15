function replace_line {
   MATCH_PATTERN=$1
   NEW_VALUE="$2"
   IS_STRING=$3

   LINE=`grep -n "$MATCH_PATTERN" elmfire.data | cut -d: -f1`
   sed -i "$LINE d" elmfire.data
   if [ "$IS_STRING" = "yes" ]; then
      sed -i "$LINE i $MATCH_PATTERN = '$NEW_VALUE'" elmfire.data
   else
      sed -i "$LINE i $MATCH_PATTERN = $NEW_VALUE" elmfire.data
   fi
}

function parse_control_file {
   local CONTROL_FILE=$1
   local ECHO_OUTPUTS=no

   PARAMETERS='FIRENAME INITIALIZATION_TIME INITIALIZATION_TYPE IGNITION_LAT IGNITION_LON ACTIVE_FIRE_TIMESTAMP ALREADY_BURNED_TIMESTAMP RUN_HOURS NORTH_BUFFER EAST_BUFFER WEST_BUFFER SOUTH_BUFFER IGNITION_RADIUS SETUP_CALIBRATION_POLYGONS CALIBRATION_POLYGONS_DIRECTORY SETUP_BARRIER_POLYGONS BARRIER_POLYGONS_DIRECTORY NUM_ENSEMBLE_MEMBERS USE_SPECIFIC_WX_CYCLE FUEL_SOURCE FUEL_VERSION RUN_TEMPLATE DO_WUI'

   while read LINE; do
      if [ ${LINE:0:1} = "#" ]; then
         continue
      fi
      for PARAMETER in $PARAMETERS; do
         VALUE=`echo $LINE | grep $PARAMETER | cut -d= -f2 | cut -d'#' -f1`
         if [ -z "$VALUE" ]; then
            continue
         fi
         if [ "$PARAMETER" = "FIRENAME"                       ]; then FIRENAME=$VALUE                       ; fi
         if [ "$PARAMETER" = "INITIALIZATION_TIME"            ]; then INITIALIZATION_TIME=$VALUE            ; fi
         if [ "$PARAMETER" = "INITIALIZATION_TYPE"            ]; then INITIALIZATION_TYPE=$VALUE            ; fi
         if [ "$PARAMETER" = "IGNITION_LAT"                   ]; then IGNITION_LAT=$VALUE                   ; fi
         if [ "$PARAMETER" = "IGNITION_LON"                   ]; then IGNITION_LON=$VALUE                   ; fi
         if [ "$PARAMETER" = "ACTIVE_FIRE_TIMESTAMP"          ]; then ACTIVE_FIRE_TIMESTAMP=$VALUE          ; fi
         if [ "$PARAMETER" = "ALREADY_BURNED_TIMESTAMP"       ]; then ALREADY_BURNED_TIMESTAMP=$VALUE       ; fi
         if [ "$PARAMETER" = "RUN_HOURS"                      ]; then RUN_HOURS=$VALUE                      ; fi
         if [ "$PARAMETER" = "NORTH_BUFFER"                   ]; then NORTH_BUFFER=$VALUE                   ; fi
         if [ "$PARAMETER" = "EAST_BUFFER"                    ]; then EAST_BUFFER=$VALUE                    ; fi
         if [ "$PARAMETER" = "WEST_BUFFER"                    ]; then WEST_BUFFER=$VALUE                    ; fi
         if [ "$PARAMETER" = "SOUTH_BUFFER"                   ]; then SOUTH_BUFFER=$VALUE                   ; fi
         if [ "$PARAMETER" = "IGNITION_RADIUS"                ]; then IGNITION_RADIUS=$VALUE                ; fi
         if [ "$PARAMETER" = "SETUP_CALIBRATION_POLYGONS"     ]; then SETUP_CALIBRATION_POLYGONS=$VALUE     ; fi
         if [ "$PARAMETER" = "CALIBRATION_POLYGONS_DIRECTORY" ]; then CALIBRATION_POLYGONS_DIRECTORY=$VALUE ; fi
         if [ "$PARAMETER" = "SETUP_BARRIER_POLYGONS"         ]; then SETUP_BARRIER_POLYGONS=$VALUE         ; fi
         if [ "$PARAMETER" = "BARRIER_POLYGONS_DIRECTORY"     ]; then BARRIER_POLYGONS_DIRECTORY=$VALUE     ; fi
         if [ "$PARAMETER" = "NUM_ENSEMBLE_MEMBERS"           ]; then NUM_ENSEMBLE_MEMBERS=$VALUE           ; fi
         if [ "$PARAMETER" = "USE_SPECIFIC_WX_CYCLE"          ]; then USE_SPECIFIC_WX_CYCLE=$VALUE          ; fi
         if [ "$PARAMETER" = "FUEL_SOURCE"                    ]; then FUEL_SOURCE=$VALUE                    ; fi
         if [ "$PARAMETER" = "FUEL_VERSION"                   ]; then FUEL_VERSION=$VALUE                   ; fi
         if [ "$PARAMETER" = "RUN_TEMPLATE"                   ]; then RUN_TEMPLATE=$VALUE                   ; fi
         if [ "$PARAMETER" = "DO_WUI"                         ]; then DO_WUI=$VALUE                         ; fi
      done
   done < $CONTROL_FILE

   FIRENAME=`echo "$FIRENAME" | xargs`
   INITIALIZATION_TIME=`echo "$INITIALIZATION_TIME" | xargs`
   INITIALIZATION_TYPE=`echo "$INITIALIZATION_TYPE" | xargs`
   IGNITION_LAT=`echo "$IGNITION_LAT" | xargs`
   IGNITION_LON=`echo "$IGNITION_LON" | xargs`
   ACTIVE_FIRE_TIMESTAMP=`echo "$ACTIVE_FIRE_TIMESTAMP" | xargs`
   ALREADY_BURNED_TIMESTAMP=`echo "$ALREADY_BURNED_TIMESTAMP" | xargs`
   RUN_HOURS=`echo "$RUN_HOURS" | xargs`
   NORTH_BUFFER=`echo "$NORTH_BUFFER" | xargs`
   EAST_BUFFER=`echo "$EAST_BUFFER" | xargs`
   WEST_BUFFER=`echo "$WEST_BUFFER" | xargs`
   SOUTH_BUFFER=`echo "$SOUTH_BUFFER" | xargs`
   IGNITION_RADIUS=`echo "$IGNITION_RADIUS" | xargs`
   SETUP_CALIBRATION_POLYGONS=`echo "$SETUP_CALIBRATION_POLYGONS" | xargs`
   CALIBRATION_POLYGONS_DIRECTORY=`echo "$CALIBRATION_POLYGONS_DIRECTORY" | xargs`
   SETUP_BARRIER_POLYGONS=`echo "$SETUP_BARRIER_POLYGONS" | xargs`
   BARRIER_POLYGONS_DIRECTORY=`echo "$BARRIER_POLYGONS_DIRECTORY" | xargs`
   NUM_ENSEMBLE_MEMBERS=`echo "$NUM_ENSEMBLE_MEMBERS" | xargs`
   USE_SPECIFIC_WX_CYCLE=`echo "$USE_SPECIFIC_WX_CYCLE" | xargs`
   FUEL_SOURCE=`echo "$FUEL_SOURCE" | xargs`
   FUEL_VERSION=`echo "$FUEL_VERSION" | xargs`
   RUN_TEMPLATE=`echo "$RUN_TEMPLATE" | xargs`
   DO_WUI=`echo "$DO_WUI" | xargs`

   FIRENAME=${FIRENAME:-'null'}
   INITIALIZATION_TIME=${INITIALIZATION_TIME:-'null'}
   INITIALIZATION_TYPE=${INITIALIZATION_TYPE:-'null'}
   IGNITION_LAT=${IGNITION_LAT:--9999.}
   IGNITION_LON=${IGNITION_LON:--9999.}
   ACTIVE_FIRE_TIMESTAMP=${ACTIVE_FIRE_TIMESTAMP:-'null'}
   ALREADY_BURNED_TIMESTAMP=${ALREADY_BURNED_TIMESTAMP:-'null'}
   RUN_HOURS=${RUN_HOURS:-24}
   NORTH_BUFFER=${NORTH_BUFFER:-30000.}
   EAST_BUFFER=${EAST_BUFFER:-30000.}
   WEST_BUFFER=${WEST_BUFFER:-30000.}
   SOUTH_BUFFER=${SOUTH_BUFFER:-30000.}
   IGNITION_RADIUS=${IGNITION_RADIUS:-300.}
   SETUP_CALIBRATION_POLYGONS=${SETUP_CALIBRATION_POLYGONS:-'no'}
   CALIBRATION_POLYGONS_DIRECTORY=${CALIBRATION_POLYGONS_DIRECTORY:-'null'}
   SETUP_BARRIER_POLYGONS=${SETUP_BARRIER_POLYGONS:-'no'}
   BARRIER_POLYGONS_DIRECTORY=${BARRIER_POLYGONS_DIRECTORY:-'null'}
   NUM_ENSEMBLE_MEMBERS=${NUM_ENSEMBLE_MEMBERS:-200}
   USE_SPECIFIC_WX_CYCLE=${USE_SPECIFIC_WX_CYCLE:-'no'}
   FUEL_SOURCE=${FUEL_SOURCE:-'landfire'}
   FUEL_VERSION=${FUEL_VERSION:-'2.2.0'}
   RUN_TEMPLATE=${RUN_TEMPLATE:-'default'}
   DO_WUI=${DO_WUI:-'no'}

   if [ "$ECHO_OUTPUTS" = "yes" ]; then
      echo "FIRENAME                       : $FIRENAME"
      echo "INITIALIZATION_TIME            : $INITIALIZATION_TIME"
      echo "INITIALIZATION_TYPE            : $INITIALIZATION_TYPE"
      echo "IGNITION_LAT                   : $IGNITION_LAT"
      echo "IGNITION_LON                   : $IGNITION_LON"
      echo "ACTIVE_FIRE_TIMESTAMP          : $ACTIVE_FIRE_TIMESTAMP"
      echo "ALREADY_BURNED_TIMESTAMP       : $ALREADY_BURNED_TIMESTAMP"
      echo "RUN_HOURS                      : $RUN_HOURS"
      echo "NORTH_BUFFER                   : $NORTH_BUFFER"
      echo "EAST_BUFFER                    : $EAST_BUFFER"
      echo "WEST_BUFFER                    : $WEST_BUFFER"
      echo "SOUTH_BUFFER                   : $SOUTH_BUFFER"
      echo "IGNITION_RADIUS                : $IGNITION_RADIUS"
      echo "SETUP_CALIBRATION_POLYGONS     : $SETUP_CALIBRATION_POLYGONS"
      echo "CALIBRATION_POLYGONS_DIRECTORY : $CALIBRATION_POLYGONS_DIRECTORY"
      echo "SETUP_BARRIER_POLYGONS         : $SETUP_BARRIER_POLYGONS"
      echo "BARRIER_POLYGONS_DIRECTORY     : $BARRIER_POLYGONS_DIRECTORY"
      echo "NUM_ENSEMBLE_MEMBERS           : $NUM_ENSEMBLE_MEMBERS"
      echo "USE_SPECIFIC_WX_CYCLE          : $USE_SPECIFIC_WX_CYCLE"
      echo "RUN_TEMPLATE                   : $RUN_TEMPLATE"
      echo "DO_WUI                         : $DO_WUI"
   fi

}
