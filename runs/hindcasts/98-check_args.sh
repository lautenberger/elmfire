function check_args {

# Check $FIRENAME
   if [ -z "$FIRENAME" ]; then
      STATUS=1
      MESSAGE="No fire name"
   fi

# Check $IGNITION_TIME
   if [ -z "$IGNITION_TIME" ]; then
      STATUS=1
      MESSAGE="No ignition time"
   fi

   if [ "$INITIALIZATION_TYPE" = "active_fire_polygon" ]; then
      if [ -z "$ACTIVE_FIRE_TIMESTAMP" ]; then
         STATUS=1
         MESSAGE="Active fire timestamp not specified"
      fi
   else

      if [ "$INITIALIZATION_TYPE" = "points_within_polygon" ]; then

         if [ -z "$CENTER_LON" ]; then
            CENTER_LON=$IGNITION_LON
         fi
         if [ -z "$CENTER_LAT" ]; then
            CENTER_LAT=$IGNITION_LAT
         fi

         if [ -z "$IGNITION_LON" ]; then
            IGNITION_LON=$CENTER_LON
         fi
         if [ -z "$IGNITION_LAT" ]; then
            IGNITION_LAT=$CENTER_LAT
         fi

# Check $CENTER_LON
         if [ -z "$CENTER_LON" ]; then
            STATUS=1
            MESSAGE="No center longitude"
         fi
         ISLO=`echo "$CENTER_LON < -125" | bc`
         if [ "$ISLO" = "1" ]; then
            STATUS=1
            MESSAGE="Center longitude less than -125"
         fi
         ISHI=`echo "$CENTER_LON > -66" | bc`
         if [ "$ISHI" = "1" ]; then
            STATUS=1
            MESSAGE="Center longitude greater than -66"
         fi

# Check $CENTER_LAT
         if [ -z "$CENTER_LAT" ]; then
            STATUS=1
            MESSAGE="No center latitude"
         fi
         ISLO=`echo "$CENTER_LAT < 25" | bc`
         if [ "$ISLO" = "1" ]; then
            STATUS=1
            MESSAGE="Center latitude less than 25"
         fi
         ISHI=`echo "$CENTER_LAT > 50" | bc`
         if [ "$ISHI" = "1" ]; then
            STATUS=1
            MESSAGE="Center latitude greater than 50"
         fi

# Check $IGNITION_LON
         if [ -z "$IGNITION_LON" ]; then
            STATUS=1
            MESSAGE="No ignition longitude"
         fi
         ISLO=`echo "$IGNITION_LON < -125" | bc`
         if [ "$ISLO" = "1" ]; then
            STATUS=1
            MESSAGE="Ignition longitude less than -125"
         fi
         ISHI=`echo "$IGNITION_LON > -66" | bc`
         if [ "$ISHI" = "1" ]; then
            STATUS=1
            MESSAGE="Ignition longitude greater than -66"
         fi

# Check $IGNITION_LAT
         if [ -z "$IGNITION_LAT" ]; then
            STATUS=1
            MESSAGE="No ignition latitude"
         fi
         ISLO=`echo "$IGNITION_LAT < 25" | bc`
         if [ "$ISLO" = "1" ]; then
            STATUS=1
            MESSAGE="Ignition latitude less than 25"
         fi
         ISHI=`echo "$IGNITION_LAT > 50" | bc`
         if [ "$ISHI" = "1" ]; then
            STATUS=1
            MESSAGE="Ignition latitude greater than 50"
         fi

      fi

   fi

# Check $WEST_BUFFER_KM
   if [ -z "$WEST_BUFFER_KM" ]; then
      STATUS=1 # No west buffer
      MESSAGE="No west buffer"
   fi
   ISLO=`echo "$WEST_BUFFER_KM < 1.2" | bc`
   if [ "$ISLO" = "1" ]; then
      STATUS=1
      MESSAGE="West buffer < 1.2 km"
   fi
   ISHI=`echo "$WEST_BUFFER_KM > 120" | bc`
   if [ "$ISHI" = "1" ]; then
      STATUS=1
      MESSAGE="West buffer > 120 km"
   fi

# Check $SOUTH_BUFFER_KM
   if [ -z "$SOUTH_BUFFER_KM" ]; then
      STATUS=1
      MESSAGE="No south buffer"
   fi
   ISLO=`echo "$SOUTH_BUFFER_KM < 1.2" | bc`
   if [ "$ISLO" = "1" ]; then
      STATUS=1
      MESSAGE="South buffer < 1.2 km"
   fi
   ISHI=`echo "$SOUTH_BUFFER_KM > 120" | bc`
   if [ "$ISHI" = "1" ]; then
      STATUS=1
      MESSAGE=" South buffer > 120 km"
   fi

# Check $EAST_BUFFER_KM
   if [ -z "$EAST_BUFFER_KM" ]; then
      STATUS=1
      MESSAGE="No east buffer"
   fi
   ISLO=`echo "$EAST_BUFFER_KM < 1.2" | bc`
   if [ "$ISLO" = "1" ]; then
      STATUS=1
      MESSAGE="East buffer < 1.2 km"
   fi
   ISHI=`echo "$EAST_BUFFER_KM > 120" | bc`
   if [ "$ISHI" = "1" ]; then
      STATUS=1
      MESSAGE="East buffer > 120 km"
   fi

# Check $NORTH_BUFFER_KM
   if [ -z "$NORTH_BUFFER_KM" ]; then
      STATUS=1
      MESSAGE="No north buffer"
   fi
   ISLO=`echo "$NORTH_BUFFER_KM < 1.2" | bc`
   if [ "$ISLO" = "1" ]; then
      STATUS=1
      MESSAGE="North buffer < 1.2 km"
   fi
   ISHI=`echo "$NORTH_BUFFER_KM > 120" | bc`
   if [ "$ISHI" = "1" ]; then
      STATUS=1
      MESSAGE="North buffer > 120 km"
   fi

# Check $ADD_TO_ACTIVE_FIRES
   if [ "$ADD_TO_ACTIVE_FIRES" != "no" ] && [ "$ADD_TO_ACTIVE_FIRES" != "yes" ]; then
      STATUS=1
      MESSAGE="addToActiveFires not set to 'yes' or 'no'"
   fi

# Check $SCP_INPUT_DECK
   if [ "$SCP_INPUT_DECK" != "elmfire" ] && [ "$SCP_INPUT_DECK" != "gridfire" ] && [ "$SCP_INPUT_DECK" != "none" ] && [ "$SCP_INPUT_DECK" != "both" ]; then
      STATUS=1
      MESSAGE="scpInputDeck not set to 'gridfire', 'elmfire', 'both', or 'none'"
   fi

# Check $RUN_TEMPLATE
   if [ -z "$RUN_TEMPLATE" ]; then
      RUN_TEMPLATE='default'
   fi

   if [ "$STATUS" = "0" ]; then
      MESSAGE="Valid inputs received"
   fi

}
