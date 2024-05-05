#!/bin/bash

FORECAST_CYCLE=`basename $(pwd) | head -c 11`
PATTERN=`basename $(pwd) | cut -d- -f2`
UPLOAD_CONFIG_FILE=$ELMFIRE_BASE_DIR/runs/risk/template/config/uploads.sh

if [ -e $UPLOAD_CONFIG_FILE ]; then
   . $UPLOAD_CONFIG_FILE
else
   case $PATTERN in
     tlines)
       REMOTE_USER=elmfire
       REMOTE_OWNER=elmfire
       REMOTE_HOST=shasta
       REMOTE_DIRECTORY=/srv/gis
       UPLOAD_PSPS_ZONAL_STATS=no
       ;;

     all)
       REMOTE_USER=elmfire
       REMOTE_OWNER=elmfire
       REMOTE_HOST=shasta
       REMOTE_DIRECTORY=/srv/gis
       UPLOAD_PSPS_ZONAL_STATS=no
       ;;

     *)
       echo "PATTERN $PATTERN not known"
       exit 1
       ;;
   esac

fi

if [ "$REMOTE_HOST" = "none" ]; then
   exit 0
fi

PATTERN_OUT=$PATTERN

# Start with times burned & split lines
ssh $REMOTE_USER@$REMOTE_HOST "sudo mkdir -p $REMOTE_DIRECTORY/fire_risk_forecast/$PATTERN_OUT/$FORECAST_CYCLE/elmfire/landfire"
if [ "$REMOTE_OWNER" = "sig-reax" ]; then
   ssh $REMOTE_USER@$REMOTE_HOST "sudo chmod -R 775 $REMOTE_DIRECTORY/fire_risk_forecast/$PATTERN_OUT/$FORECAST_CYCLE; sudo chown -R 'sig-reax:domain users' $REMOTE_DIRECTORY/fire_risk_forecast/$PATTERN_OUT/$FORECAST_CYCLE"
else
   ssh $REMOTE_USER@$REMOTE_HOST "sudo chmod -R 775 $REMOTE_DIRECTORY/fire_risk_forecast/$PATTERN_OUT/$FORECAST_CYCLE; sudo chown -R 'elmfire:domain users'  $REMOTE_DIRECTORY/fire_risk_forecast/$PATTERN_OUT/$FORECAST_CYCLE"
fi

if [ "$PATTERN_OUT" != "all" ]; then
   rm -f ./out/fire*.tif ./out/impacted*.tif ./out/crown*.tif ./out/plign*.tif
fi

FNLIST=`ls ./out/*.tif ./out/*.shp ./out/*.dbf ./out/*.prj ./out/*.shx`
GO=yes
while [ "$GO" = "yes" ]; do
   rsync -rv --timeout=10 $FNLIST $REMOTE_USER@$REMOTE_HOST:$REMOTE_DIRECTORY/fire_risk_forecast/$PATTERN_OUT/$FORECAST_CYCLE/elmfire/landfire/
   if [ "$?" = "0" ]; then
      GO=no
   fi
done

if [ "$REMOTE_OWNER" = "sig-reax" ]; then
   ssh $REMOTE_USER@$REMOTE_HOST "sudo chmod -R 775 $REMOTE_DIRECTORY/fire_risk_forecast/$PATTERN_OUT/$FORECAST_CYCLE; sudo chown -R 'sig-reax:domain users' $REMOTE_DIRECTORY/fire_risk_forecast/$PATTERN_OUT/$FORECAST_CYCLE"
else
   ssh $REMOTE_USER@$REMOTE_HOST "sudo chmod -R 775 $REMOTE_DIRECTORY/fire_risk_forecast/$PATTERN_OUT/$FORECAST_CYCLE; sudo chown -R 'elmfire:domain users'  $REMOTE_DIRECTORY/fire_risk_forecast/$PATTERN_OUT/$FORECAST_CYCLE"
fi

# Now on to zonal statistics
if [ "$UPLOAD_PSPS_ZONAL_STATS" = "yes" ]; then
   ssh $REMOTE_USER@$REMOTE_HOST "sudo mkdir -p $REMOTE_DIRECTORY/psps_zonal/$PATTERN_OUT/$FORECAST_CYCLE"
   ssh $REMOTE_USER@$REMOTE_HOST "sudo chmod -R 775 $REMOTE_DIRECTORY/psps_zonal/$PATTERN_OUT/$FORECAST_CYCLE; sudo chown -R 'sig-reax:domain users' $REMOTE_DIRECTORY/psps_zonal/$PATTERN_OUT/$FORECAST_CYCLE"
   FNLIST=`ls ./deenergization-zones/*`

   GO=yes
   while [ "$GO" = "yes" ]; do
      rsync -rv --timeout=10 $FNLIST $REMOTE_USER@$REMOTE_HOST:$REMOTE_DIRECTORY/psps_zonal/$PATTERN_OUT/$FORECAST_CYCLE/
      if [ "$?" = "0" ]; then
         GO=no
      fi
   done

   if [ "$REMOTE_OWNER" = "sig-reax" ]; then
      ssh $REMOTE_USER@$REMOTE_HOST "sudo chmod -R 775 $REMOTE_DIRECTORY/psps_zonal/$PATTERN_OUT/$FORECAST_CYCLE; sudo chown -R 'sig-reax:domain users' $REMOTE_DIRECTORY/psps_zonal/$PATTERN_OUT/$FORECAST_CYCLE"
   else
      ssh $REMOTE_USER@$REMOTE_HOST "sudo chmod -R 775 $REMOTE_DIRECTORY/psps_zonal/$PATTERN_OUT/$FORECAST_CYCLE; sudo chown -R 'elmfire:domain users'  $REMOTE_DIRECTORY/psps_zonal/$PATTERN_OUT/$FORECAST_CYCLE"
   fi

fi

exit 0
