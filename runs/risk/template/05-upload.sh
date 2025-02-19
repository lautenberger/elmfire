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
       REMOTE_OWNER='domain users'
       REMOTE_HOST=shasta
       REMOTE_DIRECTORY=/srv/gis
       UPLOAD_PSPS_ZONAL_STATS=no
       PATTERN_OUT=$PATTERN
       ;;

     all)
       REMOTE_USER=elmfire
       REMOTE_OWNER='domain users'
       REMOTE_HOST=shasta
       REMOTE_DIRECTORY=/srv/gis
       UPLOAD_PSPS_ZONAL_STATS=no
       PATTERN_OUT=$PATTERN
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

echo "Pattern: $PATTERN"
echo "Pattern out: $PATTERN_OUT"
echo "Remote user: $REMOTE_USER"
echo "Remote host: $REMOTE_HOST"
echo "Remote host: $REMOTE_DIRECTORY"

# Start with times burned & split lines
PARTIALPATH=$REMOTE_DIRECTORY/fire_risk_forecast/$PATTERN_OUT/$FORECAST_CYCLE
FULLPATH=$PARTIALPATH/elmfire/landfire
ssh $REMOTE_USER@$REMOTE_HOST "sudo mkdir -p $FULLPATH"
ssh $REMOTE_USER@$REMOTE_HOST "sudo chmod -R 775 $PARTIALPATH; sudo chown -R 'sig-app:domain users' $PARTIALPATH"

if [ "$PATTERN_OUT" = "all" ]; then
   FNLIST=`ls ./post/*.tif ./post/*.shp ./post/*.dbf ./post/*.prj ./post/*.shx`
else
   FNLIST=`ls ./post/times*.tif ./post/*.shp ./post/*.dbf ./post/*.prj ./post/*.shx`
fi

GO=yes
while [ "$GO" = "yes" ]; do
   rsync -rv --timeout=10 $FNLIST $REMOTE_USER@$REMOTE_HOST:$FULLPATH/
   if [ "$?" = "0" ]; then
      GO=no
   fi
done

ssh $REMOTE_USER@$REMOTE_HOST "sudo chmod -R 775 $PARTIALPATH; sudo chown -R 'sig-app:domain users' $PARTIALPATH"

# Now on to zonal statistics
if [ "$UPLOAD_PSPS_ZONAL_STATS" = "yes" ]; then
   FULLPATH=$REMOTE_DIRECTORY/psps_zonal/$PATTERN_OUT/$FORECAST_CYCLE

   ssh $REMOTE_USER@$REMOTE_HOST "sudo mkdir -p $FULLPATH"
   ssh $REMOTE_USER@$REMOTE_HOST "sudo chmod -R 775 $FULLPATH; sudo chown -R 'sig-app:domain users' $FULLPATH"
   FNLIST=`ls ./deenergization-zones/*`

   GO=yes
   while [ "$GO" = "yes" ]; do
      rsync -rv --timeout=10 $FNLIST $REMOTE_USER@$REMOTE_HOST:$FULLPATH/
      if [ "$?" = "0" ]; then
         GO=no
      fi
   done

   ssh $REMOTE_USER@$REMOTE_HOST "sudo chmod -R 775 $FULLPATH; sudo chown -R 'sig-app:domain users' $FULLPATH"
fi

ssh $REMOTE_USER@$REMOTE_HOST "/opt/geosync-scripts/$REMOTE_HOST/sync-layers.sh -s fire_risk_forecast -f 2 -t 2"

exit 0
