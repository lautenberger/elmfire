#!/bin/bash

. ./99-post_funcs.sh --source-only

progress_message "Starting 02-postprocess.sh"

CREATE_FIREMAP_OUTPUTS=yes
CREATE_DAILY_OUTPUTS=yes
LIMIT_TO_SEVEN_DAYS=yes
UPLOAD_TO_PYRECAST="${UPLOAD_TO_PYRECAST:-no}"
GEOSERVER_USERNAME="${GEOSERVER_USERNAME:-elmfire}"
GEOSERVER_HOSTNAME_PROD="${GEOSERVER_HOSTNAME_PROD:-trinity}"
GEOSERVER_HOSTNAME_DEV="${GEOSERVER_HOSTNAME_DEV:-swift}"
GEOSERVER_BASEDIR_PROD="${GEOSERVER_BASEDIR_PROD:-/srv/gis}"
GEOSERVER_BASEDIR_DEV="${GEOSERVER_BASEDIR_DEV:-/srv/gis}"
GEOSERVER_INCOMINGDIR="${GEOSERVER_INCOMINGDIR:-/incoming}"
OWNERSHIP="${OWNERSHIP:-'elmfire:domain users'}"
BUFFER_SCRIPT=$ELMFIRE_BASE_DIR/etc/buffer.py
ELMFIRE_INSTALL_DIR=${ELMFIRE_INSTALL_DIR:-$ELMFIRE_BASE_DIR/build/linux/bin}
ELMFIRE_VER=${ELMFIRE_VER:-2025.0212}

HRS_IN_FORECAST[7]=168  # 7-day forecast
HRS_IN_FORECAST[14]=336 #14-day forecast

ELMFIRE_POST=$ELMFIRE_INSTALL_DIR/elmfire_post_$ELMFIRE_VER
SCRATCH=$(pwd)/scratch
echo "SCRATCH: $SCRATCH"

rm -f -r $SCRATCH
mkdir $SCRATCH

if [ -e ./already_burned.tif ]; then
   READ_ALREADY_BURNED=yes
   rm -f ./already_burned_float.tif
   gdal_translate -ot Float32 ./already_burned.tif  ./already_burned_float.tif 1>& /dev/null && \
   gdal_calc.py -A ./already_burned_float.tif --type=Int16 --calc="0+(A>0)*-2" --NoDataValue=0 --outfile=$SCRATCH/already_burned.tif 1>& /dev/null && \
   gdal_polygonize.py $SCRATCH/already_burned.tif $SCRATCH/already_burned.shp 1>& /dev/null &
else
   READ_ALREADY_BURNED=no
fi

PHIMIN=`gdalinfo -stats ./phi.tif  | grep STATISTICS_MINIMUM | cut -d= -f2 | xargs`
ISLT0=`echo "$PHIMIN < 0" | bc`
if [ "$ISLT0" = "1" ]; then
   READ_PHI=yes
   gdal_calc.py -A ./phi.tif --type=Int16 --calc="0+(A<0)*-1" --NoDataValue=0 --outfile=$SCRATCH/burning.tif 1>& /dev/null && \
   gdal_polygonize.py $SCRATCH/burning.tif $SCRATCH/burning.shp 1>& /dev/null &
else
   READ_PHI=no
fi

HOSTS=`printf "$(hostname),%.0s" {1..128}`
SOCKETS=`lscpu | grep 'Socket(s)' | cut -d: -f2 | xargs`
CORES_PER_SOCKET=`lscpu | grep 'Core(s) per socket' | cut -d: -f2 | xargs`
let "NP = SOCKETS * CORES_PER_SOCKET"
echo "NP: $NP"

CELLSIZE=`cat ./elmfire.data | grep COMPUTATIONAL_DOMAIN_CELLSIZE | cut -d= -f2 | xargs`
XLLCORNER=`cat ./elmfire.data | grep COMPUTATIONAL_DOMAIN_XLLCORNER | cut -d= -f2 | xargs`
YLLCORNER=`cat ./elmfire.data | grep COMPUTATIONAL_DOMAIN_YLLCORNER | cut -d= -f2 | xargs`
NCASES=`cat ./elmfire.data | grep NUM_ENSEMBLE_MEMBERS | cut -d= -f2 | xargs`
DT=3600.
SIMULATION_TSTOP=`cat ./elmfire.data | grep SIMULATION_TSTOP | cut -d= -f2 | xargs`
SIMULATION_TSTART=`cat ./elmfire.data | grep SIMULATION_TSTART | cut -d= -f2 | xargs`
NX=`gdalinfo ./slp.tif | grep "Size is" | cut -ds -f2 | cut -d, -f1 | xargs`
NY=`gdalinfo ./slp.tif | grep "Size is" | cut -ds -f2 | cut -d, -f2 | xargs`

FIRE_NAME=`basename $(pwd) | cut -d_ -f1`
START_DATE=`basename $(pwd) | cut -d_ -f2`
START_TIME=`basename $(pwd) | cut -d_ -f3`
START_HOUR=${START_TIME:0:2}
START_MIN=${START_TIME:2:2}
start_min=$((10#$START_MIN))

ISMATCHDROP=`echo $FIRE_NAME | grep matchdrop | wc -l`
if [ "$ISMATCHDROP" != "0" ]; then
   ISMATCHDROP=yes
else
   ISMATCHDROP=no
fi
if [ "$ISMATCHDROP" = "yes" ]; then
   N_PERCENTILES=5
   PERCS='10 30 50 70 90'
   PERCS_GEOSERVER='10 30 50 70 90'
   PERCS_CSV='10., 30., 50., 70., 90.'
   DUMP_PERCS='.TRUE., .TRUE., .TRUE., .TRUE., .TRUE.'
   RASTERS='hours-since-burned'
   DUMP_FLAME_LENGTH=.FALSE.
   DUMP_SPREAD_RATE=.FALSE.
   DUMP_CROWN_FIRE=.FALSE.
else
   N_PERCENTILES=15
   PERCS='1 5 10 20 30 40 50 60 70 75 80 85 90 95 99'
   PERCS_GEOSERVER='10 30 50 70 90'
   PERCS_CSV='1., 5., 10., 20., 30., 40., 50., 60., 70., 75., 80., 85., 90., 95., 99.'
#                 1        5       10      20       30      40       50      60      70       75       80       85       90       95      99
   DUMP_PERCS='.FALSE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE., .FALSE., .FALSE., .TRUE., .FALSE., .FALSE.'
#   DUMP_PERCS='.TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE., .TRUE.'
   PERC_FROM_FID[0]=1
   PERC_FROM_FID[1]=5
   PERC_FROM_FID[2]=10
   PERC_FROM_FID[3]=20
   PERC_FROM_FID[4]=30
   PERC_FROM_FID[5]=40
   PERC_FROM_FID[6]=50
   PERC_FROM_FID[7]=60
   PERC_FROM_FID[8]=70
   PERC_FROM_FID[9]=75
   PERC_FROM_FID[10]=80
   PERC_FROM_FID[11]=85
   PERC_FROM_FID[12]=90
   PERC_FROM_FID[13]=95
   PERC_FROM_FID[14]=99
   RASTERS='crown-fire flame-length hours-since-burned spread-rate'
   DUMP_FLAME_LENGTH=.TRUE.
   DUMP_SPREAD_RATE=.TRUE.
   DUMP_CROWN_FIRE=.TRUE.
fi

NUM_TIMESTEPS=`echo "($SIMULATION_TSTOP - $SIMULATION_TSTART) / $DT" | bc -l | cut -d. -f1`
if [ "$start_min" != "0" ]; then
   let "NUM_TIMESTEPS = NUM_TIMESTEPS + 1"
fi
if [ "$LIMIT_TO_SEVEN_DAYS" = "yes" ]; then
   NUM_TIMESTEPS=169
fi

TIMESTAMP_BY_FRAME[1]='null'
for frame in $(eval echo "{1..$NUM_TIMESTEPS}"); do
   timestamp_by_frame $frame
done
echo ${TIMESTAMP_BY_FRAME[@]}

echo '&ELMFIRE_POST_INPUTS'                       > elmfire_post.data
echo "NX = $NX"                                  >> elmfire_post.data
echo "NY = $NY"                                  >> elmfire_post.data
echo "NCASES = $NCASES"                          >> elmfire_post.data
echo "N_PERCENTILES = $N_PERCENTILES"            >> elmfire_post.data
echo "PERCENTILES(:) = $PERCS_CSV"               >> elmfire_post.data
echo "DUMP_HOURLY_OUTPUTS_FOR_PERC_NUM(:) = $DUMP_PERCS" >> elmfire_post.data
echo "XLLCORNER = $XLLCORNER"                    >> elmfire_post.data
echo "YLLCORNER = $YLLCORNER"                    >> elmfire_post.data
echo "CELLSIZE = $CELLSIZE"                      >> elmfire_post.data
echo "OUTPUTS_DIRECTORY = '"$(pwd)"/'"           >> elmfire_post.data
echo "DT = $DT"                                  >> elmfire_post.data
echo "NUM_TIMESTEPS = $NUM_TIMESTEPS"            >> elmfire_post.data
echo "POSTPROCESS_TYPE = 1"                      >> elmfire_post.data
echo "HOURLY_PYREGENCE_OUTPUTS = .TRUE."         >> elmfire_post.data
echo "START_TIME_MINUTES_PAST_HOUR = $start_min" >> elmfire_post.data
echo "FIREMODEL_SIMULATION_TSTART = $SIMULATION_TSTART" >> elmfire_post.data
echo "DUMP_FLAME_LENGTH = $DUMP_FLAME_LENGTH"    >> elmfire_post.data
echo "DUMP_SPREAD_RATE = $DUMP_SPREAD_RATE"      >> elmfire_post.data
echo "DUMP_CROWN_FIRE = $DUMP_CROWN_FIRE"        >> elmfire_post.data
echo "DUMP_TIME_OF_ARRIVAL = .TRUE."             >> elmfire_post.data
echo "SCRATCH = '"$SCRATCH"/'"                   >> elmfire_post.data
echo "PATH_TO_GDAL = '/usr/bin/'"                >> elmfire_post.data
echo "FLAME_LENGTH_MIN = 1.0"                    >> elmfire_post.data
echo "SPREAD_RATE_MIN = 1.0"                     >> elmfire_post.data

if [ "$READ_ALREADY_BURNED" = "yes" ]; then
   echo "READ_ALREADY_BURNED = .TRUE."                     >> elmfire_post.data
   echo "ALREADY_BURNED_FILENAME = 'already_burned_float'" >> elmfire_post.data
fi
if [ "$READ_PHI" = "yes" ]; then
   echo "READ_PHI = .TRUE."                                >> elmfire_post.data
   echo "PHI_FILENAME = 'phi'"                             >> elmfire_post.data
fi
echo "/" >> elmfire_post.data

wait
progress_message "Done with initial setup, running elmfire_post"
export OMP_NUM_THREADS=$NP
mpirun --mca btl tcp,self --map-by core --bind-to core --oversubscribe -np $NP -x OMP_NUM_THREADS $ELMFIRE_POST elmfire_post.data #2> /dev/null

progress_message "Converting .bil/.hdr files to .tif"
./03-make_tifs.sh >& log_make_tifs.sh

progress_message "Preparing tifs for GeoServer"
FIRE_DATE_TIME=$FIRE_NAME/${START_DATE}_${START_TIME}
for PERC in $PERCS; do
   PERC_TWO=`printf %02d $PERC`
   for RASTER in $RASTERS; do
      mkdir -p ./geoserver/$FIRE_DATE_TIME/elmfire/landfire/$PERC_TWO/$RASTER
      cp -f ./imagemosaic_properties/* ./geoserver/$FIRE_DATE_TIME/elmfire/landfire/$PERC_TWO/$RASTER/
   done
done

progress_message "Renaming files"
for frame in $(eval echo "{1..$NUM_TIMESTEPS}"); do
   rename $frame &
done
wait

progress_message "Creating 7 and 14 day fire perimeters"
rm -f $SCRATCH/intermediate*

for days in 7 14; do
   DAYS=`printf %02d $days`
   HR_HI=${HRS_IN_FORECAST[days]}
   echo "HR_HI: $HR_HI"
   for PERC in $PERCS; do
      PERC_TWO=`printf %02d $PERC`
      cp -f time-of-arrival_$PERC_TWO.tif toa_$PERC_TWO.tif
      gdal_edit.py -unsetnodata toa_$PERC_TWO.tif
      gdal_calc.py -A toa_$PERC_TWO.tif --type=Byte --NoDataValue=0 \
                   --calc="0 + (A>0.0001)*(A<$HR_HI)*$PERC" --outfile=$SCRATCH/intermediate_multi_${DAYS}_$PERC_TWO.tif 1>& /dev/null && \
      gdal_polygonize.py $SCRATCH/intermediate_multi_${DAYS}_$PERC_TWO.tif $SCRATCH/intermediate_multi_${DAYS}_$PERC_TWO.shp 1>& /dev/null && \
      ogr2ogr -progress -skipfailures $SCRATCH/intermediate_${DAYS}_$PERC_TWO.shp $SCRATCH/intermediate_multi_${DAYS}_$PERC_TWO.shp \
              -dialect sqlite -sql "SELECT ST_Union(geometry) as geometry FROM intermediate_multi_${DAYS}_$PERC_TWO" 1>& /dev/null && \
      set_percentile $SCRATCH/intermediate_${DAYS}_$PERC_TWO.shp $PERC 1>& /dev/null &
   done
done
wait

FIRST=yes
for PERC in $PERCS; do
   PERC_TWO=`printf %02d $PERC`
   mv time-of-arrival_$PERC_TWO.tif ./geoserver/$FIRE_DATE_TIME/elmfire/landfire/$PERC_TWO/time-of-arrival.tif

   for days in 7 14; do
      DAYS=`printf %02d $days`
      if [ "$FIRST" = "yes" ]; then
         ogr2ogr                 $SCRATCH/out_$DAYS.shp $SCRATCH/intermediate_${DAYS}_$PERC_TWO.shp                1>& /dev/null
      else
         ogr2ogr -update -append $SCRATCH/out_$DAYS.shp $SCRATCH/intermediate_${DAYS}_$PERC_TWO.shp -nln out_$DAYS 1>& /dev/null
      fi
   done
   FIRST=no
done

progress_message "Checking burning.shp"
if [ -e $SCRATCH/burning.shp ]; then
   ogr2ogr $SCRATCH/burning_onefeature.shp $SCRATCH/burning.shp -dialect sqlite -sql "SELECT ST_Union(geometry) as geometry FROM burning" 1>& /dev/null
   set_percentile $SCRATCH/burning_onefeature.shp -1 1>& /dev/null
   ogr2ogr -update -append $SCRATCH/out.shp $SCRATCH/burning_onefeature.shp -nln out 1>& /dev/null
fi

progress_message "Checking already_burned.shp"
if [ -e $SCRATCH/already_burned.shp ]; then
   ogr2ogr $SCRATCH/already_burned_onefeature.shp $SCRATCH/already_burned.shp -dialect sqlite -sql "SELECT ST_Union(geometry) as geometry FROM already_burned" 1>& /dev/null
   set_percentile $SCRATCH/already_burned_onefeature.shp -2 1>& /dev/null
   for days in 7 14; do
      DAYS=`printf %02d $days`
      ogr2ogr -update -append $SCRATCH/out_$DAYS.shp $SCRATCH/already_burned_onefeature.shp -nln out_$DAYS 1>& /dev/null
   done
fi

progress_message "Running ogr2ogr"
for days in 7 14; do
   DAYS=`printf %02d $days`
   ogr2ogr $SCRATCH/${FIRE_NAME}_${START_DATE}_${START_TIME}_${DAYS}_elmfire.shp $SCRATCH/out_$DAYS.shp 1>& /dev/null
done

wait

# Create individual shapefiles (and convert to MDT)
progress_message "Creating individual shapefiles"

HH=${START_TIME:0:2}
MM=${START_TIME:2:2}
MDT=`date -u -d "$START_DATE $HH:$MM UTC - 6 hours" +%Y%m%d_%H%M`

if [ "$ISMATCHDROP" = "yes" ]; then
   for FID in {0..4}; do
      let "PERC = (FID + 1)*20 - 10"
      FNIN=$SCRATCH/${FIRE_NAME}_${START_DATE}_${START_TIME}_14_elmfire.shp
      FNOUT_NOPATH=isochrones_${FIRE_NAME}_${START_DATE}_${START_TIME}_$PERC.shp
      PATHNAME=./geoserver/$FIRE_NAME/${START_DATE}_${START_TIME}/elmfire/landfire/$PERC

      for days in 7 14; do
         DAYS=`printf %02d $days`
         ogr2ogr -fid $FID $SCRATCH/${FIRE_NAME}_${MDT}_${DAYS}_elmfire_$PERC.shp $SCRATCH/${FIRE_NAME}_${START_DATE}_${START_TIME}_${DAYS}_elmfire.shp &
      done
      posnegbuffer $FNIN $FNOUT_NOPATH $PATHNAME $FID 60.0 &
   done
else
   for FID in {0..14}; do
      PERC=${PERC_FROM_FID[FID]}
      PERC_TWO=`printf %02d $PERC`
      FNIN=$SCRATCH/${FIRE_NAME}_${START_DATE}_${START_TIME}_14_elmfire.shp
      FNOUT_NOPATH=isochrones_${FIRE_NAME}_${START_DATE}_${START_TIME}_$PERC_TWO.shp
      PATHNAME=./geoserver/$FIRE_NAME/${START_DATE}_${START_TIME}/elmfire/landfire/$PERC_TWO

      for days in 7 14; do
         DAYS=`printf %02d $days`
         ogr2ogr -fid $FID $SCRATCH/${FIRE_NAME}_${MDT}_${DAYS}_elmfire_$PERC_TWO.shp $SCRATCH/${FIRE_NAME}_${START_DATE}_${START_TIME}_${DAYS}_elmfire.shp &
      done
      posnegbuffer $FNIN $FNOUT_NOPATH $PATHNAME $FID 60.0 &
   done
   wait

   for FID in {0..14}; do
      PERC=${PERC_FROM_FID[FID]}
      PERC_TWO=`printf %02d $PERC`
      for days in 7 14; do
         DAYS=`printf %02d $days`
         add_acreage $PERC_TWO $DAYS &
      done
   done
   wait

fi
wait

progress_message "Zipping up"
for days in 7 14; do
   DAYS=`printf %02d $days`
   zip -9 -j $ELMFIRE_BASE_DIR/runs/forecasts/rsync/${FIRE_NAME}_${MDT}_${DAYS}_elmfire.zip $SCRATCH/${FIRE_NAME}_${MDT}_${DAYS}*_elm* 1>& /dev/null
done

if [ "$CREATE_DAILY_OUTPUTS" = "yes" ]; then
   progress_message "Running daily post-processing"
   ./40-daily.sh
fi

for PERC_TWO in 01 05 20 40 60 75 80 85 95 99; do
   rm -f -r ./geoserver/$FIRE_NAME/${START_DATE}_${START_TIME}/elmfire/landfire/$PERC_TWO
done

# Process geoserver directory
progress_message "Creating Geoserver tarball"
TARBALL=$FIRE_NAME-${START_DATE}_$START_TIME
chmod -R 775 ./geoserver
cd ./geoserver
tar -cf $TARBALL.tar * >& /dev/null

if [ "$UPLOAD_TO_PYRECAST" = "yes" ]; then
   progress_message "Uploading tarball to production $GEOSERVER_HOSTNAME_PROD"
   scp $TARBALL.tar $GEOSERVER_USERNAME@$GEOSERVER_HOSTNAME_PROD:$GEOSERVER_INCOMINGDIR/elmfire-$TARBALL.tar
   progress_message "Extracting tarball on production geoserver $GEOSERVER_HOSTNAME_PROD to $GEOSERVER_BASEDIR_PROD"
   ssh $GEOSERVER_USERNAME@$GEOSERVER_HOSTNAME_PROD "cd $GEOSERVER_INCOMINGDIR; tar -xf elmfire-$TARBALL.tar -C $GEOSERVER_BASEDIR_PROD/fire_spread_forecast/; sudo chown -R $OWNERSHIP $GEOSERVER_BASEDIR_PROD/fire_spread_forecast/$FIRE_NAME/; sudo chmod -R 775 $GEOSERVER_BASEDIR_PROD/fire_spread_forecast/$FIRE_NAME/; rm elmfire-$TARBALL.tar"

   progress_message "Uploading tarball to development $GEOSERVER_HOSTNAME_DEV"
   scp $TARBALL.tar $GEOSERVER_USERNAME@$GEOSERVER_HOSTNAME_DEV:$GEOSERVER_INCOMINGDIR/elmfire-$TARBALL.tar
   progress_message "Extracting tarball on $development GEOSERVER_HOSTNAME_DEV to $GEOSERVER_BASEDIR_DEV"
   ssh $GEOSERVER_USERNAME@$GEOSERVER_HOSTNAME_DEV "cd $GEOSERVER_INCOMINGDIR; tar -xf elmfire-$TARBALL.tar -C $GEOSERVER_BASEDIR_DEV/fire_spread_forecast/; sudo chown -R $OWNERSHIP $GEOSERVER_BASEDIR_DEV/fire_spread_forecast/$FIRE_NAME/; sudo chmod -R 775 $GEOSERVER_BASEDIR_DEV/fire_spread_forecast/$FIRE_NAME/; rm elmfire-$TARBALL.tar"
fi
mv $TARBALL.tar ..

wait

progress_message "Compressing ASCII & binary files and cleaning up"
cd ..

if [ "$CREATE_FIREMAP_OUTPUTS" = "yes" ]; then
   progress_message "Running WIFIRE firemap post-processing"
   ./20-firemap.sh
fi

tar cf - *.bin | pigz > binary_outputs.tar.gz && rm -f -r *.bin diag*.csv geoserver $SCRATCH

wait

progress_message "Done compressing and cleaning up"

exit 0
