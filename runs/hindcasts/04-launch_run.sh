#!/bin/bash

#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=112
#SBATCH --mem=250G
#SBATCH -t 12:00:00

echo "In 04-launch_run.sh"

CWD=$(pwd)
CONTROL_FILE=${CONTROL_FILE:-$1}

if [ -z $CONTROL_FILE ]; then
   echo "Error:  specify control file name as command line argument or slurm variable"
   exit 1
fi

function progress_message {
   NOW=`date -u +"%Y-%m-%d %H:%M:%S"`
   echo "$NOW - $1"
}

CWD=$(pwd)
FUEL_WX_IGN_CLI=$ELMFIRE_BASE_DIR/cloudfire/fuel_wx_ign.py

. ./99-setup_funcs.sh --source-only

parse_control_file $CONTROL_FILE

SIMULATION_START_TIME=`date -u -d "$INITIALIZATION_TIME" +"%Y-%m-%d %H:%M"`
INITIALIZATION_TIMESTAMP=`date -u -d "$INITIALIZATION_TIME" +%Y%m%d_%H%M00`
YYYY_START=`echo $SIMULATION_START_TIME | cut -d- -f1`
WX_TYPE='historical'

RUN_NAME=${FIRENAME}_$INITIALIZATION_TIMESTAMP

SCRATCH=$ELMFIRE_SCRATCH_BASE/$RUN_NAME
rm -f -r $SCRATCH
mkdir $SCRATCH

ISMATCHDROP=`echo $CONTROL_FILE | wc -l`
if [ "$ISMATCHDROP" != "0" ]; then
   progress_message "Creating fuel, weather, and ignition inputs"
   if [ "$INITIALIZATION_TYPE" = "points_within_polygon" ] ; then
      CENTER_LON=$IGNITION_LON
      CENTER_LAT=$IGNITION_LAT

      $FUEL_WX_IGN_CLI --name=$RUN_NAME --outdir=$SCRATCH \
                       --center_lon=$CENTER_LON --center_lat=$CENTER_LAT \
                       --do_fuel=True \
                       --fuel_source="$FUEL_SOURCE" --fuel_version="$FUEL_VERSION" \
                       --do_wx=True \
                       --wx_type="$WX_TYPE" --wx_start_time="$SIMULATION_START_TIME" --wx_num_hours=$RUN_HOURS \
                       --do_ignition=True \
                       --point_ignition=True --polygon_ignition=False \
                       --ignition_lon=$IGNITION_LON --ignition_lat=$IGNITION_LAT \
                       --ignition_radius=$IGNITION_RADIUS
   else
      $FUEL_WX_IGN_CLI --name=$RUN_NAME --outdir=$SCRATCH \
                       --do_fuel=True \
                       --fuel_source="$FUEL_SOURCE" --fuel_version="$FUEL_VERSION" \
                       --do_wx=True \
                       --wx_type="$WX_TYPE" --wx_start_time="$SIMULATION_START_TIME" --wx_num_hours=$RUN_HOURS \
                       --do_ignition=True \
                       --point_ignition=False --polygon_ignition=True \
                       --active_fire_timestamp="$ACTIVE_FIRE_TIMESTAMP" \
                       --already_burned_timestamp="$ALREADY_BURNED_TIMESTAMP"

   fi
fi

echo "RUN_TEMPLATE: $RUN_TEMPLATE"
if [ ! -d ./templates/$RUN_TEMPLATE ] || [ -z "$RUN_TEMPLATE" ]; then
   progress_message "Error:  RUN_TEMPLATE is set to $RUN_TEMPLATE" && exit 1
fi

progress_message "Setting up ELMFIRE run"
cp -f -r ./templates/$RUN_TEMPLATE/* $SCRATCH
cp -f $ELMFIRE_BASE_DIR/build/source/fuel_models.csv $SCRATCH/
if [ "$DO_WUI" = "yes" ]; then
   cp -f $ELMFIRE_BASE_DIR/build/source/building_fuel_models.csv $SCRATCH/
fi
cp -f $CONTROL_FILE $SCRATCH
cd $SCRATCH
tar -xf $RUN_NAME.tar && rm $RUN_NAME.tar

XLL=`gdalinfo ./asp.tif | grep "Lower Left" | cut -d ')' -f1  | cut -d '(' -f2 | cut -d, -f1`
YLL=`gdalinfo ./asp.tif | grep "Lower Left" | cut -d ')' -f1  | cut -d '(' -f2 | cut -d, -f2`
XUR=`gdalinfo ./asp.tif | grep "Upper Right" | cut -d ')' -f1  | cut -d '(' -f2 | cut -d, -f1`
YUR=`gdalinfo ./asp.tif | grep "Upper Right" | cut -d ')' -f1  | cut -d '(' -f2 | cut -d, -f2`
A_SRS=`gdalsrsinfo ./asp.tif | grep PROJ.4 | cut -d: -f2 | xargs`

FORECAST_START_HOUR=`cat metadata.out | grep WX_BEGIN | cut -d= -f2 | xargs | cut -d' ' -f2 | cut -d: -f1`

CURRENT_YEAR=`echo $SIMULATION_START_TIME | cut -d- -f1`
JDAY=`date -u -d "$SIMULATION_START_TIME" +%j`
HOUR=`date -u -d "$SIMULATION_START_TIME" +%H`
jday=`echo $((10#$JDAY))`
hour=`echo $((10#$HOUR))`
let "HOUR_OF_YEAR = (jday + 7 - 1)*24 + hour"

WX_START_DATE=`cat $SCRATCH/metadata.out | grep WX_BEGIN | cut -d= -f2 | xargs`
CENTER_LAT=`cat $SCRATCH/metadata.out | grep CENTER_LAT | cut -d= -f2 | xargs`
CENTER_LON=`cat $SCRATCH/metadata.out | grep CENTER_LON | cut -d= -f2 | xargs`
FORECAST_CYCLE=`date -u -d "$WX_START_DATE" +"%Y%m%d_%H"`

YYYYMMDD=`echo $FORECAST_CYCLE | cut -d_ -f1`
HH=`echo $FORECAST_CYCLE | cut -d_ -f2`
FORECAST_CYCLE_START_SEC=`date -u -d "$YYYYMMDD $HH:00" +%s`
SIMULATION_START_SEC=`date -u -d "$INITIALIZATION_TIME" +%s`
let "SIMULATION_TSTART = SIMULATION_START_SEC - FORECAST_CYCLE_START_SEC"
let "SIMULATION_TSTOP = SIMULATION_TSTART + RUN_HOURS * 3600"

replace_line FORECAST_START_HOUR $FORECAST_START_HOUR no
replace_line LONGITUDE $CENTER_LON no
replace_line LATITUDE $CENTER_LAT no
replace_line CURRENT_YEAR $CURRENT_YEAR no
replace_line HOUR_OF_YEAR $HOUR_OF_YEAR no
replace_line A_SRS "$A_SRS" yes
replace_line COMPUTATIONAL_DOMAIN_XLLCORNER $XLL no
replace_line COMPUTATIONAL_DOMAIN_YLLCORNER $YLL no
replace_line SIMULATION_TSTART $SIMULATION_TSTART no
replace_line SIMULATION_TSTOP $SIMULATION_TSTOP no
replace_line NUM_ENSEMBLE_MEMBERS $NUM_ENSEMBLE_MEMBERS no

if [ "$INITIALIZATION_TYPE" = "points_within_polygon" ] ; then
   replace_line RANDOM_IGNITIONS .TRUE. no
   replace_line USE_IGNITION_MASK .TRUE. no
   replace_line ALLOW_MULTIPLE_IGNITIONS_AT_A_PIXEL .TRUE. no
else
   replace_line RANDOM_IGNITIONS .FALSE. no
   replace_line USE_IGNITION_MASK .FALSE. no
fi

if [ "$DO_WUI" = "yes" ] ; then
   replace_line USE_BLDG_SPREAD_MODEL .TRUE. no
else
   replace_line USE_BLDG_SPREAD_MODEL .FALSE. no
fi

progress_message "Done setting up, tarring up input deck"
mkdir -p $CWD/input_decks
tar -cf $CWD/input_decks/$RUN_NAME.tar ./*

progress_message "Launching ELMFIRE run"

./01-run.sh $CWD/$RUN_NAME

exit 0
