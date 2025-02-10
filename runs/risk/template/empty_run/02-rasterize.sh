#!/bin/bash

date
START_SEC=`date +%s`

PATTERN=$1
NP=$2

if [ -z "$NP" ]; then
   SOCKETS=`lscpu | grep 'Socket(s)' | cut -d: -f2 | xargs`
   CORES_PER_SOCKET=`lscpu | grep 'Core(s) per socket' | cut -d: -f2 | xargs`
   let "NP = SOCKETS * CORES_PER_SOCKET"
fi

CLOUDFIRE_VER=${CLOUDFIRE_VER:-2025.0209}
IGNITION_DENSITY_GRID=$CLOUDFIRE_BASE_DIR/code/linux/bin/ignition_density_grid_$CLOUDFIRE_VER

XLLCORNER=`gdalinfo times_burned.bil | grep "Lower Left" | cut -d'(' -f2 | cut -d')' -f1 | cut -d, -f1 | xargs`
YLLCORNER=`gdalinfo times_burned.bil | grep "Lower Left" | cut -d'(' -f2 | cut -d')' -f1 | cut -d, -f2 | xargs`
cp kernel_density.data kd.data
sed -i "s/XLLCORNER_DUMMY/$XLLCORNER/g" kd.data
sed -i "s/YLLCORNER_DUMMY/$YLLCORNER/g" kd.data
if [ "$PATTERN" = "all" ]; then
   sed -i "s/SEARCH_RADIUS      = 200.0/SEARCH_RADIUS      = 106.1/g" kd.data
fi

cat fire_size_stats.csv | tr -d " " > fire_size_stats_nowhitespace.csv
mv fire_size_stats_nowhitespace.csv fire_size_stats.csv

function create_inputs {
   local h=$1
   local N=$2
   H=`printf %04d $h`
   STR="awk -F, '"
   STR=$STR'$2 == '$h' { print } '
   STR=$STR" ' fire_size_stats.csv"
   echo $STR > $H.sh
   chmod +x ./$H.sh
   taskset -a -c $N ./$H.sh > $H.csv

   rm -f ./$H.sh

   taskset -a -c $N cat $H.csv | csvcut -c 3,4,7  > surface_fire_area_$H.csv
#   taskset -a -c $N cat $H.csv | csvcut -c 3,4,8  > crown_fire_area_$H.csv
   taskset -a -c $N cat $H.csv | csvcut -c 3,4,9  > fire_volume_$H.csv
   taskset -a -c $N cat $H.csv | csvcut -c 3,4,10 > affected_population_$H.csv
#   cat $H.csv | csvcut -c 3,4,11 > affected_real_estate_value_$H.csv
#   cat $H.csv | csvcut -c 3,4,12 > affected_land_value_$H.csv

   cp -f kd.data surface_fire_area_$H.data
#   cp -f kd.data crown_fire_area_$H.data
   cp -f kd.data fire_volume_$H.data
   cp -f kd.data affected_population_$H.data
#   cp -f kd.data affected_real_estate_value_$H.data
#   cp -f kd.data affected_land_value_$H.data

   sed -i "s/dummy_in.csv/surface_fire_area_$H.csv/g" surface_fire_area_$H.data
#   sed -i "s/dummy_in.csv/crown_fire_area_$H.csv/g"   crown_fire_area_$H.data
   sed -i "s/dummy_in.csv/fire_volume_$H.csv/g"  fire_volume_$H.data
   sed -i "s/dummy_in.csv/affected_population_$H.csv/g"   affected_population_$H.data
#   sed -i "s/dummy_in.csv/affected_real_estate_value_$H.csv/g"  affected_real_estate_value_$H.data
#   sed -i "s/dummy_in.csv/affected_land_value_$H.csv/g"         affected_land_value_$H.data

   sed -i "s/dummy_out/surface_fire_area_$H/g" surface_fire_area_$H.data
#   sed -i "s/dummy_out/crown_fire_area_$H/g"   crown_fire_area_$H.data
   sed -i "s/dummy_out/fire_volume_$H/g"  fire_volume_$H.data
   sed -i "s/dummy_out/affected_population_$H/g"   affected_population_$H.data
#   sed -i "s/dummy_out/affected_real_estate_value_$H/g"  affected_real_estate_value_$H.data
#   sed -i "s/dummy_out/affected_land_value_$H/g"         affected_land_value_$H.data

}

N=0
for h in {7..114}; do
   create_inputs $h $N &
   let "N=N+1"
   if [ "$N" = "$NP" ]; then
      N=0
      wait
   fi
done
wait

N=0
for QUANTITY in surface_fire_area fire_volume affected_population; do
   for h in {7..114}; do
      H=`printf %04d $h`
      taskset -a -c $N $IGNITION_DENSITY_GRID ${QUANTITY}_$H.data && \
      taskset -a -c $N gdal_translate -a_srs "EPSG:5070" -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" ${QUANTITY}_$H.bil ${QUANTITY}_$H.tif &&
      taskset -a -c $N rm -f ${QUANTITY}_$H.bil ${QUANTITY}_$H.hdr ${QUANTITY}_$H.data ${QUANTITY}_$H.csv &
      let "N=N+1"
      if [ "$N" = "$NP" ]; then
         N=0
         wait
      fi
   done
done
wait

## Now do plignrate:
#N=0
#if [ "$PATTERN" != "all" ]; then
#   for SUBTILE in 1_1 1_2 1_3 2_1 2_2 2_3 3_1 3_2 3_3; do
#      FNLIST="$FNLIST plignrate_${PATTERN}_$SUBTILE.bsq"
#   done
#   gdal_merge.py -o intermediate_$PATTERN.tif $FNLIST
#   gdalwarp -multi -tr 150 150 -r bilinear intermediate_$PATTERN.tif plignrate_$PATTERN.tif
#
#   for h in {7..114}; do
#      H=`printf %04d $h`
#      gdal_translate -b $h -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" plignrate_$PATTERN.tif plignrate_$H.tif &
#      let "N=N+1"
#      if [ "$N" = "$NP" ]; then
#         N=0
#         wait
#      fi
#   done
#   wait
#   rm -f intermediate_$PATTERN* plignrate_$PATTERN*.bil plignrate_$PATTERN*.hdr plignrate_$PATTERN*.prj plignrate_$PATTERN*.xml plignrate_$PATTERN.tif
#fi

date
END_SEC=`date +%s`

let "ELAPSED_TIME = END_SEC - START_SEC"
echo "Elapsed time:  $ELAPSED_TIME s"

exit 0
