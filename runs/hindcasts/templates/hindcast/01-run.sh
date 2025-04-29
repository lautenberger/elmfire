#!/bin/bash

OUTDIR=$1

. ./99-post_funcs.sh --source-only

STARTSEC=`date +%s`

progress_message "Start"

LOCAL_SCRATCH=$(pwd)
ELMFIRE_VER=${ELMFIRE_VER:-2025.0429}
ELMFIRE_INSTALL_DIR=${ELMFIRE_INSTALL_DIR:-$ELMFIRE_BASE_DIR/build/linux/bin}
ELMFIRE=$ELMFIRE_INSTALL_DIR/elmfire_$ELMFIRE_VER
FIRE_NAME=`echo $LOCAL_SCRATCH | rev | cut -d/ -f1 | rev | cut -d_ -f1`
DATE_START=`echo $LOCAL_SCRATCH | rev | cut -d/ -f1 | rev | cut -d_ -f2`
TIME_START=`echo $LOCAL_SCRATCH | rev | cut -d/ -f1 | rev | cut -d_ -f3`
TIMESTAMP_START="${DATE_START}_${TIME_START}"

SOCKETS=`lscpu | grep 'Socket(s)' | cut -d: -f2 | xargs`
CORES_PER_SOCKET=`lscpu | grep 'Core(s) per socket' | cut -d: -f2 | xargs`
let "NP = SOCKETS * CORES_PER_SOCKET"

progress_message "Launching ELMFIRE"
mpirun --mca btl tcp,self,vader --map-by core --bind-to core --oversubscribe --mca btl_tcp_if_exclude 172.17.0.0/24,127.0.0.0/24 -np $NP $ELMFIRE elmfire.data >& elmfire.out

progress_message "ELMFIRE complete, starting .bin->.bil conversion"
./02-elmfire_post.sh >& elmfire_post.log

progress_message "Postprocessing complete, cleaning up"

rm -f *.aux.xml *.bsq *.hdr *.aux.xml

progress_message "Calculating fitness"
./04-fitness.sh >& fitness.log
mv coeffs_w_fitness.csv coeffs.csv

mkdir ./wx ./fuel
for QUANTITY in ws wd m1 lh lw; do
   mv $QUANTITY*.tif ./wx
done
for QUANTITY in asp dem slp adj fbfm40 cc ch cbd cbh phi burning ignition_mask; do
   mv $QUANTITY*.tif ./fuel
done
mv active* ./fuel/ 2> /dev/null
mv already* ./fuel/ 2> /dev/null

rm -f -r $OUTDIR
mkdir -p $OUTDIR

cp -f -r * $OUTDIR
cd $OUTDIR
rm -f -r $LOCAL_SCRATCH

ENDSEC=`date +%s`
let "RUNTIME = ENDSEC - STARTSEC"
progress_message "ELMFIRE run is complete"
echo "Wall clock time:  $RUNTIME s"

exit 0
