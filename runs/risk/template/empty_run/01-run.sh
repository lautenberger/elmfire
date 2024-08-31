#!/bin/bash

PATTERN=$1
FUELS_INPUTS=$2
FORECAST_CYCLE=$3
TILE=$4
OUTDIR=$ELMFIRE_BASE_DIR/runs/risk/runs/$FORECAST_CYCLE-$PATTERN-$FUELS_INPUTS/$TILE

ELMFIRE_VER=${ELMFIRE_VER:-2024.0831}
ELMFIRE_INSTALL_DIR=${ELMFIRE_INSTALL_DIR:-$ELMFIRE_BASE_DIR/build/linux/bin}
ELMFIRE=$ELMFIRE_INSTALL_DIR/elmfire_$ELMFIRE_VER
TIMEOUT=3600

TR='150 150'
SRCWIN='1600 1600 1600 1600'
MPIRUN=`which mpirun`
SCRATCH=$ELMFIRE_SCRATCH_BASE/risk_run
INTERMEDIATE_OUTPUTS_DIR=$ELMFIRE_SCRATCH_BASE/$FORECAST_CYCLE-$PATTERN-$FUELS_INPUTS/$TILE
mkdir -p $INTERMEDIATE_OUTPUTS_DIR

SOCKETS=`lscpu | grep 'Socket(s)' | cut -d: -f2 | xargs`
CORES_PER_SOCKET=`lscpu | grep 'Core(s) per socket' | cut -d: -f2 | xargs`
let "NP = SOCKETS * CORES_PER_SOCKET"
SRS='EPSG:5070'
CWD=$(pwd)
#TILE=`basename $CWD`
#PATTERN=`echo $CWD | rev | cut -d/ -f2 | rev | cut -d- -f2`

compress () {
   local f=$1
   local CLIP=$2
   local STUB=`echo $f | cut -d. -f1`
   if [ "$CLIP" = "yes" ]; then
      gdal_translate -a_srs "$SRS" -r average -tr $TR -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" -srcwin $SRCWIN $STUB.bil $STUB.tif
   else
      gdal_translate -a_srs "$SRS" -r average -tr $TR -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" $STUB.bil $STUB.tif
   fi
   rm -f $STUB.bil $STUB.hdr
}

killall $MPIRUN $ELMFIRE postprocess.sh 2> /dev/null

# Run elmfire
echo "localhost slots=$NP" > $CWD/hosts
#$MPIRUN --mca btl tcp,self --bind-to core --oversubscribe -np $NP -machinefile hosts -x OMP_NUM_THREADS $ELMFIRE elmfire.data >& elmfire.log
timeout ${TIMEOUT}s $MPIRUN --mca btl tcp,self --bind-to core --oversubscribe -np $NP -machinefile hosts -x OMP_NUM_THREADS $ELMFIRE elmfire.data >& elmfire.log

if [ "$PATTERN" != "all" ]; then
   ./02-rasterize.sh $PATTERN >& rasterize.log
fi

N=0
for f in times_burned*.bil; do
   compress "$f" no &
   let "N=N+1"
   if [ "$N" = "$NP" ]; then
      N=0
      wait
   fi
done
wait

if [ "$PATTERN" != "all" ]; then
   ./03-shapefiles.sh $PATTERN $TILE >& shapefiles.log
fi

rm -f *.bsq *.hdr *.xml

mv * $INTERMEDIATE_OUTPUTS_DIR/

exit 0
