#!/bin/bash

SEC_START=`date +%s`

if [ ! -d $CLOUDFIRE_BASE_DIR/inputs/wx ]; then
   echo "Exiting because raw wx data are not available"
   exit 1
fi

WX_MODELS='hrrr nam_conusnest nam_awip12 gfs0p125 gfs0p25 nbm ecmwf adswrf cansac_wrf hybrid'
#WX_MODELS_SHORT='h n1 n2 g1 g2 b e a c y'
 WX_MODELS_SHORT='r n1 n2 g1 g2 b e a c y'
WX_MODEL_QUANTITIES='ffwi pign rh tmpf wd wg ws'

SPREAD_MODEL_QUANTITIES='fire-area impacted-structures fire-volume plignrate'
SPREAD_MODEL_QUANTITIES_SHORT='area str vol pligr'

SCRATCH=$(pwd)/scratch_attribute
OUTDIR=$(pwd)/deenergization-zones
NPARALLEL=`grep ^cpu\\scores /proc/cpuinfo | uniq |  awk '{print $4}'`

fhs=`seq 0 120`
fhs="$fhs "`seq 123 3 192`

T_SRS='EPSG: 5070'

FORECAST_CYCLE=`basename $(pwd) | cut -d- -f1`
PATTERN=`basename $(pwd) | cut -d- -f2`

POLYGONS=$ELMFIRE_BASE_DIR/config/polygons/${PATTERN}_polygons.shp

DATE_START=`echo $FORECAST_CYCLE | cut -d_ -f1`
HH_START=`echo $FORECAST_CYCLE | cut -d_ -f2`
TIMESTAMP_START=`date -u -d "$DATE_START $HH_START:00 UTC" +"%Y-%m-%d %H:%M"`

rm -f -r $SCRATCH $OUTDIR
mkdir $SCRATCH $OUTDIR

j=0 #attribute table column
i=0 #spread / wx model

# Set up spread model quantities:
for SPREAD_MODEL_QUANTITY in $SPREAD_MODEL_QUANTITIES; do
   let "i=i+1"
   SPREAD_MODEL_QUANTITY_SHORT=`echo $SPREAD_MODEL_QUANTITIES_SHORT | cut -d' ' -f$i`

   let "j=j+1"
   DATADIR[j]=$(pwd)/polygon_zonal_stats
   QUANTITY[j]=$SPREAD_MODEL_QUANTITY
   QUANTITY_SHORT[j]=$SPREAD_MODEL_QUANTITY_SHORT
   COL[j]=2 # 2=lo, 3=hi, 4=avg
   LO_HI_AVG[j]=l
#   MODEL_ID[j]=l
   MODEL_ID[j]=m

   let "j=j+1"
   DATADIR[j]=$(pwd)/polygon_zonal_stats
   QUANTITY[j]=$SPREAD_MODEL_QUANTITY
   QUANTITY_SHORT[j]=$SPREAD_MODEL_QUANTITY_SHORT
   COL[j]=4 # 2=lo, 3=hi, 4=avg
   LO_HI_AVG[j]=a
#   MODEL_ID[j]=l
   MODEL_ID[j]=m

   let "j=j+1"
   DATADIR[j]=$(pwd)/polygon_zonal_stats
   QUANTITY[j]=$SPREAD_MODEL_QUANTITY
   QUANTITY_SHORT[j]=$SPREAD_MODEL_QUANTITY_SHORT
   COL[j]=3 # 2=lo, 3=hi, 4=avg
   LO_HI_AVG[j]=h
#   MODEL_ID[j]=l
   MODEL_ID[j]=m

done

# Set up weather model quantities:
i=0
for WX_MODEL in $WX_MODELS; do
   let "i=i+1"
   WX_MODEL_SHORT=`echo $WX_MODELS_SHORT | cut -d' ' -f$i`

   for WX_MODEL_QUANTITY in $WX_MODEL_QUANTITIES; do

      TIMESERIES_DIR=$CLOUDFIRE_BASE_DIR/inputs/wx/$WX_MODEL/timeseries_$PATTERN/$FORECAST_CYCLE/timeseries

      let "j=j+1"
      DATADIR[j]=$TIMESERIES_DIR
      QUANTITY[j]=$WX_MODEL_QUANTITY
      QUANTITY_SHORT[j]=$WX_MODEL_QUANTITY
      COL[j]=2 # 2=lo, 3=hi, 4=avg
      LO_HI_AVG[j]=l
      MODEL_ID[j]=$WX_MODEL_SHORT

      let "j=j+1"
      DATADIR[j]=$TIMESERIES_DIR
      QUANTITY[j]=$WX_MODEL_QUANTITY
      QUANTITY_SHORT[j]=$WX_MODEL_QUANTITY
      COL[j]=4 # 2=lo, 3=hi, 4=avg
      LO_HI_AVG[j]=a
      MODEL_ID[j]=$WX_MODEL_SHORT

      let "j=j+1"
      DATADIR[j]=$TIMESERIES_DIR
      QUANTITY[j]=$WX_MODEL_QUANTITY
      QUANTITY_SHORT[j]=$WX_MODEL_QUANTITY
      COL[j]=3 # 2=lo, 3=hi, 4=avg
      LO_HI_AVG[j]=h
      MODEL_ID[j]=$WX_MODEL_SHORT

   done

done

J=$j

# Get data
NF=`ogrinfo -al -so $POLYGONS | grep Feature | cut -d: -f2 | xargs`
let "NFM1 = NF - 1"

# Set header
HEADER='zone'
HEADER_CSVT='"String"'

for j in $(eval echo "{1..$J}"); do #quantity
   HEADER="$HEADER,${MODEL_ID[j]}_${QUANTITY_SHORT[j]}_${LO_HI_AVG[j]}"
   HEADER_CSVT="$HEADER_CSVT,"'"Real","Real","Real"'
done
echo $HEADER > $SCRATCH/header.csv

function do_attribute_table {
   local fh=$1
   TIMESTAMP1=`date -u -d "$TIMESTAMP_START UTC + $fh HOURS" +"%Y-%m-%d %H:%M"`
   TIMESTAMP2=`date -u -d "$TIMESTAMP_START UTC + $fh HOURS" +"%Y%m%d_%H%M00"`

   echo $TIMESTAMP1

   cp $SCRATCH/header.csv $SCRATCH/$fh.csv

   for FID in $(eval echo "{0..$NFM1}"); do

#      ZONE=`ogrinfo -al -so -fid $FID $POLYGONS | grep 'Short name (String)' | cut -d= -f2 | xargs`
      ZONE=`ogrinfo -al -so -fid $FID $POLYGONS | grep 'Name (String)' | cut -d= -f2 | xargs`

      if [ -z "$ZONE" ]; then
         ZONE=`ogrinfo -al -so -fid $FID $POLYGONS | grep 'NAME (String)' | cut -d= -f2 | xargs`
      fi

      WRITESTR="$ZONE"
      for j in $(eval echo "{1..$J}"); do #quantity
         FNIN=${DATADIR[j]}/${QUANTITY[j]}_${FORECAST_CYCLE}0000_$FID.csv
         LINE=`cat $FNIN | grep "$TIMESTAMP1"`
         VAL=`echo $LINE | cut -d, -f${COL[j]} | xargs`
         if [ -z "$VAL" ]; then
            VAL=-9999
         fi
         WRITESTR="$WRITESTR,$VAL"
      done
      echo $WRITESTR >> $SCRATCH/$fh.csv
   done

   ogr2ogr -t_srs "$T_SRS" $OUTDIR/deenergization-zones_$TIMESTAMP2.shp $POLYGONS

   rm -f $SCRATCH/intermediate_$fh.*
#   echo '"String","Real","Real","Real","Real","Real","Real","Real","Real","Real"' > $SCRATCH/$fh.csvt
   echo "$HEADER_CSVT" > $SCRATCH/$fh.csvt
   ogr2ogr $SCRATCH/intermediate_$fh.shp $SCRATCH/$fh.csv
   cp -f $SCRATCH/intermediate_$fh.dbf $OUTDIR/deenergization-zones_$TIMESTAMP2.dbf

}

# Loop over forecast hours:
N=0
for fh in $fhs; do
   do_attribute_table $fh &
   let "N = N + 1"
   if [ "$N" = "$NPARALLEL" ]; then
      wait
      N=0
   fi
done

wait

rm -f -r $SCRATCH

SEC_NOW=`date +%s`
let "ELAPSED = SEC_NOW - SEC_START"
echo "Elapsed time: $ELAPSED"

exit 0
