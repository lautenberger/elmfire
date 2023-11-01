#!/bin/bash

NUM_FUEL_MODELS=9

# Fuel model values:
FMV[1]=181
FMV[2]=182
FMV[3]=183
FMV[4]=184
FMV[5]=185
FMV[6]=186
FMV[7]=187
FMV[8]=188
FMV[9]=189

# Fuel model adjustment factor
FMA[1]=1.0
FMA[2]=1.0
FMA[3]=1.0
FMA[4]=1.0
FMA[5]=1.0
FMA[6]=1.0
FMA[7]=1.0
FMA[8]=1.0
FMA[9]=1.0

mv adj.tif adj_ones.tif

for (( i=1; i<=$NUM_FUEL_MODELS; i++ )); do
   if [ "$i" = "1" ]; then
      gdal_calc.py -A fbfm40.tif -B adj_ones.tif      --calc="(A==${FMV[i]})*${FMA[i]} + (A!=${FMV[i]})*B" --outfile="intermediate1.tif"
   else
      gdal_calc.py -A fbfm40.tif -B intermediate1.tif --calc="(A==${FMV[i]})*${FMA[i]} + (A!=${FMV[i]})*B" --outfile="intermediate2.tif"
      mv intermediate2.tif intermediate1.tif
   fi
done
gdal_translate -a_nodata -9999 -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" intermediate1.tif adj.tif
rm -f intermediate1.tif intermediate2.tif

exit 0
