#!/bin/bash

. ~/.bashrc

function wait_for_slurm {
   local JOB_ID=$1
   ISRUNNING=1
   while [ "$ISRUNNING" != "0" ]; do
      NOW=`date -u +"%Y-%m-%d %H:%M:%S"`
      echo "$NOW waiting for slurm job $JOB_ID to finish"
      sleep 1
      ISRUNNING=`squeue -j $JOB_ID 2> /dev/null | wc -l`
   done
}

DATADIR=$1
if [ -z "$DATADIR" ]; then
   echo "Error, specify DATADIR as command line argument"
   exit 1
fi

# Configuration
USE_SLURM=no
PUSH_TO_GCS=no
PUSH_TO_GEOSERVER=yes

# Parameters for no slurm
CPUS_PER_TASK_MAX=112
MEM_MAX=250G
TIME_MAX='12:00:00'

# Parameters with slurm
CPUS_PER_TASK=7
MEM02=16G
MEM0304=32G
TIME='01:00:00'

OUTDIR=$DATADIR/post

FHS=`seq -f %04g 7 114`
LO=7 ; HI=13
for i in `seq 1 16`; do
   FHS_ARR[i]=`seq -f %04g $LO $HI`
   let "LO = LO + 7"; let "HI = HI + 7"
   if [ "$HI" -gt "114" ]; then HI=114; fi
done

RUN_ID=`echo $DATADIR | rev | cut -d'/' -f1 | rev`
PATTERN=`echo $RUN_ID | cut -d'-' -f2`

mkdir -p $OUTDIR $DATADIR/log

# Copy post-processing scripts
cp -f ./template/02-post.sh               $DATADIR
if [ "$PATTERN" = "all" ]; then
   cp -f ./template/0304-rasterize.sh     $DATADIR
else
   cp -f ./template/03-zonal.sh           $DATADIR
   cp -f ./template/04-attribute_table.sh $DATADIR
fi

if [ "$PUSH_TO_GEOSERVER" = "yes" ]; then
   cp -f ./template/05-upload.sh $DATADIR
fi

cd $DATADIR

# Mosaic times burned, etc.
if [ "$USE_SLURM" = "yes" ]; then
   for i in `seq 1 16`; do
      JOB_ID1[i]=$(sbatch --job-name=02post${i}_$PATTERN --chdir=$DATADIR \
                 --output=$DATADIR/log/02-post_${i}_%j.txt \
                 --cpus-per-task=$CPUS_PER_TASK --mem=$MEM02 --time="$TIME" \
                 --export=ALL,DATADIR=$DATADIR,NPARALLEL=$CPUS_PER_TASK,FHS="${FHS_ARR[i]}" \
                 ./02-post.sh | awk '{print $4}')
   done
else
   ./02-post.sh $DATADIR $CPUS_PER_TASK_MAX "$FHS"
fi

# Create rasters from point data
if [ "$PATTERN" = "all" ]; then
   if [ "$USE_SLURM" = "yes" ]; then
      for i in `seq 1 16`; do
         JOB_ID2[i]=$(sbatch --job-name=0304rasterize${i}_$PATTERN --chdir=$DATADIR \
                             --output=$DATADIR/log/0304-rasterize_${i}_%j.txt \
                             --cpus-per-task=$CPUS_PER_TASK --mem=$MEM0304 --time="$TIME" \
                             --export=ALL,DATADIR=$DATADIR,NPARALLEL=$CPUS_PER_TASK,FHS="${FHS_ARR[i]}" \
                             ./0304-rasterize.sh | awk '{print $4}')
      done
   else
      ./0304-rasterize.sh $DATADIR $CPUS_PER_TASK_MAX "$FHS" >& $DATADIR/log/0304-rasterize.txt
   fi
fi

if [ "$USE_SLURM" = "yes" ]; then
   for i in `seq 1 16`; do
      wait_for_slurm ${JOB_ID1[i]}
      if [ "$PATTERN" = "all" ]; then
         wait_for_slurm ${JOB_ID2[i]}
      fi
   done
fi

if [ "$PATTERN" != "all" ]; then
   ./03-zonal.sh           >& $DATADIR/log/log_03-zonal.txt
   ./04-attribute_table.sh >& $DATADIR/log/log_04-attribute_table.txt
fi

# Push to storage bucket
if [ "$PUSH_TO_GCS" = "yes" ]; then
   gcloud storage rsync ./post/ gs://shasta/fire_risk_forecast/$RUN_ID/ --recursive
fi

if [ "$PUSH_TO_GEOSERVER" = "yes" ]; then
   ./05-upload.sh
fi

exit 0
