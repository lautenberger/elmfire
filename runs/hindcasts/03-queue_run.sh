#/bin/bash

echo "In 03-queue_run.sh"

mkdir ./log 2> /dev/null

CONTROL_FILE=${1:-'./control_files/example.ctl'}
USE_SLURM=${USE_SLURM:-'yes'}
JOB_NAME=`basename -s .ctl $CONTROL_FILE`

if [ "$USE_SLURM" = "yes" ]; then
   sbatch --priority=4000000000 --job-name=$JOB_NAME \
          --chdir=$(pwd) --output=$(pwd)/log/${JOB_NAME}_%j.txt \
          --export=ALL,CONTROL_FILE=$CONTROL_FILE 04-launch_run.sh

else
   ./04-launch_run.sh $CONTROL_FILE >& ./log/$JOB_NAME.txt
fi

exit 0
