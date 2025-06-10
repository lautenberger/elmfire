#!/bin/bash

# ELMFIRE version:
ELMFIRE_VER=2025.0609

# Begin by pre-processing source code files to comment out specific code blocks
SOURCE_FILES='elmfire elmfire_level_set elmfire_subs elmfire_vars elmfire_spread_rate elmfire_io'
NUM_CODE_BLOCK_TYPES_TO_PREPROCESS=4
CODE_BLOCK_TYPE[1]=smoke
CODE_BLOCK_TYPE[2]=wui
CODE_BLOCK_TYPE[3]=umd_spotting
CODE_BLOCK_TYPE[4]=suppression

for SOURCE_FILE in $SOURCE_FILES; do
   cp -f ../source/$SOURCE_FILE.f90 ../source/${SOURCE_FILE}_lowmem.f90
   for i in `seq 1 $NUM_CODE_BLOCK_TYPES_TO_PREPROCESS`; do
      NUM_BLOCKS=`cat ../source/${SOURCE_FILE}_lowmem.f90 | grep "! Start code block for ${CODE_BLOCK_TYPE[i]}" | wc -l`
      if [ "$NUM_BLOCKS" -gt "0" ]; then
         grep -n  "! Start code block for ${CODE_BLOCK_TYPE[i]}" ../source/${SOURCE_FILE}_lowmem.f90 > start.txt
         grep -n  "! End code block for ${CODE_BLOCK_TYPE[i]}"   ../source/${SOURCE_FILE}_lowmem.f90 > end.txt
         for j in `seq 1 $NUM_BLOCKS`; do
            START_LINE=`sed "${j}q;d" start.txt | cut -d: -f1`
            END_LINE=`sed "${j}q;d"   end.txt   | cut -d: -f1`
            for k in `seq $START_LINE $END_LINE`; do
               sed -i "${k}s/^/!/" ../source/${SOURCE_FILE}_lowmem.f90
            done
         done
      fi
   done
done
rm -f start.txt end.txt

# ELMFIRE uses several environment variables for compilation. If the default
# values specified on lines 14 - 16 below are not appropriate for your system,
# you can add lines similar to the following to your ~/.bashrc file:
#
# export ELMFIRE_FCOMPL_SERIAL_GNU=gfortran
# export ELMFIRE_FCOMPL_MPI_GNU=mpifort
# export ELMFIRE_INSTALL_DIR=$(pwd)/bin

export ELMFIRE_FCOMPL_SERIAL_GNU=${ELMFIRE_FCOMPL_SERIAL_GNU:-gfortran}
export ELMFIRE_FCOMPL_MPI_GNU=${ELMFIRE_FCOMPL_MPI_GNU:-mpifort}
export ELMFIRE_INSTALL_DIR=${ELMFIRE_INSTALL_DIR:-$(pwd)/bin}

# Build main ELMFIRE executables:

echo "Making elmfire_gnu_mpi_linux"
mkdir elmfire 2> /dev/null
cd elmfire
rm -f *.o *.mod elmfire_lowmeme
make -f ../Makefile_elmfire_lowmem gnu_mpi_linux
cp -f elmfire_lowmem ../bin/elmfire_lowmem_$ELMFIRE_VER
ln -fs ../bin/elmfire_lowmem_$ELMFIRE_VER ../bin/elmfire_lowmem
rm -f *.o *.mod elmfire_lowmem

echo "Making elmfire_gnu_mpi_debug_linux"
rm -f *.o *.mod elmfire_lowmem_debug
make -f ../Makefile_elmfire_lowmem gnu_mpi_debug_linux
cp -f elmfire_lowmem_debug ../bin/elmfire_lowmem_debug_$ELMFIRE_VER
ln -fs ../bin/elmfire_lowmem_debug_$ELMFIRE_VER ../bin/elmfire_lowmem_debug
rm -f *.o *.mod elmfire_lowmem_debug

echo "Cleaning up"
cd ..
rm -f -r elmfire elmfire_post ../source/*lowmem.f90

exit 0
