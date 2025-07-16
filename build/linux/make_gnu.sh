#!/bin/bash

# ELMFIRE version:
ELMFIRE_VER=2025.0716

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

if [ -z ${ELMFIRE_LOWMEM} ]; then
    export ELMFIRE_PRECOMPILER_FLAGS="-D_SMOKE -D_WUI -D_UMDSPOTTING -D_SUPPRESSION"
    export ELMFIRE_BIN_SUFFIX=""
    export ELMFIRE_OBJECTS="elmfire_vars.o sort.o elmfire_init.o elmfire_namelists.o elmfire_subs.o elmfire_spread_rate.o  elmfire_ignition.o elmfire_io.o elmfire_spotting.o elmfire_suppression.o elmfire_spotting_superseded.o elmfire_calibration.o elmfire_level_set.o elmfire.o"
else
    echo "LOWMEM build, removing precompiler flags: smoke wui umdspotting suppression"
    export ELMFIRE_PRECOMPILER_FLAGS=""
    export ELMFIRE_BIN_SUFFIX="_lowmem"
    export ELMFIRE_OBJECTS="elmfire_vars.o sort.o elmfire_init.o elmfire_namelists.o elmfire_subs.o elmfire_spread_rate.o  elmfire_ignition.o elmfire_io.o elmfire_spotting.o elmfire_spotting_superseded.o elmfire_calibration.o elmfire_level_set.o elmfire.o"
fi

# Build main ELMFIRE executables:

echo "Making elmfire_gnu_mpi_linux"
mkdir elmfire 2> /dev/null
cd elmfire
rm -f *.o *.mod elmfire
make -f ../Makefile_elmfire gnu_mpi_linux
cp -f elmfire $ELMFIRE_INSTALL_DIR/elmfire_$ELMFIRE_VER
ln -fs $ELMFIRE_INSTALL_DIR/elmfire_$ELMFIRE_VER $ELMFIRE_INSTALL_DIR/elmfire$ELMFIRE_BIN_SUFFIX
rm -f *.o *.mod elmfire

echo "Making elmfire_gnu_mpi_debug_linux"
rm -f *.o *.mod elmfire_debug
make -f ../Makefile_elmfire gnu_mpi_debug_linux
cp -f elmfire_debug $ELMFIRE_INSTALL_DIR/elmfire_debug_$ELMFIRE_VER
ln -fs $ELMFIRE_INSTALL_DIR/elmfire_debug_$ELMFIRE_VER $ELMFIRE_INSTALL_DIR/elmfire_debug$ELMFIRE_BIN_SUFFIX
rm -f *.o *.mod elmfire_debug

echo "Making elmfire_post"
cd ..
mkdir elmfire_post 2> /dev/null
cd elmfire_post
rm -f *.o *.mod elmfire_post
make -f ../Makefile_elmfire_post gnu_linux
cp -f elmfire_post $ELMFIRE_INSTALL_DIR/elmfire_post_$ELMFIRE_VER
ln -fs $ELMFIRE_INSTALL_DIR/elmfire_post_$ELMFIRE_VER $ELMFIRE_INSTALL_DIR/elmfire_post$ELMFIRE_BIN_SUFFIX

echo "Making elmfire_post_gnu_debug_linux"
rm -f *.o *.mod elmfire_post_debug
make -f ../Makefile_elmfire_post gnu_debug_linux
cp -f elmfire_post_debug $ELMFIRE_INSTALL_DIR/elmfire_post_debug_$ELMFIRE_VER
ln -fs $ELMFIRE_INSTALL_DIR/elmfire_post_debug_$ELMFIRE_VER $ELMFIRE_INSTALL_DIR/elmfire_post_debug$ELMFIRE_BIN_SUFFIX

rm -f *.o *.mod

echo "Cleaning up"
cd ..
rm -f -r elmfire elmfire_post

exit 0
