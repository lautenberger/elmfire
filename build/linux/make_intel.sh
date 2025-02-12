#!/bin/bash

# ELMFIRE version:
ELMFIRE_VER=2025.0212

# ELMFIRE uses several environment variables for compilation. If the default
# values specified on lines 14 - 16 below are not appropriate for your system,
# you can add lines similar to the following to your ~/.bashrc file:
#
# export ELMFIRE_FCOMPL_SERIAL_INTEL=ifort
# export ELMFIRE_FCOMPL_MPI_INTEL=mpifort
# export ELMFIRE_INSTALL_DIR=$(pwd)/bin

export ELMFIRE_FCOMPL_SERIAL_INTEL=${ELMFIRE_FCOMPL_SERIAL_INTEL:-ifort}
export ELMFIRE_FCOMPL_MPI_INTEL=${ELMFIRE_FCOMPL_MPI_INTEL:-mpifort}
export ELMFIRE_INSTALL_DIR=${ELMFIRE_INSTALL_DIR:-$(pwd)/bin}

# Build main ELMFIRE executables:

echo "Making elmfire_intel_mpi_linux"
mkdir elmfire 2> /dev/null
cd elmfire
rm -f *.o *.mod elmfire
make -f ../Makefile_elmfire intel_mpi_linux
cp -f elmfire $ELMFIRE_INSTALL_DIR/elmfire_$ELMFIRE_VER
ln -fs $ELMFIRE_INSTALL_DIR/elmfire_$ELMFIRE_VER $ELMFIRE_INSTALL_DIR/elmfire
rm -f *.o *.mod elmfire

echo "Making elmfire_intel_mpi_debug_linux"
rm -f *.o *.mod elmfire_debug
make -f ../Makefile_elmfire intel_mpi_debug_linux
cp -f elmfire_debug $ELMFIRE_INSTALL_DIR/elmfire_debug_$ELMFIRE_VER
ln -fs $ELMFIRE_INSTALL_DIR/elmfire_debug_$ELMFIRE_VER $ELMFIRE_INSTALL_DIR/elmfire_debug
rm -f *.o *.mod elmfire_debug

echo "Making elmfire_post"
cd ..
mkdir elmfire_post 2> /dev/null
cd elmfire_post
rm -f *.o *.mod elmfire_post
make -f ../Makefile_elmfire_post intel_linux
ln -fs $ELMFIRE_INSTALL_DIR/elmfire_post_$ELMFIRE_VER $ELMFIRE_INSTALL_DIR/elmfire_post
cp -f elmfire_post $ELMFIRE_INSTALL_DIR/elmfire_post_$ELMFIRE_VER

echo "Making elmfire_post_intel_debug_linux"
rm -f *.o *.mod elmfire_post_debug
make -f ../Makefile_elmfire_post intel_debug_linux
cp -f elmfire_post_debug $ELMFIRE_INSTALL_DIR/elmfire_post_debug_$ELMFIRE_VER
ln -fs $ELMFIRE_INSTALL_DIR/elmfire_post_debug_$ELMFIRE_VER $ELMFIRE_INSTALL_DIR/elmfire_post_debug
rm -f *.o *.mod

echo "Cleaning up"
cd ..
rm -f -r elmfire elmfire_post

exit 0
