#!/bin/bash

# Always operate from the directory this script lives in, so the relative
# paths below (../Makefile_elmfire, etc.) resolve no matter where it's invoked.
cd "$(dirname "$(readlink -f "$0")")"

# ELMFIRE version: the repo-root VERSION file is the single source of truth.
# Override at build time by exporting ELMFIRE_VER. The compiled-in banner
# (VERSIONSTRING in ../source/elmfire.f90) is kept in sync below.
ELMFIRE_VER=${ELMFIRE_VER:-$(tr -d '[:space:]' < ../../VERSION)}
sed -i "s/VERSIONSTRING='ELMFIRE [^']*'/VERSIONSTRING='ELMFIRE $ELMFIRE_VER'/" ../source/elmfire.f90

# Fast/debug build: compile only the main elmfire executable and skip the
# gprof/block/perf/debug variants and elmfire_post. Enable with either:
#   ./make_gnu.sh elmfire      (or: fast / --fast / -f)
#   ELMFIRE_FAST=1 ./make_gnu.sh
case "$1" in
    elmfire|fast|--fast|-f) export ELMFIRE_FAST=1 ;;
esac

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
    export ELMFIRE_OBJECTS="elmfire_vars.o sort.o elmfire_subs.o elmfire_init.o elmfire_namelists.o elmfire_spread_rate.o  elmfire_ignition.o elmfire_io.o elmfire_spotting.o elmfire_suppression.o elmfire_spotting_superseded.o elmfire_calibration.o elmfire_level_set.o elmfire.o"
else
    echo "LOWMEM build, removing precompiler flags: smoke wui umdspotting suppression"
    export ELMFIRE_PRECOMPILER_FLAGS=""
    export ELMFIRE_BIN_SUFFIX="_lowmem"
    export ELMFIRE_OBJECTS="elmfire_vars.o sort.o elmfire_subs.o elmfire_init.o elmfire_namelists.o elmfire_spread_rate.o  elmfire_ignition.o elmfire_io.o elmfire_spotting.o elmfire_spotting_superseded.o elmfire_calibration.o elmfire_level_set.o elmfire.o"
fi

# Build main ELMFIRE executables:

echo "Making elmfire_gnu_mpi_linux"
mkdir elmfire 2> /dev/null
cd elmfire
rm -f *.o *.mod elmfire
make -f ../Makefile_elmfire gnu_mpi_linux
cp -f elmfire $ELMFIRE_INSTALL_DIR/elmfire_$ELMFIRE_VER
ln -fs $ELMFIRE_INSTALL_DIR/elmfire_$ELMFIRE_VER $ELMFIRE_INSTALL_DIR/elmfire$ELMFIRE_BIN_SUFFIX
ln -fs $ELMFIRE_INSTALL_DIR/elmfire_$ELMFIRE_VER $ELMFIRE_INSTALL_DIR/elmfire$ELMFIRE_BIN_SUFFIX
rm -f *.o *.mod elmfire

if [ -n "${ELMFIRE_FAST}" ]; then
    echo "FAST build: skipping gprof/block/perf/debug variants and elmfire_post"
    cd ..
    rm -f -r elmfire
    exit 0
fi

echo "Making elmfire_gnu_mpi_gprof_linux"
rm -f *.o *.mod elmfire_gprof
make -f ../Makefile_elmfire gnu_mpi_gprof_linux
cp -f elmfire_gprof $ELMFIRE_INSTALL_DIR/elmfire_gprof_$ELMFIRE_VER
ln -fs $ELMFIRE_INSTALL_DIR/elmfire_gprof_$ELMFIRE_VER $ELMFIRE_INSTALL_DIR/elmfire_gprof$ELMFIRE_BIN_SUFFIX
rm -f *.o *.mod elmfire_gprof

echo "Making elmfire_gnu_mpi_block_linux"
rm -f *.o *.mod elmfire_block
make -f ../Makefile_elmfire gnu_mpi_block_linux
cp -f elmfire_block $ELMFIRE_INSTALL_DIR/elmfire_block_$ELMFIRE_VER
ln -fs $ELMFIRE_INSTALL_DIR/elmfire_block_$ELMFIRE_VER $ELMFIRE_INSTALL_DIR/elmfire_block$ELMFIRE_BIN_SUFFIX
rm -f *.o *.mod elmfire_block

echo "Making elmfire_gnu_mpi_perf_linux"
rm -f *.o *.mod elmfire_perf
make -f ../Makefile_elmfire gnu_mpi_perf_linux
cp -f elmfire_perf $ELMFIRE_INSTALL_DIR/elmfire_perf_$ELMFIRE_VER
ln -fs $ELMFIRE_INSTALL_DIR/elmfire_perf_$ELMFIRE_VER $ELMFIRE_INSTALL_DIR/elmfire_perf$ELMFIRE_BIN_SUFFIX
rm -f *.o *.mod elmfire_perf

echo "Making elmfire_gnu_mpi_debug_linux"
rm -f *.o *.mod elmfire_debug
make -f ../Makefile_elmfire gnu_mpi_debug_linux
cp -f elmfire_debug $ELMFIRE_INSTALL_DIR/elmfire_debug_$ELMFIRE_VER
ln -fs $ELMFIRE_INSTALL_DIR/elmfire_debug_$ELMFIRE_VER $ELMFIRE_INSTALL_DIR/elmfire_debug$ELMFIRE_BIN_SUFFIX
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
