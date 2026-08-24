FROM ubuntu:24.04 AS intermediate
ENV DEBIAN_FRONTEND=noninteractive

RUN mkdir -p /elmfire/elmfire /scratch/elmfire && \
    apt-get update -y && \
    apt-get upgrade -y && \
    apt-get install --no-install-recommends -y \
        bc \
        build-essential \
        bzip2 \
        ca-certificates \
        csvkit \
        gfortran \
        jq \
        libopenmpi-dev \
        locales \
        nano \
        openmpi-bin \
        pigz \
        sudo \
        wget && \
    locale-gen en_US.UTF-8 && export LANG=en_US.UTF-8

# Install micromamba (standalone mamba) and create the `elmfire` conda env.
# conda-forge provides Python, GDAL/rasterio/geopandas, and the gdal CLI tools,
# so no python/gdal packages come from apt (avoids apt-vs-pip numpy/GDAL clashes).
ENV MAMBA_ROOT_PREFIX=/opt/conda
RUN wget -qO- https://micro.mamba.pm/api/micromamba/linux-64/latest | \
        tar -xvj -C /usr/local/bin --strip-components=1 bin/micromamba

# Create the env first (cached unless environment.yml changes).
COPY environment.yml /tmp/environment.yml
RUN micromamba create -y -n elmfire -f /tmp/environment.yml && \
    micromamba clean --all --yes

# Put the env on PATH so `python`, `gdalwarp`, etc. resolve without activation.
ENV PATH=/opt/conda/envs/elmfire/bin:$PATH

# We never `conda activate` the env (just prepend its bin/ to PATH), so conda's
# activation scripts that normally export PROJ_DATA/GDAL_DATA never run. Without
# them PROJ can't find proj.db and the GDAL CLI tools emit "PROJ: ... Open of
# .../share/proj failed", reporting EPSG:-1. That makes verify_guide.py's
# find_working_gdal() reject every candidate ("no working GDAL found"). Point
# them at the env's data dirs explicitly.
ENV PROJ_DATA=/opt/conda/envs/elmfire/share/proj
ENV GDAL_DATA=/opt/conda/envs/elmfire/share/gdal


FROM intermediate

COPY . /elmfire/elmfire

WORKDIR /elmfire/elmfire/build/linux
RUN ./make_gnu.sh

RUN apt-get purge -y build-essential && \
    apt-get autoremove -y && \
    apt-get clean && \
    rm -f -r /var/lib/apt/lists/*

WORKDIR /elmfire

# ELMFIRE_VER is the version-suffixed binary name (elmfire_$ELMFIRE_VER) that
# the bundled tutorial/example scripts invoke. The repo-root VERSION file is the
# single source of truth; this ARG defaults to it and can be overridden with
# --build-arg ELMFIRE_VER=... The unsuffixed `elmfire` symlink always points at
# the current build regardless of this value.
ARG ELMFIRE_VER
ENV ELMFIRE_VER=${ELMFIRE_VER:-1.1}
ENV ELMFIRE_BASE_DIR=/elmfire/elmfire
ENV ELMFIRE_SCRATCH_BASE=/scratch/elmfire
ENV ELMFIRE_INSTALL_DIR=$ELMFIRE_BASE_DIR/build/linux/bin
ENV CLOUDFIRE_SERVER=worldgen.cloudfire.io
ENV PATH=$PATH:$ELMFIRE_INSTALL_DIR:$ELMFIRE_BASE_DIR/cloudfire