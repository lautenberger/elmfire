# -------- Stage 1: Build elmfire binaries --------
FROM ubuntu:22.04 AS builder
ENV DEBIAN_FRONTEND=noninteractive LANG=en_US.UTF-8

# Base build deps + toolchain + (temporarily) GDAL headers (compile stage)
RUN apt-get update -y && apt-get install -y --no-install-recommends \
      software-properties-common ca-certificates gnupg \
      build-essential gfortran openmpi-bin libopenmpi-dev \
      bc csvkit jq nano pigz sudo wget locales \
      gdal-bin libgdal-dev proj-bin libproj-dev \
      python3 python3-pip \
    && locale-gen en_US.UTF-8

# Minimal Python bits some tools/scripts expect
RUN pip3 install --no-cache-dir google-api-python-client python-dateutil grpcio grpcio-tools

# Copy the whole repo (which already contains vnv_suite/ at the right place)
# IMPORTANT: build context must be the elmfire repo root.
COPY . /elmfire/elmfire

# Build elmfire
WORKDIR /elmfire/elmfire/build/linux
RUN ./make_gnu.sh

# -------- Stage 2: Runtime + VnV environment --------
FROM ubuntu:22.04
ENV DEBIAN_FRONTEND=noninteractive LANG=en_US.UTF-8

# Create expected dirs early
RUN mkdir -p /elmfire/elmfire /scratch/elmfire

# Upgrade GDAL/PROJ (ubuntugis) and install runtime deps
RUN apt-get update -y && apt-get install -y --no-install-recommends \
      software-properties-common ca-certificates gnupg && \
    add-apt-repository -y ppa:ubuntugis/ubuntugis-unstable && \
    apt-get update -y && apt-get install -y --no-install-recommends \
      # MPI runtime
      openmpi-bin \
      # GDAL/PROJ runtime + Python geospatial stack from APT
      gdal-bin libgdal-dev proj-bin libproj-dev \
      python3 python3-pip \
      python3-rasterio python3-fiona python3-shapely python3-geopandas \
      # quality-of-life + tools used by suite/scripts
      bc csvkit jq nano pigz sudo wget make locales \
    && locale-gen en_US.UTF-8 \
    && rm -rf /var/lib/apt/lists/*

# Bring in the compiled repo from builder (sources + built bin)
COPY --from=builder /elmfire /elmfire

# ---- Python deps for VnV (excluding geospatial already provided via APT) ----
WORKDIR /elmfire/elmfire/vnv_suite
RUN python3 -m pip install --upgrade pip && \
    python3 -m pip install --no-cache-dir -r requirements.txt  && \
    python3 -m pip install --no-cache-dir google-cloud-storage crcmod

# ---- Environment ----
ENV ELMFIRE_VER=2025.0717 \
    ELMFIRE_BASE_DIR=/elmfire/elmfire \
    ELMFIRE_SCRATCH_BASE=/scratch/elmfire \
    ELMFIRE_INSTALL_DIR=/elmfire/elmfire/build/linux/bin \
    ELMFIRE_BIN=/elmfire/elmfire/build/linux/bin/elmfire \
    CLOUDFIRE_SERVER=worldgen.cloudfire.io \
    ROOT_DIR=/elmfire/elmfire/vnv_suite \
    PATH=$PATH:/elmfire/elmfire/build/linux/bin:/elmfire/elmfire/cloudfire

# Configure the VnV suite (points Makefile at system GDAL)
RUN make -C /elmfire/elmfire/vnv_suite configure PATH_TO_GDAL=/usr/bin

# Default working dir: the suite
WORKDIR /elmfire/elmfire/vnv_suite
