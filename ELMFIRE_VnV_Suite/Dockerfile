FROM elmfire:wuerefac
ENV DEBIAN_FRONTEND=noninteractive

# Upgrade GDAL/PROJ (gives GDAL ≥ 3.5 on 22.04)
RUN apt-get update -y && apt-get install -y --no-install-recommends \
      software-properties-common ca-certificates gnupg && \
    add-apt-repository -y ppa:ubuntugis/ubuntugis-unstable && \
    apt-get update -y && \
    apt-get install -y --no-install-recommends \
      gdal-bin libgdal-dev proj-bin libproj-dev \
      # Python geospatial stack from APT (compatible with the above GDAL)
      python3-rasterio python3-fiona python3-shapely python3-geopandas \
      make && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /workspace/ELMFIRE_VnV_Suite

# IMPORTANT: remove rasterio/fiona/geopandas/shapely from requirements.txt
# (APT already installed them). Keep your other deps there.
COPY requirements.txt .
RUN python3 -m pip install --upgrade pip && \
    python3 -m pip install --no-cache-dir -r requirements.txt

COPY . /workspace/ELMFIRE_VnV_Suite

ENV ELMFIRE_BASE_DIR=/elmfire/elmfire \
    ELMFIRE_INSTALL_DIR=/elmfire/elmfire/build/linux/bin \
    ELMFIRE_BIN=/elmfire/elmfire/build/linux/bin/elmfire \
    ELMFIRE_SCRATCH_BASE=/scratch/elmfire \
    PATH=$PATH:/elmfire/elmfire/build/linux/bin \
    ROOT_DIR=/workspace/ELMFIRE_VnV_Suite

RUN make configure PATH_TO_GDAL=/usr/bin
