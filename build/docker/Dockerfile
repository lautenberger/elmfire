FROM ubuntu:22.04 as intermediate
ENV DEBIAN_FRONTEND noninteractive

RUN mkdir -p /elmfire/elmfire /scratch/elmfire /scratch/cloudfire && \
    apt-get update -y && \
    apt-get upgrade -y && \
    apt-get install --no-install-recommends -y \
        bc \
        build-essential \
        csvkit \
        gdal-bin \
        gfortran \
        jq \
        libopenmpi-dev \
        locales \
        nano \
        openmpi-bin \
        pigz \
        python3-minimal \
        python3-pip \
        sudo \
        wget && \
    pip3 install --no-cache-dir google-api-python-client python-dateutil && \
    python3 -m pip install grpcio grpcio-tools && \
    locale-gen en_US.UTF-8 && export LANG=en_US.UTF-8

COPY ./temp/ /elmfire/

WORKDIR /elmfire/elmfire/build/linux
RUN ./make_gnu.sh

WORKDIR /elmfire/cloudfire/code/linux
RUN ./make_gnu.sh && \
    cp -f ./bin/nfdrs* /elmfire/elmfire/build/linux/bin/ && \
    cp -f ./bin/raster* /elmfire/elmfire/build/linux/bin/ && \
    rm -f /elmfire/elmfire/build/linux/bin/*debug* && \
    rm -f -r /elmfire/cloudfire && \
    mkdir -p /elmfire/cloudfire/code/linux/bin && \
    mv /elmfire/elmfire/build/linux/bin/nfdrs* /elmfire/cloudfire/code/linux/bin/ && \
    mv /elmfire/elmfire/build/linux/bin/raster* /elmfire/cloudfire/code/linux/bin/ && \
    apt-get purge -y build-essential && \
    apt-get autoremove -y && \
    apt-get clean && \
    rm -f -r /var/lib/apt/lists/*

WORKDIR /

ENV ELMFIRE_BASE_DIR=/elmfire/elmfire
ENV CLOUDFIRE_BASE_DIR=/elmfire/cloudfire
ENV ELMFIRE_SCRATCH_BASE=/scratch/elmfire
ENV CLOUDFIRE_SCRATCH_BASE=/scratch/cloudfire
ENV CLOUDFIRE_SERVER=172.92.17.198
ENV USE_SLURM=no
