FROM ubuntu:22.04 as intermediate
ENV DEBIAN_FRONTEND noninteractive

RUN mkdir -p /elmfire/elmfire /scratch/elmfire && \
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


FROM intermediate

COPY . /elmfire/elmfire

WORKDIR /elmfire/elmfire/build/linux
RUN ./make_gnu.sh

RUN apt-get purge -y build-essential && \
    apt-get autoremove -y && \
    apt-get clean && \
    rm -f -r /var/lib/apt/lists/*

WORKDIR /

ENV ELMFIRE_VER=2025.0212
ENV ELMFIRE_BASE_DIR=/elmfire/elmfire
ENV ELMFIRE_SCRATCH_BASE=/scratch/elmfire
ENV ELMFIRE_INSTALL_DIR=$ELMFIRE_BASE_DIR/build/linux/bin
ENV CLOUDFIRE_SERVER=worldgen.cloudfire.io
ENV PATH=$PATH:$ELMFIRE_INSTALL_DIR:$ELMFIRE_BASE_DIR/cloudfire
