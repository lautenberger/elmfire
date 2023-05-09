FROM ubuntu:22.04 as intermediate
LABEL org.opencontainers.image.authors="Chris Lautenberger <chris@cloudfire.ai>"
ENV DEBIAN_FRONTEND noninteractive

RUN mkdir -p /elmfire/elmfire && \
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
    locale-gen en_US.UTF-8 && export LANG=en_US.UTF-8 && \
    apt-get clean -y && \
    rm -rf /var/lib/apt/lists/*

COPY ./ /elmfire/elmfire/

WORKDIR /elmfire/elmfire/build/linux

RUN ./make_gnu.sh && \
    apt-get purge -y build-essential && \
    apt-get autoremove -y

WORKDIR /

ENV CLOUDFIRE_SCRATCH_BASE=/scratch
ENV ELMFIRE_SCRATCH_BASE=/scratch
ENV CLOUDFIRE_BASE_DIR=/elmfire/cloudfire
ENV ELMFIRE_BASE_DIR=/elmfire/elmfire
ENV CLOUDFIRE_SERVER=172.92.17.198
ENV USE_SLURM=no
