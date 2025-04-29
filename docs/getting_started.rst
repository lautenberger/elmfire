.. _getting_started:

Getting Started
===============

The steps below will install and configure ELMFIRE. This has been tested 
on a clean install of `Ubuntu Server 24.04.1 LTS 
<https://ubuntu.com/download/server>`_. Modifications to run on other 
Linux distributions may be required.

.. _install-prerequisites:

Install prerequisites
---------------------

The following commands will install packages needed to build and run ELMFIRE:

.. code-block:: console

   sudo apt-get update && sudo apt-get upgrade -y
   sudo apt-get install -y bc csvkit gdal-bin gfortran git jq libopenmpi-dev \
                           openmpi-bin pigz python3 python3-pip unzip wget zip

Several Python libraries / packages, including three related to Google Remote
Proecedure Call (gRPC), are needed to run the CloudFire microservices that provide
ELMFIRE with fuel, weather, and ignition geospatial data. These can be installed
system-wide as follows:

.. code-block:: console

   sudo pip3 install google-api-python-client grpcio grpcio-tools python-dateutil \
                     --break-system-packages

Alternatively, `virtualenv` can be used to prevent forcing a system-wide install as follows:

.. code-block:: console

   sudo apt-get install python3-virtualenv
   virtualenv $HOME/virtualenv/elmfire
   source $HOME/virtualenv/elmfire/bin/activate
   /$HOME/virtualenv/elmfire/bin/pip3 install google-api-python-client grpcio grpcio-tools \
                                              python-dateutil pytz

.. _clone-repo:

Clone ELMFIRE Github repository
-------------------------------

The current ELMFIRE repository can be cloned as follows:

.. code-block:: console

   git clone https://github.com/lautenberger/elmfire.git

Since this will clone the current repository, including all recent 
commits, for use in production environments a user may want to clone the 
latest stable release / branch instead, i.e.:

.. code-block:: console

   git clone --branch 2025.0429 --single-branch https://github.com/lautenberger/elmfire.git

.. _set-env-vars:

Set environment variables
-------------------------

ELMFIRE uses four environment variables:

1. ``ELMFIRE_SCRATCH_BASE``: Full path to a scratch directory where the 
   user has read/write access.
2. ``ELMFIRE_BASE_DIR``: Full path to the directory to which the ELMFIRE 
   Git repository was cloned (i.e., the directory containing 
   ``README.md``).
3. ``ELMFIRE_INSTALL_DIR``: Full path to the directory where the 
   ELMFIRE executable files will be installed (default: 
   ``$ELMFIRE_BASE_DIR/build/linux/bin``).
4. ``CLOUDFIRE_SERVER``: IP address of Cloudfire microservices server 
   (this should be set to ``worldgen.cloudfire.io``, more on this later).

The easiest way to specify these variables is by exporting them from 
your ``~/.bashrc`` file. For example, one could ``pico ~/.bashrc`` and 
then add the following lines (replacing ``/scratch/clauten``, 
``/home/clauten/elmfire``, etc.):

.. code-block:: console

   export ELMFIRE_SCRATCH_BASE=/scratch/clauten
   export ELMFIRE_BASE_DIR=/home/clauten/elmfire
   export ELMFIRE_INSTALL_DIR=$ELMFIRE_BASE_DIR/build/linux/bin
   export CLOUDFIRE_SERVER=worldgen.cloudfire.io
   export PATH=$PATH:$ELMFIRE_INSTALL_DIR:$ELMFIRE_BASE_DIR/cloudfire

These environment variables will be set on the next login or after 
sourcing the newly-edited file (``. ~/.bashrc``).

.. _build-executables:

Build ELMFIRE executables
-------------------------

ELMFIRE and its postprocessing tool can be built as follows:

.. code-block:: console

   cd $ELMFIRE_BASE_DIR/build/linux
   ./make_gnu.sh

Unless an error occurs, this will build the executables 
``elmfire_VERSION`` and ``elmfire_post_VERSION`` (where version is, for 
example, 2025.0429) and copy them to ``$ELMFIRE_INSTALL_DIR``. If 
this directory is not in the user's ``$PATH`` it should be added at this 
time. Note that two debug executables are also built.
