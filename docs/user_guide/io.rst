Basic Inputs and Outputs
========================

ELMFIRE requires two primary types of inputs:

   1. Configuration parameters specified in a plain text input file 
   (often, ``elmfire.data``) consisting of several Fortran namelists. 
   This file format and its associated syntax is not specific to ELMFIRE 
   and is used by other Fortran-based scientific models such as the
   `Fire Dynamics Simulator <https://pages.nist.gov/fds-smv>`_ and 
   `Weather Research and Forecasting 
   <https://www.mmm.ucar.edu/models/wrf>`_ model.

   2. Geospatial inputs such as fuel, weather, topography, and structure 
   density. ELMFIRE reads only raster (e.g., GeoTiff) inputs. Vector 
   inputs such as a polygon corresponding to the perimeter of an actively 
   burning fire must be burned into a raster.

.. _inputs:

Inputs
------

.. _fuelswxtopo:

Fuels, weather, and topography
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``$ELMFIRE_BASE_DIR/tutorials/01-constant-wind/elmfire.data.in`` 
file contains an ``&INPUTS`` namelist group with the following entries:

.. code-block::

   &INPUTS
   FUELS_AND_TOPOGRAPHY_DIRECTORY = './inputs'
   ASP_FILENAME                   = 'asp'
   CBD_FILENAME                   = 'cbd'
   CBH_FILENAME                   = 'cbh'
   CC_FILENAME                    = 'cc'
   CH_FILENAME                    = 'ch'
   DEM_FILENAME                   = 'dem'
   FBFM_FILENAME                  = 'fbfm40'
   SLP_FILENAME                   = 'slp'
   ADJ_FILENAME                   = 'adj'
   PHI_FILENAME                   = 'phi'
   DT_METEOROLOGY                 = 3600.0
   WEATHER_DIRECTORY              = './inputs'
   WS_FILENAME                    = 'ws'
   WD_FILENAME                    = 'wd'
   M1_FILENAME                    = 'm1'
   M10_FILENAME                   = 'm10'
   M100_FILENAME                  = 'm100'
   LH_MOISTURE_CONTENT            = 30.0
   LW_MOISTURE_CONTENT            = 60.0
   /

ELMFIRE reads GIS inputs in GeoTiff format. The keyword 
``FUELS_AND_TOPOGRAPHY_DIRECTORY`` points ELMFIRE to the directory 
containing ten single-band fuels/topography raster:

   #. ``ASP_FILENAME``: Topographic aspect in degrees (16-bit integer)
   #. ``CBD_FILENAME``: Canopy bulk density in units of 100 kg per meter cubed (16-bit integer)
   #. ``CBH_FILENAME``: Canopy base height in units of 10 meters (16-bit integer)
   #. ``CC_FILENAME``: Canopy cover in units of percent (16-bit integer)
   #. ``CH_FILENAME``: Canopy height in units of 10 meters (16-bit integer)
   #. ``DEM_FILENAME``: Digital elevation model data in units of meters (16-bit integer)
   #. ``FBFM_FILENAME``: Fire behavior fuel model file (16-bit integer)
   #. ``SLP_FILENAME``: Topographic slope in degrees (16-bit integer)
   #. ``ADJ_FILENAME``: Surface spread rate adjustment factor (32-bit float)
   #. ``PHI_FILENAME``: Initial :math:`{\phi}` (level set variable) field (32-bit float)

Filenames should be specified without a suffix (*e.g.*, ``'slp'``, not 
``'slp.tif'`` because ELMFIRE automatically appends the .tif suffix to 
filenames. All GIS inputs must have the same projection, resolution/cell 
size, and extents. ELMFIRE does not reproject, warp, or clip GIS inputs 
and there is currently no error trapping to catch mismatched inputs. The 
user must ensure that all GIS inputs are self-consistent.

In addition to the above ten fuels/topography inputs, ELMFIRE may be 
configured to read additional rasters to quantify impacts to assets at 
risk (structures, timber, *etc.*).

The keyword ``WEATHER_DIRECTORY`` points ELMFIRE to a directory 
containing five multi-band (or single-band) wind/weather 32-bit float 
rasters:

   #. ``WS_FILENAME``: 20-ft wind speed in mph
   #. ``WD_FILENAME``: 20-ft wind direction in degrees
   #. ``M1_FILENAME``: 1-hour dead fuel moisture content in %
   #. ``M10_FILENAME``: 10-hour dead fuel moisture content in %
   #. ``M100_FILENAME``: 100-hour dead fuel moisture content in %

If the rasters specified above are single-band, then wind/weather 
conditions are assumed to be unchanging for the duration of the 
simulation. Transient wind/weather streams can be provided as input by 
using multi-band, or "stacked", rasters. In that case, the parameter 
``DT_METEOROLOGY`` should be specified on the ``&INPUT`` line. 
``DT_METEOROLOGY`` is the time interval (in seconds) between bands in 
the weather rasters. ``DT_METEOROLOGY`` is commonly set to 3600 seconds 
due to the use of hourly reanalysis or forecast products to drive fire 
spread simulation. If wind speed is provided at 10 meters instead of 20 
feet, the parameter ``WS_AT_10M`` should be set to ``.TRUE.``.

By default, live fuel moisture content (herbaceous, woody, and foliar) 
are spatially uniform and temporally unchanging. They are specified in 
the ``&INPUTS`` namelist group via the keywords ``LH_MOISTURE_CONTENT``, 
``LW_MOISTURE_CONTENT``, and ``FOLIAR_MOISTURE_CONTENT``, respectively. 
Spatially varying live fuel moisture content can be specified by setting 
``USE_CONSTANT_LH = .FALSE.``, ``USE_CONSTANT_LW = .FALSE.``, and 
``USE_CONSTANT_FMC = .FALSE.``. This directs ELMFIRE to read live fuel 
moisture from the rasters specified by ``MLH_FILENAME``, 
``MLW_FILENAME``, ``FMC_FILENAME`` (live herbaceous, live woody, and 
foliar, respectively). Even though live fuel moistures will typically 
change very little during a fire forecast (or hindcast), ELMFIRE expects 
the live fuel moisture rasters to have the same number of bands as the 
weather rasters, with the time interval between bands specified by 
``DT_METEOROLOGY``.

.. _assetsrisk:

Assets at Risk
~~~~~~~~~~~~~~

An important part of quantifying fire risk is assessing potential 
impacts to assets at risk. ELMFIRE is currently capable of quantifying 
impacts to three assets at risk. Since some of the most relevant assets 
at risk are population, real estate/structures, and land/timber, the 
keywords that are used to specify assets at risk in the ``&INPUTS`` 
namelist group are hardcoded as ``REAL_ESTATE``, ``POPULATION_DENSITY``, 
and ``LAND_VALUE`` although each of these is treated identically. Since 
other assets at risk may be relevant (cultural resources, sensitive 
habitat, watershed, *etc.*), the total number of assets at risk 
eventually will be expanded. However, for the time being, assets at risk 
are specified as follows:

   * Land value: Set ``USE_LAND_VALUE = .TRUE.`` and specify the land 
     value filename via ``LAND_VALUE_FILENAME``
   * Real estate value: Set ``USE_REAL_ESTATE_VALUE = .TRUE.`` and 
     specify the real estate value filename via 
     ``REAL_ESTATE_VALUE_FILENAME``
   * Population density: Set ``USE_POPULATION_DENSITY = .TRUE.`` and 
     specify the population density filename via 
     ``POPULATION_DENSITY_FILENAME``

.. note::

   Assets at risk are read in from the ``FUELS_AND_TOPOGRAPHY`` 
   directory. Additionally, all assets at risk should be Float32 GeoTiff 
   rasters. Units should be quantity per acre, *e.g.* structures per 
   acre, population per acre, $ per acre, *etc.*

If assets at risk rasters are provided as input, ELMFIRE will sum total 
impacts by integrating fire area over asset at risk density and report 
this in the ``fire_size_stats.csv`` output file. ELMFIRE will also 
create impact rasters ``affected_land_value.tif``, 
``affected_real_estate_value.tif``, and ``affected_population.tif``.

Computational domain size, extents, and resolution
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Input parameters controlling configuration of the computational domain 
are specified via the ``&COMPUTATIONAL_DOMAIN`` namelist group. The 
computational domain has the same extents of the input GIS fuels data, 
but its resolution can be the same or finer than the fuels inputs, 
meaning a computational domain with 10 m grid spacing can be used even 
if fuels inputs have a spatial resolution of 30 m.

The primary constraint is that the computational domain must 
fall completely within the GIS input data. This means that the 
computational domain can be the same size as or smaller than the input 
GIS data. Its spatial resolution can also be the same or finer than the 
input GIS fuels data,

The computational domain is specified by the following parameters:

   * ``A_SRS``: Projection of output files. Typically, a proj string 
     would be used, i.e. ``A_SRS = 'EPSG:32610'``.
   * ``COMPUTATIONAL_DOMAIN_CELLSIZE``: spatial resolution of the 
     computational domain, uniform in the x and y directions. This is 
     commonly set to the spatial resolution of the input fuels layers 
     (often 30 m) but can be set to a smaller value (e.g., 10 m) if a 
     more highly resolved simulation is desired.
   * ``COMPUTATIONAL_DOMAIN_XLLCORNER``: x-coordinate of the lower left 
     corner of the fuels inputs. 
   * ``COMPUTATIONAL_DOMAIN_YLLCORNER``: y-coordinate of the lower left 
     corner of the fuels inputs. 

.. note::

   This namelist group will soon be deleted since 
   ``COMPUTATIONAL_DOMAIN_XLLCORNER``, 
   ``COMPUTATIONAL_DOMAIN_YLLCORNER``, and ``A_SRS`` can be determined 
   internally from the fuels inputs' metadata. When this happens, 
   ``COMPUTATIONAL_DOMAIN_CELLSIZE`` will be moved to the ``&SIMULATOR`` 
   namelist group.

.. _time:

Time
~~~~

Parameters related to time (simulation duration, computational timestep, 
*etc.*) are specified via the ``&TIME_CONTROL`` namelist group. A sample
``&TIME_CONTROL`` namelist group with key inputs is shown below: 

.. code-block::

   &TIME_CONTROL
   SIMULATION_TSTART   = 0.0
   SIMULATION_TSTOP    = 3600.0
   SIMULATION_DT       = 5.0 
   SIMULATION_DTMAX    = 600.0
   TARGET_CFL          = 0.4
   DT_INTERPOLATE_M1   = 300.0
   DT_INTERPOLATE_M10  = 3000.0
   DT_INTERPOLATE_M100 = 30000.0
   DT_INTERPOLATE_MLH  = 9E8
   DT_INTERPOLATE_MLW  = 9E8
   DT_INTERPOLATE_FMC  = 9E8
   DT_INTERPOLATE_WIND = 300.0
   /

Simulation start and stop times are specified via the keywords 
``SIMULATION_TSTART`` and ``SIMULATION_TSTOP``, respectively. These 
parameters have units of seconds, so a 12-hour simulation corresponds to 
``SIMULATION_TSTOP = 43200``. The default value of 
``SIMULATION_TSTART`` is 0 seconds, meaning computations start at 
:math:`{t}` = 0 seconds. This is generally appropriate for simulations 
driven by idealized or synthetic weather data. For transient 
wind/weather/fuel moisture multi-band rasters, Band 1 always corresponds 
to :math:`{t}` = 0 seconds in ELMFIRE.

When simulating real fires driven by transient, often hourly, weather 
streams it is usually desirable to start a fire spread simulation at a 
time > 0 seconds. Assuming that hourly weather fields are provided as 
input and fire's time of ignition is 14:20, ``SIMULATION_TSTART`` should 
be set to ``1200.0``, *i.e.*, 20 minutes after the hour. In this 
particular case, Band 1 in all transient raster inputs should correspond 
to 14:00 and Band 2 should correspond to 15:00. 

The initial timestep is specified with the ``SIMULATION_DT`` keyword. 
The timestep is automatically adjusted at runtime based on the 
Courant-Friedrichs-Lewy (CFL) conditions. The target CFL number can be 
specified by ``TARGET_CFL``. Since the internal timestep will change 
during a simulation, an upper limit on the allowable timestep can be 
specified with the ``SIMULATION_DTMAX`` keyword.

Since wind/weather fields are often provided at hourly intervals but 
ELMFIRE's computational timestep is usually on the order of a few to at 
most tens of seconds, ELMFIRE uses linear interpolation to determine 
wind/weather/fuel moisture conditions at intermediate times. This 
interpolation can be computationally expensive, so the user is provided 
with some control over the interpolation frequency. The keywords 
``DT_INTERPOLATE_M1``, ``DT_INTERPOLATE_M10``, and 
``DT_INTERPOLATE_M100`` control the time between interpolations for 
1-hour, 10-hour, and 100-hour fuel moistures. Wind speed and wind 
direction are controlled by ``DT_INTERPOLATE_WS`` and 
``DT_INTERPOLATE_WD``. As with other temporal inputs, units are 
seconds.


.. _fireinit:

Fire Initialization
~~~~~~~~~~~~~~~~~~~

The two primary methods to initialize a fire spread simulation include 
point source ignitions and active fire perimeter initialization. These 
methods can be used concurrently, *e.g.* to simulate an active fire 
perimeter with additional point ignitions or spot fire initiation 
outside of the fire perimeter.

Point source ignitions
^^^^^^^^^^^^^^^^^^^^^^

One or more point source ignitions can be specified on the 
``&SIMULATOR`` namelist group via the keywords ``X_IGN(:)``, 
``Y_IGN(:)``, and ``T_IGN(:)`` which respectively control point 
ignitions' :math:`{x}`- and :math:`{y}`-coordinates, and time of 
ignitions. As an example, the following lines specify two separate point 
source ignitions:

.. code-block::

   NUM_IGNITIONS = 2
   X_IGN(1)      = 1000.0
   Y_IGN(1)      = 1000.0
   T_IGN(1)      = 0.0
   X_IGN(2)      = 2000.0
   Y_IGN(2)      = 2000.0
   T_IGN(2)      = 7200.0

The first ignition occurs at (:math:`{x}`, :math:`{y}`) = (1000.0, 
1000.0) at simulation time 0.0 seconds and the second occurs at 
(:math:`{x}`, :math:`{y}`) = (2000.0, 2000.0) at simulation time = 
7200.0 seconds. The keyword ``NUM_IGNITIONS`` specifies the total number 
of point source ignitions. Ignitions should be numbered sequentially 
starting at 1 and ending at ``NUM_IGNITIONS``. The number of point 
source ignitions is currently limited to 100.

Active fire perimeters
^^^^^^^^^^^^^^^^^^^^^^

As described in ELMFIRE's :ref:`tech_ref`, fire front position is 
tracked by solving a conservation equation for the level set variable 
:math:`{\phi}` where unburned areas correspond to :math:`{\phi}` > 0, 
burned areas correspond to :math:`{\phi}` < 0, and the fire front 
position is the level set corresponding to :math:`{\phi}` = 0. At the 
start of a simulation ELMFIRE reads the initial :math:`{\phi}` field 
from a 32-bit floating point raster with filename ``PHI_FILENAME`` as 
specified in the ``&INPUTS`` namelist group.

If there is no active fire at the start of a simulation, then all pixels 
in the ``PHI_FILENAME`` raster should be initialized with a single 
start value greater than 0 (usually 1.0). An initial fire front 
position can be specified by burning a value less than 0 (usually -1.0) 
into the ``PHI_FILENAME`` raster. All pixels with an initial 
:math:`{\phi}` value less than 0 will be marked as burned and fire 
spread will be initiated from those pixels.

Extinguished or "cold" segments of the fire perimeter can be simulated 
by modifying the fuel model raster to have a non-burnable fuel model in 
extinguished segments of the fire perimeter.

.. _outputs:

Outputs
-------

A sample ``&OUTPUTS`` namelist group is shown below:

.. code-block::

   &OUTPUTS
   OUTPUTS_DIRECTORY         = './outputs'
   DTDUMP                    = 3600.
   DUMP_FLIN                 = .TRUE.
   DUMP_SPREAD_RATE          = .TRUE.
   DUMP_SURFACE_FIRE         = .TRUE.
   DUMP_TIME_OF_ARRIVAL      = .TRUE.
   /

The keyword ``OUTPUTS_DIRECTORY`` specifies the directory to which 
ELMFIRE will write its output files. This output directory must exist at 
run-time; it will not be automatically created. Outputs will be dumped 
every ``DTDUMP`` seconds.

In general, outputs to be dumped are specified using a logical keyword 
that begins with ``DUMP_``. The following is a summary of primary 
raster outputs and the logical keywords that control whether 
they are written to disk:

   * Crown fire occurrence (-): ``DUMP_CROWN_FIRE``
   * Flame length (ft): ``DUMP_FLAME_LENGTH``
   * Fireline intensity (kW/m): ``DUMP_FLIN``
   * Heat per unit area (kJ/m^2): ``DUMP_HPUA``
   * Reaction intensity (kW/m^2): ``DUMP_REACTION_INTENSITY``
   * Surface fire occurrence (%): ``DUMP_SURFACE_FIRE``
   * Time of arrival (s): ``DUMP_TIME_OF_ARRIVAL``
   * Spread rate (ft/min): ``DUMP_SPREAD_RATE``
   * 20-ft wind direction (): ``DUMP_WD20``
   * 20-ft wind speed (mph): ``DUMP_WS20``

Output filenames are hardcoded but should be readily discernable, *e.g.* 
fireline intensity outputs begin with ``flin_``, time of arrival outputs 
begin with ``toa_``, *etc*. Since ELMFIRE is sometimes used to run 
multiple cases as part of a Monte Carlo analysis or sensitivity 
analysis, a seven-digit sequential identifier is prepended to the name of 
each output raster, and the time at which the raster was dumped is 
appended to the filename.

In addition to raster-based outputs, ESRI Shapefiles with fire front 
isochrones can be written to disk. To enable this, set 
``DUMP_ISOCHRONE_SHAPEFILES = .TRUE.``.

Several text output files can also be written to disk. Fire area as a 
function of time can be written to Disk by setting 
``DUMP_TRANSIENT_ACREAGE = .TRUE.``, and overall fire size statistics at 
the end of each run can be requested with ``DUMP_FIRE_SIZE_STATS = 
.TRUE.``.

Virtual weather stations can be specified by setting 
``NUM_VIRTUAL_STATIONS`` to an integer greater than zero, and then 
specifying x and y coordinates of each station. As a simple example, 
data from two virtual stations would be written to disk by adding the 
following lines to the ``&OUTPUTS`` namelist group:

.. code-block::

   NUM_VIRTUAL_STATIONS = 2
   VIRTUAL_STATION_X(1) = 12345.0
   VIRTUAL_STATION_Y(1) = 67890.0
   VIRTUAL_STATION_X(2) = 98765.0
   VIRTUAL_STATION_Y(2) = 43210.0

The index inside the parentheses denotes the station number. For each 
virtual station a separate .csv file will be written that includes 
transient and fixed quantities as a function of time. These quantities 
are currently:

   * Elevation
   * Slope
   * Aspect
   * Canopy Bulk Density
   * Canopy Base Height
   * Canopy Cover
   * Canopy Height
   * Fuel model
   * 1-hour dead fuel moisture
   * 10-hour dead fuel moisture
   * 100-hour dead fuel moisture
   * Live herbaceous fuel moisture
   * Live woody fuel moisture
   * Foliar fuel moisture
   * 20-ft wind speed
   * 20-ft wind direction
   * :math:`{\phi}` (level set variable)

.. note::

   Virtual stations have been temporarily disabled
