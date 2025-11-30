# Subroutine Summary by Source File

## build/source/elmfire_ignition.f90
- `ALLOCATE_IGNITION_ARRAYS` allocates and initializes ignition case arrays based on whether random ignition sampling is enabled. 【F:build/source/elmfire_ignition.f90†L17-L38】
- `DETERMINE_NUM_CASES_TOTAL` counts ignitable pixels and determines ensemble case totals across weather bands. 【F:build/source/elmfire_ignition.f90†L48-L218】
- `DETERMINE_IGNITION_LOCATIONS` samples spatial ignition points for each case using ignition masks and ERC factors when enabled. 【F:build/source/elmfire_ignition.f90†L227-L305】
- `DETERMINE_NUM_CASES_TOTAL_CSV` reads ignition definitions from a CSV file and populates case tracking arrays. 【F:build/source/elmfire_ignition.f90†L311-L368】

## build/source/elmfire_spotting.f90
- `SET_SPOTTING_PARAMETERS` maps random coefficients to the various spotting model parameters and global arrays. 【F:build/source/elmfire_spotting.f90†L13-L40】
- `SPOTTING` emits embers from a source cell and chooses Lagrangian or Eulerian transport based on configuration. 【F:build/source/elmfire_spotting.f90†L45-L118】
- `EMBER_TRAJECTORY` advances individual embers along stochastic trajectories to place spot fires. 【F:build/source/elmfire_spotting.f90†L273-L566】
- `EMBER_TRAJECTORY_EULERIAN` transports embers using an Eulerian grid-based approach. 【F:build/source/elmfire_spotting.f90†L570-L829】
- `CLEAR_USED_EMBER` removes consumed ember particles to free Lagrangian storage. 【F:build/source/elmfire_spotting.f90†L833-L857】
- `EMBER_CONSUMPTION` computes ember mass loss and determines whether particles remain active. 【F:build/source/elmfire_spotting.f90†L970-L1058】

## build/source/elmfire_spotting_superseded.f90
- `SPOTTING_SUPERSEDED` legacy spotting implementation calculating ember release and landing. 【F:build/source/elmfire_spotting_superseded.f90†L13-L79】
- `EMBER_TRAJECTORY_SUPERSEDED` earlier ember trajectory solver retained for reference. 【F:build/source/elmfire_spotting_superseded.f90†L83-L314】

## build/source/elmfire_calibration.f90
- `READ_CALIBRATION_BY_PYROME` loads calibration constants mapped to pyrome regions. 【F:build/source/elmfire_calibration.f90†L12-L112】

## build/source/elmfire_subs.f90
- `WRITE_TIMINGS_TO_DISK` outputs accumulated timing metrics for each host process. 【F:build/source/elmfire_subs.f90†L13-L33】
- `ACCUMULATE_CPU_USAGE` updates timing counters for a profiled block. 【F:build/source/elmfire_subs.f90†L37-L49】
- `MPI_BCAST_RASTER_HEADER` broadcasts raster metadata between MPI ranks, optionally sending only dimensions. 【F:build/source/elmfire_subs.f90†L53-L86】
- `BCAST_WEATHER_FUEL_TOPOGRAPHY` shares weather, fuel, and topography rasters to peer processes on each host. 【F:build/source/elmfire_subs.f90†L90-L144】
- `PERTURB_RASTERS` applies random perturbations to configured raster inputs. 【F:build/source/elmfire_subs.f90†L153-L192】
- `GET_OPERATING_SYSTEM` detects platform-specific path separators and delete commands. 【F:build/source/elmfire_subs.f90†L196-L214】
- `ALLOCATE_EMPTY_RASTER` allocates raster storage with supplied dimensions and metadata. 【F:build/source/elmfire_subs.f90†L218-L272】
- `MAP_FINE_TO_COARSE` aggregates fine-grid rasters into coarser resolutions. 【F:build/source/elmfire_subs.f90†L276-L303】
- `INTERP_WD_RASTER` and `INTERP_WD_RASTER_SINGLE` interpolate wind direction rasters at linked-list nodes or single cells. 【F:build/source/elmfire_subs.f90†L363-L416】
- `INTERP_RASTER_LINKEDLIST`, `INTERP_RASTER_LINKEDLIST_BILINEAR`, and `INTERP_RASTER_LINKEDLIST_SINGLE` variants interpolate raster quantities using nearby linked-list nodes, including bilinear options. 【F:build/source/elmfire_subs.f90†L420-L560】【F:build/source/elmfire_subs.f90†L903-L953】
- `INTERP_WIND_LINKEDLIST_BILINEAR` and `INTERP_WIND_SINGLE_BILINEAR` perform bilinear interpolation for wind speed and direction to linked-list structures or single cells. 【F:build/source/elmfire_subs.f90†L564-L763】
- `GET_BILINEAR_INTERPOLATE_COEFFS` calculates indices and coefficients for bilinear interpolation across grid cells. 【F:build/source/elmfire_subs.f90†L785-L822】
- `INTERP_RASTER_LINKEDLIST_SINGLE_BILINEAR` handles single-cell bilinear raster interpolation with linked-list support. 【F:build/source/elmfire_subs.f90†L957-L1004】
- `APPLY_WIND_FLUCTUATIONS` perturbs wind fields with stochastic fluctuations across a list of cells. 【F:build/source/elmfire_subs.f90†L1008-L1035】
- `APPEND`, `APPEND_TO_DYNAMIC_ARRAY`, `INIT`, `DELETE_NODE`, and `TIDY` manage dynamic linked-list and array storage for fire front cells. 【F:build/source/elmfire_subs.f90†L1049-L1239】
- `LOCATE` implements a search helper used for locating positions in sorted arrays. 【F:build/source/elmfire_subs.f90†L1243-L1273】
- `SUNRISE_SUNSET_CALCS` computes sunrise and sunset times from latitude, longitude, and day-of-year inputs. 【F:build/source/elmfire_subs.f90†L1277-L1322】
- `SHUTDOWN` finalizes MPI and shared-memory resources, deleting intermediate files as needed. 【F:build/source/elmfire_subs.f90†L1326-L1420】
- `ERC_IGNITION_FACTOR` evaluates ignition factors from Energy Release Component rasters for neighboring cells. 【F:build/source/elmfire_subs.f90†L1424-L1472】

## build/source/elmfire_init.f90
- `SET_MISC_PARAMETERS` initializes miscellaneous control parameters and options. 【F:build/source/elmfire_init.f90†L10-L32】
- `CHECK_INPUTS` validates configuration options for consistency before simulation. 【F:build/source/elmfire_init.f90†L36-L90】
- `INIT_LOOKUP_TABLES` precomputes lookup tables used throughout spread calculations. 【F:build/source/elmfire_init.f90†L94-L131】
- `INIT_RASTERS` allocates and initializes base raster datasets. 【F:build/source/elmfire_init.f90†L135-L177】
- `SETUP_SHARED_MEMORY_1` and `SETUP_SHARED_MEMORY_2` allocate shared-memory windows for large rasters and derived arrays. 【F:build/source/elmfire_init.f90†L181-L528】
- `CALC_WIND_ADJUSTMENT_FACTOR_EVERYWHERE` computes wind adjustment factors across the domain. 【F:build/source/elmfire_init.f90†L532-L583】
- `ROTATE_ASP_AND_WD` rotates aspect and wind direction rasters according to coordinate system type. 【F:build/source/elmfire_init.f90†L587-L610】
- `WRITE_FUEL_MODEL_TABLE` outputs default fuel model parameters to disk. 【F:build/source/elmfire_init.f90†L655-L735】
- `READ_FUEL_MODEL_TABLE` and `READ_BUILDING_FUEL_MODEL_TABLE` ingest fuel model definitions for vegetation and buildings. 【F:build/source/elmfire_init.f90†L739-L964】

## build/source/elmfire_io.f90
- `SETUP_PARALLEL_IO` assigns MPI ranks to load different rasters and toggles optional datasets. 【F:build/source/elmfire_io.f90†L11-L120】
- `READ_WEATHER_FUEL_TOPOGRAPHY` reads weather, fuel, and topographic rasters (non-tiled). 【F:build/source/elmfire_io.f90†L168-L407】
- `READ_WEATHER_FUEL_TOPOGRAPHY_TILED` processes the same datasets using tiled input files. 【F:build/source/elmfire_io.f90†L411-L700】
- `WRITE_BIL_RASTER` recursively writes rasters to BIL or GeoTIFF formats with optional compression. 【F:build/source/elmfire_io.f90†L704-L881】
- `READ_BSQ_XML_HEADER`, `READ_BSQ_HDR_HEADER`, and `READ_BSQ_HEADER_EXISTING_TILED` parse BSQ metadata from XML or HDR files. 【F:build/source/elmfire_io.f90†L885-L1237】
- `READ_BSQ_RASTER` and `READ_BSQ_RASTER_EXISTING_TILED` load BSQ raster data into existing buffers. 【F:build/source/elmfire_io.f90†L1241-L1691】
- `LL_DUMP_ROUTINE` dumps linked-list fire front data for debugging. 【F:build/source/elmfire_io.f90†L1695-L1763】
- `MAIN_DUMP_ROUTINE` orchestrates periodic diagnostic dumps, including raster outputs. 【F:build/source/elmfire_io.f90†L1767-L2003】
- `FIRE_SIZE_STATS_TO_RASTERS` and `_HOURLY` convert fire size statistics into raster outputs. 【F:build/source/elmfire_io.f90†L2007-L2096】
- `POSTPROCESS` performs final output conversions and cleanup after simulation. 【F:build/source/elmfire_io.f90†L2100-L2299】
- `WRITE_STATION` writes station time series data for a given case and rank. 【F:build/source/elmfire_io.f90†L2303-L2328】
- `PARSE_ENVI_HEADER` and `PARSE_MAP_INFO` extract raster metadata from ENVI header fields. 【F:build/source/elmfire_io.f90†L2332-L2539】

## build/source/elmfire_level_set.f90
- `LEVEL_SET_PROPAGATION` advances the fire front using a level-set solver across time steps. 【F:build/source/elmfire_level_set.f90†L20-L1528】
- `TAG_BAND` labels a band of cells around the fire perimeter for updating. 【F:build/source/elmfire_level_set.f90†L1544-L1571】
- `UNTAG_CELLS` clears band tagging based on burn state and arrival times. 【F:build/source/elmfire_level_set.f90†L1575-L1677】
- `CALC_NORMAL_VECTORS` computes normals of the level-set function for flux calculations. 【F:build/source/elmfire_level_set.f90†L1681-L1719】
- `UX_AND_UY_ELLIPTICAL` calculates fire spread velocity components considering elliptical growth. 【F:build/source/elmfire_level_set.f90†L1723-L1952】
- `RK2_INTEGRATE` advances the level-set field with a second-order Runge–Kutta step. 【F:build/source/elmfire_level_set.f90†L1956-L1982】
- `CFL_AND_FLUX_LIMITER` enforces Courant stability and gradient limiting, including nested `LIMIT_GRADIENTS`. 【F:build/source/elmfire_level_set.f90†L1986-L2120】

## build/source/elmfire_spread_rate.f90
- `SURFACE_SPREAD_RATE` recursively computes surface fire spread based on fuel and wind inputs. 【F:build/source/elmfire_spread_rate.f90†L13-L132】
- `CROWN_SPREAD_RATE` recursively evaluates crown fire spread rates. 【F:build/source/elmfire_spread_rate.f90†L136-L211】
- `HAMADA` provides an alternative spread-rate contribution from an external model. 【F:build/source/elmfire_spread_rate.f90†L216-L301】
- `UMD_UCB_BLDG_SPREAD` models spread between buildings using building fuel parameters. 【F:build/source/elmfire_spread_rate.f90†L305-L501】
- `ELLIPSE_UCB` computes elliptical spread geometry adjustments. 【F:build/source/elmfire_spread_rate.f90†L505-L588】
- `HRR_TRANSIENT` applies transient heat release rate adjustments. 【F:build/source/elmfire_spread_rate.f90†L592-L624】
- `BURNED_FILTER` filters out burned cells to manage processing bounds. 【F:build/source/elmfire_spread_rate.f90†L629-L658】

## build/source/elmfire_suppression.f90
- `CENTROID` calculates suppression line centroids for an incident time. 【F:build/source/elmfire_suppression.f90†L13-L49】
- `CONTAINMENT` simulates suppression containment progress over time. 【F:build/source/elmfire_suppression.f90†L53-L179】

## build/source/elmfire_namelists.f90
- `READ_MISC` ingests miscellaneous configuration options from the namelist. 【F:build/source/elmfire_namelists.f90†L10-L43】
- `READ_SMOKE` reads smoke-related options. 【F:build/source/elmfire_namelists.f90†L47-L76】
- `READ_INPUTS` loads input file paths and related flags. 【F:build/source/elmfire_namelists.f90†L80-L212】
- `READ_OUTPUTS` configures output directories, formats, and dump intervals. 【F:build/source/elmfire_namelists.f90†L216-L316】
- `READ_COMPUTATIONAL_DOMAIN` sets grid dimensions and coordinate parameters. 【F:build/source/elmfire_namelists.f90†L320-L346】
- `READ_TIME_CONTROL` defines simulation start, end, and step timings. 【F:build/source/elmfire_namelists.f90†L350-L398】
- `READ_MONTE_CARLO` configures Monte Carlo ensemble parameters. 【F:build/source/elmfire_namelists.f90†L402-L571】
- `READ_SIMULATOR` reads simulator execution options. 【F:build/source/elmfire_namelists.f90†L575-L632】
- `READ_WUI` gathers wildland–urban interface model options. 【F:build/source/elmfire_namelists.f90†L636-L664】
- `READ_SPOTTING` sets spotting model controls. 【F:build/source/elmfire_namelists.f90†L668-L768】
- `READ_SUPPRESSION` pulls suppression model settings. 【F:build/source/elmfire_namelists.f90†L772-L802】
- `READ_CALIBRATION` reads calibration options. 【F:build/source/elmfire_namelists.f90†L806-L839】

## build/source/elmfire_post.f90
- `POPULATE_RASTER_FROM_ELMFIRE_BINARY_FILE` and `POPULATE_RASTER_FROM_GRIDFIRE_BINARY_FILE` fill rasters from corresponding binary outputs. 【F:build/source/elmfire_post.f90†L629-L784】
- `POPULATE_RASTER_FROM_ELMFIRE_BINARY_FILE_ANNUAL_BURNPROB` builds burn-probability rasters per meteorology band. 【F:build/source/elmfire_post.f90†L788-L826】
- `CALC_HOURS_SINCE_BURNED` computes elapsed hours since each cell burned. 【F:build/source/elmfire_post.f90†L830-L907】
- `DUMP_ELMFIRE` and `DUMP_GRIDFIRE` write model outputs to binary files for post-processing. 【F:build/source/elmfire_post.f90†L911-L1043】

## Other source files
- `build/source/elmfire_vars.f90` defines shared data structures and parameters without declaring subroutines. 【F:build/source/elmfire_vars.f90†L1-L80】
- `build/source/elmfire.f90` contains the main program coordinating initialization, simulation, and shutdown rather than standalone subroutines. 【F:build/source/elmfire.f90†L1-L120】
- `build/source/sort.for` provides the `DSORT` and `DSORT_DOUBLE_PRECISION` sorting subroutines reused from an external utility. 【F:build/source/sort.for†L6-L663】
- `build/source/zonal_stats.py` is a command-line helper script without defined Python functions. 【F:build/source/zonal_stats.py†L1-L10】
