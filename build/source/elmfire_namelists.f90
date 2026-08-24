MODULE ELMFIRE_NAMELISTS

USE ELMFIRE_VARS

IMPLICIT NONE

CONTAINS

! *****************************************************************************
SUBROUTINE READ_MISC
! *****************************************************************************
! Reads the &MISCELLANEOUS namelist group, setting defaults for fuel model and
! GDAL paths and scratch/input directories, and appends path separators to the
! directory paths.

INTEGER :: IOS, LUGDAL, ISLASH
CHARACTER(400) :: GDALEXE, GDALTMP
CHARACTER(32)  :: RANKSTR
CHARACTER(256) :: IOSMSG

NAMELIST /MISCELLANEOUS/ BUILDING_FUEL_MODEL_FILE, FUEL_MODEL_FILE, MISCELLANEOUS_INPUTS_DIRECTORY, PATH_TO_GDAL, SCRATCH

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &MISCELLANEOUS namelist group'

!Set default values:
BUILDING_FUEL_MODEL_FILE       = 'building_fuel_models.csv'
FUEL_MODEL_FILE                = 'null'
MISCELLANEOUS_INPUTS_DIRECTORY = 'null'
PATH_TO_GDAL                   = 'auto'
SCRATCH                        = 'null'

READ(LUINPUT,NML=MISCELLANEOUS,IOSTAT=IOS,IOMSG=IOSMSG)
IF (IOS > 0) THEN
   WRITE(*,*) 'Error reading &MISCELLANEOUS namelist group: ', TRIM(IOSMSG)
   STOP
ENDIF

! If PATH_TO_GDAL is left at the default 'auto', discover the directory that
! contains gdal_translate from the PATH (via 'command -v') so users don't have
! to hard-code it. Falls back to '/usr/bin' if GDAL can't be located. An
! explicit PATH_TO_GDAL in the namelist always takes precedence.
IF (TRIM(PATH_TO_GDAL) .EQ. 'auto') THEN
   WRITE(RANKSTR,'(I0)') IRANK_WORLD
   GDALTMP = '.elmfire_gdal_path_' // TRIM(RANKSTR) // '.txt'
   CALL EXECUTE_COMMAND_LINE('command -v gdal_translate > ' // TRIM(GDALTMP) // ' 2>/dev/null', EXITSTAT=IOS)

   GDALEXE = ''
   OPEN(NEWUNIT=LUGDAL, FILE=TRIM(GDALTMP), STATUS='OLD', ACTION='READ', IOSTAT=IOS)
   IF (IOS .EQ. 0) THEN
      READ(LUGDAL,'(A)',IOSTAT=IOS) GDALEXE
      CLOSE(LUGDAL)
   ENDIF
   CALL EXECUTE_COMMAND_LINE(TRIM(DELETECOMMAND) // ' ' // TRIM(GDALTMP))

   ISLASH = INDEX(TRIM(GDALEXE), PATH_SEPARATOR, BACK=.TRUE.)
   IF (ISLASH .GT. 1) THEN
      PATH_TO_GDAL = GDALEXE(1:ISLASH-1)
      IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Auto-detected PATH_TO_GDAL: ', TRIM(PATH_TO_GDAL)
   ELSE
      PATH_TO_GDAL = '/usr/bin'
      IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Could not auto-detect GDAL on PATH; falling back to PATH_TO_GDAL = ', TRIM(PATH_TO_GDAL)
   ENDIF
ENDIF

PATH_TO_GDAL = TRIM(PATH_TO_GDAL) // PATH_SEPARATOR

! if dirs are still null don't add a path separator
IF (MISCELLANEOUS_INPUTS_DIRECTORY .NE. 'null') THEN
   MISCELLANEOUS_INPUTS_DIRECTORY = TRIM(MISCELLANEOUS_INPUTS_DIRECTORY) // PATH_SEPARATOR
ENDIF
IF (SCRATCH .NE. 'null') THEN
   SCRATCH = TRIM(SCRATCH) // PATH_SEPARATOR
ENDIF

! *****************************************************************************
END SUBROUTINE READ_MISC
! *****************************************************************************

! *****************************************************************************
SUBROUTINE READ_SMOKE
! *****************************************************************************
! Reads the &SMOKE namelist group and sets defaults for smoke/PM emission
! outputs (emission factors, calorific value, flaming/smoldering times,
! output interval and enable flag).

INTEGER :: IOS
CHARACTER(256) :: IOSMSG

NAMELIST /SMOKE/ DT_SMOKE_OUTPUTS, ENABLE_SMOKE_OUTPUTS, PM_EMISSION_FACTOR_FLAMING, &
                 PM_EMISSION_FACTOR_SMOLDERING, DRY_WOOD_CALORIFIC_VALUE, FLAMING_TIME, &
                 SMOLDERING_TIME

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &SMOKE namelist group'

!Set default values:
DT_SMOKE_OUTPUTS               = 3600.0
ENABLE_SMOKE_OUTPUTS           = .FALSE.
PM_EMISSION_FACTOR_FLAMING     = 17.4
PM_EMISSION_FACTOR_SMOLDERING  = 49.8
DRY_WOOD_CALORIFIC_VALUE       = 19
FLAMING_TIME                   = 180
SMOLDERING_TIME                = 3600

READ(LUINPUT,NML=SMOKE,IOSTAT=IOS,IOMSG=IOSMSG)
IF (IOS > 0) THEN
   WRITE(*,*) 'Error reading &SMOKE namelist group: ', TRIM(IOSMSG)
   STOP
ENDIF

! *****************************************************************************
END SUBROUTINE READ_SMOKE
! *****************************************************************************

! *****************************************************************************
SUBROUTINE READ_INPUTS
! *****************************************************************************
! Reads the &INPUTS namelist group and sets defaults for all raster filenames,
! input/weather directories and units/option flags; also opens and counts the
! optional TIMED_LOCATIONS_CSV, populating TIMED_LOCATIONS_TRACKER.

INTEGER :: I, IOS
INTEGER(8) :: I8DUMMY
REAL :: RDUMMY
CHARACTER(400) :: FN
CHARACTER(256) :: IOSMSG

NAMELIST /INPUTS/ &
ADJ_FILENAME, ASP_FILENAME, BARRIER_FILENAME, BLDG_AREA_FILENAME, BLDG_FOOTPRINT_FRAC_FILENAME, BLDG_FUEL_MODEL_FILENAME, &
BLDG_NONBURNABLE_FRAC_FILENAME, BLDG_SEPARATION_DIST_FILENAME, &
CBD_FILENAME, CBD_TIMES_100, CBH_FILENAME, CBH_TIMES_10, CC_FILENAME, CC_IN_PERCENT, &
CH_FILENAME, CH_TIMES_10, DEM_FILENAME, DT_METEOROLOGY, FBFM_FILENAME, FMC_FILENAME, FOLIAR_MOISTURE_CONTENT, &
FUELS_AND_TOPOGRAPHY_DIRECTORY, GRID_DECLINATION, &
IGNITIONS_CSV_FILENAME, IGNITION_MASK_FILENAME, LAND_VALUE_FILENAME, LH_MOISTURE_CONTENT, LW_MOISTURE_CONTENT, &
DEAD_MC_IN_PERCENT, LIVE_MC_IN_PERCENT, PHI_FILENAME, POPULATION_DENSITY_FILENAME, REAL_ESTATE_VALUE_FILENAME, &
SLP_FILENAME, ERC_FILENAME, M100_FILENAME, M10_FILENAME, M1_FILENAME, MLH_FILENAME, MLW_FILENAME, &
PYROMES_FILENAME, USE_BSQ_XML_HEADER, ROTATE_ASP, ROTATE_WD, WD_FILENAME, WS_FILENAME, USE_CONSTANT_FMC, &
USE_CONSTANT_LH, USE_CONSTANT_LW, USE_EXISTING_BSQS, USE_LAND_VALUE, USE_POPULATION_DENSITY, USE_REAL_ESTATE_VALUE, &
USE_TILED_IO, USE_BARRIERS, WEATHER_DIRECTORY, WS_AT_10M, WS_IN_KPH, VRT_INSTEAD_OF_TIF, SDI_FILENAME, TIMED_LOCATIONS_CSV, & 
ONLY_READ_NEEDED_WX_BANDS, START_DC, START_DMC, DAILY_WEATHER_FILENAME, SURFACE_SPREAD_MODEL, &
LANDSCAPE_FILENAME, &
! new suppression model :: added below
PCL_FILENAME

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &INPUTS namelist group'

!Set default values:
ADJ_FILENAME                   = ' '
ASP_FILENAME                   = ' '
BARRIER_FILENAME               = ' '
BLDG_AREA_FILENAME             = ' '
BLDG_FOOTPRINT_FRAC_FILENAME   = ' '
BLDG_FUEL_MODEL_FILENAME       = ' '
BLDG_NONBURNABLE_FRAC_FILENAME = ' '
BLDG_SEPARATION_DIST_FILENAME  = ' '
CBD_FILENAME                   = ' '
CBD_TIMES_100                  = .TRUE.
CBH_FILENAME                   = ' '
CBH_TIMES_10                   = .TRUE.
CC_FILENAME                    = ' '
CC_IN_PERCENT                  = .TRUE.
CH_FILENAME                    = ' '
CH_TIMES_10                    = .TRUE.
DEM_FILENAME                   = ' '
DT_METEOROLOGY                 = -9999.9
FBFM_FILENAME                  = ' '
FMC_FILENAME                   = ' '
FOLIAR_MOISTURE_CONTENT        = 90.0
FUELS_AND_TOPOGRAPHY_DIRECTORY = ' '
GRID_DECLINATION               = 0.0
IGNITION_MASK_FILENAME         = ' '
LANDSCAPE_FILENAME             = ' '
LAND_VALUE_FILENAME            = ' '
LH_MOISTURE_CONTENT            = 60.0
LW_MOISTURE_CONTENT            = 60.0 
DEAD_MC_IN_PERCENT             = .TRUE.
LIVE_MC_IN_PERCENT             = .TRUE.
PHI_FILENAME                   = ' '
POPULATION_DENSITY_FILENAME    = ' '
REAL_ESTATE_VALUE_FILENAME     = ' '
SDI_FILENAME                   = ' '
PCL_FILENAME                   = ' ' ! new suppression model :: added this
SLP_FILENAME                   = ' '
ERC_FILENAME                   = ' '
IGNITIONS_CSV_FILENAME         = ' '
M100_FILENAME                  = ' '
M10_FILENAME                   = ' '
M1_FILENAME                    = ' '
MLH_FILENAME                   = ' '
MLW_FILENAME                   = ' '
PYROMES_FILENAME               = ' '
ROTATE_ASP                     = .FALSE.
ROTATE_WD                      = .FALSE.
TIMED_LOCATIONS_CSV            = 'null'
WD_FILENAME                    = ' '
WS_FILENAME                    = ' '
USE_BSQ_XML_HEADER             = .TRUE.
USE_CONSTANT_FMC               = .TRUE.
USE_CONSTANT_LH                = .TRUE.
USE_CONSTANT_LW                = .TRUE.
USE_EXISTING_BSQS              = .FALSE.
USE_LAND_VALUE                 = .FALSE.
USE_POPULATION_DENSITY         = .FALSE.
USE_REAL_ESTATE_VALUE          = .FALSE.
USE_TILED_IO                   = .FALSE.
USE_BARRIERS                   = .FALSE.
VRT_INSTEAD_OF_TIF             = .FALSE.
WEATHER_DIRECTORY              = ' '
WS_AT_10M                      = .FALSE.
WS_IN_KPH                      = .FALSE. 
ONLY_READ_NEEDED_WX_BANDS      = .FALSE.
SURFACE_SPREAD_MODEL           = "ROTHERMEL"
START_DC                       = 400.0
START_DMC                      = 80.0
DAILY_WEATHER_FILENAME         = ' '

READ(LUINPUT,NML=INPUTS,IOSTAT=IOS,IOMSG=IOSMSG)
IF (IOS > 0) THEN
   WRITE(*,*) 'Error reading &INPUTS namelist group: ', TRIM(IOSMSG)
   STOP
ENDIF

FUELS_AND_TOPOGRAPHY_DIRECTORY = TRIM(FUELS_AND_TOPOGRAPHY_DIRECTORY) // PATH_SEPARATOR
WEATHER_DIRECTORY              = TRIM(WEATHER_DIRECTORY             ) // PATH_SEPARATOR

! A landscape file is a single multiband GeoTIFF holding (in band order) elevation,
! slope, aspect, fuel model, canopy cover, canopy height, canopy base height, and
! canopy bulk density. When specified, it is read instead of the individual rasters.
USE_LANDSCAPE_FILE = (LEN_TRIM(LANDSCAPE_FILENAME) .GT. 0)

PROCESS_TIMED_LOCATIONS = .FALSE.
IF (TRIM(TIMED_LOCATIONS_CSV) .EQ. 'null' ) RETURN

FN = TRIM(TIMED_LOCATIONS_CSV)
OPEN(LUAUXINPUT,FILE=TRIM(FN),FORM='FORMATTED',STATUS='OLD',IOSTAT=IOS)

IF (IOS .NE. 0) THEN 
   WRITE(*,*) 'Could not open TIMED_LOCATIONS_CSV'
   RETURN
ENDIF

READ (LUAUXINPUT,*,IOSTAT=IOS)
IF (IOS .NE. 0) THEN 
   WRITE(*,*) 'Bad header in TIMED_LOCATIONS_CSV'
   RETURN
ENDIF

NUM_TIMED_LOCATIONS = 0
DO WHILE (IOS .EQ. 0)
   READ (LUAUXINPUT,*,IOSTAT=IOS) I8DUMMY, RDUMMY, RDUMMY
   IF (IOS .EQ. 0) NUM_TIMED_LOCATIONS = NUM_TIMED_LOCATIONS + 1
ENDDO
CLOSE(LUAUXINPUT)

IF (NUM_TIMED_LOCATIONS .EQ. 0) RETURN

PROCESS_TIMED_LOCATIONS = .TRUE.

ALLOCATE(TIMED_LOCATIONS_TRACKER(1:NUM_TIMED_LOCATIONS))

OPEN(LUAUXINPUT,FILE=TRIM(FN),FORM='FORMATTED',STATUS='OLD',IOSTAT=IOS)
READ (LUAUXINPUT,*,IOSTAT=IOS)
DO I = 1, NUM_TIMED_LOCATIONS
    READ (LUAUXINPUT,*,IOSTAT=IOS) TIMED_LOCATIONS_TRACKER(I)%ID, TIMED_LOCATIONS_TRACKER(I)%X, TIMED_LOCATIONS_TRACKER(I)%Y
ENDDO

CLOSE(LUAUXINPUT)

! *****************************************************************************
END SUBROUTINE READ_INPUTS
! *****************************************************************************

! *****************************************************************************
SUBROUTINE READ_OUTPUTS
! *****************************************************************************
! Reads the &OUTPUTS namelist group and sets defaults for all DUMP_*/output
! options; compacts the TIME_AT_BURNED_ACRES list to its used entries and
! appends a path separator to OUTPUTS_DIRECTORY.

INTEGER :: I, IOS
CHARACTER(256) :: IOSMSG
REAL, ALLOCATABLE, DIMENSION (:) :: TABA ! Time at burned acres

NAMELIST /OUTPUTS/ &
ACCUMULATE_EMBER_FLUX, BINARY_OUTPUTS_DUMP_FRACTION, CALCULATE_TIMES_BURNED, CALCULATE_FLAME_LENGTH_STATS, &
CONVERT_TO_GEOTIFF, DTDUMP, DUMP_AFFECTED_LAND_VALUE, DUMP_AFFECTED_POPULATION, DUMP_AFFECTED_REAL_ESTATE_VALUE, &
DUMP_BINARY_OUTPUTS, DUMP_CROWN_FIRE, DUMP_CRITICAL_FLIN, DUMP_EMBER_FLUX, DUMP_CROWN_FIRE_AREA, DUMP_EMITIMES, &
DUMP_FIRE_SIZE_STATS, DUMP_FIRE_VOLUME, DUMP_FLAME_LENGTH, DUMP_FLIN, DUMP_HOURLY_RASTERS, &
DUMP_HPUA, DUMP_PHI, &
DUMP_REACTION_INTENSITY, &
DUMP_SPREAD_RATE, SPREAD_RATE_IN_M, DUMP_SPREAD_DIRECTION, DUMP_SURFACE_FIRE, DUMP_SURFACE_FIRE_AREA, DUMP_TAGGED, DUMP_TIME_OF_ARRIVAL, DUMP_TIMINGS, &
DUMP_TRANSIENT_ACREAGE, DUMP_VELOCITY, DUMP_WD20, DUMP_CFFDRS_DEBUG, DUMP_WS20, EMBER_COUNT_BIN_LO, EMBER_COUNT_BIN_HI, &
FULL_BINARY_OUTPUTS, NUM_EMBER_COUNT_BINS, NUM_VIRTUAL_STATIONS, &
FLAME_LENGTH_BIN_LO, FLAME_LENGTH_BIN_HI, MINIMUM_AREA_FOR_BINARY_OUTPUTS, &
NUM_FLAME_LENGTH_BINS, OUTPUTS_DIRECTORY, USE_EMBER_COUNT_BINS, USE_FLAME_LENGTH_BINS, &
DUMP_SPOTTING_OUTPUTS, DUMP_EMBER_FLUX_TRANSIENT, DUMP_EMBER_IGNITION, RUN_ID, DUMP_TOTAL_DFC_RECEIVED, DUMP_TOTAL_RAD_RECEIVED, DUMP_TRANSIENT_DFC, DUMP_TRANSIENT_RAD, DUMP_FUEL_CONSUMPTION, &
DUMP_HRR_TRANSIENT, TIME_AT_BURNED_ACRES, USE_FOUR_DIGITS_IN_IWX_BAND, VIRTUAL_STATION_X, VIRTUAL_STATION_Y, DUMP_EVERY_STEP

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &OUTPUTS namelist group'

ALLOCATE(TABA(1:1000), TIME_AT_BURNED_ACRES(1:1000))

ACCUMULATE_EMBER_FLUX             = .FALSE.
BINARY_OUTPUTS_DUMP_FRACTION      = 1E0
CALCULATE_FLAME_LENGTH_STATS      = .FALSE.
CALCULATE_TIMES_BURNED            = .FALSE.
CONVERT_TO_GEOTIFF                = .TRUE. 
DTDUMP                            = 3600.0
DUMP_EVERY_STEP                   = .FALSE.
DUMP_AFFECTED_LAND_VALUE          = .FALSE.
DUMP_AFFECTED_POPULATION          = .FALSE.
DUMP_AFFECTED_REAL_ESTATE_VALUE   = .FALSE.
DUMP_BINARY_OUTPUTS               = .FALSE.
DUMP_CROWN_FIRE                   = .FALSE.
DUMP_CROWN_FIRE_AREA              = .FALSE.
DUMP_CRITICAL_FLIN                = .FALSE.
DUMP_EMITIMES                     = .FALSE.
DUMP_EMBER_FLUX                   = .FALSE.
DUMP_SPOTTING_OUTPUTS             = .FALSE.
DUMP_FIRE_SIZE_STATS              = .TRUE. 
DUMP_FIRE_VOLUME                  = .FALSE.
DUMP_FLAME_LENGTH                 = .FALSE. 
DUMP_FLIN                         = .FALSE.
DUMP_HOURLY_RASTERS               = .FALSE.
DUMP_HPUA                         = .FALSE.
DUMP_PHI                          = .FALSE. 
DUMP_REACTION_INTENSITY           = .FALSE. 
DUMP_SPREAD_RATE                  = .FALSE. 
DUMP_SPREAD_DIRECTION             = .FALSE.
DUMP_SURFACE_FIRE                 = .FALSE. 
DUMP_SURFACE_FIRE_AREA            = .FALSE.
DUMP_TAGGED                       = .FALSE. 
DUMP_TIME_OF_ARRIVAL              = .FALSE. 
DUMP_TIMINGS                      = .FALSE.
DUMP_TRANSIENT_ACREAGE            = .FALSE. 
DUMP_VELOCITY                     = .FALSE. 
DUMP_WD20                         = .FALSE. 
DUMP_WS20                         = .FALSE.
DUMP_CFFDRS_DEBUG                 = .FALSE. 
EMBER_COUNT_BIN_HI(:)             = 0
EMBER_COUNT_BIN_LO(:)             = 0
FLAME_LENGTH_BIN_HI(:)            = 0.
FLAME_LENGTH_BIN_LO(:)            = 0.
FULL_BINARY_OUTPUTS               = .TRUE.
MINIMUM_AREA_FOR_BINARY_OUTPUTS   = 0.
NUM_EMBER_COUNT_BINS              = 0
NUM_FLAME_LENGTH_BINS             = 0
NUM_VIRTUAL_STATIONS              = 0
OUTPUTS_DIRECTORY                 = ' '
RUN_ID                            = ''
SPREAD_RATE_IN_M                  = .FALSE.
TIME_AT_BURNED_ACRES(:)           = -9E9
USE_EMBER_COUNT_BINS              = .FALSE.
USE_FLAME_LENGTH_BINS             = .FALSE.
USE_FOUR_DIGITS_IN_IWX_BAND       = .FALSE.
VIRTUAL_STATION_X(:)              = 0.0
VIRTUAL_STATION_Y(:)              = 0.0
! WU-E Related Outputs
DUMP_HRR_TRANSIENT                = .FALSE. 
DUMP_TOTAL_DFC_RECEIVED           = .FALSE.
DUMP_TOTAL_RAD_RECEIVED           = .FALSE. 
DUMP_TRANSIENT_DFC                = .FALSE.
DUMP_TRANSIENT_RAD                = .FALSE.
DUMP_FUEL_CONSUMPTION             = .FALSE.
! Eulerian firebrnand model outputs
DUMP_EMBER_FLUX_TRANSIENT         = .FALSE.
DUMP_EMBER_IGNITION               = .FALSE. 

READ(LUINPUT,NML=OUTPUTS,IOSTAT=IOS,IOMSG=IOSMSG)
IF (IOS > 0) THEN
   WRITE(*,*) 'Error reading &OUTPUTS namelist group: ', TRIM(IOSMSG)
   STOP
ENDIF

OUTPUTS_DIRECTORY = TRIM(OUTPUTS_DIRECTORY) // PATH_SEPARATOR

NUM_TIME_AT_BURNED_ACRES = 0
DO I = 1, 1000
   IF (TIME_AT_BURNED_ACRES(I) .LE. 0.) CYCLE
   NUM_TIME_AT_BURNED_ACRES = NUM_TIME_AT_BURNED_ACRES + 1
   TABA(NUM_TIME_AT_BURNED_ACRES) = TIME_AT_BURNED_ACRES(I)    
ENDDO

DEALLOCATE (TIME_AT_BURNED_ACRES)
ALLOCATE(TIME_AT_BURNED_ACRES(1:NUM_TIME_AT_BURNED_ACRES))
TIME_AT_BURNED_ACRES=TABA(1:NUM_TIME_AT_BURNED_ACRES)
DEALLOCATE(TABA)

! *****************************************************************************
END SUBROUTINE READ_OUTPUTS
! *****************************************************************************

! *****************************************************************************
SUBROUTINE READ_TIME_CONTROL
! *****************************************************************************
! Reads the &TIME_CONTROL namelist group and sets defaults for simulation
! timing, time-step/CFL controls, meteorology interpolation intervals and
! diurnal/burn-period parameters.

INTEGER :: IOS
CHARACTER(256) :: IOSMSG

NAMELIST /TIME_CONTROL/ &
BAND_ONE_HOUR_OF_YEAR, BURN_PERIOD_CENTER_FRAC, BURN_PERIOD_LENGTH, CURRENT_YEAR, DT_INTERPOLATE_M1, &
DT_INTERPOLATE_M10, DT_INTERPOLATE_M100, DT_INTERPOLATE_MLH, DT_INTERPOLATE_MLW, DT_INTERPOLATE_FMC, &
DT_INTERPOLATE_WIND, FORECAST_START_HOUR, HOUR_OF_YEAR, &
OVERNIGHT_ADJUSTMENT_FACTOR, RANDOMIZE_SIMULATION_TSTOP, SIMULATION_DT, SIMULATION_DTMAX, &
SIMULATION_TSTART, SIMULATION_TSTOP, TARGET_CFL, &
USE_DIURNAL_ADJUSTMENT_FACTOR

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &TIME_CONTROL namelist group'

BAND_ONE_HOUR_OF_YEAR         = 0
BURN_PERIOD_CENTER_FRAC       = 0.667
BURN_PERIOD_LENGTH            = 10.0
CURRENT_YEAR                  = -1
DT_INTERPOLATE_M1             = 300.0
DT_INTERPOLATE_M10            = 3000.0
DT_INTERPOLATE_M100           = 30000.0
DT_INTERPOLATE_MLH            = 9E8
DT_INTERPOLATE_MLW            = 9E8
DT_INTERPOLATE_FMC            = 9E8
DT_INTERPOLATE_WIND           = 300.0
FORECAST_START_HOUR           = 20.0
HOUR_OF_YEAR                  = -1
OVERNIGHT_ADJUSTMENT_FACTOR   = 0.1
RANDOMIZE_SIMULATION_TSTOP    = .FALSE. 
SIMULATION_DT                 = 5.0 
SIMULATION_DTMAX              = 600.0
SIMULATION_TSTART             = 0.0
SIMULATION_TSTOP              = 3600.0
SUNRISE_HOUR                  = -1.0 ! UTC, over-ridden by call to SUNRISE_SUNSET_CALCS
SUNSET_HOUR                   = -1.0 ! UTC, over-ridden by call to SUNRISE_SUNSET_CALCS
TARGET_CFL                    = 0.4
USE_DIURNAL_ADJUSTMENT_FACTOR = .FALSE.

READ(LUINPUT,NML=TIME_CONTROL,IOSTAT=IOS,IOMSG=IOSMSG)
IF (IOS > 0) THEN
   WRITE(*,*) 'Error reading &TIME_CONTROL namelist group: ', TRIM(IOSMSG)
   STOP
ENDIF

! *****************************************************************************
END SUBROUTINE READ_TIME_CONTROL
! *****************************************************************************

! *****************************************************************************
SUBROUTINE READ_MONTE_CARLO
! *****************************************************************************
! Reads the &MONTE_CARLO namelist group and sets defaults for ensemble,
! ignition, ERC and raster-perturbation settings; validates perturbation
! options, counts Monte Carlo variables/parameters, allocates COEFFS arrays and
! sets the weather band start/stop/skip range.

INTEGER :: IOS, IVARN
CHARACTER(256) :: IOSMSG

NAMELIST /MONTE_CARLO/ ADD_TO_IGNITION_MASK, ALLOW_MULTIPLE_IGNITIONS_AT_A_PIXEL, CSV_FIXED_IGNITION_LOCATIONS, &
EDGEBUFFER, ERC_IS_PLIGNRATE, &
IGNITION_MASK_SCALE_FACTOR, METEOROLOGY_BAND_START, METEOROLOGY_BAND_STOP, METEOROLOGY_BAND_SKIP_INTERVAL, &
NUM_ENSEMBLE_MEMBERS, NUM_METEOROLOGY_TIMES, NUM_RASTERS_TO_PERTURB, PDF_LOWER_LIMIT, PDF_TYPE, PDF_UPPER_LIMIT, &
PERCENT_OF_PIXELS_TO_IGNITE, RANDOM_IGNITIONS, RANDOM_IGNITIONS_TYPE, RASTER_TO_PERTURB, SEED, SPATIAL_PERTURBATION, &
TEMPORAL_PERTURBATION, USE_ERC, USE_IGNITION_MASK, WIND_DIRECTION_FLUCTUATION_INTENSITY_MAX, &
WIND_DIRECTION_FLUCTUATION_INTENSITY_MIN, WIND_SPEED_FLUCTUATION_INTENSITY_MAX, WIND_SPEED_FLUCTUATION_INTENSITY_MIN, &
PDF_MEAN, PDF_SIGMA, POINT_WIND_TO_CENTER

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &MONTE_CARLO namelist group'

!Set default values:
ADD_TO_IGNITION_MASK                     = -9E9
ALLOW_MULTIPLE_IGNITIONS_AT_A_PIXEL      = .FALSE.
CSV_FIXED_IGNITION_LOCATIONS             = .FALSE.
EDGEBUFFER                               = 3000.
ERC_IS_PLIGNRATE                         = .FALSE.
IGNITION_MASK_SCALE_FACTOR               = 1.0
METEOROLOGY_BAND_START                   = -1
METEOROLOGY_BAND_STOP                    = -1
METEOROLOGY_BAND_SKIP_INTERVAL           = -1
NUM_ENSEMBLE_MEMBERS                     = 1
NUM_RASTERS_TO_PERTURB                   = 0
NUM_METEOROLOGY_TIMES                    = -1
PDF_LOWER_LIMIT(:)                       = 0.
PDF_TYPE(:)                              = 'null'
PDF_UPPER_LIMIT(:)                       = 0.
PDF_MEAN(:)                              = 0.
PDF_SIGMA(:)                             = 0.
PERCENT_OF_PIXELS_TO_IGNITE              = 5.0
RANDOM_IGNITIONS                         = .FALSE.
RANDOM_IGNITIONS_TYPE                    = 1
RASTER_TO_PERTURB(:)                     = 'null'
SEED                                     = 2024
SPATIAL_PERTURBATION(:)                  = 'null'
TEMPORAL_PERTURBATION(:)                 = 'null'
USE_ERC                                  = .FALSE.
USE_IGNITION_MASK                        = .FALSE.
WIND_DIRECTION_FLUCTUATION_INTENSITY_MAX = -1.0 
WIND_DIRECTION_FLUCTUATION_INTENSITY_MIN = -1.0
WIND_SPEED_FLUCTUATION_INTENSITY_MAX     = -1.0
WIND_SPEED_FLUCTUATION_INTENSITY_MIN     = -1.0 
POINT_WIND_TO_CENTER                     = .FALSE.

! Not part of namelist group but set here:
PERTURB_WIND_DIRECTION_FLUCTUATION_INTENSITY = .FALSE.
PERTURB_WIND_SPEED_FLUCTUATION_INTENSITY     = .FALSE. 

READ(LUINPUT,NML=MONTE_CARLO,IOSTAT=IOS,IOMSG=IOSMSG)
IF (IOS > 0) THEN
   WRITE(*,*) 'Error reading &MONTE_CARLO namelist group: ', TRIM(IOSMSG)
   STOP
ENDIF

NUM_PARAMETERS_RASTERS    = 0
NUM_PARAMETERS_MISC       = 0
NUM_MONTE_CARLO_VARIABLES = 0

IF (RANDOM_IGNITIONS) THEN
   IF (USE_IGNITION_MASK) THEN
      IF ( TRIM(IGNITIONS_CSV_FILENAME) .EQ. '' .AND. TRIM(IGNITION_MASK_FILENAME) .EQ. '' ) THEN
         WRITE(*,200) 'When RANDOM_IGNITIONS = .TRUE. ELMFIRE requires a Float32 ignition'
         WRITE(*,200) 'mask specified via the keyword IGNITION_MASK_FILENAME on the &INPUTS'
         WRITE(*,200) 'namelist group OR a sequence of igntitions contained in a .csv file'
         WRITE(*,200) 'specified via the keyworkd IGNITIONS_CSV_FILENAME on the &INPUTS'
         WRITE(*,200) 'namelist group. Please set IGNITION_MASK_FILENAME or IGNITIONS_CSV_FILENAME'
         WRITE(*,200) 'and rerun.'
         STOP
      ENDIF
   ELSE
      IF (TRIM(IGNITIONS_CSV_FILENAME) .EQ. '') THEN
         WRITE(*,200) 'When RANDOM_IGNITIONS = .TRUE., setting USE_IGNITION_MASK = .FALSE. is now deprecated.'
         WRITE(*,200) 'unless IGNITIONS_CSV_FILENAME is specified.'
         WRITE(*,200) 'ELMFIRE assumes when RANDOM_IGNITIONS = .TRUE. that a Float32 ignition mask is provided'
         WRITE(*,200) 'via the keyword IGNITION_MASK_FILENAME on the &INPUTS namelist group. The keyword '
         WRITE(*,200) 'USE_IGNITION_MASK is scheduled for removal from the &MONTE_CARLO namelist group. '
         WRITE(*,200) 'Until that time, please set USE_IGNITION_MASK = .TRUE. and set IGNITION_MASK_FILENAME.'
         STOP
      ENDIF
   ENDIF
ENDIF

DO IVARN = 1, NUM_RASTERS_TO_PERTURB
   IF (SPATIAL_PERTURBATION(IVARN) .NE. 'GLOBAL' .AND. SPATIAL_PERTURBATION(IVARN) .NE. 'PIXEL') THEN
      WRITE(*,200) 'Error, SPATIAL_PERTURBATION must be GLOBAL or PIXEL. Variation: ', IVARN
      STOP
   ENDIF
   IF (TEMPORAL_PERTURBATION(IVARN) .NE. 'STATIC' .AND. TEMPORAL_PERTURBATION(IVARN) .NE. 'DYNAMIC') THEN
      WRITE(*,200) 'Error, TEMPORAL_PERTURBATION must be STATIC or DYNAMIC. Variation: ', IVARN
      STOP
   ENDIF
   IF (PDF_TYPE(IVARN) .NE. 'UNIFORM' .and. PDF_TYPE(IVARN) .NE. 'GAUSSIAN' .and. PDF_TYPE(IVARN) .NE. 'LOGNORMAL') THEN
      WRITE(*,200) 'Error, PDF_TYPE must be UNIFORM, GAUSSIAN or LOGNORMAL. Variation: ', IVARN
      STOP
   ENDIF
      
   SELECT CASE (TRIM(RASTER_TO_PERTURB(IVARN)))
   CASE ('ADJ','CBD','CBH','CC','CH','FBFM','FMC','M1','M10','M100','MLH','MLW','WAF','WD','WS')
      ! valid - no action
   CASE DEFAULT
      WRITE(*,200) 'Error on variation ', IVARN, ' RASTER_TO_PERTURB must be one of: '
      WRITE(*,200) 'ADJ, CBD, CBH, CC, CH, FBFM, FMC, M1, M10, M100, MLH, MLW, WAF, WD, WS'
      STOP
   END SELECT
      
   IF (TRIM(SPATIAL_PERTURBATION(IVARN)) .NE. 'PIXEL') THEN
      IF (TRIM(TEMPORAL_PERTURBATION(IVARN)) .EQ. 'STATIC') THEN
         NUM_PARAMETERS_RASTERS = NUM_PARAMETERS_RASTERS + 1
      ELSE
         NUM_PARAMETERS_RASTERS = NUM_PARAMETERS_RASTERS + NUM_METEOROLOGY_TIMES
      ENDIF      
   ENDIF
      
ENDDO

IF (WIND_DIRECTION_FLUCTUATION_INTENSITY_MIN .GT. 0. .AND. WIND_DIRECTION_FLUCTUATION_INTENSITY_MAX .GT. 0.) THEN
   PERTURB_WIND_DIRECTION_FLUCTUATION_INTENSITY = .TRUE.
   NUM_PARAMETERS_MISC = NUM_PARAMETERS_MISC + 1
ENDIF

IF (WIND_SPEED_FLUCTUATION_INTENSITY_MIN .GT. 0. .AND. WIND_SPEED_FLUCTUATION_INTENSITY_MAX .GT. 0.) THEN
   PERTURB_WIND_SPEED_FLUCTUATION_INTENSITY = .TRUE.
   NUM_PARAMETERS_MISC = NUM_PARAMETERS_MISC + 1
ENDIF

NUM_MONTE_CARLO_VARIABLES = NUM_PARAMETERS_RASTERS + NUM_PARAMETERS_MISC + NUM_PARAMETERS_SPOTTING
ALLOCATE(COEFFS         (1:NUM_MONTE_CARLO_VARIABLES))
ALLOCATE(COEFFS_UNSCALED(1:NUM_MONTE_CARLO_VARIABLES))

! Figure out which bands to read and loop over
IF (METEOROLOGY_BAND_START.GT.0 .AND. METEOROLOGY_BAND_STOP.GT.0 .AND. METEOROLOGY_BAND_SKIP_INTERVAL.GT.0) THEN
   IWX_BAND_START = METEOROLOGY_BAND_START
   IWX_BAND_STOP  = METEOROLOGY_BAND_STOP
   IWX_BAND_SKIP  = METEOROLOGY_BAND_SKIP_INTERVAL
ELSE
   IWX_BAND_START = 1
   IWX_BAND_STOP  = 1
   IWX_BAND_SKIP  = 1
ENDIF

200 FORMAT(A, I9)

! *****************************************************************************
END SUBROUTINE READ_MONTE_CARLO
! *****************************************************************************

! *****************************************************************************
SUBROUTINE READ_SIMULATOR
! *****************************************************************************
! Reads the &SIMULATOR namelist group and sets defaults for run mode, crown
! fire, ignition points/lines, wind fluctuations and runtime/feedback options

INTEGER :: IOS
CHARACTER(256) :: IOSMSG

NAMELIST /SIMULATOR/ &
ALLOW_NONBURNABLE_PIXEL_IGNITION, BANDTHICKNESS, CRITICAL_CANOPY_COVER, &
CROWN_FIRE_ADJ, CROWN_FIRE_MODEL, CROWN_FIRE_SPREAD_RATE_LIMIT, CROWN_RATIO, FEEDBACK_LEVEL, DT_WIND_FLUCTUATIONS, &
ESTIMATE_URBAN_LOSSES, MAX_LOW, MAX_RUNTIME, MODE, MULTIPLE_HOSTS, NUM_IGNITIONS, &
PHIS_ADJ, PHIW_ADJ, PLIGNRATE_MIN, RANDOMIZE_RANDOM_SEED,SURFACE_ACCELERATION_TIME_CONSTANT, T_IGN, &
UNTAG_CELLS_TIMESTEP_INTERVAL, UNTAG_TYPE_2, UNTAG_TYPE_3, USE_PYROMES, &
WIND_DIRECTION_FLUCTUATION_INTENSITY, WIND_FLUCTUATIONS, WIND_SPEED_FLUCTUATION_INTENSITY, X_IGN, Y_IGN, &
WSMFEFF_LOW_MULT, WX_BILINEAR_INTERPOLATION, WX_BANDS_KEPT_IN_MEM, CLEAN_SCRATCH, T_LINE_IGN, X_LINE_IGN_START, &
X_LINE_IGN_END, Y_LINE_IGN_START, Y_LINE_IGN_END

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &SIMULATOR namelist group'

ALLOW_NONBURNABLE_PIXEL_IGNITION     = .TRUE.
BANDTHICKNESS                        = 2
CRITICAL_CANOPY_COVER                = 0.39
CROWN_FIRE_ADJ                       = 1.0
CROWN_FIRE_MODEL                     = 1
CROWN_FIRE_SPREAD_RATE_LIMIT         = 250.0
CROWN_RATIO                          = 1.0
FEEDBACK_LEVEL                       = 0 ! options: 0, 1, 2, 3
DT_WIND_FLUCTUATIONS                 = 15.0
ESTIMATE_URBAN_LOSSES                = .FALSE.
MAX_LOW                              = 8.0
MAX_RUNTIME                          = 999999.
MODE                                 = 1 !1 = level set propagation; 2 = fire potential; 3 = both
MULTIPLE_HOSTS                       = .FALSE.
NUM_IGNITIONS                        = 0
PHIS_ADJ                             = 1E0
PHIW_ADJ                             = 1E0
PLIGNRATE_MIN                        = 0E0
RANDOMIZE_RANDOM_SEED                = .FALSE. 
SURFACE_ACCELERATION_TIME_CONSTANT   = 1.0
T_IGN(:)                             = -1.0
UNTAG_CELLS_TIMESTEP_INTERVAL        = 10
UNTAG_TYPE_2                         = .FALSE.
UNTAG_TYPE_3                         = .FALSE.
WIND_DIRECTION_FLUCTUATION_INTENSITY = 0.0
WIND_FLUCTUATIONS                    = .FALSE.
WIND_SPEED_FLUCTUATION_INTENSITY     = 0.0
X_IGN(:)                             = 0.0
Y_IGN(:)                             = 0.0
USE_PYROMES                          = .FALSE.
!WSMFEFF_LOW_MULT                     = 5.07955E-3
WSMFEFF_LOW_MULT                     = 60.0/5280.0
WX_BILINEAR_INTERPOLATION            = .FALSE.
WX_BANDS_KEPT_IN_MEM                 = 30
CLEAN_SCRATCH                        = .FALSE.
T_LINE_IGN(:) = -1
X_LINE_IGN_START(:) = -1
Y_LINE_IGN_START(:) = -1
X_LINE_IGN_END(:) = -1
Y_LINE_IGN_END(:) = -1

READ(LUINPUT,NML=SIMULATOR,IOSTAT=IOS,IOMSG=IOSMSG)
IF (IOS > 0) THEN
   WRITE(*,*) 'Error reading &SIMULATOR namelist group: ', TRIM(IOSMSG)
   STOP
ENDIF

! *****************************************************************************
END SUBROUTINE READ_SIMULATOR
! *****************************************************************************

! *****************************************************************************
SUBROUTINE READ_WUI
! *****************************************************************************
! Reads the &WUI namelist group and sets defaults for the building (WUI) spread
! model: building area/separation/fuel parameters, spread and interface model
! types, hardening factor and band thickness.

INTEGER :: IOS
CHARACTER(256) :: IOSMSG

NAMELIST /WUI/ BLDG_AREA_CONSTANT, BLDG_NONBURNABLE_FRAC_CONSTANT, BLDG_SEPARATION_DIST_CONSTANT, &
               BLDG_SPREAD_MODEL_TYPE, BLDG_FOOTPRINT_FRAC_CONSTANT, BLDG_FUEL_MODEL_CONSTANT, &
               USE_BLDG_SPREAD_MODEL, USE_CONSTANT_BLDG_SPREAD_MODEL_PARAMS, GLOBAL_HARDENING_FACTOR, INTERFACE_MODEL_TYPE, &
               BANDTHICKNESS_WUI, CRITICL_HF_WUI, HRR_ELLIPSE_ADJ

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &WUI namelist group'

BLDG_AREA_CONSTANT                    = 20.0
BLDG_SEPARATION_DIST_CONSTANT         = 10.0
BLDG_NONBURNABLE_FRAC_CONSTANT        = 0.0
BLDG_FOOTPRINT_FRAC_CONSTANT          = 1.0
BLDG_FUEL_MODEL_CONSTANT              = 1
BLDG_SPREAD_MODEL_TYPE                = 1 ! 1 = Hamada, 2 = UCB / UMD
USE_BLDG_SPREAD_MODEL                 = .FALSE.
USE_CONSTANT_BLDG_SPREAD_MODEL_PARAMS = .TRUE.
GLOBAL_HARDENING_FACTOR               = 1.0
INTERFACE_MODEL_TYPE                  = 1 ! 1 = Ellipse, 2 = Threshold
BANDTHICKNESS_WUI                     = 5
CRITICL_HF_WUI                        = 0.0
HRR_ELLIPSE_ADJ                       = 0.5

READ(LUINPUT,NML=WUI,IOSTAT=IOS,IOMSG=IOSMSG)
IF (IOS > 0) THEN
   WRITE(*,*) 'Error reading &WUI namelist group: ', TRIM(IOSMSG)
   STOP
ENDIF

! *****************************************************************************
END SUBROUTINE READ_WUI
! *****************************************************************************

! *****************************************************************************
SUBROUTINE READ_SPOTTING
! *****************************************************************************
! Reads the &SPOTTING namelist group and sets defaults for ember spotting
! (generation/distance/accumulation/ignition models, spotting percentages,
! ember counts and distances); sets NUM_PARAMETERS_SPOTTING and allocates
! SPOTTING_STATS.

INTEGER :: IOS
CHARACTER(256) :: IOSMSG

NAMELIST /SPOTTING/ CRITICAL_SPOTTING_FIRELINE_INTENSITY, CROWN_FIRE_SPOTTING_PERCENT, CROWN_FIRE_SPOTTING_PERCENT_MAX, &
CROWN_FIRE_SPOTTING_PERCENT_MIN, ENABLE_SPOTTING, ENABLE_SURFACE_FIRE_SPOTTING, &
GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT, GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MAX, GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN, &
MAX_SPOTTING_DISTANCE, MEAN_SPOTTING_DIST, MEAN_SPOTTING_DIST_MAX, MEAN_SPOTTING_DIST_MIN, MIN_SPOTTING_DISTANCE, &
NEMBERS_MAX, NEMBERS_MAX_HI, NEMBERS_MAX_LO, NEMBERS_MIN, NEMBERS_MIN_HI, NEMBERS_MIN_LO, &
NORMALIZED_SPOTTING_DIST_VARIANCE, NORMALIZED_SPOTTING_DIST_VARIANCE_MAX, NORMALIZED_SPOTTING_DIST_VARIANCE_MIN, &
PIGN, PIGN_MAX, PIGN_MIN, SPOTTING_DISTRIBUTION_TYPE, SPOT_FLIN_EXP, SPOT_FLIN_EXP_HI, SPOT_FLIN_EXP_LO, &
SPOT_WS_EXP, SPOT_WS_EXP_HI, SPOT_WS_EXP_LO, STOCHASTIC_SPOTTING, SURFACE_FIRE_SPOTTING_PERCENT, &
SURFACE_FIRE_SPOTTING_PERCENT_MULT, TAU_EMBERGEN, EMBER_GR, SOURCE_FUEL_IGN_MULT, &
P_EPS, USE_PHYSICAL_SPOTTING_DURATION, EMBER_SAMPLING_FACTOR, &
USE_SUPERSEDED_SPOTTING, NO_SURFACE_FIRE, &
LOCAL_IGNITION_TIME, CELL_IGNITION_DELAY, USE_CUSTOMIZED_PDF, MU_CROSSWIND, SIGMA_CROSSWIND, MU_DOWNWIND, &
SIGMA_DOWNWIND, EMBER_GR_PER_MW_BLDG, EMBER_GR_PER_MW_VEGE, DIFF_WILDLAND_IGNITION, USE_EMBER_CONSUMPTION, &
USE_CROSSWIND_DISTRIBUTION, GENERATION_MODEL, SPOTTING_DISTANCE_MODEL, ACCUMULATION_MODEL, IGNITION_MODEL

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &SPOTTING namelist group'

GENERATION_MODEL     = 'RANDOM' ! 'RANDOM' or 'PER-AREA' or 'PER-MW'  
SPOTTING_DISTANCE_MODEL = 'UNIFORM' ! 'UNIFORM' or 'LOGNORMAL' or 'EMPIRICAL'
ACCUMULATION_MODEL   = 'LAGRANGIAN' ! 'LAGRANGIAN' or 'EULERIAN'
IGNITION_MODEL       = 'DIRECT' ! 'DIRECT' or 'SIMPLE' or 'PHYSICAL'

CRITICAL_SPOTTING_FIRELINE_INTENSITY(:)   = 0.
CROWN_FIRE_SPOTTING_PERCENT               = 100.
CROWN_FIRE_SPOTTING_PERCENT_MAX           = 100.
CROWN_FIRE_SPOTTING_PERCENT_MIN           = 100.
EMBER_GR                                  = 1E-3
EMBER_SAMPLING_FACTOR                     = 1.0
ENABLE_SPOTTING                           = .FALSE. 
ENABLE_SURFACE_FIRE_SPOTTING              = .FALSE.
GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT      = 0.0
GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MAX  = 0.0
GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN  = 0.0
MAX_SPOTTING_DISTANCE                     = 0.
MEAN_SPOTTING_DIST                        = 0. 
MEAN_SPOTTING_DIST_MAX                    = 0.
MEAN_SPOTTING_DIST_MIN                    = 0.
MIN_SPOTTING_DISTANCE                     = 0.
NEMBERS                                   = 25
NEMBERS_MAX                               = 1
NEMBERS_MAX_HI                            = 1
NEMBERS_MAX_LO                            = 1
NEMBERS_MIN                               = 30
NEMBERS_MIN_HI                            = 0
NEMBERS_MIN_LO                            = 0
NORMALIZED_SPOTTING_DIST_VARIANCE         = 0.
NORMALIZED_SPOTTING_DIST_VARIANCE_MAX     = 0.
NORMALIZED_SPOTTING_DIST_VARIANCE_MIN     = 0.
PIGN                                      = 1.0 ! PERCENT
PIGN_MAX                                  = 1.0
PIGN_MIN                                  = 1.0
LOCAL_IGNITION_TIME                       = 30.0 ! seconds, for simple ignition model only
CELL_IGNITION_DELAY                       = 100.0 ! seconds, Delay of setting phi = -1 when attacked by firebrands and ignited, for simple ignition model only
SOURCE_FUEL_IGN_MULT(:)                   = 1.0
SPOTTING_DISTRIBUTION_TYPE                = 'LOGNORMAL'
SPOT_FLIN_EXP                             = 0.5
SPOT_FLIN_EXP_HI                          = 0.5
SPOT_FLIN_EXP_LO                          = 0.5
SPOT_WS_EXP                               = 0.9
SPOT_WS_EXP_HI                            = 0.9
SPOT_WS_EXP_LO                            = 0.9
STOCHASTIC_SPOTTING                       = .FALSE. !This is now being used.
SURFACE_FIRE_SPOTTING_PERCENT(:)          = 100.
SURFACE_FIRE_SPOTTING_PERCENT_MULT(:)     = 1.0
TAU_EMBERGEN                              = 6.0
P_EPS                                     = 0.01 
USE_PHYSICAL_SPOTTING_DURATION            = .FALSE.
USE_CUSTOMIZED_PDF                        = .FALSE.
USE_SUPERSEDED_SPOTTING                   = .TRUE.
NO_SURFACE_FIRE                           = .FALSE.
MU_CROSSWIND                              = 0.0
SIGMA_CROSSWIND                           = 0.0
MU_DOWNWIND                               = 0.0
SIGMA_DOWNWIND                            = 0.0
EMBER_GR_PER_MW_BLDG                      = 10.0
EMBER_GR_PER_MW_VEGE                      = 33.3
DIFF_WILDLAND_IGNITION                    = .FALSE.
USE_EMBER_CONSUMPTION                     = .FALSE.
USE_CROSSWIND_DISTRIBUTION                = .FALSE.

READ(LUINPUT,NML=SPOTTING,IOSTAT=IOS,IOMSG=IOSMSG)
IF (IOS > 0) THEN
   WRITE(*,*) 'Error reading &SPOTTING namelist group: ', TRIM(IOSMSG)
   STOP
ENDIF

IF (ENABLE_SPOTTING) THEN
   NUM_PARAMETERS_SPOTTING = 8
ELSE
   NUM_PARAMETERS_SPOTTING = 0
ENDIF

ALLOCATE (SPOTTING_STATS(1:EMBER_TRACKER_SIZE))

! *****************************************************************************
END SUBROUTINE READ_SPOTTING
! *****************************************************************************

! *****************************************************************************
SUBROUTINE READ_SUPPRESSION
! *****************************************************************************
! Reads the &SUPPRESSION namelist group and sets defaults for initial/extended
! attack suppression and SDI (suppression difficulty index) containment
! parameters.

INTEGER :: IOS
CHARACTER(256) :: IOSMSG

NAMELIST /SUPPRESSION/ AREA_NO_CONTAINMENT_CHANGE, B_SDI, DT_EXTENDED_ATTACK, &
                       ENABLE_EXTENDED_ATTACK, ENABLE_INITIAL_ATTACK, ENABLE_INDIRECT_ATTACK,&
                       INITIAL_ATTACK_TIME, MAX_CONTAINMENT_PER_DAY, SDI_FACTOR, USE_SDI, USE_SDI_LOG_FUNCTION, &
                       ! new suppression model :: added below
                       EXTENDED_ATTACK_MODEL, EXTENDED_ATTACK_TIME, AVAILABLE_SUPPRESSION_CAPACITY, DELTA_ROS, DELTA_FL, &
                        DELTA_SDI, DELTA_PCL, FIRE_LINE_THICKNESS, SDI_MAX_DIRECT_ATTACK, FL_MAX_DIRECT_ATTACK, PCL_THRESHOLD, &
                        INITIAL_CONTAINMENT_SHAPE_FACTOR, FIRELINE_LENGTH_REF


IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &SUPPRESSION namelist group'
! Set default values:
B_SDI                       = 1.0
DT_EXTENDED_ATTACK          = 3600.
ENABLE_EXTENDED_ATTACK      = .FALSE.
ENABLE_INITIAL_ATTACK       = .FALSE.
AREA_NO_CONTAINMENT_CHANGE  = 10000.0
INITIAL_ATTACK_TIME         = 1800.0
MAX_CONTAINMENT_PER_DAY     = 100.0
SDI_FACTOR                  = 1.0
USE_SDI                     = .FALSE.
USE_SDI_LOG_FUNCTION        = .FALSE.

! Spatially explicit suppression model
EXTENDED_ATTACK_MODEL              = 0          ! 0: Area-Growth-Based Containment Model; 1: Spatially Explicit Suppression Model
ENABLE_INDIRECT_ATTACK             = .TRUE.     ! Enable indirect attack
EXTENDED_ATTACK_TIME               = -1.0       ! Time (s) when full suppression capacity is available. If <0, estimated from first-day fireline growth and FIRELINE_LENGTH_REF
INITIAL_CONTAINMENT_SHAPE_FACTOR   = 10         ! Controls buildup of suppression capacity; larger values produce greater initial delay and a sharper increase
AVAILABLE_SUPPRESSION_CAPACITY     = 2000       ! m/hr
FIRELINE_LENGTH_REF                = 15000      ! m

! Suggested default values
PCL_THRESHOLD                      = 30.0       ! PCL threshold for indirect attack (0--100)
FL_MAX_DIRECT_ATTACK               = 8.0        ! ft; reference flame length used for both direct and indirect attack
SDI_MAX_DIRECT_ATTACK              = 100.0      ! Reference SDI; USDA SDI data stored as SDI*100
FIRE_LINE_THICKNESS                = 1          ! Number of cells used to represent the active fireline
DELTA_ROS                          = 10         ! ft/min
DELTA_FL                           = 2          ! ft
DELTA_SDI                          = 10         ! SDI units; USDA SDI data stored as SDI*100
DELTA_PCL                          = 10         ! PCL units (0--100)


READ(LUINPUT,NML=SUPPRESSION,IOSTAT=IOS,IOMSG=IOSMSG)

IF (ENABLE_EXTENDED_ATTACK .AND. EXTENDED_ATTACK_MODEL .EQ. 1) USE_SDI = .TRUE.

IF (INITIAL_CONTAINMENT_SHAPE_FACTOR .LT. 1.0) THEN
   WRITE(*,*) 'Error: Problem with namelist group &SUPPRESSION.'
   WRITE(*,*) 'INITIAL_CONTAINMENT_SHAPE_FACTOR should be larger or equal to 1.0'
   STOP
ENDIF

IF (IOS > 0) THEN
   WRITE(*,*) 'Error reading &SUPPRESSION namelist group: ', TRIM(IOSMSG)
   STOP
ENDIF

! *****************************************************************************
END SUBROUTINE READ_SUPPRESSION
! *****************************************************************************

! *****************************************************************************
SUBROUTINE READ_CALIBRATION
! *****************************************************************************
! Opens the namelist input file and reads the &CALIBRATION namelist group,
! setting defaults for per-pyrome adjustment factors, calibration constants and
! duration PDF filenames/flags and the maximum fire duration.

INTEGER :: IOS
CHARACTER(256) :: IOSMSG

NAMELIST /CALIBRATION/ ADJUSTMENT_FACTORS_BY_PYROME, ADJUSTMENT_FACTORS_FILENAME, &
CALIBRATION_CONSTANTS_BY_PYROME, CALIBRATION_CONSTANTS_FILENAME, DURATION_MAX_DAYS, &
DURATION_PDF_BY_PYROME, DURATION_PDF_FILENAME

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'Reading &CALIBRATION namelist group'

ADJUSTMENT_FACTORS_BY_PYROME    = .FALSE.
ADJUSTMENT_FACTORS_FILENAME     = ''
CALIBRATION_CONSTANTS_BY_PYROME = .FALSE.
CALIBRATION_CONSTANTS_FILENAME  = ''
DURATION_MAX_DAYS               = 28
DURATION_PDF_BY_PYROME          = .FALSE.
DURATION_PDF_FILENAME           = ''

! Open input file and read &CALIBRATION namelist groups
OPEN(LUINPUT,FILE=TRIM(NAMELIST_FN),FORM='FORMATTED',STATUS='OLD',IOSTAT=IOS)
IF (IOS .GT. 0) THEN
   WRITE(*,*) 'Problem opening input file ', TRIM(NAMELIST_FN)
   STOP
ENDIF

READ(LUINPUT,NML=CALIBRATION,IOSTAT=IOS,IOMSG=IOSMSG)
IF (IOS > 0) THEN
   WRITE(*,*) 'Error reading &CALIBRATION namelist group: ', TRIM(IOSMSG)
   STOP
ENDIF

! *****************************************************************************
END SUBROUTINE READ_CALIBRATION
! *****************************************************************************

END MODULE
