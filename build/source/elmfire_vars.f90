! *****************************************************************************
MODULE ELMFIRE_VARS
! *****************************************************************************

USE MPI_F08
USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR, C_F_POINTER

IMPLICIT NONE

include 'vars_declarations/global_constants.inc'
!***************************** Namelist group inputs, organized by namelist group: ************************************************

include 'vars_declarations/calibration.inc'
include 'vars_declarations/computational_domain.inc'
include 'vars_declarations/inputs.inc'
include 'vars_declarations/miscellaneous.inc'
include 'vars_declarations/monte_carlo.inc'
include 'vars_declarations/outputs.inc'
include 'vars_declarations/simulator.inc'
include 'vars_declarations/spotting.inc'
include 'vars_declarations/smoke.inc'
include 'vars_declarations/suppression.inc'
include 'vars_declarations/time_control.inc'

include 'vars_declarations/mpi.inc'

! For broadcasting all raster headers (NRMAX is number of rasters)
INTEGER      , PARAMETER :: NRMAX = 36
INTEGER      , DIMENSION(1:NRMAX) :: BANDROWBYTES_ARR, NBANDS_ARR, NBITS_ARR, NCOLS_ARR, NROWS_ARR, TOTALROWBYTES_ARR
REAL         , DIMENSION(1:NRMAX) :: CELLSIZE_ARR, NODATA_VALUE_ARR, ULXMAP_ARR, ULYMAP_ARR, XDIM_ARR, YDIM_ARR, XLLCORNER_ARR, &
                                     YLLCORNER_ARR
CHARACTER    , DIMENSION(1:NRMAX) :: BYTEORDER_ARR
CHARACTER(3) , DIMENSION(1:NRMAX) :: LAYOUT_ARR
CHARACTER(10), DIMENSION(1:NRMAX) :: PIXELTYPE_ARR

! Profiling:
INTEGER :: CLOCK_COUNT_MAX, CLOCK_COUNT_RATE, IT_START, IT_STOP, IT1_LSP
REAL, POINTER, DIMENSION (:,:) :: TIMINGS

! Lookup tables:
REAL, DIMENSION(0:90) :: COSSLP=0, TANSLP2=0. 
REAL, DIMENSION(-1:360) :: ABSSINASP=0., ABSCOSASP=0., SINASPM180=0., COSASPM180=0.
REAL, DIMENSION(0:3600) :: COSWDMPI, SINWDMPI
REAL, DIMENSION(0:100,0:120) :: SHELTERED_WAF_TABLE
REAL, DIMENSION(0:303) :: WSMFEFF_COEFF, B_COEFF_INVERSE, TR
INTEGER, ALLOCATABLE, DIMENSION(:) :: ICOL_ANALYSIS_F2C, IROW_ANALYSIS_F2C
        ! lookup tables for UMD spotting model
INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:) :: EMBER_TARGET_IX, EMBER_TARGET_IY
REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: EMBER_TOA
REAL, ALLOCATABLE, DIMENSION(:) :: TIME_LIST

! 2D geospatial arrays that are not TYPE(RASTER)
INTEGER*2, POINTER, DIMENSION(:,:) :: SURFACE_FIRE, EMBER_COUNT
LOGICAL*1, ALLOCATABLE, DIMENSION (:,:) :: TAGGED
LOGICAL*1, POINTER, DIMENSION (:,:) :: ISNONBURNABLE
REAL, ALLOCATABLE, DIMENSION(:,:) :: PHIP
LOGICAL*1, ALLOCATABLE, DIMENSION(:,:) :: EVERTAGGED
REAL, ALLOCATABLE, DIMENSION (:,:) :: TIME_OF_ARRIVAL, EMBER_TIGN

! 1D geospatial arrays
INTEGER*2, ALLOCATABLE, DIMENSION(:) :: EVERTAGGED_IX, EVERTAGGED_IY

REAL, POINTER, DIMENSION(:) :: STATS_X, STATS_Y, STATS_ASTOP, STATS_TSTOP, STATS_SURFACE_FIRE_AREA, STATS_CROWN_FIRE_AREA, &
                               STATS_FIRE_VOLUME, STATS_AFFECTED_POPULATION, STATS_AFFECTED_REAL_ESTATE_VALUE, &
                               STATS_AFFECTED_LAND_VALUE, STATS_NEMBERS, STATS_SIMULATION_TSTOP_HOURS, &
                               STATS_WALL_CLOCK_TIME, STATS_FINAL_CONTAINMENT_FRAC, STATS_PM2P5_RELEASE, STATS_HRR_PEAK
INTEGER, ALLOCATABLE, DIMENSION(:) :: IX_IGNFAC, IY_IGNFAC
INTEGER :: IXL_EDGEBUFFER, IYL_EDGEBUFFER, IXH_EDGEBUFFER, IYH_EDGEBUFFER
INTEGER*2, ALLOCATABLE, DIMENSION (:) :: EMBER_OUTPUTS_IX
INTEGER*2, ALLOCATABLE, DIMENSION (:) :: EMBER_OUTPUTS_IY
INTEGER*2, ALLOCATABLE, DIMENSION (:) :: EMBER_OUTPUTS_COUNT

! Calibration / optimization
REAL, ALLOCATABLE, DIMENSION(:) :: COEFFS, COEFFS_UNSCALED
REAL, ALLOCATABLE, DIMENSION(:,:) :: COEFFS_UNSCALED_BY_CASE

! OS-specific
CHARACTER(12) :: DELETECOMMAND = '/bin/rm -f '
CHARACTER(1) :: PATH_SEPARATOR
CHARACTER(7) :: OPERATING_SYSTEM

! Binary outputs
INTEGER*2, POINTER, DIMENSION (:) :: BINARY_OUTPUTS_IX            => NULL()
INTEGER*2, POINTER, DIMENSION (:) :: BINARY_OUTPUTS_IY            => NULL()
REAL, POINTER, DIMENSION      (:) :: BINARY_OUTPUTS_TOA           => NULL()
REAL, POINTER, DIMENSION      (:) :: BINARY_OUTPUTS_FLAME_LENGTH  => NULL()
REAL, POINTER, DIMENSION      (:) :: BINARY_OUTPUTS_VELOCITY_FPM  => NULL()
INTEGER*1, POINTER, DIMENSION (:) :: BINARY_OUTPUTS_CROWN_FIRE    => NULL()

! Miscellaneous variables (Broadcast EMBER_FLUX_TABLE_LEN for UMD Spotting Model):
INTEGER :: ANALYSIS_NCOLS,  ANALYSIS_NROWS, IWX_BAND_SKIP, IWX_BAND_START, IWX_BAND_STOP, NUM_CASES_TOTAL, &
           NUM_ENSEMBLE_MEMBERS0, NUM_EVERTAGGED, NUM_STARTING_WX_BANDS, NUM_TRACKED_EMBERS, WX_NCOLS, WX_NROWS, WX_NBANDS, &
           EMBER_FLUX_TABLE_LEN

INTEGER, POINTER, DIMENSION(:) :: STATS_ICASE, STATS_IWX_BAND_START, STATS_IWX_SERIAL_BAND
INTEGER, ALLOCATABLE, DIMENSION(:) :: CSV_IBANDARR, CSV_ICASEARR, NUM_CASES_PER_STARTING_WX_BAND, &
                                      NUM_CASES_COMPLETE_PER_STARTING_WX_BAND, IWX_BAND_START_TEMP, &
                                      IWX_SERIAL_BAND_TEMP 

REAL :: DIURNAL_ADJUSTMENT_FACTOR = 1.0
REAL, POINTER, DIMENSION(:,:,:) :: WSP, WDP, M1P, M10P, M100P, MLHP, MLWP, MFOLP, ERCP, IGNFACP
REAL, ALLOCATABLE, DIMENSION(:) :: CSV_XARR, CSV_YARR, CSV_ASTOP, CSV_TSTOP

LOGICAL, ALLOCATABLE, DIMENSION(:) :: HOURLY_OUTPUTS_FROM_STARTING_WX_BAND_DUMPED

CHARACTER(400) :: NAMELIST_FN

REAL, ALLOCATABLE, DIMENSION(:) :: PROB, IGN_MASK_ARR, N_ARR_R
REAL, PARAMETER :: IGN_MASK_CRIT = 1E-30
INTEGER, ALLOCATABLE, DIMENSION(:) :: ICOL_ARR, IROW_ARR, N_ARR
INTEGER :: NUM_IGNITABLE_PIXELS

! Derived types:
TYPE :: FUEL_MODEL_TABLE_TYPE
   CHARACTER(400) :: SHORTNAME ! Short character string describing fuel model
   LOGICAL        :: DYNAMIC   ! Is this a dynamic fuel model?
   REAL           :: W0 (1:6)  ! Total fuel loading, lb/ft2
   REAL           :: WN (1:6)  ! Net   fuel loading, lb/ft2
   REAL           :: SIG(1:6)  ! Surface area to volume ratio, 1/ft
   REAL           :: DELTA     ! Fuel bed thickness, ft
   REAL           :: MEX_DEAD  ! Dead fuel moisture of extinction
   REAL           :: MEX_LIVE  ! Dead fuel moisture of extinction (starting)
   REAL           :: HOC       ! Heat of combustion, Btu/lb
   REAL           :: RHOB      ! Bulk density, lb/ft3
   REAL           :: RHOP      ! Particle density, lb/ft3
   REAL           :: ST        ! Mineral content, lb minerals / lb ovendry mass
   REAL           :: SE        ! Factor in ETAS
   REAL           :: ETAS      ! Mineral damping coefficient, dimensionless
   REAL           :: BETA      ! Packing ratio, dimensionless
   REAL           :: BETAOP    ! Optimal packing ratio, dimensionless
   REAL           :: XI        ! Propagating flux ratio, dimensionless
   
   REAL           :: A_COEFF ! "A" coefficient in Rothermel model
   REAL           :: B_COEFF ! "B" coefficient in Rothermel model
   REAL           :: C_COEFF ! "C" coefficient in Rothermel model
   REAL           :: E_COEFF ! "E" coefficient in Rothermel model

   REAL           :: GAMMAPRIME     ! Reaction velocity, 1/min
   REAL           :: GAMMAPRIMEPEAK ! Peak reaction velocity, 1/min 

   REAL           :: A_DEAD    ! Area factor for dead fuels
   REAL           :: A_LIVE    ! Area factor for live fuels
   REAL           :: A_OVERALL ! Area factor (overall)

   REAL           :: F_DEAD ! f factor for dead fuels
   REAL           :: F_LIVE ! f factor for live fuels

   REAL           :: W0_DEAD ! Total fuel loading for dead fuels, lb/ft2
   REAL           :: W0_LIVE ! Total fuel loading for live fuels, lb/ft2

   REAL           :: WN_DEAD ! Net fuel loading for dead fuels, lb/ft2
   REAL           :: WN_LIVE ! Net fuel loading for live fuels, lb/ft2
   
   REAL           :: SIG_DEAD    ! Surface area to volume ratio for dead fuels, 1ft
   REAL           :: SIG_LIVE    ! Surface area to volume ratio for live fuels, 1/ft
   REAL           :: SIG_OVERALL ! Surface area to volume ratio overall, 1/ft
   
   REAL           :: TR          ! Residence time, min

   REAL, DIMENSION (1:6) :: A           ! Area factor 
   REAL, DIMENSION (1:6) :: F           ! f factor
   REAL, DIMENSION (1:4) :: FMEX        ! f * mex_dead
   REAL, DIMENSION (1:6) :: FW0         ! f * w0
   REAL, DIMENSION (1:6) :: FSIG        ! f * sigma
   REAL, DIMENSION (1:6) :: EPS         ! epsilon (Surface heating number)
   REAL, DIMENSION (1:6) :: FEPS        ! f * epsilon
   REAL, DIMENSION (1:6) :: WPRIMENUMER ! Numerator of W'
   REAL, DIMENSION (1:6) :: WPRIMEDENOM ! Denominator of W'
   REAL, DIMENSION (1:6) :: MPRIMEDENOM ! Denominator of M'

! These are for performance optimizations:
   REAL :: GP_WND_EMD_ES_HOC ! = FM%GAMMAPRIME * FM%WN_DEAD * FM%ETAS * FM%HOC
   REAL :: GP_WNL_EML_ES_HOC ! = FM%GAMMAPRIME * FM%WN_LIVE * FM%ETAS * FM%HOC
   REAL :: PHISTERM          ! = (5.275 / FUEL_MODEL_TABLE(FBFM(IX,IY))%BETA**0.3)
   REAL :: PHIWTERM          ! = FM%C_COEFF * (FM%BETA / FM%BETAOP)**(-FM%E_COEFF)

   REAL :: WPRIMEDENOM56SUM ! = SUM(FM%WPRIMEDENOM(5:6))
   REAL :: WPRIMENUMER14SUM ! = SUM(FM%WPRIMENUMER(1:4))
   REAL :: MPRIMEDENOM14SUM ! = SUM(FM%MPRIMEDENOM(1:4))
   REAL :: R_MPRIMEDENOME14SUM_MEX_DEAD ! = 1. / (FM%MPRIMEDENOM14SUM * FM%MEX_DEAD)

   REAL :: UNSHELTERED_WAF !Wind adjustment factor when unsheltered

   REAL :: B_COEFF_INVERSE
   REAL :: WSMFEFF_COEFF 

END TYPE

TYPE(FUEL_MODEL_TABLE_TYPE), DIMENSION(0:303,30:120) :: FUEL_MODEL_TABLE_2D !Table for holding fuel models

INTEGER, PARAMETER :: NUM_BUILDING_FUEL_MODELS=100
TYPE :: BUILDING_FUEL_MODEL_TABLE_TYPE
   CHARACTER(80) :: SHORTNAME
   REAL :: T_1MW
   REAL :: T_EARLY
   REAL :: T_FULLDEV
   REAL :: T_DECAY
   REAL :: FUEL_LOAD
   REAL :: HRRPUA_PEAK
   REAL :: FTP_CRIT
   REAL :: Q_CRIT
   REAL :: ABSORPTIVITY
   REAL :: HEIGHT
   REAL :: NONBURNABLE_FRAC
   REAL :: P_IGNITION
   REAL :: HARDENING_FACTOR
   REAL :: TAU_IGN
END TYPE
TYPE(BUILDING_FUEL_MODEL_TABLE_TYPE), DIMENSION(0:NUM_BUILDING_FUEL_MODELS) :: BUILDING_FUEL_MODEL_TABLE

#ifdef _SUPPRESSION
! Suppression
TYPE SUPPRESSION_TRACKER
   INTEGER :: NCELLS(0:359)
   REAL    :: VELOCITY(0:359)
   REAL    :: VELOCITY_SMOOTHED(0:359)
   REAL    :: FIRELINE_FRACTION(0:359)
   REAL    :: SUPPRESSED_FRACTION(0:359)
   REAL    :: T
   REAL    :: ACRES
   REAL    :: ACRES_SDI
   REAL    :: TARGET_CONTAINMENT
   REAL    :: DC_PER_DAY
   REAL    :: DADT
   REAL    :: DASDIDT
   REAL    :: SDIBAR
   INTEGER :: IXCEN
   INTEGER :: IYCEN
END TYPE SUPPRESSION_TRACKER
TYPE(SUPPRESSION_TRACKER), ALLOCATABLE, DIMENSION (:) :: SUPP
#endif

! Spotting
TYPE SPOTTING_TRACKER
   REAL :: X_FROM
   REAL :: Y_FROM
   REAL :: X_TO
   REAL :: Y_TO
   INTEGER :: IX_FROM
   INTEGER :: IY_FROM
   INTEGER :: IX_TO
   INTEGER :: IY_TO
   REAL :: DIST
   REAL :: TTRAVEL
   REAL :: TLAUNCH
   REAL :: TIGN
   REAL :: TAU
   REAL :: FLIN
   LOGICAL :: POSITIVE_IGNITION
   LOGICAL :: ALREADY_IGNITED
END TYPE SPOTTING_TRACKER
TYPE (SPOTTING_TRACKER), ALLOCATABLE, DIMENSION(:) :: SPOTTING_STATS

!Types
TYPE :: RASTER_TYPE
   CHARACTER     :: BYTEORDER
   CHARACTER(3)  :: LAYOUT
   INTEGER       :: NROWS
   INTEGER       :: NCOLS
   INTEGER       :: NBANDS   
   INTEGER       :: NBITS
   INTEGER       :: BANDROWBYTES
   INTEGER       :: TOTALROWBYTES
   CHARACTER(10) :: PIXELTYPE
   REAL          :: ULXMAP
   REAL          :: ULYMAP
   REAL          :: XDIM
   REAL          :: YDIM
   REAL          :: NODATA_VALUE
   REAL          :: CELLSIZE
   REAL          :: XLLCORNER
   REAL          :: YLLCORNER
   REAL,POINTER, DIMENSION(:,:,:) :: R4 =>NULL()
   INTEGER*2,POINTER, DIMENSION(:,:,:) :: I2 =>NULL()
END TYPE

TYPE(RASTER_TYPE), TARGET :: ADJ, ANALYSIS_SURFACE_FIRE, ANALYSIS_TIMES_BURNED, ASP, BARRIER_WIDTH, &
                             CBD, CBH, CC, CH, DEM, FLAME_LENGTH_SUM, EMBER_BIN_COUNT, &
                             FLAME_LENGTH_BIN_COUNT, FLAME_LENGTH_MAX, FBFM, IGN_MASK, LAND_VALUE, OMCOSSLPRAD, &
                             PHI0, POPULATION_DENSITY, REAL_ESTATE_VALUE, SLP, ERC, MFOL, IGNFAC, M1, M10, M100, &
                             MLH, MLW, PYROMES, RH, TMP, WD, WS, WAF, EMBER_FLUX, TIMES_BURNED, TIMES_BURNED_HOURLY, SDI

TYPE(RASTER_TYPE), TARGET :: BLDG_AREA             ! Previously HAMADA_A
TYPE(RASTER_TYPE), TARGET :: BLDG_SEPARATION_DIST  ! Previously HAMADA_D
TYPE(RASTER_TYPE), TARGET :: BLDG_NONBURNABLE_FRAC ! Previously HAMADA_FB
TYPE(RASTER_TYPE), TARGET :: BLDG_FOOTPRINT_FRAC
TYPE(RASTER_TYPE), TARGET :: BLDG_FUEL_MODEL


! UCB declares variables
TYPE :: UCB_ELLIPSE
   REAL     :: DIST_DOWNWIND ! Downwind distance (a) [m]
   REAL     :: DIST_UPWIND ! Upwind distance (b) [m]
   REAL     :: DIST_SIDEWIND ! Sidewind distance (c) [m]

   INTEGER  :: FOREST_FACTOR ! urban = 1; forest = 3
   REAL     :: WIND_POWER = 2 ! Default 1

   REAL     :: ELLIPSE_MINOR ! Semiminor (A) [m]
   REAL     :: ELLIPSE_MAJOR ! Semimajor (B) [m]
   REAL     :: ELLIPSE_ECCENTRICITY ! Eccentricity (E) [m]
END TYPE UCB_ELLIPSE

! Doubly linked list variables
TYPE NODE
   TYPE(NODE), POINTER :: NEXT => NULL()
   TYPE(NODE), POINTER :: PREV => NULL()

   INTEGER*1 :: CROWN_FIRE =  0
   INTEGER   :: IX         = -1
   INTEGER   :: IY         = -1

   LOGICAL :: BURNED            = .FALSE.
   LOGICAL :: JUST_TAGGED       = .TRUE.

   REAL :: CRITICAL_FLIN                 = 9E9
   REAL :: DPHIDX_LIMITED                = 0.
   REAL :: DPHIDY_LIMITED                = 0.
   REAL :: FLAME_LENGTH                  = 0.
   REAL :: FLIN_CANOPY                   = 0.
   REAL :: FLIN_DMS_SURFACE              = 0.
   REAL :: FLIN_SURFACE                  = 0.
   REAL :: FMC                           = 0.
   REAL :: HPUA_CANOPY                   = 0.
   REAL :: HPUA_SURFACE                  = 0.
   REAL :: IR                            = 0.
   REAL :: M1                            = 0.
   REAL :: M10                           = 0.
   REAL :: M100                          = 0.
   REAL :: MLH                           = 0.
   REAL :: MLW                           = 0.
   REAL :: NORMVECTORX                   = 0.
   REAL :: NORMVECTORY                   = 0.
   REAL :: PHIS_SURFACE                  = 0.
   REAL :: PHIW_CROWN                    = 0.
   REAL :: PHIW_SURFACE                  = 0.
   REAL :: SPREAD_DIRECTION              = 0.
   REAL :: TIME_ADDED                    = -1.0
   REAL :: TIME_OF_ARRIVAL               = -1.0
   REAL :: UX                            = 0.
   REAL :: UY                            = 0.
   REAL :: VELOCITY                      = 0.
   REAL :: VELOCITY_DMS                  = 0.
   REAL :: VS0                           = 0.
   REAL :: WD20_INTERP                   = 0.
   REAL :: WD20_NOW                      = 0.
   REAL :: WS20_INTERP                   = 0.
   REAL :: WS20_NOW                      = 0.
   REAL :: WSMF                          = 0.
   REAL :: VELOCITY_DMS_SURFACE          = 0.

! For optimization purposes
   LOGICAL   :: NEED_SLOPE_CALC = .TRUE.
   REAL      :: PHISX = -9999.
   REAL      :: PHISY = -9999.
   REAL      :: UXOUSX = -9999.
   REAL      :: UYOUSY = -9999.
   REAL      :: NORMVECTORX_DMS = -9999.
   REAL      :: NORMVECTORY_DMS = -9999.
   REAL      :: VBACK = -9999.
   REAL      :: LOW = -9999.
   INTEGER*2 :: IFBFM = 0
   REAL      :: ADJ
   REAL      :: TANSLP2
   REAL      :: PHIP_OLD
   REAL      :: DUMPME
   REAL      :: HRRPUA = 0.
   REAL      :: TIME_SUPPRESSED = -1.0 ! Used outside of suppression algorithm

#ifdef _SUPPRESSION
! Suppression
   REAL    :: SUPPRESSION_ADJUSTMENT_FACTOR = 1.0
   INTEGER :: SUPPRESSION_IDEG              = 9999
   REAL    :: SDI                           = 0.
#endif

#ifdef _SMOKE
! Smoke
   REAL :: TIME_IGNITED      = 0.
   REAL :: TIME_EXTINGUISHED = 0.
   REAL :: SMOKE_TFRAC       = 0.
   REAL :: QDOT_AVG          = 0.
#endif

#ifdef _WUI
! WUI model parameters
   TYPE(UCB_ELLIPSE) :: ELLIPSE_PARAMETERS
   INTEGER :: BLDG_FUEL_MODEL       = 0
   INTEGER :: IBLDGFM               = 1
   INTEGER :: SIGN_X                = 1
   INTEGER :: SIGN_Y                = 1
   REAL    :: BLDG_AREA             = 0. ! Was HAMADA_A
   REAL    :: BLDG_NONBURNABLE_FRAC = 0. ! Was HAMADA_FB
   REAL    :: BLDG_SEPARATION_DIST  = 0. ! Was HAMADA_D
   REAL    :: BLDG_FOOTPRINT_FRAC   = 0.
   REAL    :: RAD_DIST              = 100.
   REAL    :: WIND_PROP             = 1.
   REAL    :: HEAT_VALUE            = 0.
   REAL    :: HRR_TRANSIENT         = 0.
   REAL    :: ABSOLUTE_U            = 0.
   REAL    :: TOTAL_DFC_RECEIVED    = 0.
   REAL    :: TOTAL_RAD_RECEIVED    = 0.
#endif

#ifdef _UMDSPOTTING
! UMD spotting parameters
   REAL :: TAU_EMBERGEN            = 0.
   REAL :: LOCAL_EMBERGEN_DURATION = 0.
   REAL :: T_START_SPOTTING        = -1. 
   REAL :: T_END_SPOTTING          = -1. 
#endif
END TYPE NODE

! Wrapper of pointers DWI_SU
TYPE :: NODE_WRAPPER
    TYPE(NODE), POINTER :: PTR  ! Each wrapper holds a pointer to a NODE
END TYPE NODE_WRAPPER
 
TYPE DLL
  TYPE(NODE), POINTER :: HEAD => NULL()
  TYPE(NODE), POINTER :: TAIL => NULL()
  INTEGER :: NUM_NODES = 0
  INTEGER :: NUM_NODES_PREVIOUS = 0
  TYPE(NODE_WRAPPER), ALLOCATABLE :: NODE_POINTERS(:)   ! Array of pointers DWI_SU
END TYPE DLL
TYPE(DLL), TARGET :: LIST_TAGGED, LIST_BURNED, LIST_SUPPRESSED, LIST_VIRTUAL_STATIONS, LIST_EMBER_DEPOSITED

LOGICAL, ALLOCATABLE, DIMENSION(:) :: ALREADY_REACHED_BURNED_ACRES
LOGICAL :: PROCESS_TIMED_LOCATIONS
INTEGER :: NUM_TIMED_LOCATIONS

TYPE TIMED_LOCATIONS_TRACKER_TYPE
   INTEGER(8) :: ID
   REAL       :: X
   REAL       :: Y
   INTEGER    :: IX
   INTEGER    :: IY
END TYPE TIMED_LOCATIONS_TRACKER_TYPE
TYPE(TIMED_LOCATIONS_TRACKER_TYPE), ALLOCATABLE, DIMENSION (:) :: TIMED_LOCATIONS_TRACKER

! Weather band memory optimizations
INTEGER :: IGN_IWX_BAND_LO, IGN_IWX_BAND_HI, IWX_BAND_OFFSET=0
LOGICAL :: ONLY_READ_NEEDED_WX_BANDS = .FALSE.

! *****************************************************************************
END MODULE ELMFIRE_VARS
! *****************************************************************************
