! *****************************************************************************
MODULE ELMFIRE_SPOTTING
! *****************************************************************************

USE ELMFIRE_VARS
USE ELMFIRE_SUBS

IMPLICIT NONE

CONTAINS

! *****************************************************************************
SUBROUTINE SET_SPOTTING_PARAMETERS(R1)
! *****************************************************************************

REAL, DIMENSION(:) :: R1
INTEGER :: I
I = NUM_PARAMETERS_RASTERS

COEFFS_UNSCALED(I+1) = MEAN_SPOTTING_DIST_MIN                   + R1(I+1)*(MEAN_SPOTTING_DIST_MAX                   - MEAN_SPOTTING_DIST_MIN                  )
COEFFS_UNSCALED(I+2) = NORMALIZED_SPOTTING_DIST_VARIANCE_MIN    + R1(I+2)*(NORMALIZED_SPOTTING_DIST_VARIANCE_MAX    - NORMALIZED_SPOTTING_DIST_VARIANCE_MIN   )
COEFFS_UNSCALED(I+3) = SPOT_WS_EXP_LO                           + R1(I+3)*(SPOT_WS_EXP_HI                           - SPOT_WS_EXP_LO                          )
COEFFS_UNSCALED(I+4) = SPOT_FLIN_EXP_LO                         + R1(I+4)*(SPOT_FLIN_EXP_HI                         - SPOT_FLIN_EXP_LO                        )
COEFFS_UNSCALED(I+5) = NINT(NEMBERS_MAX_LO                      + R1(I+5)*(NEMBERS_MAX_HI                           - NEMBERS_MAX_LO                          ))
COEFFS_UNSCALED(I+6) = GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN + R1(I+6)*(GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MAX - GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN)
COEFFS_UNSCALED(I+7) = CROWN_FIRE_SPOTTING_PERCENT_MIN          + R1(I+7)*(CROWN_FIRE_SPOTTING_PERCENT_MAX          - CROWN_FIRE_SPOTTING_PERCENT_MIN         )
COEFFS_UNSCALED(I+8) = PIGN_MIN                                 + R1(I+8)*(PIGN_MAX                                 - PIGN_MIN                                )
 
MEAN_SPOTTING_DIST                   = COEFFS_UNSCALED(I+1)
NORMALIZED_SPOTTING_DIST_VARIANCE    = COEFFS_UNSCALED(I+2)
SPOT_WS_EXP                          = COEFFS_UNSCALED(I+3)
SPOT_FLIN_EXP                        = COEFFS_UNSCALED(I+4)
NEMBERS_MAX                          = COEFFS_UNSCALED(I+5)
GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT = COEFFS_UNSCALED(I+6)
CROWN_FIRE_SPOTTING_PERCENT          = COEFFS_UNSCALED(I+7)
PIGN                                 = COEFFS_UNSCALED(I+8)

SURFACE_FIRE_SPOTTING_PERCENT(:) = MIN(MAX(GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT * SURFACE_FIRE_SPOTTING_PERCENT_MULT(:),0.),100.)

! *****************************************************************************
END SUBROUTINE SET_SPOTTING_PARAMETERS
! *****************************************************************************

! *****************************************************************************
SUBROUTINE SPOTTING(IX0,IY0,WS20_NOW,FLIN, N_SPOT_FIRES, IX_SPOT_FIRE, IY_SPOT_FIRE, &
                    ICASE, DT, TIME_NOW, TAU, IGNMULT, BLDG_FOOTPRINT_FRAC_LOCAL, &
                    FMC, IFBFM, WN_FUEL)
! *****************************************************************************

IMPLICIT NONE

INTEGER, INTENT(IN) :: IX0, IY0, ICASE
INTEGER*2, INTENT(IN) :: IFBFM
INTEGER :: N_SPOT_FIRES, IX_SPOT_FIRE(:), IY_SPOT_FIRE(:)
REAL, INTENT(IN) :: WS20_NOW, FLIN, DT, TIME_NOW, TAU, IGNMULT, &
                    BLDG_FOOTPRINT_FRAC_LOCAL, FMC, WN_FUEL
REAL :: R0, X0(1:3), MSD, SIGMA_DIST, MU_DIST, MU_SPANWISE, SIGMA_SPANWISE, &
         NEMBERS_REAL, P, EMBERGEN_DT, SARDOY_PARAMETERS(1:4)
REAL, PARAMETER :: TSTOP_SPOT= 1200.

X0(1) = (REAL(IX0)-0.5) * CC%CELLSIZE 
X0(2) = (REAL(IY0)-0.5) * CC%CELLSIZE 
X0(3) = DEM%R4(IX0,IY0,1) + MAX(CH%R4(IX0,IY0,1),2.0)

MSD        = MAX( MEAN_SPOTTING_DIST*(FLIN**SPOT_FLIN_EXP)*(WS20_NOW**SPOT_WS_EXP), 1.0)
MU_DIST    = LOG(MSD*MSD / SQRT(MSD * NORMALIZED_SPOTTING_DIST_VARIANCE + MSD*MSD))
SIGMA_DIST = SQRT(LOG(1. + MSD * NORMALIZED_SPOTTING_DIST_VARIANCE / (MSD*MSD)))

IF (USE_UMD_SPOTTING_MODEL) THEN

   ! CALCULATE DISTRIBUTION PARAMETERS FROM LOCAL WIND SPEED & FIRELINE INTENSITY FROM SARDOY'S MODEL
   IF (.NOT. USE_CUSTOMIZED_PDF) THEN
      SARDOY_PARAMETERS= SARDOY_PDF_PARAMETERS(WS20_NOW, FLIN)
      MU_DIST          = SARDOY_PARAMETERS(1)
      SIGMA_DIST       = SARDOY_PARAMETERS(2)
      MU_SPANWISE      = SARDOY_PARAMETERS(3)
      SIGMA_SPANWISE   = SARDOY_PARAMETERS(4)
   ELSE
      MU_DIST          = MU_DOWNWIND
      SIGMA_DIST       = SIGMA_DOWNWIND
      MU_SPANWISE      = MU_CROSSWIND
      SIGMA_SPANWISE   = SIGMA_CROSSWIND
   ENDIF

   ! Calculate number of embers to emit
   IF (USE_PHYSICAL_EMBER_NUMBER) THEN
      ! NEMBERS_REAL = EMBER_TO_EMIT_PER_CELL(WS20_NOW, EMBER_SAMPLING_FACTOR, CC%CELLSIZE, &
      !                   BLDG_FOOTPRINT_FRAC_LOCAL, FMC, WN_FUEL, IFBFM, TAU_EMBERGEN, FLIN)
      NEMBERS_REAL = EMBER_TO_EMIT_PER_CELL(CC%CELLSIZE, IFBFM, FLIN, EMBER_GR_PER_MW_BLDG, EMBER_GR_PER_MW_VEGE)
   ELSE
      EMBERGEN_DT = MAX(MIN(DT, TAU_EMBERGEN - TAU),0.)
      NEMBERS_REAL = EMBER_GR * CC%CELLSIZE * CC%CELLSIZE * EMBERGEN_DT
   ENDIF

   NEMBERS = FLOOR(NEMBERS_REAL/EMBER_SAMPLING_FACTOR)
   P = MOD(NEMBERS_REAL,1.0)
   CALL RANDOM_NUMBER(R0)
   IF (R0 .LE. P) NEMBERS = NEMBERS + 1
   CONTINUE
ELSE
   CALL RANDOM_NUMBER(R0)
   NEMBERS = NEMBERS_MIN + NINT (R0 * REAL(NEMBERS_MAX - NEMBERS_MIN) )
ENDIF

IF (NEMBERS .EQ. 0) RETURN

IF (USE_EULERIAN_SPOTTING) THEN

   CALL EMBER_TRAJECTORY_EULERIAN(  &
      CC%NCOLS                  , &
      CC%NROWS                  , &
      CC%CELLSIZE               , &
      NEMBERS_REAL              , &
      X0                        , &
      TSTOP_SPOT                , & 
      IRANK_WORLD               , &
      SIGMA_DIST                , &
      MU_DIST                   , &
      SIGMA_SPANWISE            , &
      MU_SPANWISE               , &
      TIME_NOW)
ELSE
   CALL EMBER_TRAJECTORY ( &
      CC%NCOLS                  , &
      CC%NROWS                  , &
      CC%CELLSIZE               , &
      NEMBERS                   , &
      X0                        , & 
      TSTOP_SPOT                , & 
      PIGN                      , &
      PHIP                      , &
      IRANK_WORLD               , &
      MIN_SPOTTING_DISTANCE     , &
      MAX_SPOTTING_DISTANCE     , &
      SIGMA_DIST                , &
      MU_DIST                   , &
      SIGMA_SPANWISE            , &
      MU_SPANWISE               , &
      SPOTTING_DISTRIBUTION_TYPE, &
      N_SPOT_FIRES              , &
      IX_SPOT_FIRE              , &
      IY_SPOT_FIRE              , &
      ICASE                     , &
      TIME_NOW                  , &
      IGNMULT )
ENDIF

CONTAINS

! *****************************************************************************
FUNCTION SARDOY_PDF_PARAMETERS(WS, FI)
! *****************************************************************************
! FUNCTION CALCULATES THE SPOTTING DISTANCE DISTRIBUTION BASED ON THE SARDOY'S MODEL
! TAKE THE INPUTS LOCAL WIND SPEED AND FIRELINE INTENSITY, RETURE MU AND SIGMA
REAL, INTENT(IN) :: WS, FI
REAL, PARAMETER :: RHO_INF = 1.1 ! Air density, kg/m^2
REAL, PARAMETER :: C_PG    = 1.0 ! Air heat capacity, kJ/kg-K
REAL, PARAMETER :: T_INF   = 300.0 ! Ambient temperature, K
REAL, PARAMETER :: G       = 9.81! Gravitional acceleration, m^2/s
REAL :: I, U_WIND, LC, FR, MU_DIST, SIGMA_DIST, MU_SPANWISE, SIGMA_SPANWISE
REAL, DIMENSION(4) :: SARDOY_PDF_PARAMETERS
U_WIND = 0.447 * WS / 0.87 ! Wind speed in m/s, Use 10-m wind speed
I  = MAX(FI,1E-6) / 1000.0 ! Fireline intensity in MW/m
LC = (I*1000.0 / (RHO_INF * C_PG * T_INF * SQRT(G))) ** 0.67  ! Characteristic length scale
FR = U_WIND / SQRT(G * LC) ! Froude number

IF (FR .LE. 1.0) THEN
   MU_DIST    = (I ** 0.54) / MAX(U_WIND ** 0.55,1.0E-5)
   MU_DIST    = 1.47 * MU_DIST + 1.14
   SIGMA_DIST = (U_WIND ** 0.44) / MAX(I ** 0.21,1.0E-5) 
   SIGMA_DIST = 0.86 * SIGMA_DIST + 0.19
ELSE
   MU_DIST    = I ** 0.26 * U_WIND ** 0.11
   MU_DIST    = 1.32 * MU_DIST - 0.02
   SIGMA_DIST = 1.0 / MAX(I ** 0.01,1.0E-5) / MAX(U_WIND ** 0.02,1.0E-5)
   SIGMA_DIST = 4.95 * SIGMA_DIST - 3.48
ENDIF

MU_SPANWISE = 0.0
SIGMA_SPANWISE = 0.92 * LC
SARDOY_PDF_PARAMETERS(1) = MU_DIST
SARDOY_PDF_PARAMETERS(2) = SIGMA_DIST
SARDOY_PDF_PARAMETERS(3) = MU_SPANWISE
SARDOY_PDF_PARAMETERS(4) = SIGMA_SPANWISE

! *****************************************************************************
END FUNCTION SARDOY_PDF_PARAMETERS
! *****************************************************************************

! ! *****************************************************************************
! FUNCTION EMBER_TO_EMIT_PER_CELL(WS, N0, CELLSIZE_ELM, AF, FMC, WN_FUEL, IFBFM, TAU_EMBERGEN, FLIN)
! ! *****************************************************************************
! ! Calculates spotting distance distribution based on Sardoy's model.
! ! Takes as input local in speed and fireline intensity, reutrns MU and SIGMA
! REAL, INTENT(IN) :: WS, N0, CELLSIZE_ELM, AF, FMC, WN_FUEL, TAU_EMBERGEN, FLIN
! INTEGER*2, INTENT(IN) :: IFBFM
! REAL, PARAMETER :: D_TRUNK        = 0.2 ! Trunk diameter, m
! REAL, PARAMETER :: M_FIREBRAND    = 2.0e-4 ! firebrand mass, kg
! REAL, PARAMETER :: G       = 9.81! Gravitional acceleration, m^2/s
! REAL :: M_FUEL, U_WIND, N_EMBER, Y_FIREBRAND
! REAL :: EMBER_TO_EMIT_PER_CELL

! M_FUEL = CELLSIZE_ELM*CELLSIZE_ELM*WN_FUEL ! Available vegetation fuel mass in a cell, kg
! U_WIND = WS*0.447 ! wind speed, m/s (This is 20-ft wind, to be verified)

! IF (IFBFM .EQ. 91) THEN
!     ! Lee and Davidson, 2010, ember from structure
!     ! N_EMBER = 206.66*EXP(0.1876*U_WIND)*(CELLSIZE_ELM*CELLSIZE_ELM*AF)
!     N_EMBER = FLIN * CELLSIZE_ELM * EMBER_GR_PER_MW_VEGE
! ELSE
!     ! Ju et al, 2023, ember from vegetation
!     ! Y_FIREBRAND = 1.70*MAX(FMC,1E-6)**(-0.14)*(U_WIND/SQRT(G*D_TRUNK))**0.63+0.15
!     ! N_EMBER = Y_FIREBRAND*M_FUEL/M_FIREBRAND
!     N_EMBER = FLIN * CELLSIZE_ELM * EMBER_GR_PER_MW_BLDG
! ENDIF

! ! EMBER_TO_EMIT_PER_CELL = N_EMBER/MAX(N0*TAU_EMBERGEN,1E-6)
! EMBER_TO_EMIT_PER_CELL = N_EMBER

! ! *****************************************************************************
! END FUNCTION EMBER_TO_EMIT_PER_CELL
! ! *****************************************************************************

! *****************************************************************************
FUNCTION EMBER_TO_EMIT_PER_CELL(CELLSIZE_ELM, IFBFM, FLIN, EMBER_GR_PER_MW_BLDG, EMBER_GR_PER_MW_VEGE)
! *****************************************************************************
! Calculates spotting distance distribution based on Sardoy's model.
! Takes as input local in speed and fireline intensity, reutrns MU and SIGMA
REAL, INTENT(IN) :: CELLSIZE_ELM, FLIN, EMBER_GR_PER_MW_BLDG, EMBER_GR_PER_MW_VEGE
INTEGER*2, INTENT(IN) :: IFBFM
REAL :: EMBER_TO_EMIT_PER_CELL, N_EMBER

IF (IFBFM .EQ. 91) THEN
    N_EMBER = FLIN * CELLSIZE_ELM * EMBER_GR_PER_MW_BLDG
ELSE
    ! Ju et al, 2023, ember from vegetation
    N_EMBER = FLIN * CELLSIZE_ELM * EMBER_GR_PER_MW_VEGE
ENDIF

EMBER_TO_EMIT_PER_CELL = N_EMBER

! *****************************************************************************
END FUNCTION EMBER_TO_EMIT_PER_CELL
! *****************************************************************************

! *****************************************************************************
END SUBROUTINE SPOTTING
! *****************************************************************************

! *****************************************************************************
SUBROUTINE EMBER_TRAJECTORY( &
NX_ELM                     , &
NY_ELM                     , &
CELLSIZE_ELM               , &
NUM_EMBERS                 , &
X0_ELM                     , & 
TSTOP_ELM                  , &
PIGN_ELM                   , & 
PHIP                       , &
IRANK_WORLD                , &
MIN_SPOTTING_DISTANCE      , &
MAX_SPOTTING_DISTANCE      , &
SIGMA_DIST                 , &
MU_DIST                    , &
SIGMA_SPANWISE             , &
MU_SPANWISE                , &
SPOTTING_DISTRIBUTION_TYPE , &
N_SPOT_FIRES               , &
IX_SPOT_FIRE               , &
IY_SPOT_FIRE               , &
ICASE                      , &
TIME_NOW                   , &
IGNMULT)
! *****************************************************************************

INTEGER, INTENT(IN) :: NX_ELM, NY_ELM, NUM_EMBERS, IRANK_WORLD, ICASE
INTEGER :: N_SPOT_FIRES, IX_SPOT_FIRE(:), IY_SPOT_FIRE(:)
REAL, INTENT(IN) :: CELLSIZE_ELM, PIGN_ELM, MIN_SPOTTING_DISTANCE, MAX_SPOTTING_DISTANCE, &
                    SIGMA_DIST, MU_DIST, SIGMA_SPANWISE, MU_SPANWISE, TIME_NOW, IGNMULT
REAL, INTENT(IN) :: PHIP(:,:)

CHARACTER(60), INTENT(IN) :: SPOTTING_DISTRIBUTION_TYPE
REAL :: F_WIND, SPOTTING_DISTANCE, DIST, EPS, SPANWISE_DEVIATION, UWIND_ABS, INV_UWIND_TIMES_SPANWISE_DEVIATION

!These also come from elmfire but have local analogs:
REAL, INTENT(IN) :: X0_ELM(:), TSTOP_ELM
CHARACTER(3) :: THREE
CHARACTER(400) :: FNOUT
REAL :: R0, IGNPROB, WD1TO, WD2TO, WDTO, WS20, T, TSTOP, DT, HIGH, LOW, X_MAX, P_IGNITION, LNORM_QUANTILE, QUANTILE
REAL, DIMENSION(3) :: X, X0, UWIND, OFFSET
REAL, POINTER, DIMENSION(:,:), SAVE :: WS20_LO, WS20_HI, WD20_LO, WD20_HI
INTEGER :: IEMBER, I, J, I1, I2, J1, J2, IX, IY, IXLAST, IYLAST, ICOL, IROW, ICOUNT, K_MAX, ITLO_METEOROLOGY, ITHI_METEOROLOGY, IFBFM, IBLDGFM
LOGICAL :: GO
REAL, PARAMETER :: SQRT_2 = 1.4142135623731

X0   (:) = X0_ELM(:)               ! Initial position vector
TSTOP    = TSTOP_ELM               ! Stop time 

WRITE(THREE, '(I3.3)') IRANK_WORLD
FNOUT = 'ignitions_' // THREE // '.csv'

UWIND (3) = 0. 
OFFSET(3) = 0.

! Find the maximum spotting distance accoring to criterion P_EPS (P_EPS=0.001 by default)
IF (USE_UMD_SPOTTING_MODEL) THEN
   QUANTILE=SQRT_2*ERFINV(2.0*P_EPS-1.0)
   LNORM_QUANTILE = EXP(MU_DIST + SIGMA_DIST * QUANTILE)
   K_MAX = NINT(LNORM_QUANTILE/CELLSIZE_ELM)
   X_MAX = K_MAX*CELLSIZE_ELM
ENDIF

DO IEMBER = 1, NUM_EMBERS

   CALL RANDOM_NUMBER(R0); EPS = 8.*(R0 - 0.5)

! Get spotting distance
   CALL RANDOM_NUMBER(R0)
   IF (TRIM(SPOTTING_DISTRIBUTION_TYPE) .EQ. 'UNIFORM') THEN   
      SPOTTING_DISTANCE = MIN_SPOTTING_DISTANCE + R0 * (MAX_SPOTTING_DISTANCE - MIN_SPOTTING_DISTANCE)
   ELSE IF (USE_UMD_SPOTTING_MODEL) THEN
      LOW  = SARDOY_PDFINV(0.0, MU_DIST, SIGMA_DIST)
      HIGH = SARDOY_PDFINV(X_MAX+CELLSIZE_ELM*0.5, MU_DIST, SIGMA_DIST)
      R0   = R0 * (HIGH - LOW) + LOW
      SPOTTING_DISTANCE = EXP(SQRT(2.) * SIGMA_DIST * ERFINV(2.*R0-1.) + MU_DIST)
      SPOTTING_DISTANCE = NINT(SPOTTING_DISTANCE/CELLSIZE_ELM)*CELLSIZE_ELM
   ELSE !Lognormal
      IF (R0 .GT. 0.5) THEN
         SPOTTING_DISTANCE = EXP(SQRT(2.) * SIGMA_DIST * ERFINV(2.*R0-1.) + MU_DIST)
      ELSE
         SPOTTING_DISTANCE = EXP(MU_DIST - SQRT(2.) * SIGMA_DIST * ERFINV(1.-2.*R0))
      ENDIF      
   ENDIF

   DIST = 0.

   OFFSET(1:2) = X0(1:2)
   X   (:)   = X0(:) - OFFSET(:)
   T         = 0.
  
   IXLAST = 0
   IYLAST = 0

   IX = CEILING ((X(1) + OFFSET(1)) / CELLSIZE_ELM)
   IX = MAX(IX,1) ; IX = MIN (IX,NX_ELM)
   ICOL = ICOL_ANALYSIS_F2C(IX)

   IY = CEILING ((X(2) + OFFSET(2)) / CELLSIZE_ELM)
   IY = MAX(IY,1) ; IY = MIN (IY,NY_ELM)
   IROW = IROW_ANALYSIS_F2C(IY)

   ITLO_METEOROLOGY = MAX(1 + FLOOR((T+TIME_NOW) / DT_METEOROLOGY),1)
   ITLO_METEOROLOGY = MIN(ITLO_METEOROLOGY, NUM_METEOROLOGY_TIMES)
   ITHI_METEOROLOGY = MIN(ITLO_METEOROLOGY + 1, NUM_METEOROLOGY_TIMES)
   F_WIND = (T + TIME_NOW - REAL(ITLO_METEOROLOGY-1) * DT_METEOROLOGY) / DT_METEOROLOGY
   IF (ITLO_METEOROLOGY .EQ. ITHI_METEOROLOGY) F_WIND = 1.

   WS20_LO => WSP   (:,:,ITLO_METEOROLOGY)
   WS20_HI => WSP   (:,:,ITHI_METEOROLOGY)
   WD20_LO => WDP   (:,:,ITLO_METEOROLOGY)
   WD20_HI => WDP   (:,:,ITHI_METEOROLOGY)
   
   WS20 = WS20_LO(ICOL,IROW) * (1. - F_WIND) + F_WIND * WS20_HI(ICOL,IROW) 
   WS20 = 0.447 * WS20

   IF (USE_HALF_CFL_DT_FOR_SPOTTING) THEN
      DT = MIN ( 0.5 * CELLSIZE_ELM / MAX (WS20, 0.01), 5.0)
   ELSE
      DT =  CELLSIZE_ELM / MAX(WS20, 0.01) 
   ENDIF

   IF (USE_UMD_SPOTTING_MODEL) THEN
      NUM_TRACKED_EMBERS = MIN (NUM_TRACKED_EMBERS + 1, EMBER_TRACKER_SIZE)
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%X_FROM            = X(1) + OFFSET(1)
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%Y_FROM            = X(2) + OFFSET(2)
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%IX_FROM           = IX
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%IY_FROM           = IY
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%X_TO              = -9E9
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%Y_TO              = -9E9
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%IX_TO             = -9999
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%IY_TO             = -9999
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%DIST              = -9999
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%TTRAVEL           = -9E9
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%TLAUNCH           = TIME_NOW
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%TIGN              = -9E9
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%POSITIVE_IGNITION = .FALSE.
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%ALREADY_IGNITED   = .FALSE.
   ENDIF

   ICOUNT = 0

   DO WHILE (T .LT. TSTOP .AND. DIST .LT. SPOTTING_DISTANCE )
      ICOUNT = ICOUNT + 1
      T = T + DT
      IX = CEILING ((X(1) + OFFSET(1)) / CELLSIZE_ELM)
      IX = MAX(IX,1) ; IX = MIN (IX,NX_ELM)
      ICOL = ICOL_ANALYSIS_F2C(IX)

      IY = CEILING ((X(2) + OFFSET(2)) / CELLSIZE_ELM)
      IY = MAX(IY,1) ; IY = MIN (IY,NY_ELM)
      IROW = IROW_ANALYSIS_F2C(IY)

      IF (IX .NE. IXLAST .OR. IY .NE. IYLAST) THEN
         IF (IX .GE. NX_ELM .OR. IX .LE. 1) THEN
            T = 9E9; CYCLE
         ENDIF
         IF (IY .GE. NY_ELM .OR. IY .LE. 1) THEN
            T = 9E9; CYCLE
         ENDIF

         ITLO_METEOROLOGY = MAX(1 + FLOOR((T+TIME_NOW) / DT_METEOROLOGY),1)
         ITLO_METEOROLOGY = MIN(ITLO_METEOROLOGY, NUM_METEOROLOGY_TIMES)
         ITHI_METEOROLOGY = MIN(ITLO_METEOROLOGY + 1, NUM_METEOROLOGY_TIMES)
         F_WIND = (T + TIME_NOW - REAL(ITLO_METEOROLOGY-1) * DT_METEOROLOGY) / DT_METEOROLOGY
         IF (ITLO_METEOROLOGY .EQ. ITHI_METEOROLOGY) F_WIND = 1.

         WS20_LO => WSP   (:,:,ITLO_METEOROLOGY)
         WS20_HI => WSP   (:,:,ITHI_METEOROLOGY)
         WD20_LO => WDP   (:,:,ITLO_METEOROLOGY)
         WD20_HI => WDP   (:,:,ITHI_METEOROLOGY)

         WS20 = WS20_LO(ICOL,IROW) * (1. - F_WIND) + F_WIND * WS20_HI(ICOL,IROW) 
         WS20 = 0.447 * WS20

         IF (USE_HALF_CFL_DT_FOR_SPOTTING) THEN
            DT = MIN ( 0.5 * CELLSIZE_ELM / MAX (WS20, 0.01), 5.0)
         ELSE
            DT =  CELLSIZE_ELM / MAX(WS20, 0.01) 
         ENDIF

         WD1TO = WD20_LO(ICOL,IROW) + 180. ; IF (WD1TO .GT. 360) WD1TO = WD1TO - 360.
         WD2TO = WD20_HI(ICOL,IROW) + 180. ; IF (WD2TO .GT. 360) WD2TO = WD2TO - 360.

         WDTO  = WD1TO + F_WIND * (WD2TO - WD1TO)
         WDTO  = WDTO + EPS
         IF (WDTO .GT. 360.) WDTO = WDTO - 360.
         IF (WDTO .LT.   0.) WDTO = WDTO + 360.

         UWIND(1) = WS20 * SIN(WDTO*PI/180.)
         UWIND(2) = WS20 * COS(WDTO*PI/180.)
      ENDIF

      IF (ABS(UWIND(1)) .LT. 1E-6 .AND. ABS(UWIND(2)) .LT. 1E-6) T=9E9
      IF (ICOUNT .GT. 100000) T=9E9

      X(1:2)   = X(1:2) + UWIND(1:2) * DT
      DIST     = DIST + WS20 * DT
      
      IXLAST = IX
      IYLAST = IY
   ENDDO

   IF (T .LT. 1E9) THEN
      ! Add spanwise distribution
      IF (USE_UMD_SPOTTING_MODEL) THEN
         CALL RANDOM_NUMBER(R0)
         SPANWISE_DEVIATION = SQRT_2 * ERFINV(2.0*R0-1.0) * SIGMA_SPANWISE + MU_SPANWISE

         UWIND_ABS = NORM2(UWIND(1:2))
         INV_UWIND_TIMES_SPANWISE_DEVIATION = SPANWISE_DEVIATION/MAX(1E-6,UWIND_ABS)
         X(1)=X(1)+(COS(90*PI/180.)*UWIND(1)-SIN(90*PI/180.)*UWIND(2))*INV_UWIND_TIMES_SPANWISE_DEVIATION
         X(2)=X(2)+(SIN(90*PI/180.)*UWIND(1)+COS(90*PI/180.)*UWIND(2))*INV_UWIND_TIMES_SPANWISE_DEVIATION      
      ENDIF

      IX = CEILING ((X(1) + OFFSET(1)) / CELLSIZE_ELM) ; IX = MAX(IX,1) ; IX = MIN (IX,NX_ELM)
      IY = CEILING ((X(2) + OFFSET(2)) / CELLSIZE_ELM) ; IY = MAX(IY,1) ; IY = MIN (IY,NY_ELM)
      IF (DUMP_EMBER_FLUX) EMBER_FLUX%R4(IX,IY,1) = EMBER_FLUX%R4(IX,IY,1) + EMBER_SAMPLING_FACTOR
      STATS_NEMBERS(ICASE) = STATS_NEMBERS(ICASE) + 1.
      IF (USE_EMBER_COUNT_BINS) EMBER_COUNT(IX,IY) = EMBER_COUNT(IX,IY) + 1

      IF (USE_UMD_SPOTTING_MODEL) THEN
         SPOTTING_STATS(NUM_TRACKED_EMBERS)%X_TO    = X(1) + OFFSET(1)
         SPOTTING_STATS(NUM_TRACKED_EMBERS)%Y_TO    = X(2) + OFFSET(2)
         SPOTTING_STATS(NUM_TRACKED_EMBERS)%IX_TO   = IX
         SPOTTING_STATS(NUM_TRACKED_EMBERS)%IY_TO   = IY
         SPOTTING_STATS(NUM_TRACKED_EMBERS)%DIST    = DIST
         SPOTTING_STATS(NUM_TRACKED_EMBERS)%TTRAVEL = T
         SPOTTING_STATS(NUM_TRACKED_EMBERS)%TIGN    = T+TIME_NOW
      ENDIF

      ! Following modification per UCB's request to apply building model-sensitive ignition probability by Yiren
      IFBFM = FBFM%I2(IX,IY,1)
      IF(IFBFM .NE. 91) THEN
         P_IGNITION = PIGN_ELM
      ELSE
         IF(USE_BLDG_SPREAD_MODEL .AND. BLDG_SPREAD_MODEL_TYPE .EQ. 2) THEN
            IBLDGFM = BLDG_FUEL_MODEL%I2(IX,IY,1)
            P_IGNITION = BUILDING_FUEL_MODEL_TABLE(IBLDGFM)%P_IGNITION
         ELSE
            P_IGNITION = PIGN_ELM
         ENDIF
      ENDIF
      IGNPROB=0.01*P_IGNITION*IGNMULT

      CALL RANDOM_NUMBER(R0)

      IF (IGNPROB .GT. R0 .AND. DIST .GT. 0.5*CELLSIZE_ELM) THEN
         
         GO = .TRUE. 
         IF (.NOT. USE_UMD_SPOTTING_MODEL) THEN
            I1 = MAX(IX-3,1     )
            I2 = MIN(IX+3,NX_ELM)
            J1 = MAX(IY-3,1     )
            J2 = MIN(IY+3,NY_ELM)
            
            DO J = J1, J2
            DO I = I1, I2
               IF (PHIP(I,J) .LT. 0.) GO = .FALSE.
            ENDDO
            ENDDO
         ENDIF

         IF (GO) THEN
            IF (USE_UMD_SPOTTING_MODEL) THEN
               SPOTTING_STATS(NUM_TRACKED_EMBERS)%POSITIVE_IGNITION = .TRUE.
            ELSE
               N_SPOT_FIRES = N_SPOT_FIRES + 1
               IX_SPOT_FIRE(N_SPOT_FIRES) = IX
               IY_SPOT_FIRE(N_SPOT_FIRES) = IY
            ENDIF
         ENDIF
      ENDIF

   ENDIF

ENDDO

CONTAINS

! *****************************************************************************
REAL FUNCTION SARDOY_PDFINV(X, MU_DIST, SIGMA_DIST)
! *****************************************************************************

REAL, INTENT(IN) :: X, MU_DIST, SIGMA_DIST
REAL, PARAMETER :: SQRT_2 = 1.4142135623731

SARDOY_PDFINV = 0.5*(1+ERF((LOG(MAX(X,1E-6))-MU_DIST)/SQRT_2/SIGMA_DIST))

! *****************************************************************************
END FUNCTION SARDOY_PDFINV
! *****************************************************************************

! *****************************************************************************
END SUBROUTINE EMBER_TRAJECTORY
! *****************************************************************************

! *****************************************************************************
SUBROUTINE EMBER_TRAJECTORY_EULERIAN( &
NX_ELM                     , &
NY_ELM                     , &
CELLSIZE_ELM               , &
NUM_EMBERS                 , &
X0_ELM                     , &
TSTOP_ELM                  , &
IRANK_WORLD                , &
SIGMA_DIST                 , &
MU_DIST                    , &
SIGMA_SPANWISE             , &
MU_SPANWISE                , &
TIME_NOW)
! *****************************************************************************

INTEGER, INTENT(IN) :: NX_ELM, NY_ELM, IRANK_WORLD
REAL, INTENT(IN) :: CELLSIZE_ELM, SIGMA_DIST, MU_DIST, SIGMA_SPANWISE, MU_SPANWISE, NUM_EMBERS 

REAL :: DIST, EPS

!These also come from elmfire but have local analogs:
REAL, INTENT(IN) :: X0_ELM(:), TSTOP_ELM, TIME_NOW
CHARACTER(3) :: THREE
CHARACTER(400) :: FNOUT
REAL :: R0, WD1TO, WD2TO, WDTO, WS20, T, TSTOP, DT, DIST_LAST, F_WIND, X_MAX, Y_MAX, P_LAND, P_LAND_SPANWISE, LNORM_QUANTILE, &
        NORM_QUANTILE, QUANTILE, NORM_FACTOR_SPANWISE, NORM_FACTOR, X_SPANWISE, Y_SPANWISE, INV_UWIND_TIMES_SPANWISE_DEVIATION, UWIND_ABS
REAL, DIMENSION(3) :: X, X0, UWIND, OFFSET
INTEGER :: IX, IY, IXLAST, IYLAST,ICOL, IROW, ICOUNT, K_MAX, K_MAX_SPANWISE, IT_IGN, ITLO_METEOROLOGY, ITHI_METEOROLOGY, I, &
           IX_SPANWISE, IY_SPANWISE
REAL, POINTER, DIMENSION(:,:), SAVE :: WS20_LO_SPOTTING, WS20_HI_SPOTTING, WD20_LO_SPOTTING, WD20_HI_SPOTTING
REAL, PARAMETER :: SQRT_2 = 1.4142135623731

X0   (:) = X0_ELM(:)               ! Initial position vector
TSTOP    = TSTOP_ELM               ! Stop time 

WRITE(THREE, '(I3.3)') IRANK_WORLD
FNOUT = 'ignitions_' // THREE // '.csv'

UWIND (3) = 0. 
OFFSET(3) = 0.

! Find the maximum spotting distance accoring to criterion P_EPS (P_EPS=0.001 by default)
IF (USE_UMD_SPOTTING_MODEL) THEN
   QUANTILE=SQRT_2*ERFINV(2.0*(1.0-P_EPS)-1.0)
   LNORM_QUANTILE = EXP(MU_DIST + SIGMA_DIST * QUANTILE)
   NORM_QUANTILE  = MU_SPANWISE + SIGMA_SPANWISE * QUANTILE
   K_MAX = NINT(LNORM_QUANTILE/CELLSIZE_ELM)
   K_MAX_SPANWISE = CEILING(NORM_QUANTILE/CELLSIZE_ELM)
   K_MAX_SPANWISE = MAX(K_MAX_SPANWISE, 1)
   X_MAX = K_MAX*CELLSIZE_ELM
   Y_MAX = K_MAX_SPANWISE*CELLSIZE_ELM
ENDIF

NORM_FACTOR = SARDOY_CDF(1E-6,REAL(K_MAX)*CELLSIZE_ELM, MU_DIST, SIGMA_DIST)
IF (K_MAX_SPANWISE .GT. 1) THEN
   NORM_FACTOR_SPANWISE = 0.5*(ERF((Y_MAX+CELLSIZE_ELM*0.5-MU_SPANWISE)/SQRT_2/SIGMA_SPANWISE)- &
                               ERF((-Y_MAX-CELLSIZE_ELM*0.5-MU_SPANWISE)/SQRT_2/SIGMA_SPANWISE))
ELSE
   NORM_FACTOR_SPANWISE = 1.0
ENDIF

DIST = 0.
DIST_LAST = DIST

OFFSET(1:2) = X0(1:2)
X   (:)   = X0(:) - OFFSET(:)
T         = 0.

IXLAST = 0
IYLAST = 0

IX = CEILING ((X(1) + OFFSET(1)) / CELLSIZE_ELM)
IX = MAX(IX,1) ; IX = MIN (IX,NX_ELM)
ICOL = ICOL_ANALYSIS_F2C(IX)

IY = CEILING ((X(2) + OFFSET(2)) / CELLSIZE_ELM)
IY = MAX(IY,1) ; IY = MIN (IY,NY_ELM)
IROW = IROW_ANALYSIS_F2C(IY)


ITLO_METEOROLOGY = MAX(1 + FLOOR((T+TIME_NOW) / DT_METEOROLOGY),1)
ITLO_METEOROLOGY = MIN(ITLO_METEOROLOGY, NUM_METEOROLOGY_TIMES)
ITHI_METEOROLOGY = MIN(ITLO_METEOROLOGY + 1, NUM_METEOROLOGY_TIMES)
F_WIND = (T + TIME_NOW  - REAL(ITLO_METEOROLOGY-1) * DT_METEOROLOGY) / DT_METEOROLOGY
IF (ITLO_METEOROLOGY .EQ. ITHI_METEOROLOGY) F_WIND = 1.

WS20_LO_SPOTTING => WSP   (:,:,ITLO_METEOROLOGY)
WS20_HI_SPOTTING => WSP   (:,:,ITHI_METEOROLOGY)

WS20 = WS20_LO_SPOTTING(ICOL,IROW) * (1. - F_WIND) + F_WIND * WS20_HI_SPOTTING(ICOL,IROW) 
WS20 = 0.447 * WS20

IF (USE_HALF_CFL_DT_FOR_SPOTTING) THEN
   DT = MIN ( 0.5 * CELLSIZE_ELM / MAX (WS20, 0.01), 5.0)
ELSE
   DT =  CELLSIZE_ELM / MAX(WS20, 0.01) 
ENDIF

ICOUNT = 0

CALL RANDOM_NUMBER(R0); EPS = 8.*(R0 - 0.5)

DO WHILE (T .LT. TSTOP .AND. DIST .LT. X_MAX )
   ICOUNT = ICOUNT + 1
   T = T + DT
   IX = CEILING ((X(1) + OFFSET(1)) / CELLSIZE_ELM)
   IX = MAX(IX,1) ; IX = MIN (IX,NX_ELM)
   ICOL = ICOL_ANALYSIS_F2C(IX)

   IY = CEILING ((X(2) + OFFSET(2)) / CELLSIZE_ELM)
   IY = MAX(IY,1) ; IY = MIN (IY,NY_ELM)
   IROW = IROW_ANALYSIS_F2C(IY)

   IF (IX .NE. IXLAST .OR. IY .NE. IYLAST) THEN
      IF (IX .GE. NX_ELM .OR. IX .LE. 1) THEN
         T = 9E9; CYCLE
      ENDIF
      IF (IY .GE. NY_ELM .OR. IY .LE. 1) THEN
         T = 9E9; CYCLE
      ENDIF

      ITLO_METEOROLOGY = MAX(1 + FLOOR((T+TIME_NOW) / DT_METEOROLOGY),1)
      ITLO_METEOROLOGY = MIN(ITLO_METEOROLOGY, NUM_METEOROLOGY_TIMES)
      ITHI_METEOROLOGY = MIN(ITLO_METEOROLOGY + 1, NUM_METEOROLOGY_TIMES)
      F_WIND = (T + TIME_NOW - REAL(ITLO_METEOROLOGY-1) * DT_METEOROLOGY) / DT_METEOROLOGY
      IF (ITLO_METEOROLOGY .EQ. ITHI_METEOROLOGY) F_WIND = 1.

      WS20_LO_SPOTTING => WSP   (:,:,ITLO_METEOROLOGY)
      WS20_HI_SPOTTING => WSP   (:,:,ITHI_METEOROLOGY)
      WD20_LO_SPOTTING => WDP   (:,:,ITLO_METEOROLOGY)
      WD20_HI_SPOTTING => WDP   (:,:,ITHI_METEOROLOGY)

      WS20 = WS20_LO_SPOTTING(ICOL,IROW) * (1. - F_WIND) + F_WIND * WS20_HI_SPOTTING(ICOL,IROW) 
      WS20 = 0.447 * WS20

      IF (USE_HALF_CFL_DT_FOR_SPOTTING) THEN
         DT = MIN ( 0.5 * CELLSIZE_ELM / MAX (WS20, 0.01), 5.0)
      ELSE
         DT =  CELLSIZE_ELM / MAX(WS20, 0.01) 
      ENDIF

      WD1TO = WD20_LO_SPOTTING(ICOL,IROW) + 180. ; IF (WD1TO .GT. 360) WD1TO = WD1TO - 360.
      WD2TO = WD20_HI_SPOTTING(ICOL,IROW) + 180. ; IF (WD2TO .GT. 360) WD2TO = WD2TO - 360.

      WDTO  = WD1TO + F_WIND * (WD2TO - WD1TO)
      WDTO  = WDTO + EPS
      IF (WDTO .GT. 360.) WDTO = WDTO - 360.
      IF (WDTO .LT.   0.) WDTO = WDTO + 360.

      UWIND(1) = WS20 * SIN(WDTO*PI/180.)
      UWIND(2) = WS20 * COS(WDTO*PI/180.)
   ENDIF

   IF (ABS(UWIND(1)) .LT. 1E-6 .AND. ABS(UWIND(2)) .LT. 1E-6) T=9E9
   IF (ICOUNT .GT. 100000) T=9E9
   
   X(1:2)   = X(1:2) + UWIND(1:2) * DT
   DIST     = DIST + WS20 * DT
   
   IXLAST = IX
   IYLAST = IY

   ! Filling entries for ember flux table, which will be used as input for the ignition model
   IX = CEILING ((X(1) + OFFSET(1)) / CELLSIZE_ELM)
   IX = MAX(IX,1) ; IX = MIN (IX,NX_ELM)

   IY = CEILING ((X(2) + OFFSET(2)) / CELLSIZE_ELM)
   IY = MAX(IY,1) ; IY = MIN (IY,NY_ELM)

   IT_IGN = CEILING((T+TIME_NOW)/DT_DUMP_EMBER_FLUX)
   IT_IGN = MAX(IT_IGN,1)
   IT_IGN = MIN(IT_IGN,EMBER_FLUX_TABLE_LEN)
   P_LAND = SARDOY_CDF(DIST_LAST,DIST, MU_DIST, SIGMA_DIST)/MAX(NORM_FACTOR,1E-6)

   CALL RANDOM_NUMBER(R0)

   UWIND_ABS = NORM2(UWIND(1:2))

   IF (K_MAX_SPANWISE .GT. 1) THEN
      P_LAND_SPANWISE = 0.5*(ERF((CELLSIZE_ELM*0.5-MU_SPANWISE)/SQRT_2/SIGMA_SPANWISE)- &
                             ERF((-CELLSIZE_ELM*0.5-MU_SPANWISE)/SQRT_2/SIGMA_SPANWISE))/NORM_FACTOR_SPANWISE
      EMBER_FLUX%R4(IX,IY,IT_IGN) = EMBER_FLUX%R4(IX,IY,IT_IGN) + NUM_EMBERS*P_LAND*P_LAND_SPANWISE
      ! If probability of ignition is 100%, EMBER_TIGN will be used, avoid allocating a big array of EMBER_ACCUMULATION_RATE
      IF(T+TIME_NOW .LT. EMBER_TIGN(IX,IY) .OR. EMBER_TIGN(IX,IY) .LT. 0.0) THEN
         EMBER_TIGN(IX,IY) = T+TIME_NOW
      ENDIF
      DO I=2, (K_MAX_SPANWISE+1)
         P_LAND_SPANWISE = 0.5*(ERF((CELLSIZE_ELM*(I-1+0.5)-MU_SPANWISE)/SQRT_2/SIGMA_SPANWISE)- &
                                ERF((CELLSIZE_ELM*(I-2+0.5)-MU_SPANWISE)/SQRT_2/SIGMA_SPANWISE))/NORM_FACTOR_SPANWISE
         ! Side-1
         INV_UWIND_TIMES_SPANWISE_DEVIATION = CELLSIZE_ELM*(I-1)/MAX(1E-6,UWIND_ABS)
         X_SPANWISE=X(1)+(COS(90*PI/180.)*UWIND(1)-SIN(90*PI/180.)*UWIND(2))*INV_UWIND_TIMES_SPANWISE_DEVIATION
         Y_SPANWISE=X(2)+(SIN(90*PI/180.)*UWIND(1)+COS(90*PI/180.)*UWIND(2))*INV_UWIND_TIMES_SPANWISE_DEVIATION

         ! These lines ensures a symetric distribution on the map in ideal conditions
         IX_SPANWISE = CEILING ((X_SPANWISE + OFFSET(1)) / CELLSIZE_ELM)
         IX_SPANWISE = MAX(IX_SPANWISE,1) ; IX_SPANWISE = MIN (IX_SPANWISE,NX_ELM)

         IY_SPANWISE = CEILING ((Y_SPANWISE + OFFSET(2)) / CELLSIZE_ELM)
         IY_SPANWISE = MAX(IY_SPANWISE,1) ; IY_SPANWISE = MIN (IY_SPANWISE,NY_ELM)

         EMBER_FLUX%R4(IX_SPANWISE,IY_SPANWISE,IT_IGN) = EMBER_FLUX%R4(IX_SPANWISE,IY_SPANWISE,IT_IGN) + NUM_EMBERS*P_LAND*P_LAND_SPANWISE
         ! If probability of ignition is 100%, EMBER_TIGN will be used, avoid allocating a big array of EMBER_ACCUMULATION_RATE
         IF(T+TIME_NOW .LT. EMBER_TIGN(IX_SPANWISE,IY_SPANWISE) .OR. EMBER_TIGN(IX_SPANWISE,IY_SPANWISE) .LT. 0.0) THEN
            EMBER_TIGN(IX_SPANWISE,IY_SPANWISE) = T+TIME_NOW
         ENDIF
         ! Side-2
         INV_UWIND_TIMES_SPANWISE_DEVIATION = -CELLSIZE_ELM*(I-1)/MAX(1E-6,UWIND_ABS)
         X_SPANWISE=X(1)+(COS(90*PI/180.)*UWIND(1)-SIN(90*PI/180.)*UWIND(2))*INV_UWIND_TIMES_SPANWISE_DEVIATION
         Y_SPANWISE=X(2)+(SIN(90*PI/180.)*UWIND(1)+COS(90*PI/180.)*UWIND(2))*INV_UWIND_TIMES_SPANWISE_DEVIATION

         IX_SPANWISE = CEILING ((X_SPANWISE + OFFSET(1)) / CELLSIZE_ELM)
         IX_SPANWISE = MAX(IX_SPANWISE,1) ; IX_SPANWISE = MIN (IX_SPANWISE,NX_ELM)
         
         IY_SPANWISE = CEILING ((Y_SPANWISE + OFFSET(2)) / CELLSIZE_ELM)
         IY_SPANWISE = MAX(IY_SPANWISE,1) ; IY_SPANWISE = MIN (IY_SPANWISE,NY_ELM)

         EMBER_FLUX%R4(IX_SPANWISE,IY_SPANWISE,IT_IGN) = EMBER_FLUX%R4(IX_SPANWISE,IY_SPANWISE,IT_IGN) + NUM_EMBERS*P_LAND*P_LAND_SPANWISE
         IF(T+TIME_NOW .LT. EMBER_TIGN(IX_SPANWISE,IY_SPANWISE) .OR. EMBER_TIGN(IX_SPANWISE,IY_SPANWISE) .LT. 0.0) THEN
            EMBER_TIGN(IX_SPANWISE,IY_SPANWISE) = T+TIME_NOW
         ENDIF
      ENDDO
   ELSE
      EMBER_FLUX%R4(IX,IY,IT_IGN) = EMBER_FLUX%R4(IX,IY,IT_IGN) + NUM_EMBERS*P_LAND
      ! If probability of ignition is 100%, EMBER_TIGN will be used, avoid allocating a big array of EMBER_ACCUMULATION_RATE
      IF(T+TIME_NOW .LT. EMBER_TIGN(IX,IY) .OR. EMBER_TIGN(IX,IY) .LT. 0.0) THEN
         EMBER_TIGN(IX,IY) = T+TIME_NOW
      ENDIF
   ENDIF

   

   DIST_LAST = DIST

ENDDO

CONTAINS

! *****************************************************************************
REAL FUNCTION SARDOY_CDF(X_START, X_END, MU_DIST, SIGMA_DIST)
! *****************************************************************************

REAL, INTENT(IN) :: X_START, X_END,  MU_DIST, SIGMA_DIST
REAL, PARAMETER :: SQRT_2 = 1.4142135623731

SARDOY_CDF = 0.5*(1+ERF((LOG(MAX(X_END,1E-6))-MU_DIST)/SQRT_2/SIGMA_DIST)) - &
             0.5*(1+ERF((LOG(MAX(X_START,1E-6))-MU_DIST)/SQRT_2/SIGMA_DIST))

! *****************************************************************************
END FUNCTION SARDOY_CDF
! *****************************************************************************

! *****************************************************************************
END SUBROUTINE EMBER_TRAJECTORY_EULERIAN
! *****************************************************************************

! The following model was substituted by the UCB model
! ! *****************************************************************************
! SUBROUTINE STRUCTURE_DESIGN_FIRE_CURVE(C, STRUCTURE_AREA, T_ELMFIRE)
! ! *****************************************************************************
! ! This is to calculate the fireline intensity of a structural pixel since its iginition
! ! The fireline intensity will be used to determine the ember emitting duration
! USE ELMFIRE_VARS

! ! T_ALPHA(1:256) ! Time needed to reach the heat release of 1000 KW
! ! Q_F(1:256) ! Fire load
! ! HRR_F! is the thermal heat energy release per unite of gross floor area. Appendix E.4 of Eurocode 1 UNI EN 1991-1-2 
!        ! shows some values of HHRf for different occupancies [kW/sqm];
! ! HRR_MAX = HRR_F*A_F ! kW

! TYPE(NODE), POINTER, INTENT(INOUT) :: C

! REAL, INTENT(IN) :: STRUCTURE_AREA, T_ELMFIRE

! REAL, PARAMETER :: T_ALPHA = 300.0, Q_F = 1000000.0

! REAL :: TIME_SINCE_IGNITION, T_MAX, T_DECAY, T_TOTAL, HRR_MAX, HRR, &
!         T_EMBERGEN_START, T_EMBERGEN_END

! TIME_SINCE_IGNITION = T_ELMFIRE-C%TIME_OF_ARRIVAL

! ! Use Q_dot = Q_dot0*t^2 for growth
! HRR_MAX = 400.0*STRUCTURE_AREA ! kW
! T_MAX   = SQRT(HRR_MAX*T_ALPHA**2.0/1000.0)
! T_DECAY = T_MAX+(0.7*Q_F*STRUCTURE_AREA - 1000.0/3.0/T_ALPHA/T_ALPHA*T_MAX**3.0)/HRR_MAX
! T_TOTAL = T_DECAY+2*0.3*Q_F*STRUCTURE_AREA/HRR_MAX

! T_EMBERGEN_START = SQRT(CRITICAL_SPOTTING_FIRELINE_INTENSITY(FBFM%I2(C%IX,C%IY,1))/1000.0)*T_ALPHA
! T_EMBERGEN_START = MIN(T_EMBERGEN_START,T_MAX)
! T_EMBERGEN_START = MAX(T_EMBERGEN_START,0.0)

! T_EMBERGEN_END = T_TOTAL-CRITICAL_SPOTTING_FIRELINE_INTENSITY(FBFM%I2(C%IX,C%IY,1))/HRR_MAX*(T_TOTAL-T_DECAY);
! T_EMBERGEN_END = MAX(T_EMBERGEN_END,T_DECAY)
! T_EMBERGEN_END = MIN(T_EMBERGEN_END,T_TOTAL)

! C%LOCAL_EMBERGEN_DURATION   = T_EMBERGEN_END-T_EMBERGEN_START

! IF (C%TIME_OF_ARRIVAL .LT. 0.0) THEN
!     C%FLIN_SURFACE = 0.0
! ELSE
!     IF (TIME_SINCE_IGNITION .LT. T_MAX .AND. TIME_SINCE_IGNITION .GE. 0.0) THEN
!         HRR = 1000.0*(TIME_SINCE_IGNITION/T_ALPHA)**2.0
!         C%FLIN_SURFACE = HRR/CC%CELLSIZE
!     ELSEIF (TIME_SINCE_IGNITION .GE. T_MAX .AND. TIME_SINCE_IGNITION .LT. T_DECAY) THEN
!         HRR = HRR_MAX
!         C%FLIN_SURFACE = HRR/CC%CELLSIZE
!     ELSE
!         HRR = HRR_MAX*(T_TOTAL-TIME_SINCE_IGNITION)/(T_TOTAL-T_DECAY)
!         IF (HRR .LE. 0) HRR = 0.0
!         C%FLIN_SURFACE = HRR/CC%CELLSIZE
!     ENDIF
! ENDIF

! ! *****************************************************************************
! END SUBROUTINE STRUCTURE_DESIGN_FIRE_CURVE
! ! *****************************************************************************

! *****************************************************************************
SUBROUTINE CLEAR_USED_EMBER(T_ELMFIRE)
! *****************************************************************************
! Subroutine to delete used particles, save space for Lagrangian scheme
USE ELMFIRE_VARS

REAL, INTENT(IN) :: T_ELMFIRE

INTEGER :: I, NUM_UNUSED_EMBERS
TYPE(SPOTTING_TRACKER), ALLOCATABLE, DIMENSION(:) :: SPOTTING_STATS_TEMP

ALLOCATE(SPOTTING_STATS_TEMP(SIZE(SPOTTING_STATS)))
SPOTTING_STATS_TEMP = SPOTTING_STATS

NUM_UNUSED_EMBERS=0

DO I=1,NUM_TRACKED_EMBERS
   IF (SPOTTING_STATS(I)%TIGN .GE. T_ELMFIRE) THEN
      NUM_UNUSED_EMBERS=NUM_UNUSED_EMBERS+1
      SPOTTING_STATS_TEMP(NUM_UNUSED_EMBERS) = SPOTTING_STATS(I)
   ENDIF
ENDDO
NUM_TRACKED_EMBERS =  NUM_UNUSED_EMBERS
SPOTTING_STATS = SPOTTING_STATS_TEMP
! *****************************************************************************
END SUBROUTINE CLEAR_USED_EMBER
! *****************************************************************************

! *****************************************************************************
LOGICAL FUNCTION EMBER_IGNITION(IX,IY,T_ELMFIRE, DT_ELMFIRE, UWIND, P_IGN_INPUT, TAU_IGN_INPUT, T_DEVELOP_INPUT, HARDENING_FACTCOR)
! *****************************************************************************
! Firebrand ignition model, based on the ember accumulation history
USE ELMFIRE_VARS

REAL, INTENT(IN) :: T_ELMFIRE, DT_ELMFIRE, UWIND, P_IGN_INPUT, TAU_IGN_INPUT, T_DEVELOP_INPUT, HARDENING_FACTCOR
INTEGER, INTENT(IN) :: IX,IY

REAL :: NUM_ACCUMULATED_EMBERS_PUA, V_AIR, P_IGN, TAU_IGN, T_DEVELOP, &
        COEF_WIND, PSI, M_EMBER, IGNITION_CRITERION, P_N, R0, HFT, F
INTEGER :: IT_IGN_HI, IT_IGN_LO, IFBFM, IBLDGFM

EMBER_IGNITION = .FALSE.
P_IGN = P_IGN_INPUT*0.01
TAU_IGN = TAU_IGN_INPUT
T_DEVELOP = T_DEVELOP_INPUT

IF (.NOT. LOCAL_IGNITION(IX, IY)) THEN
   IT_IGN_HI = CEILING(T_ELMFIRE/DT_DUMP_EMBER_FLUX)
   IT_IGN_HI = MIN(IT_IGN_HI,EMBER_FLUX_TABLE_LEN)
   IT_IGN_HI = MAX(IT_IGN_HI,1)
   IT_IGN_LO = MAX(IT_IGN_HI-1, 1)

   NUM_ACCUMULATED_EMBERS_PUA = SUM(EMBER_FLUX%R4(IX,IY,1:IT_IGN_LO:1))
   NUM_ACCUMULATED_EMBERS_PUA = NUM_ACCUMULATED_EMBERS_PUA + EMBER_FLUX%R4(IX,IY,IT_IGN_LO) + &
                                 (EMBER_FLUX%R4(IX,IY,IT_IGN_HI) - EMBER_FLUX%R4(IX,IY,IT_IGN_LO))/DT_DUMP_EMBER_FLUX * &
                                 (T_ELMFIRE-IT_IGN_LO*DT_DUMP_EMBER_FLUX)
   NUM_ACCUMULATED_EMBERS_PUA = NUM_ACCUMULATED_EMBERS_PUA/ANALYSIS_CELLSIZE/ANALYSIS_CELLSIZE
   
   IF(.NOT. USE_SIMPLE_IGNITION_MODEL)THEN
      IFBFM = FBFM%I2(IX,IY,1)
              
      ! Ignition critical ember mass density from De Beer' Thesis, 2023
      M_EMBER = 0.2 ! Estimated ember mass, 0.2 g

      IF(IFBFM .NE. 91) THEN
         IF (CC%R4(IX,IY, 1) .GT. 1E-4 .AND. CH%R4(IX,IY, 1) .GT. 1E-4) THEN !Canopy is present
            HFT = CH%R4(IX,IY, 1) / 0.3048
            F = 0.3333 * CC%R4(IX,IY, 1) * CROWN_RATIO !Same as BEHAVE
         ELSE !Canopy is not present
            HFT = FUEL_MODEL_TABLE_2D(IFBFM,30)%DELTA
            F = 0.05
         ENDIF
         COEF_WIND = 0.555/SQRT(F*HFT)/LOG((HFT+20-0.64*HFT)/(0.13*HFT))
      ELSE
         F = 0.3 !30% volume filling percentage
         HFT = 25.23 ! ft, z_0=1 m=3.28 ft, HFT=z_0(ft)/0.13
         COEF_WIND = 0.555/SQRT(F*HFT)/LOG((HFT+20-0.64*HFT)/(0.13*HFT))
      ENDIF

      PSI = NUM_ACCUMULATED_EMBERS_PUA * M_EMBER/1E4 ! Firebrand coverage density (mass load, g/cm2) 
      V_AIR = UWIND*COEF_WIND*0.447
      IGNITION_CRITERION = HARDENING_FACTCOR*PSI*(V_AIR-0.003)*(V_AIR-4.017)+0.188 ! UMD Fitted curve at P_IGN = 0.5

      IF(IGNITION_CRITERION .LT. 0)THEN
         P_IGN = 0.90
      ELSE
         P_IGN = 0.0
      ENDIF
      ! End of De Beer's model

      IF (IFBFM .EQ. 91) THEN
         IF(USE_BLDG_SPREAD_MODEL .AND. BLDG_SPREAD_MODEL_TYPE .EQ. 2) THEN
            IBLDGFM = BLDG_FUEL_MODEL%I2(IX,IY,1)
            TAU_IGN = BUILDING_FUEL_MODEL_TABLE(IBLDGFM)%TAU_IGN ! Value derived from ThermaKin simulation for WRC.
            ! T_DEVELOP = 80.0 * BUILDING_FUEL_MODEL_TABLE(IBLDGFM)%T_EARLY / BUILDING_FUEL_MODEL_TABLE(IBLDGFM)%HRRPUA_PEAK ! 80 kW/m2 is an estimated HRRPUA at the ignition by firebrands
            T_DEVELOP = BUILDING_FUEL_MODEL_TABLE(IBLDGFM)%T_EARLY !
         ELSE
            TAU_IGN = 42.1 ! Value derived from ThermaKin simulation for WRC.
            T_DEVELOP = 300.0 ! Assume a medium fire growth rate for not defined structural fuels
         ENDIF
      ELSE
         TAU_IGN = 42.1 ! Value derived from ThermaKin simulation for WRC.
         T_DEVELOP = 75.0 ! Assume a ultrafast fire growth rate for non-structural fuels
      ENDIF

   ENDIF ! IF(.NOT. USE_SIMPLE_IGNITION_MODEL)THEN

   IF(NUM_ACCUMULATED_EMBERS_PUA .GT. 0) THEN
      P_N = 1.0-(1.0-P_IGN)**(DT_ELMFIRE/MAX(TAU_IGN, 1E-6)) ! Equivalent ignition probability at current time step
   ELSE
      P_N = 0.0
   ENDIF

   CALL RANDOM_NUMBER(R0)
   IF(R0 .LT. P_N) THEN
      T_LOCAL_IGNITION(IX, IY) = T_ELMFIRE+DT_ELMFIRE
      LOCAL_IGNITION(IX, IY) = .TRUE.
   ENDIF
ENDIF

IF(LOCAL_IGNITION(IX, IY) .AND. T_ELMFIRE+DT_ELMFIRE-T_LOCAL_IGNITION(IX,IY) .GE. T_DEVELOP) THEN
   EMBER_IGNITION = .TRUE.
ENDIF

! *****************************************************************************
END FUNCTION EMBER_IGNITION
! *****************************************************************************

! *****************************************************************************
END MODULE ELMFIRE_SPOTTING
! *****************************************************************************