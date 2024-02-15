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
   SARDOY_PARAMETERS= SARDOY_PDF_PARAMETERS(WS20_NOW, FLIN)
   MU_DIST          = SARDOY_PARAMETERS(1)
   SIGMA_DIST       = SARDOY_PARAMETERS(2)
   MU_SPANWISE      = SARDOY_PARAMETERS(3)
   SIGMA_SPANWISE   = SARDOY_PARAMETERS(4)

   ! Calculate number of embers to emit
   IF (USE_PHYSICAL_EMBER_NUMBER) THEN
      NEMBERS_REAL = EMBER_TO_EMIT_PER_CELL(WS20_NOW, EMBER_SAMPLING_FACTOR, CC%CELLSIZE, &
                                 BLDG_FOOTPRINT_FRAC_LOCAL, FMC, WN_FUEL, IFBFM, TAU_EMBERGEN)
   ELSE
      EMBERGEN_DT = MAX(MIN(DT, TAU_EMBERGEN - TAU),0.)
      NEMBERS_REAL = EMBER_GR * CC%CELLSIZE * CC%CELLSIZE * EMBERGEN_DT
   ENDIF

   NEMBERS = FLOOR(NEMBERS_REAL)
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

! *****************************************************************************
FUNCTION EMBER_TO_EMIT_PER_CELL(WS, N0, CELLSIZE_ELM, AF, FMC, WN_FUEL, IFBFM, TAU_EMBERGEN)
! *****************************************************************************
! Calculates spotting distance distribution based on Sardoy's model.
! Takes as input local in speed and fireline intensity, reutrns MU and SIGMA
REAL, INTENT(IN) :: WS, N0, CELLSIZE_ELM, AF, FMC, WN_FUEL, TAU_EMBERGEN
INTEGER*2, INTENT(IN) :: IFBFM
REAL, PARAMETER :: D_TRUNK        = 0.2 ! Trunk diameter, m
REAL, PARAMETER :: M_FIREBRAND    = 2.0e-4 ! firebrand mass, kg
REAL, PARAMETER :: G       = 9.81! Gravitional acceleration, m^2/s
REAL :: M_FUEL, U_WIND, N_EMBER, Y_FIREBRAND
REAL :: EMBER_TO_EMIT_PER_CELL

M_FUEL = CELLSIZE_ELM*CELLSIZE_ELM*WN_FUEL ! Available vegetation fuel mass in a cell, kg
U_WIND = WS*0.447 ! wind speed, m/s (This is 20-ft wind, to be verified)

IF (IFBFM .EQ. 91) THEN
    ! Lee and Davidson, 2010, ember from structure
    N_EMBER = 206.66*EXP(0.1876*U_WIND)*(CELLSIZE_ELM*CELLSIZE_ELM*AF)
ELSE
    ! Ju et al, 2023, ember from vegetation
    Y_FIREBRAND = 1.70*MAX(FMC,1E-6)**(-0.14)*(U_WIND/SQRT(G*D_TRUNK))**0.63+0.15
    N_EMBER = Y_FIREBRAND*M_FUEL/M_FIREBRAND
ENDIF

EMBER_TO_EMIT_PER_CELL = N_EMBER/MAX(N0*TAU_EMBERGEN,1E-6)

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
REAL :: F_WIND, SPOTTING_DISTANCE, DIST, EPS, PDF_K, SPANWISE_DEVIATION, UWIND_ABS, INV_UWIND_TIMES_SPANWISE_DEVIATION 

!These also come from elmfire but have local analogs:
REAL, INTENT(IN) :: X0_ELM(:), TSTOP_ELM
CHARACTER(3) :: THREE
CHARACTER(400) :: FNOUT
REAL :: R0, IGNPROB, WD1TO, WD2TO, WDTO, WS20, WS20_0, T, TSTOP, DT, X_HIGH, X_LOW, HIGH, LOW, X_MAX
REAL, DIMENSION(3) :: X, X0, UWIND, UWIND0, OFFSET
REAL, POINTER, DIMENSION(:,:), SAVE :: WS20_LO, WS20_HI, WD20_LO, WD20_HI
INTEGER :: IEMBER, I, J, I1, I2, J1, J2, IX, IY, IXLAST, IYLAST, ICOL, IROW, ICOUNT, K_MAX, ITLO_METEOROLOGY, ITHI_METEOROLOGY
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
   K_MAX = 0;
   PDF_K = 1;
   DO WHILE(PDF_K .GT. P_EPS)
      K_MAX = K_MAX+1;
      X_HIGH = (K_MAX+0.5)*CELLSIZE_ELM
      X_LOW  = (K_MAX-0.5)*CELLSIZE_ELM
      PDF_K = 0.5 * (ERF((LOG(X_HIGH)-MU_DIST) / SQRT_2 / SIGMA_DIST) - &
                     ERF((LOG(X_LOW) -MU_DIST) / SQRT_2 / SIGMA_DIST))/CELLSIZE_ELM
   END DO
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

      IF (ICOUNT .EQ. 1) THEN
         UWIND0(1) = UWIND(1)
         UWIND0(2) = UWIND(2)
         UWIND0(3) = 0.
         WS20_0 = WS20
      ELSE
         UWIND(1) = UWIND0(1)
         UWIND(2) = UWIND0(2)
         WS20 = WS20_0
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
      IF (DUMP_EMBER_FLUX) EMBER_FLUX%R4(IX,IY,1) = EMBER_FLUX%R4(IX,IY,1) + 1.0
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

      IGNPROB=0.01*PIGN_ELM*IGNMULT

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
TIME_NOW)
! *****************************************************************************

INTEGER, INTENT(IN) :: NX_ELM, NY_ELM, IRANK_WORLD
REAL, INTENT(IN) :: CELLSIZE_ELM, SIGMA_DIST, MU_DIST, NUM_EMBERS 

REAL :: DIST, EPS, PDF_K

!These also come from elmfire but have local analogs:
REAL, INTENT(IN) :: X0_ELM(:), TSTOP_ELM, TIME_NOW
CHARACTER(3) :: THREE
CHARACTER(400) :: FNOUT
REAL :: R0, WD1TO, WD2TO, WDTO, WS20, WS20_0, T, TSTOP, DT, X_MAX, NORM_FACTOR, P_LAND, DIST_LAST, F_WIND!, XLAST_IGN, YLAST_IGN, IGNPROB, X_HIGH, X_LOW
REAL, DIMENSION(3) :: X, X0, UWIND, UWIND0, OFFSET
INTEGER :: IX, IY, IXLAST, IYLAST,ICOL, IROW, ICOUNT, K_MAX, IT_IGN, ITLO_METEOROLOGY, ITHI_METEOROLOGY !, IXLAST_IGN, IYLAST_IGN
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
   K_MAX = 0
   PDF_K = 1
   DO WHILE(PDF_K .GT. P_EPS)
      K_MAX = K_MAX+1;
      PDF_K = SARDOY_CDF(REAL(K_MAX)*CELLSIZE_ELM,(REAL(K_MAX)+1)*CELLSIZE_ELM, MU_DIST, SIGMA_DIST)/CELLSIZE_ELM
   END DO
   X_MAX = K_MAX*CELLSIZE_ELM
ENDIF

NORM_FACTOR = SARDOY_CDF(1E-6,REAL(K_MAX)*CELLSIZE_ELM, MU_DIST, SIGMA_DIST)

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
! XLAST_IGN = X(1)
! YLAST_IGN = X(2)

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

   IF (ICOUNT .EQ. 1) THEN
      UWIND0(1) = UWIND(1)
      UWIND0(2) = UWIND(2)
      UWIND0(3) = 0.
      WS20_0 = WS20
   ELSE
      UWIND(1) = UWIND0(1)
      UWIND(2) = UWIND0(2)
      WS20 = WS20_0
   ENDIF

   IF (ABS(UWIND(1)) .LT. 1E-6 .AND. ABS(UWIND(2)) .LT. 1E-6) T=9E9
   IF (ICOUNT .GT. 100000) T=9E9

   X(1:2)   = X(1:2) + UWIND(1:2) * DT
   DIST     = DIST + WS20 * DT
   
   IXLAST = IX
   IYLAST = IY

   ! Filling entries for ember flux table, which will be used as input for the ignition model
   IT_IGN = CEILING((T+TIME_NOW)/DT_DUMP_EMBER_FLUX)
   IT_IGN = MAX(IT_IGN,1)
   IT_IGN = MIN(IT_IGN,EMBER_FLUX_TABLE_LEN)
   P_LAND = SARDOY_CDF(DIST_LAST,DIST, MU_DIST, SIGMA_DIST)/MAX(NORM_FACTOR,1E-6)
   EMBER_FLUX%R4(IX,IY,IT_IGN) = EMBER_FLUX%R4(IX,IY,IT_IGN) + NUM_EMBERS*P_LAND
   ! This line is temporarly unused, wait until changing the datatype of EMBER_COUNT to float
   ! IF (USE_EMBER_COUNT_BINS) EMBER_COUNT(IX,IY) = EMBER_COUNT(IX,IY) + NUM_EMBERS*P_LAND

   ! If probability of ignition is 100%, EMBER_TIGN will be used, avoid allocating a big array of EMBER_ACCUMULATION_RATE
   IF(T+TIME_NOW .LT. EMBER_TIGN(IX,IY) .OR. EMBER_TIGN(IX,IY) .LT. 0.0) THEN
      EMBER_TIGN(IX,IY) = T+TIME_NOW
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
LOGICAL FUNCTION EMBER_IGNITION(IX,IY,T_ELMFIRE, UWIND, P_IGN_10CM)
! *****************************************************************************
! Firebrand ignition model, based on the ember accumulation history
USE ELMFIRE_VARS

REAL, INTENT(IN) :: T_ELMFIRE, UWIND, P_IGN_10CM  ! 10-cm wind-facing edge pile ignition probability, default 0.999
INTEGER, INTENT(IN) :: IX,IY

REAL :: NUM_ACCUMULATED_EMBERS_PUA, CRITICAL_SPOTTING_IGNITION_EMBER_DENSITY_LOCAL, V_AIR, S, MU, P_IGN_5CM, T_SUB, &
        T_AIR, L_LOCAL, K_FORCED, PR, NU, HOC_SUB, HOV_SUB, HF_T0, REDUCE_FACTOR, RE, NU_LOCAL, H_CONV, Q_CONV, &
        OMEGA_PEAK, HRR_SUB_PEAK, MLR_SUB_PEAK, HF_MAX_REDUCED, HF_MAX, T_RISE, PSI_CRIT, PSI_CRIT_KGM2, M_EMBER
INTEGER :: IT_IGN

EMBER_IGNITION = .FALSE.
IF(.NOT. USE_SIMPLE_IGNITION_MODEL)THEN
   ! Ignition critical ember mass density from De Beer' Thesis, 2023

   V_AIR = UWIND*0.447 ! Air velocity above the ember pile, Need correction
   V_AIR = MIN(V_AIR, 4.0) ! Ensure real value solution

   S = 1.0
   MU = 10.8

   T_SUB = 1573 ! SUBSTRATE IGNITION TEMPERATURE, K
   T_AIR = 293 ! AMBIENT AIR TEMPERATURE
   L_LOCAL = 0.125 ! LOCAL LENGTH SCALE FOR RE AND NU CALCULATION, FROM DE BEER'S EXPERIMENT CONFIGURATION
   K_FORCED = 64.26E-3 ! LOCAL AIR CONDUCTIVITY, EVALUATED AT FILM TEMPERATURE 933 K
   PR = 0.73      ! LOCAL AIR PR NUMBER, EVALUATED AT FILM TEMPERATURE 933 K
   NU = 105.25E-6 ! LOCAL AIR KINEMATIC VISCOSITY, EVALUATED AT FILM TEMPERATURE 933 K

   HOC_SUB = 11.87E6 ! SUBSTRATE HEAT OF COMBUSTION, J/KG
   HOV_SUB = 800E3 ! SUBSTRATE HEAT OF VAPORIZATION (PYROLYSIS), J/KG

   HF_T0 = 0 
   REDUCE_FACTOR = 0.8

   RE = V_AIR * L_LOCAL/NU 
   NU_LOCAL = 0.332*RE**0.5*PR**0.333
   H_CONV = NU_LOCAL*K_FORCED/L_LOCAL
   Q_CONV = H_CONV*(T_SUB-T_AIR)

   P_IGN_5CM = 1-(1-P_IGN_10CM)**0.227
   OMEGA_PEAK = S*(MU-LOG(1/P_IGN_5CM-1))

   HRR_SUB_PEAK = OMEGA_PEAK*Q_CONV ! W/M2
   MLR_SUB_PEAK = HRR_SUB_PEAK/HOC_SUB
   HF_MAX_REDUCED = MLR_SUB_PEAK*HOV_SUB+Q_CONV ! W/M2
   HF_MAX = HF_MAX_REDUCED / REDUCE_FACTOR /1000 ! KW/M2

   T_RISE = MAX(-0.58*V_AIR*V_AIR-0.50*V_AIR+43,0.0)!S
   PSI_CRIT = 0.0833*ATANH(MIN(MAX((HF_MAX-HF_T0)/T_RISE/(-0.29*V_AIR*V_AIR+1.49*V_AIR+0.1),0.0),1.0)) ! G/CM2
   PSI_CRIT_KGM2 = PSI_CRIT*10

   ! End of De Beer's model

   M_EMBER = 0.2E-3 ! Estimated ember mass, 200 mg
   CRITICAL_SPOTTING_IGNITION_EMBER_DENSITY_LOCAL = PSI_CRIT_KGM2 / M_EMBER
ELSE
   CRITICAL_SPOTTING_IGNITION_EMBER_DENSITY_LOCAL = CRITICAL_IGNITION_EMBER_NUMBER_LOAD

ENDIF
IT_IGN = CEILING(T_ELMFIRE/DT_DUMP_EMBER_FLUX)
IT_IGN = MAX(IT_IGN,1)
IT_IGN = MIN(IT_IGN,EMBER_FLUX_TABLE_LEN)
NUM_ACCUMULATED_EMBERS_PUA = SUM(EMBER_FLUX%R4(IX,IY,1:IT_IGN:1))/ANALYSIS_CELLSIZE/ANALYSIS_CELLSIZE

IF (NUM_ACCUMULATED_EMBERS_PUA .GT. CRITICAL_SPOTTING_IGNITION_EMBER_DENSITY_LOCAL) EMBER_IGNITION = .TRUE.

! *****************************************************************************
END FUNCTION EMBER_IGNITION
! *****************************************************************************

! *****************************************************************************
END MODULE ELMFIRE_SPOTTING
! *****************************************************************************