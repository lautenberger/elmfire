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
SUBROUTINE SPOTTING(IX0,IY0,WS20_NOW,FLIN,F_WIND,WS20_LO, WS20_HI, WD20_LO, WD20_HI, &
                    N_SPOT_FIRES, IX_SPOT_FIRE, IY_SPOT_FIRE, ICASE, DT, TIME_NOW, TAU, IGNMULT, &
                    FOOTPRINT_PERCENTAGE_LOCAL, FMC, IFBFM, WN_FUEL)
! *****************************************************************************

IMPLICIT NONE

INTEGER, INTENT(IN) :: IX0, IY0, ICASE
INTEGER*2, INTENT(IN) :: IFBFM
INTEGER :: N_SPOT_FIRES, IX_SPOT_FIRE(:), IY_SPOT_FIRE(:)
REAL, INTENT(IN) :: WS20_NOW, FLIN, F_WIND, DT, TIME_NOW, TAU, IGNMULT, &
                    FOOTPRINT_PERCENTAGE_LOCAL, FMC, WN_FUEL
REAL, INTENT(IN), DIMENSION(:,:)  ::  WS20_LO, WS20_HI, WD20_LO, WD20_HI
REAL :: R0, X0(1:3), MSD, SIGMA_DIST, MU_DIST, MU_SPANWISE, SIGMA_SPANWISE, &
         NEMBERS_REAL, P, EMISSION_DT, SARDOY_PARAMETERS(1:4)
REAL, PARAMETER :: TSTOP_SPOT= 1200.

X0(1) = (REAL(IX0)-0.5) * CC%CELLSIZE 
X0(2) = (REAL(IY0)-0.5) * CC%CELLSIZE 
X0(3) = DEM%R4(IX0,IY0,1) + MAX(CH%R4(IX0,IY0,1),2.0)

MSD        = MAX( MEAN_SPOTTING_DIST*(FLIN**SPOT_FLIN_EXP)*(WS20_NOW**SPOT_WS_EXP), 1.0)
MU_DIST    = LOG(MSD*MSD / SQRT(MSD * NORMALIZED_SPOTTING_DIST_VARIANCE + MSD*MSD))
SIGMA_DIST = SQRT(LOG(1. + MSD * NORMALIZED_SPOTTING_DIST_VARIANCE / (MSD*MSD)))

CONTINUE

IF (USE_UMD_SPOTTING_MODEL) THEN

   ! CALCULATE DISTRIBUTION PARAMETERS FROM LOCAL WIND SPEED & FIRELINE INTENSITY FROM SARDOY'S MODEL
   SARDOY_PARAMETERS= SARDOY_PDF_PARAMETERS(WS20_NOW, FLIN)
   MU_DIST          = SARDOY_PARAMETERS(1)
   SIGMA_DIST       = SARDOY_PARAMETERS(2)
   MU_SPANWISE      = SARDOY_PARAMETERS(3)
   SIGMA_SPANWISE   = SARDOY_PARAMETERS(4)

   ! Calculate number of embers to emit
   IF (USE_PHYSICAL_EMBER_NUMBER) THEN
      NEMBERS_REAL = EMBER_TO_EMIT_PER_CELL(WS20_NOW, N_PHYSIC_EMBER_PER_NUMER, CC%CELLSIZE, &
                                 FOOTPRINT_PERCENTAGE_LOCAL, FMC, WN_FUEL, IFBFM, TAU_EMISSION)
   ELSE
      EMISSION_DT = MAX(MIN(DT, TAU_EMISSION - TAU),0.)
      NEMBERS_REAL = EMBER_GR * CC%CELLSIZE * CC%CELLSIZE * EMISSION_DT
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

IF (USE_SPOTTING_LOOKUP_TABLE) THEN

   CALL FAST_SPOTTING(  &
      CC%NCOLS                  , &
      CC%NROWS                  , &
      CC%CELLSIZE               , &
      TIME_NOW                  , &
      X0                        , &
      MU_DIST                    , &
      SIGMA_DIST                 , &
      PHIP                       , &
      N_SPOT_FIRES               , &
      IX_SPOT_FIRE               , &
      IY_SPOT_FIRE)
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
      SIGMA_SPANWISE             , &
      MU_SPANWISE                , &
      SPOTTING_DISTRIBUTION_TYPE, &
      WS20_LO                   , &
      WS20_HI                   , &
      WD20_LO                   , &
      WD20_HI                   , &
      F_WIND                    , &
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
U_WIND = 0.447 * WS ! WIND SPEED IN M/S
I  = MAX(FI,1E-6) / 1000.0                                          ! FIRELINE INTENSITY IN MW/M
! WRITE(*,*) 'WIND_SPEED',U_WIND,'FLIN',I
LC = (I*1000.0 / (RHO_INF * C_PG * T_INF * SQRT(G))) ** 0.67  ! Character length scale
FR = U_WIND / SQRT(G * LC)                                ! FROUDE NUMBER

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
! MU_DIST    = 0.1
! SIGMA_DIST  = 0.1
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
FUNCTION EMBER_TO_EMIT_PER_CELL(WS, N0, CELLSIZE_ELM, AF, FMC, WN_FUEL, IFBFM, TAU_EMISSION)
! *****************************************************************************
! FUNCTION CALCULATES THE SPOTTING DISTANCE DISTRIBUTION BASED ON THE SARDOY'S MODEL
! TAKE THE INPUTS LOCAL WIND SPEED AND FIRELINE INTENSITY, RETURE MU AND SIGMA
REAL, INTENT(IN) :: WS, N0, CELLSIZE_ELM, AF, FMC, WN_FUEL, TAU_EMISSION
INTEGER*2, INTENT(IN) :: IFBFM
REAL, PARAMETER :: D_TRUNK        = 0.2 ! Trunk diameter, m
REAL, PARAMETER :: M_FIREBRAND    = 2.0e-4 ! firebrand mass, kg
REAL, PARAMETER :: G       = 9.81! Gravitional acceleration, m^2/s
REAL :: M_FUEL, U_WIND, N_EMBER, Y_FIREBRAND
REAL :: EMBER_TO_EMIT_PER_CELL

M_FUEL = CELLSIZE_ELM*CELLSIZE_ELM*WN_FUEL ! Available vegetation fuel mass in a cell, kg
U_WIND = WS*0.447 ! wind speed, m/s

IF (IFBFM .EQ. 91) THEN
    ! Lee and Davidson, 2010, ember from structure
    N_EMBER = 206.66*EXP(0.1876*U_WIND)*(CELLSIZE_ELM*CELLSIZE_ELM*AF)
ELSE
    ! Ju et al, 2023, ember from vegetation
    Y_FIREBRAND = 1.70*MAX(FMC,1E-6)**(-0.14)*(U_WIND/SQRT(G*D_TRUNK))**0.63+0.15
    N_EMBER = Y_FIREBRAND*M_FUEL/M_FIREBRAND
ENDIF

EMBER_TO_EMIT_PER_CELL = N_EMBER/MAX(N0*TAU_EMISSION,1E-6)

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
WS20_LO                    , &
WS20_HI                    , &
WD20_LO                    , &
WD20_HI                    , &
F_WIND                     , &
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
                    SIGMA_DIST, MU_DIST, SIGMA_SPANWISE, MU_SPANWISE, F_WIND, TIME_NOW, IGNMULT
REAL, INTENT(IN) :: PHIP(:,:), WS20_LO(:,:), WS20_HI(:,:), WD20_LO(:,:), WD20_HI(:,:)

CHARACTER(60), INTENT(IN) :: SPOTTING_DISTRIBUTION_TYPE
REAL :: SPOTTING_DISTANCE, DIST, EPS, PDF_K, SPANWISE_DEVIATION, UWIND_ABS, INV_UWIND_TIMES_SPANWISE_DEVIATION 

!These also come from elmfire but have local analogs:
REAL, INTENT(IN) :: X0_ELM(:), TSTOP_ELM
CHARACTER(3) :: THREE
CHARACTER(400) :: FNOUT
REAL :: R0, IGNPROB, WD1TO, WD2TO, WDTO, WS20, WS20_0, T, TSTOP, DT, X_HIGH, X_LOW, HIGH, LOW, X_MAX
REAL, DIMENSION(3) :: X, X0, UWIND, UWIND0, OFFSET
INTEGER :: IEMBER, I, J, I1, I2, J1, J2, IX, IY, IXLAST, IYLAST, ICOL, IROW, ICOUNT, K_MAX
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

   WS20 = WS20_LO(ICOL,IROW) * (1. - F_WIND) + F_WIND * WS20_HI(ICOL,IROW) 
   WS20 = 0.447 * WS20

   DT = MIN ( 0.5 * CELLSIZE_ELM / MAX (WS20, 0.01), 5.0)

   IF (USE_UMD_SPOTTING_MODEL) THEN
      NUM_TRACKED_EMBERS = NUM_TRACKED_EMBERS + 1
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

         WS20 = WS20_LO(ICOL,IROW) * (1. - F_WIND) + F_WIND * WS20_HI(ICOL,IROW) 
         WS20 = 0.447 * WS20

         DT = MIN ( 0.5 * CELLSIZE_ELM / MAX (WS20, 0.01), 5.0)

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
      IF (DUMP_EMBER_FLUX) EMBER_FLUX%I2(IX,IY,1) = EMBER_FLUX%I2(IX,IY,1) + 1
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

      ! IF (IGNPROB .GT. R0 .AND. DIST .GT. 1.5*CELLSIZE_ELM) THEN
      IF (IGNPROB .GT. R0 .AND. DIST .GT. 0.5*CELLSIZE_ELM) THEN ! SET TO 0.5*CELLSIZE TO ALLOW ADJCENT CELL IGNITION
         
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
REAL FUNCTION ERFINV(X)
! *****************************************************************************

REAL, INTENT(IN) :: X
REAL, PARAMETER :: HALFSQRTPI = 0.88622692545
REAL :: X2, X4, X6, X8
REAL, PARAMETER :: C1 = 1.000000000
REAL, PARAMETER :: C2 = 0.261799388
REAL, PARAMETER :: C3 = 0.143931731
REAL, PARAMETER :: C4 = 0.097663620
REAL, PARAMETER :: C5 = 0.073299079
REAL, PARAMETER :: C6 = 0.058372501

!ERFINV = 0.5*SQRT(PI)*(C1*X + C2*X**3 + C3*X**5 + C4*X**7 + C5*X**9 + C6*X**11) 

X2  = X  * X
X4  = X2 * X2
X6  = X4 * X2
X8  = X6 * X2
!X10 = X8 * X2

!ERFINV = HALFSQRTPI * X * (C1 + C2*X2 + C3*X4 + C4*X6 + C5*X8 + C6*X10) 

ERFINV = HALFSQRTPI * X * (C1 + C2*X2 + C3*X4 + C4*X6 + C5*X8) 

! *****************************************************************************
END FUNCTION ERFINV
! *****************************************************************************

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
SUBROUTINE BUILD_EMBER_TRAJECTORY_TABLE( &
NX_ELM                     , & 
NY_ELM                     , &
CELLSIZE_ELM               , &
DT_ELMFIRE                 , &
MAX_SPOTTING_DISTANCE      , & 
TSTOP_ELM)
! *****************************************************************************
! Function used to construct ember trajectory table, global variables used are:
! WSP, WDP, DT_METEOROLOGY, NUM_METEOROLOGY_TIMES

! Result array to be used for subroutine FAST_SPOTTING are:
! EMBER_TARGET_IX, EMBER_TARGET_IY, EMBER_TOA, TIME_LIST

INTEGER, INTENT(IN) :: NX_ELM, NY_ELM
REAL, INTENT(IN) :: TSTOP_ELM, CELLSIZE_ELM, MAX_SPOTTING_DISTANCE, DT_ELMFIRE
REAL, ALLOCATABLE :: WS20_LO(:,:), WS20_HI(:,:), WD20_LO(:,:), WD20_HI(:,:)

REAL :: SPOTTING_DISTANCE, DIST, EPS, R0

!These also come from elmfire but have local analogs:
REAL :: WD1TO, WD2TO, WDTO, WS20, WS20_0, T, TSTOP, DT, F_METEOROLOGY, T0
REAL, DIMENSION(3) :: X, X0, UWIND, UWIND0, OFFSET
! INTEGER, ALLOCATABLE, INTENT(OUT) :: EMBER_TARGET_IX(:,:,:,:),EMBER_TARGET_IY(:,:,:,:)
! REAL, ALLOCATABLE, INTENT(OUT) :: EMBER_TOA(:,:,:,:), TIME_LIST(:)
INTEGER :: DIM_ET, DIM_SP, I_DT, I_SP, IX0, IY0, IX, IY, IXLAST, IYLAST, ICOL, IROW, &
           ICOUNT, ITLO_METEOROLOGY, ITHI_METEOROLOGY

! Table struct:n_t x n_source x n_SpotDist
! t: Ember emission time
! SPOTTING DIST   DX  2DX  3DX  4DX  ...
! [[t=0:  SOURCE  [ 1    1    1    1  ...
!                   2    2    2    2  ... 
!                   3    3    3    3  
!                ...                    ]
!   t=DT: SOURCE  [ 1    1    1    1  ...
!                   2    2    2    2  ... 
!                   3    3    3    3  
!                ...                    ]
!   t=2DT: 
!   ...                                  
!   t=nDT: SOURCE  [ 1    1    1    1  ...
!                    2    2    2    2  ... 
!                    3    3    3    3  
!                ...                    ]]

WRITE(*,*) 'Updating Spotting table...'

TSTOP = TSTOP_ELM
DIM_ET  = CEILING(TSTOP/DT_ELMFIRE)
DIM_SP  = CEILING(MAX_SPOTTING_DISTANCE/CELLSIZE_ELM)

! These variable are declared in elmfire_vars.f90
ALLOCATE(EMBER_TOA(DIM_ET,NX_ELM,NY_ELM,DIM_SP))
ALLOCATE(EMBER_TARGET_IX(DIM_ET,NX_ELM,NY_ELM,DIM_SP))
ALLOCATE(EMBER_TARGET_IY(DIM_ET,NX_ELM,NY_ELM,DIM_SP))
ALLOCATE(TIME_LIST(DIM_ET))

DO I_DT=1,DIM_ET
   T0 = (REAL(I_DT)-1.0)*DT_ELMFIRE
   T = T0
   TIME_LIST(I_DT) = T0

   ! Determine current time in the wind raster
   IF (I_DT .EQ. 1 .OR. NUM_METEOROLOGY_TIMES .GT. 1) THEN
      ITLO_METEOROLOGY = MAX(1 + FLOOR(T / DT_METEOROLOGY),1)
      ITLO_METEOROLOGY = MIN(ITLO_METEOROLOGY, NUM_METEOROLOGY_TIMES)
      ITHI_METEOROLOGY = MIN(ITLO_METEOROLOGY + 1, NUM_METEOROLOGY_TIMES)
      F_METEOROLOGY = (T - REAL(ITLO_METEOROLOGY-1) * DT_METEOROLOGY) / DT_METEOROLOGY
      IF (ITLO_METEOROLOGY .EQ. ITHI_METEOROLOGY) F_METEOROLOGY = 1.

      WS20_LO = WSP(:,:,ITLO_METEOROLOGY)
      WS20_HI = WSP(:,:,ITHI_METEOROLOGY)
      WD20_LO = WDP(:,:,ITLO_METEOROLOGY)
      WD20_HI = WDP(:,:,ITHI_METEOROLOGY)
      
   ENDIF

   ! Emit ember with all possible spotting distance at all locations at the current time
   DO IX0 = 1,NX_ELM
      DO IY0 = 1,NY_ELM
         DO I_SP = 1,DIM_SP
            CALL RANDOM_NUMBER(R0); EPS = 8.*(R0 - 0.5)
            SPOTTING_DISTANCE = I_SP*CELLSIZE_ELM;
            T = T0
            X0(1) = (REAL(IX0)-0.5) * CC%CELLSIZE 
            X0(2) = (REAL(IY0)-0.5) * CC%CELLSIZE 
            X0(3) = DEM%R4(IX0,IY0,1) + MAX(CH%R4(IX0,IY0,1),2.0)

            DIST = 0.

            OFFSET(1:2) = X0(1:2)
            X   (:)   = X0(:) - OFFSET(:)
           
            IXLAST = 0
            IYLAST = 0

            IX = CEILING ((X(1) + OFFSET(1)) / CELLSIZE_ELM)
            IX = MAX(IX,1) ; IX = MIN (IX,NX_ELM)
            ICOL = ICOL_ANALYSIS_F2C(IX)

            IY = CEILING ((X(2) + OFFSET(2)) / CELLSIZE_ELM)
            IY = MAX(IY,1) ; IY = MIN (IY,NY_ELM)
            IROW = IROW_ANALYSIS_F2C(IY)

            WS20 = WS20_LO(ICOL,IROW) * (1. - F_METEOROLOGY) + F_METEOROLOGY * WS20_HI(ICOL,IROW) 
            WS20 = 0.447 * WS20

            ! DT = MIN ( 0.5 * CELLSIZE_ELM / MAX (WS20, 0.01), 5.0)
            DT = DT_ELMFIRE

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

                  WS20 = WS20_LO(ICOL,IROW) * (1. - F_METEOROLOGY) + F_METEOROLOGY * WS20_HI(ICOL,IROW) 
                  WS20 = 0.447 * WS20

                  ! DT = MIN ( 0.5 * CELLSIZE_ELM / MAX (WS20, 0.01), 5.0)
                  DT = DT_ELMFIRE

                  WD1TO = WD20_LO(ICOL,IROW) + 180. ; IF (WD1TO .GT. 360) WD1TO = WD1TO - 360.
                  WD2TO = WD20_HI(ICOL,IROW) + 180. ; IF (WD2TO .GT. 360) WD2TO = WD2TO - 360.

                  WDTO  = WD1TO + F_METEOROLOGY * (WD2TO - WD1TO)
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
               IX = CEILING ((X(1) + OFFSET(1)) / CELLSIZE_ELM) ; IX = MAX(IX,1) ; IX = MIN (IX,NX_ELM)
               IY = CEILING ((X(2) + OFFSET(2)) / CELLSIZE_ELM) ; IY = MAX(IY,1) ; IY = MIN (IY,NY_ELM)
               EMBER_TARGET_IX(I_DT,IX0,IY0,I_SP) = IX
               EMBER_TARGET_IY(I_DT,IX0,IY0,I_SP) = IY
               EMBER_TOA(I_DT,IX0,IY0,I_SP) = T
            ENDIF
         ENDDO 
      ENDDO
   ENDDO
ENDDO

! Write the arrays to binary files for fast future rendering
! OPEN(UNIT=10, FILE='EMBER_TARGET_IX.bin', FORM='BINARY')
! WRITE(10) EMBER_TARGET_IX
! CLOSE(10)

! OPEN(UNIT=10, FILE='EMBER_TARGET_IY.bin', FORM='BINARY')
! WRITE(10) EMBER_TARGET_IY
! CLOSE(10)

! OPEN(UNIT=10, FILE='EMBER_TOA.bin', FORM='BINARY')
! WRITE(10) EMBER_TOA
! CLOSE(10)

! OPEN(UNIT=10, FILE='TIME_LIST.bin', FORM='BINARY')
! WRITE(10) TIME_LIST
! CLOSE(10)

WRITE(*,*) 'Spotting Table updated!'

! *****************************************************************************
END SUBROUTINE BUILD_EMBER_TRAJECTORY_TABLE
! *****************************************************************************

! *****************************************************************************
SUBROUTINE FAST_SPOTTING(  NX_ELM                     , &
                           NY_ELM                     , &
                           CELLSIZE_ELM               , &
                           T_ELMFIRE                  , &
                           X0_ELM                     , &
                           MU_DIST                    , &
                           SIGMA_DIST                 , &
                           PHIP                       , &
                           N_SPOT_FIRES               , &
                           IX_SPOT_FIRE               , &
                           IY_SPOT_FIRE)
! *****************************************************************************

! Variables: TIME_LIST_AVAIL, EMBER_TARGET_IX, EMBER_TARGET_IY, EMBER_TOA are look-up tables, 
!            and should be globally accessible
! Variable: TIME_TO_IGNITE is an array to memory and update the ignition time, also globally accessible
! Variable: P_EPS is an arbitrary small number controlling the maximum spotting distance. Global. 

INTEGER, INTENT(INOUT) :: N_SPOT_FIRES, IX_SPOT_FIRE(:), IY_SPOT_FIRE(:)
INTEGER, INTENT(IN) :: NX_ELM, NY_ELM
REAL, INTENT(IN) :: PHIP(:,:), X0_ELM(3), MU_DIST, SIGMA_DIST, CELLSIZE_ELM, T_ELMFIRE

INTEGER :: I, IX, IY, N_DX_AVAIL, K_MAX, RECORD_INDEX, UNIGNITED_CELLS_AVAIL_LENGTH
REAL :: PDF_K, X_HIGH, X_LOW, X0(3), OFFSET(3)

INTEGER, ALLOCATABLE :: I_TIME(:), EMBER_TARGET_IX_LOCAL(:), EMBER_TARGET_IY_LOCAL(:), SPOTTING_DX(:), &
                        IX_TARGET(:), IY_TARGET(:), UNIGNITED_CELLS_AVAIL(:), SPOTTING_DX_IGNITED_REMOVED(:)
REAL, ALLOCATABLE :: EMBER_TOA_LOCAL(:), T_EMBER(:), PHIP_LOC(:), TIME_DIFF(:)
REAL, PARAMETER :: SQRT_2 = 1.4142135623731

! Definition of X_max - PDF(k_max)<1e-3
K_MAX = 0
PDF_K = 1
DO WHILE(PDF_K .GT. P_EPS)
   K_MAX = K_MAX+1;
   X_HIGH = (K_MAX+0.5)*CELLSIZE_ELM
   X_LOW  = (K_MAX-0.5)*CELLSIZE_ELM
   PDF_K = 0.5 * (ERF((LOG(X_HIGH)-MU_DIST) / SQRT_2 / SIGMA_DIST) - &
                  ERF((LOG(X_LOW) -MU_DIST) / SQRT_2 / SIGMA_DIST))/CELLSIZE_ELM
END DO

! IF(X_MAX .GT. SIZE(EMBER_TARGET,3)*delX .OR. T_ELMFIRE .GT. MAX(TIME_LIST_AVAIL))
!     ! Add entris to search table
!     [EMBER_TOA, EMBER_TARGET, TIME_LIST_AVAIL] = EMBER_TOA_TABLE(U_wind, WIND_TIME_LIST, SIMU_MESH, DT_ELMFIRE, delX, X_MAX, T_ELMFIRE+10000);
! ENDIF
X0 = X0_ELM;

! Search pertinent table
ALLOCATE(TIME_DIFF(SIZE(TIME_LIST)))
TIME_DIFF = ABS(T_ELMFIRE-TIME_LIST);
I_TIME = MINLOC(TIME_DIFF);

IX = CEILING ((X0(1) + OFFSET(1)) / CELLSIZE_ELM)
IX = MAX(IX,1) ; IX = MIN (IX,NX_ELM)

IY = CEILING ((X0(2) + OFFSET(2)) / CELLSIZE_ELM)
IY = MAX(IY,1) ; IY = MIN (IY,NY_ELM)

N_DX_AVAIL=SIZE(EMBER_TARGET_IX,4)
ALLOCATE(EMBER_TARGET_IX_LOCAL(N_DX_AVAIL))
ALLOCATE(EMBER_TARGET_IY_LOCAL(N_DX_AVAIL))
ALLOCATE(EMBER_TOA_LOCAL(N_DX_AVAIL))

EMBER_TARGET_IX_LOCAL = RESHAPE(EMBER_TARGET_IX(I_TIME, IX, IY, :),[N_DX_AVAIL])
EMBER_TARGET_IY_LOCAL = RESHAPE(EMBER_TARGET_IY(I_TIME, IX, IY, :),[N_DX_AVAIL])
EMBER_TOA_LOCAL       = RESHAPE(EMBER_TOA(I_TIME, IX, IY, :),[N_DX_AVAIL])

! Ignite all possible pixels
ALLOCATE(SPOTTING_DX(K_MAX))
SPOTTING_DX = (/(i, i=1, K_MAX)/)

! Apply ignition probability
! IF(IGNITION_PROBABILITY) THEN
!    R0=rand(1,NUM_EMBERS_PER_TORCH_ELM);
!    Fx=@(x)1/2*(1+erf((log(x)-MU_DIST)/sqrt(2)/SIGMA_DIST));
!    Low = Fx(delX/2);High=Fx(X_MAX);
!    R0 = R0*(High-Low)+Low;
!    SPOTTING_DISTANCE = exp(sqrt(2.) * SIGMA_DIST * erfinv(2.*R0-1.) + MU_DIST);

!    IGNPROB=0.01*PIGN;
!    R0_IGN = rand(1,NUM_EMBERS_PER_TORCH_ELM);
!    SPOTTING_DX = SPOTTING_DX(R0_IGN < IGNPROB);
! ENDIF

! Record all generated embers

ALLOCATE(IX_TARGET(K_MAX))
ALLOCATE(IY_TARGET(K_MAX))
ALLOCATE(T_EMBER(K_MAX))
IX_TARGET = EMBER_TARGET_IX_LOCAL(SPOTTING_DX)
IY_TARGET = EMBER_TARGET_IY_LOCAL(SPOTTING_DX)
T_EMBER   = EMBER_TOA_LOCAL(SPOTTING_DX)

! OUTPUT DIAGNOSETIC
! RES_DIST_ALL=sprintf('/Dist_all_%03d.bin',I_SIMU);
! if(~exist([RES_DIR,RES_DIST_ALL],'file'))
!     FileID_All=fopen([RES_DIR,RES_DIST_ALL],'w');
! else
!     FileID_All=fopen([RES_DIR,RES_DIST_ALL],'a');
! end
! !         fwrite(FileID_All,[T_ELMFIRE,T_ELMFIRE+ceil(T_ember/DT_ELMFIRE)*DT_ELMFIRE,X0_ELM,(max(1,(IX-2))-0.5) * delX],'double');
! DATA_TO_WRITE = [zeros(length(IX_target),1)+T_ELMFIRE,...
!                 T_EMBER,...
!                 zeros(length(IX_target),1)+i_loc+2,...
!                 IX_target];
! DATA_TO_WRITE = reshape(DATA_TO_WRITE,1,[]);
! fwrite(FileID_All,DATA_TO_WRITE,'double');
! fclose(FileID_All);

! Remove ignited targets
ALLOCATE(PHIP_LOC(K_MAX))
DO I=1,K_MAX
   PHIP_LOC(I) = PHIP(IX_TARGET(I),IY_TARGET(I))
ENDDO
UNIGNITED_CELLS_AVAIL=PACK(SPOTTING_DX, PHIP_LOC>=0)
UNIGNITED_CELLS_AVAIL_LENGTH = SIZE(UNIGNITED_CELLS_AVAIL)

IF (UNIGNITED_CELLS_AVAIL_LENGTH<1) THEN
   RETURN
ELSE
   ALLOCATE(SPOTTING_DX_IGNITED_REMOVED(UNIGNITED_CELLS_AVAIL_LENGTH))
   SPOTTING_DX_IGNITED_REMOVED = SPOTTING_DX(UNIGNITED_CELLS_AVAIL)

   ! Get toa and target location
   DEALLOCATE(IX_TARGET)
   DEALLOCATE(IY_TARGET)
   DEALLOCATE(T_EMBER)

   ALLOCATE(IX_TARGET(UNIGNITED_CELLS_AVAIL_LENGTH))
   ALLOCATE(IY_TARGET(UNIGNITED_CELLS_AVAIL_LENGTH))
   ALLOCATE(T_EMBER(UNIGNITED_CELLS_AVAIL_LENGTH))
   IX_TARGET = EMBER_TARGET_IX_LOCAL(SPOTTING_DX_IGNITED_REMOVED)
   IY_TARGET = EMBER_TARGET_IY_LOCAL(SPOTTING_DX_IGNITED_REMOVED)
   T_EMBER   = EMBER_TOA_LOCAL(SPOTTING_DX_IGNITED_REMOVED)

   ! Update ignition time on simulation map
   RECORD_INDEX = N_SPOT_FIRES + 1
   DO I=1,UNIGNITED_CELLS_AVAIL_LENGTH
      IX_SPOT_FIRE(RECORD_INDEX) = IX_TARGET(I)
      IY_SPOT_FIRE(RECORD_INDEX) = IY_TARGET(I)
      IF(TIME_TO_IGNITE(IX_TARGET(I),IY_TARGET(I)) .LT. 0) THEN
         TIME_TO_IGNITE(IX_TARGET(I),IY_TARGET(I)) = T_EMBER(I)
      ELSE
         TIME_TO_IGNITE(IX_TARGET(I),IY_TARGET(I)) = MIN(T_EMBER(I), TIME_TO_IGNITE(IX_TARGET(I),IY_TARGET(I)));
      ENDIF
      RECORD_INDEX = RECORD_INDEX + 1
   ENDDO
ENDIF

! *****************************************************************************
END SUBROUTINE FAST_SPOTTING
! *****************************************************************************

! *****************************************************************************
SUBROUTINE STRUCTURE_DESIGN_FIRE_CURVE(C, STRUCTURE_AREA, T_ELMFIRE)
! *****************************************************************************
! This is to calculate the fireline intensity of a structural pixel since its iginition
! The fireline intensity will be used to determine the ember emitting duration
USE ELMFIRE_VARS

TYPE(NODE), POINTER, INTENT(INOUT) :: C

REAL, INTENT(IN) :: STRUCTURE_AREA, T_ELMFIRE

REAL, PARAMETER :: T_ALPHA = 300.0, Q_F = 1000000.0

REAL :: TIME_SINCE_IGNITION, T_MAX, T_DECAY, T_TOTAL, HRR_MAX, HRR, &
        T_EMISSION_START, T_EMISSION_END

TIME_SINCE_IGNITION = T_ELMFIRE-C%TIME_OF_ARRIVAL

! Use Q_dot = Q_dot0*t^2 for growth
HRR_MAX = 400.0*STRUCTURE_AREA ! kW
T_MAX   = SQRT(HRR_MAX*T_ALPHA**2.0/1000.0)
T_DECAY = T_MAX+(0.7*Q_F*STRUCTURE_AREA - 1000.0/3.0/T_ALPHA/T_ALPHA*T_MAX**3.0)/HRR_MAX
T_TOTAL = T_DECAY+2*0.3*Q_F*STRUCTURE_AREA/HRR_MAX

T_EMISSION_START = SQRT(CRITICAL_SPOTTING_FIRELINE_INTENSITY(FBFM%I2(C%IX,C%IY,1))/1000.0)*T_ALPHA
T_EMISSION_START = MIN(T_EMISSION_START,T_MAX)
T_EMISSION_START = MAX(T_EMISSION_START,0.0)

T_EMISSION_END = T_TOTAL-CRITICAL_SPOTTING_FIRELINE_INTENSITY(FBFM%I2(C%IX,C%IY,1))/HRR_MAX*(T_TOTAL-T_DECAY);
T_EMISSION_END = MAX(T_EMISSION_END,T_DECAY)
T_EMISSION_END = MIN(T_EMISSION_END,T_TOTAL)

C%LOCAL_EMISSION_DURATION   = T_EMISSION_END-T_EMISSION_START

IF (C%TIME_OF_ARRIVAL .LT. 0.0) THEN
    C%FLIN_SURFACE = 0.0
ELSE
    IF (TIME_SINCE_IGNITION .LT. T_MAX .AND. TIME_SINCE_IGNITION .GE. 0.0) THEN
        HRR = 1000.0*(TIME_SINCE_IGNITION/T_ALPHA)**2.0
        C%FLIN_SURFACE = HRR/CC%CELLSIZE
    ELSEIF (TIME_SINCE_IGNITION .GE. T_MAX .AND. TIME_SINCE_IGNITION .LT. T_DECAY) THEN
        HRR = HRR_MAX
        C%FLIN_SURFACE = HRR/CC%CELLSIZE
    ELSE
        HRR = HRR_MAX*(T_TOTAL-TIME_SINCE_IGNITION)/(T_TOTAL-T_DECAY)
        IF (HRR .LE. 0) HRR = 0.0
        C%FLIN_SURFACE = HRR/CC%CELLSIZE
    ENDIF
ENDIF

! *****************************************************************************
END SUBROUTINE STRUCTURE_DESIGN_FIRE_CURVE
! *****************************************************************************

! *****************************************************************************
SUBROUTINE CLEAR_USED_EMBER(T_ELMFIRE)
! *****************************************************************************
! Subroutine to delete used particles, save space
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
END MODULE ELMFIRE_SPOTTING
! *****************************************************************************
