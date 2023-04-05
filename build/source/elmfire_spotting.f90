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
                    N_SPOT_FIRES, IX_SPOT_FIRE, IY_SPOT_FIRE, ICASE, DT, TIME_NOW, TAU, IGNMULT)
! *****************************************************************************

IMPLICIT NONE

INTEGER, INTENT(IN) :: IX0, IY0, ICASE
INTEGER :: N_SPOT_FIRES, IX_SPOT_FIRE(:), IY_SPOT_FIRE(:)
REAL, INTENT(IN) :: WS20_NOW, FLIN, F_WIND, DT, TIME_NOW, TAU, IGNMULT
REAL, INTENT(IN), DIMENSION(:,:)  ::  WS20_LO, WS20_HI, WD20_LO, WD20_HI
REAL :: R0, X0(1:3), MSD, SIGMA_DIST, MU_DIST, NEMBERS_REAL, P, EMISSION_DT
REAL, PARAMETER :: TSTOP_SPOT= 1200.

X0(1) = (REAL(IX0)-0.5) * CC%CELLSIZE 
X0(2) = (REAL(IY0)-0.5) * CC%CELLSIZE 
X0(3) = DEM%R4(IX0,IY0,1) + MAX(CH%R4(IX0,IY0,1),2.0)

MSD        = MAX( MEAN_SPOTTING_DIST*(FLIN**SPOT_FLIN_EXP)*(WS20_NOW**SPOT_WS_EXP), 1.0)
MU_DIST    = LOG(MSD*MSD / SQRT(MSD * NORMALIZED_SPOTTING_DIST_VARIANCE + MSD*MSD))
SIGMA_DIST = SQRT(LOG(1. + MSD * NORMALIZED_SPOTTING_DIST_VARIANCE / (MSD*MSD)))

CONTINUE

IF (USE_UMD_SPOTTING_MODEL) THEN
   EMISSION_DT = MAX(MIN(DT, TAU_EMISSION - TAU),0.)
   NEMBERS_REAL = EMBER_GR * CC%CELLSIZE * CC%CELLSIZE * EMISSION_DT
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
IGNMULT )
! *****************************************************************************

INTEGER, INTENT(IN) :: NX_ELM, NY_ELM, NUM_EMBERS, IRANK_WORLD, ICASE
INTEGER :: N_SPOT_FIRES, IX_SPOT_FIRE(:), IY_SPOT_FIRE(:)
REAL, INTENT(IN) :: CELLSIZE_ELM, PIGN_ELM, MIN_SPOTTING_DISTANCE, MAX_SPOTTING_DISTANCE, &
                    SIGMA_DIST, MU_DIST, F_WIND, TIME_NOW, IGNMULT
REAL, INTENT(IN) :: PHIP(:,:), WS20_LO(:,:), WS20_HI(:,:), WD20_LO(:,:), WD20_HI(:,:)

CHARACTER(60), INTENT(IN) :: SPOTTING_DISTRIBUTION_TYPE
REAL :: SPOTTING_DISTANCE, DIST, EPS

!These also come from elmfire but have local analogs:
REAL, INTENT(IN) :: X0_ELM(:), TSTOP_ELM
CHARACTER(3) :: THREE
CHARACTER(400) :: FNOUT
REAL :: R0, IGNPROB, WD1TO, WD2TO, WDTO, WS20, WS20_0, T, TSTOP, DT
REAL, DIMENSION(3) :: X, X0, UWIND, UWIND0, OFFSET
INTEGER :: IEMBER, I, J, I1, I2, J1, J2, IX, IY, IXLAST, IYLAST, ICOL, IROW, ICOUNT
LOGICAL :: GO

X0   (:) = X0_ELM(:)               ! Initial position vector
TSTOP    = TSTOP_ELM               ! Stop time 

WRITE(THREE, '(I3.3)') IRANK_WORLD
FNOUT = 'ignitions_' // THREE // '.csv'

UWIND (3) = 0. 
OFFSET(3) = 0.

DO IEMBER = 1, NUM_EMBERS

   CALL RANDOM_NUMBER(R0); EPS = 8.*(R0 - 0.5)

! Get spotting distance
   CALL RANDOM_NUMBER(R0)
   IF (TRIM(SPOTTING_DISTRIBUTION_TYPE) .EQ. 'UNIFORM') THEN   
      SPOTTING_DISTANCE = MIN_SPOTTING_DISTANCE + R0 * (MAX_SPOTTING_DISTANCE - MIN_SPOTTING_DISTANCE)
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
         SPOTTING_STATS(NUM_TRACKED_EMBERS)%TTRAVEL = DIST / WS20
         SPOTTING_STATS(NUM_TRACKED_EMBERS)%TIGN    = TIME_NOW + DIST / WS20
      ENDIF

      IGNPROB=0.01*PIGN_ELM*IGNMULT

      CALL RANDOM_NUMBER(R0)

      IF (IGNPROB .GT. R0 .AND. DIST .GT. 1.5*CELLSIZE_ELM) THEN
         
         GO = .TRUE. 
         I1 = MAX(IX-3,1     )
         I2 = MIN(IX+3,NX_ELM)
         J1 = MAX(IY-3,1     )
         J2 = MIN(IY+3,NY_ELM)
            
         DO J = J1, J2
         DO I = I1, I2
            IF (PHIP(I,J) .LT. 0.) GO = .FALSE.
         ENDDO
         ENDDO

         IF (GO) THEN
            N_SPOT_FIRES = N_SPOT_FIRES + 1
            IX_SPOT_FIRE(N_SPOT_FIRES) = IX
            IY_SPOT_FIRE(N_SPOT_FIRES) = IY
            IF (USE_UMD_SPOTTING_MODEL) SPOTTING_STATS(NUM_TRACKED_EMBERS)%POSITIVE_IGNITION = .TRUE.
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
END SUBROUTINE EMBER_TRAJECTORY
! *****************************************************************************

! *****************************************************************************
END MODULE ELMFIRE_SPOTTING
! *****************************************************************************
