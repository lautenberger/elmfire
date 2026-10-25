! *****************************************************************************
MODULE ELMFIRE_SPREAD_RATE
! *****************************************************************************

USE ELMFIRE_VARS
USE ELMFIRE_SUBS

IMPLICIT NONE

CONTAINS

! *****************************************************************************
RECURSIVE SUBROUTINE ROTHERMEL_SURFACE_SPREAD_RATE(L,DUMMY_NODE)
! *****************************************************************************
! Applies Rothermel suface fire spread model to calculate surface fire rate
! of spread, heat per unit area, fireline intensity, flame length, and 
! reaction intensity

TYPE (DLL), INTENT(INOUT) :: L
TYPE (NODE), POINTER, INTENT(INOUT) :: DUMMY_NODE
!Local variables:
INTEGER :: I, ILH, NUM_NODES, IX, IY
REAL :: WS_LIMIT, WSMF_LIMITED, PHIS_MAX, MOMEX2, MOMEX3, MEX_LIVE, M_DEAD, M_LIVE,ETAM_DEAD, ETAM_LIVE, &
        RHOBEPSQIG_DEAD, RHOBEPSQIG_LIVE, RHOBEPSQIG, IR_DEAD, IR_LIVE, MOMEX, SUM_MPRIMENUMER
REAL, DIMENSION(1:6) :: M, QIG, FEPSQIG, FMC, FMEX, MPRIMENUMER
TYPE (FUEL_MODEL_TABLE_TYPE) :: FMT
TYPE(NODE), POINTER :: C

IF (ASSOCIATED (DUMMY_NODE) ) THEN
   NUM_NODES = 1
   C => DUMMY_NODE
ELSE
   NUM_NODES = L%NUM_NODES
   C => L%HEAD
ENDIF

DO I = 1, NUM_NODES
   IX = C%IX
   IY = C%IY
   IF (USE_BLDG_SPREAD_MODEL .AND. C%IFBFM .EQ. 91) THEN
      C => C%NEXT
      CYCLE
   ENDIF

   IF ( ISNONBURNABLE(C%IX,C%IY) ) THEN
      C%IR = 0.
      C%PHIW_SURFACE = 0.
      C%PHIS_SURFACE = 0.
      C%VS0 = 0.
      C%VELOCITY_DMS_SURFACE = 0.
      C%HPUA_SURFACE = 0.
      C%FLIN_DMS_SURFACE = 0.
      C => C%NEXT
      CYCLE
   ENDIF

   M(1)  = C%M1
   M(2)  = C%M10
   M(3)  = C%M100
   M(4)  = C%M1 !Set dynamic dead to m1
   M(5)  = C%MLH
   M(6)  = C%MLW

   ! if (IRANK_HOST .gt. 0) print *, "M1 =", M(1), ", M2 =", M(2),", M3 =", M(3),", M4 =", M(4),", M5 =", M(5),", M6 =", M(6)
   
   ILH = MAX(MIN(NINT(100.*M(5)),120),30)
   FMT=FUEL_MODEL_TABLE_2D(C%IFBFM,ILH)
!Calculate live fuel moisture of extinction:
   MPRIMENUMER(1:4) = FMT%WPRIMENUMER(1:4) * M(1:4)
   SUM_MPRIMENUMER=SUM(MPRIMENUMER(1:4))
   MEX_LIVE = FMT%MEX_LIVE * (1. - FMT%R_MPRIMEDENOME14SUM_MEX_DEAD * SUM_MPRIMENUMER ) - 0.226
   MEX_LIVE = MAX(MEX_LIVE, FMT%MEX_DEAD)
   FMEX(5:6) = FMT%F(5:6) * MEX_LIVE

   FMEX(1:4) = FMT%FMEX(1:4)

   FMC(:) = FMT%F(:) * M(:)

   QIG(:) = 250. + 1116.*M(:)

   FEPSQIG(:) = FMT%FEPS(:) * QIG(:)

   RHOBEPSQIG_DEAD = FMT%RHOB * SUM(FEPSQIG(1:4))
   RHOBEPSQIG_LIVE = FMT%RHOB * SUM(FEPSQIG(5:6))
   RHOBEPSQIG = FMT%F_DEAD * RHOBEPSQIG_DEAD + FMT%F_LIVE * RHOBEPSQIG_LIVE

   M_DEAD    = SUM(FMC(1:4))
   MOMEX     = M_DEAD / FMT%MEX_DEAD
   MOMEX2    = MOMEX * MOMEX
   MOMEX3    = MOMEX2 * MOMEX
   ETAM_DEAD = 1.0 - 2.59*MOMEX + 5.11*MOMEX2 - 3.52*MOMEX3
   ETAM_DEAD = MAX(0.,MIN(ETAM_DEAD,1.))
   IR_DEAD   = FMT%GP_WND_EMD_ES_HOC * ETAM_DEAD

   M_LIVE    = SUM(FMC(5:6))
   MOMEX     = M_LIVE / MEX_LIVE
   MOMEX2    = MOMEX * MOMEX
   MOMEX3    = MOMEX2 * MOMEX
   ETAM_LIVE = 1.0 - 2.59*MOMEX + 5.11*MOMEX2 - 3.52*MOMEX3
   ETAM_LIVE = MAX(0.,MIN(ETAM_LIVE,1.))
   IR_LIVE   = FMT%GP_WNL_EML_ES_HOC * ETAM_LIVE

   C%IR = IR_DEAD + IR_LIVE !Btu/(ft^2-min)

!   WS_LIMIT = 96.8*C%IR**0.3333333 !Andrews, Cruz, and Rothermel (2013) limit
   WS_LIMIT = 0.9*C%IR !Original limit
   WSMF_LIMITED = MIN(C%WSMF, WS_LIMIT)
   C%PHIW_SURFACE = FMT%PHIWTERM * WSMF_LIMITED**FMT%B_COEFF

! Max slope factor is equal to max wind factor:
   PHIS_MAX = FMT%PHIWTERM * WS_LIMIT**FMT%B_COEFF
   C%PHIS_SURFACE = MIN(FMT%PHISTERM * C%TANSLP2, PHIS_MAX)

#ifdef _SUPPRESSION
   ! new suppression model :: modified below
   IF (ENABLE_EXTENDED_ATTACK) THEN
      IF (EXTENDED_ATTACK_MODEL .EQ. 0) THEN
         C%VS0 = (C%ADJ + PERTURB_ADJ) * C%SUPPRESSION_ADJUSTMENT_FACTOR * DIURNAL_ADJUSTMENT_FACTOR * C%IR * FMT%XI / RHOBEPSQIG !ft/min
      ELSE IF (EXTENDED_ATTACK_MODEL .EQ. 1) THEN
         C%VS0 = (C%ADJ + PERTURB_ADJ) * C%SUPPRESSION_ADJUSTMENT_FACTOR * DIURNAL_ADJUSTMENT_FACTOR * C%IR * FMT%XI / RHOBEPSQIG !ft/min
      ELSE
         WRITE(*,*) 'Error: "EXTENDED_ATTACK_MODEL" should be 0 or 1 in namelist!'
         STOP
      ENDIF
   ENDIF
   ! new suppression model
#endif
   IF (.NOT. ENABLE_EXTENDED_ATTACK) C%VS0 = (C%ADJ + PERTURB_ADJ) * DIURNAL_ADJUSTMENT_FACTOR * C%IR * FMT%XI / RHOBEPSQIG !ft/min

   C%VELOCITY_DMS_SURFACE = C%VS0 * (1.0 + C%PHIS_SURFACE + C%PHIW_SURFACE) !ft/min

! Convert reaction intensity to SI:
   C%IR           = C%IR * BTUPFT2MIN_TO_KWPM2 ! kW/m2
   C%HPUA_SURFACE = C%IR * FMT%TR * 60. ! kJ/m2
   C%FLIN_DMS_SURFACE = FMT%TR * C%IR * C%VELOCITY_DMS_SURFACE * 0.3048 ! kW/m
   
   C => C%NEXT
ENDDO

! *****************************************************************************
END SUBROUTINE ROTHERMEL_SURFACE_SPREAD_RATE
! *****************************************************************************

! *****************************************************************************
SUBROUTINE CFFDRS_SPREAD_RATE(L,DUMMY_NODE,BUI_FALLBACK)
! *****************************************************************************
! Applies the Canadian Forest Fire Behavior Prediction (CFFDRS/FBP) model to
! compute surface rate of spread, fireline intensity, ISI, surface fuel
! consumption, and length-to-width for each node in L (or a single DUMMY_NODE).
! BUI is the daily Buildup Index used in the buildup-effect term.

TYPE (DLL), INTENT(INOUT) :: L
TYPE (NODE), POINTER, INTENT(INOUT) :: DUMMY_NODE
REAL, OPTIONAL, INTENT(IN) :: BUI_FALLBACK
REAL :: BUI_LOCAL

INTEGER :: NUM_NODES, I, IX, IY
TYPE(NODE), POINTER :: C
REAL :: M, FF, SF, RSF, ISF_c, WSE, WSE1, WSE2, WSX, WSY, &
         FW, RSI_c, BE, ROS, FFMC, CF, slope, aspect, RSF_1, RSF_2, ISI_s

IF (ASSOCIATED (DUMMY_NODE) ) THEN
   NUM_NODES = 1
   C => DUMMY_NODE
ELSE
   NUM_NODES = L%NUM_NODES
   C => L%HEAD
ENDIF

DO I = 1, NUM_NODES
   IX = C%IX
   IY = C%IY

! Reset crown-fire quantities so the current evaluation cannot inherit
! crown state from a previous weather/time-step evaluation.
   C%CROWN_FIRE = 0
   C%CFB = 0.0
   C%CFC = 0.0
   C%TFC = 0.0
   C%RSI_FBP = 0.0
   C%BE_FBP = 0.0
   C%RSO_FBP = 0.0
   C%FLIN_CANOPY = 0.0
   C%PHIW_CROWN = 0.0
   C%VELOCITY_DMS = 0.0

   ! Building fuel is handled by the building-spread model.
   IF (USE_BLDG_SPREAD_MODEL .AND. C%IFBFM .EQ. 101) THEN
      C => C%NEXT
      CYCLE
   ENDIF

   ! CFFDRS nonburnable fuel models.
   IF (C%IFBFM .GE. 100 .AND. C%IFBFM .LE. 106) THEN
      C%IR = 0.0
      C%PHIW_SURFACE = 0.0
      C%PHIS_SURFACE = 0.0
      C%VS0 = 0.0
      C%VELOCITY_DMS_SURFACE = 0.0
      C%HPUA_SURFACE = 0.0
      C%FLIN_DMS_SURFACE = 0.0
      C => C%NEXT
      CYCLE
   ENDIF

   ! Select spatial BUI raster or legacy scalar daily BUI.
   IF (USE_BUI_RASTER) THEN
      BUI_LOCAL = MAX(C%BUI,0.0)
   ELSE
      IF (.NOT. PRESENT(BUI_FALLBACK)) THEN
         WRITE(*,*) "CFFDRS error: no BUI input available"
         STOP
      ENDIF
      BUI_LOCAL = MAX(BUI_FALLBACK,0.0)
   ENDIF
   C%BUI = BUI_LOCAL

   ! Degree of grass curing, percent.  Prefer the explicit CWFIS/FBP GCF raster;
   ! retain the former MLH-derived relationship only as a compatibility fallback for older inputs.
   IF (USE_GCF_RASTER) THEN
      C%C = MAX(0.0,MIN(100.0,C%GCF))
   ELSE
      C%C = 100.0 * MIN(1.0,MAX(0.0,1.33 - 1.11*C%MLH))
   ENDIF
   ! Keep the diagnostic node field equal to the curing percent actually used.
   C%GCF = C%C

   ! Mixedwood fractions encoded in the ELMFIRE FBP fuel-model number.
   C%PC = MOD(C%IFBFM,100) / 100.0
   C%PDF = C%PC

   ! Fine-fuel moisture content expected by the FBP equations, percent.
   M = C%M1 * 100.0

   ! Terrain.
   slope = TAN(SLP%R4(IX,IY,1) * PIO180) * 100.0
   aspect = MODULO(ASP%R4(IX,IY,1),360.0)

   ! --------------------------------------------------------------------------
   ! INITIAL SPREAD INDEX
   ! --------------------------------------------------------------------------

   FF = 91.9 * EXP(-0.1386*M) * (1.0 + (M**5.31)/(4.93E7))

   ! IF (slope .LE. 63.0) THEN
   IF (slope .LT. 70.0) THEN
      SF = EXP(3.533 * (slope/100.0)**1.2)
   ELSE
      SF = 10.0
   ENDIF

   ! Grass curing factor.
   IF (C%IFBFM .GE. 31 .AND. C%IFBFM .LE. 33) THEN
      IF (C%C .LT. 58.8) THEN
         CF = 0.005 * (EXP(0.061*C%C) - 1.0)
      ELSE
         CF = 0.176 + 0.02 * (C%C - 58.8)
      ENDIF
   ELSE
      CF = 1.0
   ENDIF

   ! Slope-only spread rate used to obtain equivalent slope ISI/wind.
   RSF = RSI(C%IFBFM,0.208*FF,CF) * SF

   IF (C%IFBFM .EQ. 40 .OR. C%IFBFM .EQ. 50 .OR. C%IFBFM .EQ. 60 .OR. &
      (C%IFBFM .GE. 400 .AND. C%IFBFM .LE. 699)) THEN
      ! M-1 / M-2
      RSF_1 = RSI(2_2,0.208*FF,CF) * SF
      RSF_2 = RSI(11_2,0.208*FF,CF) * SF
      ISF_c = C%PC*ISF(2_2,RSF_1,CF) + &
              (1.0-C%PC)*ISF(11_2,RSF_2,CF)

   ELSE IF (C%IFBFM .EQ. 70 .OR. C%IFBFM .EQ. 90 .OR. &
            (C%IFBFM .GE. 700 .AND. C%IFBFM .LE. 799) .OR. &
            (C%IFBFM .GE. 900 .AND. C%IFBFM .LE. 999)) THEN
      ! M-3
      RSF_1 = RSI(795_2,0.208*FF,CF) * SF
      RSF_2 = RSI(11_2,0.208*FF,CF) * SF
      ISF_c = C%PDF*ISF(795_2,RSF_1,CF) + &
              (1.0-C%PDF)*ISF(11_2,RSF_2,CF)

   ELSE IF (C%IFBFM .EQ. 80 .OR. &
            (C%IFBFM .GE. 800 .AND. C%IFBFM .LE. 899)) THEN
      ! M-4
      RSF_1 = RSI(895_2,0.208*FF,CF) * SF
      RSF_2 = RSI(11_2,0.208*FF,CF) * SF
      ISF_c = C%PDF*ISF(895_2,RSF_1,CF) + &
              (1.0-C%PDF)*ISF(11_2,RSF_2,CF)

   ELSE
      ISF_c = ISF(C%IFBFM,RSF,CF)
   ENDIF

   ! Equivalent slope wind speed.
   WSE1 = LOG(ISF_c/(0.208*FF)) / 0.05039

   IF (ISF_c .LT. 0.999*2.496*FF) THEN
      WSE2 = 28.0 - LOG(1.0 - ISF_c/(2.496*FF)) / 0.0818
   ELSE
      WSE2 = 112.45
   ENDIF

   IF (WSE1 .LE. 40.0) THEN
      WSE = WSE1
   ELSE
      WSE = WSE2
   ENDIF

   ! Combine actual 10-m wind with equivalent slope wind.
   WSX = C%WS20_NOW * MPH_20FT_TO_KMPH_10M * &
         SIN(C%WD20_NOW*PIO180 + PI) + WSE*SIN(aspect*PIO180 + PI)

   WSY = C%WS20_NOW * MPH_20FT_TO_KMPH_10M * &
         COS(C%WD20_NOW*PIO180 + PI) + WSE*COS(aspect*PIO180 + PI)

   C%WSV = SQRT(WSX*WSX + WSY*WSY)

   ! RAZ is the resultant direction of maximum spread, degrees clockwise
   ! from north in the TO direction.
   IF (C%WSV .LE. 1.0E-6) THEN
      C%RAZ = 0.0
   ELSE
      C%RAZ = MODULO(ATAN2(WSX,WSY)/PIO180 + 360.0,360.0)
   ENDIF

   FW = CFFDRS_FW(C%WSV)
   C%ISI = 0.208 * FF * FW

   ! --------------------------------------------------------------------------
   ! RATE OF SPREAD
   ! --------------------------------------------------------------------------

   RSI_c = RSI(C%IFBFM,C%ISI,CF)
   C%RSI_FBP = RSI_c

   ! Buildup effect.
   IF (BUI_LOCAL .LE. 0.0) THEN
      BE = 0.0
   ELSE IF (FUEL_MODEL_TABLE_FBP(C%IFBFM)%BUI0 .LE. 0.0) THEN
      BE = 1.0
   ELSE
      BE = EXP(50.0 * LOG(FUEL_MODEL_TABLE_FBP(C%IFBFM)%q) * &
           (1.0/BUI_LOCAL - 1.0/FUEL_MODEL_TABLE_FBP(C%IFBFM)%BUI0))
      BE = MAX(0.0,MIN(BE,FUEL_MODEL_TABLE_FBP(C%IFBFM)%BE_max))
   ENDIF
   C%BE_FBP = BE

   ! Surface head ROS, m/min before ELMFIRE operational adjustment factors.
   ROS = RSI_c * BE

   ! Store adjusted surface head ROS internally in ft/min.
   C%VELOCITY_DMS_SURFACE = ROS * 3.28 * &
                            (C%ADJ + PERTURB_ADJ) * &
                            DIURNAL_ADJUSTMENT_FACTOR

#ifdef _SUPPRESSION
   IF (ENABLE_EXTENDED_ATTACK) THEN
      IF (EXTENDED_ATTACK_MODEL .EQ. 0 .OR. EXTENDED_ATTACK_MODEL .EQ. 1) THEN
         C%VELOCITY_DMS_SURFACE = C%VELOCITY_DMS_SURFACE * &
                                  C%SUPPRESSION_ADJUSTMENT_FACTOR
      ELSE
         WRITE(*,*) 'Error: "EXTENDED_ATTACK_MODEL" should be 0 or 1 in namelist!'
         STOP
      ENDIF
   ENDIF
#endif

   ! Surface fuel consumption.
   FFMC = (14867.2 - 59.5*M) / (147.2 + M)
   C%SFC = SFC(C%IFBFM,FFMC,BUI_LOCAL)

   ! No-wind/no-slope surface ROS, m/min.
   C%VS0 = RSI(C%IFBFM,0.208*FF,CF) * BE

   ! --------------------------------------------------------------------------
   ! SLOPE/WIND MAGNITUDES
   !
   ! These are retained for diagnostics/legacy state. CFFDRS propagation
   ! direction itself is now taken directly from RAZ.
   ! --------------------------------------------------------------------------

   FW = CFFDRS_FW(WSE)
   ISI_s = 0.208 * FF * FW
   RSF = RSI(C%IFBFM,ISI_s,CF) * BE

   IF (C%VS0 .LE. 0.0) THEN
      C%PHIS_SURFACE = 0.0
      C%PHIW_SURFACE = 0.0
   ELSE
      C%PHIS_SURFACE = MAX(RSF/C%VS0 - 1.0,0.0)
      C%PHIW_SURFACE = MAX(ROS/C%VS0 - 1.0 - C%PHIS_SURFACE,0.0)
   ENDIF

   ! Convert no-wind/no-slope ROS to ELMFIRE internal ft/min.
   C%VS0 = C%VS0 * 3.28

   ! FBP does not use the Rothermel residence-time / HPUA formulation.
   C%IR = 0.0
   C%HPUA_SURFACE = 0.0

   ! Compute current CFB/CFC/fire type and final C-6 head ROS deterministically.
   ! This sets C%VELOCITY_DMS to the final FBP head ROS and updates
   ! C%FLIN_DMS_SURFACE using current crown state.
   CALL CFFDRS_HEAD_CROWN_BEHAVIOR(C)

   C => C%NEXT
ENDDO

! ***************************************************************************** 
END SUBROUTINE CFFDRS_SPREAD_RATE
! *****************************************************************************

! *****************************************************************************
SUBROUTINE CFFDRS_HEAD_CROWN_BEHAVIOR(C)
! *****************************************************************************
! Computes deterministic CFFDRS/FBP head-fire crown behavior from the current
! surface-fire state. No crown quantities from a previous evaluation are used.
!
! VELOCITY_DMS_SURFACE remains the surface head ROS.
! VELOCITY_DMS becomes the final FBP head ROS, including C-6 crown adjustment.
! FLIN_DMS_SURFACE stores total CFFDRS head-fire intensity (surface + crown).

TYPE(NODE), POINTER, INTENT(INOUT) :: C
REAL :: RSS, RSO, RSC, FME, ROS_FINAL

! Reset all CFFDRS crown state before evaluating the current conditions.
C%CROWN_FIRE = 0
C%CFB = 0.0
C%CFC = 0.0
C%TFC = C%SFC
C%RSO_FBP = 0.0
C%FLIN_CANOPY = 0.0
C%PHIW_CROWN = 0.0

! Surface head ROS currently being propagated, m/min.
RSS = MAX(C%VELOCITY_DMS_SURFACE / 3.28, 0.0)
ROS_FINAL = RSS
C%VELOCITY_DMS = C%VELOCITY_DMS_SURFACE

! Recalculate the crown-initiation threshold from the current FMC rather than
! retaining a value calculated during an earlier weather evaluation.
C%CRITICAL_FLIN = 9E9
CALL CROWN_CRITICAL_FLIN(C)

! Surface-only head-fire intensity before crown involvement.
C%FLIN_DMS_SURFACE = 300.0 * C%SFC * RSS

! RSO is an FBP diagnostic/output, not merely a crown-fire gate.  Calculate it
! whenever SFC is positive, even when the current head fire does not initiate crowning.
IF (C%SFC .GT. 1.0E-6) THEN
   RSO = C%CRITICAL_FLIN / (300.0*C%SFC)
   C%RSO_FBP = RSO
ELSE
   RSO = 0.0
   C%RSO_FBP = 0.0
ENDIF

IF (CROWN_FIRE_MODEL .LE. 0) RETURN
IF (C%SFC .LE. 1.0E-6) RETURN
IF (FUEL_MODEL_TABLE_FBP(C%IFBFM)%CFL .LE. 0.0) RETURN
IF (CBH%R4(C%IX,C%IY,1) .LE. 0.0) RETURN
IF (RSS .LE. RSO) RETURN

C%CFB = MIN(1.0,MAX(0.0,1.0-EXP(-0.23*(RSS-RSO))))

IF (C%CFB .LE. 0.1) THEN
   C%CROWN_FIRE = 0
ELSE IF (C%CFB .LT. 0.9) THEN
   C%CROWN_FIRE = 1
ELSE
   C%CROWN_FIRE = 2
ENDIF

IF (C%IFBFM .EQ. 40 .OR. C%IFBFM .EQ. 50 .OR. C%IFBFM .EQ. 60 .OR. &
    (C%IFBFM .GE. 400 .AND. C%IFBFM .LE. 699)) THEN
   C%CFC = FUEL_MODEL_TABLE_FBP(C%IFBFM)%CFL*C%CFB*C%PC
ELSE IF (C%IFBFM .EQ. 70 .OR. C%IFBFM .EQ. 80 .OR. C%IFBFM .EQ. 90 .OR. &
         (C%IFBFM .GE. 700 .AND. C%IFBFM .LE. 999)) THEN
   C%CFC = FUEL_MODEL_TABLE_FBP(C%IFBFM)%CFL*C%CFB*C%PDF
ELSE
   C%CFC = FUEL_MODEL_TABLE_FBP(C%IFBFM)%CFL*C%CFB
ENDIF
C%TFC = C%SFC + C%CFC

IF (C%IFBFM .EQ. 6) THEN
   IF (C%CFC .LE. 0.0) THEN
      RSC = 0.0
   ELSE
      FME = 1000.0*((1.5-0.00275*C%FMC)**4.0)/(460.0+25.9*C%FMC)
      RSC = 60.0*(1.0-EXP(-0.0497*C%ISI))*FME/0.778237
   ENDIF
   ROS_FINAL = RSS + C%CFB*(RSC-RSS)
ENDIF

C%VELOCITY_DMS = ROS_FINAL * 3.28
C%FLIN_DMS_SURFACE = 300.0 * (C%SFC + C%CFC) * ROS_FINAL

! *****************************************************************************
END SUBROUTINE CFFDRS_HEAD_CROWN_BEHAVIOR
! *****************************************************************************

! *****************************************************************************
SUBROUTINE UPDATE_LOCAL_SPREAD_PROPERTIES(L,DUMMY_NODE,DEFER_DERIVED)
! *****************************************************************************
! Finalizes local fireline intensity and derived output quantities.
!
! CFFDRS crown state (CFB, CFC, CROWN_FIRE and C-6 final head ROS) is computed
! deterministically in CFFDRS_HEAD_CROWN_BEHAVIOR before ellipse propagation.
! This routine therefore does not derive CFFDRS crown behavior from stale or
! local post-ellipse state.
!
! Rothermel crown-fire behavior remains separate and unchanged.

TYPE(DLL), INTENT(INOUT) :: L
TYPE(NODE), POINTER, INTENT(INOUT) :: DUMMY_NODE
LOGICAL, OPTIONAL, INTENT(IN) :: DEFER_DERIVED

TYPE(NODE), POINTER :: C
INTEGER :: I, NUM_NODES, IX, IY
REAL :: CROS, CBD_EFF, WS10KMPH, CROSA, R0, CAC
LOGICAL :: UPDATE_DERIVED

UPDATE_DERIVED = .TRUE.
IF (PRESENT(DEFER_DERIVED)) UPDATE_DERIVED = .NOT. DEFER_DERIVED

! Conservatively preserve stage values for physics consumers. Evaluate per
! call, not once per run: feature state can change.
UPDATE_DERIVED = UPDATE_DERIVED .OR. ASSOCIATED(DUMMY_NODE) .OR. USE_BARRIERS
#ifdef _WUI
UPDATE_DERIVED = UPDATE_DERIVED .OR. USE_BLDG_SPREAD_MODEL
#endif
#ifdef _SUPPRESSION
UPDATE_DERIVED = UPDATE_DERIVED .OR. ENABLE_EXTENDED_ATTACK
#endif
#ifdef _SMOKE
UPDATE_DERIVED = UPDATE_DERIVED .OR. ENABLE_SMOKE_OUTPUTS
#endif

IF (ASSOCIATED(DUMMY_NODE)) THEN
   NUM_NODES = 1
   C => DUMMY_NODE
ELSE
   NUM_NODES = L%NUM_NODES
   C => L%HEAD
ENDIF

DO I = 1, NUM_NODES
   IX = C%IX
   IY = C%IY

   IF (SURFACE_MODEL_CFFDRS) THEN
      ! CFFDRS stores total surface+crown intensity in FLIN_SURFACE. Crown
      ! consumption was already calculated from the current head-fire state.
      C%FLIN_CANOPY = 0.0
      C%FLIN_SURFACE = 300.0 * (C%SFC + C%CFC) * MAX(C%VELOCITY/3.28,0.0)

   ELSE IF (SURFACE_MODEL_ROTHERMEL) THEN
      CALL CROWN_CRITICAL_FLIN(C)

      IF (C%FLIN_SURFACE .LT. C%CRITICAL_FLIN .OR. CROWN_FIRE_MODEL .LE. 0) THEN
         C%CROWN_FIRE = 0
         C%FLIN_CANOPY = 0.0
      ELSE IF (C%VS0 .GT. 0.0 .AND. CBD%R4(IX,IY,1) .GT. 1E-3 .AND. &
               CC%R4(IX,IY,1) .GT. 1E-3) THEN
         C%FLIN_CANOPY = C%HPUA_CANOPY * C%VELOCITY * 5.08E-3
         CROS = 0.0
         CBD_EFF = MAX(CBD%R4(IX,IY,1) + PERTURB_CBD,0.01)
         WS10KMPH = C%WS20_NOW * MPH_20FT_TO_KMPH_10M
         CROSA = CROWN_FIRE_ADJ * 11.02 * WS10KMPH**0.9 * CBD_EFF**0.19 * &
                 EXP(-0.17*100.0*C%M1) / 0.3048
         CROSA = MIN(CROSA,CROWN_FIRE_SPREAD_RATE_LIMIT)
         R0 = (3.0/CBD_EFF) / 0.3048
         CAC = CROSA/R0

         IF (CAC .GT. 1.0) THEN
            IF (CC%R4(IX,IY,1) .GE. CRITICAL_CANOPY_COVER) THEN
               C%CROWN_FIRE = 2
               CROS = CROSA
               C%PHIW_CROWN = MIN(MAX(CROS/MAX(C%VS0,0.001) - 1.0,0.0),200.0)
            ELSE
               C%CROWN_FIRE = 1
            ENDIF
         ELSE
            C%CROWN_FIRE = 1
            IF (CC%R4(IX,IY,1) .GE. CRITICAL_CANOPY_COVER) THEN
               CROS = CROSA * EXP(-CAC)
               C%PHIW_CROWN = MIN(MAX(CROS/MAX(C%VS0,0.001) - 1.0,0.0),200.0)
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   IF (UPDATE_DERIVED) THEN
      C%FLAME_LENGTH = (0.0775/0.3048) * (C%FLIN_SURFACE + C%FLIN_CANOPY)**0.46
      C%HRRPUA = (C%FLIN_SURFACE + C%FLIN_CANOPY) / ASP%CELLSIZE
   ENDIF

   C => C%NEXT
ENDDO

! *****************************************************************************
END SUBROUTINE UPDATE_LOCAL_SPREAD_PROPERTIES
! *****************************************************************************

! *****************************************************************************
SUBROUTINE CROWN_CRITICAL_FLIN(C)
! *****************************************************************************
! Computes (once, then caches) the critical surface fireline intensity required
! for crown-fire initiation at node C, along with canopy heat-per-unit-area,
! from canopy bulk density, canopy/base heights, and foliar moisture content.

TYPE (NODE), POINTER, INTENT(INOUT) :: C
REAL :: FMCTERM, CBH_EFF
INTEGER :: IX, IY

IX = C%IX
IY = C%IY

IF (C%CRITICAL_FLIN .GT. 1E9) THEN
   IF (SURFACE_MODEL_CFFDRS) THEN
      ! FBP crown initiation uses CBH and FMC only.  Do not touch CBD here:
      ! CFFDRS does not require a canopy-bulk-density raster.  Fuels with zero
      ! CFL are non-crowning in the FBP reference and therefore have CSFI=0.
      C%HPUA_CANOPY = 0.0
      IF (FUEL_MODEL_TABLE_FBP(C%IFBFM)%CFL .LE. 0.0) THEN
         C%CRITICAL_FLIN = 0.0
      ELSE IF (CBH%R4(IX,IY,1) .GE. 0.0) THEN
         FMCTERM = 460.0 + 25.9*C%FMC
         CBH_EFF = MAX(CBH%R4(IX,IY,1) + PERTURB_CBH, 0.0)
         C%CRITICAL_FLIN = (0.01 * CBH_EFF * FMCTERM) ** 1.5
      ELSE
         C%CRITICAL_FLIN = 9E9
      ENDIF
   ELSE
      ! Preserve the existing Rothermel crown-fire formulation.
      C%HPUA_CANOPY = CBD%R4(IX,IY,1) * &
                      MAX(CH%R4(IX,IY,1) - CBH%R4(IX,IY,1),0.) * 12000. !kJ/m2
      IF (CBH%R4(IX,IY,1) .GE. 0.0) THEN
         FMCTERM = 460.0 + 26.0*C%FMC
         CBH_EFF = MAX(CBH%R4(IX,IY,1) + PERTURB_CBH, 0.1)
         C%CRITICAL_FLIN = (0.01 * CBH_EFF * FMCTERM) ** 1.5
      ELSE
         C%CRITICAL_FLIN = 9E9
      ENDIF
   ENDIF
ENDIF

! *****************************************************************************
END SUBROUTINE CROWN_CRITICAL_FLIN
! *****************************************************************************

#ifdef _WUI
! *****************************************************************************
SUBROUTINE HAMADA(C)
! *****************************************************************************
! USE HAMADA MODEL TO CALCULATE THE ROS AT ANY WIND DIRECTION RELATIVE TO A GIVEN DIRECTION OF FIRE FRONT
! This subroutine is a contribution from Yiren Qin (yqin123@umd.edu)

TYPE(NODE), POINTER, INTENT(INOUT) :: C

REAL :: A_0 , D , F_B , V , X_T ! INPUTS 

! COEFFICIENT FOR HAMADA MODEL 
REAL, PARAMETER :: &
   C_14 = 1.6, C_24 = 0.1, C_34 = 0.007, C_44 = 25.0, C_54 = 2.5 , &
   C_1S = 1.0, C_2S = 0.0, C_3S = 0.005, C_4S = 5.0 , C_5S = 0.25, & 
   C_1U = 1.0, C_2U = 0.0, C_3U = 0.002, C_4U = 5.0 , C_5U = 0.2

REAL :: CV_4 , CV_S , CV_U 
REAL :: K_D , K_S , K_U, K_D_C , K_S_C , K_U_C , T_4 , T_S , T_U , & 
        V_D , V_D_C , V_S , V_S_C , V_U , V_U_C 

! HAMADA ELLIPSE DEFINITION 
X_T = 120.0      ! TIME IN MINUTES, the ROS predicted by Hamada model is a function of time, but will converge to a constant value in short. 
V   = C%WS20_NOW * 0.447 ! WIND SPEED , M / S 

! These values are taken at constant at this stage, but should vary with the footprint.
!A_0 = 23        ! AVERAGE BUILDING PLAN DIMENSION , M 
!D   = 45         ! AVERAGE BUILDING SEPERATION , M 
!F_B = 0       ! RATIO OF FIRE RESISTANCE BUILDINGS

A_0 = BLDG_AREA%R4 (C%IX,C%IY,1) ! AVERAGE BUILDING PLAN DIMENSION , M 
D   = BLDG_SEPARATION_DIST%R4 (C%IX,C%IY,1) ! AVERAGE BUILDING SEPERATION , M 
F_B = BLDG_NONBURNABLE_FRAC%R4(C%IX,C%IY,1) ! RATIO OF FIRE RESISTANCE BUILDINGS

CV_4 = C_14 * ( 1 + C_24 * V + C_34 * V ** 2 ) 
CV_S = C_1S * ( 1 + C_2S * V + C_3S * V ** 2 ) 
CV_U = C_1U * ( 1 + C_2U * V + C_3U * V ** 2 ) 

! TIME IN MINUTES THE FULLY DEVELOPED FIRE REQUIRES TO ADVANCE TO THE NEXT BUILDING 
T_4 = (( 1-F_B ) * ( 3 + 0.375 * A_0 + ( 8 * D / ( C_44 + C_54 * V ) ) ) + & 
      F_B * ( 5 + 0.625 * A_0 + 16 * D / ( C_44 + C_54 * V ) ) )/ CV_4 
T_S = (( 1-F_B ) * ( 3 + 0.375 * A_0 + ( 8 * D / ( C_4S + C_5S * V ) ) ) + & 
      F_B * ( 5 + 0.625 * A_0 + 16 * D / ( C_4S + C_5S * V ) )) / CV_S 
T_U = (( 1-F_B ) * ( 3 + 0.375 * A_0 + ( 8 * D / ( C_4U + C_5U * V ) ) ) + & 
      F_B * ( 5 + 0.625 * A_0 + 16 * D / ( C_4U + C_5U * V ) ) )/ CV_U 

K_D = MAX(( A_0 + D ) / T_4 * X_T ,1E-10)
K_S = MAX(( A_0 / 2 + D ) + ( A_0 + D ) / T_S * ( X_T-T_S ),1E-10) 
K_U = MAX(( A_0 / 2 + D ) + ( A_0 + D ) / T_U * ( X_T-T_U ),1E-10)

V_D = MAX(( A_0 + D ) / T_4,1E-10) 
V_S = MAX(( A_0 + D ) / T_S,1E-10) 
V_U = MAX(( A_0 + D ) / T_U,1E-10)

! HAZUS CORRECTION
IF(V .LE. 10) THEN

   K_D_C = K_D * V/10.0+SQRT((K_D+K_U)/2*K_S)*(1-V/10.0)
   K_U_C = K_U * V/10.0+SQRT((K_D+K_U)/2*K_S)*(1-V/10.0)
   K_S_C = K_S * V/10.0+SQRT((K_D+K_U)/2*K_S)*(1-V/10.0)
   
   V_D_C = MAX(V_D * V / 10 + & 
            ( K_D * V_S + V_D * K_S + K_U * V_S + V_U * K_S ) * & 
           SQRT( 2 / ( K_D + K_U )/K_S ) * ( 1-V / 10 )/4,1E-10)
   V_S_C = MAX(V_S * V / 10 + & 
            ( K_D * V_S + V_D * K_S + K_U * V_S + V_U * K_S ) * & 
           SQRT( 2 / ( K_D + K_U )/K_S ) * ( 1-V / 10 )/4,1E-10)
   V_U_C = MAX(V_U * V / 10 + & 
            ( K_D * V_S + V_D * K_S + K_U * V_S + V_U * K_S ) * & 
           SQRT( 2 / ( K_D + K_U )/K_S ) * ( 1-V / 10 )/4 ,1E-10)

   V_D = V_D_C !M/MIN
   V_S = V_S_C !M/MIN
   V_U = V_U_C !M/MIN
ENDIF

IF(MIN(K_D,MIN(K_S,K_U)) .LE. 1E-1) THEN
    V_D = K_D/MAX(X_T,1E-10)
    V_S = K_S/MAX(X_T,1E-10)
    V_U = K_U/MAX(X_T,1E-10)
ENDIF

C%VELOCITY_DMS = V_D /0.3048 ! Unit Transform to ft/min
C%VBACK = V_U/0.3048
C%LOW = MIN((V_D+V_U)/2/V_S,10.0)

! *****************************************************************************
END SUBROUTINE HAMADA
! *****************************************************************************

! *****************************************************************************
SUBROUTINE UMD_UCB_BLDG_SPREAD(C, DT_ELMFIRE)
! *****************************************************************************
! The function calculates the surface fire spreading rate for FBFM91
USE ELMFIRE_VARS
!ANALYSIS_CELLSIZE, BUILDING_FUEL_MODEL_TABLE

TYPE(NODE), POINTER, INTENT(INOUT) :: C
REAL, INTENT(IN) :: DT_ELMFIRE
TYPE(UCB_ELLIPSE) :: ELLIPSE_PARAMETERS
INTEGER :: IX, IY, BLDG_FM
REAL :: FTP_PA, ANALYSIS_CELLSIZE_SQUARED, TOTAL_TRANSIENT_DFC, TOTAL_TRANSIENT_RADIATION, &
        RAD_PER_SQCELL, UCB_DIV, FLAME_FRONT, FLAME_SIDE, FLAME_BACK, SUM_ELLIPSE, V_S !DWI_M1
INTEGER, PARAMETER :: NO_DATA = -9999

BLDG_FM = C%IBLDGFM
IF (C%IBLDGFM .EQ. NO_DATA) BLDG_FM = 1

IX = C%IX
IY = C%IY

FTP_PA = BUILDING_FUEL_MODEL_TABLE(BLDG_FM)%FTP_CRIT

! Set a few constants:
ANALYSIS_CELLSIZE_SQUARED = ANALYSIS_CELLSIZE * ANALYSIS_CELLSIZE

TOTAL_TRANSIENT_DFC = TRANSIENT_DFC_WUI(IX, IY)*DT_ELMFIRE*ANALYSIS_CELLSIZE_SQUARED
TOTAL_TRANSIENT_RADIATION = TRANSIENT_RADIATION_WUI(IX, IY)*DT_ELMFIRE*ANALYSIS_CELLSIZE_SQUARED

RAD_PER_SQCELL = (TOTAL_TRANSIENT_DFC + TOTAL_TRANSIENT_RADIATION)/ANALYSIS_CELLSIZE_SQUARED

IF ((TOTAL_TRANSIENT_DFC + TOTAL_TRANSIENT_RADIATION)>30000) THEN
   FTP_PA = 3000
ELSE
   FTP_PA = 3000000/AMAX1(1E-3, RAD_PER_SQCELL*RAD_PER_SQCELL)
ENDIF

IF (C%WS20_NOW .LE. 35) THEN
   UCB_DIV = 1.8
ELSE
   UCB_DIV = 1.0
ENDIF
! WRITE(*,*) TOTAL_TRANSIENT_DFC, TOTAL_TRANSIENT_RADIATION
C%ABSOLUTE_U = 60*(TOTAL_TRANSIENT_DFC + TOTAL_TRANSIENT_RADIATION)/(0.3048*DT_ELMFIRE*ANALYSIS_CELLSIZE*FTP_PA)/UCB_DIV ! Unit: ft/min
C%ABSOLUTE_U = MIN(C%ABSOLUTE_U, 1E5)

ELLIPSE_PARAMETERS = ELLIPSE_PROPERTY_MAP(IX, IY)

FLAME_FRONT = ELLIPSE_PARAMETERS%ELLIPSE_MAJOR + ELLIPSE_PARAMETERS%ELLIPSE_ECCENTRICITY
FLAME_SIDE = 2*ELLIPSE_PARAMETERS%ELLIPSE_MINOR
FLAME_BACK = ELLIPSE_PARAMETERS%ELLIPSE_MAJOR - ELLIPSE_PARAMETERS%ELLIPSE_ECCENTRICITY

SUM_ELLIPSE = FLAME_FRONT + FLAME_SIDE + FLAME_BACK   !DWI_M6

C%VELOCITY_DMS = C%ABSOLUTE_U*FLAME_FRONT/MAX(1E-5,SUM_ELLIPSE)   !DWI_M7
C%VBACK = C%ABSOLUTE_U*FLAME_BACK/MAX(1E-5,SUM_ELLIPSE)    !DWI_M7
V_S = C%ABSOLUTE_U*FLAME_SIDE/MAX(1E-5,SUM_ELLIPSE)    !DWI_M7

IF (V_S .GT. 1E-4) THEN
   C%LOW = AMIN1((C%VELOCITY_DMS+C%VBACK)/2/V_S,10.0)
ELSE
   C%LOW = 1.0
ENDIF

! Interface Model
IF (C%TEST_INTERFACE) THEN
   C%VELOCITY_DMS = 0.0
   C%VBACK = 0.0
   C%LOW = 1.0
   ! PRINT *, C%VELOCITY_DMS
ENDIF

! C%HEAT_VALUE = 0.

! *****************************************************************************
END SUBROUTINE UMD_UCB_BLDG_SPREAD
! ****************************************************************************

! *****************************************************************************
SUBROUTINE CALC_WUI_HEATFLUX(BURNING_NODES, NX, NY, DT_ELMFIRE)
! *****************************************************************************
! Calculate the emitted heat flux from BURNING_NODES to adjcent cells (IX,IY), unit kW
USE ELMFIRE_VARS
! BANDTHICKNESS_WUI, ANALYSIS_CELLSIZE, BUILDING_FUEL_MODEL_TABLE, PIO180, TOTAL_DFC_WUI, 
! TOTAL_RADIATION_WUI, TRANSIENT_DFC_WUI, TRANSIENT_RADIATION_WUI

TYPE(NODE), POINTER, INTENT(INOUT) :: BURNING_NODES
INTEGER, INTENT(IN) :: NX, NY
REAL, INTENT(IN) :: DT_ELMFIRE

INTEGER :: IX_BURNING, IY_BURNING, IXTAGSTART, IXTAGSTOP, IYTAGSTART, IYTAGSTOP, IX, IY, &
           DEL_X, DEL_Y, IBLDGFM
INTEGER, PARAMETER :: NO_DATA = -9999
REAL :: DFC_COEFF, RAD_COEFF, ANALYSIS_CELLSIZE_SQUARED, RANALYSIS_CELLSIZE, HALF_ANALYSIS_CELLSIZE, HRR_ADJUSTER, ELLIPSE_MINOR_SQUARED, & 
        RDEL_X, RDEL_Y, TARGET_R, TARGET_R_METERS, TARGET_THETA, WIND_THETA, TARGET_THETA_F, MAX_ELLIPSE_DIST, &
        ELLIPSE_DIST_THETA, DFC_CHECKER, DFC_FACTOR, DFC_HEAT_RECEIVED, RAD_LIMIT_THETA, RAD_CHECKER, DELTA_RAD, RAD_FACTOR, &
        RAD_EFF_DIST, RAD_HEAT_RECEIVED, WTU_DIST_LIMIT, WTU_FLIN_LIMIT, HRR_BURNING_NODE

TYPE(UCB_ELLIPSE) :: ELLIPSE_PARAMETERS
! Interface submodel
WTU_DIST_LIMIT = 1  ! Arbitrary
WTU_FLIN_LIMIT = 1.0E3  ! Arbitrary

IX_BURNING = BURNING_NODES%IX
IY_BURNING = BURNING_NODES%IY

ANALYSIS_CELLSIZE_SQUARED = ANALYSIS_CELLSIZE*ANALYSIS_CELLSIZE
RANALYSIS_CELLSIZE = 1. / ANALYSIS_CELLSIZE !Reciprocal analysis cellsize
HALF_ANALYSIS_CELLSIZE = 0.5 * ANALYSIS_CELLSIZE

ELLIPSE_PARAMETERS = ELLIPSE_PROPERTY_MAP(IX_BURNING, IY_BURNING)

! Introducing effective axes radius of ellipse. Improve physics consideration and make the calibration process less stiff.
HRR_ADJUSTER = ANALYSIS_CELLSIZE_SQUARED/(PI*(HRR_ELLIPSE_ADJ*ELLIPSE_PARAMETERS%ELLIPSE_MAJOR)*(HRR_ELLIPSE_ADJ*ELLIPSE_PARAMETERS%ELLIPSE_MINOR))  !DWI_M2

ELLIPSE_MINOR_SQUARED = ELLIPSE_PARAMETERS%ELLIPSE_MINOR * ELLIPSE_PARAMETERS%ELLIPSE_MINOR

IXTAGSTART = MAX(3,    IX_BURNING - BANDTHICKNESS_WUI) 
IXTAGSTOP  = MIN(NX-2, IX_BURNING + BANDTHICKNESS_WUI)
IYTAGSTART = MAX(3,    IY_BURNING - BANDTHICKNESS_WUI)
IYTAGSTOP  = MIN(NY-2, IY_BURNING + BANDTHICKNESS_WUI)

! These quantities depend only on the burning node / its ellipse, not on the target (IX,IY),
! so hoist them out of the band loop below (they are otherwise recomputed for every target cell).
WIND_THETA = PIO180 * (270. - BURNING_NODES%WD20_NOW) !in radians
MAX_ELLIPSE_DIST = 0.3 * ELLIPSE_PARAMETERS%DIST_DOWNWIND * (ELLIPSE_PARAMETERS%ELLIPSE_MAJOR - ELLIPSE_PARAMETERS%ELLIPSE_ECCENTRICITY) / ELLIPSE_MINOR_SQUARED
HRR_BURNING_NODE = HRR_TRANSIENT_MAP(IX_BURNING, IY_BURNING)

! Update the aggregated heat flux for each target (IX,IY) from all burning nodes at current time
DO IY = IYTAGSTART, IYTAGSTOP
DO IX = IXTAGSTART, IXTAGSTOP
   DEL_X = IX - IX_BURNING
   DEL_Y = IY - IY_BURNING

   RDEL_X = REAL(DEL_X)
   RDEL_Y = REAL(DEL_Y)

   TARGET_R = SQRT( RDEL_X*RDEL_X + RDEL_Y*RDEL_Y)
   TARGET_R_METERS = TARGET_R*ANALYSIS_CELLSIZE

   IF (TARGET_R .LT. 1E-3) THEN
      CYCLE
   ENDIF

   ! Interface Model
   IF ((CRITICAL_HF_WUI .EQ. 2) .AND. &
      (BURNING_NODES%IFBFM .NE. 91) .AND. &
      (TARGET_R .LE. WTU_DIST_LIMIT).AND. &
      (FBFM%I2(IX,IY,1) .EQ. 91)) THEN

      TEST_INTERFACE_WUI(IX,IY) = .TRUE.

      IF (BURNING_NODES%FLIN_SURFACE .GT. WTU_FLIN_LIMIT) THEN
         WTU_SPREAD_WUI(IX,IY) = .TRUE.
      ENDIF

      CYCLE
   ENDIF

   IF(BLDG_FUEL_MODEL%I2(IX,IY,1) .NE. NO_DATA) THEN
      IBLDGFM =  BLDG_FUEL_MODEL%I2(IX,IY,1)
   ELSE
      IBLDGFM =  1
   ENDIF
   DFC_COEFF = 1 - BUILDING_FUEL_MODEL_TABLE(IBLDGFM)%NONBURNABLE_FRAC
   RAD_COEFF = BUILDING_FUEL_MODEL_TABLE(IBLDGFM)%ABSORPTIVITY

   TARGET_THETA = ATAN2(RDEL_Y, RDEL_X) !in radians
   TARGET_THETA_F = TARGET_THETA - WIND_THETA !in radians

   ! Direct Flame Contact
   ELLIPSE_DIST_THETA = MAX_ELLIPSE_DIST*ELLIPSE_MINOR_SQUARED / (ELLIPSE_PARAMETERS%ELLIPSE_MAJOR - ELLIPSE_PARAMETERS%ELLIPSE_ECCENTRICITY*COS(TARGET_THETA_F))

   DFC_CHECKER = RANALYSIS_CELLSIZE * (ELLIPSE_DIST_THETA + HALF_ANALYSIS_CELLSIZE - TARGET_R_METERS)

   DFC_FACTOR = AMAX1(0.0,AMIN1(1.0,DFC_CHECKER))

   DFC_HEAT_RECEIVED = DFC_COEFF*DFC_FACTOR*HRR_BURNING_NODE*HRR_ADJUSTER  !DWI_M3

   ! Radiation
   RAD_LIMIT_THETA = ELLIPSE_DIST_THETA + BURNING_NODES%RAD_DIST
   RAD_CHECKER = RANALYSIS_CELLSIZE * (RAD_LIMIT_THETA + HALF_ANALYSIS_CELLSIZE - TARGET_R_METERS)

   DELTA_RAD = AMAX1(0.0,AMIN1(1.0,RAD_CHECKER))
   RAD_FACTOR = DELTA_RAD - DELTA_RAD*DFC_FACTOR

   IF ((DFC_FACTOR .LT. 1) .AND. (DFC_FACTOR .GT. 0)) THEN
        RAD_EFF_DIST = ANALYSIS_CELLSIZE - DFC_FACTOR*ANALYSIS_CELLSIZE
   ELSE
        RAD_EFF_DIST  = TARGET_R_METERS - ELLIPSE_DIST_THETA
   ENDIF

   RAD_HEAT_RECEIVED = HRR_ADJUSTER*(0.3*DFC_COEFF*RAD_COEFF*RAD_FACTOR*HRR_BURNING_NODE*ANALYSIS_CELLSIZE_SQUARED)/(4*PI*RAD_EFF_DIST*RAD_EFF_DIST)  !DWI_M4
   
   TRANSIENT_DFC_WUI(IX,IY) = TRANSIENT_DFC_WUI(IX,IY) + DFC_HEAT_RECEIVED
   TRANSIENT_RADIATION_WUI(IX,IY) = TRANSIENT_RADIATION_WUI(IX,IY) + RAD_HEAT_RECEIVED
   
   ! Update the accumulated heat uptill now
   TOTAL_DFC_WUI(IX,IY) = TOTAL_DFC_WUI(IX,IY) + DFC_HEAT_RECEIVED * DT_ELMFIRE * ANALYSIS_CELLSIZE_SQUARED
   TOTAL_RADIATION_WUI(IX,IY) = TOTAL_RADIATION_WUI(IX,IY) + RAD_HEAT_RECEIVED * DT_ELMFIRE * ANALYSIS_CELLSIZE_SQUARED

ENDDO
ENDDO

! *****************************************************************************
END SUBROUTINE CALC_WUI_HEATFLUX
! *****************************************************************************

! *****************************************************************************
SUBROUTINE ELLIPSE_UCB(C)
! *****************************************************************************
! Builds the UCB WUI fire-footprint ellipse for node C from wind speed and
! building area/separation (Hamada-derived regressions, with HAZUS and high-wind
! branches): computes downwind/upwind/sidewind distances and the resulting
! ellipse major/minor/eccentricity, storing them in ELLIPSE_PROPERTY_MAP(IX,IY).

USE ELMFIRE_VARS
!ELLIPSE_PROPERTY_MAP

TYPE(NODE), POINTER, INTENT(INOUT) :: C
REAL :: V_MPS, EB2, D1, D2, D3, S1, S2, S3, U1, U2, U3, HAMADA_A, HAMADA_D
INTEGER, PARAMETER :: NO_DATA = -9999

V_MPS = C%WS20_NOW * 0.447 ! WIND SPEED , M / S

IF (C%IFBFM .EQ. 91) THEN
   C%ELLIPSE_PARAMETERS%FOREST_FACTOR = 1
ELSE
   C%ELLIPSE_PARAMETERS%FOREST_FACTOR = 3
ENDIF

HAMADA_A = BLDG_AREA%R4 (C%IX,C%IY,1) ! AVERAGE BUILDING PLAN DIMENSION , M 
HAMADA_D = BLDG_SEPARATION_DIST%R4 (C%IX,C%IY,1) ! AVERAGE BUILDING SEPERATION , M 


! Inputs incompatibility traps
IF (HAMADA_A .EQ. NO_DATA) HAMADA_A = 10
IF (HAMADA_D .EQ. NO_DATA) HAMADA_D = 10

IF (HAMADA_D .GT. 50) HAMADA_D = 50

! This is regression from HAMADA. Subject of changes. ------------------------------------------

IF (V_MPS .LT. 10) THEN  ! HAZUS CORRECTION

   D1 = 1.679463256 - 0.123901243*HAMADA_A + 0.307612446*HAMADA_D
   D2 = 78.62957398 + 1.536189561*HAMADA_A - 0.5662073*HAMADA_D

   S1 = -2.922896622 - 0.05550541*HAMADA_A + 0.017291361*HAMADA_D
   S2 = 39.31478699 + 0.768094781*HAMADA_A - 0.28310365*HAMADA_D

   U1 = -6.297892493 - 0.119654483*HAMADA_A + 0.037754535*HAMADA_D
   U2 = 78.62957398 + 1.536189561*HAMADA_A - 0.5662073*HAMADA_D

   C%ELLIPSE_PARAMETERS%DIST_DOWNWIND = C%WIND_PROP*(D1*V_MPS + D2)
   C%ELLIPSE_PARAMETERS%DIST_UPWIND = C%WIND_PROP*(U1*V_MPS + U2)
   C%ELLIPSE_PARAMETERS%DIST_SIDEWIND = C%WIND_PROP*(S1*V_MPS + S2)

ELSEIF (V_MPS .GT. 17.3) THEN  ! HIGH WIND SPEED

   D1 = -7.159031537 - 0.043555289*HAMADA_A - 0.14894238*HAMADA_D
   D2 = 394.4930697 + 0.720929023*HAMADA_A + 11.42149084*HAMADA_D

   S1 = -0.577270631 - 0.015285438*HAMADA_A + 0.012786629*HAMADA_D
   S2 = 38.11784939 + 0.800599307*HAMADA_A - 0.412476476*HAMADA_D

   U1 = -1.092711783 - 0.025390239*HAMADA_A + 0.016740663*HAMADA_D
   U2 = 52.39584604 + 1.104793131*HAMADA_A - 0.57241037*HAMADA_D

   C%ELLIPSE_PARAMETERS%DIST_DOWNWIND = C%WIND_PROP*(D1*V_MPS + D2)
   C%ELLIPSE_PARAMETERS%DIST_UPWIND = C%WIND_PROP*(U1*V_MPS + U2)
   C%ELLIPSE_PARAMETERS%DIST_SIDEWIND = C%WIND_PROP*(S1*V_MPS + S2)

ELSE  
   
   D1 = 4.099488028 - 0.000767118*HAMADA_A + 0.134372426*HAMADA_D
   D2 = -94.26651508 - 0.000694022*HAMADA_A - 3.053034015*HAMADA_D
   D3 = 615.192675 + 0.300438559*HAMADA_A + 19.34120221*HAMADA_D

   S1 = 0.437844987 + 0.008280661*HAMADA_A - 0.002833081*HAMADA_D
   S2 = -10.13978982 - 0.192922421*HAMADA_A + 0.067862023*HAMADA_D
   S3 = 66.32382799 + 1.282260348*HAMADA_A - 0.484673257*HAMADA_D

   U1 = 0.525004045 + 0.01046073*HAMADA_A - 0.004473105*HAMADA_D
   U2 = -12.4091466 - 0.249326233*HAMADA_A + 0.109759448*HAMADA_D
   U3 = 84.64808209 + 1.727651884*HAMADA_A - 0.801945211*HAMADA_D


   C%ELLIPSE_PARAMETERS%DIST_DOWNWIND = C%WIND_PROP*(D1*V_MPS**2 + D2*V_MPS + D3)
   C%ELLIPSE_PARAMETERS%DIST_UPWIND = C%WIND_PROP*(U1*V_MPS**2 + U2*V_MPS + U3)
   C%ELLIPSE_PARAMETERS%DIST_SIDEWIND = C%WIND_PROP*(S1*V_MPS**2 + S2*V_MPS + S3)

ENDIF

! ----------------------------------------------------------------------------------------

C%ELLIPSE_PARAMETERS%ELLIPSE_MAJOR = (C%ELLIPSE_PARAMETERS%DIST_DOWNWIND + C%ELLIPSE_PARAMETERS%DIST_UPWIND)/2
C%ELLIPSE_PARAMETERS%ELLIPSE_ECCENTRICITY = AMIN1(C%ELLIPSE_PARAMETERS%ELLIPSE_MAJOR/2,C%ELLIPSE_PARAMETERS%ELLIPSE_MAJOR - C%ELLIPSE_PARAMETERS%DIST_UPWIND)
EB2 = 1.0-(C%ELLIPSE_PARAMETERS%ELLIPSE_ECCENTRICITY/C%ELLIPSE_PARAMETERS%ELLIPSE_MAJOR)**2
IF (EB2 .GT. 0.0) THEN
   C%ELLIPSE_PARAMETERS%ELLIPSE_MINOR = C%ELLIPSE_PARAMETERS%DIST_SIDEWIND/SQRT(EB2)
ELSE
   C%ELLIPSE_PARAMETERS%ELLIPSE_MINOR = 0.0
ENDIF

ELLIPSE_PROPERTY_MAP(C%IX, C%IY)%FOREST_FACTOR = C%ELLIPSE_PARAMETERS%FOREST_FACTOR

ELLIPSE_PROPERTY_MAP(C%IX, C%IY)%DIST_DOWNWIND = C%ELLIPSE_PARAMETERS%DIST_DOWNWIND
ELLIPSE_PROPERTY_MAP(C%IX, C%IY)%DIST_UPWIND = C%ELLIPSE_PARAMETERS%DIST_UPWIND
ELLIPSE_PROPERTY_MAP(C%IX, C%IY)%DIST_SIDEWIND = C%ELLIPSE_PARAMETERS%DIST_SIDEWIND

ELLIPSE_PROPERTY_MAP(C%IX, C%IY)%ELLIPSE_MAJOR = C%ELLIPSE_PARAMETERS%ELLIPSE_MAJOR
ELLIPSE_PROPERTY_MAP(C%IX, C%IY)%ELLIPSE_ECCENTRICITY = C%ELLIPSE_PARAMETERS%ELLIPSE_ECCENTRICITY
ELLIPSE_PROPERTY_MAP(C%IX, C%IY)%ELLIPSE_MINOR = C%ELLIPSE_PARAMETERS%ELLIPSE_MINOR

! *****************************************************************************
END SUBROUTINE ELLIPSE_UCB
! *****************************************************************************

! *****************************************************************************
SUBROUTINE HRR_TRANSIENT(BURNING_NODES, T)
! *****************************************************************************
! Evaluates the transient heat-release rate per unit area of a burning node at
! time T from its time-of-arrival, following the building design-fire curve
! (growth/full-development/decay) for FBFM91 cells or a residence-time pulse for
! vegetative cells. Updates HRR_TRANSIENT, FLIN_SURFACE, and HRR_TRANSIENT_MAP.

USE ELMFIRE_VARS
! HRR_TRANSIENT_MAP, BUILDING_FUEL_MODEL_TABLE

TYPE(NODE), POINTER, INTENT(INOUT) :: BURNING_NODES
REAL(8), INTENT(IN) :: T

INTEGER :: IX, IY
REAL ::  BURNING_TIME, TOA, HRR_PEAK, EARLY_TIME,  DEVELOPED_TIME, DECAY_TIME

INTEGER :: BLDG_FM
INTEGER, PARAMETER :: NO_DATA = -9999
REAL(8), PARAMETER :: FT_PER_MIN_TO_MPS = 0.00508

IX = BURNING_NODES%IX
IY = BURNING_NODES%IY

IF (PHIP(IX,IY) .GT. 0) RETURN

TOA = TIME_OF_ARRIVAL(IX,IY)
BURNING_TIME = T - TOA

! Pre-burned cells (initial ignition zone) : no HRR transient
IF (TOA .LE. SIMULATION_TSTART) THEN
   BURNING_NODES%HRR_TRANSIENT = 0.
   HRR_TRANSIENT_MAP(IX,IY)    = 0.
   RETURN
ENDIF

! This is to be modified. Maybe introduce a design fire curve for non-FBFM91 fuel.
IF (BURNING_NODES%IFBFM .NE. 91) THEN
   IF (BURNING_TIME .LT. ANALYSIS_CELLSIZE/MAX(1E-5, BURNING_NODES%VELOCITY*FT_PER_MIN_TO_MPS)) THEN
      BURNING_NODES%HRR_TRANSIENT = BURNING_NODES%HRRPUA
   ELSE
      BURNING_NODES%HRR_TRANSIENT = 0.
   ENDIF
   HRR_TRANSIENT_MAP(IX,IY) = BURNING_NODES%HRR_TRANSIENT
   RETURN
ENDIF

BLDG_FM = BURNING_NODES%IBLDGFM
IF (BLDG_FM .NE. NO_DATA) THEN
   EARLY_TIME = BUILDING_FUEL_MODEL_TABLE(BLDG_FM)%T_EARLY
   DEVELOPED_TIME = BUILDING_FUEL_MODEL_TABLE(BLDG_FM)%T_FULLDEV
   DECAY_TIME = BUILDING_FUEL_MODEL_TABLE(BLDG_FM)%T_DECAY
   HRR_PEAK = BUILDING_FUEL_MODEL_TABLE(BLDG_FM)%HRRPUA_PEAK

   IF (BURNING_TIME .LE. EARLY_TIME) THEN
      BURNING_NODES%HRR_TRANSIENT = (HRR_PEAK/ EARLY_TIME)*BURNING_TIME
   ELSEIF ((BURNING_TIME .GT. EARLY_TIME) .AND. (BURNING_TIME .LE. DEVELOPED_TIME)) THEN
      BURNING_NODES%HRR_TRANSIENT = HRR_PEAK
   ELSEIF (BURNING_TIME .GT. DECAY_TIME) THEN
      BURNING_NODES%HRR_TRANSIENT = 0.
      BURNING_NODES%BURNED = .TRUE.
   ELSE
      BURNING_NODES%HRR_TRANSIENT = (HRR_PEAK/(DEVELOPED_TIME - DECAY_TIME))*(BURNING_TIME - DECAY_TIME)
   ENDIF
ELSE
   BURNING_NODES%HRR_TRANSIENT = 0.0
ENDIF

BURNING_NODES%HRR_TRANSIENT = AMAX1(0.0, BURNING_NODES%HRR_TRANSIENT)
BURNING_NODES%FLIN_SURFACE = BURNING_NODES%HRR_TRANSIENT*ANALYSIS_CELLSIZE ! kW/m
HRR_TRANSIENT_MAP(IX,IY) = BURNING_NODES%HRR_TRANSIENT

! *****************************************************************************
END SUBROUTINE HRR_TRANSIENT
! *****************************************************************************

! *****************************************************************************
SUBROUTINE CALC_FUEL_CONSUMPTION(DT_ELMFIRE, NX, NY)
! *****************************************************************************
! Calculate the remaining fuel load at urban cells
USE ELMFIRE_VARS
! FUEL_LOAD_REMAIN, HRR_TRANSIENT_MAP, TRANSIENT_DFC_WUI, TRANSIENT_RADIATION_WUI, TIME_OF_ARRIVAL, FBFM, CRITICL_HF_WUI

REAL, INTENT(IN) :: DT_ELMFIRE
INTEGER, INTENT(IN) :: NX, NY

INTEGER :: IX, IY
REAL :: TOTAL_HEAT_FLUX, HRR_TRANSIENT

DO IY=1,NY 
DO IX=1,NX 
   IF (FBFM%I2(IX,IY,1) .NE. 91) CYCLE
   IF (TIME_OF_ARRIVAL(IX,IY) .LT. 0.) CYCLE
   ! CRITICL_HF_WUI is the critical heat flux below which the fuel is considered to extinguish. This value is subject to changes and should be calibrated with real fire data.
   TOTAL_HEAT_FLUX = TRANSIENT_DFC_WUI(IX,IY)+TRANSIENT_RADIATION_WUI(IX,IY)
   IF (TOTAL_HEAT_FLUX .LE. CRITICL_HF_WUI ) THEN
      HRR_TRANSIENT = 0.
   ELSE
      HRR_TRANSIENT = HRR_TRANSIENT_MAP(IX,IY)
   ENDIF
   FUEL_LOAD_REMAIN(IX,IY) = FUEL_LOAD_REMAIN(IX,IY) - DT_ELMFIRE * HRR_TRANSIENT
ENDDO 
ENDDO 

! *****************************************************************************
END SUBROUTINE CALC_FUEL_CONSUMPTION
! *****************************************************************************

#endif

! *****************************************************************************
PURE REAL FUNCTION CFFDRS_FW(WS)
! *****************************************************************************
! CFFDRS wind function FW (the ISI wind multiplier) as a function of wind speed WS.
REAL, INTENT(IN) :: WS
if (WS .le. 40) then
   CFFDRS_FW = exp(0.05039*WS)
else
   CFFDRS_FW = 12*(1-exp(-0.0818*(WS-28)))
endif
! *****************************************************************************
END FUNCTION CFFDRS_FW
! *****************************************************************************

END MODULE
