! *****************************************************************************
MODULE ELMFIRE_LEVEL_SET
! *****************************************************************************

USE ELMFIRE_SPOTTING
USE ELMFIRE_SPOTTING_SUPERSEDED
USE ELMFIRE_IO
USE ELMFIRE_SUBS
#ifdef _SUPPRESSION
USE ELMFIRE_SUPPRESSION
#endif
USE ELMFIRE_SPREAD_RATE
USE ELMFIRE_VARS

IMPLICIT NONE

CONTAINS

! *****************************************************************************
SUBROUTINE LEVEL_SET_PROPAGATION(IWX_BAND,ICASE,NTIMESTEPS, IS_VIRTUAL_RUN)
! *****************************************************************************
! Top-level driver for one fire simulation case: marches the level-set field
! PHIP forward in time over weather bands, igniting cells, computing spread
! rate, advancing the front (RK2), handling spotting, suppression, WUI/smoke,
! and writing outputs. Returns the number of timesteps taken in NTIMESTEPS.

! INTENT(IN) and INTENT(OUT) variables:
INTEGER, INTENT(IN) :: IWX_BAND, ICASE
INTEGER, INTENT(OUT) :: NTIMESTEPS
LOGICAL, INTENT(IN) :: IS_VIRTUAL_RUN

! Local variables & pointers:
INTEGER :: I, ILOC, J, IX, IY, ITIMESTEP, IX_IGN, IY_IGN, ISTEP, K, LU, IT1, IT2, &
           COUNT_START, COUNT_END, ICOUNT, IXSTART, IYSTART, IXSTOP, IYSTOP, IX2, IY2, &
           ITLO_METEOROLOGY, ITHI_METEOROLOGY, BINARY_OUTPUTS_SIZE, IT_EA, IXCEN, IYCEN, IOS, IPYROME, &
           N_TO_TAG, N_SPOT_FIRES, IT2_LSP, ICOL, IROW, YEAR , MONTH, DAY_OF_MONTH, HOUR, &
           BAND_L, BAND_H, IERR=0, DAY_OF_SIM, totalDuration,  BLDGFM, IDUMP_OUTPUT
INTEGER, SAVE :: NX, NY
INTEGER, POINTER, SAVE, DIMENSION(:) :: IX_TO_TAG, IY_TO_TAG, IX_SPOT_FIRE, IY_SPOT_FIRE
INTEGER, PARAMETER :: NO_DATA = -9999

REAL :: SURFACE_ACCELERATION_FACTOR, F_METEOROLOGY, R0, TAU, ACRES, ACRES_SDI, ELAPSED_TIME, E, FLIN_MAX, HECTARES, POC, SIMULATION_TSTOP_HOURS, &
        BURN_PERIOD_CENTER_HOUR, BURN_PERIOD_START_HOUR, BURN_PERIOD_STOP_HOUR, HOUR_OF_DAY, DT_DAY, TBURN, &
        T_LAST_SMOKE_OUTPUT, RUNTIME, &
        CELLTIME, WET_WOOD_CALORIFIC_VALUE, CELLENERGYRELEASE, PM_FLAMING_MAX, PM_SMOLDERING_MAX , LAT, LON, XCEN, YCEN, &
        DT_SPOTTING, FLIN, DT, NEXT_DUMP_TIME

REAL(8) :: TOTALENERGY, T, T_LAST_EXTENDED_ATTACK, T_LAST_INTERPOLATE_M1, T_LAST_INTERPOLATE_M10, T_LAST_INTERPOLATE_M100, &
        T_LAST_INTERPOLATE_MLH, T_LAST_INTERPOLATE_MLW, T_LAST_INTERPOLATE_FMC, T_LAST_INTERPOLATE_WIND, &
        T_LAST_WIND_FLUCTUATIONS

REAL, SAVE :: ACRES_PER_PIXEL, RCELLSIZE, HALFRCELLSIZE, TSTOP
REAL, ALLOCATABLE, SAVE, DIMENSION(:) :: X,Y
REAL, POINTER, DIMENSION(:,:), SAVE :: M1_LO, M1_HI, M10_LO, M10_HI, M100_LO, M100_HI, WS20_LO, WS20_HI, &
                                       WD20_LO, WD20_HI, MLH_LO, MLH_HI, MLW_LO, MLW_HI, FMC_LO, FMC_HI
REAL, POINTER, SAVE, DIMENSION(:,:,:) :: A_TIMES_BURNED

LOGICAL :: IA_HAS_OCCURRED, LOPEN, GO, CALL_SPOTTING, JUST_INTERPOLATED, DUMP_SMOKE_OUTPUTS, RUN, &
            INITIATED, START_CALCS, IS_FINAL_DUMP
LOGICAL, SAVE :: FIRSTCALL
LOGICAL, DIMENSION(1:100) :: ALREADY_IGNITED

real, allocatable, dimension(:,:) :: phi_previous

CHARACTER(4) :: FOUR_IWX_BAND, FOUR_IRANK_WORLD
CHARACTER(256) :: LOG_MSG
CHARACTER(7) :: SEVEN_ICASE
CHARACTER(16) :: TIMESTAMP
CHARACTER(400) :: FN

! parameters for checking if all simulations are finished
integer :: rank_finished , global_flag

TYPE(NODE), POINTER :: C => NULL(), DUMMY_NODE => NULL(), L_WUI_P => NULL()

! TYPE (FUEL_MODEL_TABLE_TYPE) :: FMT

BAND_L = MIN_IWX_BAND
BAND_H = min(WS%NBANDS, WX_BANDS_KEPT_IN_MEM + MIN_IWX_BAND - 1)
T=(MIN_IWX_BAND-1)*DT_METEOROLOGY
INITIATED = .FALSE.
START_CALCS = .FALSE.
DT = 1
TSTOP = SIMULATION_TSTOP + (IWX_BAND - 1) * DT_METEOROLOGY ! Default stop time for inactive/virtual ranks; real cases may override during initiation.
rank_finished = 0

if (FEEDBACK_LEVEL .ge. 1) then
   WRITE(LOG_MSG,'(A,I0,A,I0)') '[',ICASE,'] STARTING LEVEL SET PROPAGATION, WEATHER BAND START: ',IWX_BAND
   WRITE(*,'(A)') TRIM(LOG_MSG)
endif
if (IS_VIRTUAL_RUN) then
   WRITE(LOG_MSG,'(A,I0,A)') '[',ICASE,'] VIRTUAL RUN CASE'
   WRITE(*,'(A)') TRIM(LOG_MSG)
   rank_finished = 1
endif

CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
if (FEEDBACK_LEVEL .ge. 3) then
   WRITE(LOG_MSG,'(A,I0,A,I0,A,I0,A)') '[',ICASE,'] INITIATING WEATHER SLICE FROM [',BAND_L,', ',BAND_H,']'
   WRITE(*,'(A)') TRIM(LOG_MSG)
endif
CALL UPDATE_WEATHER_SLICE(BAND_L, BAND_H)
if (FEEDBACK_LEVEL .ge. 3) then
   WRITE(LOG_MSG,'(A,I0,A,I0,A,I0,A)') '[',ICASE,'] INITIATED WEATHER SLICE TO [',BAND_L,', ',BAND_H,']'
   WRITE(*,'(A)') TRIM(LOG_MSG)
endif
IF (MULTIPLE_HOSTS) CALL BCAST_WEATHER()
CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)

!MAIN DO LOOP, CONCURRENT FOR ALL THREADS

if (WS%NBANDS .eq. 1) then
   totalDuration = SIMULATION_TSTOP
else
   totalDuration = WS%NBANDS * DT_METEOROLOGY
endif

if (FEEDBACK_LEVEL .ge. 3) then
   WRITE(LOG_MSG,'(A,I0,A,F10.1,A,F10.1,A,F10.1)') '[',ICASE,'] PRIOR TO MAIN LOOP. T: ',T,', Total Duration: ',REAL(totalDuration),', Start: ',SIMULATION_TSTART+(IWX_BAND-1)*DT_METEOROLOGY
   WRITE(*,'(A)') TRIM(LOG_MSG)
endif

if (.not. allocated(phi_previous)) allocate(phi_previous(ANALYSIS_NCOLS, ANALYSIS_NROWS))
phi_previous(:,:) = 2

DO WHILE (T .le. totalDuration)
   ! if (ICASE .ge. 9945) then
   !    WRITE(LOG_MSG,'(A,I0,A,F12.1)') '[',ICASE,'] IN LEVEL SET LOOP, T IS ',T
   !    WRITE(*,'(A)') TRIM(LOG_MSG)
   ! endif
   DAY_OF_SIM = ceiling(((12 + mod(HOUR_OF_YEAR, 24) + IWX_BAND + floor(T/3600) - 1)/24.0))
   IF (T > BAND_H * DT_METEOROLOGY .and. WS%NBANDS .gt. 1) THEN ! LOAD NEXT WEATHER SLICE (unless weather is constant)
      if (FEEDBACK_LEVEL .ge. 3) then
         WRITE(LOG_MSG,'(A,I0,A,I0,A,I0,A)') '[',ICASE,'] UPDATING WEATHER SLICE FROM [',BAND_L,', ',BAND_H,']'
         WRITE(*,'(A)') TRIM(LOG_MSG)
      endif
      CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
      BAND_L = BAND_H 
      BAND_H = min(WS%NBANDS, BAND_H - 1 + WX_BANDS_KEPT_IN_MEM)
      CALL UPDATE_WEATHER_SLICE(BAND_L, BAND_H)
      if (FEEDBACK_LEVEL .ge. 3) then
         WRITE(LOG_MSG,'(A,I0,A,I0,A,I0,A)') '[',ICASE,'] UPDATED WEATHER SLICE TO [',BAND_L,', ',BAND_H,']'
         WRITE(*,'(A)') TRIM(LOG_MSG)
      endif
      IF (MULTIPLE_HOSTS) CALL BCAST_WEATHER()
      call MPI_Allreduce(rank_finished, global_flag, 1, MPI_INTEGER, MPI_MIN, MPI_COMM_WORLD, ierr)
      IF (global_flag .gt. 0) T = (WS%NBANDS+1)*DT_METEOROLOGY
   ENDIF
   ! if (ICASE .ge. 9945) then
   !    WRITE(LOG_MSG,'(A,I0,A,F12.1)') '[',ICASE,'] PASSED WEATHER SLICE CHECK, T IS ',T
   !    WRITE(*,'(A)') TRIM(LOG_MSG)
   ! endif
   IF (T .ge. SIMULATION_TSTART + (IWX_BAND - 1) * DT_METEOROLOGY .and. .not. INITIATED .and. .not. IS_VIRTUAL_RUN) THEN ! START SIM
      ! ***************************************************************************************
      if (FEEDBACK_LEVEL .ge. 3) then
         WRITE(LOG_MSG,'(A,I0,A,F12.1)') '[',ICASE,'] LEVEL SET CASE INITIATING AT T = ',T
         WRITE(*,'(A)') TRIM(LOG_MSG)
      endif
      CALL SYSTEM_CLOCK(COUNT_START, CLOCK_COUNT_RATE)
      IF (.NOT. ALLOCATED(PHIP)) FIRSTCALL = .TRUE.

      IF (RANDOMIZE_SIMULATION_TSTOP) THEN
         CALL RANDOM_NUMBER(R0)
         TSTOP = SIMULATION_TSTART + R0 * (SIMULATION_TSTOP - SIMULATION_TSTART) + (IWX_BAND - 1) * DT_METEOROLOGY
      ELSE
         TSTOP = SIMULATION_TSTOP + (IWX_BAND - 1) * DT_METEOROLOGY
      ENDIF

      IF (CSV_FIXED_IGNITION_LOCATIONS) THEN
         IF (STATS_TSTOP(ICASE) .GT. 0.) TSTOP = 3600. * STATS_TSTOP(ICASE) + (IWX_BAND - 1) * DT_METEOROLOGY
      ENDIF

      SIMULATION_TSTOP_HOURS = -1.
      STATS_SIMULATION_TSTOP_HOURS(ICASE) = SIMULATION_TSTOP_HOURS
      NEXT_DUMP_TIME = SIMULATION_TSTART + (IWX_BAND - 1) * DT_METEOROLOGY + DTDUMP
      IDUMP_OUTPUT = 0

      IF (FIRSTCALL) THEN
         FIRSTCALL      = .FALSE.
         RCELLSIZE       = 1. / ANALYSIS_CELLSIZE
         HALFRCELLSIZE   = 0.5 * RCELLSIZE
         ACRES_PER_PIXEL = 2.47105E-4 * ANALYSIS_CELLSIZE*ANALYSIS_CELLSIZE
         NX              = ANALYSIS_NCOLS
         NY              = ANALYSIS_NROWS

         ALLOCATE(X(1:NX))
         ALLOCATE(Y(1:NY))
         ALLOCATE(IX_TO_TAG   (1:100000))
         ALLOCATE(IY_TO_TAG   (1:100000))
         IF (.NOT. USE_SUPERSEDED_SPOTTING) THEN
            ALLOCATE(IX_SPOT_FIRE(1:NX*NY))
            ALLOCATE(IY_SPOT_FIRE(1:NX*NY))
         ELSE
            ALLOCATE(IX_SPOT_FIRE(1:100000))
            ALLOCATE(IY_SPOT_FIRE(1:100000))
         ENDIF
         
         X(1) = ANALYSIS_XLLCORNER + 0.5 * ANALYSIS_CELLSIZE
         DO IX = 2, NX
            X(IX) = X(IX-1) + ANALYSIS_CELLSIZE
         ENDDO

         Y(1) = ANALYSIS_YLLCORNER + 0.5 * ANALYSIS_CELLSIZE
         DO IY = 2, NY
            Y(IY) = Y(IY-1) + ANALYSIS_CELLSIZE
         ENDDO
         ALLOCATE(TIME_OF_ARRIVAL (1:NX,1:NY)); TIME_OF_ARRIVAL(:,:) = -1.
         ALLOCATE(TAGGED          (1:NX,1:NY)); TAGGED(:,:) = .FALSE.
         ALLOCATE(PHIP            (1:NX,1:NY)); PHIP(:,:) = 1
         ! new suppression model
         ALLOCATE(PCL_HOLD_PROB            (1:NX,1:NY)); PCL_HOLD_PROB(:,:) = 0.0
         ! new suppression model
         ALLOCATE(EVERTAGGED      (1:NX,1:NY)); EVERTAGGED(:,:) = .FALSE.
         ALLOCATE(EVERTAGGED_IX   (1:NX*NY))
         ALLOCATE(EVERTAGGED_IY   (1:NX*NY))

         ! Allocate HRR_TRANSIENT_MAP if dumping of transient HRRPUA is enabled, or if WUI spread model is enabled (since it requires transient HRRPUA for all cells, not just burning cells)
         IF (DUMP_HRR_TRANSIENT .OR. USE_BLDG_SPREAD_MODEL) THEN
            ALLOCATE(HRR_TRANSIENT_MAP(1:NX,1:NY))
            HRR_TRANSIENT_MAP(:,:) = 0.
         ENDIF
#ifdef _WUI
         ! Allocate additional arrays for WU-E calculation and outputs
         IF (USE_BLDG_SPREAD_MODEL) THEN
         IF (BLDG_SPREAD_MODEL_TYPE .EQ. 2) THEN
            ALLOCATE(TOTAL_DFC_WUI          (1:NX,1:NY)); TOTAL_DFC_WUI(:,:) = 0
            ALLOCATE(TOTAL_RADIATION_WUI    (1:NX,1:NY)); TOTAL_RADIATION_WUI(:,:) = 0
            ALLOCATE(TRANSIENT_DFC_WUI      (1:NX,1:NY)); TRANSIENT_DFC_WUI(:,:) = 0
            ALLOCATE(TRANSIENT_RADIATION_WUI(1:NX,1:NY)); TRANSIENT_RADIATION_WUI(:,:) = 0
            ALLOCATE(FUEL_LOAD_REMAIN       (1:NX,1:NY)); FUEL_LOAD_REMAIN(:,:) = 0
            DO IY=1,NY
            DO IX=1,NX
               IF (FBFM%I2(IX,IY,1) .EQ. 91) THEN
                  BLDGFM=BLDG_FUEL_MODEL%I2(IX,IY,1)
                  IF(BLDGFM .EQ. NO_DATA) BLDGFM=1
                  FUEL_LOAD_REMAIN(IX,IY) = BUILDING_FUEL_MODEL_TABLE(BLDGFM)%FUEL_LOAD
               ENDIF
            ENDDO
            ENDDO
            ALLOCATE(TAGGED_WUI    (1:NX,1:NY)); TAGGED_WUI(:,:) = .FALSE.
            ALLOCATE(EVERTAGGED_WUI(1:NX,1:NY)); EVERTAGGED_WUI(:,:) = .FALSE.
            ALLOCATE(TEST_INTERFACE_WUI(1:NX,1:NY)); TEST_INTERFACE_WUI(:,:) = .FALSE.
            ALLOCATE(WTU_SPREAD_WUI(1:NX,1:NY)); WTU_SPREAD_WUI(:,:) = .FALSE.

            ALLOCATE(ELLIPSE_PROPERTY_MAP(1:NX,1:NY))
         ENDIF
         ENDIF
#endif

         IF (.NOT. USE_SUPERSEDED_SPOTTING .AND. trim(ACCUMULATION_MODEL).EQ. 'EULERIAN') THEN
            ALLOCATE(EMBER_TOA(1:NX,1:NY)); EMBER_TOA(:,:) = -1.
         ENDIF

         IF (DUMP_BINARY_OUTPUTS) THEN
            BINARY_OUTPUTS_SIZE = NX*NY
            ALLOCATE(BINARY_OUTPUTS_IX           (1:BINARY_OUTPUTS_SIZE) )
            ALLOCATE(BINARY_OUTPUTS_IY           (1:BINARY_OUTPUTS_SIZE) )
            ALLOCATE(BINARY_OUTPUTS_TOA          (1:BINARY_OUTPUTS_SIZE) )
            ALLOCATE(BINARY_OUTPUTS_FLAME_LENGTH (1:BINARY_OUTPUTS_SIZE) )
            ALLOCATE(BINARY_OUTPUTS_VELOCITY_FPM (1:BINARY_OUTPUTS_SIZE) )
            ALLOCATE(BINARY_OUTPUTS_CROWN_FIRE   (1:BINARY_OUTPUTS_SIZE) )
         ENDIF

         IF (USE_EMBER_COUNT_BINS) THEN
            ALLOCATE(EMBER_COUNT(1:NX,1:NY))
            EMBER_COUNT(:,:) = 0
         ENDIF

         ! Point pointers to analysis rasters:
         A_TIMES_BURNED => ANALYSIS_TIMES_BURNED%R4 (:,:,:)
         SURFACE_FIRE   => ANALYSIS_SURFACE_FIRE%I2(:,:,1); SURFACE_FIRE(:,:) = 0.
         
         IF (ENABLE_SPOTTING) THEN
            IF (DUMP_EMBER_FLUX_TRANSIENT) EMBER_FLUX_TRANSIENT%R4(:,:,1) = 0
            IF (DUMP_EMBER_FLUX .OR. (.NOT. USE_SUPERSEDED_SPOTTING .AND. trim(IGNITION_MODEL) .NE. 'DIRECT')) THEN
               EMBER_FLUX%R4(:,:,1) = 0.
            ENDIF
         ENDIF

         WRITE(FOUR_IRANK_WORLD, '(I4.4)') IRANK_WORLD
         IF (NUM_TIME_AT_BURNED_ACRES .GT. 0) ALLOCATE(ALREADY_REACHED_BURNED_ACRES(1:NUM_TIME_AT_BURNED_ACRES))
            
         IF (PROCESS_TIMED_LOCATIONS) THEN
            DO I = 1, NUM_TIMED_LOCATIONS
               TIMED_LOCATIONS_TRACKER(I)%IX = ICOL_FROM_X(TIMED_LOCATIONS_TRACKER(I)%X,ANALYSIS_XLLCORNER,ANALYSIS_CELLSIZE)
               TIMED_LOCATIONS_TRACKER(I)%IY = IROW_FROM_Y(TIMED_LOCATIONS_TRACKER(I)%Y,ANALYSIS_YLLCORNER,ANALYSIS_CELLSIZE)
            ENDDO
         ENDIF

         IF (NUM_VIRTUAL_STATIONS .GT. 0) THEN
            LIST_VIRTUAL_STATIONS = NEW_DLL()
            DO I = 1, NUM_VIRTUAL_STATIONS
               VIRTUAL_STATION_IX(I) = ICOL_FROM_X(VIRTUAL_STATION_X(I),ANALYSIS_XLLCORNER,ANALYSIS_CELLSIZE)
               VIRTUAL_STATION_IY(I) = IROW_FROM_Y(VIRTUAL_STATION_Y(I),ANALYSIS_YLLCORNER,ANALYSIS_CELLSIZE)
               CALL APPEND(LIST_VIRTUAL_STATIONS,VIRTUAL_STATION_IX(I),VIRTUAL_STATION_IY(I),0.0_8)
            ENDDO
         ENDIF

      ENDIF !FIRSTCALL

      CALL ACCUMULATE_CPU_USAGE(30, IT1, IT2)

      WRITE(FOUR_IWX_BAND, '(I4.4)') IWX_BAND
      WRITE(SEVEN_ICASE  , '(I7.7)') ICASE

      !T=SIMULATION_TSTART ! should already be that
      IF (RANDOM_IGNITIONS) THEN
         IX_IGN = ICOL_FROM_X(STATS_X(ICASE),ANALYSIS_XLLCORNER,ANALYSIS_CELLSIZE)
         IY_IGN = IROW_FROM_Y(STATS_Y(ICASE),ANALYSIS_YLLCORNER,ANALYSIS_CELLSIZE)
         IF (USE_PYROMES .AND. CALIBRATION_CONSTANTS_BY_PYROME) THEN
            IPYROME = MIN(MAX(PYROMES%I2(IX_IGN,IY_IGN,1),1),128)
            IF (CALIBRATION_CONSTANTS_BY_PYROME) THEN
               INITIAL_ATTACK_TIME        = INITIAL_ATTACK_TIME_PYROME(IPYROME)
               B_SDI                      = B_SDI_PYROME(IPYROME)
               MAX_CONTAINMENT_PER_DAY    = MAX_CONTAINMENT_PER_DAY_PYROME(IPYROME)
               AREA_NO_CONTAINMENT_CHANGE = AREA_NO_CONTAINMENT_CHANGE_PYROME(IPYROME)

               IF (RANDOMIZE_SIMULATION_TSTOP) THEN
                  CALL RANDOM_NUMBER(R0)
                  TSTOP = SIMULATION_TSTART + R0 * (SIMULATION_DURATION_PYROME(IPYROME) - SIMULATION_TSTART) + (IWX_BAND-1)*DT_METEOROLOGY
               ELSE
                  TSTOP = SIMULATION_DURATION_PYROME(IPYROME)+ (IWX_BAND-1)*DT_METEOROLOGY
               ENDIF
            ENDIF
            IF (ADJUSTMENT_FACTORS_BY_PYROME) CROWN_FIRE_ADJ = ADJ_PYROME(IPYROME,0)
         ENDIF

         IF (USE_PYROMES .AND. DURATION_PDF_BY_PYROME) THEN
            IPYROME = MIN(MAX(PYROMES%I2(IX_IGN,IY_IGN,1),1),128)
            CALL RANDOM_NUMBER(R0)
            CALL LOCATE(DURATION_CDF_PYROME(IPYROME,:), DURATION_MAX_DAYS, R0, ILOC)
            ILOC = MAX(MIN(ILOC + 1, DURATION_MAX_DAYS),1)
            TSTOP = 86400.0 * REAL(ILOC)+ (IWX_BAND-1)*DT_METEOROLOGY
         ENDIF

         IF (ISNONBURNABLE(IX_IGN,IY_IGN)) THEN
            STATS_SURFACE_FIRE_AREA          (ICASE) = 0. 
            STATS_CROWN_FIRE_AREA            (ICASE) = 0. 
            STATS_FIRE_VOLUME                (ICASE) = 0.
            STATS_AFFECTED_POPULATION        (ICASE) = 0.
            STATS_AFFECTED_REAL_ESTATE_VALUE (ICASE) = 0.
            STATS_AFFECTED_LAND_VALUE        (ICASE) = 0.
            STATS_FINAL_CONTAINMENT_FRAC     (ICASE) = 0.
            STATS_NEMBERS                    (ICASE) = 0.
            STATS_SIMULATION_TSTOP_HOURS     (ICASE) = -9999.
            STATS_PM2P5_RELEASE              (ICASE) = 0.
            STATS_HRR_PEAK                   (ICASE) = 0.
            WRITE(LOG_MSG,'(A,I0,A)') '[',ICASE,']: IGNITION CELL IS NONBURNABLE, STOPPING'
            WRITE(*,'(A)') TRIM(LOG_MSG)
            rank_finished = 1
            DT = DT_METEOROLOGY
         ENDIF

         if (POINT_WIND_TO_CENTER) then
            XCEN = REAL(ASP%NCOLS) / 2.0
            YCEN = REAL(ASP%NROWS) / 2.0
            WD_TO_CENTER = atan2d(XCEN - IX_IGN, YCEN - IY_IGN)
            WD_TO_CENTER = MODULO(WD_TO_CENTER + 180.0, 360.0)
         endif

      ENDIF

      ! Check for errant IWX_BAND
      RUN = .TRUE.

      IF (IWX_BAND .LT. 1) THEN
         WRITE(*,*) 'CYCLING BECAUSE IWX_BAND .LT. 1: ', IWX_BAND
         RUN = .FALSE.
      ENDIF 
               
      IF (IWX_BAND - IWX_BAND_OFFSET + NUM_METEOROLOGY_TIMES - 1 .GT. WS%NBANDS) THEN
         WRITE(*,*) 'CYCLING BECAUSE IWX_BAND - IWX_BAND_OFFSET + NUM_METEOROLOGY_TIMES - 1 .GT. WS%NBANDS: ', IWX_BAND
         RUN = .FALSE.
      ENDIF 

      IF (.NOT. RUN) THEN
         STATS_SURFACE_FIRE_AREA          (ICASE) = 0. 
         STATS_CROWN_FIRE_AREA            (ICASE) = 0. 
         STATS_FIRE_VOLUME                (ICASE) = 0.
         STATS_AFFECTED_POPULATION        (ICASE) = 0.
         STATS_AFFECTED_REAL_ESTATE_VALUE (ICASE) = 0.
         STATS_AFFECTED_LAND_VALUE        (ICASE) = 0.
         STATS_FINAL_CONTAINMENT_FRAC     (ICASE) = 0.
         STATS_NEMBERS                    (ICASE) = 0.
         STATS_SIMULATION_TSTOP_HOURS     (ICASE) = -9999.
         STATS_PM2P5_RELEASE              (ICASE) = 0.
         STATS_HRR_PEAK                   (ICASE) = 0.
         TSTOP = SIMULATION_TSTART + 0.01
      ENDIF

      CALL ACCUMULATE_CPU_USAGE(31, IT1, IT2)

      ! Initialize viariables on each new call:
      ITIMESTEP                   = 0
      T_LAST_EXTENDED_ATTACK      = -9E9
      T_LAST_INTERPOLATE_M1       = -9E9
      T_LAST_INTERPOLATE_M10      = -9E9
      T_LAST_INTERPOLATE_M100     = -9E9
      T_LAST_INTERPOLATE_MLH      = -9E9
      T_LAST_INTERPOLATE_MLW      = -9E9
      T_LAST_INTERPOLATE_FMC      = -9E9
      T_LAST_INTERPOLATE_WIND     = -9E9
      T_LAST_WIND_FLUCTUATIONS    = -9E9

      T_LAST_SMOKE_OUTPUT         = 0 !-9E9 breaks first check (T-T_LAST_SMOKE_OUTPUT)

      DT                          = SIMULATION_DT
      SURFACE_ACCELERATION_FACTOR =  1.
      ACRES                       = 0.
      ACRES_SDI                   = 0.

      LIST_TAGGED                 = NEW_DLL(); LIST_TAGGED%NUM_NODES=0
      LIST_SUPPRESSION_BLOCK      = NEW_DLL(); LIST_SUPPRESSION_BLOCK%NUM_NODES=0
      LIST_BURNED                 = NEW_DLL(); LIST_BURNED%NUM_NODES=0
      LIST_SUPPRESSED             = NEW_DLL(); LIST_SUPPRESSED%NUM_NODES=0
      LIST_WUI_BURNING            = NEW_DLL(); LIST_WUI_BURNING%NUM_NODES=0
      LIST_EMBER_DEPOSITED        = NEW_DLL(); LIST_EMBER_DEPOSITED%NUM_NODES=0 ! linked list for ember deposited cells
      NUM_EVERTAGGED              = 0

      IA_HAS_OCCURRED             = .FALSE.
      ALREADY_IGNITED(:)          = .FALSE.

      NUM_TRACKED_EMBERS          = 0 ! Only used in the Lagrangian spotting model, but initialize here to be safe

      CALL ACCUMULATE_CPU_USAGE(32, IT1, IT2)

#ifdef _SUPPRESSION
      IF (ENABLE_EXTENDED_ATTACK) THEN
         ! new suppression model :: modified below
         IF (EXTENDED_ATTACK_MODEL .EQ. 0) THEN
            DO IT_EA = 0, 1000
               SUPP(IT_EA)%NCELLS(:)=0
               SUPP(IT_EA)%VELOCITY(:)=0.
               SUPP(IT_EA)%VELOCITY_SMOOTHED(:)=0.
               SUPP(IT_EA)%FIRELINE_FRACTION(:)=0.
               SUPP(IT_EA)%SUPPRESSED_FRACTION(:)=0.
               SUPP(IT_EA)%T=0.
               SUPP(IT_EA)%ACRES=0.
               SUPP(IT_EA)%ACRES_SDI=0.
               SUPP(IT_EA)%TARGET_CONTAINMENT=0.
               SUPP(IT_EA)%DC_PER_DAY=0.
               SUPP(IT_EA)%DADT=0.
               SUPP(IT_EA)%DASDIDT=0.
               SUPP(IT_EA)%SDIBAR=0.
               SUPP(IT_EA)%IXCEN=0
               SUPP(IT_EA)%IYCEN=0

               SUPP(IT_EA)%FIRE_LINE_LENGTH=0.
               SUPP(IT_EA)%SUPPRESSED_FIRELINE_LENGTH=0.
               SUPP(IT_EA)%INDIRECT_SUPPRESSED_FIRELINE_LENGTH = 0.
               SUPP(IT_EA)%CONTAINMENT=0.
            ENDDO
         ELSE IF (EXTENDED_ATTACK_MODEL .EQ. 1) THEN

            IF (ENABLE_INDIRECT_ATTACK) CALL CALCULATE_PCL_HOLD_MAP(NX, NY)

            DO IY = 1, NY
               DO IX = 1, NX

                  IF (PCL_HOLD_PROB(IX,IY) .NE. 0.0) THEN
                     CALL APPEND(LIST_SUPPRESSION_BLOCK, IX, IY, T)
                     LIST_SUPPRESSION_BLOCK%TAIL%TIME_SUPPRESSED = PCL_HOLD_PROB(IX,IY)
                  ENDIF

               ENDDO
            ENDDO

            CALL LL_DUMP_ROUTINE(LIST_SUPPRESSION_BLOCK,"indirect_suppression_block", T,"time_suppressed",1)


            DO IT_EA = 0, 1000
               SUPP(IT_EA)%NCELLS(:)=0
               SUPP(IT_EA)%VELOCITY(:)=0.
               SUPP(IT_EA)%VELOCITY_SMOOTHED(:)=0.
               SUPP(IT_EA)%FIRELINE_FRACTION(:)=0.
               SUPP(IT_EA)%SUPPRESSED_FRACTION(:)=0.
               SUPP(IT_EA)%T=0.
               SUPP(IT_EA)%ACRES=0.
               SUPP(IT_EA)%ACRES_SDI=0.
               SUPP(IT_EA)%TARGET_CONTAINMENT=0.
               SUPP(IT_EA)%DC_PER_DAY=0.
               SUPP(IT_EA)%DADT=0.
               SUPP(IT_EA)%DASDIDT=0.
               SUPP(IT_EA)%SDIBAR=0.
               SUPP(IT_EA)%IXCEN=0
               SUPP(IT_EA)%IYCEN=0

               SUPP(IT_EA)%FIRE_LINE_LENGTH=0.
               SUPP(IT_EA)%SUPPRESSED_FIRELINE_LENGTH=0.
               SUPP(IT_EA)%INDIRECT_SUPPRESSED_FIRELINE_LENGTH = 0.
               SUPP(IT_EA)%CONTAINMENT=0.
            ENDDO
         ELSE
            WRITE(*,*) 'Error: "EXTENDED_ATTACK_MODEL" should be 0 or 1 in namelist!'
            STOP
         ENDIF
      ENDIF
#endif
      IT_EA=0

      CALL ACCUMULATE_CPU_USAGE(33, IT1, IT2)

      ! IF (DUMP_EMBER_FLUX .AND. (.NOT. ACCUMULATE_EMBER_FLUX) ) EMBER_FLUX%R4(:,:,1) = 0

      IF (USE_BARRIERS) BANDTHICKNESS = 1
      ! new suppression model
      FIRE_LINE_THICKNESS = MIN(REAL(BANDTHICKNESS), FIRE_LINE_THICKNESS)  
      ! new suppression model

      ! Tag bands where initial phi values are less than 0:
      IF (.NOT. RANDOM_IGNITIONS) THEN
         PHIP(:,:) = PHI0%R4(:,:,1)

         ! Get initial wind information for nodes in LIST_BURNED, for spotting model
         ITLO_METEOROLOGY = IWX_BAND_START - BAND_L + 1
         ITHI_METEOROLOGY = IWX_BAND_START - BAND_L + 1
         F_METEOROLOGY = 1.
         
         M1_LO   => M1%R4   (:,:,ITLO_METEOROLOGY)
         M1_HI   => M1%R4   (:,:,ITHI_METEOROLOGY)
         M10_LO  => M10%R4  (:,:,ITLO_METEOROLOGY)
         M10_HI  => M10%R4  (:,:,ITHI_METEOROLOGY)
         M100_LO => M100%R4 (:,:,ITLO_METEOROLOGY)
         M100_HI => M100%R4 (:,:,ITHI_METEOROLOGY)
         WS20_LO => WS%R4   (:,:,ITLO_METEOROLOGY)
         WS20_HI => WS%R4   (:,:,ITHI_METEOROLOGY)
         WD20_LO => WD%R4   (:,:,ITLO_METEOROLOGY)
         WD20_HI => WD%R4   (:,:,ITHI_METEOROLOGY)
         MLH_LO  => MLH%R4  (:,:,ITLO_METEOROLOGY)
         MLH_HI  => MLH%R4  (:,:,ITHI_METEOROLOGY)
         MLW_LO  => MLW%R4  (:,:,ITLO_METEOROLOGY)
         MLW_HI  => MLW%R4  (:,:,ITHI_METEOROLOGY)
         FMC_LO  => MFOL%R4 (:,:,ITLO_METEOROLOGY)
         FMC_HI  => MFOL%R4 (:,:,ITHI_METEOROLOGY)

         ICOUNT=0
         DO IY = 1, NY
         DO IX = 1, NX
            IF (PHIP(IX,IY) .LE. 0.) THEN
               SURFACE_FIRE(IX,IY) = 1
               ACRES = ACRES + ACRES_PER_PIXEL
               TIME_OF_ARRIVAL(IX,IY) = MAX(0.,SIMULATION_TSTART)
               ICOUNT = ICOUNT + 1

               CALL APPEND(LIST_BURNED, IX, IY, T)
               
               IF (WX_BILINEAR_INTERPOLATION) THEN
                  CALL INTERP_RASTER_LINKEDLIST_SINGLE_BILINEAR (LIST_BURNED%TAIL, M1_LO  (:,:), M1_HI  (:,:), F_METEOROLOGY, 1)
                  CALL INTERP_RASTER_LINKEDLIST_SINGLE_BILINEAR (LIST_BURNED%TAIL, M10_LO (:,:), M10_HI (:,:), F_METEOROLOGY, 2)
                  CALL INTERP_RASTER_LINKEDLIST_SINGLE_BILINEAR (LIST_BURNED%TAIL, M100_LO(:,:), M100_HI(:,:), F_METEOROLOGY, 3)
                  CALL INTERP_RASTER_LINKEDLIST_SINGLE_BILINEAR (LIST_BURNED%TAIL, MLH_LO (:,:), MLH_HI (:,:), F_METEOROLOGY, 4)
                  CALL INTERP_RASTER_LINKEDLIST_SINGLE_BILINEAR (LIST_BURNED%TAIL, MLW_LO (:,:), MLW_HI (:,:), F_METEOROLOGY, 5)
                  CALL INTERP_RASTER_LINKEDLIST_SINGLE_BILINEAR (LIST_BURNED%TAIL, FMC_LO (:,:), FMC_HI (:,:), F_METEOROLOGY, 6)
                  CALL INTERP_RASTER_LINKEDLIST_SINGLE_BILINEAR (LIST_BURNED%TAIL, WS20_LO(:,:), WS20_HI(:,:), F_METEOROLOGY, 7)
               ELSE
                  CALL INTERP_RASTER_LINKEDLIST_SINGLE (LIST_BURNED%TAIL, M1_LO  (:,:), M1_HI  (:,:), F_METEOROLOGY, 1)
                  CALL INTERP_RASTER_LINKEDLIST_SINGLE (LIST_BURNED%TAIL, M10_LO (:,:), M10_HI (:,:), F_METEOROLOGY, 2)
                  CALL INTERP_RASTER_LINKEDLIST_SINGLE (LIST_BURNED%TAIL, M100_LO(:,:), M100_HI(:,:), F_METEOROLOGY, 3)
                  CALL INTERP_RASTER_LINKEDLIST_SINGLE (LIST_BURNED%TAIL, MLH_LO (:,:), MLH_HI (:,:), F_METEOROLOGY, 4)
                  CALL INTERP_RASTER_LINKEDLIST_SINGLE (LIST_BURNED%TAIL, MLW_LO (:,:), MLW_HI (:,:), F_METEOROLOGY, 5)
                  CALL INTERP_RASTER_LINKEDLIST_SINGLE (LIST_BURNED%TAIL, FMC_LO (:,:), FMC_HI (:,:), F_METEOROLOGY, 6)
                  CALL INTERP_RASTER_LINKEDLIST_SINGLE (LIST_BURNED%TAIL, WS20_LO(:,:), WS20_HI(:,:), F_METEOROLOGY, 7)
               ENDIF
               
               CALL UPDATE_WD_RASTER_SINGLE(LIST_BURNED%TAIL, WD20_LO(:,:), WD20_HI(:,:), F_METEOROLOGY)

               ICOL = ICOL_ANALYSIS_F2C(IX)
               IROW = IROW_ANALYSIS_F2C(IY)
               LIST_BURNED%TAIL%TIME_OF_ARRIVAL        = T
               LIST_BURNED%TAIL%WS20_NOW               = WS20_LO(ICOL,IROW) * (1. - F_METEOROLOGY) + F_METEOROLOGY * WS20_HI(ICOL,IROW)
               LIST_BURNED%TAIL%BURNED                 = .FALSE.
#ifdef _WUI
               IF (USE_BLDG_SPREAD_MODEL) THEN
                  
                  IF(BLDG_FUEL_MODEL%I2(IX,IY,1) .NE. NO_DATA) THEN
                     LIST_BURNED%TAIL%IBLDGFM =  BLDG_FUEL_MODEL%I2(IX,IY,1)
                  ELSE
                     LIST_BURNED%TAIL%IBLDGFM =  NO_DATA
                  ENDIF
                  ! Tagged WUI cells, for use in the refactored WUI spread model
                  IF(BLDG_SPREAD_MODEL_TYPE .EQ. 2) CALL TAG_WUI(NX, NY, IX, IY, T)
               ELSE
                  LIST_BURNED%TAIL%IBLDGFM = NO_DATA
               ENDIF
#endif

#ifdef _SUPPRESSION
         ! new suppression model :: modified below
         IF (ENABLE_EXTENDED_ATTACK) THEN
            IF (EXTENDED_ATTACK_MODEL .EQ. 0) THEN
               IF (USE_SDI) C%SDI = SDI_FACTOR * SDI%R4(ICOL,IROW,1)
            ELSE IF (EXTENDED_ATTACK_MODEL .EQ. 1) THEN
               C%SDI = SDI%R4(ICOL,IROW,1)
               C%PCL = PCL%R4(ICOL,IROW,1)
            ELSE
               WRITE(*,*) 'Error: "EXTENDED_ATTACK_MODEL" should be 0 or 1 in namelist!'
               STOP
            ENDIF
         ENDIF
#endif
            ENDIF
         ENDDO
         ENDDO

         ! Call relevant functions, assign values to FLIN_SURFACE and FLIN_CANOPY
         continue
         IF (ASSOCIATED(C)) DEALLOCATE(C)
            continue
         if (trim(SURFACE_SPREAD_MODEL) .eq. "ROTHERMEL") then
            CALL ROTHERMEL_SURFACE_SPREAD_RATE(LIST_BURNED, C)
         else if (trim(SURFACE_SPREAD_MODEL) .eq. "CFFDRS") then
            CALL CFFDRS_SPREAD_RATE(LIST_BURNED, C, daily_bui(DAY_OF_SIM))
         ENDIF

         DO ISTEP=1,2
            ! Calcaulate components of normal vector
            CALL CALC_NORMAL_VECTORS (ISTEP, HALFRCELLSIZE)

            ! Calculate x and y components of velocity from elliptical spread dimensions
            CALL UX_AND_UY_ELLIPTICAL(LIST_BURNED, 1.0, ISTEP, DT)
            
            !Apply canopy fire and other parts that depend on directional ROS (instead of max head ros)
            call UPDATE_LOCAL_SPREAD_PROPERTIES(LIST_BURNED, C)
         ENDDO
         
         C => LIST_BURNED%HEAD
         DO I = 1, LIST_BURNED%NUM_NODES
            C%BURNED = .TRUE.
            C => C%NEXT
         ENDDO

#ifdef _WUI
         ! Initialize the linked list for WUI, following added for the refactored WUI spread model 
         IF (USE_BLDG_SPREAD_MODEL .AND. (BLDG_SPREAD_MODEL_TYPE .EQ. 2) .AND. (LIST_WUI_BURNING%NUM_NODES .GT. 0)) THEN
            IF (WX_BILINEAR_INTERPOLATION) THEN
               CALL INTERP_RASTER_LINKEDLIST_BILINEAR (LIST_WUI_BURNING, M1_LO  (:,:), M1_HI  (:,:), F_METEOROLOGY, 1)
               CALL INTERP_RASTER_LINKEDLIST_BILINEAR (LIST_WUI_BURNING, M10_LO (:,:), M10_HI (:,:), F_METEOROLOGY, 2)
               CALL INTERP_RASTER_LINKEDLIST_BILINEAR (LIST_WUI_BURNING, M100_LO(:,:), M100_HI(:,:), F_METEOROLOGY, 3)
               CALL INTERP_RASTER_LINKEDLIST_BILINEAR (LIST_WUI_BURNING, MLH_LO (:,:), MLH_HI (:,:), F_METEOROLOGY, 4)
               CALL INTERP_RASTER_LINKEDLIST_BILINEAR (LIST_WUI_BURNING, MLW_LO (:,:), MLW_HI (:,:), F_METEOROLOGY, 5)
               CALL INTERP_RASTER_LINKEDLIST_BILINEAR (LIST_WUI_BURNING, FMC_LO (:,:), FMC_HI (:,:), F_METEOROLOGY, 6)
               CALL INTERP_RASTER_LINKEDLIST_BILINEAR (LIST_WUI_BURNING, WS20_LO(:,:), WS20_HI(:,:), F_METEOROLOGY, 7)
            ELSE
               CALL INTERP_RASTER_LINKEDLIST (LIST_WUI_BURNING, M1_LO  (:,:), M1_HI  (:,:), F_METEOROLOGY, 1)
               CALL INTERP_RASTER_LINKEDLIST (LIST_WUI_BURNING, M10_LO (:,:), M10_HI (:,:), F_METEOROLOGY, 2)
               CALL INTERP_RASTER_LINKEDLIST (LIST_WUI_BURNING, M100_LO(:,:), M100_HI(:,:), F_METEOROLOGY, 3)
               CALL INTERP_RASTER_LINKEDLIST (LIST_WUI_BURNING, MLH_LO (:,:), MLH_HI (:,:), F_METEOROLOGY, 4)
               CALL INTERP_RASTER_LINKEDLIST (LIST_WUI_BURNING, MLW_LO (:,:), MLW_HI (:,:), F_METEOROLOGY, 5)
               CALL INTERP_RASTER_LINKEDLIST (LIST_WUI_BURNING, FMC_LO (:,:), FMC_HI (:,:), F_METEOROLOGY, 6)
               CALL INTERP_RASTER_LINKEDLIST (LIST_WUI_BURNING, WS20_LO(:,:), WS20_HI(:,:), F_METEOROLOGY, 7)
            ENDIF
            
            CALL UPDATE_WD_RASTER(LIST_WUI_BURNING, WD20_LO(:,:), WD20_HI(:,:), F_METEOROLOGY)

            L_WUI_P => LIST_WUI_BURNING%HEAD
            DO I = 1, LIST_WUI_BURNING%NUM_NODES
               IX = L_WUI_P%IX
               IY = L_WUI_P%IY
               CALL ELLIPSE_UCB(L_WUI_P)
               ! Skip non-burning pixels
               IF (PHIP(IX,IY) .GT. 0.) THEN
                  L_WUI_P => L_WUI_P%NEXT
                  CYCLE
               ELSE
                  L_WUI_P%TIME_OF_ARRIVAL=TIME_OF_ARRIVAL(IX,IY)
               ENDIF
               ! Prepare vegetative cell HRRPUA in WUI for ellipse/heat flux calculation
               IF (L_WUI_P%IFBFM .NE. 91) THEN
                  IF (TRIM(SURFACE_SPREAD_MODEL) .EQ. "ROTHERMEL") THEN
                     CALL ROTHERMEL_SURFACE_SPREAD_RATE(LIST_WUI_BURNING, L_WUI_P)
                  ELSE IF (TRIM(SURFACE_SPREAD_MODEL) .EQ. "CFFDRS") THEN
                     CALL CFFDRS_SPREAD_RATE(LIST_WUI_BURNING, L_WUI_P, daily_bui(DAY_OF_SIM))
                  ENDIF

                  ! Approximate vegetative WU-E source intensity using head-fire FLIN.
                  L_WUI_P%FLIN_SURFACE = L_WUI_P%FLIN_DMS_SURFACE
                  L_WUI_P%HRRPUA = L_WUI_P%FLIN_SURFACE / ANALYSIS_CELLSIZE
               ENDIF
               CALL HRR_TRANSIENT(L_WUI_P, T)
               CALL CALC_WUI_HEATFLUX(L_WUI_P, NX, NY, DT)
           
               HRR_TRANSIENT_MAP(IX,IY) = L_WUI_P%HRR_TRANSIENT

               L_WUI_P => L_WUI_P%NEXT
            ENDDO
         ENDIF
#endif

#ifdef _SUPPRESSION
         ! new suppression model :: modified below
         IF (ENABLE_EXTENDED_ATTACK) THEN
            IF (EXTENDED_ATTACK_MODEL .EQ. 0) THEN
               SUPP(0)%ACRES = ACRES
            ELSE IF (EXTENDED_ATTACK_MODEL .EQ. 1) THEN
               WRITE(*,*)
               WRITE(*,'(A)') 'Time (s),Suppressed Line (m),Fire Line (m),Contained'
               CONTINUE
            ELSE
               WRITE(*,*) 'Error: "EXTENDED_ATTACK_MODEL" should be 0 or 1 in namelist!'
               STOP
            ENDIF
         ENDIF
#endif
         ICOUNT = 0
         DO IY = 1, NY
         DO IX = 1, NX

            IF (PHIP(IX,IY) .LE. 0.) THEN

               IXSTART = MAX(1 , IX - BANDTHICKNESS) ; IXSTART = MIN(IXSTART,NX)
               IXSTOP  = MIN(NX, IX + BANDTHICKNESS) ; IXSTOP  = MAX(IXSTOP , 1)
               IYSTART = MAX(1 , IY - BANDTHICKNESS) ; IYSTART = MIN(IYSTART,NY)
               IYSTOP  = MIN(NY, IY + BANDTHICKNESS) ; IYSTOP  = MAX(IYSTOP , 1)

               GO = .FALSE. 

               IF (IXSTOP .GE. IXSTART .AND. IYSTOP .GE. IYSTART) THEN
                  DO IY2 = IYSTART, IYSTOP
                  DO IX2 = IXSTART, IXSTOP
                        IF (PHIP(IX2,IY2) .GT. 0.) GO = .TRUE. 
                  ENDDO
                  ENDDO
               ENDIF

               IF (GO .AND. (.NOT. ISNONBURNABLE(IX,IY)) ) THEN
                  ICOUNT = ICOUNT + 1 
                  CALL TAG_BAND(NX,NY,IX,IY,T)
               ENDIF
            ENDIF
         ENDDO
         ENDDO

      ENDIF

      CALL ACCUMULATE_CPU_USAGE(34, IT1, IT2)

#ifdef _SMOKE
      IF (ENABLE_SMOKE_OUTPUTS) THEN
         DUMP_SMOKE_OUTPUTS = .TRUE.
      ENDIF
#endif

      IF (NUM_TIME_AT_BURNED_ACRES .GT. 0) ALREADY_REACHED_BURNED_ACRES(:) = .FALSE.
      
      ! ***************************************************************************************
      START_CALCS = .TRUE.
      INITIATED = .TRUE.

      if (FEEDBACK_LEVEL .ge. 3) then
         WRITE(LOG_MSG,'(A,I0,A)') '[',ICASE,'] INITIATION ENDED'
         WRITE(*,'(A)') TRIM(LOG_MSG)
      endif
   ENDIF

   ! if (ICASE .ge. 9945) then
   !    WRITE(LOG_MSG,'(A,I0,A,F12.1)') '[',ICASE,'] PASSED CASE START CHECK, T IS ',T
   !    WRITE(*,'(A)') TRIM(LOG_MSG)
   ! endif
   
   !main calculation section
   IF (START_CALCS .and. rank_finished .ne. 1) THEN
      CALL SYSTEM_CLOCK(IT1)

      if (FEEDBACK_LEVEL .GE. 1 .and. NPROC .eq. 1) THEN
         write(*,'(A)', advance='no') char(13)   ! carriage return
         write(*,'(A,I0,A,F0.1,A,F0.1,A,I7,A,I0,A,I0,A,F0.1,A,F0.1)', advance='no') '[',ICASE,'] Current Timestep: ', T - (IWX_BAND - 1)*DT_METEOROLOGY, ' of ', SIMULATION_TSTOP, ', tracked nodes: ', LIST_TAGGED%NUM_NODES, ". Weather bands ", BAND_L, " to ", BAND_H!, " | Actual timings: ", T, " of ", TSTOP
         call flush(6)
      END IF
      ITIMESTEP = ITIMESTEP + 1

      IF (USE_DIURNAL_ADJUSTMENT_FACTOR) THEN

         BURN_PERIOD_CENTER_HOUR = SUNRISE_HOUR + BURN_PERIOD_CENTER_FRAC * (SUNSET_HOUR - SUNRISE_HOUR)
         BURN_PERIOD_START_HOUR  = BURN_PERIOD_CENTER_HOUR - 0.5 * BURN_PERIOD_LENGTH
         BURN_PERIOD_STOP_HOUR   = BURN_PERIOD_CENTER_HOUR + 0.5 * BURN_PERIOD_LENGTH
         IF (BURN_PERIOD_STOP_HOUR .GT. 24.) BURN_PERIOD_STOP_HOUR = BURN_PERIOD_STOP_HOUR - 24.

         HOUR_OF_DAY = FORECAST_START_HOUR + T / 3600.0
         HOUR_OF_DAY = MODULO(HOUR_OF_DAY, 24.)
         DIURNAL_ADJUSTMENT_FACTOR = 1.0
         IF (BURN_PERIOD_STOP_HOUR .GT. BURN_PERIOD_START_HOUR) THEN
            IF (HOUR_OF_DAY .LT. BURN_PERIOD_START_HOUR .OR. HOUR_OF_DAY .GT. BURN_PERIOD_STOP_HOUR) THEN
               DIURNAL_ADJUSTMENT_FACTOR = OVERNIGHT_ADJUSTMENT_FACTOR
            ENDIF
         ELSE
            IF (HOUR_OF_DAY .GT. BURN_PERIOD_STOP_HOUR .AND. HOUR_OF_DAY .LT. BURN_PERIOD_START_HOUR) THEN
               DIURNAL_ADJUSTMENT_FACTOR = OVERNIGHT_ADJUSTMENT_FACTOR
            ENDIF
         ENDIF

      ENDIF

      IF (RANDOM_IGNITIONS) THEN
         PHIP    (IX_IGN,IY_IGN) = -1.0
         ! IF (ITIMESTEP .EQ. 1) CALL TAG_BAND(NX,NY,IX_IGN,IY_IGN,T)
         IF (ITIMESTEP .EQ. 1) THEN
            TIME_OF_ARRIVAL(IX_IGN,IY_IGN) = T
            SURFACE_FIRE(IX_IGN,IY_IGN) = 1

            CALL TAG_BAND(NX,NY,IX_IGN,IY_IGN,T)
            CALL APPEND(LIST_BURNED,IX_IGN,IY_IGN,T)
#ifdef _WUI
            IF (USE_BLDG_SPREAD_MODEL .AND. BLDG_SPREAD_MODEL_TYPE .EQ. 2) THEN
               ! Tag WUI cells
               CALL TAG_WUI(NX, NY, IX_IGN, IY_IGN, T) 
            ENDIF
#endif
         ENDIF
      ENDIF

      IF (NUM_IGNITIONS .GT. 0) THEN
         DO I = 1, NUM_IGNITIONS
            IF (ALREADY_IGNITED(I)) then
               IX_IGN = ICOL_FROM_X(X_IGN(I),ANALYSIS_XLLCORNER,ANALYSIS_CELLSIZE)
               IY_IGN = IROW_FROM_Y(Y_IGN(I),ANALYSIS_YLLCORNER,ANALYSIS_CELLSIZE)
               PHIP    (IX_IGN,IY_IGN) = -1.0
               CYCLE
            endif
            IF (T .GE. T_IGN(I)) THEN
               ALREADY_IGNITED(I) = .TRUE.
               IX_IGN = ICOL_FROM_X(X_IGN(I),ANALYSIS_XLLCORNER,ANALYSIS_CELLSIZE)
               IY_IGN = IROW_FROM_Y(Y_IGN(I),ANALYSIS_YLLCORNER,ANALYSIS_CELLSIZE)
               PHIP    (IX_IGN,IY_IGN) = -1.0
               TIME_OF_ARRIVAL(IX_IGN,IY_IGN) = T
               SURFACE_FIRE(IX_IGN,IY_IGN) = 1

               CALL TAG_BAND(NX,NY,IX_IGN,IY_IGN,T)
               CALL APPEND(LIST_BURNED,IX_IGN,IY_IGN,T)

               IF (USE_BLDG_SPREAD_MODEL .AND. BLDG_SPREAD_MODEL_TYPE .EQ. 2) THEN
                  ! Tag WUI cells
                  CALL TAG_WUI(NX, NY, IX_IGN, IY_IGN, T) 
               ENDIF
            ENDIF
         ENDDO
      ENDIF

      CALL ACCUMULATE_CPU_USAGE(36, IT1, IT2)

      ! Determine where we are in the wind and weather arrays::
      IF (ITIMESTEP .EQ. 1 .OR. NUM_METEOROLOGY_TIMES .GT. 1) THEN
         ITLO_METEOROLOGY = MAX(FLOOR(T / DT_METEOROLOGY) - BAND_L + 1,1)
         ITLO_METEOROLOGY = MIN(ITLO_METEOROLOGY, BAND_H)
         ITHI_METEOROLOGY = MIN(ITLO_METEOROLOGY + 1, BAND_H)
         F_METEOROLOGY = (T - REAL(ITLO_METEOROLOGY+BAND_L-2) * DT_METEOROLOGY) / DT_METEOROLOGY
         IF (ITLO_METEOROLOGY .EQ. ITHI_METEOROLOGY) F_METEOROLOGY = 1.

         M1_LO   => M1%R4   (:,:,ITLO_METEOROLOGY)
         M1_HI   => M1%R4   (:,:,ITHI_METEOROLOGY)
         M10_LO  => M10%R4  (:,:,ITLO_METEOROLOGY)
         M10_HI  => M10%R4  (:,:,ITHI_METEOROLOGY)
         M100_LO => M100%R4 (:,:,ITLO_METEOROLOGY)
         M100_HI => M100%R4 (:,:,ITHI_METEOROLOGY)
         WS20_LO => WS%R4   (:,:,ITLO_METEOROLOGY)
         WS20_HI => WS%R4   (:,:,ITHI_METEOROLOGY)
         WD20_LO => WD%R4   (:,:,ITLO_METEOROLOGY)
         WD20_HI => WD%R4   (:,:,ITHI_METEOROLOGY)
         MLH_LO  => MLH%R4  (:,:,ITLO_METEOROLOGY)
         MLH_HI  => MLH%R4  (:,:,ITHI_METEOROLOGY)
         MLW_LO  => MLW%R4  (:,:,ITLO_METEOROLOGY)
         MLW_HI  => MLW%R4  (:,:,ITHI_METEOROLOGY)
         FMC_LO  => MFOL%R4 (:,:,ITLO_METEOROLOGY)
         FMC_HI  => MFOL%R4 (:,:,ITHI_METEOROLOGY)
      ENDIF

      IF (NUM_VIRTUAL_STATIONS .GT. 0) THEN
         C=>LIST_VIRTUAL_STATIONS%HEAD
         DO I = 1, NUM_VIRTUAL_STATIONS
            CALL INTERP_RASTER_LINKEDLIST_SINGLE (C, M1_LO  (:,:), M1_HI  (:,:), F_METEOROLOGY, 1)
            CALL INTERP_RASTER_LINKEDLIST_SINGLE (C, M10_LO (:,:), M10_HI (:,:), F_METEOROLOGY, 2)
            CALL INTERP_RASTER_LINKEDLIST_SINGLE (C, M100_LO(:,:), M100_HI(:,:), F_METEOROLOGY, 3)
            CALL INTERP_RASTER_LINKEDLIST_SINGLE (C, MLH_LO (:,:), MLH_HI (:,:), F_METEOROLOGY, 4)
            CALL INTERP_RASTER_LINKEDLIST_SINGLE (C, MLW_LO (:,:), MLW_HI (:,:), F_METEOROLOGY, 5)
            CALL INTERP_RASTER_LINKEDLIST_SINGLE (C, FMC_LO (:,:), FMC_HI (:,:), F_METEOROLOGY, 6)
            CALL INTERP_RASTER_LINKEDLIST_SINGLE (C, WS20_LO(:,:), WS20_HI(:,:), F_METEOROLOGY, 7)
            CALL UPDATE_WD_RASTER_SINGLE(C, WD20_LO(:,:), WD20_HI(:,:), F_METEOROLOGY)
            CALL WRITE_STATION(C,IRANK_WORLD,ICASE,T)
            C=>C%NEXT
         ENDDO
      ENDIF

      ! If user-specified SURFACE_ACCELERATION_TIME_CONSTANT is greater than 30 seconds,
      ! calculate SURFACE_ACCELERATION_FACTOR:
      IF (SURFACE_ACCELERATION_TIME_CONSTANT .GT. 30.) THEN
         TAU = T / SURFACE_ACCELERATION_TIME_CONSTANT
         IF (TAU .GT. 7) THEN
            SURFACE_ACCELERATION_FACTOR = 1.000000
         ELSE
            SURFACE_ACCELERATION_FACTOR = 1.000000 - EXP(-TAU)
         ENDIF
      ENDIF

      CALL ACCUMULATE_CPU_USAGE(37, IT1, IT2)

   ! Interpolate / map transient weather rasters
      JUST_INTERPOLATED = .FALSE.
      
      IF (T - T_LAST_INTERPOLATE_M1 .GE. DT_INTERPOLATE_M1) THEN
         T_LAST_INTERPOLATE_M1 = T
         JUST_INTERPOLATED = .TRUE.
         IF (WX_BILINEAR_INTERPOLATION) THEN
            CALL INTERP_RASTER_LINKEDLIST_BILINEAR(LIST_TAGGED, M1_LO  (:,:), M1_HI  (:,:), F_METEOROLOGY, 1)
            IF (USE_BLDG_SPREAD_MODEL .AND. (BLDG_SPREAD_MODEL_TYPE .EQ. 2)) THEN
               CALL INTERP_RASTER_LINKEDLIST_BILINEAR(LIST_WUI_BURNING, M1_LO  (:,:), M1_HI  (:,:), F_METEOROLOGY, 1)
            ENDIF
         ELSE
            CALL INTERP_RASTER_LINKEDLIST(LIST_TAGGED, M1_LO  (:,:), M1_HI  (:,:), F_METEOROLOGY, 1)
            IF (USE_BLDG_SPREAD_MODEL .AND. (BLDG_SPREAD_MODEL_TYPE .EQ. 2)) THEN
               CALL INTERP_RASTER_LINKEDLIST(LIST_WUI_BURNING, M1_LO  (:,:), M1_HI  (:,:), F_METEOROLOGY, 1)
            ENDIF
         ENDIF
      ENDIF

      IF (T - T_LAST_INTERPOLATE_M10 .GE. DT_INTERPOLATE_M10) THEN
         T_LAST_INTERPOLATE_M10 = T
         JUST_INTERPOLATED = .TRUE.
         IF (WX_BILINEAR_INTERPOLATION) THEN
            CALL INTERP_RASTER_LINKEDLIST_BILINEAR(LIST_TAGGED, M10_LO (:,:), M10_HI (:,:), F_METEOROLOGY, 2)
            IF (USE_BLDG_SPREAD_MODEL .AND. (BLDG_SPREAD_MODEL_TYPE .EQ. 2)) THEN
               CALL INTERP_RASTER_LINKEDLIST_BILINEAR(LIST_WUI_BURNING, M10_LO (:,:), M10_HI (:,:), F_METEOROLOGY, 2)
            ENDIF
         ELSE
            CALL INTERP_RASTER_LINKEDLIST(LIST_TAGGED, M10_LO (:,:), M10_HI (:,:), F_METEOROLOGY, 2)
            IF (USE_BLDG_SPREAD_MODEL .AND. (BLDG_SPREAD_MODEL_TYPE .EQ. 2)) THEN
               CALL INTERP_RASTER_LINKEDLIST(LIST_WUI_BURNING, M10_LO (:,:), M10_HI (:,:), F_METEOROLOGY, 2)
            ENDIF
         ENDIF
      ENDIF

      IF (T - T_LAST_INTERPOLATE_M100 .GE. DT_INTERPOLATE_M100) THEN
         T_LAST_INTERPOLATE_M100 = T
         JUST_INTERPOLATED = .TRUE.
         IF (WX_BILINEAR_INTERPOLATION) THEN
            CALL INTERP_RASTER_LINKEDLIST_BILINEAR(LIST_TAGGED, M100_LO(:,:), M100_HI(:,:), F_METEOROLOGY, 3)
            IF (USE_BLDG_SPREAD_MODEL .AND. (BLDG_SPREAD_MODEL_TYPE .EQ. 2)) THEN
               CALL INTERP_RASTER_LINKEDLIST_BILINEAR(LIST_WUI_BURNING, M100_LO(:,:), M100_HI(:,:), F_METEOROLOGY, 3)
            ENDIF
         ELSE
            CALL INTERP_RASTER_LINKEDLIST(LIST_TAGGED, M100_LO(:,:), M100_HI(:,:), F_METEOROLOGY, 3)
            IF (USE_BLDG_SPREAD_MODEL .AND. (BLDG_SPREAD_MODEL_TYPE .EQ. 2)) THEN
               CALL INTERP_RASTER_LINKEDLIST(LIST_WUI_BURNING, M100_LO(:,:), M100_HI(:,:), F_METEOROLOGY, 3)
            ENDIF
         ENDIF
      ENDIF

      IF (T - T_LAST_INTERPOLATE_MLH .GE. DT_INTERPOLATE_MLH) THEN
         T_LAST_INTERPOLATE_MLH = T
         JUST_INTERPOLATED = .TRUE.
         IF (WX_BILINEAR_INTERPOLATION) THEN
            CALL INTERP_RASTER_LINKEDLIST_BILINEAR(LIST_TAGGED, MLH_LO (:,:), MLH_HI (:,:), F_METEOROLOGY, 4)
            IF (USE_BLDG_SPREAD_MODEL .AND. (BLDG_SPREAD_MODEL_TYPE .EQ. 2)) THEN
               CALL INTERP_RASTER_LINKEDLIST_BILINEAR(LIST_WUI_BURNING, MLH_LO (:,:), MLH_HI (:,:), F_METEOROLOGY, 4)
            ENDIF
         ELSE
            CALL INTERP_RASTER_LINKEDLIST(LIST_TAGGED, MLH_LO (:,:), MLH_HI (:,:), F_METEOROLOGY, 4)
            IF (USE_BLDG_SPREAD_MODEL .AND. (BLDG_SPREAD_MODEL_TYPE .EQ. 2)) THEN
               CALL INTERP_RASTER_LINKEDLIST(LIST_WUI_BURNING, MLH_LO (:,:), MLH_HI (:,:), F_METEOROLOGY, 4)
            ENDIF
         ENDIF
      ENDIF

      IF (T - T_LAST_INTERPOLATE_MLW .GE. DT_INTERPOLATE_MLW) THEN
         T_LAST_INTERPOLATE_MLW = T
         JUST_INTERPOLATED = .TRUE.
         IF (WX_BILINEAR_INTERPOLATION) THEN
            CALL INTERP_RASTER_LINKEDLIST_BILINEAR(LIST_TAGGED, MLW_LO (:,:), MLW_HI (:,:), F_METEOROLOGY, 5)
            IF (USE_BLDG_SPREAD_MODEL .AND. (BLDG_SPREAD_MODEL_TYPE .EQ. 2)) THEN
               CALL INTERP_RASTER_LINKEDLIST_BILINEAR(LIST_WUI_BURNING, MLW_LO (:,:), MLW_HI (:,:), F_METEOROLOGY, 5)
            ENDIF
         ELSE
            CALL INTERP_RASTER_LINKEDLIST(LIST_TAGGED, MLW_LO (:,:), MLW_HI (:,:), F_METEOROLOGY, 5)
            IF (USE_BLDG_SPREAD_MODEL .AND. (BLDG_SPREAD_MODEL_TYPE .EQ. 2)) THEN
               CALL INTERP_RASTER_LINKEDLIST(LIST_WUI_BURNING, MLW_LO (:,:), MLW_HI (:,:), F_METEOROLOGY, 5)
            ENDIF
         ENDIF
      ENDIF

      IF (T - T_LAST_INTERPOLATE_FMC .GE. DT_INTERPOLATE_FMC) THEN
         T_LAST_INTERPOLATE_FMC = T
         JUST_INTERPOLATED = .TRUE.
         IF (WX_BILINEAR_INTERPOLATION) THEN
            CALL INTERP_RASTER_LINKEDLIST_BILINEAR(LIST_TAGGED, FMC_LO (:,:), FMC_HI (:,:), F_METEOROLOGY, 6)
            IF (USE_BLDG_SPREAD_MODEL .AND. (BLDG_SPREAD_MODEL_TYPE .EQ. 2)) THEN
               CALL INTERP_RASTER_LINKEDLIST_BILINEAR(LIST_WUI_BURNING, FMC_LO (:,:), FMC_HI (:,:), F_METEOROLOGY, 6)
            ENDIF
         ELSE
            CALL INTERP_RASTER_LINKEDLIST(LIST_TAGGED, FMC_LO (:,:), FMC_HI (:,:), F_METEOROLOGY, 6)
            IF (USE_BLDG_SPREAD_MODEL .AND. (BLDG_SPREAD_MODEL_TYPE .EQ. 2)) THEN
               CALL INTERP_RASTER_LINKEDLIST(LIST_WUI_BURNING, FMC_LO (:,:), FMC_HI (:,:), F_METEOROLOGY, 6)
            ENDIF
         ENDIF
      ENDIF

      IF (T - T_LAST_INTERPOLATE_WIND .GE. DT_INTERPOLATE_WIND) THEN
         T_LAST_INTERPOLATE_WIND = T
         JUST_INTERPOLATED = .TRUE.
         IF (WX_BILINEAR_INTERPOLATION) THEN
            CALL INTERP_WIND_LINKEDLIST_BILINEAR(LIST_TAGGED, WS20_LO(:,:), WS20_HI(:,:), WD20_LO(:,:), WD20_HI(:,:), F_METEOROLOGY)
            IF (USE_BLDG_SPREAD_MODEL .AND. (BLDG_SPREAD_MODEL_TYPE .EQ. 2)) THEN
               CALL INTERP_WIND_LINKEDLIST_BILINEAR(LIST_WUI_BURNING, WS20_LO(:,:), WS20_HI(:,:), WD20_LO(:,:), WD20_HI(:,:), F_METEOROLOGY)
            ENDIF
         ELSE
            CALL INTERP_RASTER_LINKEDLIST(LIST_TAGGED, WS20_LO(:,:), WS20_HI(:,:), F_METEOROLOGY, 7)
            CALL UPDATE_WD_RASTER(LIST_TAGGED, WD20_LO(:,:), WD20_HI(:,:), F_METEOROLOGY)

            IF (USE_BLDG_SPREAD_MODEL .AND. (BLDG_SPREAD_MODEL_TYPE .EQ. 2)) THEN
               CALL INTERP_RASTER_LINKEDLIST(LIST_WUI_BURNING, WS20_LO(:,:), WS20_HI(:,:), F_METEOROLOGY, 7)
               CALL UPDATE_WD_RASTER(LIST_WUI_BURNING, WD20_LO(:,:), WD20_HI(:,:), F_METEOROLOGY)
            ENDIF
         ENDIF
   !      CALL INTERP_WD_RASTER(LIST_TAGGED, WD20_LO(:,:), WD20_HI(:,:), F_METEOROLOGY)
      ENDIF

      IF (WIND_FLUCTUATIONS .AND. T - T_LAST_WIND_FLUCTUATIONS .GE. DT_WIND_FLUCTUATIONS) THEN
         T_LAST_WIND_FLUCTUATIONS = T
         JUST_INTERPOLATED = .TRUE.
         CALL APPLY_WIND_FLUCTUATIONS(LIST_TAGGED)
      ENDIF

      CALL ACCUMULATE_CPU_USAGE(38, IT1, IT2)

      ! Main call to get spread rate:
      IF (JUST_INTERPOLATED) THEN
         if (trim(SURFACE_SPREAD_MODEL) .eq. "ROTHERMEL") then
            CALL ROTHERMEL_SURFACE_SPREAD_RATE(LIST_TAGGED, DUMMY_NODE)
         else if (trim(SURFACE_SPREAD_MODEL) .eq. "CFFDRS") then
            CALL CFFDRS_SPREAD_RATE(LIST_TAGGED, DUMMY_NODE, daily_bui(DAY_OF_SIM))
         ENDIF
      ENDIF
      CALL ACCUMULATE_CPU_USAGE(39, IT1, IT2)

      ! Adjust spread rate for passive and active crown fire (Cruz):
      ! Note that this adjusts spread rate in not only burned cells but nearby cells
      ! that are "about to burn"

#ifdef _WUI
      ! Refactored WU-E model, call these out of the spreading rate subroutines

      IF (USE_BLDG_SPREAD_MODEL .AND. (BLDG_SPREAD_MODEL_TYPE .EQ. 2) .AND. (LIST_WUI_BURNING%NUM_NODES .GT. 0)) THEN
         L_WUI_P => LIST_WUI_BURNING%HEAD
         DO I = 1, LIST_WUI_BURNING%NUM_NODES
            IX = L_WUI_P%IX
            IY = L_WUI_P%IY
            ! Update the ellipse parimeter (does it make sense? Unignited cells has fire perimeter already?)
            CALL ELLIPSE_UCB(L_WUI_P)
            ! Skip non-burning pixels
            IF (PHIP(IX,IY) .GT. 0.) THEN
               L_WUI_P => L_WUI_P%NEXT
               CYCLE
            ELSE
               L_WUI_P%TIME_OF_ARRIVAL=TIME_OF_ARRIVAL(IX,IY)
            ENDIF
            ! Prepare vegetative cell HRRPUA in WUI for ellipse/heat flux calculation
            IF (L_WUI_P%IFBFM .NE. 91) THEN
               IF (TRIM(SURFACE_SPREAD_MODEL) .EQ. "ROTHERMEL") THEN
                  CALL ROTHERMEL_SURFACE_SPREAD_RATE(LIST_WUI_BURNING, L_WUI_P)
               ELSE IF (TRIM(SURFACE_SPREAD_MODEL) .EQ. "CFFDRS") THEN
                  CALL CFFDRS_SPREAD_RATE(LIST_WUI_BURNING, L_WUI_P, daily_bui(DAY_OF_SIM))
               ENDIF

               ! Approximate vegetative WU-E source intensity using head-fire FLIN.
               L_WUI_P%FLIN_SURFACE = L_WUI_P%FLIN_DMS_SURFACE
               L_WUI_P%HRRPUA = L_WUI_P%FLIN_SURFACE / ANALYSIS_CELLSIZE
            ENDIF
            CALL HRR_TRANSIENT(L_WUI_P, T) ! This is to be modified to update HRR_TRANSIENT for all burning cells.
            CALL CALC_WUI_HEATFLUX(L_WUI_P, NX, NY, DT)

            L_WUI_P => L_WUI_P%NEXT
         ENDDO
      ENDIF
#endif

      DO ISTEP = 1, 2

         ! Calculate components of normal vector
         CALL CALC_NORMAL_VECTORS (ISTEP, HALFRCELLSIZE)
         CALL ACCUMULATE_CPU_USAGE(41, IT1, IT2)

         ! Calculate x and y components of velocity from elliptical spread dimensions
         CALL UX_AND_UY_ELLIPTICAL(LIST_TAGGED, SURFACE_ACCELERATION_FACTOR, ISTEP, DT)
         CALL ACCUMULATE_CPU_USAGE(42, IT1, IT2)
         
         ! Update local spread properties that depend on canopy / fire velocity
         CALL UPDATE_LOCAL_SPREAD_PROPERTIES(LIST_TAGGED, DUMMY_NODE)
         ! Check CFL criterion, adjust timestep, AND apply flux limiter (merged)
         CALL CFL_AND_FLUX_LIMITER(DT, RCELLSIZE, PHIP, ISTEP, ITIMESTEP)
         CALL ACCUMULATE_CPU_USAGE(43, IT1, IT2)

         IF (T + REAL(DT,8) .GT. REAL(TSTOP,8)) DT = MAX(0.0, REAL(TSTOP - T))

         ! 2nd order Runge Kutta integration:
         CALL RK2_INTEGRATE(DT, ISTEP)
         CALL ACCUMULATE_CPU_USAGE(44, IT1, IT2)

      ENDDO !ISTEP=1,2

   ! Find newly-burned cells and call spotting:
      N_TO_TAG = 0
      N_SPOT_FIRES = 0
      
      C => LIST_TAGGED%HEAD
      DO I = 1, LIST_TAGGED%NUM_NODES
         IX = C%IX
         IY = C%IY
        
         IF (PHIP(IX,IY) .LE. 0. .AND. SURFACE_FIRE(IX,IY) .EQ. 0) THEN
         
            ACRES = ACRES + ACRES_PER_PIXEL
#ifdef _SUPPRESSION
         ! new suppression model :: modified_below
         IF (ENABLE_EXTENDED_ATTACK) THEN
            IF (EXTENDED_ATTACK_MODEL .EQ. 0) THEN
               IF (USE_SDI) THEN
                  ACRES_SDI = ACRES_SDI + ACRES_PER_PIXEL * (1.0 + C%SDI )
               ELSE
                  ACRES_SDI = ACRES
               ENDIF
            ELSE IF (EXTENDED_ATTACK_MODEL .EQ. 1) THEN
               CONTINUE
            ELSE
               WRITE(*,*) 'Error: "EXTENDED_ATTACK_MODEL" should be 0 or 1 in namelist!'
               STOP
            ENDIF

         ENDIF
#endif
            C%BURNED               = .TRUE.
            C%TIME_OF_ARRIVAL      = T
            SURFACE_FIRE   (IX,IY) = 1
            TIME_OF_ARRIVAL(IX,IY) = T
            
            IF (C%CROWN_FIRE .LT. 0) C%CROWN_FIRE = 0
            
   ! Note that per Thomas (1963) and Rothermel (1991), crown fire flame length is Lf=0.2*I^2/3
            IF (C%FLIN_SURFACE .GT. 0.) THEN
               IF (C%CROWN_FIRE .EQ. 2) THEN
                  C%FLAME_LENGTH = 2.5 * CH%R4 (C%IX,C%IY,1)   !Active crown fire flame length modification taken from Prometheus
               ELSE
                  C%FLAME_LENGTH = (0.0775 / 0.3048) * (C%FLIN_SURFACE + C%FLIN_CANOPY) ** 0.46 
               ENDIF
            ELSE
               C%FLAME_LENGTH = 0.
            ENDIF

            CALL APPEND(LIST_BURNED, IX, IY, T)

            LIST_BURNED%TAIL%IR                     = C%IR
            LIST_BURNED%TAIL%VS0                    = C%VS0
            LIST_BURNED%TAIL%PHIW_SURFACE           = C%PHIW_SURFACE
            LIST_BURNED%TAIL%PHIW_CROWN             = C%PHIW_CROWN
            LIST_BURNED%TAIL%PHIS_SURFACE           = C%PHIS_SURFACE
            LIST_BURNED%TAIL%VELOCITY_DMS           = C%VELOCITY_DMS
            LIST_BURNED%TAIL%VELOCITY               = C%VELOCITY
            LIST_BURNED%TAIL%FLIN_SURFACE           = C%FLIN_SURFACE
            LIST_BURNED%TAIL%FLIN_CANOPY            = C%FLIN_CANOPY
            LIST_BURNED%TAIL%FLAME_LENGTH           = C%FLAME_LENGTH
            LIST_BURNED%TAIL%HPUA_SURFACE           = C%HPUA_SURFACE
            LIST_BURNED%TAIL%HPUA_CANOPY            = C%HPUA_CANOPY
            LIST_BURNED%TAIL%UX                     = C%UX
            LIST_BURNED%TAIL%UY                     = C%UY
            LIST_BURNED%TAIL%SPREAD_DIRECTION       = C%SPREAD_DIRECTION
            LIST_BURNED%TAIL%TIME_OF_ARRIVAL        = C%TIME_OF_ARRIVAL
            LIST_BURNED%TAIL%CRITICAL_FLIN          = C%CRITICAL_FLIN
            LIST_BURNED%TAIL%CROWN_FIRE             = C%CROWN_FIRE
            LIST_BURNED%TAIL%BURNED                 = .TRUE.
            
            ! new suppression model :: added below
            LIST_BURNED%TAIL%TYPE_GROUP             = C%TYPE_GROUP
            LIST_BURNED%TAIL%SEGMENT_GROUP          = C%SEGMENT_GROUP
            LIST_BURNED%TAIL%STS                    = C%STS
            
            LIST_BURNED%TAIL%IFBFM                  = C%IFBFM
            LIST_BURNED%TAIL%WS20_NOW               = C%WS20_NOW
            LIST_BURNED%TAIL%WD20_NOW               = C%WD20_NOW
            LIST_BURNED%TAIL%LOCAL_EMBERGEN_DURATION= C%LOCAL_EMBERGEN_DURATION

            LIST_BURNED%TAIL%VELOCITY_DMS_SURFACE = C%VELOCITY_DMS_SURFACE
            LIST_BURNED%TAIL%HRRPUA = (C%FLIN_SURFACE + C%FLIN_CANOPY) / ASP%CELLSIZE

#ifdef _WUI
            IF (USE_BLDG_SPREAD_MODEL) THEN
               IF(BLDG_FUEL_MODEL%I2(IX,IY,1) .NE. NO_DATA) THEN
                  LIST_BURNED%TAIL%IBLDGFM =  BLDG_FUEL_MODEL%I2(IX,IY,1)
               ELSE
                  LIST_BURNED%TAIL%IBLDGFM =  NO_DATA
               ENDIF
               ! Tag WUI cells
               IF(BLDG_SPREAD_MODEL_TYPE .EQ. 2) CALL TAG_WUI(NX, NY, IX, IY, T) 
            ELSE
               LIST_BURNED%TAIL%IBLDGFM = NO_DATA
            ENDIF
#endif

#ifdef _SMOKE
            IF (ENABLE_SMOKE_OUTPUTS) THEN
               LIST_BURNED%TAIL%TIME_IGNITED = T
               IF (C%VELOCITY .GT. 0.) THEN
                  TBURN = ASP%CELLSIZE / (C%VELOCITY * 0.3048 / 60.)
               ELSE
                  TBURN = 30.0 
               ENDIF
               TBURN = MIN(MAX(TBURN,1.),10800.0)
               LIST_BURNED%TAIL%TIME_EXTINGUISHED = T + TBURN
               LIST_BURNED%TAIL%HRRPUA = (C%FLIN_SURFACE + C%FLIN_CANOPY) / ASP%CELLSIZE
            ENDIF
#endif

            IF (DUMP_BINARY_OUTPUTS) THEN
               BINARY_OUTPUTS_IX           (LIST_BURNED%NUM_NODES) = INT(IX, KIND=KIND(BINARY_OUTPUTS_IX(1)))
               BINARY_OUTPUTS_IY           (LIST_BURNED%NUM_NODES) = INT(IY, KIND=KIND(BINARY_OUTPUTS_IX(1)))
               BINARY_OUTPUTS_TOA          (LIST_BURNED%NUM_NODES) = T
               BINARY_OUTPUTS_FLAME_LENGTH (LIST_BURNED%NUM_NODES) = C%FLAME_LENGTH
               BINARY_OUTPUTS_VELOCITY_FPM (LIST_BURNED%NUM_NODES) = C%VELOCITY
               BINARY_OUTPUTS_CROWN_FIRE   (LIST_BURNED%NUM_NODES) = C%CROWN_FIRE
            ENDIF

            IF (USE_BARRIERS) THEN
               IF (CHECK_BARRIER_BREACH(C)) THEN
                  N_TO_TAG = N_TO_TAG + 1
                  IX_TO_TAG(N_TO_TAG) = IX
                  IY_TO_TAG(N_TO_TAG) = IY
               ENDIF
            ELSE
               N_TO_TAG = N_TO_TAG + 1
               IX_TO_TAG(N_TO_TAG) = IX
               IY_TO_TAG(N_TO_TAG) = IY
            ENDIF

            IF (ENABLE_SPOTTING .AND. USE_SUPERSEDED_SPOTTING) THEN
               CALL_SPOTTING = .FALSE.
               IF(C%IFBFM .EQ. 91) THEN
                  FLIN = HRR_TRANSIENT_MAP(C%IX,C%IY)*ANALYSIS_CELLSIZE+1E-5
               ELSE
                  FLIN = C%FLIN_SURFACE
               ENDIF
               IF (FLIN .GE. C%CRITICAL_FLIN) THEN
                  CALL RANDOM_NUMBER(R0)
                  IF (R0 .LT. 0.01*CROWN_FIRE_SPOTTING_PERCENT) CALL_SPOTTING = .TRUE.
               ENDIF

               IF (ENABLE_SURFACE_FIRE_SPOTTING .AND. (.NOT. CALL_SPOTTING) ) THEN
                  IF (FLIN .GE. CRITICAL_SPOTTING_FIRELINE_INTENSITY(FBFM%I2(C%IX,C%IY,1))) THEN
                     CALL RANDOM_NUMBER(R0)
                     IF (R0 .LT. 0.01*SURFACE_FIRE_SPOTTING_PERCENT(FBFM%I2(C%IX,C%IY,1))) CALL_SPOTTING = .TRUE.
                     CONTINUE
                  ENDIF
               ENDIF

               IF (CALL_SPOTTING) THEN
                  CALL SPOTTING_SUPERSEDED ( IX,IY,C%WS20_NOW,FLIN,F_METEOROLOGY,WS20_LO,WS20_HI, WD20_LO, WD20_HI, &
                                    N_SPOT_FIRES,IX_SPOT_FIRE,IY_SPOT_FIRE,ICASE, SOURCE_FUEL_IGN_MULT (FBFM%I2(C%IX,C%IY,1)) )
               ENDIF
            ENDIF ! ENABLE_SPOTTING
         ENDIF
         C => C%NEXT

      ENDDO ! I = 1, LIST_TAGGED%NUM_NODES

#ifdef _UMDSPOTTING
      IF (ENABLE_SPOTTING .AND. (.NOT. USE_SUPERSEDED_SPOTTING)) THEN
         C => LIST_BURNED%HEAD
         DO I = 1, LIST_BURNED%NUM_NODES

#ifdef _WUI
            ! Refresh transient HRRPUA for Hamada model, to be used in eulerian firebrand model
            IF(USE_BLDG_SPREAD_MODEL .AND. BLDG_SPREAD_MODEL_TYPE .EQ. 1) CALL HRR_TRANSIENT(C, T)
#endif
            CALL_SPOTTING = .FALSE.
            IF (.NOT. C%SPOTTING_DURATION_CALCULATED) CALL CALC_SPOTTING_DURATION(C)

            ! Set DT_SPOTTING to the overlap length between [T, T+DT] and [C%T_END_SPOTTING,C%T_START_SPOTTING]
            DT_SPOTTING = MIN(T+DT, C%T_END_SPOTTING)-MAX(T, C%T_START_SPOTTING)
            DT_SPOTTING = MAX(0.0,DT_SPOTTING)
            IF (DT_SPOTTING .GT. 1E-5) THEN
               IF(C%IFBFM .EQ. 91 .AND. USE_BLDG_SPREAD_MODEL) THEN
                  FLIN = C%HRR_TRANSIENT+1E-5
               ELSE
                  FLIN = C%FLIN_SURFACE
               ENDIF
               IF (FLIN .GT. CRITICAL_SPOTTING_FIRELINE_INTENSITY(FBFM%I2(C%IX,C%IY,1))) THEN
                  CALL RANDOM_NUMBER(R0)
                  IF (R0 .LT. 0.01*SURFACE_FIRE_SPOTTING_PERCENT(FBFM%I2(C%IX,C%IY,1))) CALL_SPOTTING = .TRUE. 
                  CONTINUE
               ENDIF
               
               IF (CALL_SPOTTING) THEN ! If using Eulerian firebrand solver, no trajectory calculated at this step, only initiate trackers
                  CALL SPOTTING(C%IX,C%IY,C%WS20_NOW,FLIN, ICASE, DT_SPOTTING, T, &
                              SOURCE_FUEL_IGN_MULT(FBFM%I2(C%IX,C%IY,1)),  C%IFBFM, LIST_EMBER_TRACKER, BAND_L)
               ENDIF
            ENDIF
            ! C%TAU_EMBERGEN = MIN (TAU_EMBERGEN, C%TAU_EMBERGEN + DT)
            C => C%NEXT
         ENDDO
      ENDIF
#endif

      CALL ACCUMULATE_CPU_USAGE(45, IT1, IT2)
! Main firebrand ignition and Eulerian ember trajectory integration:
      DO I = 1, N_TO_TAG
         CALL TAG_BAND(NX, NY, IX_TO_TAG(I), IY_TO_TAG(I), T)
      ENDDO

      IF (.NOT. USE_SUPERSEDED_SPOTTING .and. ENABLE_SPOTTING) THEN
         IF (trim(ACCUMULATION_MODEL) .eq. 'EULERIAN') THEN
            ! Main call to ember trajectory integration and ignition determination
            CALL EULERIAN_SPOTTING_MAIN(NX, NY, ANALYSIS_CELLSIZE, T, DT, F_METEOROLOGY, WS20_LO, WS20_HI, BAND_L)
         ELSE IF (trim(ACCUMULATION_MODEL) .eq. 'LAGRANGIAN') THEN
            CALL LAGRANGIAN_SPOTTING_MAIN(NX, NY, T, DT, F_METEOROLOGY, WS20_LO, WS20_HI)
         ENDIF
      ELSE
         DO I = 1, N_SPOT_FIRES
            IX = IX_SPOT_FIRE(I)
            IY = IY_SPOT_FIRE(I)
            IF (SURFACE_FIRE(IX,IY) .LE. 0 .AND. ADJ%R4(IX,IY,1) .GT. 0. .AND. (.NOT. ISNONBURNABLE(IX,IY) ) ) THEN
               CALL TAG_BAND(NX, NY, IX, IY, T)
               TIME_OF_ARRIVAL(IX,IY) = T
               PHIP           (IX,IY) = -1.0
               IF (DUMP_EMBER_IGNITION) EMBER_IGNITION_MAP%I2(IX,IY,1) = 1
            ENDIF
         ENDDO
      ENDIF

      CALL ACCUMULATE_CPU_USAGE(46, IT1, IT2)

      ! Map wind & fuel moisture fields to newly tagged cells:
      C => LIST_TAGGED%HEAD
      DO I = 1, LIST_TAGGED%NUM_NODES
         IF (C%JUST_TAGGED) THEN
            C%JUST_TAGGED = .FALSE.
            IF (WX_BILINEAR_INTERPOLATION) THEN
               CALL INTERP_RASTER_LINKEDLIST_SINGLE_BILINEAR (C, M1_LO  (:,:), M1_HI  (:,:), F_METEOROLOGY, 1)
               CALL INTERP_RASTER_LINKEDLIST_SINGLE_BILINEAR (C, M10_LO (:,:), M10_HI (:,:), F_METEOROLOGY, 2)
               CALL INTERP_RASTER_LINKEDLIST_SINGLE_BILINEAR (C, M100_LO(:,:), M100_HI(:,:), F_METEOROLOGY, 3)
               CALL INTERP_RASTER_LINKEDLIST_SINGLE_BILINEAR (C, MLH_LO (:,:), MLH_HI (:,:), F_METEOROLOGY, 4)
               CALL INTERP_RASTER_LINKEDLIST_SINGLE_BILINEAR (C, MLW_LO (:,:), MLW_HI (:,:), F_METEOROLOGY, 5)
               CALL INTERP_RASTER_LINKEDLIST_SINGLE_BILINEAR (C, FMC_LO (:,:), FMC_HI (:,:), F_METEOROLOGY, 6)
               CALL INTERP_RASTER_LINKEDLIST_SINGLE_BILINEAR (C, WS20_LO(:,:), WS20_HI(:,:), F_METEOROLOGY, 7)
            ELSE
               CALL INTERP_RASTER_LINKEDLIST_SINGLE (C, M1_LO  (:,:), M1_HI  (:,:), F_METEOROLOGY, 1)
               CALL INTERP_RASTER_LINKEDLIST_SINGLE (C, M10_LO (:,:), M10_HI (:,:), F_METEOROLOGY, 2)
               CALL INTERP_RASTER_LINKEDLIST_SINGLE (C, M100_LO(:,:), M100_HI(:,:), F_METEOROLOGY, 3)
               CALL INTERP_RASTER_LINKEDLIST_SINGLE (C, MLH_LO (:,:), MLH_HI (:,:), F_METEOROLOGY, 4)
               CALL INTERP_RASTER_LINKEDLIST_SINGLE (C, MLW_LO (:,:), MLW_HI (:,:), F_METEOROLOGY, 5)
               CALL INTERP_RASTER_LINKEDLIST_SINGLE (C, FMC_LO (:,:), FMC_HI (:,:), F_METEOROLOGY, 6)
               CALL INTERP_RASTER_LINKEDLIST_SINGLE (C, WS20_LO(:,:), WS20_HI(:,:), F_METEOROLOGY, 7)
            ENDIF
            
            CALL UPDATE_WD_RASTER_SINGLE(C, WD20_LO(:,:), WD20_HI(:,:), F_METEOROLOGY)
            if (trim(SURFACE_SPREAD_MODEL) .eq. "ROTHERMEL") then
               CALL ROTHERMEL_SURFACE_SPREAD_RATE(LIST_TAGGED, C)
            else if (trim(SURFACE_SPREAD_MODEL) .eq. "CFFDRS") then
               CALL CFFDRS_SPREAD_RATE(LIST_TAGGED, C, daily_bui(DAY_OF_SIM))
            ENDIF
#ifdef _SUPPRESSION
            ! new suppression model :: modified below
            IF (ENABLE_EXTENDED_ATTACK) THEN
               IF (EXTENDED_ATTACK_MODEL .EQ. 0) THEN
                  IF (USE_SDI) C%SDI = SDI_FACTOR * SDI%R4(C%IX,C%IY,1)
               ELSE IF (EXTENDED_ATTACK_MODEL .EQ. 1) THEN
                     C%PCL = PCL%R4(C%IX,C%IY,1)
                     C%SDI = SDI%R4(C%IX,C%IY,1)
               ELSE
                  WRITE(*,*) 'Error: "EXTENDED_ATTACK_MODEL" should be 0 or 1 in namelist!'
                  STOP
               ENDIF
            ENDIF
#endif
         ENDIF
         C => C%NEXT
      ENDDO
         
      CALL ACCUMULATE_CPU_USAGE(47, IT1, IT2)

   ! Initial attack model:
      IF (ENABLE_INITIAL_ATTACK .AND. T .GE. INITIAL_ATTACK_TIME .AND. (.NOT. IA_HAS_OCCURRED) ) THEN
         IA_HAS_OCCURRED = .TRUE.
         HECTARES = 0.
         FLIN_MAX = 0.
         C => LIST_BURNED%HEAD
         DO I = 1, LIST_BURNED%NUM_NODES
            IX = C%IX
            IY = C%IY
            HECTARES = HECTARES + 1.
            IF (T - TIME_OF_ARRIVAL(IX,IY) .LE. MAX(300.,DT)) THEN
               IF ( (C%FLIN_SURFACE + C%FLIN_CANOPY) .GT. FLIN_MAX) FLIN_MAX = (C%FLIN_SURFACE + C%FLIN_CANOPY)
            ENDIF   
         C => C%NEXT
         ENDDO
         HECTARES = 0.404686 * ACRES_PER_PIXEL * HECTARES

         E = EXP(4.6835 - 0.7043 * HECTARES - 0.00041 * FLIN_MAX - 0.000052 * HECTARES * FLIN_MAX)
         POC = MIN(MAX(0.,E/(1.+E)),1.0)
         CALL RANDOM_NUMBER(R0)
         IF (R0 .LE. POC) THEN !Fire is contained
            if (FEEDBACK_LEVEL .ge. 3) then
               WRITE(LOG_MSG,'(A,I0,A)') '[',ICASE,'] INITIAL ATTACK CONTAINMENT SUCCESSFUL'
               WRITE(*,'(A)') TRIM(LOG_MSG)
            endif
            rank_finished = 1
            DT = DT_METEOROLOGY
            STATS_FINAL_CONTAINMENT_FRAC(ICASE) = 1.0
            STATS_SIMULATION_TSTOP_HOURS(ICASE) = INITIAL_ATTACK_TIME / 3600.0
         ENDIF
      ENDIF
      
      CALL ACCUMULATE_CPU_USAGE(48, IT1, IT2)
      
      CALL ACCUMULATE_CPU_USAGE(49, IT1, IT2)

   ! Untag
      IF (MOD(ITIMESTEP,UNTAG_CELLS_TIMESTEP_INTERVAL) .EQ. 0 .AND. LIST_TAGGED%NUM_NODES .GT. 100) THEN 
         CALL UNTAG_CELLS(NX,NY,TIME_OF_ARRIVAL,T,SURFACE_FIRE)
      ENDIF

      CALL ACCUMULATE_CPU_USAGE(50, IT1, IT2)

      IF (LIST_TAGGED%NUM_NODES .LE. 2) THEN
         IF(.NOT. (ENABLE_SPOTTING .AND. (.NOT. USE_SUPERSEDED_SPOTTING))) THEN
            if (FEEDBACK_LEVEL .ge. 3) then
               WRITE(LOG_MSG,'(A,I0,A)') '[',ICASE,'] STOPPING: LESS THAN 2 NODES TAGGED FOR FIRE SPREAD'
               WRITE(*,'(A)') TRIM(LOG_MSG)
            endif
            SIMULATION_TSTOP_HOURS = T / 3600.
            STATS_SIMULATION_TSTOP_HOURS(ICASE) = SIMULATION_TSTOP_HOURS
            STATS_FINAL_CONTAINMENT_FRAC(ICASE) = 1.0
            rank_finished = 1
            DT = DT_METEOROLOGY
         ELSE
            IF((trim(ACCUMULATION_MODEL) .eq. 'EULERIAN' .AND. LIST_EMBER_TRACKER%NUM_NODES .LT. 1) .OR. &
               (trim(ACCUMULATION_MODEL) .eq. 'LAGRANGIAN' .AND. NUM_TRACKED_EMBERS .LT. 1)) THEN
               if (FEEDBACK_LEVEL .ge. 3) then
               WRITE(LOG_MSG,'(A,I0,A)') '[',ICASE,'] STOPPING: LESS THAN 2 NODES TAGGED FOR FIRE SPREAD'
               WRITE(*,'(A)') TRIM(LOG_MSG)
            endif
               SIMULATION_TSTOP_HOURS = T / 3600.
               STATS_SIMULATION_TSTOP_HOURS(ICASE) = SIMULATION_TSTOP_HOURS
               STATS_FINAL_CONTAINMENT_FRAC(ICASE) = 1.0
               rank_finished = 1
               DT = DT_METEOROLOGY
            ENDIF
         ENDIF
      ENDIF

      IF (ACRES .GT. STATS_ASTOP(ICASE) ) THEN
         if (FEEDBACK_LEVEL .ge. 3) then
            WRITE(LOG_MSG,'(A,I0,A)') '[',ICASE,'] STOPPING: SIMULATED ACRES MORE THAN STOP CONDITION'
            WRITE(*,'(A)') TRIM(LOG_MSG)
         endif
         SIMULATION_TSTOP_HOURS = T / 3600.
         STATS_SIMULATION_TSTOP_HOURS(ICASE) = SIMULATION_TSTOP_HOURS
         rank_finished = 1
         DT = DT_METEOROLOGY
      ENDIF

      IF (NUM_TIME_AT_BURNED_ACRES .GT. 0) THEN
         FN = TRIM(OUTPUTS_DIRECTORY) // 'burned-acres-timings_' // FOUR_IRANK_WORLD // '.csv'
         INQUIRE(UNIT=LUBAT+IRANK_WORLD,OPENED=LOPEN)
         IF (.NOT. LOPEN) OPEN(LUBAT+IRANK_WORLD,FILE=TRIM(FN),FORM='FORMATTED',STATUS='REPLACE',IOSTAT=IOS)
         DO I = 1, NUM_TIME_AT_BURNED_ACRES
            IF (ALREADY_REACHED_BURNED_ACRES(I)) CYCLE
            IF (ACRES .LT. TIME_AT_BURNED_ACRES(I)) CYCLE
            ALREADY_REACHED_BURNED_ACRES(I) = .TRUE.
            CALL SYSTEM_CLOCK(IT2_LSP)
            RUNTIME = (IT2_LSP - IT1_LSP) / REAL(CLOCK_COUNT_RATE)

            IF (T .GT. TSTOP .AND. STATS_SIMULATION_TSTOP_HOURS(ICASE) .GT. 0) THEN
               WRITE(LUBAT+IRANK_WORLD, 777) ICASE, TIME_AT_BURNED_ACRES(I), STATS_SIMULATION_TSTOP_HOURS(ICASE), RUNTIME
            ELSE
               WRITE(LUBAT+IRANK_WORLD, 777) ICASE, TIME_AT_BURNED_ACRES(I), T/3600.0, RUNTIME
            ENDIF
         ENDDO
      ENDIF
      777 FORMAT (I7, ',', F8.1, ',', F7.2, ',', F8.3)

      ! Extended attack model
      ! new suppression model :: modified below
      IF (ITIMESTEP .EQ. 1) THEN
         IF (EXTENDED_ATTACK_MODEL .EQ. 0) T_LAST_EXTENDED_ATTACK = T
         IF (EXTENDED_ATTACK_MODEL .EQ. 1) T_LAST_EXTENDED_ATTACK = T
      ENDIF
#ifdef _SUPPRESSION   
   ! new suppression model :: modified below
      IF (ENABLE_EXTENDED_ATTACK) THEN
         IF (EXTENDED_ATTACK_MODEL .EQ. 0) THEN
            IF (T - T_LAST_EXTENDED_ATTACK .GT. DT_EXTENDED_ATTACK .AND. LIST_BURNED%NUM_NODES .GT. 0) THEN
               IT_EA = IT_EA + 1
               DT_DAY = (T - T_LAST_EXTENDED_ATTACK) / 86400.
               SUPP(IT_EA)%T         = T
               SUPP(IT_EA)%ACRES     = ACRES
               SUPP(IT_EA)%ACRES_SDI = ACRES_SDI

               SUPP(IT_EA)%DADT    = (ACRES     - SUPP(IT_EA-1)%ACRES    ) / DT_DAY
               SUPP(IT_EA)%DASDIDT = (ACRES_SDI - SUPP(IT_EA-1)%ACRES_SDI) / DT_DAY
               IF (SUPP(IT_EA)%DADT .GT. 0.) THEN
                  SUPP(IT_EA)%SDIBAR = MIN(MAX(SUPP(IT_EA)%DASDIDT / SUPP(IT_EA)%DADT - 1.0, 0.0), 3.0)
               ELSE
                  SUPP(IT_EA)%SDIBAR = 0
               ENDIF
               ! IF ( ABS(SUPP(IT_EA)%DADT) .LT. 1E-6 ) THEN
               !    SUPP(IT_EA)%DC_PER_DAY = 0.
               ! ELSE
               IF (USE_SDI_LOG_FUNCTION) THEN
                  SUPP(IT_EA)%DC_PER_DAY = 0.01 * DIURNAL_ADJUSTMENT_FACTOR * MAX_CONTAINMENT_PER_DAY * (1. - LOG10(SUPP(IT_EA)%DADT) / LOG10(AREA_NO_CONTAINMENT_CHANGE) )
               ELSE
                  SUPP(IT_EA)%DC_PER_DAY = 0.01 * DIURNAL_ADJUSTMENT_FACTOR * MAX_CONTAINMENT_PER_DAY * (1. - SUPP(IT_EA)%DADT        / AREA_NO_CONTAINMENT_CHANGE       )
               ENDIF
               ! ENDIF
               IF (SUPP(IT_EA)%DC_PER_DAY .GT. 0.) THEN
                  SUPP(IT_EA)%DC_PER_DAY = SUPP(IT_EA)%DC_PER_DAY * EXP(-B_SDI * SUPP(IT_EA)%SDIBAR)
               ELSE
                  SUPP(IT_EA)%DC_PER_DAY = SUPP(IT_EA)%DC_PER_DAY * EXP( B_SDI * SUPP(IT_EA)%SDIBAR)
               ENDIF
               
               SUPP(IT_EA)%TARGET_CONTAINMENT = SUPP(IT_EA-1)%TARGET_CONTAINMENT  + SUPP(IT_EA)%DC_PER_DAY * DT_DAY
               IF (SUPP(IT_EA)%TARGET_CONTAINMENT .GT. 1. ) SUPP(IT_EA)%TARGET_CONTAINMENT = 1.
               IF (SUPP(IT_EA)%TARGET_CONTAINMENT .LT. 0. ) SUPP(IT_EA)%TARGET_CONTAINMENT = 0.

               print *, T, ACRES, SUPP(IT_EA)%TARGET_CONTAINMENT, SUPP(IT_EA)%DC_PER_DAY, SUPP(IT_EA)%DADT
               
               CALL CENTROID(IT_EA)
               CALL CONTAINMENT(IT_EA,T)
               CALL UNTAG_CELLS(NX,NY,TIME_OF_ARRIVAL,T,SURFACE_FIRE)
               T_LAST_EXTENDED_ATTACK = T

            ENDIF

         ELSE IF (EXTENDED_ATTACK_MODEL .EQ. 1) THEN
            IF (ENABLE_INDIRECT_ATTACK .AND. ((T / EXTENDED_ATTACK_TIME) ** INITIAL_CONTAINMENT_SHAPE_FACTOR) .GE. 1.0) THEN
               CALL DETECT_FIRELINE
               CALL INDIRECT_ATTACK(NX, NY, T)
               CALL UNTAG_CELLS(NX,NY,TIME_OF_ARRIVAL,T,SURFACE_FIRE)

               C => LIST_TAGGED%HEAD
               DO I = 1, LIST_TAGGED%NUM_NODES
                  C%FIRE_LINE = .FALSE.
                  C => C%NEXT
               ENDDO
            ENDIF
            IF (T - T_LAST_EXTENDED_ATTACK .GT. DT_EXTENDED_ATTACK) THEN

               IT_EA = IT_EA + 1
               SUPP(IT_EA)%T = T

               IF (IT_EA .EQ. 1) THEN
                  DO I = 1, LIST_SUPPRESSED%NUM_NODES
                     SUPP(IT_EA)%INDIRECT_SUPPRESSED_FIRELINE_LENGTH = SUPP(IT_EA)%INDIRECT_SUPPRESSED_FIRELINE_LENGTH + ANALYSIS_CELLSIZE
                  ENDDO
               ELSE
                  I = IT_EA
                  DO WHILE (I .GE. 2)
                     SUPP(IT_EA)%INDIRECT_SUPPRESSED_FIRELINE_LENGTH = SUPP(IT_EA)%INDIRECT_SUPPRESSED_FIRELINE_LENGTH - SUPP(I-1)%INDIRECT_SUPPRESSED_FIRELINE_LENGTH
                     I = I-1
                  ENDDO
                  DO I = 1, LIST_SUPPRESSED%NUM_NODES
                     SUPP(IT_EA)%INDIRECT_SUPPRESSED_FIRELINE_LENGTH = SUPP(IT_EA)%INDIRECT_SUPPRESSED_FIRELINE_LENGTH + ANALYSIS_CELLSIZE
                  ENDDO
               ENDIF


               CALL SEGMENT_FIRELINE
               CALL CALCULATE_STS
               CALL SORT_STS
               CALL DIRECT_ATTACK(T, IT_EA, rank_finished, DT, ICASE, TSTOP)
               
               
               ! CALL LL_DUMP_ROUTINE(LIST_TAGGED,"time_suppressed",T,"time_suppressed",1)
               ! CALL LL_DUMP_ROUTINE(LIST_TAGGED,"ROS",T,"ROS",1)
               ! CALL LL_DUMP_ROUTINE(LIST_TAGGED,"STS",T,"STS",1)
               ! CALL LL_DUMP_ROUTINE(LIST_TAGGED,"SEGMENT",T,"SEGMENT",1)
               ! CALL LL_DUMP_ROUTINE(LIST_TAGGED,"time_of_arrival",T,"time_of_arrival",1)

               DEALLOCATE(SUPPRESSION_TYPE_SCORE)
               DEALLOCATE(SUPPRESSION_TYPE_SCORE_RANK)

               
               C => LIST_TAGGED%HEAD
               DO I = 1, LIST_TAGGED%NUM_NODES
                  C%FIRE_LINE = .FALSE.
                  C%SEGMENT_GROUP = -1
                  C%STS = -1
                  C => C%NEXT
               ENDDO

               T_LAST_EXTENDED_ATTACK = T
               CALL UNTAG_CELLS(NX,NY,TIME_OF_ARRIVAL,T,SURFACE_FIRE)
               ! CALL LL_DUMP_ROUTINE(LIST_SUPPRESSED,"time_suppressed",T,"time_suppressed",1)
               
            ENDIF
         ELSE
            WRITE(*,*) 'Error: "EXTENDED_ATTACK_MODEL" should be 0 or 1 in namelist!'
            STOP
         ENDIF
      ENDIF
#endif   

      CALL ACCUMULATE_CPU_USAGE(51, IT1, IT2)

#ifdef _SMOKE
      IF (ENABLE_SMOKE_OUTPUTS .AND. T - T_LAST_SMOKE_OUTPUT .GE. DT_SMOKE_OUTPUTS .AND. LIST_BURNED%NUM_NODES .gt. 0) THEN

         C => LIST_BURNED%HEAD
         TOTALENERGY = 0
         STATS_PM2P5_RELEASE(ICASE) = 0
         CELLENERGYRELEASE = 0
         XCEN = 0
         YCEN = 0
         DO I = 1, LIST_BURNED%NUM_NODES
            IF (C%TIME_IGNITED .GE. T_LAST_SMOKE_OUTPUT .AND. C%VELOCITY .GT. 0.01) THEN
               CELLTIME = C%TIME_IGNITED - T_LAST_SMOKE_OUTPUT 
               WET_WOOD_CALORIFIC_VALUE = DRY_WOOD_CALORIFIC_VALUE*(1-C%M10)-2.44*C%M10
               CELLENERGYRELEASE = 0.06*(C%FLIN_SURFACE + C%FLIN_CANOPY) * ASP%CELLSIZE**2 / (C%VELOCITY * 0.3048 / 60)
               PM_FLAMING_MAX = PM_EMISSION_FACTOR_FLAMING * CELLENERGYRELEASE / WET_WOOD_CALORIFIC_VALUE 
               PM_SMOLDERING_MAX = 0.43 * PM_EMISSION_FACTOR_SMOLDERING * CELLENERGYRELEASE / WET_WOOD_CALORIFIC_VALUE

               IF (CELLTIME .LE. DT_SMOKE_OUTPUTS - FLAMING_TIME - SMOLDERING_TIME) THEN ! EXTINGUISHED
                  STATS_PM2P5_RELEASE(ICASE) = STATS_PM2P5_RELEASE(ICASE) + PM_FLAMING_MAX + PM_SMOLDERING_MAX
                  TOTALENERGY = TOTALENERGY + CELLENERGYRELEASE * 1.43
               ELSE IF (CELLTIME .LE. DT_SMOKE_OUTPUTS - FLAMING_TIME) THEN ! STILL SMOLDERING
                  STATS_PM2P5_RELEASE(ICASE) = STATS_PM2P5_RELEASE(ICASE) + PM_FLAMING_MAX + PM_SMOLDERING_MAX * (DT_SMOKE_OUTPUTS - CELLTIME) / SMOLDERING_TIME
                  TOTALENERGY = TOTALENERGY + CELLENERGYRELEASE * (1 + (DT_SMOKE_OUTPUTS - CELLTIME) / SMOLDERING_TIME)
               ELSE ! STILL FLAMING
                  STATS_PM2P5_RELEASE(ICASE) = STATS_PM2P5_RELEASE(ICASE) + PM_FLAMING_MAX * (DT_SMOKE_OUTPUTS - CELLTIME) / FLAMING_TIME
                  TOTALENERGY = TOTALENERGY + CELLENERGYRELEASE * (DT_SMOKE_OUTPUTS - CELLTIME) / SMOLDERING_TIME
               ENDIF
               XCEN = XCEN + C%IX
               YCEN = YCEN + C%IY
            ENDIF 
            C => C%NEXT
         ENDDO

         IXCEN = NINT(XCEN/LIST_BURNED%NUM_NODES)
         IYCEN = NINT(YCEN/LIST_BURNED%NUM_NODES)

         XCEN = X_FROM_ICOL (IXCEN, ADJ%XLLCORNER, ADJ%CELLSIZE)
         YCEN = Y_FROM_IROW (IYCEN, ADJ%YLLCORNER, ADJ%CELLSIZE)

         T_LAST_SMOKE_OUTPUT = REAL (NINT ( T / DT_SMOKE_OUTPUTS)) * DT_SMOKE_OUTPUTS

         LAT = 0
         LON = 0
         CALL XY_TO_LATLON(XCEN, YCEN, LAT, LON)

         IF (DUMP_SMOKE_OUTPUTS) THEN
            FN = TRIM(OUTPUTS_DIRECTORY) // 'smoke_' // SEVEN_ICASE // '.csv'
            INQUIRE(UNIT=LUSMOKE+IRANK_WORLD,OPENED=LOPEN)
            IF (.NOT. LOPEN) THEN
               OPEN(LUSMOKE+IRANK_WORLD,FILE=TRIM(FN),FORM='FORMATTED',STATUS='REPLACE',IOSTAT=IOS)
               WRITE(LUSMOKE+IRANK_WORLD,'(A)') 't (h),timestamp,xcen (m),ycen (m),area (ha),Average HRR (MW),mdotsmoke (g/h)'
            ENDIF
            HOUR_OF_YEAR = NINT( REAL(BAND_ONE_HOUR_OF_YEAR + IWX_BAND) + T_LAST_SMOKE_OUTPUT/3600. - 1.)
            TIMESTAMP = HOUR_OF_YEAR_TO_TIMESTAMP (CURRENT_YEAR, HOUR_OF_YEAR)
            WRITE(LUSMOKE+IRANK_WORLD,999) T_LAST_SMOKE_OUTPUT/3600.0, TIMESTAMP, XCEN, YCEN, ACRES*0.404686, &
                           TOTALENERGY * 0.001 / DT_SMOKE_OUTPUTS, STATS_PM2P5_RELEASE(ICASE)/DT_SMOKE_OUTPUTS
         ENDIF
         HOUR_OF_YEAR = NINT( REAL(BAND_ONE_HOUR_OF_YEAR + IWX_BAND) + T_LAST_SMOKE_OUTPUT/3600. - 1.)
         TIMESTAMP = HOUR_OF_YEAR_TO_TIMESTAMP (CURRENT_YEAR, HOUR_OF_YEAR)

         IF (DUMP_EMITIMES) THEN

            FN = TRIM(OUTPUTS_DIRECTORY)//'EMITIMES.txt'

            INQUIRE(UNIT=LUEMIT+IRANK_WORLD, OPENED=LOPEN)
            
            TIMESTAMP = HOUR_OF_YEAR_TO_TIMESTAMP (CURRENT_YEAR, HOUR_OF_YEAR)
            READ(TIMESTAMP, '(I4,1X,I2,1X,I2,1X,I2.2)') YEAR, MONTH, DAY_OF_MONTH, HOUR

            IF (.NOT. LOPEN) THEN
               OPEN(LUEMIT+IRANK_WORLD, FILE=TRIM(FN), FORM='FORMATTED', STATUS='REPLACE', IOSTAT=IOS)

               WRITE(LUEMIT+IRANK_WORLD,'(A)') 'YYYY MM DD HH DURATION(hhhh) RECORDS'
               WRITE(LUEMIT+IRANK_WORLD,'(A)') 'YYYY MM DD HH MM DURATION(hhmm) LAT LON  HGT(m)  RATE(/h)  AREA(m2)   HEAT(w)'

            ENDIF 
            XCEN = ICOL_FROM_X(XCEN, ASP%XLLCORNER, ASP%CELLSIZE)
            YCEN = IROW_FROM_Y(YCEN, ASP%YLLCORNER, ASP%CELLSIZE)

            WRITE(LUEMIT+IRANK_WORLD,'(I4,1X,I2.2,1X,I2.2,1X,I2.2,1X,I4.4,1X,"1")') &
               YEAR, MONTH, DAY_OF_MONTH, HOUR, &
               NINT(DT_SMOKE_OUTPUTS/3600)

            WRITE(LUEMIT+IRANK_WORLD, &
               '(I4, 1X, I2.2, 1X, I2.2, 1X, I2.2, 1X, "00" , 1X, I2.2, I2.2, 1X, F8.3, 1X, F9.3, 1X, F6.1, 1X, 3ES11.3)')     &
               YEAR, MONTH, DAY_OF_MONTH, HOUR, NINT(DT_SMOKE_OUTPUTS/3600), MOD(NINT(DT_SMOKE_OUTPUTS/60),60),  &
               LAT, LON, DEM%R4(NINT(XCEN),NINT(YCEN),1),                                               &
               STATS_PM2P5_RELEASE(ICASE) / DT_SMOKE_OUTPUTS, ACRES*4047, TOTALENERGY * 0.001 / DT_SMOKE_OUTPUTS

         ENDIF
      ENDIF ! ENABLE_SMOKE_OUTPUTS

#endif

#ifdef _WUI
      ! Update the transient HRRPUA for all burning cells within the residence time dx/V_VMS_SURFACE, for complete transient HRRPUA field
      IF (DUMP_HRR_TRANSIENT .AND. LIST_BURNED%NUM_NODES .GT. 0) THEN
         C => LIST_BURNED%HEAD
         DO I = 1, LIST_BURNED%NUM_NODES
            IF (C%IFBFM .NE. 91 .AND. &
                C%TIME_OF_ARRIVAL .GT. SIMULATION_TSTART) CALL HRR_TRANSIENT(C, T)
            C => C%NEXT
         ENDDO
      ENDIF
#endif
#ifndef _WUI
      IF (DUMP_HRR_TRANSIENT .AND. LIST_BURNED%NUM_NODES .GT. 0) THEN
         C => LIST_BURNED%HEAD
         DO I = 1, LIST_BURNED%NUM_NODES
            IX = C%IX
            IY = C%IY
            IF (PHIP(IX,IY) .LE. 0.) THEN
               IF (C%IFBFM .NE. 91 .AND. &
                   T - TIME_OF_ARRIVAL(IX,IY) .LT. ANALYSIS_CELLSIZE/MAX(1E-5, C%VELOCITY_DMS_SURFACE*0.00508)) THEN
                  HRR_TRANSIENT_MAP(IX,IY) = C%HRRPUA
               ELSE
                  HRR_TRANSIENT_MAP(IX,IY) = 0.
               ENDIF
            ENDIF
            C => C%NEXT
         ENDDO
      ENDIF
#endif

      999 FORMAT(F9.2,',',A,',',F10.1,',',F10.1,',',E12.5,',',E12.5,',',E12.5)

      CALL ACCUMULATE_CPU_USAGE(52, IT1, IT2)

      !   IF (T .GE. TSTOP) THEN
      !      CALL LL_DUMP_ROUTINE(LIST_SUPPRESSED,'time_suppressed',T,'time_suppressed',ICASE) 
      !      CALL LL_DUMP_ROUTINE(LIST_BURNED,'time_of_arrival',T,'time_of_arrival',ICASE) 
      !   ENDIF

      CALL ACCUMULATE_CPU_USAGE(53, IT1, IT2)

      CALL SYSTEM_CLOCK(COUNT_END)
      ELAPSED_TIME = REAL(COUNT_END - COUNT_START, 8) / REAL(CLOCK_COUNT_RATE, 8)

      IF (ELAPSED_TIME .GT. MAX_RUNTIME) THEN
         WRITE(LOG_MSG,'(A,I0,A,F10.1,A,F10.1)') '[',ICASE,'] STOPPED: ELAPSED TIME ',ELAPSED_TIME,' GREATER THAN MAX_RUNTIME ',MAX_RUNTIME
         WRITE(*,'(A)') TRIM(LOG_MSG)
         SIMULATION_TSTOP_HOURS = T / 3600.
         STATS_SIMULATION_TSTOP_HOURS(ICASE) = SIMULATION_TSTOP_HOURS
         rank_finished = 1
         DT = DT_METEOROLOGY
      ENDIF

      ! check if propagation has stalled for an early exit
      if ( ALL(ALREADY_IGNITED(1:MIN(NUM_IGNITIONS,100))) .AND. &
           ( (.NOT. ENABLE_SPOTTING) .OR. USE_SUPERSEDED_SPOTTING .OR. &
             (TRIM(ACCUMULATION_MODEL) .EQ. 'EULERIAN'   .AND. LIST_EMBER_TRACKER%NUM_NODES .LT. 1) .OR. &
             (TRIM(ACCUMULATION_MODEL) .EQ. 'LAGRANGIAN' .AND. NUM_TRACKED_EMBERS .LT. 1) ) ) then
         if (all(abs(PHIP - phi_previous) .lt. 0.001)) then
            WRITE(LOG_MSG,'(A,I0,A,F10.1,A,F10.1)') '[',ICASE,'] STOPPED: FIRE FRONT PROPAGATION STALLED'
            WRITE(*,'(A)') TRIM(LOG_MSG)
            SIMULATION_TSTOP_HOURS = T / 3600.
            STATS_SIMULATION_TSTOP_HOURS(ICASE) = SIMULATION_TSTOP_HOURS
            rank_finished = 1
            DT = DT_METEOROLOGY
         endif
      endif

      phi_previous = PHIP

      CALL ACCUMULATE_CPU_USAGE(54, IT1, IT2)

      IS_FINAL_DUMP = T .GE. TSTOP
      IF (DUMP_EVERY_STEP) THEN
         IDUMP_OUTPUT = IDUMP_OUTPUT + 1
         CALL MAIN_DUMP_ROUTINE(IS_FINAL_DUMP, IDUMP_OUTPUT, ICASE, T, ACRES)
      ELSEIF (T .GE. NEXT_DUMP_TIME) THEN
         ! Assume DT is always less than DTDUMP, so we won't miss the dump time step
         IDUMP_OUTPUT = IDUMP_OUTPUT + 1
         CALL MAIN_DUMP_ROUTINE(IS_FINAL_DUMP, IDUMP_OUTPUT, ICASE, T, ACRES)
         NEXT_DUMP_TIME = NEXT_DUMP_TIME + DTDUMP
      ENDIF

#ifdef _WUI
      IF (USE_BLDG_SPREAD_MODEL .AND. (BLDG_SPREAD_MODEL_TYPE .EQ. 2)) THEN
         IF (DUMP_FUEL_CONSUMPTION) CALL CALC_FUEL_CONSUMPTION(DT, NX, NY)
         ! Untag excessive wui nodes
         CALL UNTAG_CELLS_WUI(T, NX, NY)
         ! Reset the transient heat flux.
         TRANSIENT_DFC_WUI(:,:) = 0.
         TRANSIENT_RADIATION_WUI(:,:) = 0.
         HRR_TRANSIENT_MAP(:,:) = 0.
         TEST_INTERFACE_WUI(:,:) = .FALSE.
         WTU_SPREAD_WUI(:,:) = .FALSE.
      ENDIF
#endif
   ENDIF

   ! if (ICASE .ge. 9945) then
   !    WRITE(LOG_MSG,'(A,I0,A,F12.1)') '[',ICASE,'] PASSED MAIN BODY, T IS ',T
   !    WRITE(*,'(A)') TRIM(LOG_MSG)
   ! endif

   T = T + DT

   IF ((T .ge. TSTOP) .and. START_CALCS) THEN ! END SIM
      if (FEEDBACK_LEVEL .ge. 3) then
         WRITE(LOG_MSG,'(A,I0,A,F12.1)') '[',ICASE,'] LEVEL SET CASE OUTPUT STARTED AT T ',T
         WRITE(*,'(A)') TRIM(LOG_MSG)
      endif
      CALL SYSTEM_CLOCK(IT1)

      NTIMESTEPS = ITIMESTEP
      IDUMP_OUTPUT = IDUMP_OUTPUT + 1
      CALL MAIN_DUMP_ROUTINE(.TRUE., IDUMP_OUTPUT, ICASE, T, ACRES)

      CALL ACCUMULATE_CPU_USAGE(55, IT1, IT2)

      IF (PROCESS_TIMED_LOCATIONS) THEN
         FN = TRIM(OUTPUTS_DIRECTORY) // 'timed-locations-events_' // FOUR_IRANK_WORLD // '.csv'
         INQUIRE(UNIT=LUTASL+IRANK_WORLD,OPENED=LOPEN)
         IF (.NOT. LOPEN) OPEN(LUTASL+IRANK_WORLD,FILE=TRIM(FN),FORM='FORMATTED',STATUS='REPLACE',IOSTAT=IOS)

         C => LIST_BURNED%HEAD
         DO I = 1, LIST_BURNED%NUM_NODES
            DO J = 1, NUM_TIMED_LOCATIONS
               IF (C%IX .NE. TIMED_LOCATIONS_TRACKER(J)%IX ) CYCLE
               IF (C%IY .NE. TIMED_LOCATIONS_TRACKER(J)%IY ) CYCLE
               WRITE(LUTASL+IRANK_WORLD,888) ICASE, TIMED_LOCATIONS_TRACKER(J)%ID, C%TIME_OF_ARRIVAL / 3600.
            ENDDO
            C => C%NEXT
         ENDDO
      ENDIF
      888 FORMAT (I7, ',', I8, ',', F7.2)

      ! Calculate fire area
      IF (DUMP_FIRE_SIZE_STATS) THEN
         STATS_SURFACE_FIRE_AREA(ICASE) = 0.
         STATS_CROWN_FIRE_AREA  (ICASE) = 0.
         STATS_FIRE_VOLUME      (ICASE) = 0.

         C => LIST_BURNED%HEAD
         DO I = 1, LIST_BURNED%NUM_NODES
            IX = C%IX
            IY = C%IY
            STATS_SURFACE_FIRE_AREA(ICASE)   = STATS_SURFACE_FIRE_AREA(ICASE)   + 1.
            STATS_FIRE_VOLUME(ICASE) = STATS_FIRE_VOLUME(ICASE) + C%FLAME_LENGTH
            IF (C%CROWN_FIRE .GT. 0) STATS_CROWN_FIRE_AREA(ICASE) = STATS_CROWN_FIRE_AREA(ICASE) + 1.
            C => C%NEXT
         ENDDO
         
         STATS_SURFACE_FIRE_AREA(ICASE) = ACRES_PER_PIXEL * STATS_SURFACE_FIRE_AREA(ICASE)
         STATS_FIRE_VOLUME      (ICASE) = ACRES_PER_PIXEL * STATS_FIRE_VOLUME      (ICASE)
         STATS_CROWN_FIRE_AREA  (ICASE) = ACRES_PER_PIXEL * STATS_CROWN_FIRE_AREA  (ICASE)

         IF (USE_POPULATION_DENSITY) THEN
            STATS_AFFECTED_POPULATION(ICASE) = 0.0
            C => LIST_BURNED%HEAD
            DO I = 1, LIST_BURNED%NUM_NODES
               IX = C%IX
               IY = C%IY
               IF (POPULATION_DENSITY%R4(IX,IY,1) .LE. 0.) THEN
                  C => C%NEXT
                  CYCLE 
               ENDIF
               STATS_AFFECTED_POPULATION(ICASE) = STATS_AFFECTED_POPULATION(ICASE) + POPULATION_DENSITY%R4(IX,IY,1)
               C => C%NEXT
            ENDDO

            IF (ESTIMATE_URBAN_LOSSES) THEN
               C => LIST_BURNED%HEAD
               DO K = 1, LIST_BURNED%NUM_NODES
                  IX = C%IX
                  IY = C%IY
                  DO J = MAX(1,IY-1), MIN(NY,IY+1)
                  DO I = MAX(1,IX-1), MIN(NX,IX+1)
                     IF ( (FBFM%I2(I,J,1) .EQ. 91) .AND. (POPULATION_DENSITY%R4(I,J,1) .GT. 0.) ) THEN
                        SURFACE_FIRE(I,J) = 1

                        CALL APPEND(LIST_BURNED, I, J, T)
                        
                        STATS_SURFACE_FIRE_AREA(ICASE) = STATS_SURFACE_FIRE_AREA(ICASE) + ACRES_PER_PIXEL
                        STATS_AFFECTED_POPULATION(ICASE) = STATS_AFFECTED_POPULATION(ICASE) + POPULATION_DENSITY%R4(I,J,1)
                     ENDIF
                  ENDDO !I
                  ENDDO !J
                  C => C%NEXT
               ENDDO
            ENDIF !ESTIMATE_URBAN_LOSSES

      ! Population density is per acre
            STATS_AFFECTED_POPULATION(ICASE) = STATS_AFFECTED_POPULATION(ICASE)  * ACRES_PER_PIXEL
         ELSE
            STATS_AFFECTED_POPULATION(ICASE) = -1.0 
         ENDIF

         IF (USE_REAL_ESTATE_VALUE) THEN
            STATS_AFFECTED_REAL_ESTATE_VALUE(ICASE) = 0.0
            C => LIST_BURNED%HEAD
            DO I = 1, LIST_BURNED%NUM_NODES
               IX = C%IX
               IY = C%IY
               IF (REAL_ESTATE_VALUE%R4(IX,IY,1) .LE. 0.) THEN
                  C => C%NEXT
                  CYCLE 
               ENDIF
               STATS_AFFECTED_REAL_ESTATE_VALUE(ICASE) = STATS_AFFECTED_REAL_ESTATE_VALUE(ICASE) + REAL_ESTATE_VALUE%R4(IX,IY,1)
               C => C%NEXT
            ENDDO
            
      ! Since each cell as units of sq m, we have to convert:
            STATS_AFFECTED_REAL_ESTATE_VALUE(ICASE) = STATS_AFFECTED_REAL_ESTATE_VALUE(ICASE) * ACRES_PER_PIXEL
         ELSE
            STATS_AFFECTED_REAL_ESTATE_VALUE(ICASE) = -1.0 
         ENDIF

         IF (USE_LAND_VALUE) THEN
            STATS_AFFECTED_LAND_VALUE(ICASE) = 0.0
            C => LIST_BURNED%HEAD
            DO I = 1, LIST_BURNED%NUM_NODES
               IX = C%IX
               IY = C%IY
               IF (LAND_VALUE%R4(IX,IY,1) .LE. 0.) THEN
                  C => C%NEXT
                  CYCLE 
               ENDIF
               IF (C%CROWN_FIRE .GE. 1) STATS_AFFECTED_LAND_VALUE(ICASE) = STATS_AFFECTED_LAND_VALUE(ICASE) + LAND_VALUE%R4(IX,IY,1)
               C => C%NEXT
            ENDDO
      ! Since each cell as units of sq m, we have to convert:
            STATS_AFFECTED_LAND_VALUE(ICASE) = STATS_AFFECTED_LAND_VALUE(ICASE)  * ACRES_PER_PIXEL
         ELSE
            STATS_AFFECTED_LAND_VALUE(ICASE) = -1.0 
         ENDIF

      ENDIF

      CALL ACCUMULATE_CPU_USAGE(56, IT1, IT2)

      IF (DUMP_BINARY_OUTPUTS .AND. ACRES .GT. MINIMUM_AREA_FOR_BINARY_OUTPUTS) THEN
         CALL RANDOM_NUMBER(R0)
         IF (R0 .LT. BINARY_OUTPUTS_DUMP_FRACTION) THEN
            LU=LUOUTPUT + IRANK_WORLD
            FN = TRIM(OUTPUTS_DIRECTORY) // 'toa_' // FOUR_IWX_BAND // '_' // SEVEN_ICASE // '.bin'
            OPEN(LU, FILE=TRIM(FN), FORM='UNFORMATTED', ACCESS='SEQUENTIAL', STATUS='REPLACE') 
            WRITE(LU) LIST_BURNED%NUM_NODES
            WRITE(LU) (BINARY_OUTPUTS_IX(I), I=1, LIST_BURNED%NUM_NODES)
            WRITE(LU) (BINARY_OUTPUTS_IY(I), I=1, LIST_BURNED%NUM_NODES)

            IF (FULL_BINARY_OUTPUTS) THEN
               WRITE(LU) (BINARY_OUTPUTS_TOA          (I), I=1, LIST_BURNED%NUM_NODES)
               WRITE(LU) (BINARY_OUTPUTS_FLAME_LENGTH (I), I=1, LIST_BURNED%NUM_NODES)
               WRITE(LU) (BINARY_OUTPUTS_VELOCITY_FPM (I), I=1, LIST_BURNED%NUM_NODES)
               WRITE(LU) (BINARY_OUTPUTS_CROWN_FIRE   (I), I=1, LIST_BURNED%NUM_NODES)
            ENDIF
            CLOSE(LU)
         ENDIF
      ENDIF

      CALL ACCUMULATE_CPU_USAGE(57, IT1, IT2)

      IF (USE_EMBER_COUNT_BINS) THEN
         IF (ALLOCATED(EMBER_OUTPUTS_IX)) THEN
            DEALLOCATE (EMBER_OUTPUTS_IX)
            DEALLOCATE (EMBER_OUTPUTS_IY)
            DEALLOCATE (EMBER_OUTPUTS_COUNT)
         ENDIF

         ALLOCATE(EMBER_OUTPUTS_IX   (1:INT(STATS_NEMBERS(ICASE))))
         ALLOCATE(EMBER_OUTPUTS_IY   (1:INT(STATS_NEMBERS(ICASE))))
         ALLOCATE(EMBER_OUTPUTS_COUNT(1:INT(STATS_NEMBERS(ICASE))))

      ICOUNT=0
      DO IY = 1, NY
      DO IX = 1, NX
         IF (EMBER_COUNT(IX,IY) .GT. 0) THEN
            ICOUNT = MIN(ICOUNT + 1, INT(STATS_NEMBERS(ICASE)))
            EMBER_OUTPUTS_IX   (ICOUNT) = INT(IX, KIND=KIND(EMBER_OUTPUTS_IX(1)))
            EMBER_OUTPUTS_IY   (ICOUNT) = INT(IY, KIND=KIND(EMBER_OUTPUTS_IY(1)))
            EMBER_OUTPUTS_COUNT(ICOUNT) = EMBER_COUNT(IX,IY)
            EMBER_COUNT(IX,IY) = 0
         ENDIF
      ENDDO
      ENDDO

         IF (ICOUNT .LT. INT(STATS_NEMBERS(ICASE))) THEN
            EMBER_OUTPUTS_COUNT(ICOUNT+1:) = 0
            EMBER_OUTPUTS_IX   (ICOUNT+1:) = 0
            EMBER_OUTPUTS_IY   (ICOUNT+1:) = 0
         ENDIF

      ENDIF

      CALL ACCUMULATE_CPU_USAGE(58, IT1, IT2)

      DO I = 1, NUM_EVERTAGGED
         IX = EVERTAGGED_IX(I)
         IY = EVERTAGGED_IY(I)
         SURFACE_FIRE   (IX,IY) = 0
         TAGGED         (IX,IY) = .FALSE.
         TIME_OF_ARRIVAL(IX,IY) = -1.
         EVERTAGGED(IX,IY) = .FALSE.
         IF (RANDOM_IGNITIONS) PHIP (IX,IY) = 1
      ENDDO

      CALL ACCUMULATE_CPU_USAGE(59, IT1, IT2)

      ! Close smoke file
#ifdef _SMOKE
      IF (ENABLE_SMOKE_OUTPUTS .AND. DUMP_SMOKE_OUTPUTS) THEN
         INQUIRE(UNIT=LUSMOKE+IRANK_WORLD,OPENED=LOPEN)
         IF (LOPEN) CLOSE(LUSMOKE+IRANK_WORLD)
      ENDIF

      IF (ENABLE_SMOKE_OUTPUTS .AND. DUMP_EMITIMES) THEN
         INQUIRE(UNIT=LUEMIT+IRANK_WORLD,OPENED=LOPEN)
         IF (LOPEN) CLOSE(LUEMIT+IRANK_WORLD)
      ENDIF
#endif

      ! Close spotting file
      IF (ENABLE_SPOTTING .AND. DUMP_SPOTTING_OUTPUTS) THEN
         INQUIRE(UNIT=LUSPOT+IRANK_WORLD,OPENED=LOPEN)
         IF (LOPEN) CLOSE(LUSPOT+IRANK_WORLD)
      ENDIF


      ! Close virtual station file
      IF (NUM_VIRTUAL_STATIONS .GT. 0) THEN
         INQUIRE(UNIT=LUNODES+IRANK_WORLD,OPENED=LOPEN)
         IF (LOPEN) CLOSE(LUNODES+IRANK_WORLD)
      ENDIF

      ! Deallocate linked lists
      IF (LIST_TAGGED%NUM_NODES .GT.     0) THEN
         CALL TIDY(LIST_TAGGED)
         LIST_TAGGED%NUM_NODES=0
      ENDIF

      LIST_BURNED%NUM_NODES_PREVIOUS = LIST_BURNED%NUM_NODES
      IF (LIST_BURNED%NUM_NODES .GT.     0) THEN
         CALL TIDY(LIST_BURNED)
         LIST_BURNED%NUM_NODES=0
      !   C => LIST_BURNED%HEAD
      !   DO I = 1, LIST_BURNED%NUM_NODES - 1
      !      CALL DELETE_NODE(LIST_BURNED, C)
      !      C => C%NEXT
      !   ENDDO
      ENDIF

      IF (LIST_SUPPRESSED%NUM_NODES .GT. 0) THEN 
         CALL TIDY(LIST_SUPPRESSED)
         LIST_SUPPRESSED%NUM_NODES=0
      !   C => LIST_SUPPRESSED%HEAD
      !   DO I = 1, LIST_SUPPRESSED%NUM_NODES - 1
      !      CALL DELETE_NODE(LIST_SUPPRESSED, C)
      !      C => C%NEXT
      !   ENDDO
      !   LIST_SUPPRESSED%NUM_NODES=0
      ENDIF

      CALL ACCUMULATE_CPU_USAGE(60, IT1, IT2)

      IF (SIMULATION_TSTOP_HOURS .LT. 0. ) STATS_SIMULATION_TSTOP_HOURS(ICASE) = T / 3600.
#ifdef _SUPPRESSION   
      ! new suppression model :: modified below  
      IF (ENABLE_EXTENDED_ATTACK) THEN
         IF (EXTENDED_ATTACK_MODEL .EQ. 0) THEN
            IF (STATS_FINAL_CONTAINMENT_FRAC(ICASE) .LT. 0.) STATS_FINAL_CONTAINMENT_FRAC(ICASE) = SUPP(IT_EA)%TARGET_CONTAINMENT
         ELSE IF (EXTENDED_ATTACK_MODEL .EQ. 1) THEN
            IF (STATS_FINAL_CONTAINMENT_FRAC(ICASE) .LT. 0.) STATS_FINAL_CONTAINMENT_FRAC(ICASE) = SUPP(IT_EA)%CONTAINMENT
         ELSE
            WRITE(*,*) 'Error: "EXTENDED_ATTACK_MODEL" should be 0 or 1 in namelist!'
            STOP
         ENDIF
      ENDIF
#endif   

      CALL ACCUMULATE_CPU_USAGE(61, IT1, IT2)
      START_CALCS = .FALSE.
      rank_finished = 1
      DT = DT_METEOROLOGY

      if (FEEDBACK_LEVEL .ge. 3) then
         WRITE(LOG_MSG,'(A,I0,A)') '[',ICASE,'] LEVEL SET CASE ENDED'
         WRITE(*,'(A)') TRIM(LOG_MSG)
      endif

      CALL SYSTEM_CLOCK(IT2)
      STATS_WALL_CLOCK_TIME(ICASE) = REAL(IT2 - IT1_LSP) / REAL(CLOCK_COUNT_RATE)
   ENDIF
   ! if (ICASE .ge. 9945) then
   !    WRITE(LOG_MSG,'(A,I0,A,F12.1)') '[',ICASE,'] PASSED CASE END CHECK, T IS ',T
   !    WRITE(*,'(A)') TRIM(LOG_MSG)
   ! endif
ENDDO

! *****************************************************************************
END SUBROUTINE LEVEL_SET_PROPAGATION
! *****************************************************************************

! *****************************************************************************
REAL FUNCTION HALF_SUPERBEE(R)
! *****************************************************************************
! Returns half the Superbee flux-limiter value for gradient ratio R, used by
! the flux-limited upwind scheme when reconstructing face values of PHI.

REAL, INTENT(IN) :: R

HALF_SUPERBEE = MAX(0.,MAX(MIN(0.5*R,1.),MIN(R,0.5)))

! *****************************************************************************
END FUNCTION HALF_SUPERBEE
! *****************************************************************************

! *****************************************************************************
SUBROUTINE TAG_BAND(NX, NY, IXLOC, IYLOC, T)
! *****************************************************************************
! Tags the burnable, not-yet-tagged cells in the BANDTHICKNESS-wide band around
! (IXLOC,IYLOC), appending each to LIST_TAGGED and recording it in the
! TAGGED/EVERTAGGED arrays so the front can advance into them.

INTEGER, INTENT(IN) :: NX, NY, IXLOC, IYLOC
REAL(8), INTENT(IN) :: T
INTEGER :: IXTAGSTART, IXTAGSTOP, IYTAGSTART, IYTAGSTOP, IX, IY

IXTAGSTART = MAX(3,    IXLOC - BANDTHICKNESS) 
IXTAGSTOP  = MIN(NX-2, IXLOC + BANDTHICKNESS)
IYTAGSTART = MAX(3,    IYLOC - BANDTHICKNESS) 
IYTAGSTOP  = MIN(NY-2, IYLOC + BANDTHICKNESS)

DO IY = IYTAGSTART, IYTAGSTOP
DO IX = IXTAGSTART, IXTAGSTOP
   IF (ISNONBURNABLE(IX,IY)) CYCLE
   IF (.NOT. TAGGED(IX,IY) .AND. (.NOT. EVERTAGGED(IX,IY)) ) THEN 
      TAGGED    (IX,IY) = .TRUE.
      EVERTAGGED(IX,IY) = .TRUE.
      CALL APPEND(LIST_TAGGED, IX, IY, T)
      NUM_EVERTAGGED = NUM_EVERTAGGED + 1
      EVERTAGGED_IX(NUM_EVERTAGGED) = INT(IX, KIND=KIND(EVERTAGGED_IX(1)))
      EVERTAGGED_IY(NUM_EVERTAGGED) = INT(IY, KIND=KIND(EVERTAGGED_IY(1)))
   ENDIF
ENDDO
ENDDO

! *****************************************************************************
END SUBROUTINE TAG_BAND
! *****************************************************************************

! *****************************************************************************
SUBROUTINE UNTAG_CELLS(NX, NY, TOA, T, BURNED)
! *****************************************************************************
! Prunes LIST_TAGGED by removing nodes no longer needed for front advancement:
! cells tagged too long, isolated tagged pixels, fully-burned interior cells,
! and suppressed cells. Clears their TAGGED flag and deletes them from the list.

TYPE(NODE), POINTER :: C => NULL()

INTEGER, INTENT(IN) :: NX, NY
INTEGER :: IXLO,IXHI,IYLO,IYHI
REAL(8), INTENT(IN) :: TOA(:,:), T
INTEGER*2, INTENT(IN) :: BURNED(:,:)
LOGICAL :: UNTAG_BECAUSE_BURNED

INTEGER :: I, IX, IY, IX1, IX2, IY1, IY2, IXSTART, IXSTOP, IYSTART, IYSTOP, NUM_DELETED=0

IF (LIST_TAGGED%NUM_NODES .EQ. 0) RETURN
IXLO = NX
IXHI = 1
IYLO = NY
IYHI = 1
NUM_DELETED = 0

!WRITE(*,*) 'LIST_TAGGED%NUM_NODES BEFORE DELETING: ', LIST_TAGGED%NUM_NODES
C=>LIST_TAGGED%HEAD
I = 0
DO
   IF (LIST_TAGGED%NUM_NODES .LE. 0) EXIT
   IF (.NOT. ASSOCIATED(C)) EXIT
   I = I + 1
   IX = C%IX
   IY = C%IY

! Remove cells that have been tagged for more than 1 week:
   IF (PHIP(IX,IY) .GE. 0. .AND. C%TIME_ADDED .GT. 0. .AND. T - C%TIME_ADDED .GT. 604800. ) THEN 
      C%TIME_SUPPRESSED = T
      NUM_DELETED = NUM_DELETED + 1
      CALL DELETE_NODE(LIST_TAGGED, C)
      TAGGED(IX,IY) = .FALSE.
      C => C%NEXT
      CYCLE
   ENDIF

! Remove single isolated tagged pixels:
   IF(.NOT. USE_BLDG_SPREAD_MODEL) THEN!Additional condition added to avoid isolated burning structures to be removed from the list_tagged 
      IX1 = MAX(1, IX-1)
      IF (.NOT. TAGGED(IX1,IY) ) THEN
         IX2 = MIN(NX, IX+1) 
         IF (.NOT. TAGGED(IX2,IY) ) THEN
            IY1 = MAX(1, IY-1) 
            IF (.NOT. TAGGED(IX,IY1)) THEN
               IY2 = MIN(NY, IY+1)
               IF (.NOT. TAGGED(IX,IY2)) THEN
                  C%TIME_SUPPRESSED = T
                  NUM_DELETED = NUM_DELETED + 1
                  CALL DELETE_NODE(LIST_TAGGED, C)
                  TAGGED(IX,IY) = .FALSE.
                  C => C%NEXT
                  CYCLE
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   IXSTART = MAX(1 , IX - BANDTHICKNESS) ; IXSTART = MIN(IXSTART,NX)
   IXSTOP  = MIN(NX, IX + BANDTHICKNESS) ; IXSTOP  = MAX(IXSTOP , 1)
   IYSTART = MAX(1 , IY - BANDTHICKNESS) ; IYSTART = MIN(IYSTART,NY)
   IYSTOP  = MIN(NY, IY + BANDTHICKNESS) ; IYSTOP  = MAX(IYSTOP , 1)

   UNTAG_BECAUSE_BURNED = .TRUE.
   DO IX2 = IXSTART, IXSTOP
      IF (PHIP(IX2,IY) .GE. 0.) UNTAG_BECAUSE_BURNED = .FALSE.
   ENDDO
   DO IY2 = IYSTART, IYSTOP
      IF (PHIP(IX,IY2) .GE. 0.) UNTAG_BECAUSE_BURNED = .FALSE.
   ENDDO

   IF ((.NOT. UNTAG_BECAUSE_BURNED) .AND. UNTAG_TYPE_2) THEN
!      IF ( TOA(IX,IY) .GT. 0. .AND. T - TOA(IX,IY) .GT. 20*DT ) UNTAG_BECAUSE_BURNED = .TRUE. 
      IF ( TOA(IX,IY) .GT. 0. .AND. T - TOA(IX,IY) .GT. 72000. ) UNTAG_BECAUSE_BURNED = .TRUE. 
   ENDIF

   IF ((.NOT. UNTAG_BECAUSE_BURNED) .AND. UNTAG_TYPE_3) THEN
      IF ( PHIP(IX,IY) .LT. -0.9999 .AND. BURNED(IX,IY) .EQ. 1 ) UNTAG_BECAUSE_BURNED = .TRUE. 
   ENDIF

   IF (UNTAG_BECAUSE_BURNED) THEN
      PHIP(IX,IY) = -1E0
      NUM_DELETED = NUM_DELETED + 1
      CALL DELETE_NODE(LIST_TAGGED, C)
      TAGGED(IX,IY) = .FALSE.
   ENDIF

   IF (C%TIME_SUPPRESSED .GT. 0.) THEN
      CALL APPEND(LIST_SUPPRESSED, IX, IY, T)
      LIST_SUPPRESSED%TAIL%TIME_SUPPRESSED = T
      NUM_DELETED = NUM_DELETED + 1
      CALL DELETE_NODE(LIST_TAGGED, C)
      TAGGED(IX,IY) = .FALSE.
   ENDIF

   C => C%NEXT    

ENDDO

! *****************************************************************************
END SUBROUTINE UNTAG_CELLS
! *****************************************************************************

! *****************************************************************************
SUBROUTINE CALC_NORMAL_VECTORS(ISTEP, HALFRCELLSIZE)
! *****************************************************************************
! Computes the unit normal vector (NORMVECTORX/Y) of the level-set field at
! every node in LIST_TAGGED via central differences of PHIP; on ISTEP 1 also
! caches the current PHIP value as PHIP_OLD for the RK2 integration.

INTEGER, INTENT(IN) :: ISTEP
INTEGER :: I, IX, IY
REAL :: HALFRCELLSIZE, DPHIDX, DPHIDY, RMAGGRADPHI
TYPE(NODE), POINTER :: C
REAL, PARAMETER :: EPSILON = 1E-30, BIG=3E4

C => LIST_TAGGED%HEAD
DO I = 1, LIST_TAGGED%NUM_NODES
   IX=C%IX
   IY=C%IY
   IF (ISTEP .EQ. 1) C%PHIP_OLD = PHIP(IX,IY)
   DPHIDY = MAX(MIN( HALFRCELLSIZE * (PHIP(IX,IY+1) - PHIP(IX,IY-1)), BIG ), -BIG)
   DPHIDX = MAX(MIN( HALFRCELLSIZE * (PHIP(IX+1,IY) - PHIP(IX-1,IY)), BIG ), -BIG)
   RMAGGRADPHI = 1. / MAX(SQRT( DPHIDX * DPHIDX + DPHIDY * DPHIDY ), EPSILON)
   C%NORMVECTORY = RMAGGRADPHI * DPHIDY
   C%NORMVECTORX = RMAGGRADPHI * DPHIDX
   C => C%NEXT
ENDDO

! *****************************************************************************
END SUBROUTINE CALC_NORMAL_VECTORS
! *****************************************************************************

! *****************************************************************************
SUBROUTINE UX_AND_UY_ELLIPTICAL(L, ACCELERATION_FACTOR, ISTEP, DT_ELMFIRE)
! *****************************************************************************
! Computes the x/y front-propagation velocity components (UX,UY), spread
! direction, and fireline intensity for each node in L from the elliptical
! spread template: combines slope/wind phi factors, length-to-width ratio,
! head/back speeds, crown-fire and WUI (Hamada/UCB) submodels.
! Parameter T_ELMFIRE added to update fireline intensity of structures over time
REAL, INTENT(IN) :: ACCELERATION_FACTOR, DT_ELMFIRE
TYPE(DLL), INTENT(INOUT) :: L
INTEGER, INTENT(IN) :: ISTEP
TYPE(NODE), POINTER :: C

REAL :: PHIMAG, PHIWX, PHIWY, PHIX, PHIY, WSMFEFF, BOH, APHIS, APHIW, SINASPMPI, COSASPMPI, &
        RPHIMAG, SQRT_LOW2_M1
INTEGER :: IASP, I, ILH, NITER
REAL, PARAMETER :: KWPM2_TO_BTUPFT2MIN = 60. * 0.3048 * 0.3048 / 1.055, FTPMIN_TO_MPS = 0.3048 / 60.
LOGICAL :: DONE, CROWN_FIRE_AT_START, CROWN_FIRE_AT_END

C => L%HEAD

IF (ISTEP .EQ. 1) THEN
   DO I = 1, L%NUM_NODES

      IF (.NOT. C%BURNED) THEN

         IF (C%NEED_SLOPE_CALC) THEN
! Determine individual slope and wind components and velocity in direction of maximum spread (DMS):
            IASP=MIN(MAX(NINT(ASP%R4(C%IX,C%IY,1)),0),360)
            SINASPMPI=SINASPM180(IASP)
            COSASPMPI=COSASPM180(IASP) 
            APHIS = ACCELERATION_FACTOR * PHIS_ADJ * C%PHIS_SURFACE
            C%PHISX = APHIS * SINASPMPI
            C%PHISY = APHIS * COSASPMPI
            C%UXOUSX = 1. - ABSSINASP(IASP) * OMCOSSLPRAD%R4(C%IX,C%IY,1)
            C%UYOUSY = 1. - ABSCOSASP(IASP) * OMCOSSLPRAD%R4(C%IX,C%IY,1)
            C%NEED_SLOPE_CALC = .FALSE.
         ENDIF
         DONE = .FALSE.
         NITER = 0
         DO WHILE (.NOT. DONE)
            CROWN_FIRE_AT_START = .FALSE.
            IF (CROWN_FIRE_MODEL .GT. 0 .AND. C%FLIN_SURFACE .GE. C%CRITICAL_FLIN) THEN
               APHIW = PHIW_ADJ * MAX(C%PHIW_SURFACE, C%PHIW_CROWN)
               CROWN_FIRE_AT_START = .TRUE.
            ELSE
               APHIW = PHIW_ADJ * ACCELERATION_FACTOR * C%PHIW_SURFACE
            ENDIF

            IF (USE_BLDG_SPREAD_MODEL .AND. C%IFBFM .EQ. 91) THEN
               APHIW   = 1.0
               C%PHISX = 0.0
               C%PHISY = 0.0
            ENDIF

            PHIWX = APHIW * SIN( (C%WD20_NOW - 180.) * PIO180)
            PHIX  = C%PHISX + PHIWX

            PHIWY = APHIW * COS( (C%WD20_NOW - 180.) * PIO180)
            PHIY  = C%PHISY + PHIWY

            PHIMAG = MAX(SQRT(PHIX*PHIX+PHIY*PHIY),1E-10)
            IF (PHIMAG .LT. 1.1E-10) THEN
               C%NORMVECTORX_DMS  = 1.0
               C%NORMVECTORY_DMS  = 0.0
            ELSE
               RPHIMAG = 1. / PHIMAG
               C%NORMVECTORX_DMS = RPHIMAG * PHIX
               C%NORMVECTORY_DMS = RPHIMAG * PHIY
            ENDIF
            C%VELOCITY_DMS = C%VS0 * (ACCELERATION_FACTOR + PHIMAG)
            if (trim(SURFACE_SPREAD_MODEL) .eq. "CFFDRS") C%VELOCITY_DMS = C%VELOCITY_DMS_SURFACE

! Calculate length over width:
            if (trim(SURFACE_SPREAD_MODEL) .eq. "CFFDRS") then
               if (C%IFBFM .ge. 31 .and. C%IFBFM .le. 33) then !grass
                  C%LOW = max(1.0,1.1+C%WSV**0.464)
               else
                  C%LOW = 1+8.729*(1-exp(-0.03*C%WSV))**2.155
               endif
            else if (trim(SURFACE_SPREAD_MODEL) .eq. "ROTHERMEL") then
               ! Determine effective mid flame wind speed (not needed for CFFDRS)
               WSMFEFF = FUEL_MODEL_TABLE_2D(C%IFBFM,30)%WSMFEFF_COEFF * PHIMAG ** FUEL_MODEL_TABLE_2D(C%IFBFM,30)%B_COEFF_INVERSE
               IF (C%FLIN_SURFACE .LT. C%CRITICAL_FLIN .OR. CROWN_FIRE_MODEL .LE. 0) WSMFEFF = MIN(WSMFEFF, 0.9*KWPM2_TO_BTUPFT2MIN*C%IR)
               C%LOW = MIN( 0.936*EXP(0.1147*WSMFEFF*WSMFEFF_LOW_MULT) + 0.461*EXP(-0.0692*WSMFEFF*WSMFEFF_LOW_MULT) - 0.397, MAX_LOW)
            endif
            
            IF (C%LOW .GT. 0.999 .AND. C%LOW .LT. 1.001) THEN
               BOH = 1.0
            ELSE
               SQRT_LOW2_M1 = SQRT(C%LOW*C%LOW - 1.0)
               BOH = (C%LOW - SQRT_LOW2_M1) / (C%LOW + SQRT_LOW2_M1 )
            ENDIF
            C%VBACK = BOH * C%VELOCITY_DMS
#ifdef _WUI
            C%TEST_INTERFACE = .FALSE.
            C%WTU_SPREAD = .FALSE.

            IF (USE_BLDG_SPREAD_MODEL .AND. BLDG_SPREAD_MODEL_TYPE .EQ. 2 .AND. CRITICAL_HF_WUI .EQ. 2) THEN
               C%TEST_INTERFACE = TEST_INTERFACE_WUI(C%IX,C%IY)
               C%WTU_SPREAD = WTU_SPREAD_WUI(C%IX,C%IY)
            ENDIF

            IF (USE_BLDG_SPREAD_MODEL .AND. C%IFBFM .EQ. 91) THEN
               CONTINUE
               IF (BLDG_SPREAD_MODEL_TYPE .EQ. 1) CALL HAMADA(C) ! GET C%VELOCITY_DMS, C%VBACK & C%LOW
               IF (BLDG_SPREAD_MODEL_TYPE .EQ. 2) CALL UMD_UCB_BLDG_SPREAD(C, DT_ELMFIRE) ! GET C%VELOCITY_DMS, C%VBACK & C%LOW
               CONTINUE
            ENDIF
#endif      

            CALL COMPUTE_SPREAD_VELOCITIES(C, ILH)

            CROWN_FIRE_AT_END = .FALSE.
            IF (CROWN_FIRE_MODEL .GT. 0 .AND. C%FLIN_SURFACE .GE. C%CRITICAL_FLIN) then
               CROWN_FIRE_AT_END = .TRUE.
            else
               C%CROWN_FIRE = 0
            endif

            DONE = .TRUE.
            IF (CROWN_FIRE_AT_END .AND. (.NOT. CROWN_FIRE_AT_START)) DONE = .FALSE.
            NITER = NITER + 1
            IF (NITER .EQ. 2) DONE = .TRUE. !Prevent infinite loop if something goes awry
         ENDDO

      ENDIF
      ! print *, C%VELOCITY * 0.3048, C%VELOCITY_DMS * 0.3048, C%VELOCITY_DMS_SURFACE * 0.3048, C%VS0 * 0.3048, C%PHIS_SURFACE, C%PHIW_SURFACE, PHIMAG
      C => C%NEXT

   ENDDO

ELSE !ISTEP .EQ. 2

   DO I = 1, L%NUM_NODES
      IF (.NOT. C%BURNED) THEN ! This condition is not functioning. C%BURNED are only assigned to LIST_BURNED but not to LIST_TAGGED
         CONTINUE

         CALL COMPUTE_SPREAD_VELOCITIES(C, ILH)

         IF (trim(SURFACE_SPREAD_MODEL) .eq. "ROTHERMEL" .and. CROWN_FIRE_MODEL .GT. 0 .AND. C%FLIN_SURFACE .GE. C%CRITICAL_FLIN) then 
            C%FLIN_CANOPY = C%HPUA_CANOPY * C%VELOCITY * 5.08E-3
         else
            C%CROWN_FIRE = 0
         endif

#ifdef _UMDSPOTTING
         IF ((.NOT. USE_SUPERSEDED_SPOTTING) .AND. USE_PHYSICAL_SPOTTING_DURATION .and. ENABLE_SPOTTING) THEN
            IF(ABS(C%UX)> 1E-3 .AND. ABS(C%UY)> 1E-3) C%LOCAL_EMBERGEN_DURATION = ANALYSIS_CELLSIZE/MIN(ABS(C%UX), ABS(C%UY)) ! seconds
            IF(ABS(C%UX)> 1E-3 .AND. ABS(C%UY)<=1E-3) C%LOCAL_EMBERGEN_DURATION = ANALYSIS_CELLSIZE/ABS(C%UX) ! seconds
            IF(ABS(C%UX)<=1E-3 .AND. ABS(C%UY)> 1E-3) C%LOCAL_EMBERGEN_DURATION = ANALYSIS_CELLSIZE/ABS(C%UY) ! seconds
            IF(ABS(C%UX)<=1E-3 .AND. ABS(C%UY)<=1E-3) C%LOCAL_EMBERGEN_DURATION = FUEL_MODEL_TABLE_2D(C%IFBFM,ILH)%TR * 60 ! seconds
         ENDIF
#endif

#ifdef _WUI                  
         IF (USE_BLDG_SPREAD_MODEL .AND. (C%IFBFM .EQ. 91)) THEN
            C%FLIN_SURFACE = HRR_TRANSIENT_MAP(C%IX,C%IY)*ANALYSIS_CELLSIZE ! kW/m
         ENDIF
         ! Model type 3: FLIN_SURFACE is already set in BLDG_SPREAD_MODEL_3
#endif

      ENDIF
      C => C%NEXT

   ENDDO

ENDIF !ISTEP .EQ. 1

CONTAINS

! *****************************************************************************
SUBROUTINE COMPUTE_SPREAD_VELOCITIES(C, ILH_OUT)
! Computes UX, UY, VELOCITY, SPREAD_DIRECTION, and FLIN_SURFACE for a node
! from its pre-computed ellipse parameters (VELOCITY_DMS, VBACK, LOW) and
! normal vector components.
! *****************************************************************************
TYPE(NODE), POINTER :: C
INTEGER, INTENT(OUT) :: ILH_OUT
REAL :: COSANG, SINANG, A, B, AACOSANG, BBSINANG, DENOM, RDENOM, DYDT, DXDT, DXDT_ROTATED, DYDT_ROTATED

! We can get sin(theta - dms) and cos(theta - dms) directly:
COSANG   = C%NORMVECTORY*C%NORMVECTORY_DMS + C%NORMVECTORX*C%NORMVECTORX_DMS
A        = MAX(0.5 * (C%VELOCITY_DMS + C%VBACK), 1E-10)
AACOSANG = A*A*COSANG

SINANG   = C%NORMVECTORX*C%NORMVECTORY_DMS - C%NORMVECTORY*C%NORMVECTORX_DMS
B        = 0.5 * MAX( (C%VELOCITY_DMS + C%VBACK) / C%LOW, 1E-10)
BBSINANG = B*B*SINANG

DENOM    = MAX(SQRT(AACOSANG*COSANG + BBSINANG*SINANG),1E-10)
RDENOM   = 1. / DENOM

DYDT     = (RDENOM * AACOSANG ) + 0.5 * (C%VELOCITY_DMS - C%VBACK)
DXDT     = RDENOM * BBSINANG

! Rotate based on direction of maximum spread:
DXDT_ROTATED    = DYDT*C%NORMVECTORX_DMS + DXDT*C%NORMVECTORY_DMS ! ft/min, parallel to slope
C%UX       = DXDT_ROTATED * C%UXOUSX * FTPMIN_TO_MPS               ! m/s, projected

DYDT_ROTATED    = DYDT*C%NORMVECTORY_DMS - DXDT*C%NORMVECTORX_DMS ! ft/min, parallel to slope
C%UY       = DYDT_ROTATED * C%UYOUSY * FTPMIN_TO_MPS               ! m/s, projected
C%VELOCITY = SQRT(DXDT_ROTATED*DXDT_ROTATED + DYDT_ROTATED*DYDT_ROTATED) ! ft/min, parallel to slope

IF (ABS(C%UX) + ABS(C%UY) .GT. 1.0e-20) THEN
   C%SPREAD_DIRECTION = ATAN2(C%UX, C%UY) * 180.0 / ACOS(-1.0)
   IF (C%SPREAD_DIRECTION .LT. 0.0) C%SPREAD_DIRECTION = C%SPREAD_DIRECTION + 360.0
ELSE
   C%SPREAD_DIRECTION = 0.0
END IF

ILH_OUT = MAX(MIN(NINT(100.*C%MLH),120),30)
IF (TRIM(SURFACE_SPREAD_MODEL) .EQ. "CFFDRS") THEN
   C%FLIN_SURFACE = C%FLIN_DMS_SURFACE
ELSE IF (TRIM(SURFACE_SPREAD_MODEL) .EQ. "ROTHERMEL") THEN
   C%FLIN_SURFACE = FUEL_MODEL_TABLE_2D(C%IFBFM,ILH_OUT)%TR * C%IR * C%VELOCITY * 0.3048 ! kW/m
END IF

IF (NO_SURFACE_FIRE) THEN
   C%UX = 1E-5
   C%UY = 1E-5
END IF

END SUBROUTINE COMPUTE_SPREAD_VELOCITIES

! *****************************************************************************
END SUBROUTINE UX_AND_UY_ELLIPTICAL
! *****************************************************************************

! *****************************************************************************
SUBROUTINE RK2_INTEGRATE(DT,ISTEP)
! *****************************************************************************
! Advances the level-set field PHIP over LIST_TAGGED by one 2nd-order Runge-Kutta
! sub-step (predictor on ISTEP 1, corrector on ISTEP 2) using the limited
! gradients and node velocities; clamps PHIP and forces ignition on WUI spread.

REAL, INTENT(IN) :: DT
INTEGER, INTENT(IN) :: ISTEP
INTEGER :: I
TYPE(NODE), POINTER :: C
REAL :: LIMIT

LIMIT = 1.0

! 2nd order Runge Kutta integration:
C => LIST_TAGGED%HEAD
IF (ISTEP .EQ. 1) THEN
   DO I = 1, LIST_TAGGED%NUM_NODES
      PHIP(C%IX,C%IY) = C%PHIP_OLD - DT * (C%UX * C%DPHIDX_LIMITED + C%UY * C%DPHIDY_LIMITED)
      IF ( PHIP(C%IX,C%IY) .NE. PHIP(C%IX,C%IY)) PHIP(C%IX,C%IY) = 1.0  !NaN check
      IF ( PHIP(C%IX,C%IY) .LT. -1 * LIMIT ) PHIP(C%IX,C%IY) = -1 * LIMIT
      IF ( PHIP(C%IX,C%IY) .GT.  LIMIT ) PHIP(C%IX,C%IY) =  LIMIT

      IF (C%WTU_SPREAD) THEN 
         PHIP (C%IX, C%IY) = -1.0   ! Interface Model
      ENDIF
      
      C => C%NEXT
   ENDDO
ELSE
   DO I = 1, LIST_TAGGED%NUM_NODES
      PHIP(C%IX,C%IY) = 0.5 * (C%PHIP_OLD + (PHIP(C%IX,C%IY) - DT * (C%UX * C%DPHIDX_LIMITED + C%UY * C%DPHIDY_LIMITED )))

      IF (C%WTU_SPREAD) THEN 
         PHIP (C%IX, C%IY) = -1.0   ! Interface Model
      ENDIF
      
      C => C%NEXT
   ENDDO
ENDIF

! *****************************************************************************
END SUBROUTINE RK2_INTEGRATE
! *****************************************************************************

! *****************************************************************************
SUBROUTINE CFL_AND_FLUX_LIMITER(DT, RCELLSIZE, PHI, ISTEP, ITIMESTEP)
! *****************************************************************************
! This subroutine merges the former CALC_CFL and LIMIT_GRADIENTS subroutines
! to reduce loops through LIST_TAGGED
! Contributed by Adam Laird (adam.laird@berkeley.edu)

! Inputs
REAL, INTENT(IN) :: RCELLSIZE
REAL, DIMENSION(:,:), INTENT(IN) :: PHI
INTEGER, INTENT(IN) :: ISTEP, ITIMESTEP

! In/Out
REAL, INTENT(INOUT) :: DT

! Locals
REAL :: CFL, COND, UMAX
REAL, PARAMETER :: EPSILON = 1E-30, CEILING = 1E3
INTEGER :: I
TYPE(NODE), POINTER :: C

UMAX = 0.
 C => LIST_TAGGED%HEAD

IF (ISTEP .EQ. 1 .AND. ITIMESTEP .GT. 5) THEN
   DO I = 1, LIST_TAGGED%NUM_NODES
      ! Calculate UMAX Condition for CFL
      IF (.NOT. C%BURNED) THEN
         COND = MAX(ABS(C%UX),ABS(C%UY))
         IF (COND .GT. UMAX) UMAX = COND
      ENDIF

      CALL LIMIT_GRADIENTS(C)

      C => C%NEXT
   ENDDO !I=1, LIST_TAGGED%NUM_NODE
ELSE
   DO I = 1, LIST_TAGGED%NUM_NODES
      CALL LIMIT_GRADIENTS(C)
      C => C%NEXT
   ENDDO !I=1, LIST_TAGGED%NUM_NODE
ENDIF

! Calculate Time Step Size based on CFL
IF (ISTEP .EQ. 1 .AND. ITIMESTEP .GT. 5) THEN
   CFL = UMAX * DT / ANALYSIS_CELLSIZE
   IF (CFL .GT. 0.) THEN
      DT = MIN(TARGET_CFL * DT / CFL, SIMULATION_DTMAX)
   ELSE
      DT = SIMULATION_DTMAX
   ENDIF
ENDIF

CONTAINS
   SUBROUTINE LIMIT_GRADIENTS(C)
      ! Computes the flux-limited (Superbee) spatial derivatives DPHIDX_LIMITED and
      ! DPHIDY_LIMITED at node C using upwind-biased stencils chosen by the sign of
      ! UX/UY, then clamps them and guards against NaNs.
      TYPE(NODE), POINTER :: C
      REAL :: DELTAUP, DELTALOC, PHIEAST=1.0, PHIWEST=1.0, PHINORTH=1.0, PHISOUTH=1.0

      ! Apply flux limiter
      IF (C%UX .GE. 0E0) THEN

         ! PHIEAST
         DELTAUP  = PHI(C%IX,  C%IY) - PHI(C%IX-1,C%IY)
         DELTALOC = PHI(C%IX+1,C%IY) - PHI(C%IX  ,C%IY)
         IF (ABS(DELTALOC) > EPSILON) PHIEAST = PHI(C%IX,C%IY) + HALF_SUPERBEE(DELTAUP / DELTALOC)*DELTALOC

         ! PHIWEST
         DELTALOC = -DELTAUP
         IF (ABS(DELTALOC) > EPSILON) THEN
            DELTAUP = PHI(C%IX-2,C%IY) - PHI(C%IX-1,C%IY)
            PHIWEST = PHI(C%IX-1,C%IY) - HALF_SUPERBEE(DELTAUP / DELTALOC)*DELTALOC
         ENDIF

      ELSE ! UX .LT. 0

         ! PHIEAST
         DELTALOC = PHI(C%IX+1,C%IY) - PHI(C%IX, C%IY)      
         IF (ABS(DELTALOC) > EPSILON) THEN    
            DELTAUP  = PHI(C%IX+2,C%IY) - PHI(C%IX+1,C%IY)
            PHIEAST = PHI(C%IX+1,C%IY) - HALF_SUPERBEE(DELTAUP / DELTALOC)*DELTALOC
         ENDIF

         ! PHIWEST
         DELTAUP  = -DELTALOC
         DELTALOC = PHI(C%IX-1,C%IY) - PHI(C%IX,C%IY)
         IF (ABS(DELTALOC) > EPSILON) PHIWEST = PHI(C%IX,C%IY)+ HALF_SUPERBEE(DELTAUP / DELTALOC)*DELTALOC

      ENDIF

      C%DPHIDX_LIMITED = (PHIEAST  - PHIWEST) * RCELLSIZE

      IF (C%UY .GT. 0E0) THEN

         ! PHINORTH
         DELTAUP  = PHI(C%IX,C%IY) - PHI(C%IX,C%IY-1)
         DELTALOC = PHI(C%IX,C%IY+1) - PHI(C%IX,C%IY)      
         IF (ABS(DELTALOC) > EPSILON) PHINORTH = PHI(C%IX,C%IY) + HALF_SUPERBEE(DELTAUP / DELTALOC)*DELTALOC

         ! PHISOUTH
         DELTALOC = -DELTAUP      
         IF (ABS(DELTALOC) .GT. EPSILON) THEN
            DELTAUP  = PHI(C%IX,C%IY-2) - PHI(C%IX,C%IY-1)
            PHISOUTH = PHI(C%IX,C%IY-1) - HALF_SUPERBEE(DELTAUP / DELTALOC)*DELTALOC
         ENDIF

      ELSE !UY .LT. 0

         ! PHINORTH
         DELTALOC = PHI(C%IX,C%IY+1) - PHI(C%IX,C%IY  )
         IF (ABS(DELTALOC) > EPSILON) THEN
            DELTAUP  = PHI(C%IX,C%IY+2) - PHI(C%IX,C%IY+1)
            PHINORTH = PHI(C%IX,C%IY+1) - HALF_SUPERBEE(DELTAUP / DELTALOC)*DELTALOC
         ENDIF

         ! PHISOUTH
         DELTAUP  = -DELTALOC
         DELTALOC = PHI(C%IX,C%IY-1) - PHI(C%IX,C%IY)
         IF (ABS(DELTALOC) > EPSILON) PHISOUTH = PHI(C%IX,C%IY) + HALF_SUPERBEE(DELTAUP / DELTALOC)*DELTALOC

      ENDIF

      C%DPHIDY_LIMITED = (PHINORTH - PHISOUTH) * RCELLSIZE

      IF (C%DPHIDX_LIMITED .GT.  CEILING) C%DPHIDX_LIMITED = CEILING
      IF (C%DPHIDY_LIMITED .GT.  CEILING) C%DPHIDY_LIMITED = CEILING

      IF (C%DPHIDX_LIMITED .LT. -CEILING) C%DPHIDX_LIMITED = -CEILING
      IF (C%DPHIDY_LIMITED .LT. -CEILING) C%DPHIDY_LIMITED = -CEILING

      IF (C%DPHIDX_LIMITED .NE.  C%DPHIDX_LIMITED) C%DPHIDX_LIMITED = 0.
      IF (C%DPHIDY_LIMITED .NE.  C%DPHIDY_LIMITED) C%DPHIDY_LIMITED = 0.

   END SUBROUTINE LIMIT_GRADIENTS

! *****************************************************************************
END SUBROUTINE CFL_AND_FLUX_LIMITER
! *****************************************************************************

#ifdef _WUI
! *****************************************************************************
SUBROUTINE TAG_WUI(NX, NY, IXLOC, IYLOC, T)
! *****************************************************************************
! Adds cells to LIST_WUI_BURNING for the refactored WUI spread model: if
! (IXLOC,IYLOC) is an urban (FBFM91) cell, tags its whole BANDTHICKNESS_WUI
! neighborhood; if it is a burning vegetative cell, tags it (and nearby urban
! cells) only when an urban cell lies within that band.

INTEGER, INTENT(IN) :: NX, NY, IXLOC, IYLOC
REAL(8), INTENT(IN) :: T
INTEGER :: IXTAGSTART, IXTAGSTOP, IYTAGSTART, IYTAGSTOP, IX, IY
LOGICAL :: CELL_IN_WUI

IXTAGSTART = MAX(3,    IXLOC - BANDTHICKNESS_WUI) 
IXTAGSTOP  = MIN(NX-2, IXLOC + BANDTHICKNESS_WUI)
IYTAGSTART = MAX(3,    IYLOC - BANDTHICKNESS_WUI) 
IYTAGSTOP  = MIN(NY-2, IYLOC + BANDTHICKNESS_WUI)

IF (FBFM%I2(IXLOC, IYLOC, 1) .EQ. 91) THEN
   ! Tag all cells within the BANDTHICKNESS_WUI surrounding an urban cell to LIST_WUI_BURNING
   DO IY = IYTAGSTART, IYTAGSTOP
   DO IX = IXTAGSTART, IXTAGSTOP
      IF (ISNONBURNABLE(IX,IY)) CYCLE
      IF (.NOT. TAGGED_WUI(IX,IY) .AND. (.NOT. EVERTAGGED_WUI(IX,IY)) ) THEN 
         TAGGED_WUI    (IX,IY) = .TRUE.
         EVERTAGGED_WUI(IX,IY) = .TRUE.
         CALL APPEND(LIST_WUI_BURNING, IX, IY, T)

         LIST_WUI_BURNING%TAIL%TIME_OF_ARRIVAL = TIME_OF_ARRIVAL(IX,IY)
      ENDIF
   ENDDO
   ENDDO
ELSE
   ! Only tag the burning vegetative cells when it is close enough (<BANDTHICKNESS_WUI) to urban cells
   IF (ISNONBURNABLE(IXLOC, IYLOC)) RETURN
   IF (.NOT. TAGGED_WUI(IXLOC, IYLOC) .AND. (.NOT. EVERTAGGED_WUI(IXLOC, IYLOC)) ) THEN 

      CELL_IN_WUI = .FALSE.
      DO IY = IYTAGSTART, IYTAGSTOP
      DO IX = IXTAGSTART, IXTAGSTOP
         IF (FBFM%I2(IX,IY,1) .EQ. 91) THEN
            CELL_IN_WUI = .TRUE.
            IF (.NOT. TAGGED_WUI(IX,IY) .AND. (.NOT. EVERTAGGED_WUI(IX,IY)) ) THEN 
               TAGGED_WUI    (IX,IY) = .TRUE.
               EVERTAGGED_WUI(IX,IY) = .TRUE.
               CALL APPEND(LIST_WUI_BURNING, IX, IY, T)

               LIST_WUI_BURNING%TAIL%TIME_OF_ARRIVAL = TIME_OF_ARRIVAL(IX,IY)
            ENDIF
         ENDIF
      ENDDO
      ENDDO

      IF (CELL_IN_WUI) THEN
         TAGGED_WUI    (IXLOC, IYLOC) = .TRUE.
         EVERTAGGED_WUI(IXLOC, IYLOC) = .TRUE.
         CALL APPEND(LIST_WUI_BURNING, IXLOC, IYLOC, T)

         LIST_WUI_BURNING%TAIL%TIME_OF_ARRIVAL = TIME_OF_ARRIVAL(IXLOC, IYLOC)
      ENDIF
   ENDIF
ENDIF

! *****************************************************************************
END SUBROUTINE TAG_WUI
! *****************************************************************************

! *****************************************************************************
SUBROUTINE UNTAG_CELLS_WUI(T, NX, NY)
! *****************************************************************************
! Delete WUI nodes when they stop burning:
! (reach end of design fire curve or heat flux drop below threshold value)
TYPE(NODE), POINTER :: C => NULL(), NEXT_C => NULL()
REAL(8), INTENT(IN) :: T
INTEGER, INTENT(IN) :: NX, NY
INTEGER :: IXLOC, IYLOC, IX, IY, IXTAGSTART, IXTAGSTOP, IYTAGSTART, IYTAGSTOP
! REAL :: TOTAL_HEAT_FLUX
LOGICAL :: UNBURNED_IN_BANDTHICKNESS_WUI

IF (LIST_WUI_BURNING%NUM_NODES .LE. 0) RETURN

C=>LIST_WUI_BURNING%HEAD

DO 
   IF (LIST_WUI_BURNING%NUM_NODES .LE. 0) EXIT
   IF (.NOT. ASSOCIATED(C)) EXIT
! Remove cells that have been reached the end of HRR curve:
   IXLOC=C%IX
   IYLOC=C%IY
   IF (C%BURNED) THEN
      NEXT_C => C%NEXT
      CALL DELETE_NODE(LIST_WUI_BURNING, C)
      C => NEXT_C
      CYCLE
   ENDIF


! Remove stale vegetative WUI cells after their transient HRR has ended:
   ! TOTAL_HEAT_FLUX = TRANSIENT_DFC_WUI(IXLOC,IYLOC)+TRANSIENT_RADIATION_WUI(IXLOC,IYLOC)
   ! IF (TOTAL_HEAT_FLUX .LE. CRITICL_HF_WUI .AND. TIME_OF_ARRIVAL(IXLOC,IYLOC) .GT. 0. .AND. T-TIME_OF_ARRIVAL(IXLOC,IYLOC) .GT. 3000. ) THEN
   IF (C%IFBFM .NE. 91 .AND. C%HRR_TRANSIENT .LE. 0. .AND. &
       TIME_OF_ARRIVAL(IXLOC, IYLOC) .GT. 0. .AND. T-TIME_OF_ARRIVAL(IXLOC, IYLOC) .GT. 5000. ) THEN
      TAGGED_WUI    (IXLOC,IYLOC) = .FALSE.
      EVERTAGGED_WUI(IXLOC,IYLOC) = .FALSE.
      NEXT_C => C%NEXT
      CALL DELETE_NODE(LIST_WUI_BURNING, C)
      C => NEXT_C
      CYCLE
   ENDIF

! Accelerate calculation, misssing heat flux history
   IXTAGSTART = MAX(3,    IXLOC - BANDTHICKNESS_WUI) 
   IXTAGSTOP  = MIN(NX-2, IXLOC + BANDTHICKNESS_WUI)
   IYTAGSTART = MAX(3,    IYLOC - BANDTHICKNESS_WUI) 
   IYTAGSTOP  = MIN(NY-2, IYLOC + BANDTHICKNESS_WUI)
   UNBURNED_IN_BANDTHICKNESS_WUI = .FALSE.
   DO IY = IYTAGSTART, IYTAGSTOP
   DO IX = IXTAGSTART, IXTAGSTOP
      IF (ISNONBURNABLE(IX,IY)) CYCLE
      IF (PHIP(IX, IY) .GT. 0.) UNBURNED_IN_BANDTHICKNESS_WUI = .TRUE.
   ENDDO
   ENDDO
   IF (.NOT. UNBURNED_IN_BANDTHICKNESS_WUI) THEN 
      TAGGED_WUI    (IXLOC,IYLOC) = .FALSE.
      EVERTAGGED_WUI(IXLOC,IYLOC) = .FALSE.
      NEXT_C => C%NEXT
      CALL DELETE_NODE(LIST_WUI_BURNING, C)
      C => NEXT_C
      CYCLE
   ENDIF

   C => C%NEXT

ENDDO

! *****************************************************************************
END SUBROUTINE UNTAG_CELLS_WUI
! *****************************************************************************
#endif

#ifdef _UMDSPOTTING
! *****************************************************************************
SUBROUTINE EULERIAN_SPOTTING_MAIN(NX_ELM, NY_ELM, CELLSIZE_ELM, T_ELMFIRE, DT_ELMFIRE, F_METEOROLOGY, WS20_LO, WS20_HI, MINIMUM_CURRENT_WX_BAND)
! *****************************************************************************
! Main call to ember trajectory integration and ignition determination
USE ELMFIRE_VARS

REAL, INTENT(IN) :: CELLSIZE_ELM, DT_ELMFIRE, F_METEOROLOGY
REAL(8), intent(in) :: T_ELMFIRE
REAL, DIMENSION(:,:), INTENT(IN) :: WS20_LO, WS20_HI
INTEGER, INTENT(IN) :: NX_ELM, NY_ELM, MINIMUM_CURRENT_WX_BAND

TYPE (NODE), POINTER :: C => NULL(), NEXT_C => NULL()
INTEGER :: IX, IY, ICOL, IROW
REAL :: WS20

! Move all trackers forward by 1 level-set time step (tracker trajectories are solved using smaller time steps)
! It avoids allocating a big table to memorize firebrands that will be deposited in the future steps.
C => LIST_EMBER_TRACKER%HEAD
DO
   IF (LIST_EMBER_TRACKER%NUM_NODES .LE. 0) EXIT
   IF (.NOT. ASSOCIATED(C)) EXIT
   CALL EMBER_TRAJECTORY_EULERIAN(NX_ELM, NY_ELM, CELLSIZE_ELM, C, T_ELMFIRE, DT_ELMFIRE, MINIMUM_CURRENT_WX_BAND)
   NEXT_C => C%NEXT
   IF(C%TARGET_ARRIVED) THEN
      CALL DELETE_NODE(LIST_EMBER_TRACKER, C)
   ENDIF
   C => NEXT_C
ENDDO

! Ignite firebrand-landed pixels (maybe substituted by array-based algorithm in the future)
C => LIST_EMBER_DEPOSITED%HEAD
DO 
   IF (LIST_EMBER_DEPOSITED%NUM_NODES .LE. 0) EXIT
   IF (.NOT. ASSOCIATED(C)) EXIT
   NEXT_C => C%NEXT

   IX = C%IX
   IY = C%IY

   IF(USE_EMBER_CONSUMPTION) CALL EMBER_CONSUMPTION(IX, IY, T_ELMFIRE, DT_ELMFIRE)

   IF(PHIP(IX,IY) .GE. 0 .AND. SURFACE_FIRE(IX,IY) .LE. 0) THEN
      IF (trim(IGNITION_MODEL) .eq. 'SIMPLE' .or. trim(IGNITION_MODEL) .eq. 'PHYSICAL') THEN
         ! Calculate wind speed at newly ignited cells for the non-direct ignition models
         ICOL = ICOL_ANALYSIS_F2C(IX)
         IROW = IROW_ANALYSIS_F2C(IY)
         WS20 = WS20_LO(ICOL,IROW) * (1. - F_METEOROLOGY) + F_METEOROLOGY * WS20_HI(ICOL,IROW)
         ! Ignite the target according to the physics-based model
         CALL EMBER_IGNITION(C,T_ELMFIRE, DT_ELMFIRE, WS20)
         IF (.NOT. C%FULL_DEV_IGNITION) THEN
            C => NEXT_C
            CYCLE
         ENDIF
      ELSE IF (trim(IGNITION_MODEL) .eq. 'DIRECT') THEN
         ! Ignite the target immediately if any firebrand landed
         IF (EMBER_TOA(IX,IY) .GT. T_ELMFIRE+DT_ELMFIRE .OR. EMBER_TOA(IX,IY) .LT. 0) THEN
            C => NEXT_C
            CYCLE
         ENDIF
      ENDIF

      IF (ADJ%R4(IX,IY,1) .GT. 0. .AND. (.NOT. ISNONBURNABLE(IX,IY) ) ) THEN
         CALL TAG_BAND(NX_ELM, NY_ELM, IX, IY, T_ELMFIRE+DT_ELMFIRE)
         PHIP           (IX,IY) = -1.0
         ! Record firebrand ignited cells
         IF (DUMP_EMBER_IGNITION) EMBER_IGNITION_MAP%I2(IX,IY,1) = 1
         CALL DELETE_NODE(LIST_EMBER_DEPOSITED, C) ! Remove ignited cells
      ENDIF
   ENDIF
   C => NEXT_C
ENDDO

! *****************************************************************************
END SUBROUTINE EULERIAN_SPOTTING_MAIN
! *****************************************************************************

! *****************************************************************************
SUBROUTINE LAGRANGIAN_SPOTTING_MAIN(NX_ELM, NY_ELM, T_ELMFIRE, DT_ELMFIRE, F_METEOROLOGY, WS20_LO, WS20_HI)
! *****************************************************************************
! Main call to ember trajectory integration and ignition determination
USE ELMFIRE_VARS 
!NUM_TRACKED_EMBERS, SPOTTING_STATS, EMBER_FLUX, EMBER_SAMPLING_FACTOR, DUMP_EMBER_FLUX, 
!DUMP_EMBER_FLUX_TRANSIENT, IGNITION_MODEL, LIST_EMBER_DEPOSITED, SURFACE_FIRE, ADJ, 
!ISNONBURNABLE, TAG_BAND, TIME_OF_ARRIVAL, PHIP, DUMP_SPOTTING_OUTPUTS, OUTPUTS_DIRECTORY

REAL, INTENT(IN) :: DT_ELMFIRE, F_METEOROLOGY
REAL(8), intent(in) :: T_ELMFIRE
REAL, DIMENSION(:,:), INTENT(IN) :: WS20_LO, WS20_HI
INTEGER, INTENT(IN) :: NX_ELM, NY_ELM
INTEGER :: I, IX, IY, ICOL, IROW
TYPE (NODE), POINTER :: C => NULL(), NEXT_C => NULL()
REAL :: WS20

DO I = 1, NUM_TRACKED_EMBERS
   IF (SPOTTING_STATS(I)%TIGN .LT. 0.0) CYCLE
   IF (SPOTTING_STATS(I)%IX_TO .LT. 1 .OR. SPOTTING_STATS(I)%IY_TO .LT. 1) CYCLE
   ! Check if ember has landed at the current timestep, if so, determine if it causes ignition
   IF (SPOTTING_STATS(I)%TIGN .GT. T_ELMFIRE+DT_ELMFIRE) CYCLE 
   ! Accumulate ember flux for output if not already done for this ember
   IF (DUMP_EMBER_FLUX .OR. DUMP_EMBER_FLUX_TRANSIENT .OR. IGNITION_MODEL .NE. 'DIRECT') THEN
      IF (.NOT. SPOTTING_STATS(I)%ACCUMULATED) THEN
         IX = SPOTTING_STATS(I)%IX_TO
         IY = SPOTTING_STATS(I)%IY_TO
         IF (EMBER_FLUX%R4(IX,IY,1) .LE. 0.0) THEN 
            ! Record the location and time of ember deposition for flux output and non-direct ignition model.
            CALL APPEND(LIST_EMBER_DEPOSITED, IX, IY, SPOTTING_STATS(I)%TIGN)
         ENDIF
         EMBER_FLUX%R4(IX,IY,1) = EMBER_FLUX%R4(IX,IY,1) + EMBER_SAMPLING_FACTOR
         IF (DUMP_EMBER_FLUX_TRANSIENT) EMBER_FLUX_TRANSIENT%R4(IX,IY,1) = EMBER_FLUX_TRANSIENT%R4(IX,IY,1) + EMBER_SAMPLING_FACTOR
         SPOTTING_STATS(I)%ACCUMULATED = .TRUE.
      ENDIF
   ENDIF
   
   IF (IGNITION_MODEL .EQ. 'DIRECT') THEN
      IF (.NOT. SPOTTING_STATS(I)%POSITIVE_IGNITION ) CYCLE
      IF (SPOTTING_STATS(I)%ALREADY_IGNITED         ) CYCLE
      
      SPOTTING_STATS(I)%ALREADY_IGNITED = .TRUE.

      IX = SPOTTING_STATS(I)%IX_TO
      IY = SPOTTING_STATS(I)%IY_TO

      IF (SURFACE_FIRE(IX,IY) .LE. 0 .AND. ADJ%R4(IX,IY,1) .GT. 0. .AND. (.NOT. ISNONBURNABLE(IX,IY) ) ) THEN
         CALL TAG_BAND(NX_ELM, NY_ELM, IX, IY, T_ELMFIRE)
         TIME_OF_ARRIVAL(IX,IY) = T_ELMFIRE
         PHIP           (IX,IY) = -1.0
      ENDIF

   ENDIF

ENDDO

IF (TRIM(IGNITION_MODEL) .NE. 'DIRECT') THEN
   C => LIST_EMBER_DEPOSITED%HEAD
   DO 
      IF (LIST_EMBER_DEPOSITED%NUM_NODES .LE. 0) EXIT
      IF (.NOT. ASSOCIATED(C)) EXIT
      NEXT_C => C%NEXT

      IX = C%IX
      IY = C%IY

      IF(USE_EMBER_CONSUMPTION) CALL EMBER_CONSUMPTION(IX, IY, T_ELMFIRE, DT_ELMFIRE)

      IF(PHIP(IX,IY) .GE. 0 .AND. SURFACE_FIRE(IX,IY) .LE. 0) THEN
         IF (trim(IGNITION_MODEL) .eq. 'SIMPLE' .or. trim(IGNITION_MODEL) .eq. 'PHYSICAL') THEN
            ! Calculate wind speed at newly ignited cells for the non-direct ignition models
            ICOL = ICOL_ANALYSIS_F2C(IX)
            IROW = IROW_ANALYSIS_F2C(IY)
            WS20 = WS20_LO(ICOL,IROW) * (1. - F_METEOROLOGY) + F_METEOROLOGY * WS20_HI(ICOL,IROW)
            ! Ignite the target according to the physics-based model
            CALL EMBER_IGNITION(C,T_ELMFIRE, DT_ELMFIRE, WS20)
            IF (.NOT. C%FULL_DEV_IGNITION) THEN
               C => NEXT_C
               CYCLE
            ENDIF
         ENDIF

         IF (ADJ%R4(IX,IY,1) .GT. 0. .AND. (.NOT. ISNONBURNABLE(IX,IY) ) ) THEN
            CALL TAG_BAND(NX_ELM, NY_ELM, IX, IY, T_ELMFIRE+DT_ELMFIRE)
            PHIP           (IX,IY) = -1.0
            ! Record firebrand ignited cells
            IF (DUMP_EMBER_IGNITION) EMBER_IGNITION_MAP%I2(IX,IY,1) = 1
            CALL DELETE_NODE(LIST_EMBER_DEPOSITED, C) ! Remove ignited cells
         ENDIF
      ENDIF
      C => NEXT_C
   ENDDO
ENDIF

CALL CLEAR_USED_EMBER(T_ELMFIRE)
! *****************************************************************************
END SUBROUTINE LAGRANGIAN_SPOTTING_MAIN
! *****************************************************************************
#endif

! *****************************************************************************
END MODULE
! *****************************************************************************