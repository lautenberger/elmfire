! *****************************************************************************
MODULE ELMFIRE_SUBS
! *****************************************************************************

USE ELMFIRE_VARS
USE MPI_F08
USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_FINITE, IEEE_VALUE, IEEE_QUIET_NAN

IMPLICIT NONE

CONTAINS

! *****************************************************************************
SUBROUTINE WRITE_TIMINGS_TO_DISK
! *****************************************************************************
! Writes the per-rank TIMINGS array (62 timing blocks x all host ranks) to a
! formatted CSV file 'timings_<PROCNAME>.csv' in OUTPUTS_DIRECTORY.

INTEGER :: I,IOS,IR,LU
CHARACTER(400) :: FN

LU=3939

FN = TRIM(OUTPUTS_DIRECTORY) // 'timings_' // TRIM(PROCNAME) // '.csv'  
OPEN(LU,FILE=TRIM(FN),FORM='FORMATTED',STATUS='REPLACE',IOSTAT=IOS)
WRITE(LU,100) '#,', (IR, IR=0, NPROC_HOST-1)
DO I = 1, 62
   WRITE(LU,300) I, (TIMINGS(IR+1,I), IR=0, NPROC_HOST-1)
ENDDO
CLOSE(LU)

100 FORMAT (A,128(I3,','))
300 FORMAT (I3,',',128(F11.5,','))

! *****************************************************************************
END SUBROUTINE WRITE_TIMINGS_TO_DISK
! *****************************************************************************

! *****************************************************************************
SUBROUTINE ACCUMULATE_CPU_USAGE(IBLOCK,IT1,IT2)
! *****************************************************************************
! Adds the elapsed wall-clock time (since IT1) into timing block IBLOCK of the
! TIMINGS array for this rank, then resets IT1 to the current clock count.

INTEGER, INTENT(IN) :: IBLOCK
INTEGER, INTENT(INOUT) :: IT1
INTEGER :: IT2

CALL SYSTEM_CLOCK(IT2)
TIMINGS(IRANK_HOST+1,IBLOCK) = TIMINGS(IRANK_HOST+1,IBLOCK) + REAL(IT2 - IT1) / REAL(CLOCK_COUNT_RATE)
CALL SYSTEM_CLOCK(IT1)

! *****************************************************************************
END SUBROUTINE ACCUMULATE_CPU_USAGE
! *****************************************************************************

! *****************************************************************************
SUBROUTINE MPI_BCAST_RASTER_HEADER(R, IROOT, JUST_SEND_SIZE)
! *****************************************************************************
! MPI: broadcasts a raster's header metadata from rank IROOT to all ranks in
! MPI_COMM_WORLD. If JUST_SEND_SIZE, only the dimensions (NROWS/NCOLS/NBANDS)
! are sent; otherwise the full geotransform/pixel-type header is broadcast.

TYPE (RASTER_TYPE) :: R
INTEGER, INTENT(IN) :: IROOT
LOGICAL, INTENT(IN) :: JUST_SEND_SIZE
INTEGER :: IERR

IF (JUST_SEND_SIZE) THEN
   CALL MPI_BCAST(R%NROWS         ,  1, MPI_INTEGER    , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%NCOLS         ,  1, MPI_INTEGER    , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%NBANDS        ,  1, MPI_INTEGER    , IROOT, MPI_COMM_WORLD, IERR)
ELSE
   CALL MPI_BCAST(R%BYTEORDER     ,  1, MPI_CHARACTER  , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%LAYOUT        ,  3, MPI_CHARACTER  , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%NROWS         ,  1, MPI_INTEGER    , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%NCOLS         ,  1, MPI_INTEGER    , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%NBANDS        ,  1, MPI_INTEGER    , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%NBITS         ,  1, MPI_INTEGER    , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%BANDROWBYTES  ,  1, MPI_INTEGER    , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%TOTALROWBYTES ,  1, MPI_INTEGER    , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%PIXELTYPE     , 10, MPI_CHARACTER  , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%ULXMAP        ,  1, MPI_REAL       , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%ULYMAP        ,  1, MPI_REAL       , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%XDIM          ,  1, MPI_REAL       , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%YDIM          ,  1, MPI_REAL       , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%NODATA_VALUE  ,  1, MPI_REAL       , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%CELLSIZE      ,  1, MPI_REAL       , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%XLLCORNER     ,  1, MPI_REAL       , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%YLLCORNER     ,  1, MPI_REAL       , IROOT, MPI_COMM_WORLD, IERR)
ENDIF

! *****************************************************************************
END SUBROUTINE MPI_BCAST_RASTER_HEADER
! *****************************************************************************

! *****************************************************************************
SUBROUTINE BCAST_WEATHER 
! *****************************************************************************
! MPI: broadcasts all weather raster arrays (WS, WD, fuel moistures, and, if
! USE_ERC, ERC and IGNFAC) from rank 0 to the per-host rank-0 communicator
! MPI_COMM_HOST_IRANK0, so each node's leader receives a shared copy.

INTEGER :: IERR, FUEL_TOPO_COUNT, WEATHER_COUNT

FUEL_TOPO_COUNT = ASP%NCOLS * ASP%NROWS
WEATHER_COUNT= WS%NCOLS * WS%NROWS * SIZE(WS%R4, 3)

CALL MPI_BCAST(WS%R4  , WEATHER_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(WD%R4  , WEATHER_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(M1%R4  , WEATHER_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(M10%R4 , WEATHER_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(M100%R4, WEATHER_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(MLH%R4 , WEATHER_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(MLW%R4 , WEATHER_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(MFOL%R4, WEATHER_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)

IF (USE_ERC) THEN
   CALL MPI_BCAST(ERC%R4, WEATHER_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
   CALL MPI_BCAST(IGNFAC%R4, WEATHER_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
ENDIF

END SUBROUTINE BCAST_WEATHER

! *****************************************************************************
SUBROUTINE BCAST_FUEL_TOPOGRAPHY
! *****************************************************************************
! MPI: broadcasts all fuel and topography raster arrays (aspect, canopy, DEM,
! fuel model, slope, adjustments, optional WUI/population/value/pyrome layers)
! from rank 0 to the per-host leader communicator MPI_COMM_HOST_IRANK0.

INTEGER :: IERR, FUEL_TOPO_COUNT

FUEL_TOPO_COUNT = ASP%NCOLS * ASP%NROWS

IF (USE_IGNITION_MASK) CALL MPI_BCAST(IGN_MASK%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)

CALL MPI_BCAST(ASP%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(CBH%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(CBD%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(CC%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(CH%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(DEM%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(FBFM%I2, FUEL_TOPO_COUNT, MPI_SHORT, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(SLP%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(ADJ%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
IF (MODE .NE. 2) CALL MPI_BCAST(PHI0%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(WAF%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(OMCOSSLPRAD%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(ISNONBURNABLE, FUEL_TOPO_COUNT, MPI_BYTE, 0, MPI_COMM_HOST_IRANK0, IERR)

IF (USE_POPULATION_DENSITY) CALL MPI_BCAST(POPULATION_DENSITY%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
IF (USE_REAL_ESTATE_VALUE ) CALL MPI_BCAST(REAL_ESTATE_VALUE%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
IF (USE_LAND_VALUE        ) CALL MPI_BCAST(LAND_VALUE%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
IF (USE_SDI               ) CALL MPI_BCAST(SDI%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
! new suppression model :: added below
IF (ENABLE_EXTENDED_ATTACK .AND. EXTENDED_ATTACK_MODEL .EQ. 1) CALL MPI_BCAST(PCL%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
! new suppression model
IF (USE_BLDG_SPREAD_MODEL) THEN
   CALL MPI_BCAST(BLDG_AREA%R4            , FUEL_TOPO_COUNT, MPI_REAL , 0, MPI_COMM_HOST_IRANK0, IERR)
   CALL MPI_BCAST(BLDG_SEPARATION_DIST%R4 , FUEL_TOPO_COUNT, MPI_REAL , 0, MPI_COMM_HOST_IRANK0, IERR)
   CALL MPI_BCAST(BLDG_NONBURNABLE_FRAC%R4, FUEL_TOPO_COUNT, MPI_REAL , 0, MPI_COMM_HOST_IRANK0, IERR)
   CALL MPI_BCAST(BLDG_FOOTPRINT_FRAC%R4  , FUEL_TOPO_COUNT, MPI_REAL , 0, MPI_COMM_HOST_IRANK0, IERR)
   CALL MPI_BCAST(BLDG_FUEL_MODEL%I2      , FUEL_TOPO_COUNT, MPI_SHORT, 0, MPI_COMM_HOST_IRANK0, IERR)
ENDIF

IF (USE_PYROMES) CALL MPI_BCAST(PYROMES%I2, FUEL_TOPO_COUNT, MPI_SHORT, 0, MPI_COMM_HOST_IRANK0, IERR)

! *****************************************************************************
END SUBROUTINE BCAST_FUEL_TOPOGRAPHY
! *****************************************************************************

! *****************************************************************************
SUBROUTINE PERTURB_RASTERS(R1)
! *****************************************************************************
! Draws perturbation offsets for each raster flagged for perturbation, using
! the configured PDF (uniform/Gaussian/lognormal) seeded by R1, and stores the
! results in the global PERTURB_* module variables used to adjust inputs.

REAL, INTENT(IN), DIMENSION(:) :: R1
REAL :: U1, U2, NORMAL_MEAN, NORMAL_SIGMA, NORMAL_SIGMA2

INTEGER :: I

!Format is X_actual = X_input + COEFFS_UNSCALED(I)

DO I = 1, NUM_RASTERS_TO_PERTURB
   if (PDF_TYPE(I) .eq. 'UNIFORM') then
      COEFFS_UNSCALED(I) = PDF_LOWER_LIMIT(I) + R1(I) * (PDF_UPPER_LIMIT(I) - PDF_LOWER_LIMIT(I))
   else if (PDF_TYPE(I) .eq. 'GAUSSIAN') then
      call random_number(U1)
      call random_number(U2)
      U1 = max(U1, tiny(U1))
      COEFFS_UNSCALED(I) = PDF_MEAN(I) + PDF_SIGMA(I) * (sqrt(-2.0 * log(U1))*cos(2*PI*U2)) ! Box-Muller transform
   else if (PDF_TYPE(I) .eq. 'LOGNORMAL') then
      call random_number(U1)
      call random_number(U2)
      U1 = max(U1, tiny(U1))

      NORMAL_SIGMA2 = log(1.0 + (PDF_SIGMA(I) / PDF_MEAN(I))**2)
      NORMAL_SIGMA  = sqrt(NORMAL_SIGMA2)
      NORMAL_MEAN   = log(PDF_MEAN(I)) - 0.5 * NORMAL_SIGMA2

      COEFFS_UNSCALED(I) = exp(NORMAL_MEAN + NORMAL_SIGMA * (sqrt(-2.0 * log(U1))*cos(2*PI*U2))) - PDF_MEAN(I)
   endif
   SELECT CASE (TRIM(RASTER_TO_PERTURB(I)))
      CASE('ADJ')
         PERTURB_ADJ  = COEFFS_UNSCALED(I)
      CASE('CBD')
         PERTURB_CBD  = COEFFS_UNSCALED(I)
      CASE('CBH')
         PERTURB_CBH  = COEFFS_UNSCALED(I)
      CASE('FMC')
         PERTURB_FMC  = COEFFS_UNSCALED(I)
      CASE('M1')
         PERTURB_M1   = COEFFS_UNSCALED(I)
         if (DEAD_MC_IN_PERCENT) PERTURB_M1 = PERTURB_M1 * 0.01
      CASE('M10')
         PERTURB_M10  = COEFFS_UNSCALED(I)
         if (DEAD_MC_IN_PERCENT) PERTURB_M10 = PERTURB_M10 * 0.01
      CASE('M100')
         PERTURB_M100 = COEFFS_UNSCALED(I)
         if (DEAD_MC_IN_PERCENT) PERTURB_M100 = PERTURB_M100 * 0.01
      CASE('MLH')
         PERTURB_MLH  = COEFFS_UNSCALED(I)
         if (LIVE_MC_IN_PERCENT) PERTURB_MLH = PERTURB_MLH * 0.01
      CASE('MLW')
         PERTURB_MLW  = COEFFS_UNSCALED(I)
         if (LIVE_MC_IN_PERCENT) PERTURB_MLW = PERTURB_MLW * 0.01
      CASE('WAF')
         PERTURB_WAF  = COEFFS_UNSCALED(I)
      CASE('WD')
         PERTURB_WD   = COEFFS_UNSCALED(I)
      CASE('WS')
         PERTURB_WS   = COEFFS_UNSCALED(I)
   END SELECT

ENDDO

! *****************************************************************************
END SUBROUTINE PERTURB_RASTERS
! *****************************************************************************

! *****************************************************************************
SUBROUTINE GET_OPERATING_SYSTEM
! *****************************************************************************
! Use the compiler's target OS: PATH may be empty or begin with a relative path.
#if defined(_WIN32) || defined(_WIN64)
   OPERATING_SYSTEM = 'windows'
   PATH_SEPARATOR   = ACHAR(92)
   DELETECOMMAND    = 'del /f /q'
   NULL_DEVICE      = 'NUL'
#else
   OPERATING_SYSTEM = 'linux  '
   PATH_SEPARATOR   = '/'
   DELETECOMMAND    = '/bin/rm -f '
   NULL_DEVICE      = '/dev/null'
#endif

! *****************************************************************************
END SUBROUTINE GET_OPERATING_SYSTEM
! *****************************************************************************

! *****************************************************************************
SUBROUTINE RUN_SHELL_COMMAND(COMMAND, EXITSTAT)
! *****************************************************************************
! Invoke cmd explicitly so quoted executable paths work with both Intel and GNU
! runtimes. /S removes only the outer quotes; /D disables shell startup commands.
CHARACTER(*), INTENT(IN) :: COMMAND
INTEGER, OPTIONAL, INTENT(OUT) :: EXITSTAT
INTEGER :: ISTAT, CSTAT
CHARACTER(1024) :: CMSG
CHARACTER(:), ALLOCATABLE :: CMD

CMD = TRIM(COMMAND)
IF (OPERATING_SYSTEM .EQ. 'windows' .AND. LEN_TRIM(CMD) .GT. 0) THEN
   CMD = 'cmd /d /s /c "' // CMD // '"'
ENDIF
ISTAT = -1
CMSG = ''
CALL EXECUTE_COMMAND_LINE(CMD, EXITSTAT=ISTAT, CMDSTAT=CSTAT, CMDMSG=CMSG)
IF (CSTAT /= 0) THEN
   ISTAT = -1
   WRITE(*,*) 'Could not start command on rank ', IRANK_WORLD, ': ', CMD, ': ', TRIM(CMSG)
ENDIF
IF (PRESENT(EXITSTAT)) EXITSTAT = ISTAT
END SUBROUTINE RUN_SHELL_COMMAND
! *****************************************************************************

! *****************************************************************************
SUBROUTINE DELETE_FILE(FILENAME)
! *****************************************************************************
! Delete a single temporary file without relying on shell commands or quoting.
CHARACTER(*), INTENT(IN) :: FILENAME
INTEGER :: LU, IOS

OPEN(NEWUNIT=LU, FILE=FILENAME, STATUS='OLD', IOSTAT=IOS)
IF (IOS .EQ. 0) CLOSE(LU, STATUS='DELETE', IOSTAT=IOS)
END SUBROUTINE DELETE_FILE
! *****************************************************************************

! *****************************************************************************
SUBROUTINE CLEAN_SCRATCH_DIRECTORY
! *****************************************************************************
! Remove files only; quote the directory while allowing the POSIX shell to glob.
CHARACTER(:), ALLOCATABLE :: DIRECTORY
INTEGER :: I

IF (LEN_TRIM(SCRATCH) .EQ. 0 .OR. TRIM(SCRATCH) .EQ. 'null') RETURN
DIRECTORY = TRIM(SCRATCH)
IF (OPERATING_SYSTEM .EQ. 'windows') THEN
   DO I = 1, LEN(DIRECTORY)
      IF (DIRECTORY(I:I) .EQ. '/') DIRECTORY(I:I) = PATH_SEPARATOR
   ENDDO
ENDIF
IF (DIRECTORY(LEN(DIRECTORY):) .NE. PATH_SEPARATOR) DIRECTORY = DIRECTORY // PATH_SEPARATOR
IF (OPERATING_SYSTEM .EQ. 'windows') THEN
   CALL RUN_SHELL_COMMAND(TRIM(DELETECOMMAND) // ' "' // DIRECTORY // '*"')
ELSE
   CALL RUN_SHELL_COMMAND(TRIM(DELETECOMMAND) // ' "' // DIRECTORY // '"*')
ENDIF
END SUBROUTINE CLEAN_SCRATCH_DIRECTORY
! *****************************************************************************

! *****************************************************************************
SUBROUTINE ALLOCATE_EMPTY_RASTER(RASTER,NCOLS,NROWS,NBANDS,XLLCORNER,YLLCORNER,CELLSIZE,NODATA_VALUE,PIXELTYPE)
! *****************************************************************************
! Populates a RASTER_TYPE's BIL header from the given geometry and allocates
! its data array (R4 for FLOAT, I2 for SIGNEDINT), initializing it to the
! NODATA_VALUE. Stops with an error for unsupported PIXELTYPE values.

TYPE(RASTER_TYPE), INTENT(INOUT) :: RASTER
INTEGER, INTENT(IN) :: NCOLS, NROWS, NBANDS
REAL, INTENT(IN) :: XLLCORNER, YLLCORNER, CELLSIZE, NODATA_VALUE
CHARACTER(10), INTENT(IN) :: PIXELTYPE
INTEGER :: NBITS

IF (TRIM(PIXELTYPE) .NE. 'FLOAT' .AND. TRIM(PIXELTYPE) .NE. 'SIGNEDINT' ) THEN
   WRITE(*,*) "Error in ALLOCATE_EMPTY_RASTER. For now, only available PIXELTYPE are 'FLOAT' and 'SIGNEDINT'."
   STOP
ENDIF

NBITS = 0   ! unreachable default; PIXELTYPE is validated above
SELECT CASE(TRIM(PIXELTYPE))
   CASE('FLOAT')
      NBITS=32
   CASE('SIGNEDINT')
      NBITS=16
END SELECT

! Thse three variables are used by ELMFIRE but are not in the BIL header:
RASTER%XLLCORNER    = XLLCORNER
RASTER%YLLCORNER    = YLLCORNER
RASTER%CELLSIZE     = CELLSIZE

! These variables, in order, get written to the BIL header
RASTER%BYTEORDER     = 'I'
RASTER%LAYOUT        = 'BIL'
RASTER%NROWS         = NROWS
RASTER%NCOLS         = NCOLS
RASTER%NBANDS        = NBANDS
RASTER%NBITS         = NBITS
RASTER%BANDROWBYTES  = (RASTER%NBITS/8) * RASTER%NCOLS
RASTER%TOTALROWBYTES = RASTER%BANDROWBYTES * RASTER%NBANDS
RASTER%PIXELTYPE     = PIXELTYPE
RASTER%ULXMAP        = RASTER%XLLCORNER + 0.5*RASTER%CELLSIZE
RASTER%ULYMAP        = RASTER%YLLCORNER - 0.5*RASTER%CELLSIZE + REAL(RASTER%NROWS)*RASTER%CELLSIZE
RASTER%XDIM          = RASTER%CELLSIZE
RASTER%YDIM          = RASTER%CELLSIZE
RASTER%NODATA_VALUE  = NODATA_VALUE
 
SELECT CASE(TRIM(PIXELTYPE))
   CASE('FLOAT')
      ALLOCATE(RASTER%R4(1:NCOLS,1:NROWS,1:NBANDS))
      RASTER%R4(:,:,:) = NODATA_VALUE
   CASE('SIGNEDINT')
      ALLOCATE(RASTER%I2(1:NCOLS,1:NROWS,1:NBANDS))
      RASTER%I2(:,:,:) = NINT(NODATA_VALUE,2)
   CASE DEFAULT
      CONTINUE
END SELECT

! *****************************************************************************
END SUBROUTINE ALLOCATE_EMPTY_RASTER
! *****************************************************************************

! *****************************************************************************
SUBROUTINE MAP_FINE_TO_COARSE(COARSE,FINE,ICOL_COARSE,IROW_COARSE)
! *****************************************************************************
! Builds lookup tables mapping each fine-grid column/row to the coarse-grid
! column/row that contains it, returned (clamped to coarse extent) in the
! ICOL_COARSE and IROW_COARSE arrays.

TYPE(RASTER_TYPE), INTENT(IN) :: COARSE, FINE
INTEGER, DIMENSION(:), INTENT(OUT) :: ICOL_COARSE, IROW_COARSE !fine to coarse

INTEGER :: ICOL, IROW
REAL, ALLOCATABLE, DIMENSION (:) :: X, Y

ALLOCATE(X(1:FINE%NCOLS))
ALLOCATE(Y(1:FINE%NROWS))

DO ICOL = 1, FINE%NCOLS
   X(ICOL) = FINE%XLLCORNER + (REAL(ICOL) - 0.5) * FINE%CELLSIZE
ENDDO

DO IROW = 1, FINE%NROWS
   Y(IROW) = FINE%YLLCORNER + (REAL(IROW) - 0.5) * FINE%CELLSIZE
ENDDO

ICOL_COARSE(:) = MIN(MAX(CEILING( (X(:) - COARSE%XLLCORNER) / COARSE%CELLSIZE),1),COARSE%NCOLS)
IROW_COARSE(:) = MIN(MAX(CEILING( (Y(:) - COARSE%YLLCORNER) / COARSE%CELLSIZE),1),COARSE%NROWS)

DEALLOCATE(X)
DEALLOCATE(Y)

! *****************************************************************************
END SUBROUTINE MAP_FINE_TO_COARSE
! *****************************************************************************

! *****************************************************************************
INTEGER FUNCTION ICOL_FROM_X(X,XLL,CELLSIZE)
! *****************************************************************************
! Returns the raster column index containing the x-coordinate X, given the
! lower-left x (XLL) and CELLSIZE.

REAL, INTENT(IN) :: X,XLL,CELLSIZE
REAL :: DIST

DIST = X - XLL 

ICOL_FROM_X = CEILING(DIST / CELLSIZE)

! *****************************************************************************
END FUNCTION ICOL_FROM_X
! *****************************************************************************

! *****************************************************************************
INTEGER FUNCTION IROW_FROM_Y(Y,YLL,CELLSIZE)
! *****************************************************************************
! Returns the raster row index containing the y-coordinate Y, given the
! lower-left y (YLL) and CELLSIZE.

REAL, INTENT(IN) :: Y,YLL,CELLSIZE
REAL :: DIST

DIST = Y - YLL 

IROW_FROM_Y = CEILING(DIST / CELLSIZE)

! *****************************************************************************
END FUNCTION IROW_FROM_Y
! *****************************************************************************

! *****************************************************************************
REAL FUNCTION X_FROM_ICOL(ICOL,XLL,CELLSIZE)
! *****************************************************************************
! Returns the x-coordinate of the center of raster column ICOL, given the
! lower-left x (XLL) and CELLSIZE.

INTEGER, INTENT(IN) :: ICOL
REAL, INTENT(IN) :: XLL, CELLSIZE

X_FROM_ICOL = XLL + (REAL(ICOL)-0.5) * CELLSIZE 

! *****************************************************************************
END FUNCTION X_FROM_ICOL
! *****************************************************************************

! *****************************************************************************
REAL FUNCTION Y_FROM_IROW(IROW,YLL,CELLSIZE)
! *****************************************************************************
! Returns the y-coordinate of the center of raster row IROW, given the
! lower-left y (YLL) and CELLSIZE.

INTEGER, INTENT(IN) :: IROW
REAL, INTENT(IN) :: YLL, CELLSIZE

Y_FROM_IROW = YLL + (REAL(IROW)-0.5) * CELLSIZE 

! *****************************************************************************
END FUNCTION Y_FROM_IROW
! *****************************************************************************

! *****************************************************************************
SUBROUTINE UPDATE_WD_RASTER(L,WD_LO,WD_HI,F)
! *****************************************************************************
! Temporally interpolates wind direction (fraction F between the WD_LO and
! WD_HI rasters) for every node in linked list L, updating each node's
! WD20_INTERP and WD20_NOW; interpolation is done through the 180-deg opposite
! to avoid wrap-around discontinuities, and PERTURB_WD is applied.

TYPE (DLL), INTENT(INOUT) :: L
REAL, DIMENSION(:,:), INTENT(IN) :: WD_LO, WD_HI
REAL, INTENT(IN) :: F
INTEGER :: I
TYPE(NODE), POINTER :: C

C => L%HEAD
DO I = 1, L%NUM_NODES
   CALL UPDATE_WD_RASTER_SINGLE(C, WD_LO, WD_HI, F)
   C => C%NEXT
ENDDO

! *****************************************************************************
END SUBROUTINE UPDATE_WD_RASTER
! *****************************************************************************

! *****************************************************************************
SUBROUTINE UPDATE_WD_RASTER_SINGLE(NODEIN,WD_LO,WD_HI,F)
! *****************************************************************************
! Same as INTERP_WD_RASTER but for a single node NODEIN: temporally interpolates
! wind direction between WD_LO and WD_HI (fraction F) and updates the node's
! WD20_INTERP and WD20_NOW, applying PERTURB_WD.

TYPE(NODE), POINTER, INTENT(IN) :: NODEIN
REAL, DIMENSION(:,:), INTENT(IN) :: WD_LO, WD_HI
REAL, INTENT(IN) :: F
INTEGER :: ICOL, IROW
REAL :: WD1TO, WD2TO, WDTO
TYPE(NODE), POINTER :: C

C => NODEIN

ICOL = ICOL_ANALYSIS_F2C(C%IX)
IROW = IROW_ANALYSIS_F2C(C%IY)

if (POINT_WIND_TO_CENTER) then
   C%WD20_INTERP = WD_TO_CENTER
else
   WD1TO = WD_LO(ICOL,IROW) + 180. ; IF (WD1TO .GT. 360) WD1TO = WD1TO - 360.
   WD2TO = WD_HI(ICOL,IROW) + 180. ; IF (WD2TO .GT. 360) WD2TO = WD2TO - 360.
   WDTO = WD1TO + F * (WD2TO - WD1TO)
   C%WD20_INTERP = WDTO + 180. + PERTURB_WD
   IF (C%WD20_INTERP .GT. 360.) C%WD20_INTERP = C%WD20_INTERP - 360.
   IF (C%WD20_INTERP .LT.   0.) C%WD20_INTERP = C%WD20_INTERP + 360.
endif 

C%WD20_NOW = C%WD20_INTERP
  
! *****************************************************************************
END SUBROUTINE UPDATE_WD_RASTER_SINGLE
! *****************************************************************************

! *****************************************************************************
SUBROUTINE INTERP_RASTER_LINKEDLIST(L,LO,HI,F,IQUANTITY)
! *****************************************************************************
! Temporally interpolates a weather quantity (selected by IQUANTITY: 1-m/10-h/
! 100-h/live-herb/live-woody moisture, foliar MC, or wind speed) between the LO
! and HI rasters (fraction F) for every node in list L using nearest-cell
! lookup, applying the matching PERTURB_* offset and floor; for wind speed it
! also recomputes WS20_NOW and the midflame wind WSMF.

TYPE (DLL), INTENT(INOUT) :: L
REAL, DIMENSION(:,:), INTENT(IN ) :: LO,HI
INTEGER, INTENT(IN) :: IQUANTITY
REAL, INTENT(IN) :: F
INTEGER :: I,ICOL,IROW
TYPE(NODE), POINTER :: C
REAL, PARAMETER :: CONVERSION_FACTOR = 5280./60.

C => L%HEAD

SELECT CASE (IQUANTITY)

   CASE (1)
      DO I = 1, L%NUM_NODES
         ICOL = WX_ICOL_FROM_ANALYSIS_IX(C%IX)
         IROW = WX_IROW_FROM_ANALYSIS_IY(C%IY)
         C%M1 = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
         C%M1 = MAX (C%M1 + PERTURB_M1, 0.01)
         C => C%NEXT
      ENDDO

   CASE (2)
      DO I = 1, L%NUM_NODES
         ICOL = WX_ICOL_FROM_ANALYSIS_IX(C%IX)
         IROW = WX_IROW_FROM_ANALYSIS_IY(C%IY)
         C%M10 = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
         C%M10 = MAX (C%M10 + PERTURB_M10, 0.01)
         C => C%NEXT
      ENDDO

   CASE (3)
      DO I = 1, L%NUM_NODES
         ICOL = WX_ICOL_FROM_ANALYSIS_IX(C%IX)
         IROW = WX_IROW_FROM_ANALYSIS_IY(C%IY)
         C%M100 = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
         C%M100 = MAX (C%M100 + PERTURB_M100, 0.01)
         C => C%NEXT
      ENDDO

   CASE (4)
      DO I = 1, L%NUM_NODES
         ICOL = WX_ICOL_FROM_ANALYSIS_IX(C%IX)
         IROW = WX_IROW_FROM_ANALYSIS_IY(C%IY)
         C%MLH = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
         C%MLH = MAX(C%MLH + PERTURB_MLH, 0.3)
         C => C%NEXT
      ENDDO

   CASE (5)
      DO I = 1, L%NUM_NODES
         ICOL = WX_ICOL_FROM_ANALYSIS_IX(C%IX)
         IROW = WX_IROW_FROM_ANALYSIS_IY(C%IY)
         C%MLW = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
         C%MLW = MAX(C%MLW + PERTURB_MLW, 0.6)
         C => C%NEXT
      ENDDO

   CASE (6)
      DO I = 1, L%NUM_NODES
         ICOL = WX_ICOL_FROM_ANALYSIS_IX(C%IX)
         IROW = WX_IROW_FROM_ANALYSIS_IY(C%IY)
         C%FMC = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
         C%FMC = C%FMC + PERTURB_FMC
         C => C%NEXT
      ENDDO

   CASE (7)
      DO I = 1, L%NUM_NODES
         ICOL = WX_ICOL_FROM_ANALYSIS_IX(C%IX)
         IROW = WX_IROW_FROM_ANALYSIS_IY(C%IY)
         C%WS20_INTERP = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
         C%WS20_INTERP = MAX(C%WS20_INTERP + PERTURB_WS, 0.0)
         C%WS20_NOW = C%WS20_INTERP
         C%WSMF = C%WS20_NOW * MAX((WAF%R4(C%IX,C%IY,1) + PERTURB_WAF),0.) * CONVERSION_FACTOR
         C => C%NEXT
      ENDDO

END SELECT

! *****************************************************************************
END SUBROUTINE INTERP_RASTER_LINKEDLIST
! *****************************************************************************

! *****************************************************************************
SUBROUTINE INTERP_RASTER_LINKEDLIST_BILINEAR(L,LO,HI,F,IQUANTITY)
! *****************************************************************************
! Like INTERP_RASTER_LINKEDLIST but uses bilinear spatial interpolation of the
! LO and HI weather rasters at each node before temporally blending by fraction
! F; updates the node field selected by IQUANTITY (and WSMF for wind speed).

TYPE (DLL), INTENT(INOUT) :: L
REAL, DIMENSION(:,:), INTENT(IN) :: LO,HI
INTEGER, INTENT(IN) :: IQUANTITY
REAL, INTENT(IN) :: F
TYPE(NODE), POINTER :: C
REAL, PARAMETER :: CONVERSION_FACTOR = 5280./60.
REAL :: X1, X2, Y1, Y2, CX, CY, PL, PH, PNOW
REAL :: Q11L, Q21L, Q12L, Q22L, Q11H, Q21H, Q12H, Q22H
INTEGER :: I, I1, I2, J1, J2

C => L%HEAD

DO I = 1, L%NUM_NODES
    
   CALL GET_BILINEAR_INTERPOLATE_COEFFS(C%IX, C%IY, X1, Y1, X2, Y2, I1, J1, I2, J2, CX, CY)

   Q12L = LO(I1, J2) ; Q22L = LO(I2, J2)
   Q11L = LO(I1, J1) ; Q21L = LO(I2, J1)
   PL   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, Q11L, Q21L, Q12L, Q22L)
         
   Q12H = HI(I1, J2) ; Q22H = HI(I2, J2)
   Q11H = HI(I1, J1) ; Q21H = HI(I2, J1)
   PH   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, Q11H, Q21H, Q12H, Q22H)
   
   PNOW = PL + F * (PH - PL)

   SELECT CASE (IQUANTITY)
      CASE (1)
         C%M1   = MAX (PNOW + PERTURB_M1  , 0.01)
      CASE (2)
         C%M10  = MAX (PNOW + PERTURB_M10 , 0.01)
      CASE (3)
         C%M100 = MAX (PNOW + PERTURB_M100, 0.01)
      CASE (4)
         C%MLH  = MAX (PNOW + PERTURB_MLH , 0.30)
      CASE (5)
         C%MLW  = MAX (PNOW + PERTURB_MLW , 0.60)
      CASE (6)
         C%FMC = PNOW + PERTURB_FMC
      CASE (7)
         C%WS20_INTERP = MAX(PNOW + PERTURB_WS, 0.0)
         C%WS20_NOW = C%WS20_INTERP
         C%WSMF = C%WS20_NOW * MAX((WAF%R4(C%IX,C%IY,1) + PERTURB_WAF),0.) * CONVERSION_FACTOR
      END SELECT
   
   C => C%NEXT

ENDDO

! *****************************************************************************
END SUBROUTINE INTERP_RASTER_LINKEDLIST_BILINEAR
! *****************************************************************************

! *****************************************************************************
SUBROUTINE INTERP_WIND_LINKEDLIST_BILINEAR(L,WSLO,WSHI,WDLO,WDHI,F)
! *****************************************************************************
! Bilinearly + temporally interpolates the wind field for every node in list L:
! converts the LO/HI speed and direction rasters to U/V components, interpolates
! those, then recombines into WS20_NOW/WD20_NOW and the midflame wind WSMF,
! applying PERTURB_WS/WD/WAF. Vector interpolation avoids direction wrap issues.

TYPE (DLL), INTENT(INOUT) :: L
REAL, DIMENSION(:,:), INTENT(IN) :: WSLO,WSHI,WDLO,WDHI
REAL, INTENT(IN) :: F
TYPE(NODE), POINTER :: C
REAL, PARAMETER :: CONVERSION_FACTOR = 5280./60.
REAL :: X1, X2, Y1, Y2, CX, CY
REAL :: WS11L, WS21L, WS12L, WS22L, WS11H, WS21H, WS12H, WS22H, &
        WD11L, WD21L, WD12L, WD22L, WD11H, WD21H, WD12H, WD22H, &
        UX11L, UX21L, UX12L, UX22L, UX11H, UX21H, UX12H, UX22H, &
        UY11L, UY21L, UY12L, UY22L, UY11H, UY21H, UY12H, UY22H
REAL :: UXL, UXH, UYL, UYH, UXNOW, UYNOW, WSNOW, WDNOW

INTEGER :: I, I1, I2, J1, J2

C => L%HEAD

DO I = 1, L%NUM_NODES
    
   CALL GET_BILINEAR_INTERPOLATE_COEFFS(C%IX, C%IY, X1, Y1, X2, Y2, I1, J1, I2, J2, CX, CY)

! Wind speed and wind direction (lo)
   WS12L = WSLO(I1, J2) ; WS22L = WSLO(I2, J2)
   WS11L = WSLO(I1, J1) ; WS21L = WSLO(I2, J1)
   
   WD12L = WDLO(I1, J2) ; WD22L = WDLO(I2, J2)
   WD11L = WDLO(I1, J1) ; WD21L = WDLO(I2, J1)

! Convert to x and y components
   UX12L = UX_FROM_WSWD(WS12L, WD12L) ; UX22L = UX_FROM_WSWD(WS22L, WD22L)
   UX11L = UX_FROM_WSWD(WS11L, WD11L) ; UX21L = UX_FROM_WSWD(WS21L, WD21L)

   UY12L = UY_FROM_WSWD(WS12L, WD12L) ; UY22L = UY_FROM_WSWD(WS22L, WD22L)
   UY11L = UY_FROM_WSWD(WS11L, WD11L) ; UY21L = UY_FROM_WSWD(WS21L, WD21L)
   
! Interpolate spatially
   UXL   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, UX11L, UX21L, UX12L, UX22L)
   UYL   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, UY11L, UY21L, UY12L, UY22L)

! Wind speed and wind direction (hi)
   WS12H = WSHI(I1, J2) ; WS22H = WSHI(I2, J2)
   WS11H = WSHI(I1, J1) ; WS21H = WSHI(I2, J1)
   
   WD12H = WDHI(I1, J2) ; WD22H = WDHI(I2, J2)
   WD11H = WDHI(I1, J1) ; WD21H = WDHI(I2, J1)

! Convert to x and y components
   UX12H = UX_FROM_WSWD(WS12H, WD12H) ; UX22H = UX_FROM_WSWD(WS22H, WD22H)
   UX11H = UX_FROM_WSWD(WS11H, WD11H) ; UX21H = UX_FROM_WSWD(WS21H, WD21H)

   UY12H = UY_FROM_WSWD(WS12H, WD12H) ; UY22H = UY_FROM_WSWD(WS22H, WD22H)
   UY11H = UY_FROM_WSWD(WS11H, WD11H) ; UY21H = UY_FROM_WSWD(WS21H, WD21H)
   
! Interpolate spatially
   UXH   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, UX11H, UX21H, UX12H, UX22H)
   UYH   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, UY11H, UY21H, UY12H, UY22H)

! Interpolate temporally
   UXNOW = UXL + F * (UXH - UXL)
   UYNOW = UYL + F * (UYH - UYL)
   
! Convert to wind speed and direction
   WDNOW = 0.   ! exhaustive branches below set it; default keeps it defined
   IF      (UXNOW .EQ. 0. .AND. UYNOW .EQ. 0.) THEN
      WDNOW = 0.
   ELSE IF (UXNOW .GT. 0. .AND. UYNOW .EQ. 0.) THEN
      WDNOW = 0.5*PI
   ELSE IF (UXNOW .LT. 0. .AND. UYNOW .EQ. 0.) THEN          
      WDNOW = 1.5*PI          
   ELSE IF (UXNOW .EQ. 0. .AND. UYNOW .GT. 0.) THEN
      WDNOW = 0.0*PI
   ELSE IF (UXNOW .EQ. 0. .AND. UYNOW .LT. 0.) THEN          
      WDNOW = 1.0*PI
   ELSE
      IF (UXNOW .GT. 0. .AND. UYNOW .GT. 0.) WDNOW = 0.     + ATAN(  UXNOW /  UYNOW ) 
      IF (UXNOW .GT. 0. .AND. UYNOW .LT. 0.) WDNOW = 0.5*PI + ATAN( -UYNOW /  UXNOW )
      IF (UXNOW .LT. 0. .AND. UYNOW .LT. 0.) WDNOW = PI     + ATAN(  UXNOW /  UYNOW )
      IF (UXNOW .LT. 0. .AND. UYNOW .GT. 0.) WDNOW = 1.5*PI + ATAN(  UYNOW / ABS(UXNOW) )
   ENDIF

   WSNOW = SQRT(UXNOW*UXNOW + UYNOW*UYNOW)
   
   WDNOW = WDNOW * 180. / PI
   WDNOW = WDNOW + 180. 
   IF (WDNOW .GT. 360) WDNOW = WDNOW - 360.

   C%WD20_INTERP = WDNOW + PERTURB_WD
   IF (C%WD20_INTERP .GT. 360.) C%WD20_INTERP = C%WD20_INTERP - 360.
   IF (C%WD20_INTERP .LT.   0.) C%WD20_INTERP = C%WD20_INTERP + 360.
   C%WD20_NOW = C%WD20_INTERP

   C%WS20_INTERP = MAX(WSNOW + PERTURB_WS, 0.0)
   C%WS20_NOW = C%WS20_INTERP
   C%WSMF = C%WS20_NOW * MAX((WAF%R4(C%IX,C%IY,1) + PERTURB_WAF),0.) * CONVERSION_FACTOR
   
   C => C%NEXT

ENDDO

! *****************************************************************************
END SUBROUTINE INTERP_WIND_LINKEDLIST_BILINEAR
! *****************************************************************************

! *****************************************************************************
SUBROUTINE INTERP_WIND_SINGLE_BILINEAR(NODEIN,WSLO,WSHI,WDLO,WDHI,F)
! *****************************************************************************
! Single-node version of INTERP_WIND_LINKEDLIST_BILINEAR: bilinearly and
! temporally interpolates the wind field at NODEIN via U/V components and sets
! its WS20_NOW, WD20_NOW, and midflame wind WSMF (with PERTURB_WS/WD/WAF).

TYPE(NODE), POINTER, INTENT(OUT) :: NODEIN
REAL, DIMENSION(:,:), INTENT(IN) :: WSLO,WSHI,WDLO,WDHI
REAL, INTENT(IN) :: F
TYPE(NODE), POINTER :: C
REAL, PARAMETER :: CONVERSION_FACTOR = 5280./60.
REAL :: X1, X2, Y1, Y2, CX, CY
REAL :: WS11L, WS21L, WS12L, WS22L, WS11H, WS21H, WS12H, WS22H, &
        WD11L, WD21L, WD12L, WD22L, WD11H, WD21H, WD12H, WD22H, &
        UX11L, UX21L, UX12L, UX22L, UX11H, UX21H, UX12H, UX22H, &
        UY11L, UY21L, UY12L, UY22L, UY11H, UY21H, UY12H, UY22H
REAL :: UXL, UXH, UYL, UYH, UXNOW, UYNOW, WSNOW, WDNOW

INTEGER :: I1, I2, J1, J2

C => NODEIN
    
CALL GET_BILINEAR_INTERPOLATE_COEFFS(C%IX, C%IY, X1, Y1, X2, Y2, I1, J1, I2, J2, CX, CY)

! Wind speed and wind direction (lo)
WS12L = WSLO(I1, J2) ; WS22L = WSLO(I2, J2)
WS11L = WSLO(I1, J1) ; WS21L = WSLO(I2, J1)
   
WD12L = WDLO(I1, J2) ; WD22L = WDLO(I2, J2)
WD11L = WDLO(I1, J1) ; WD21L = WDLO(I2, J1)

! Convert to x and y components
UX12L = UX_FROM_WSWD(WS12L, WD12L) ; UX22L = UX_FROM_WSWD(WS22L, WD22L)
UX11L = UX_FROM_WSWD(WS11L, WD11L) ; UX21L = UX_FROM_WSWD(WS21L, WD21L)

UY12L = UY_FROM_WSWD(WS12L, WD12L) ; UY22L = UY_FROM_WSWD(WS22L, WD22L)
UY11L = UY_FROM_WSWD(WS11L, WD11L) ; UY21L = UY_FROM_WSWD(WS21L, WD21L)
   
! Interpolate spatially
UXL   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, UX11L, UX21L, UX12L, UX22L)
UYL   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, UY11L, UY21L, UY12L, UY22L)

! Wind speed and wind direction (hi)
WS12H = WSHI(I1, J2) ; WS22H = WSHI(I2, J2)
WS11H = WSHI(I1, J1) ; WS21H = WSHI(I2, J1)
WD12H = WDHI(I1, J2) ; WD22H = WDHI(I2, J2)
WD11H = WDHI(I1, J1) ; WD21H = WDHI(I2, J1)

! Convert to x and y components
UX12H = UX_FROM_WSWD(WS12H, WD12H) ; UX22H = UX_FROM_WSWD(WS22H, WD22H)
UX11H = UX_FROM_WSWD(WS11H, WD11H) ; UX21H = UX_FROM_WSWD(WS21H, WD21H)

UY12H = UY_FROM_WSWD(WS12H, WD12H) ; UY22H = UY_FROM_WSWD(WS22H, WD22H)
UY11H = UY_FROM_WSWD(WS11H, WD11H) ; UY21H = UY_FROM_WSWD(WS21H, WD21H)
   
! Interpolate spatially
UXH   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, UX11H, UX21H, UX12H, UX22H)
UYH   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, UY11H, UY21H, UY12H, UY22H)

! Interpolate temporally
UXNOW = UXL + F * (UXH - UXL)
UYNOW = UYL + F * (UYH - UYL)
   
! Convert to wind speed and direction
WDNOW = 0.   ! exhaustive branches below set it; default keeps it defined
IF      (UXNOW .EQ. 0. .AND. UYNOW .EQ. 0.) THEN
   WDNOW = 0.
ELSE IF (UXNOW .GT. 0. .AND. UYNOW .EQ. 0.) THEN
   WDNOW = 0.5*PI
ELSE IF (UXNOW .LT. 0. .AND. UYNOW .EQ. 0.) THEN          
   WDNOW = 1.5*PI          
ELSE IF (UXNOW .EQ. 0. .AND. UYNOW .GT. 0.) THEN
   WDNOW = 0.0*PI
ELSE IF (UXNOW .EQ. 0. .AND. UYNOW .LT. 0.) THEN          
   WDNOW = 1.0*PI
ELSE
   IF (UXNOW .GT. 0. .AND. UYNOW .GT. 0.) WDNOW = 0.     + ATAN(  UXNOW /  UYNOW ) 
   IF (UXNOW .GT. 0. .AND. UYNOW .LT. 0.) WDNOW = 0.5*PI + ATAN( -UYNOW /  UXNOW )
   IF (UXNOW .LT. 0. .AND. UYNOW .LT. 0.) WDNOW = PI     + ATAN(  UXNOW /  UYNOW )
   IF (UXNOW .LT. 0. .AND. UYNOW .GT. 0.) WDNOW = 1.5*PI + ATAN(  UYNOW / ABS(UXNOW) )
ENDIF

WSNOW = SQRT(UXNOW*UXNOW + UYNOW*UYNOW)
   
WDNOW = WDNOW * 180. / PI
WDNOW = WDNOW + 180. 
IF (WDNOW .GT. 360) WDNOW = WDNOW - 360.

C%WD20_INTERP = WDNOW + PERTURB_WD
IF (C%WD20_INTERP .GT. 360.) C%WD20_INTERP = C%WD20_INTERP - 360.
IF (C%WD20_INTERP .LT.   0.) C%WD20_INTERP = C%WD20_INTERP + 360.
C%WD20_NOW = C%WD20_INTERP

C%WS20_INTERP = MAX(WSNOW + PERTURB_WS, 0.0)
C%WS20_NOW = C%WS20_INTERP
C%WSMF = C%WS20_NOW * MAX((WAF%R4(C%IX,C%IY,1) + PERTURB_WAF),0.) * CONVERSION_FACTOR
   
! *****************************************************************************
END SUBROUTINE INTERP_WIND_SINGLE_BILINEAR
! *****************************************************************************

! *****************************************************************************
REAL FUNCTION UX_FROM_WSWD(WS,WD)
! *****************************************************************************
! Returns the x (east-west) component of a wind vector from speed WS and
! meteorological direction WD (degrees).
   REAL, INTENT(IN) :: WS, WD
   UX_FROM_WSWD = WS * COS( (WD + 90.) * PI / 180.) 
! *****************************************************************************
END FUNCTION UX_FROM_WSWD
! *****************************************************************************

! *****************************************************************************
REAL FUNCTION UY_FROM_WSWD(WS,WD)
! *****************************************************************************
! Returns the y (north-south) component of a wind vector from speed WS and
! meteorological direction WD (degrees).
   REAL, INTENT(IN) :: WS, WD
   UY_FROM_WSWD = WS * SIN( (WD - 90.) * PI / 180.) 
! *****************************************************************************
END FUNCTION UY_FROM_WSWD
! *****************************************************************************

! *****************************************************************************
SUBROUTINE GET_BILINEAR_INTERPOLATE_COEFFS(IX, IY, X1, Y1, X2, Y2, I1, J1, I2, J2, CX, CY)
! *****************************************************************************
! For a fuel-grid cell (IX,IY), returns the bounding weather-grid cell indices
! (I1,J1)-(I2,J2), their corner coordinates (X1,Y1)-(X2,Y2), and the cell center
! (CX,CY) needed by BILINEAR_INTERPOLATE.

INTEGER, INTENT(IN) :: IX, IY
REAL, INTENT(OUT) :: X1, Y1, X2, Y2, CX, CY
INTEGER, INTENT(OUT) :: I1, J1, I2, J2

REAL :: XLL_WX, YLL_WX, CELLSIZE_WX, XLL_FUEL, YLL_FUEL, CELLSIZE_FUEL
INTEGER :: NCOL_WX, NROW_WX

XLL_WX        = WS%XLLCORNER
YLL_WX        = WS%YLLCORNER
CELLSIZE_WX   = WS%CELLSIZE
NCOL_WX       = WS%NCOLS
NROW_WX       = WS%NROWS

XLL_FUEL      = ADJ%XLLCORNER
YLL_FUEL      = ADJ%YLLCORNER
CELLSIZE_FUEL = ADJ%CELLSIZE

CX = XLL_FUEL + (REAL(IX) - 0.5) * CELLSIZE_FUEL
CY = YLL_FUEL + (REAL(IY) - 0.5) * CELLSIZE_FUEL

I1 = 1 + NINT( (CX - XLL_WX) / CELLSIZE_WX )
I1 = MAX ( MIN(I1    , NCOL_WX), 1 )
I2 = MAX ( MIN(I1 + 1, NCOL_WX), 1 )

J1 = 1 + NINT( (CY - YLL_WX) / CELLSIZE_WX )
J1 = MAX ( MIN(J1,     NROW_WX), 1 )
J2 = MAX ( MIN(J1 + 1, NROW_WX), 1 )

X1 = XLL_WX + (REAL(I1) - 0.5) * CELLSIZE_WX
X2 = XLL_WX + (REAL(I2) - 0.5) * CELLSIZE_WX
Y1 = YLL_WX + (REAL(J1) - 0.5) * CELLSIZE_WX
Y2 = YLL_WX + (REAL(J2) - 0.5) * CELLSIZE_WX

! *****************************************************************************
END SUBROUTINE GET_BILINEAR_INTERPOLATE_COEFFS
! *****************************************************************************

! *****************************************************************************
REAL FUNCTION BILINEAR_INTERPOLATE(X, Y, X1, Y1, X2, Y2, Q11, Q21, Q12, Q22)
! *****************************************************************************
! Bilinearly interpolates the value at point (X,Y) from the four corner values
! Q11/Q21/Q12/Q22 at corners (X1,Y1)-(X2,Y2). Degenerates gracefully to 1-D
! interpolation when the cell has zero width or height.

REAL, INTENT(IN) :: X, Y, X1, Y1, X2, Y2, Q11, Q21, Q12, Q22
REAL :: X2MX1, Y2MY1, DENOM, X2MX, Y2MY, XMX1, YMY1, NUMER1, NUMER2, NUMER3, &
        NUMER4, P

X2MX1 = X2 - X1
Y2MY1 = Y2 - Y1
DENOM = X2MX1 * Y2MY1 

X2MX  = X2 - X
Y2MY  = Y2 - Y
XMX1  = X - X1
YMY1  = Y - Y1

IF (DENOM .GT. 1E-3) THEN
   NUMER1 = X2MX * Y2MY * Q11
   NUMER2 = XMX1 * Y2MY * Q21
   NUMER3 = X2MX * YMY1 * Q12
   NUMER4 = XMX1 * YMY1 * Q22
   P = ( NUMER1 + NUMER2 + NUMER3 + NUMER4 ) / DENOM
ELSE IF (X2MX1 .LT. 1E-3) THEN ! Only interpolate in y direction
   P = ( Q11 * Y2MY + Q12 * YMY1 ) / Y2MY1
ELSE ! Only interpolate in x direction
   P = ( Q11 * X2MX + Q21 * XMX1 ) / X2MX1
ENDIF

BILINEAR_INTERPOLATE = P 

! *****************************************************************************
END FUNCTION BILINEAR_INTERPOLATE
! *****************************************************************************

! *****************************************************************************
INTEGER FUNCTION WX_ICOL_FROM_ANALYSIS_IX(IX_IN)
! *****************************************************************************
! Maps an analysis/fuel-grid column IX_IN to the corresponding weather-grid
! column, clamped to the valid weather-raster column range.

INTEGER, INTENT(IN) :: IX_IN
INTEGER :: IX

IX                       = MAX ( MIN (IX_IN,                 FBFM%NCOLS ), 1 )
WX_ICOL_FROM_ANALYSIS_IX = MAX ( MIN (ICOL_ANALYSIS_F2C(IX),   WS%NCOLS ), 1 )

! *****************************************************************************
END FUNCTION WX_ICOL_FROM_ANALYSIS_IX
! *****************************************************************************

! *****************************************************************************
INTEGER FUNCTION WX_IROW_FROM_ANALYSIS_IY(IY_IN)
! *****************************************************************************
! Maps an analysis/fuel-grid row IY_IN to the corresponding weather-grid row,
! clamped to the valid weather-raster row range.

INTEGER, INTENT(IN) :: IY_IN
INTEGER :: IY

IY                       = MAX ( MIN (IY_IN,                 FBFM%NROWS ), 1 )
WX_IROW_FROM_ANALYSIS_IY = MAX ( MIN (IROW_ANALYSIS_F2C(IY),   WS%NROWS ), 1 )

! *****************************************************************************
END FUNCTION WX_IROW_FROM_ANALYSIS_IY
! *****************************************************************************

! *****************************************************************************
INTEGER FUNCTION ICOL_FINE_TO_COARSE(IX_IN)
! *****************************************************************************
! Maps a fine-grid column IX_IN to the corresponding coarse weather-grid column
! via ICOL_ANALYSIS_F2C, clamped to the weather-raster column range.

INTEGER, INTENT(IN) :: IX_IN
INTEGER :: IX

IX = MAX(MIN(IX_IN, FBFM%NCOLS),1)
ICOL_FINE_TO_COARSE = MAX(MIN(ICOL_ANALYSIS_F2C(IX),WS%NCOLS),1)

! *****************************************************************************
END FUNCTION ICOL_FINE_TO_COARSE
! *****************************************************************************

! *****************************************************************************
SUBROUTINE INTERP_RASTER_LINKEDLIST_SINGLE(NODEIN,LO,HI,F,IQUANTITY)
! *****************************************************************************
! Single-node, nearest-cell version of INTERP_RASTER_LINKEDLIST: temporally
! interpolates the weather quantity selected by IQUANTITY between LO and HI
! (fraction F) at NODEIN and applies the matching PERTURB_* offset/floor.

TYPE (NODE), POINTER, INTENT(OUT) :: NODEIN
REAL, DIMENSION(:,:), INTENT(IN) :: LO,HI
INTEGER, INTENT(IN) :: IQUANTITY
REAL, INTENT(IN) :: F
INTEGER :: ICOL,IROW
TYPE(NODE), POINTER :: C
REAL, PARAMETER :: CONVERSION_FACTOR = 5280./60.

C => NODEIN
ICOL = ICOL_ANALYSIS_F2C(C%IX)
IROW = IROW_ANALYSIS_F2C(C%IY)

SELECT CASE (IQUANTITY)

   CASE (1)
      C%M1 = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
      C%M1 = MAX(C%M1 + PERTURB_M1, 0.01)

   CASE (2)
      C%M10 = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
      C%M10 = MAX(C%M10 + PERTURB_M10, 0.01)

   CASE (3)
      C%M100 = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
      C%M100 = MAX(C%M100 + PERTURB_M100, 0.01)

   CASE (4)
      C%MLH = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
      C%MLH = MAX(C%MLH + PERTURB_MLH, 0.3)

   CASE (5)
      C%MLW = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
      C%MLW = MAX(C%MLW + PERTURB_MLW, 0.6)

   CASE (6)
      C%FMC = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
      C%FMC = C%FMC + PERTURB_FMC

   CASE (7)
      C%WS20_INTERP = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
      C%WS20_INTERP = MAX(C%WS20_INTERP + PERTURB_WS, 0.0)
      C%WS20_NOW = C%WS20_INTERP
      C%WSMF = C%WS20_NOW * MAX((WAF%R4(C%IX,C%IY,1) + PERTURB_WAF),0.) * CONVERSION_FACTOR

END SELECT

! *****************************************************************************
END SUBROUTINE INTERP_RASTER_LINKEDLIST_SINGLE
! *****************************************************************************

! *****************************************************************************
SUBROUTINE INTERP_RASTER_LINKEDLIST_SINGLE_BILINEAR(NODEIN,LO,HI,F,IQUANTITY)
! *****************************************************************************
! Single-node, bilinear version of INTERP_RASTER_LINKEDLIST_BILINEAR: spatially
! and temporally interpolates the weather quantity selected by IQUANTITY between
! LO and HI (fraction F) at NODEIN, applying the matching PERTURB_* offset.

TYPE (NODE), POINTER, INTENT(OUT) :: NODEIN
REAL, DIMENSION(:,:), INTENT(IN) :: LO,HI
INTEGER, INTENT(IN) :: IQUANTITY
REAL, INTENT(IN) :: F
TYPE(NODE), POINTER :: C
REAL, PARAMETER :: CONVERSION_FACTOR = 5280./60.
REAL :: X1, X2, Y1, Y2, CX, CY, PL, PH, PNOW
REAL :: Q11L, Q21L, Q12L, Q22L, Q11H, Q21H, Q12H, Q22H
INTEGER :: I1, I2, J1, J2

C => NODEIN

CALL GET_BILINEAR_INTERPOLATE_COEFFS(C%IX, C%IY, X1, Y1, X2, Y2, I1, J1, I2, J2, CX, CY)

Q12L = LO(I1, J2) ; Q22L = LO(I2, J2)
Q11L = LO(I1, J1) ; Q21L = LO(I2, J1)
PL   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, Q11L, Q21L, Q12L, Q22L)
         
Q12H = HI(I1, J2) ; Q22H = HI(I2, J2)
Q11H = HI(I1, J1) ; Q21H = HI(I2, J1)
PH   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, Q11H, Q21H, Q12H, Q22H)
   
PNOW = PL + F * (PH - PL)

SELECT CASE (IQUANTITY)
   CASE (1)
      C%M1   = MAX(PNOW + PERTURB_M1  , 0.01)
   CASE (2)
      C%M10  = MAX(PNOW + PERTURB_M10 , 0.01)
   CASE (3)
      C%M100 = MAX(PNOW + PERTURB_M100, 0.01)
   CASE (4)
      C%MLH  = MAX(PNOW + PERTURB_MLH , 0.30)
   CASE (5)
      C%MLW  = MAX(PNOW + PERTURB_MLW , 0.60)
   CASE (6)
      C%FMC  = PNOW + PERTURB_FMC
   CASE (7)
      C%WS20_INTERP = MAX(PNOW + PERTURB_WS, 0.0)
      C%WS20_NOW = C%WS20_INTERP
      C%WSMF = C%WS20_NOW * MAX((WAF%R4(C%IX,C%IY,1) + PERTURB_WAF),0.) * CONVERSION_FACTOR
END SELECT

! *****************************************************************************
END SUBROUTINE INTERP_RASTER_LINKEDLIST_SINGLE_BILINEAR
! *****************************************************************************

! *****************************************************************************
SUBROUTINE APPLY_WIND_FLUCTUATIONS(L)
! *****************************************************************************
! Applies random gust/veer fluctuations to every node in list L: perturbs the
! interpolated wind speed and direction by random factors scaled by the
! WIND_SPEED/DIRECTION_FLUCTUATION_INTENSITY parameters, updating WS20_NOW,
! WD20_NOW, and the midflame wind WSMF.

TYPE (DLL), INTENT(INOUT) :: L
TYPE(NODE), POINTER :: C
INTEGER :: I
REAL :: FAC1, FAC2, R2(1:2)
REAL, PARAMETER :: CONVERSION_FACTOR = 5280./60.

CALL RANDOM_NUMBER(R2)
R2(1) = WIND_SPEED_FLUCTUATION_INTENSITY     * (R2(1) - 0.5)
R2(2) = WIND_DIRECTION_FLUCTUATION_INTENSITY * (R2(2) - 0.5)

FAC1 = 1. + R2(1)
FAC2 = R2(2) * 360.

C => L%HEAD
DO I = 1, L%NUM_NODES
   C%WS20_NOW = FAC1 * C%WS20_INTERP
   C%WSMF = C%WS20_NOW * MAX((WAF%R4(C%IX,C%IY,1) + PERTURB_WAF),0.) * CONVERSION_FACTOR
   C%WD20_NOW = C%WD20_INTERP + FAC2
   IF (C%WD20_NOW .GT. 360.) C%WD20_NOW = C%WD20_NOW - 360.
   IF (C%WD20_NOW .LT.   0.) C%WD20_NOW = C%WD20_NOW + 360.
   C => C%NEXT
ENDDO

! *****************************************************************************
END SUBROUTINE APPLY_WIND_FLUCTUATIONS
! *****************************************************************************

! *****************************************************************************
ELEMENTAL TYPE(DLL) FUNCTION NEW_DLL()
! *****************************************************************************
! Returns an empty doubly-linked list (null head/tail pointers, zero nodes).

NEW_DLL = DLL(NULL(),NULL(),0)

! *****************************************************************************
END FUNCTION NEW_DLL
! *****************************************************************************
 
! *****************************************************************************
LOGICAL FUNCTION CHECK_BARRIER_BREACH(C)
! *****************************************************************************
! Returns .TRUE. if node C's flame length is large enough to breach the local
! fuel-break barrier (1.5x flame length, converted to meters, exceeds the cell's
! BARRIER_WIDTH), otherwise .FALSE.
   TYPE(NODE), POINTER :: C

   CHECK_BARRIER_BREACH = .TRUE.
   IF (0.3048 * 1.5 * C%FLAME_LENGTH .LE. BARRIER_WIDTH%R4(C%IX,C%IY,1)) THEN
      CHECK_BARRIER_BREACH = .FALSE.
   ENDIF 

! *****************************************************************************
END FUNCTION CHECK_BARRIER_BREACH
! *****************************************************************************

! *****************************************************************************
SUBROUTINE APPEND(DL2, IX, IY, T)
! *****************************************************************************
! Appends a new node for cell (IX,IY) added at time T to the tail of list DL2
! (calling INIT if the list is empty), allocating the node and caching its fuel
! model, adjustment factor, slope-squared, and (WUI build) building fuel model.

TYPE(DLL), INTENT(INOUT) :: DL2
INTEGER, INTENT(IN)      :: IX, IY
REAL(8), INTENT(IN) :: T
INTEGER, PARAMETER :: NO_DATA = -9999
 
TYPE(NODE), POINTER :: NP

! If the list is empty
IF (DL2%NUM_NODES == 0) THEN
   CALL INIT(DL2, IX, IY, T)
   RETURN
ENDIF

! Add new element ot the end
DL2%NUM_NODES = DL2%NUM_NODES + 1

NP => DL2%TAIL
ALLOCATE(DL2%TAIL)
DL2%TAIL%IX         =  IX
DL2%TAIL%IY         =  IY
DL2%TAIL%TIME_ADDED =  T

DL2%TAIL%IFBFM   =  FBFM%I2(IX,IY,1)

#ifdef _WUI
IF (USE_BLDG_SPREAD_MODEL) THEN
   IF (FBFM%I2(IX,IY,1) .EQ. 91) THEN
      IF(BLDG_FUEL_MODEL%I2(IX,IY,1) .NE. NO_DATA) THEN
         DL2%TAIL%IBLDGFM =  BLDG_FUEL_MODEL%I2(IX,IY,1)
      ELSE
         DL2%TAIL%IBLDGFM =  1
      ENDIF
   ELSE
      DL2%TAIL%IBLDGFM =  BLDG_FUEL_MODEL_CONSTANT
   ENDIF
ENDIF
#endif

DL2%TAIL%ADJ     =  ADJ%R4(IX,IY,1)
DL2%TAIL%TANSLP2 =  TANSLP2(MAX(MIN(NINT(SLP%R4(IX,IY,1)),90),0))

DL2%TAIL%PREV       => NP
DL2%TAIL%PREV%NEXT  => DL2%TAIL

! *****************************************************************************   
END SUBROUTINE APPEND
! *****************************************************************************

! *****************************************************************************
ELEMENTAL SUBROUTINE INIT(DL2, IX, IY, T)
! *****************************************************************************
! Initializes an empty list DL2 with its first node for cell (IX,IY) added at
! time T, allocating head/tail and caching the cell's fuel model, adjustment
! factor, slope-squared, and (WUI build) building fuel model.

TYPE(DLL), INTENT(INOUT) :: DL2
INTEGER, INTENT(IN)      :: IX, IY
REAL(8), INTENT(IN) :: T

INTEGER, PARAMETER :: NO_DATA = -9999

ALLOCATE(DL2%HEAD)
DL2%TAIL => DL2%HEAD
DL2%TAIL%IX = IX
DL2%TAIL%IY = IY
DL2%TAIL%IFBFM      =  FBFM%I2(IX,IY,1)
DL2%TAIL%ADJ        =  ADJ%R4(IX,IY,1)
DL2%TAIL%TANSLP2    =  TANSLP2(MAX(MIN(NINT(SLP%R4(IX,IY,1)),90),0))
DL2%TAIL%TIME_ADDED =  T
DL2%NUM_NODES = 1

#ifdef _WUI
IF (USE_BLDG_SPREAD_MODEL) THEN
   IF (FBFM%I2(IX,IY,1) .EQ. 91) THEN
      IF(BLDG_FUEL_MODEL%I2(IX,IY,1) .NE. NO_DATA) THEN
         DL2%TAIL%IBLDGFM =  BLDG_FUEL_MODEL%I2(IX,IY,1)
      ELSE
         DL2%TAIL%IBLDGFM =  1
      ENDIF
   ELSE
      DL2%TAIL%IBLDGFM =  BLDG_FUEL_MODEL_CONSTANT
   ENDIF
ENDIF
#endif
! *****************************************************************************
END SUBROUTINE INIT
! *****************************************************************************

! *****************************************************************************
SUBROUTINE DELETE_NODE(DL2, CURRENT)
! *****************************************************************************
! Removes node CURRENT from list DL2, relinking neighbors (handling head, tail,
! intermediate, and single-node cases), deallocating it, decrementing the node
! count, and resetting CURRENT to the previous (or new head/null) node.

TYPE(DLL), INTENT(INOUT) :: DL2
TYPE(NODE), POINTER, INTENT(INOUT) :: CURRENT
TYPE(NODE), POINTER :: NP

! IF (.NOT. ASSOCIATED(CURRENT)) THEN
!    WRITE(*,*) 'EXITING BECAUSE CURRENT NOT ASSOCIATED'
!    RETURN
! ENDIF

NP => CURRENT
CONTINUE

IF (ASSOCIATED(CURRENT%PREV) .AND. ASSOCIATED(CURRENT%NEXT)) THEN !Deleting intermediate node
   CURRENT%PREV%NEXT => CURRENT%NEXT
   CURRENT%NEXT%PREV => CURRENT%PREV
   CURRENT => CURRENT%PREV
CONTINUE

ELSE IF (ASSOCIATED(CURRENT%PREV) .AND. (.NOT. ASSOCIATED(CURRENT%NEXT))) THEN ! Deleting tail node
   CURRENT%PREV%NEXT => NULL()
   CURRENT => CURRENT%PREV
   DL2%TAIL => CURRENT
CONTINUE

ELSE IF (.NOT. ASSOCIATED(CURRENT%PREV) .AND. ASSOCIATED(CURRENT%NEXT)) THEN ! Deleting head node
   DL2%HEAD => CURRENT%NEXT
   CURRENT => DL2%HEAD
   CURRENT%PREV => NULL()
CONTINUE

ELSE IF (.NOT. ASSOCIATED(CURRENT%PREV) .AND. (.NOT. ASSOCIATED(CURRENT%NEXT))) THEN
   DL2%HEAD => NULL()
   DL2%TAIL => NULL()
   CURRENT => NULL()
ENDIF

DEALLOCATE(NP)

DL2%NUM_NODES = DL2%NUM_NODES - 1

! *****************************************************************************
END SUBROUTINE DELETE_NODE
! ***************************************************************************** 

! *****************************************************************************
SUBROUTINE TIDY(DL2)
! *****************************************************************************
! Deallocates every node in list DL2, freeing the entire linked list (used to
! tear down a list at end of use).

TYPE(DLL), INTENT(INOUT) :: DL2
TYPE(NODE), POINTER :: CURRENT, LAST

! INTEGER :: COUNT

! COUNT = 0 

IF (DL2%NUM_NODES .EQ. 1) THEN
   DEALLOCATE(DL2%HEAD)
ELSE
   CURRENT => DL2%HEAD
   DO
      LAST => CURRENT
      CURRENT => CURRENT%NEXT
      IF (ASSOCIATED(LAST)) THEN
         ! COUNT = COUNT + 1
         DEALLOCATE(LAST)
      END IF
      IF (ASSOCIATED(CURRENT, DL2%TAIL)) THEN
!         COUNT = COUNT + 1
         DEALLOCATE(CURRENT)
         EXIT
      ENDIF
   ENDDO
ENDIF
! *****************************************************************************
END SUBROUTINE TIDY
! *****************************************************************************

! *****************************************************************************
SUBROUTINE locate(xx,n,x,j)
! *****************************************************************************
! Given an array xx(1:n), and given a value x, returns a value j such that x is between
! xx(j) and xx(j+1). xx(1:n) must be monotonic, either increasing or decreasing. j=0
! or j=n is returned to indicate that x is out of range.

INTEGER j,n
REAL :: x,xx(n)
INTEGER jl,jm,ju
jl=0 !Initialize lower
ju=n+1 !and upper limits.
 10 if(ju-jl.gt.1)then !If we are not yet done,
       jm=(ju+jl)/2 !compute a midpoint,
       if((xx(n).ge.xx(1)).eqv.(x.ge.xx(jm)))then
          jl=jm !and replace either the lower limit
       else
          ju=jm !or the upper limit, as appropriate.
       endif
       goto 10 !Repeat until
    endif !the test condition 10 is satisfied.
      
if(x.eq.xx(1))then !Then set the output
   j=1
else if(x.eq.xx(n))then
   j=n-1
else
   j=jl
endif

! *****************************************************************************
END SUBROUTINE LOCATE
! *****************************************************************************

!******************************************************************************
INTEGER FUNCTION SOLAR_DAYS_IN_YEAR(YEAR)
INTEGER, INTENT(IN) :: YEAR
SOLAR_DAYS_IN_YEAR = 365
IF (MOD(YEAR,4) == 0 .AND. (MOD(YEAR,100) /= 0 .OR. MOD(YEAR,400) == 0)) SOLAR_DAYS_IN_YEAR = 366
END FUNCTION SOLAR_DAYS_IN_YEAR

FUNCTION SOLAR_INPUT_ERROR() RESULT(REASON)
CHARACTER(:), ALLOCATABLE :: REASON
REASON = ''
IF (.NOT. USE_DIURNAL_ADJUSTMENT_FACTOR) RETURN
IF (.NOT. IEEE_IS_FINITE(SUNRISE_HOUR) .OR. .NOT. IEEE_IS_FINITE(SUNSET_HOUR)) THEN
   REASON = 'SUNRISE_HOUR and SUNSET_HOUR must be finite UTC hours'
   RETURN
ENDIF
IF (SUNRISE_HOUR >= 0.0 .AND. SUNSET_HOUR >= 0.0) RETURN
IF (CURRENT_YEAR <= 0) THEN
   REASON = 'Automatic diurnal hours require positive CURRENT_YEAR'
ELSE IF (HOUR_OF_YEAR < 0 .OR. HOUR_OF_YEAR >= 24*SOLAR_DAYS_IN_YEAR(CURRENT_YEAR)) THEN
   REASON = 'Automatic diurnal hours require zero-based HOUR_OF_YEAR within CURRENT_YEAR'
ENDIF
END FUNCTION SOLAR_INPUT_ERROR

SUBROUTINE INITIALIZE_SOLAR_HOURS
INTEGER :: IERR
CHARACTER(:), ALLOCATABLE :: REASON
IF (.NOT. USE_DIURNAL_ADJUSTMENT_FACTOR) RETURN
REASON = SOLAR_INPUT_ERROR()
IF (LEN(REASON) > 0) CALL SPATIAL_ERROR(REASON)
IF (SUNRISE_HOUR >= 0.0 .AND. SUNSET_HOUR >= 0.0) RETURN
IF (IRANK_WORLD == PARALLEL_IO_RANK(1)) CALL SUNRISE_SUNSET_CALCS
CALL MPI_BCAST(SUNRISE_HOUR, 1, MPI_REAL, PARALLEL_IO_RANK(1), MPI_COMM_WORLD, IERR)
CALL MPI_BCAST(SUNSET_HOUR, 1, MPI_REAL, PARALLEL_IO_RANK(1), MPI_COMM_WORLD, IERR)
END SUBROUTINE INITIALIZE_SOLAR_HOURS

SUBROUTINE SUNRISE_SUNSET_CALCS
! Keep the existing NOAA equations and unwrapped UTC hours at the lower-left.
INTEGER :: DAY_OF_YEAR
REAL :: DAYS_PER_YEAR, GAMMA, EQTIME, DECL, HA_SUNRISE, LAT_RAD, &
        SUNRISE_H_UTC, SUNSET_H_UTC, LON_DEG, LAT_DEG, ACOS_ARG, DENOM
CHARACTER(:), ALLOCATABLE :: REASON
REASON = SOLAR_INPUT_ERROR()
IF (LEN(REASON) > 0) CALL SPATIAL_ERROR(REASON)
IF (.NOT. ALL(IEEE_IS_FINITE([ASP%XLLCORNER, ASP%YLLCORNER, ASP%CELLSIZE]))) &
   CALL SPATIAL_ERROR('Solar calculation requires finite aspect header coordinates/cell size')
IF (ASP%CELLSIZE <= 0.0) CALL SPATIAL_ERROR('Solar calculation requires positive aspect cell size')
CALL REQUIRE_ANALYSIS_SRS
CALL XY_TO_LATLON(ASP%XLLCORNER, ASP%YLLCORNER, LAT_DEG, LON_DEG)
LAT_RAD = LAT_DEG * PI / 180.
DAYS_PER_YEAR = REAL(SOLAR_DAYS_IN_YEAR(CURRENT_YEAR))
DAY_OF_YEAR = 1 + HOUR_OF_YEAR / 24
GAMMA = 2.0 * (PI/DAYS_PER_YEAR) * (DAY_OF_YEAR - 1)
EQTIME = 229.18 * (0.000075 + 0.001868*COS(GAMMA) - 0.032077*SIN(GAMMA) &
         - 0.014615*COS(2.*GAMMA) - 0.040849*SIN(2.*GAMMA))
DECL = 0.006918 - 0.399912*COS(GAMMA) + 0.070257*SIN(GAMMA) - 0.006758*COS(2.*GAMMA) &
       + 0.000907*SIN(2.*GAMMA) - 0.002697*COS(3.*GAMMA) + 0.00148*SIN(3.*GAMMA)
DENOM = COS(LAT_RAD)*COS(DECL)
IF (ABS(DENOM) <= TINY(DENOM)) CALL SPATIAL_ERROR('Unsupported solar no-rise/no-set location')
ACOS_ARG = COS(90.833*PI/180.) / DENOM - TAN(LAT_RAD)*TAN(DECL)
IF (.NOT. IEEE_IS_FINITE(ACOS_ARG)) CALL SPATIAL_ERROR('Nonfinite solar hour angle')
IF (ABS(ACOS_ARG) > 1.0) CALL SPATIAL_ERROR('Unsupported solar no-rise/no-set date/location')
HA_SUNRISE = ACOS(ACOS_ARG)
SUNRISE_H_UTC = (720. - 4.*(LON_DEG + HA_SUNRISE*180./PI) - EQTIME) / 60.
SUNSET_H_UTC = (720. - 4.*(LON_DEG - HA_SUNRISE*180./PI) - EQTIME) / 60.
IF (SUNRISE_HOUR < 0.0) SUNRISE_HOUR = SUNRISE_H_UTC
IF (SUNSET_HOUR < 0.0) SUNSET_HOUR = SUNSET_H_UTC
END SUBROUTINE SUNRISE_SUNSET_CALCS


!******************************************************************************

! *****************************************************************************
SUBROUTINE SHUTDOWN
! *****************************************************************************
! Performs orderly program shutdown: closes per-rank output files, frees all
! MPI shared-memory windows (raster and stats arrays), records final timings and
! optionally writes them to disk, cleans scratch files, then calls MPI_FINALIZE.

INTEGER :: I, IERR
CHARACTER(4) :: FOUR
LOGICAL :: LOPEN
CHARACTER(400) :: FN

IF (NUM_TIME_AT_BURNED_ACRES .GT. 0) THEN
   DO I = 0, NPROC - 1
      WRITE(FOUR, '(I4.4)') I
      FN = TRIM(OUTPUTS_DIRECTORY) // 'burned-acres-timings_' // FOUR // '.csv'
      INQUIRE(UNIT=LUBAT+I,OPENED=LOPEN)
      IF (LOPEN) CLOSE(LUBAT+I) 
   ENDDO
ENDIF

IF (PROCESS_TIMED_LOCATIONS) THEN
   DO I = 0, NPROC - 1
      WRITE(FOUR, '(I4.4)') I
      FN = TRIM(OUTPUTS_DIRECTORY) // 'timed-locations-events_' // FOUR // '.csv'
      INQUIRE(UNIT=LUTASL+I,OPENED=LOPEN)
      IF (LOPEN) CLOSE(LUTASL+I)
   ENDDO
ENDIF

IF (NPROC .GT. 1) THEN
   CALL MPI_WIN_FREE(WIN_WS           )
   CALL MPI_WIN_FREE(WIN_WD           )
   CALL MPI_WIN_FREE(WIN_M1           )
   CALL MPI_WIN_FREE(WIN_M10          )
   CALL MPI_WIN_FREE(WIN_M100         )
   IF (USE_ERC) THEN
      CALL MPI_WIN_FREE(WIN_ERC       )
      CALL MPI_WIN_FREE(WIN_IGNFAC    )
   ENDIF
   CALL MPI_WIN_FREE(WIN_MLH           )
   CALL MPI_WIN_FREE(WIN_MLW           )
   CALL MPI_WIN_FREE(WIN_MFOL          )
   CALL MPI_WIN_FREE(WIN_ASP           )
   CALL MPI_WIN_FREE(WIN_CBH           )
   CALL MPI_WIN_FREE(WIN_CBD           )
   CALL MPI_WIN_FREE(WIN_CC            )
   CALL MPI_WIN_FREE(WIN_CH            )
   CALL MPI_WIN_FREE(WIN_DEM)
   CALL MPI_WIN_FREE(WIN_FBFM          )
   CALL MPI_WIN_FREE(WIN_SLP           )
   CALL MPI_WIN_FREE(WIN_ADJ           )
   IF (MODE .NE. 2) CALL MPI_WIN_FREE(WIN_PHI0)
   CALL MPI_WIN_FREE(WIN_WAF           )
   CALL MPI_WIN_FREE(WIN_OMCOSSLPRAD   )
   CALL MPI_WIN_FREE(WIN_ISNONBURNABLE )

   IF (USE_POPULATION_DENSITY) CALL MPI_WIN_FREE(WIN_POPULATION_DENSITY)
   IF (USE_REAL_ESTATE_VALUE ) CALL MPI_WIN_FREE(WIN_REAL_ESTATE_VALUE )
   IF (USE_LAND_VALUE        ) CALL MPI_WIN_FREE(WIN_LAND_VALUE        )

   CALL MPI_WIN_FREE(WIN_STATS_X                         )
   CALL MPI_WIN_FREE(WIN_STATS_Y                         )
   CALL MPI_WIN_FREE(WIN_STATS_ASTOP                     )
   CALL MPI_WIN_FREE(WIN_STATS_TSTOP                     )   
   CALL MPI_WIN_FREE(WIN_STATS_SURFACE_FIRE_AREA         )
   CALL MPI_WIN_FREE(WIN_STATS_CROWN_FIRE_AREA           )
   CALL MPI_WIN_FREE(WIN_STATS_FIRE_VOLUME               )
   CALL MPI_WIN_FREE(WIN_STATS_AFFECTED_POPULATION       )
   CALL MPI_WIN_FREE(WIN_STATS_AFFECTED_REAL_ESTATE_VALUE)
   CALL MPI_WIN_FREE(WIN_STATS_AFFECTED_LAND_VALUE       )
   CALL MPI_WIN_FREE(WIN_STATS_FINAL_CONTAINMENT_FRAC    )
   CALL MPI_WIN_FREE(WIN_STATS_NEMBERS                   )
   CALL MPI_WIN_FREE(WIN_STATS_ICASE                     )
   CALL MPI_WIN_FREE(WIN_STATS_IWX_BAND_START            )
   CALL MPI_WIN_FREE(WIN_STATS_IWX_SERIAL_BAND           )
   CALL MPI_WIN_FREE(WIN_STATS_SIMULATION_TSTOP_HOURS    )
   CALL MPI_WIN_FREE(WIN_STATS_WALL_CLOCK_TIME           )
ENDIF

CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)

! Dump timings:
CALL SYSTEM_CLOCK(IT_STOP)
TIMINGS(IRANK_HOST+1,1) = REAL(IT_STOP - IT_START) / REAL(CLOCK_COUNT_RATE)
CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
IF (DUMP_TIMINGS .AND. IRANK_HOST .EQ. 0) CALL WRITE_TIMINGS_TO_DISK

CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
IF (NPROC .GT. 1) CALL MPI_WIN_FREE(WIN_TIMINGS)

CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)

CALL CLEANUP_ANALYSIS_SRS
CALL MPI_FINALIZE(IERR)

IF (IRANK_WORLD .EQ. 0 .AND. CLEAN_SCRATCH) CALL CLEAN_SCRATCH_DIRECTORY

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'End of simulation reached successfully. Shutting down.'

! *****************************************************************************
END SUBROUTINE SHUTDOWN
! *****************************************************************************

! *****************************************************************************
SUBROUTINE ERC_IGNITION_FACTOR (ERC, IGNFAC, IB1, IB2)
! *****************************************************************************
! Copies the ERC raster header into IGNFAC and fills its data (bands IB1..IB2)
! with a per-cell ignition probability factor derived from the energy release
! component, or, if ERC_IS_PLIGNRATE, with the ERC value floored at PLIGNRATE_MIN.

TYPE (RASTER_TYPE), INTENT(IN) :: ERC
TYPE (RASTER_TYPE) :: IGNFAC
INTEGER, INTENT(IN) :: IB1, IB2
INTEGER :: IBAND, IROW, ICOL
REAL :: ERCVAL, EXPONENT

IGNFAC%BYTEORDER     = ERC%BYTEORDER
IGNFAC%LAYOUT        = ERC%LAYOUT
IGNFAC%NROWS         = ERC%NROWS
IGNFAC%NCOLS         = ERC%NCOLS
IGNFAC%NBANDS        = ERC%NBANDS
IGNFAC%NBITS         = ERC%NBITS
IGNFAC%BANDROWBYTES  = ERC%BANDROWBYTES
IGNFAC%TOTALROWBYTES = ERC%TOTALROWBYTES
IGNFAC%PIXELTYPE     = ERC%PIXELTYPE
IGNFAC%ULXMAP        = ERC%ULXMAP
IGNFAC%ULYMAP        = ERC%ULYMAP
IGNFAC%XDIM          = ERC%XDIM
IGNFAC%YDIM          = ERC%YDIM
IGNFAC%NODATA_VALUE  = ERC%NODATA_VALUE
IGNFAC%CELLSIZE      = ERC%CELLSIZE
IGNFAC%XLLCORNER     = ERC%XLLCORNER
IGNFAC%YLLCORNER     = ERC%YLLCORNER

IF (ERC_IS_PLIGNRATE) THEN
   DO IBAND = IB1, IB2
      DO IROW = 1, ERC%NROWS
      DO ICOL = 1, ERC%NCOLS
         IGNFAC%R4(ICOL,IROW,IBAND) = MAX(PLIGNRATE_MIN, ERC%R4(ICOL,IROW,IBAND))
      ENDDO
      ENDDO
   ENDDO
ELSE
   DO IBAND = IB1, IB2
      DO IROW = 1, ERC%NROWS
      DO ICOL = 1, ERC%NCOLS
         ERCVAL = MIN( MAX(ERC%R4(ICOL,IROW,IBAND),0.),100.0)
         EXPONENT = 0.02768 * ERCVAL - 0.2333
         IGNFAC%R4(ICOL,IROW,IBAND) = MIN( MAX ( 2.92E-3 * 10**EXPONENT, 1E-3 ), 1E0)
      ENDDO
      ENDDO
   ENDDO
ENDIF

! *****************************************************************************
END SUBROUTINE ERC_IGNITION_FACTOR
! *****************************************************************************

! *****************************************************************************
REAL FUNCTION ERFINV(X)
! *****************************************************************************
! Returns an approximation of the inverse error function of X using a truncated
! polynomial (Maclaurin-type) series.

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
REAL FUNCTION ISF(FUEL, RSF, CF)
! *****************************************************************************
! Solves the Canadian FBP rate-of-spread relation inversely to return the
! initial spread index (ISF) consistent with a surface spread rate RSF for the
! given FUEL type and slope/curing factor CF.

REAL,INTENT(IN) :: RSF, CF
INTEGER*2, intent(in) :: FUEL
ISF = log(max(0.01,1-(RSF/(FUEL_MODEL_TABLE_FBP(FUEL)%a*CF))**(1/FUEL_MODEL_TABLE_FBP(FUEL)%c)))/(-FUEL_MODEL_TABLE_FBP(FUEL)%b)

! *****************************************************************************
END FUNCTION ISF
! *****************************************************************************

! *****************************************************************************
RECURSIVE REAL FUNCTION RSI(FUEL, ISI, CF) result(rsi_val)
! *****************************************************************************
! Returns the Canadian FBP initial-spread rate (RSI) for the given FUEL type,
! initial spread index ISI, and curing factor CF. Recurses to blend component
! fuel types for the mixedwood models (M1-M4).

REAL,INTENT(IN) :: ISI, CF
INTEGER*2, intent(in) :: FUEL
REAL RSI_100, PDF

PDF = mod(FUEL,100)/100.0
IF (FUEL .eq. 40 .or. FUEL .eq. 60 .or. (FUEL .ge. 400 .and. FUEL .le. 499) .or. (FUEL .ge. 600 .and. FUEL .le. 699)) then ! M1
   rsi_val = PDF * RSI(2_2, ISI, CF) + (1-PDF)*RSI(11_2, ISI, CF)
ELSE IF (FUEL .eq. 50 .or. (FUEL .ge. 500 .and. FUEL .le. 599)) then ! M2
   rsi_val = PDF * RSI(2_2, ISI, CF) + 0.2*(1-PDF)*RSI(11_2, ISI, CF)
ELSE IF (FUEL .eq. 70 .or. FUEL .eq. 90 .or. (FUEL .ge. 700 .and. FUEL .le. 799) .or. (FUEL .ge. 900 .and. FUEL .le. 999)) THEN ! M3
   RSI_100 = 120*(1-exp(-0.0572*ISI))**1.4
   rsi_val = PDF*RSI_100 + (1-PDF)*RSI(11_2, ISI, CF)
ELSE IF (FUEL .eq. 80 .or. (FUEL .ge. 800 .and. FUEL .le. 899)) THEN ! M4
   RSI_100 = 100*(1-exp(-0.0404*ISI))**1.48
   rsi_val = PDF*RSI_100 + 0.2*(1-PDF)*RSI(11_2, ISI, CF)
ELSE
   rsi_val = FUEL_MODEL_TABLE_FBP(FUEL)%a*(1-exp(-FUEL_MODEL_TABLE_FBP(FUEL)%b*ISI))**FUEL_MODEL_TABLE_FBP(FUEL)%c*CF
ENDIF


! *****************************************************************************
END FUNCTION RSI
! *****************************************************************************

! *****************************************************************************
RECURSIVE REAL FUNCTION SFC(FUEL, FFMC, BUI) result(out)
! *****************************************************************************
! Returns the Canadian FBP surface fuel consumption (kg/m^2) for the given FUEL
! type from the fine fuel moisture code FFMC and buildup index BUI; recurses to
! blend component fuels for the mixedwood models.
INTEGER*2, INTENT(IN) :: FUEL 
REAL, INTENT(IN) :: FFMC, BUI

REAL :: PC
PC = 0

select case (FUEL)
   case (1)
      if (FFMC .gt. 84) THEN
         out = 0.75 + 0.75*sqrt((1-exp(-0.23*(FFMC-84))))
      else
         out = 0.75 - 0.75*sqrt((1-exp(-0.23*(84-FFMC))))
      endif
   case (2, 70, 80, 90, 700:1000)
      out = 5.0*(1-exp(-0.0115*BUI))
   case (3, 4)
      out = 5.0*(1-exp(-0.0164*BUI))**2.24
   case (5, 6)
      out = 5.0*(1-exp(-0.0149*BUI))**2.48
   case (7)
      out = max(0.0,2*(1-exp(-0.104*(FFMC-70))))+1.5*(1-exp(-0.0201*BUI))
   case (11, 12, 13)
      out = 1.5*(1-exp(-0.0183*BUI))
   case (40, 50, 60, 400:699)
      PC = mod(FUEL, 100)/100.0
      out = PC * SFC(2_2,FFMC,BUI) + (1 - PC) * SFC(11_2,FFMC,BUI)
   case(31,32,33)
      out = 0.35
   case (21)
      out = max(0.0,4*(1-exp(-0.034*BUI)))+4.0*(1-exp(-0.025*BUI))
   case (22)
      out = max(0.0,10*(1-exp(-0.013*BUI)))+6.0*(1-exp(-0.060*BUI))
   case (23)
      out = max(0.0,12*(1-exp(-0.0166*BUI)))+20.0*(1-exp(-0.021*BUI))
   case Default
      out = 0
end select

! *****************************************************************************
END FUNCTION SFC
! *****************************************************************************

! *****************************************************************************
REAL FUNCTION BUI(day_of_weather, month_of_weather)
! *****************************************************************************
! Returns the Canadian FWI Buildup Index for the given day/month by advancing
! the Drought Code and Duff Moisture Code from temperature, humidity, and
! precipitation. Side effect: updates the global DC_prev and DMC_prev carry-over
! state for the next day's calculation.
REAL :: V, DC, Q_prev, Q_RT, DC_RT, K, DMC, b, Pe, M_prev, M_RT, DMC_RT
INTEGER, intent(in) :: day_of_weather, month_of_weather
! ------------- DROUGHT CODE -------------------------------

V = max(0.0,0.36*(max(0.0,T_midday(day_of_weather) + 2.8)) + Lf(month_of_weather))
if (precip(day_of_weather) .le. 2.8) THEN
   DC = DC_prev + 0.5 * V
ELSE
   Q_prev = 800 * exp(-DC_prev/400)
   Q_RT = max(0.0,Q_prev + 3.937*(0.83*precip(day_of_weather) -1.27))
   DC_RT = 400*log(800/Q_RT)
   DC=DC_RT + 0.5 * V
ENDIF

! -------------- DROUGHT MOISTURE CODE ---------------------

K = 1.894*(max(0.0,T_midday(day_of_weather) + 1.1))*(100 - H_midday(day_of_weather))*Le(month_of_weather)*10.0**(-6.0)
if (precip(day_of_weather) .le. 1.5) THEN
   DMC = DMC_prev + 100 * K
ELSE
   if (DMC_prev .le. 33) THEN
      b=100/(0.5+0.3*DMC_prev)
   ELSEIF (DMC_prev .le. 65) THEN
      b=14-1.3*log(DMC_prev)
   ELSE
      b=6.2*log(DMC_prev)-17.2
   ENDIF
   Pe = 0.92* precip(day_of_weather) - 1.27
   M_prev = 20 + exp(5.6348-DMC_prev/43.43)
   M_RT = M_prev + (1000*Pe)/(48.77+b*Pe)
   DMC_RT = max(0.0,244.72-43.43*log(M_RT-20))
   DMC=DMC_RT + 100 * K
ENDIF

DMC_prev = DMC
DC_prev = DC

! -------------- BUILD-UP INDEX ---------------------

BUI = 0.8*DMC*DC/(DMC+0.4*DC)
! *****************************************************************************
END FUNCTION BUI
! *****************************************************************************

! *****************************************************************************
CHARACTER(16) FUNCTION HOUR_OF_YEAR_TO_TIMESTAMP(YEAR, HOUR_OF_YEAR)
! *****************************************************************************
! Converts an hour-of-year count (with the given YEAR, accounting for leap
! years) into a 'YYYY-MM-DD HH:00' timestamp string.

INTEGER, INTENT(IN) :: YEAR
INTEGER, INTENT(IN) :: HOUR_OF_YEAR
INTEGER :: I, HOUR_OF_MONTH, DAY_OF_MONTH, HOUR_OF_DAY, MONTH
INTEGER, DIMENSION(12) :: DAYS_PER_MONTH=(/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /), &
                          MONTH_BEGIN_HOUR, MONTH_END_HOUR
LOGICAL :: FOUND
CHARACTER(16) :: TIMESTAMP

IF ( (MODULO(YEAR,4) .EQ. 0 .AND. MODULO(YEAR,100) .NE. 0) .OR. MODULO(YEAR,400) .EQ. 0 ) DAYS_PER_MONTH(2)=29

MONTH_BEGIN_HOUR(1) = 0
MONTH_END_HOUR  (1) = DAYS_PER_MONTH(1) * 24
DO I = 2, 12
   MONTH_BEGIN_HOUR(I) = MONTH_END_HOUR  (I-1)
   MONTH_END_HOUR  (I) = MONTH_BEGIN_HOUR(I)   + DAYS_PER_MONTH(I) * 24
ENDDO

FOUND = .FALSE.
MONTH = 1
DO I = 1, 12
   IF (FOUND) CYCLE
   IF (HOUR_OF_YEAR .GE. MONTH_BEGIN_HOUR(I) .AND. HOUR_OF_YEAR .LT. MONTH_END_HOUR(I) ) THEN
      FOUND = .TRUE.
      MONTH = I
   ENDIF
ENDDO

HOUR_OF_MONTH = HOUR_OF_YEAR - MONTH_BEGIN_HOUR(MONTH)
DAY_OF_MONTH  = 1 + HOUR_OF_MONTH / 24
HOUR_OF_DAY   = MODULO( HOUR_OF_MONTH,24)

WRITE( TIMESTAMP,  '(I4.4,"-",I2.2,"-",I2.2," ",I2.2,":00")') YEAR, MONTH, DAY_OF_MONTH, HOUR_OF_DAY

HOUR_OF_YEAR_TO_TIMESTAMP = TIMESTAMP

! *****************************************************************************
END FUNCTION HOUR_OF_YEAR_TO_TIMESTAMP
! *****************************************************************************

! *****************************************************************************
! Small files are read in full: WKT and diagnostic records must not be truncated.
SUBROUTINE READ_SPATIAL_TEXT(FILENAME, TEXT, IOS)
CHARACTER(*), INTENT(IN) :: FILENAME
CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: TEXT
INTEGER, INTENT(OUT) :: IOS
INTEGER :: LU, N, CLOSE_IOS
TEXT = ''
OPEN(NEWUNIT=LU, FILE=FILENAME, STATUS='OLD', ACCESS='STREAM', FORM='UNFORMATTED', IOSTAT=IOS)
IF (IOS /= 0) RETURN
INQUIRE(UNIT=LU, SIZE=N, IOSTAT=IOS)
IF (IOS == 0) THEN
   IF (N > 0) THEN
      TEXT = REPEAT(' ', N)
      READ(LU, IOSTAT=IOS) TEXT
   ENDIF
ENDIF
CLOSE(LU, IOSTAT=CLOSE_IOS)
IF (IOS == 0) IOS = CLOSE_IOS
END SUBROUTINE READ_SPATIAL_TEXT

FUNCTION SPATIAL_TEMP(NAME) RESULT(FILENAME)
CHARACTER(*), INTENT(IN) :: NAME
CHARACTER(:), ALLOCATABLE :: FILENAME, DIRECTORY
CHARACTER(32) :: RANKSTR
DIRECTORY = TRIM(SCRATCH)
IF (DIRECTORY == 'null' .OR. LEN(DIRECTORY) == 0) DIRECTORY = '.'
IF (DIRECTORY(LEN(DIRECTORY):) /= PATH_SEPARATOR .AND. DIRECTORY(LEN(DIRECTORY):) /= '/') &
   DIRECTORY = DIRECTORY // PATH_SEPARATOR
WRITE(RANKSTR,'(I0)') IRANK_WORLD
FILENAME = DIRECTORY // 'elmfire_' // NAME // '_' // TRIM(RANKSTR)
END FUNCTION SPATIAL_TEMP

FUNCTION SHELL_ARGUMENT(ARG) RESULT(QUOTED)
CHARACTER(*), INTENT(IN) :: ARG
CHARACTER(:), ALLOCATABLE :: QUOTED
INTEGER :: I
IF (OPERATING_SYSTEM == 'windows') THEN
   IF (INDEX(ARG, '"') /= 0) CALL SPATIAL_ERROR('Double quote in shell argument: ' // ARG)
   QUOTED = '"' // ARG // '"'
ELSE
   QUOTED = "'"
   DO I = 1, LEN(ARG)
      IF (ARG(I:I) == "'") THEN
         QUOTED = QUOTED // "'" // ACHAR(34) // "'" // ACHAR(34) // "'"
      ELSE
         QUOTED = QUOTED // ARG(I:I)
      ENDIF
   ENDDO
   QUOTED = QUOTED // "'"
ENDIF
END FUNCTION SHELL_ARGUMENT

SUBROUTINE SPATIAL_ERROR(MESSAGE, ERROR_FILE)
CHARACTER(*), INTENT(IN) :: MESSAGE
CHARACTER(*), OPTIONAL, INTENT(IN) :: ERROR_FILE
CHARACTER(:), ALLOCATABLE :: DIAGNOSTIC
INTEGER :: IOS, IERR
WRITE(*,*) 'Spatial initialization/conversion error on rank ', IRANK_WORLD, ': ', MESSAGE
IF (PRESENT(ERROR_FILE)) THEN
   CALL READ_SPATIAL_TEXT(ERROR_FILE, DIAGNOSTIC, IOS)
   IF (LEN_TRIM(DIAGNOSTIC) > 0) WRITE(*,'(A)') DIAGNOSTIC
ENDIF
! Safe before shared-memory allocation; all peers must leave pending collectives.
CALL MPI_ABORT(MPI_COMM_WORLD, 1, IERR)
ERROR STOP 1
END SUBROUTINE SPATIAL_ERROR

SUBROUTINE REQUIRE_ANALYSIS_SRS
LOGICAL :: EXISTS
IF (.NOT. ALLOCATED(ANALYSIS_SRS_FILE)) CALL SPATIAL_ERROR('Analysis CRS has not been resolved')
IF (LEN_TRIM(ANALYSIS_SRS_FILE) == 0) CALL SPATIAL_ERROR('Analysis CRS file is blank')
INQUIRE(FILE=ANALYSIS_SRS_FILE, EXIST=EXISTS)
IF (.NOT. EXISTS) CALL SPATIAL_ERROR('Analysis CRS file is missing: ' // ANALYSIS_SRS_FILE)
END SUBROUTINE REQUIRE_ANALYSIS_SRS

SUBROUTINE CLEANUP_ANALYSIS_SRS
IF (.NOT. ALLOCATED(ANALYSIS_SRS_FILE)) RETURN
CALL DELETE_FILE(ANALYSIS_SRS_FILE)
DEALLOCATE(ANALYSIS_SRS_FILE)
END SUBROUTINE CLEANUP_ANALYSIS_SRS

SUBROUTINE CHECKED_GDAL_COMMAND(COMMAND, OPERATION, SOURCE, ERROR_FILE)
CHARACTER(*), INTENT(IN) :: COMMAND, OPERATION, SOURCE, ERROR_FILE
INTEGER :: STATUS, IOS
CHARACTER(:), ALLOCATABLE :: DIAGNOSTIC
CALL RUN_SHELL_COMMAND(COMMAND // ' 2> ' // SHELL_ARGUMENT(ERROR_FILE), EXITSTAT=STATUS)
CALL READ_SPATIAL_TEXT(ERROR_FILE, DIAGNOSTIC, IOS)
! Some GDAL utilities return zero after PROJ errors; never accept a degraded CRS.
IF (STATUS /= 0 .OR. IOS /= 0 .OR. INDEX(DIAGNOSTIC, 'ERROR') > 0) &
   CALL SPATIAL_ERROR(OPERATION // ' failed for ' // SOURCE, ERROR_FILE)
IF (LEN_TRIM(DIAGNOSTIC) > 0) WRITE(*,'(A)') TRIM(DIAGNOSTIC)
CALL DELETE_FILE(ERROR_FILE)
END SUBROUTINE CHECKED_GDAL_COMMAND

LOGICAL FUNCTION NAMELIST_GROUP_PRESENT(GROUP)
CHARACTER(*), INTENT(IN) :: GROUP
CHARACTER(:), ALLOCATABLE :: TEXT, TOKEN
CHARACTER :: QUOTE, CH
INTEGER :: I, J, IOS, CODE
CALL READ_SPATIAL_TEXT(TRIM(NAMELIST_FN), TEXT, IOS)
IF (IOS /= 0) CALL SPATIAL_ERROR('Cannot inspect namelist: ' // TRIM(NAMELIST_FN))
NAMELIST_GROUP_PRESENT = .FALSE.
QUOTE = ' '
I = 1
DO WHILE (I <= LEN(TEXT))
   CH = TEXT(I:I)
   IF (QUOTE /= ' ') THEN
      IF (CH == QUOTE) THEN
         IF (I < LEN(TEXT)) THEN
            IF (TEXT(I+1:I+1) == QUOTE) THEN
               I = I + 2
               CYCLE
            ENDIF
         ENDIF
         QUOTE = ' '
      ENDIF
   ELSE IF (CH == '"' .OR. CH == "'") THEN
      QUOTE = CH
   ELSE IF (CH == '!') THEN
      DO WHILE (I < LEN(TEXT))
         IF (TEXT(I:I) == ACHAR(10)) EXIT
         I = I + 1
      ENDDO
   ELSE IF (CH == '&' .OR. CH == '$') THEN
      J = I + 1
      DO WHILE (J <= LEN(TEXT))
         CH = TEXT(J:J)
         IF (INDEX('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789', CH) == 0) EXIT
         J = J + 1
      ENDDO
      TOKEN = TEXT(I+1:J-1)
      DO IOS = 1, LEN(TOKEN)
         CODE = IACHAR(TOKEN(IOS:IOS))
         IF (CODE >= 97 .AND. CODE <= 122) TOKEN(IOS:IOS) = ACHAR(CODE-32)
      ENDDO
      IF (TOKEN == GROUP) THEN
         NAMELIST_GROUP_PRESENT = .TRUE.
         RETURN
      ENDIF
   ENDIF
   I = I + 1
ENDDO
END FUNCTION NAMELIST_GROUP_PRESENT

SUBROUTINE XY_TO_LATLON(X, Y, LAT, LON)
! Success returns finite longitude/latitude; every failure aborts the MPI job.
REAL, INTENT(IN) :: X, Y
REAL, INTENT(OUT) :: LAT, LON
CHARACTER(:), ALLOCATABLE :: TMPIN, TMPOUT, TMPERR, CMD, DIAGNOSTIC
INTEGER :: LU, IOS, CLOSE_IOS
CALL REQUIRE_ANALYSIS_SRS
! List-directed null fields must not leave either INTENT(OUT) value undefined.
LAT = IEEE_VALUE(0.0, IEEE_QUIET_NAN)
LON = IEEE_VALUE(0.0, IEEE_QUIET_NAN)
TMPIN = SPATIAL_TEMP('xy_in') // '.txt'
TMPOUT = SPATIAL_TEMP('xy_out') // '.txt'
TMPERR = SPATIAL_TEMP('xy_error') // '.txt'
IF (.NOT. IEEE_IS_FINITE(X) .OR. .NOT. IEEE_IS_FINITE(Y)) CALL FAIL('Nonfinite input coordinates')
OPEN(NEWUNIT=LU, FILE=TMPIN, STATUS='REPLACE', ACTION='WRITE', IOSTAT=IOS)
IF (IOS /= 0) CALL FAIL('Cannot open transform input: ' // TMPIN)
WRITE(LU,'(ES24.16,1X,ES24.16)',IOSTAT=IOS) X, Y
CLOSE(LU,IOSTAT=CLOSE_IOS)
IF (IOS /= 0 .OR. CLOSE_IOS /= 0) CALL FAIL('Cannot write/close transform input: ' // TMPIN)
CMD = SHELL_ARGUMENT(TRIM(PATH_TO_GDAL) // 'gdaltransform') // ' -s_srs ' // &
      SHELL_ARGUMENT(ANALYSIS_SRS_FILE) // ' -t_srs EPSG:4326 < ' // SHELL_ARGUMENT(TMPIN) // &
      ' > ' // SHELL_ARGUMENT(TMPOUT) // ' 2> ' // SHELL_ARGUMENT(TMPERR)
CALL RUN_SHELL_COMMAND(CMD, EXITSTAT=IOS)
IF (IOS /= 0) CALL FAIL('gdaltransform command failed')
CALL READ_SPATIAL_TEXT(TMPERR, DIAGNOSTIC, IOS)
IF (IOS /= 0 .OR. INDEX(DIAGNOSTIC, 'ERROR') > 0) CALL FAIL('gdaltransform diagnostic')
OPEN(NEWUNIT=LU, FILE=TMPOUT, STATUS='OLD', ACTION='READ', IOSTAT=IOS)
IF (IOS /= 0) CALL FAIL('Cannot open transform output: ' // TMPOUT)
! GDAL emits longitude, latitude, and an unused real height.
READ(LU,*,IOSTAT=IOS) LON, LAT
CLOSE(LU,IOSTAT=CLOSE_IOS)
IF (IOS /= 0 .OR. CLOSE_IOS /= 0) CALL FAIL('Cannot read/close transform output: ' // TMPOUT)
IF (.NOT. IEEE_IS_FINITE(LON) .OR. .NOT. IEEE_IS_FINITE(LAT)) CALL FAIL('Nonfinite transform output')
IF (ABS(LON) > 180.0 .OR. ABS(LAT) > 90.0) CALL FAIL('Transform output outside longitude/latitude range')
CALL DELETE_FILE(TMPIN)
CALL DELETE_FILE(TMPOUT)
CALL DELETE_FILE(TMPERR)
CONTAINS
SUBROUTINE FAIL(REASON)
CHARACTER(*), INTENT(IN) :: REASON
INTEGER :: STAT
CHARACTER(:), ALLOCATABLE :: DETAIL
WRITE(*,*) 'gdaltransform X/Y=', X, Y, ' source CRS=', ANALYSIS_SRS_FILE, ' rank=', IRANK_WORLD
CALL READ_SPATIAL_TEXT(TMPERR, DETAIL, STAT)
IF (LEN_TRIM(DETAIL) > 0) WRITE(*,'(A)') DETAIL
CALL DELETE_FILE(TMPIN)
CALL DELETE_FILE(TMPOUT)
CALL DELETE_FILE(TMPERR)
CALL SPATIAL_ERROR(REASON)
END SUBROUTINE FAIL
END SUBROUTINE XY_TO_LATLON

SUBROUTINE RESOLVE_ANALYSIS_SRS(SOURCE)
CHARACTER(*), INTENT(IN) :: SOURCE
CHARACTER(:), ALLOCATABLE :: DEFINITION, DEF_FILE, ERR_FILE, VALID_FILE, CMD, WKT, VALIDATION, REASON
INTEGER :: LU, IOS, CLOSE_IOS
DEFINITION = TRIM(A_SRS)
IF (LEN(DEFINITION) == 0) THEN
   DEFINITION = SOURCE
   IF (IRANK_WORLD == 0) WRITE(*,*) 'Analysis CRS: automatic from ', SOURCE
ELSE
   IF (IRANK_WORLD == 0) WRITE(*,*) 'Analysis CRS: explicit A_SRS: ', DEFINITION
ENDIF
DEF_FILE = SPATIAL_TEMP('srs_definition') // '.txt'
ERR_FILE = SPATIAL_TEMP('srs_error') // '.txt'
VALID_FILE = SPATIAL_TEMP('srs_validation') // '.txt'
ANALYSIS_SRS_FILE = SPATIAL_TEMP('analysis_srs') // '.wkt'
! Raw WKT contains shell quotes. Pass it to GDAL through a file without alteration.
IF (INDEX(DEFINITION, '"') > 0) THEN
   OPEN(NEWUNIT=LU, FILE=DEF_FILE, STATUS='REPLACE', ACTION='WRITE', IOSTAT=IOS)
   IF (IOS /= 0) CALL SPATIAL_ERROR('Cannot create CRS definition file: ' // DEF_FILE)
   WRITE(LU,'(A)',IOSTAT=IOS) DEFINITION
   CLOSE(LU,IOSTAT=CLOSE_IOS)
   IF (IOS /= 0 .OR. CLOSE_IOS /= 0) CALL SPATIAL_ERROR('Cannot write CRS definition file: ' // DEF_FILE)
   DEFINITION = DEF_FILE
ENDIF
CMD = SHELL_ARGUMENT(TRIM(PATH_TO_GDAL) // 'gdalsrsinfo') // ' -o wkt2 ' // SHELL_ARGUMENT(DEFINITION) // &
      ' > ' // SHELL_ARGUMENT(ANALYSIS_SRS_FILE)
CALL CHECKED_GDAL_COMMAND(CMD, 'gdalsrsinfo WKT export', DEFINITION, ERR_FILE)
CALL READ_SPATIAL_TEXT(ANALYSIS_SRS_FILE, WKT, IOS)
IF (IOS /= 0 .OR. LEN_TRIM(WKT) == 0) CALL SPATIAL_ERROR('Empty/unreadable WKT for ' // DEFINITION)
CMD = SHELL_ARGUMENT(TRIM(PATH_TO_GDAL) // 'gdalsrsinfo') // ' -V -o wkt2 ' // &
      SHELL_ARGUMENT(ANALYSIS_SRS_FILE) // ' > ' // SHELL_ARGUMENT(VALID_FILE)
CALL CHECKED_GDAL_COMMAND(CMD, 'gdalsrsinfo validation', DEFINITION, ERR_FILE)
CALL READ_SPATIAL_TEXT(VALID_FILE, VALIDATION, IOS)
IF (IOS /= 0 .OR. INDEX(VALIDATION, 'Validate Succeeds') == 0) &
   CALL SPATIAL_ERROR('Invalid WKT for ' // DEFINITION, VALID_FILE)
CALL VALIDATE_PROJECTED_METRES(WKT, REASON)
IF (LEN(REASON) > 0) CALL SPATIAL_ERROR(REASON // ': ' // DEFINITION)
CALL DELETE_FILE(DEF_FILE)
CALL DELETE_FILE(VALID_FILE)
END SUBROUTINE RESOLVE_ANALYSIS_SRS

SUBROUTINE VALIDATE_PROJECTED_METRES(WKT, REASON)
! Traverse WKT2 brackets outside quoted strings. Only the selected PROJCRS's
! Cartesian axes count, never ellipsoid, base-CRS or projection-parameter units.
CHARACTER(*), INTENT(IN) :: WKT
CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: REASON
INTEGER, ALLOCATABLE :: OPEN_AT(:), CLOSE_AT(:), PARENT(:), STACK(:)
INTEGER :: I, J, N, DEPTH, ROOT, PROJECTED, AXES, CSCOUNT, UNITCOUNT, K, IOS, DIMENSION
REAL(8) :: FACTOR
LOGICAL :: QUOTED
CHARACTER(:), ALLOCATABLE :: TOKEN, CONTENT
ALLOCATE(OPEN_AT(LEN(WKT)), CLOSE_AT(LEN(WKT)), PARENT(LEN(WKT)), STACK(LEN(WKT)))
REASON = 'Malformed WKT brackets or quoted string'
N = 0
DEPTH = 0
QUOTED = .FALSE.
I = 1
DO WHILE (I <= LEN(WKT))
   IF (WKT(I:I) == '"') THEN
      IF (QUOTED .AND. I < LEN(WKT)) THEN
         IF (WKT(I+1:I+1) == '"') THEN
            I = I + 2
            CYCLE
         ENDIF
      ENDIF
      QUOTED = .NOT. QUOTED
   ELSE IF (.NOT. QUOTED) THEN
      IF (WKT(I:I) == '[') THEN
         N = N + 1
         OPEN_AT(N) = I
         PARENT(N) = 0
         IF (DEPTH > 0) PARENT(N) = STACK(DEPTH)
         DEPTH = DEPTH + 1
         STACK(DEPTH) = N
      ELSE IF (WKT(I:I) == ']') THEN
         IF (DEPTH == 0) RETURN
         CLOSE_AT(STACK(DEPTH)) = I
         DEPTH = DEPTH - 1
      ENDIF
   ENDIF
   I = I + 1
ENDDO
IF (DEPTH /= 0 .OR. QUOTED .OR. N == 0) RETURN
REASON = 'Unsupported CRS: a horizontal projected Cartesian grid is required'
ROOT = 1
IF (NODE_TYPE(ROOT) == 'BOUNDCRS') THEN
   DO I = 2, N
      IF (PARENT(I) /= ROOT .OR. NODE_TYPE(I) /= 'SOURCECRS') CYCLE
      ROOT = I
      EXIT
   ENDDO
   DO I = ROOT+1, N
      IF (PARENT(I) /= ROOT) CYCLE
      ROOT = I
      EXIT
   ENDDO
ENDIF
IF (NODE_TYPE(ROOT) /= 'PROJCRS') RETURN
PROJECTED = ROOT
AXES = 0
CSCOUNT = 0
DO I = PROJECTED+1, N
   IF (PARENT(I) /= PROJECTED) CYCLE
   TOKEN = NODE_TYPE(I)
   IF (TOKEN == 'CS') THEN
      CONTENT = WKT(OPEN_AT(I)+1:CLOSE_AT(I)-1)
      J = INDEX(CONTENT, ',')
      IF (J == 0) RETURN
      IF (TRIM(ADJUSTL(CONTENT(:J-1))) /= 'Cartesian') RETURN
      READ(CONTENT(J+1:),*,IOSTAT=IOS) DIMENSION
      IF (IOS /= 0) RETURN
      IF (DIMENSION /= 2) RETURN
      CSCOUNT = CSCOUNT + 1
   ELSE IF (TOKEN == 'AXIS') THEN
      AXES = AXES + 1
      UNITCOUNT = 0
      DO K = I+1, N
         IF (PARENT(K) /= I .OR. NODE_TYPE(K) /= 'LENGTHUNIT') CYCLE
         UNITCOUNT = UNITCOUNT + 1
         ! Skip the quoted name, including escaped quotes/commas.
         QUOTED = .FALSE.
         DO J = OPEN_AT(K)+1, CLOSE_AT(K)-1
            IF (WKT(J:J) == '"') QUOTED = .NOT. QUOTED
            IF (WKT(J:J) == ',' .AND. .NOT. QUOTED) EXIT
         ENDDO
         CONTENT = WKT(J+1:CLOSE_AT(K)-1)
         READ(CONTENT,*,IOSTAT=IOS) FACTOR
         REASON = 'Projected axis units must be metres (LENGTHUNIT conversion factor 1)'
         IF (IOS /= 0) RETURN
         IF (.NOT. IEEE_IS_FINITE(FACTOR)) RETURN
         IF (ABS(FACTOR-1D0) > 1D-12) RETURN
      ENDDO
      IF (UNITCOUNT /= 1) THEN
         REASON = 'Projected axis is missing an unambiguous LENGTHUNIT'
         RETURN
      ENDIF
   ENDIF
ENDDO
REASON = 'Projected CRS must have exactly two Cartesian axes'
IF (AXES == 2 .AND. CSCOUNT == 1) REASON = ''
CONTAINS
FUNCTION NODE_TYPE(NODE) RESULT(NAME)
INTEGER, INTENT(IN) :: NODE
CHARACTER(:), ALLOCATABLE :: NAME
INTEGER :: FIRST, LAST
LAST = OPEN_AT(NODE)-1
DO WHILE (LAST > 0)
   IF (IACHAR(WKT(LAST:LAST)) > 32) EXIT
   LAST = LAST-1
ENDDO
FIRST = LAST
DO WHILE (FIRST > 0)
   IF (INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789', WKT(FIRST:FIRST)) == 0) EXIT
   FIRST = FIRST-1
ENDDO
NAME = WKT(FIRST+1:LAST)
END FUNCTION NODE_TYPE
END SUBROUTINE VALIDATE_PROJECTED_METRES

SUBROUTINE READ_GEOTIFF_META_GDALINFO
CHARACTER(:), ALLOCATABLE :: SOURCE, BASE, TMPFILE, ERRFILE, CMD, TEXT, LINE
INTEGER :: IOS, NCOLS, NROWS, FIRST, LAST, P, Q
REAL(8) :: X0, Y0, DX, DY
LOGICAL :: GOT_SIZE, GOT_ORIGIN, GOT_PIXEL
BASE = TRIM(ASP_FILENAME)
IF (USE_LANDSCAPE_FILE) BASE = TRIM(LANDSCAPE_FILENAME)
SOURCE = TRIM(FUELS_AND_TOPOGRAPHY_DIRECTORY) // BASE
IF (USE_TILED_IO) THEN
   SOURCE = SOURCE // '_1_1.bsq'
ELSE IF (USE_EXISTING_BSQS) THEN
   IF (TRIM(SCRATCH) /= 'null') SOURCE = TRIM(SCRATCH) // BASE
   SOURCE = SOURCE // '.bsq'
ELSE IF (VRT_INSTEAD_OF_TIF) THEN
   SOURCE = SOURCE // '.vrt'
ELSE
   SOURCE = SOURCE // '.tif'
ENDIF
IF (IRANK_WORLD == 0) WRITE(*,*) 'Analysis raster: ', SOURCE
TMPFILE = SPATIAL_TEMP('gdalinfo') // '.txt'
ERRFILE = SPATIAL_TEMP('gdalinfo_error') // '.txt'
CMD = SHELL_ARGUMENT(TRIM(PATH_TO_GDAL) // 'gdalinfo') // ' ' // SHELL_ARGUMENT(SOURCE) // &
      ' > ' // SHELL_ARGUMENT(TMPFILE)
CALL CHECKED_GDAL_COMMAND(CMD, 'gdalinfo', SOURCE, ERRFILE)
CALL READ_SPATIAL_TEXT(TMPFILE, TEXT, IOS)
IF (IOS /= 0) CALL SPATIAL_ERROR('Cannot read gdalinfo output for ' // SOURCE)
GOT_SIZE = .FALSE.
GOT_ORIGIN = .FALSE.
GOT_PIXEL = .FALSE.
NCOLS = -1
NROWS = -1
X0 = IEEE_VALUE(0D0, IEEE_QUIET_NAN)
Y0 = X0
DX = X0
DY = X0
FIRST = 1
DO WHILE (FIRST <= LEN(TEXT))
   LAST = INDEX(TEXT(FIRST:), ACHAR(10))
   IF (LAST == 0) LAST = LEN(TEXT)-FIRST+2
   LINE = TRIM(ADJUSTL(TEXT(FIRST:FIRST+LAST-2)))
   ! Stream reads retain CR on Windows; internal list-directed reads do not
   ! treat that byte as the formatted file record terminator.
   IF (LEN(LINE) > 0) THEN
      IF (LINE(LEN(LINE):) == ACHAR(13)) LINE = LINE(:LEN(LINE)-1)
   ENDIF
   FIRST = FIRST + LAST
   IF (INDEX(LINE, 'Size is ') == 1) THEN
      READ(LINE(9:),*,IOSTAT=IOS) NCOLS, NROWS
      GOT_SIZE = IOS == 0
   ELSE IF (INDEX(LINE, 'Origin = (') == 1) THEN
      P = INDEX(LINE, '(')
      Q = INDEX(LINE, ')')
      IF (Q <= P) CYCLE
      READ(LINE(P+1:Q-1),*,IOSTAT=IOS) X0, Y0
      GOT_ORIGIN = IOS == 0
   ELSE IF (INDEX(LINE, 'Pixel Size = (') == 1) THEN
      P = INDEX(LINE, '(')
      Q = INDEX(LINE, ')')
      IF (Q <= P) CYCLE
      READ(LINE(P+1:Q-1),*,IOSTAT=IOS) DX, DY
      GOT_PIXEL = IOS == 0
   ENDIF
ENDDO
IF (.NOT. GOT_SIZE .OR. .NOT. GOT_ORIGIN .OR. .NOT. GOT_PIXEL) &
   CALL SPATIAL_ERROR('Missing Size/Origin/Pixel Size (north-up grid required): ' // SOURCE)
IF (.NOT. ALL(IEEE_IS_FINITE([X0,Y0,DX,DY]))) CALL SPATIAL_ERROR('Nonfinite raster geometry: ' // SOURCE)
IF (NCOLS <= 0 .OR. NROWS <= 0 .OR. DX <= 0D0 .OR. DY >= 0D0) &
   CALL SPATIAL_ERROR('Invalid north-up raster geometry: ' // SOURCE)
IF (ABS(DX+DY) > 1D-8*DX) CALL SPATIAL_ERROR('Square raster cells required: ' // SOURCE)
ANALYSIS_CELLSIZE = REAL(DX)
ANALYSIS_XLLCORNER = REAL(X0)
! For tiled data the source is the southwestern tile, so its lower-left is the mosaic lower-left.
ANALYSIS_YLLCORNER = REAL(Y0 + DY*DBLE(NROWS))
CALL DELETE_FILE(TMPFILE)
CALL RESOLVE_ANALYSIS_SRS(SOURCE)
END SUBROUTINE READ_GEOTIFF_META_GDALINFO

END MODULE ELMFIRE_SUBS