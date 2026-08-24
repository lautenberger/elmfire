! *****************************************************************************
MODULE ELMFIRE_SUPPRESSION
! *****************************************************************************

USE ELMFIRE_VARS
USE SORT

IMPLICIT NONE

CONTAINS

! *****************************************************************************
SUBROUTINE CENTROID(IT)
! *****************************************************************************
! Computes the centroid (IXCEN, IYCEN cell indices) of the active fire perimeter
! for suppression resource IT by averaging the positions of tagged cells that are
! not yet burned or suppressed. Result is stored in SUPP(IT).

INTEGER, INTENT(IN) :: IT
INTEGER :: I, IXCEN, IYCEN, COUNT
TYPE(NODE), POINTER :: C

IXCEN=0
IYCEN=0
COUNT=0
C => LIST_TAGGED%HEAD

DO I = 1, LIST_TAGGED%NUM_NODES
   IF (C%BURNED) THEN
      C => C%NEXT
      CYCLE
   ENDIF
   IF (C%TIME_SUPPRESSED .GT. 0.) THEN
      C => C%NEXT
      CYCLE
   ENDIF

   COUNT = COUNT + 1
   IXCEN = IXCEN + C%IX
   IYCEN = IYCEN + C%IY
   C => C%NEXT
ENDDO

IF (COUNT .EQ. 0) COUNT = 1
IXCEN=NINT(REAL(IXCEN)/REAL(COUNT))
IYCEN=NINT(REAL(IYCEN)/REAL(COUNT))

SUPP(IT)%IXCEN = IXCEN
SUPP(IT)%IYCEN = IYCEN

! *****************************************************************************
END SUBROUTINE CENTROID
! *****************************************************************************

! *****************************************************************************
SUBROUTINE CONTAINMENT(IT,T)
! *****************************************************************************
! Bins tagged/suppressed fireline cells into 360 angular sectors about the centroid
! and computes current containment from per-bin suppressed and fireline fractions.
! If below the target containment, selects the lowest-spread-velocity sectors and
! marks their tagged cells as suppressed at time T (sets TIME_SUPPRESSED).

INTEGER, INTENT(IN) :: IT
REAL(8), INTENT(IN) :: T
INTEGER :: I, IDEG, J, COUNT, N_NONZERO
REAL :: DX, DY, CURRENT_CONTAINMENT, VELOCITY_SUM
REAL, DIMENSION (0:359) :: SUPPRESSED_FRACTION, FIRELINE_FRACTION, DEG, &
                           VELOCITY_SMOOTHED, VELOCITY0_SMOOTHED
LOGICAL, DIMENSION (0:359) :: SELECTED
INTEGER, PARAMETER :: DEG_SMOOTH_WIDTH=22
TYPE (DLL), POINTER :: L
TYPE(NODE), POINTER :: C

! Start by zeroing arrays
SUPP(IT)%NCELLS(:)=0
SUPP(IT)%VELOCITY(:)=0.
SUPP(IT)%SUPPRESSED_FRACTION(:)=0.

! Now loop over tagged / suppressed cells and determine which degree bin they fall in 
COUNT=0

DO J = 1, 2
   IF (J .EQ. 1) L => LIST_TAGGED
   IF (J .EQ. 2) L => LIST_SUPPRESSED
   C => L%HEAD

   DO I = 1, L%NUM_NODES
      DX = REAL(C%IX - SUPP(IT)%IXCEN) 
      DY = REAL(C%IY - SUPP(IT)%IYCEN)
!      IDEG = NINT(ATAN2D(DY,DX) - 90.0)
      IDEG = NINT(ATAN2(DY,DX)/PIO180 - 90.0)   ! PIO180 = PI/180 (full-precision), was 3.14159
      IF (IDEG .LT.   0) IDEG = IDEG + 360
      IF (IDEG .EQ. 360) IDEG = 0
      C%SUPPRESSION_IDEG = IDEG

      COUNT = COUNT + 1
      SUPP(IT)%NCELLS(IDEG) = SUPP(IT)%NCELLS(IDEG) + 1

      IF (C%TIME_SUPPRESSED .GT. 0.) THEN
         SUPP(IT)%SUPPRESSED_FRACTION(IDEG) = SUPP(IT)%SUPPRESSED_FRACTION(IDEG) + 1.
      ELSE
         SUPP(IT)%VELOCITY(IDEG) = SUPP(IT)%VELOCITY(IDEG) + C%VELOCITY
      ENDIF
      C => C%NEXT
   ENDDO
ENDDO

CONTINUE

! Determine current containment
CURRENT_CONTAINMENT = 0.
DO IDEG = 0, 359
   IF (SUPP(IT)%NCELLS(IDEG) .EQ. 0) THEN
      SUPP(IT)%VELOCITY(IDEG) = 999999.
      SUPP(IT)%SUPPRESSED_FRACTION(IDEG) = 999999.
   ELSE
      SUPP(IT)%VELOCITY(IDEG) = SUPP(IT)%VELOCITY(IDEG) / REAL(SUPP(IT)%NCELLS(IDEG))
      SUPP(IT)%SUPPRESSED_FRACTION(IDEG) = SUPP(IT)%SUPPRESSED_FRACTION(IDEG) / REAL(SUPP(IT)%NCELLS(IDEG))
   ENDIF
   SUPP(IT)%FIRELINE_FRACTION(IDEG) = REAL(SUPP(IT)%NCELLS(IDEG)) / REAL(COUNT)
   CURRENT_CONTAINMENT = CURRENT_CONTAINMENT + SUPP(IT)%SUPPRESSED_FRACTION(IDEG) * SUPP(IT)%FIRELINE_FRACTION(IDEG) 
ENDDO

! Smooth out velocity
DO IDEG = 0, 359
   N_NONZERO=0
   VELOCITY_SUM=0.
   DO I = -DEG_SMOOTH_WIDTH, DEG_SMOOTH_WIDTH
      J = IDEG + I
      IF (J .LT. 0  ) J = J + 360
      IF (J .GT. 359) J = J - 360
      IF (SUPP(IT)%NCELLS(J) .GT. 0) THEN
         N_NONZERO = N_NONZERO + 1
         VELOCITY_SUM = VELOCITY_SUM + SUPP(IT)%VELOCITY(J)
      ENDIF
   ENDDO
   IF (N_NONZERO .GT. 0) THEN
      VELOCITY_SUM = VELOCITY_SUM / REAL(N_NONZERO)
   ELSE
      VELOCITY_SUM = 999999.
   ENDIF
   SUPP(IT)%VELOCITY_SMOOTHED(IDEG) = VELOCITY_SUM
ENDDO

IF (SUPP(IT)%TARGET_CONTAINMENT .GT. CURRENT_CONTAINMENT) THEN
   DO IDEG = 0, 359
      DEG(IDEG) = REAL(IDEG)
   ENDDO
   SUPPRESSED_FRACTION(:) = SUPP(IT)%SUPPRESSED_FRACTION(:)
   FIRELINE_FRACTION  (:) = SUPP(IT)%FIRELINE_FRACTION(:)
   VELOCITY_SMOOTHED  (:) = SUPP(IT)%VELOCITY_SMOOTHED(:)
   VELOCITY0_SMOOTHED (:) = SUPP(IT)%VELOCITY_SMOOTHED(:)
   CALL DSORT(VELOCITY_SMOOTHED(0:), SUPPRESSED_FRACTION(0:), 360, 2); VELOCITY_SMOOTHED(:) = VELOCITY0_SMOOTHED(:)
   CALL DSORT(VELOCITY_SMOOTHED(0:), FIRELINE_FRACTION  (0:), 360, 2); VELOCITY_SMOOTHED(:) = VELOCITY0_SMOOTHED(:)
   CALL DSORT(VELOCITY_SMOOTHED(0:), DEG                (0:), 360, 2)

   ! Select degree bins (in increasing-velocity order) until the containment target is met.
   ! Flag the chosen bins here, then suppress their tagged cells in a single pass afterwards.
   ! This avoids re-walking the entire tagged list once per selected bin (was O(bins x nodes)).
   SELECTED(:) = .FALSE.
   I=-1
   DO WHILE (CURRENT_CONTAINMENT .LT. SUPP(IT)%TARGET_CONTAINMENT .AND. I .LT. 359)
      I = I + 1
      IDEG = INT(DEG(I))
      IF (SUPP(IT)%NCELLS(IDEG) .EQ. 0) CYCLE

      SELECTED(IDEG) = .TRUE.

      CURRENT_CONTAINMENT = CURRENT_CONTAINMENT + (1. - SUPP(IT)%SUPPRESSED_FRACTION(IDEG)) * SUPP(IT)%FIRELINE_FRACTION(IDEG)
      SUPP(IT)%SUPPRESSED_FRACTION(IDEG) = 1.0

   ENDDO

   ! Single pass over the tagged list: suppress every not-yet-suppressed cell whose bin was selected.
   C => LIST_TAGGED%HEAD
   DO J = 1, LIST_TAGGED%NUM_NODES
      IF (SELECTED(C%SUPPRESSION_IDEG) .AND. C%TIME_SUPPRESSED .LT. 0.) THEN
         C%TIME_SUPPRESSED = T
         C%SUPPRESSION_ADJUSTMENT_FACTOR = 0.0
      ENDIF
      C => C%NEXT
   ENDDO
ENDIF

CONTINUE

! *****************************************************************************
END SUBROUTINE CONTAINMENT
! *****************************************************************************





! FUNCTION AND SUBROUTINE FOR NEW SUPPRESSION MODEL
! *****************************************************************************
LOGICAL FUNCTION IS_CLOSE(a, b, tol1, tol2, tol3, tol4)
! *****************************************************************************
    TYPE(Node), INTENT(IN) :: a, b
    REAL, INTENT(IN) :: tol1, tol2, tol3, tol4

    IS_CLOSE = ABS(a%VELOCITY - b%VELOCITY) <= tol1 .AND. &
               ABS(a%FLAME_LENGTH - b%FLAME_LENGTH) <= tol2 .AND. &
               ABS(a%SDI - b%SDI) <= tol3 .AND. &
               ABS(a%PCL - b%PCL) <= tol4 .AND. &
               (a%TYPE_GROUP .EQ. b%TYPE_GROUP)
! *****************************************************************************  
END FUNCTION IS_CLOSE
! *****************************************************************************

! *****************************************************************************
SUBROUTINE ABSORB_SMALL_SEGMENTS(NODES, GRID, N, NX, NY, IX_MIN, IY_MIN, &
                                 NUM_SEGMENTS, SMALL_LIMIT)
! *****************************************************************************

TYPE(NODE_PTR), INTENT(INOUT) :: NODES(:)
INTEGER, INTENT(IN) :: GRID(:,:)
INTEGER, INTENT(IN) :: N, NX, NY
INTEGER, INTENT(IN) :: IX_MIN, IY_MIN
INTEGER, INTENT(IN) :: NUM_SEGMENTS
INTEGER, INTENT(IN) :: SMALL_LIMIT

INTEGER, ALLOCATABLE :: SEG_SIZE(:)
INTEGER, ALLOCATABLE :: SEG_COUNT(:,:)
INTEGER, ALLOCATABLE :: SEG_TARGET(:)
LOGICAL, ALLOCATABLE :: SEG_HAS_NEIGHBOR(:)

INTEGER :: I, S, DX, DY, GX, GY, IX, IY
INTEGER :: NB, NB_SEG
INTEGER :: BEST_SEG, BEST_COUNT

ALLOCATE(SEG_SIZE(NUM_SEGMENTS))
ALLOCATE(SEG_COUNT(NUM_SEGMENTS, NUM_SEGMENTS))
ALLOCATE(SEG_TARGET(NUM_SEGMENTS))
ALLOCATE(SEG_HAS_NEIGHBOR(NUM_SEGMENTS))

SEG_SIZE = 0
SEG_COUNT = 0
SEG_TARGET = -1
SEG_HAS_NEIGHBOR = .FALSE.

! Count segment sizes
DO I = 1, N

   S = NODES(I)%P%SEGMENT_GROUP

   IF (S .GE. 1 .AND. S .LE. NUM_SEGMENTS) THEN
      SEG_SIZE(S) = SEG_SIZE(S) + 1
   ENDIF

ENDDO

! Count contacts between segments
DO I = 1, N

   S = NODES(I)%P%SEGMENT_GROUP

   IF (S .LT. 1 .OR. S .GT. NUM_SEGMENTS) CYCLE

   DO DX = -1, 1
      DO DY = -1, 1

         IF (DX .EQ. 0 .AND. DY .EQ. 0) CYCLE

         IX = NODES(I)%P%IX + DX
         IY = NODES(I)%P%IY + DY

         GX = IX - IX_MIN + 1
         GY = IY - IY_MIN + 1

         IF (GX .LT. 1 .OR. GX .GT. NX) CYCLE
         IF (GY .LT. 1 .OR. GY .GT. NY) CYCLE

         NB = GRID(GX, GY)

         IF (NB .EQ. 0) CYCLE

         NB_SEG = NODES(NB)%P%SEGMENT_GROUP

         IF (NB_SEG .LT. 1 .OR. NB_SEG .GT. NUM_SEGMENTS) CYCLE
         IF (NB_SEG .EQ. S) CYCLE

         SEG_HAS_NEIGHBOR(S) = .TRUE.

         ! Count only large neighbors as absorption targets
         IF (SEG_SIZE(NB_SEG) .GT. SMALL_LIMIT) THEN
            SEG_COUNT(S, NB_SEG) = SEG_COUNT(S, NB_SEG) + 1
         ENDIF

      ENDDO
   ENDDO

ENDDO

! Choose absorption target
DO S = 1, NUM_SEGMENTS

   IF (SEG_SIZE(S) .EQ. 0) CYCLE

   IF (.NOT. SEG_HAS_NEIGHBOR(S)) THEN
      SEG_TARGET(S) = -1
      CYCLE
   ENDIF

   IF (SEG_SIZE(S) .GT. SMALL_LIMIT) THEN
      SEG_TARGET(S) = S
      CYCLE
   ENDIF

   BEST_SEG = -1
   BEST_COUNT = 0

   DO NB_SEG = 1, NUM_SEGMENTS

      IF (SEG_COUNT(S, NB_SEG) .GT. BEST_COUNT) THEN
         BEST_COUNT = SEG_COUNT(S, NB_SEG)
         BEST_SEG = NB_SEG
      ENDIF

   ENDDO

   IF (BEST_COUNT .GT. 0) THEN
      SEG_TARGET(S) = BEST_SEG
   ELSE
      SEG_TARGET(S) = -1
   ENDIF

ENDDO

! Apply decision
DO I = 1, N

   S = NODES(I)%P%SEGMENT_GROUP

   IF (S .LT. 1 .OR. S .GT. NUM_SEGMENTS) CYCLE

   IF (SEG_TARGET(S) .GT. 0) THEN

      NODES(I)%P%SEGMENT_GROUP = SEG_TARGET(S)

   ELSE

      NODES(I)%P%FIRE_LINE = .FALSE.
      NODES(I)%P%SEGMENT_GROUP = -1
      NODES(I)%P%TYPE_GROUP = -1

   ENDIF

ENDDO

! -------------------------------------------------------------------------
! Recheck after applying absorption decision.
! Some segment ids may become isolated islands after relabeling.
! Remove any segment that does not touch another segment id.
! -------------------------------------------------------------------------

SEG_HAS_NEIGHBOR = .FALSE.

DO I = 1, N

   IF (.NOT. NODES(I)%P%FIRE_LINE) CYCLE

   S = NODES(I)%P%SEGMENT_GROUP

   IF (S .LT. 1 .OR. S .GT. NUM_SEGMENTS) CYCLE

   DO DX = -1, 1
      DO DY = -1, 1

         IF (DX .EQ. 0 .AND. DY .EQ. 0) CYCLE

         IX = NODES(I)%P%IX + DX
         IY = NODES(I)%P%IY + DY

         GX = IX - IX_MIN + 1
         GY = IY - IY_MIN + 1

         IF (GX .LT. 1 .OR. GX .GT. NX) CYCLE
         IF (GY .LT. 1 .OR. GY .GT. NY) CYCLE

         NB = GRID(GX, GY)

         IF (NB .EQ. 0) CYCLE
         IF (.NOT. NODES(NB)%P%FIRE_LINE) CYCLE

         NB_SEG = NODES(NB)%P%SEGMENT_GROUP

         IF (NB_SEG .LT. 1 .OR. NB_SEG .GT. NUM_SEGMENTS) CYCLE
         IF (NB_SEG .EQ. S) CYCLE

         SEG_HAS_NEIGHBOR(S) = .TRUE.

      ENDDO
   ENDDO

ENDDO

! Remove segment ids that became islands
DO I = 1, N

   IF (.NOT. NODES(I)%P%FIRE_LINE) CYCLE

   S = NODES(I)%P%SEGMENT_GROUP

   IF (S .LT. 1 .OR. S .GT. NUM_SEGMENTS) CYCLE

   IF (.NOT. SEG_HAS_NEIGHBOR(S) .AND. SEG_SIZE(S) .LT. 7) THEN
      NODES(I)%P%FIRE_LINE = .FALSE.
      NODES(I)%P%SEGMENT_GROUP = -1
      NODES(I)%P%TYPE_GROUP = -1
   ENDIF

ENDDO

DEALLOCATE(SEG_SIZE)
DEALLOCATE(SEG_COUNT)
DEALLOCATE(SEG_TARGET)
DEALLOCATE(SEG_HAS_NEIGHBOR)

! *****************************************************************************
END SUBROUTINE ABSORB_SMALL_SEGMENTS
! *****************************************************************************

! *****************************************************************************
SUBROUTINE DETECT_FIRELINE
! *****************************************************************************

TYPE(NODE), POINTER :: C

INTEGER :: I
INTEGER :: DX, DY
INTEGER :: IX, IY
INTEGER :: IX_MIN, IX_MAX
INTEGER :: IY_MIN, IY_MAX
INTEGER :: N_NEIGHBORS

INTEGER :: TOA_IX_MIN, TOA_IX_MAX
INTEGER :: TOA_IY_MIN, TOA_IY_MAX

LOGICAL, ALLOCATABLE :: FIRELINE_MASK(:,:)

! ---------------------------------------------------------------------------
! Return immediately if the list is empty
! ---------------------------------------------------------------------------
IF (LIST_TAGGED%NUM_NODES .LE. 0) RETURN
IF (.NOT. ASSOCIATED(LIST_TAGGED%HEAD)) RETURN

! Bounds of the global TIME_OF_ARRIVAL array
TOA_IX_MIN = LBOUND(TIME_OF_ARRIVAL, 1)
TOA_IX_MAX = UBOUND(TIME_OF_ARRIVAL, 1)
TOA_IY_MIN = LBOUND(TIME_OF_ARRIVAL, 2)
TOA_IY_MAX = UBOUND(TIME_OF_ARRIVAL, 2)

! ---------------------------------------------------------------------------
! Find the bounding box of LIST_TAGGED.
!
! The temporary grid is restricted to this bounding box instead of allocating
! an array over the entire computational domain.
! ---------------------------------------------------------------------------
IX_MIN = HUGE(IX_MIN)
IY_MIN = HUGE(IY_MIN)
IX_MAX = -HUGE(IX_MAX)
IY_MAX = -HUGE(IY_MAX)

C => LIST_TAGGED%HEAD

DO I = 1, LIST_TAGGED%NUM_NODES

   IX_MIN = MIN(IX_MIN, C%IX)
   IX_MAX = MAX(IX_MAX, C%IX)
   IY_MIN = MIN(IY_MIN, C%IY)
   IY_MAX = MAX(IY_MAX, C%IY)

   ! Important if DETECT_FIRELINE is called repeatedly
   C%FIRE_LINE = .FALSE.

   C => C%NEXT

ENDDO

! Add one cell around the bounding box for checking the eight neighbors
ALLOCATE(FIRELINE_MASK(IX_MIN-1:IX_MAX+1, IY_MIN-1:IY_MAX+1))
FIRELINE_MASK = .FALSE.

! ---------------------------------------------------------------------------
! Detect candidate fireline cells
! ---------------------------------------------------------------------------
C => LIST_TAGGED%HEAD

DO I = 1, LIST_TAGGED%NUM_NODES

   IF (C%TIME_OF_ARRIVAL .EQ. -1 .AND. &
       C%TIME_SUPPRESSED .EQ. -1) THEN

      SEARCH_NEIGHBORS: DO DX = -FIRE_LINE_THICKNESS, &
                                 FIRE_LINE_THICKNESS

         IX = C%IX + DX

         ! Avoid accessing outside TIME_OF_ARRIVAL
         IF (IX .LT. TOA_IX_MIN .OR. IX .GT. TOA_IX_MAX) CYCLE

         DO DY = -FIRE_LINE_THICKNESS, FIRE_LINE_THICKNESS

            IF (DX .EQ. 0 .AND. DY .EQ. 0) CYCLE

            IY = C%IY + DY

            ! Avoid accessing outside TIME_OF_ARRIVAL
            IF (IY .LT. TOA_IY_MIN .OR. IY .GT. TOA_IY_MAX) CYCLE

            IF (TIME_OF_ARRIVAL(IX,IY) .GT. 0) THEN
               C%FIRE_LINE = .TRUE.

               ! No reason to continue searching after finding one
               EXIT SEARCH_NEIGHBORS
            ENDIF

         ENDDO
      ENDDO SEARCH_NEIGHBORS

   ENDIF

   C => C%NEXT

ENDDO

! ---------------------------------------------------------------------------
! Store candidate fireline cells in the temporary grid.
!
! This replaces searching the linked list for every neighboring coordinate.
! ---------------------------------------------------------------------------
C => LIST_TAGGED%HEAD

DO I = 1, LIST_TAGGED%NUM_NODES

   IF (C%FIRE_LINE) THEN
      FIRELINE_MASK(C%IX,C%IY) = .TRUE.
   ENDIF

   C => C%NEXT

ENDDO

! ---------------------------------------------------------------------------
! Remove isolated fireline cells
!
! The mask is not modified during this pass. Therefore, all cells are tested
! against the original candidate fireline, avoiding order-dependent results.
! ---------------------------------------------------------------------------
C => LIST_TAGGED%HEAD

DO I = 1, LIST_TAGGED%NUM_NODES

   IF (C%FIRE_LINE) THEN

      N_NEIGHBORS = 0

      DO DX = -1, 1
         DO DY = -1, 1

            IF (DX .EQ. 0 .AND. DY .EQ. 0) CYCLE

            IF (FIRELINE_MASK(C%IX + DX, C%IY + DY)) THEN
               N_NEIGHBORS = N_NEIGHBORS + 1

               ! Only one neighbor is needed to prove it is not isolated
               EXIT
            ENDIF

         ENDDO

         IF (N_NEIGHBORS .GT. 0) EXIT
      ENDDO

      IF (N_NEIGHBORS .EQ. 0) THEN
         C%FIRE_LINE = .FALSE.
      ENDIF

   ENDIF

   C => C%NEXT

ENDDO

DEALLOCATE(FIRELINE_MASK)

! *****************************************************************************
END SUBROUTINE DETECT_FIRELINE
! *****************************************************************************





! *****************************************************************************
SUBROUTINE SEGMENT_FIRELINE
! *****************************************************************************
! REAL(8), INTENT(IN) :: T
! REAL,    INTENT(IN) :: DT
TYPE(NODE), POINTER :: C
REAL :: C_ELLIPSE, X_PNT, COSANG
INTEGER  :: IX_MIN, IX_MAX, IY_MIN, IY_MAX
INTEGER :: I, SEGMENT_ID, N, NX, NY, GX, GY, DX, DY, IX, IY, HEAD, TAIL, CURRENT, NB

TYPE(NODE_PTR), ALLOCATABLE :: NODES(:)
INTEGER, ALLOCATABLE :: GRID(:,:)
INTEGER, ALLOCATABLE :: QUEUE(:)
INTEGER, ALLOCATABLE :: MAP_SEG(:)
INTEGER :: OLD_SEG

! detect fireline
CALL DETECT_FIRELINE


N = LIST_TAGGED%NUM_NODES
IF (N .LE. 0) THEN
   LIST_TAGGED%NUM_SEGMENTS = 0
   RETURN
END IF


! determine fireline type.
C => LIST_TAGGED%HEAD
DO I = 1, LIST_TAGGED%NUM_NODES

   IF (.NOT. C%FIRE_LINE) THEN
      C => C%NEXT
      CYCLE
   ENDIF

   ! We can get sin(theta - dms) and cos(theta - dms) directly:
   COSANG   = C%NORMVECTORY*C%NORMVECTORY_DMS + C%NORMVECTORX*C%NORMVECTORX_DMS

   C_ELLIPSE = (C%VELOCITY_DMS - C%VBACK)*0.5
   X_PNT = C%VELOCITY*COSANG

   ! TYPE_GROUP = 2:head; 1:flank; 0:back
   IF (X_PNT .GE. C_ELLIPSE) THEN
      C%TYPE_GROUP = 2
   ELSE IF (X_PNT .GE. 0) THEN
      C%TYPE_GROUP = 1
   ELSE
      C%TYPE_GROUP = 0
   ENDIF

   C => C%NEXT
ENDDO

! find IX_MIN, IX_MAX, IY_MIN, IY_MAX
IX_MIN = 9999; IX_MAX = -9999
IY_MIN = 9999; IY_MAX = -9999

C => LIST_TAGGED%HEAD
DO I = 1, LIST_TAGGED%NUM_NODES
   IF (C%IX .GT. IX_MAX) IX_MAX = C%IX
   IF (C%IX .LT. IX_MIN) IX_MIN = C%IX
   IF (C%IY .GT. IY_MAX) IY_MAX = C%IY
   IF (C%IY .LT. IY_MIN) IY_MIN = C%IY
   C => C%NEXT
ENDDO

! segment the fireline

NX = IX_MAX - IX_MIN + 1
NY = IY_MAX - IY_MIN + 1

ALLOCATE(NODES(N))
ALLOCATE(GRID(NX, NY))
ALLOCATE(QUEUE(N))

GRID = 0

C => LIST_TAGGED%HEAD

DO I = 1, N

   NODES(I)%P => C

   IF (.NOT. C%FIRE_LINE) THEN
      C => C%NEXT
      CYCLE
   ENDIF


   GX = C%IX - IX_MIN + 1
   GY = C%IY - IY_MIN + 1

   IF (GX .LT. 1 .OR. GX .GT. NX .OR. GY .LT. 1 .OR. GY .GT. NY) THEN
      WRITE(*,*) 'SUPPRESSION MODULE ERROR: NODE OUTSIDE GRID BOUNDS'
      STOP
   END IF

   GRID(GX, GY) = I
   C%SEGMENT_GROUP = 0

   C => C%NEXT
ENDDO

SEGMENT_ID = 0

DO I = 1, N

   IF (NODES(I)%P%SEGMENT_GROUP .NE. 0) CYCLE

   SEGMENT_ID = SEGMENT_ID + 1

   HEAD = 1
   TAIL = 1
   QUEUE(TAIL) = I
   NODES(I)%P%SEGMENT_GROUP = SEGMENT_ID

   DO WHILE (HEAD .LE. TAIL)
      CURRENT = QUEUE(HEAD)
      HEAD = HEAD + 1

      DO DX = -1, 1
         DO DY = -1, 1
            IF (DX .EQ. 0 .AND. DY .EQ. 0) CYCLE

            IX = NODES(CURRENT)%P%IX + DX
            IY = NODES(CURRENT)%P%IY + DY

            GX = IX - IX_MIN + 1
            GY = IY - IY_MIN + 1

            IF (GX .LT. 1 .OR. GX .GT. NX) CYCLE
            IF (GY .LT. 1 .OR. GY .GT. NY) CYCLE

            NB = GRID(GX, GY)

            IF (NB .EQ. 0) CYCLE

            IF (NODES(NB)%P%SEGMENT_GROUP .NE. 0) CYCLE

            IF (IS_CLOSE(NODES(CURRENT)%P, NODES(NB)%P, DELTA_ROS, DELTA_FL, DELTA_SDI, DELTA_PCL)) THEN
               TAIL = TAIL + 1
               QUEUE(TAIL) = NB
               NODES(NB)%P%SEGMENT_GROUP = SEGMENT_ID
            ENDIF
         ENDDO
      ENDDO
   ENDDO
ENDDO

CALL ABSORB_SMALL_SEGMENTS(NODES, GRID, N, NX, NY, IX_MIN, IY_MIN, SEGMENT_ID, 3)
LIST_TAGGED%NUM_SEGMENTS = SEGMENT_ID
DEALLOCATE(NODES)
DEALLOCATE(GRID)
DEALLOCATE(QUEUE)

!correcting segment_id and numbers
ALLOCATE(MAP_SEG(LIST_TAGGED%NUM_SEGMENTS))
MAP_SEG = 0
C => LIST_TAGGED%HEAD
SEGMENT_ID = 0

DO I = 1, LIST_TAGGED%NUM_NODES

   IF (.NOT. C%FIRE_LINE) THEN
      C => C%NEXT
      CYCLE
   ENDIF

   OLD_SEG = C%SEGMENT_GROUP

   IF (MAP_SEG(OLD_SEG) .EQ. 0) THEN
      SEGMENT_ID = SEGMENT_ID + 1
      MAP_SEG(OLD_SEG) = SEGMENT_ID
   ENDIF

   C%SEGMENT_GROUP = MAP_SEG(OLD_SEG)

   C => C%NEXT

ENDDO


LIST_TAGGED%NUM_SEGMENTS = SEGMENT_ID
DEALLOCATE(MAP_SEG)

! *****************************************************************************
END SUBROUTINE SEGMENT_FIRELINE
! *****************************************************************************

! *****************************************************************************
SUBROUTINE CALCULATE_STS
! *****************************************************************************

! STS: Suppression Type Score

TYPE(NODE), POINTER :: C
REAL, ALLOCATABLE ::  SDI_AVG(:), FL_AVG(:), N_CELLS(:)
INTEGER :: I
REAL :: FL_NI, SDI_NI

ALLOCATE(SUPPRESSION_TYPE_SCORE(LIST_TAGGED%NUM_SEGMENTS))
ALLOCATE(FL_AVG(LIST_TAGGED%NUM_SEGMENTS))
ALLOCATE(SDI_AVG(LIST_TAGGED%NUM_SEGMENTS))
ALLOCATE(N_CELLS(LIST_TAGGED%NUM_SEGMENTS))

SDI_AVG = 0.0
FL_AVG  = 0.0
N_CELLS = 0.0


! calculate max of FL and SDI over fireline
! calculate mean of SDI and FL over each segment
C => LIST_TAGGED%HEAD
DO I = 1, LIST_TAGGED%NUM_NODES

   IF (.NOT. C%FIRE_LINE) THEN
      C => C%NEXT
      CYCLE
   ENDIF

   SDI_AVG(C%SEGMENT_GROUP) = SDI_AVG(C%SEGMENT_GROUP) + C%SDI
   FL_AVG(C%SEGMENT_GROUP)  = FL_AVG(C%SEGMENT_GROUP)  + C%FLAME_LENGTH
   N_CELLS(C%SEGMENT_GROUP) = N_CELLS(C%SEGMENT_GROUP) + 1.0

   C => C%NEXT
ENDDO

! calculate STS for seg
DO I = 1, LIST_TAGGED%NUM_SEGMENTS

   IF (N_CELLS(I) .EQ. 0) CYCLE

   FL_AVG(I) = FL_AVG(I)/N_CELLS(I)
   FL_NI = (FL_AVG(I)/FL_MAX_DIRECT_ATTACK)

   SDI_AVG(I) = SDI_AVG(I)/N_CELLS(I)
   SDI_NI = (SDI_AVG(I)/SDI_MAX_DIRECT_ATTACK)

   IF (FL_NI .LE. 1E-6 .AND. SDI_NI .LE. 1E-6) THEN
      SUPPRESSION_TYPE_SCORE(I) = 100.0
   ELSE
      SUPPRESSION_TYPE_SCORE(I) = (0.7*FL_NI) + (0.3*SDI_NI)
   ENDIF

ENDDO


! assign sts to each node
C => LIST_TAGGED%HEAD
DO I = 1, LIST_TAGGED%NUM_NODES

   IF (.NOT. C%FIRE_LINE) THEN
      C => C%NEXT
      CYCLE
   ENDIF

   C%STS = SUPPRESSION_TYPE_SCORE(C%SEGMENT_GROUP)

   C => C%NEXT
ENDDO


DEALLOCATE(FL_AVG)
DEALLOCATE(SDI_AVG)
DEALLOCATE(N_CELLS)


! *****************************************************************************
END SUBROUTINE CALCULATE_STS
! *****************************************************************************

! *****************************************************************************
SUBROUTINE SORT_STS
! *****************************************************************************
INTEGER :: I, J, TEMP_INDX
ALLOCATE(SUPPRESSION_TYPE_SCORE_RANK(LIST_TAGGED%NUM_SEGMENTS))

! initiate the rank array
DO I = 1, LIST_TAGGED%NUM_SEGMENTS
   SUPPRESSION_TYPE_SCORE_RANK(I) = i
ENDDO

! sorting index
DO I = 1, LIST_TAGGED%NUM_SEGMENTS-1
   DO J = I+1, LIST_TAGGED%NUM_SEGMENTS
      IF (SUPPRESSION_TYPE_SCORE(SUPPRESSION_TYPE_SCORE_RANK(J)) .LT. SUPPRESSION_TYPE_SCORE(SUPPRESSION_TYPE_SCORE_RANK(I))) THEN
         TEMP_INDX = SUPPRESSION_TYPE_SCORE_RANK(I)
         SUPPRESSION_TYPE_SCORE_RANK(I) = SUPPRESSION_TYPE_SCORE_RANK(J)
         SUPPRESSION_TYPE_SCORE_RANK(J) = TEMP_INDX
      ENDIF
   ENDDO
ENDDO

! *****************************************************************************
END SUBROUTINE SORT_STS
! *****************************************************************************


! *****************************************************************************
SUBROUTINE GET_BEST_START_SEGMENT(L_REQ, SEG_SUPPRESSED, CURRENT_SEG)
! *****************************************************************************

REAL, INTENT(IN) :: L_REQ(:)
LOGICAL, INTENT(IN) :: SEG_SUPPRESSED(:)
INTEGER, INTENT(OUT) :: CURRENT_SEG

INTEGER :: I, SEG

CURRENT_SEG = -1

DO I = 1, LIST_TAGGED%NUM_SEGMENTS

   SEG = SUPPRESSION_TYPE_SCORE_RANK(I)

   IF (SEG .LE. 0) CYCLE
   IF (SEG_SUPPRESSED(SEG)) CYCLE
   IF (L_REQ(SEG) .LE. 0.0) CYCLE

   CURRENT_SEG = SEG
   EXIT

ENDDO

! *****************************************************************************
END SUBROUTINE GET_BEST_START_SEGMENT
! *****************************************************************************

! *****************************************************************************
SUBROUTINE GET_BEST_CANDIDATE_SEGMENT(SEG_CANDIDATE, L_REQ, CURRENT_SEG)
! *****************************************************************************
LOGICAL, INTENT(IN) :: SEG_CANDIDATE(:)
REAL, INTENT(IN) :: L_REQ(:)
INTEGER, INTENT(OUT) :: CURRENT_SEG

INTEGER :: I, SEG

CURRENT_SEG = -1

DO I = 1, LIST_TAGGED%NUM_SEGMENTS

   SEG = SUPPRESSION_TYPE_SCORE_RANK(I)

   IF (SEG .LE. 0) CYCLE
   IF (.NOT. SEG_CANDIDATE(SEG)) CYCLE
   IF (L_REQ(SEG) .LE. 0.0) CYCLE

   CURRENT_SEG = SEG
   EXIT

ENDDO

END SUBROUTINE GET_BEST_CANDIDATE_SEGMENT
! *****************************************************************************

! *****************************************************************************
SUBROUTINE SUPPRESS_FULL_SEGMENT(T, IT, SEG_ID)
! *****************************************************************************
REAL(8), INTENT(IN) :: T
INTEGER, INTENT(IN) :: IT, SEG_ID

TYPE(NODE), POINTER :: C
INTEGER :: J

C => LIST_TAGGED%HEAD
DO J = 1, LIST_TAGGED%NUM_NODES

   IF (.NOT. C%FIRE_LINE .OR. C%SEGMENT_GROUP .NE. SEG_ID) THEN
      C => C%NEXT
      CYCLE
   ENDIF

   C%TIME_SUPPRESSED = T
   C%SUPPRESSION_ADJUSTMENT_FACTOR = 0.0

   SUPP(IT)%SUPPRESSED_FIRELINE_LENGTH = &
      SUPP(IT)%SUPPRESSED_FIRELINE_LENGTH + ANALYSIS_CELLSIZE

   C => C%NEXT

ENDDO

END SUBROUTINE SUPPRESS_FULL_SEGMENT
! *****************************************************************************

! *****************************************************************************
SUBROUTINE FIND_ADJACENT_SEGMENTS(SEG_ID, SEG_SUPPRESSED, SEG_CANDIDATE)
! *****************************************************************************
INTEGER, INTENT(IN) :: SEG_ID
LOGICAL, INTENT(IN) :: SEG_SUPPRESSED(:)
LOGICAL, INTENT(INOUT) :: SEG_CANDIDATE(:)

TYPE(NODE), POINTER :: C1, C2
INTEGER :: I, J, SEG2

C1 => LIST_TAGGED%HEAD
DO I = 1, LIST_TAGGED%NUM_NODES

   IF (.NOT. C1%FIRE_LINE .OR. C1%SEGMENT_GROUP .NE. SEG_ID) THEN
      C1 => C1%NEXT
      CYCLE
   ENDIF

   C2 => LIST_TAGGED%HEAD
   DO J = 1, LIST_TAGGED%NUM_NODES

      IF (.NOT. C2%FIRE_LINE) THEN
         C2 => C2%NEXT
         CYCLE
      ENDIF

      SEG2 = C2%SEGMENT_GROUP

      IF (SEG2 .LE. 0 .OR. SEG2 .EQ. SEG_ID) THEN
         C2 => C2%NEXT
         CYCLE
      ENDIF

      IF (SEG_SUPPRESSED(SEG2)) THEN
         C2 => C2%NEXT
         CYCLE
      ENDIF

      IF (ABS(C1%IX - C2%IX) .LE. 1 .AND. ABS(C1%IY - C2%IY) .LE. 1) THEN
         SEG_CANDIDATE(SEG2) = .TRUE.
      ENDIF

      C2 => C2%NEXT

   ENDDO

   C1 => C1%NEXT

ENDDO

! *****************************************************************************
END SUBROUTINE FIND_ADJACENT_SEGMENTS
! *****************************************************************************



! *****************************************************************************
SUBROUTINE SUPPRESS_PARTIAL_SEGMENT_CONNECTED(T, IT, SEG_ID, PREV_SEG_ID, L_CAP)
! *****************************************************************************
REAL(8), INTENT(IN) :: T
INTEGER, INTENT(IN) :: IT, SEG_ID, PREV_SEG_ID
REAL, INTENT(IN) :: L_CAP

TYPE NODE_PTR
   TYPE(NODE), POINTER :: P
END TYPE NODE_PTR

TYPE(NODE), POINTER :: C, C_PREV, CN
TYPE(NODE_PTR), ALLOCATABLE :: NODES(:), QUEUE(:)

INTEGER :: I, J, N, HEAD, TAIL
INTEGER :: N_CELL_AVAIL, N_SUPPRESSED
INTEGER :: START_ID
LOGICAL, ALLOCATABLE :: VISITED(:)

N_CELL_AVAIL = CEILING(L_CAP * (1.0 - SUPPRESSION_TYPE_SCORE(SEG_ID)) / ANALYSIS_CELLSIZE)

IF (N_CELL_AVAIL .LE. 0) RETURN

! Count cells in current segment
N = 0
C => LIST_TAGGED%HEAD
DO I = 1, LIST_TAGGED%NUM_NODES

   IF (C%FIRE_LINE .AND. C%SEGMENT_GROUP .EQ. SEG_ID) THEN
      N = N + 1
   ENDIF

   C => C%NEXT
ENDDO

IF (N .LE. 0) RETURN

ALLOCATE(NODES(N))
ALLOCATE(QUEUE(N))
ALLOCATE(VISITED(N))

VISITED = .FALSE.

! Store current segment nodes
N = 0
C => LIST_TAGGED%HEAD
DO I = 1, LIST_TAGGED%NUM_NODES

   IF (C%FIRE_LINE .AND. C%SEGMENT_GROUP .EQ. SEG_ID) THEN
      N = N + 1
      NODES(N)%P => C
   ENDIF

   C => C%NEXT
ENDDO

! Find start cell in current segment touching previous suppressed segment
START_ID = -1

DO I = 1, N

   C => NODES(I)%P

   C_PREV => LIST_TAGGED%HEAD
   DO J = 1, LIST_TAGGED%NUM_NODES

      IF (.NOT. C_PREV%FIRE_LINE) THEN
         C_PREV => C_PREV%NEXT
         CYCLE
      ENDIF

      IF (C_PREV%SEGMENT_GROUP .NE. PREV_SEG_ID) THEN
         C_PREV => C_PREV%NEXT
         CYCLE
      ENDIF

      IF (C_PREV%SUPPRESSION_ADJUSTMENT_FACTOR .NE. 0.0) THEN
         C_PREV => C_PREV%NEXT
         CYCLE
      ENDIF

      IF (ABS(C%IX - C_PREV%IX) .LE. 1 .AND. ABS(C%IY - C_PREV%IY) .LE. 1) THEN
         START_ID = I
         EXIT
      ENDIF

      C_PREV => C_PREV%NEXT

   ENDDO

   IF (START_ID .GT. 0) EXIT

ENDDO

! If no connection found, fallback to first cell
IF (START_ID .LT. 0) START_ID = 1

! Start BFS from connected cell
HEAD = 1
TAIL = 1

QUEUE(TAIL)%P => NODES(START_ID)%P
VISITED(START_ID) = .TRUE.

N_SUPPRESSED = 0

DO WHILE (HEAD .LE. TAIL .AND. N_SUPPRESSED .LT. N_CELL_AVAIL)

   C => QUEUE(HEAD)%P
   HEAD = HEAD + 1

   C%TIME_SUPPRESSED = T
   C%SUPPRESSION_ADJUSTMENT_FACTOR = 0.0

   SUPP(IT)%SUPPRESSED_FIRELINE_LENGTH = &
      SUPP(IT)%SUPPRESSED_FIRELINE_LENGTH + ANALYSIS_CELLSIZE

   N_SUPPRESSED = N_SUPPRESSED + 1

   ! Grow to connected cells in same segment
   DO J = 1, N

      IF (VISITED(J)) CYCLE

      CN => NODES(J)%P

      IF (ABS(C%IX - CN%IX) .LE. 1 .AND. ABS(C%IY - CN%IY) .LE. 1) THEN

         TAIL = TAIL + 1
         QUEUE(TAIL)%P => CN
         VISITED(J) = .TRUE.

      ENDIF

   ENDDO

ENDDO

DEALLOCATE(NODES)
DEALLOCATE(QUEUE)
DEALLOCATE(VISITED)

! *****************************************************************************
END SUBROUTINE SUPPRESS_PARTIAL_SEGMENT_CONNECTED
! *****************************************************************************



! *****************************************************************************
SUBROUTINE GET_BEST_REMAINING_SEGMENT(DEG_UPPER_LIM, DEG_LOWER_LIM, SEG_DEG, L_REQ, SEG_SUPPRESSED, CURRENT_SEG)
! *****************************************************************************

REAL, INTENT(IN) :: L_REQ(:), SEG_DEG(:)
REAL, INTENT(IN) :: DEG_UPPER_LIM, DEG_LOWER_LIM
LOGICAL, INTENT(IN) :: SEG_SUPPRESSED(:)
INTEGER, INTENT(OUT) :: CURRENT_SEG

INTEGER :: I, SEG

CURRENT_SEG = -1

DO I = 1, LIST_TAGGED%NUM_SEGMENTS

   SEG = SUPPRESSION_TYPE_SCORE_RANK(I)

   IF (SEG .LE. 0) CYCLE
   IF (SEG_SUPPRESSED(SEG)) CYCLE
   IF (L_REQ(SEG) .LE. 0.0) CYCLE
   IF (SEG_DEG(SEG) .GT. DEG_UPPER_LIM .AND. SEG_DEG(SEG) .LT. DEG_LOWER_LIM) CYCLE

   CURRENT_SEG = SEG
   EXIT

ENDDO

END SUBROUTINE GET_BEST_REMAINING_SEGMENT
! *****************************************************************************


! *****************************************************************************
SUBROUTINE DIRECT_ATTACK(T, IT, RANK_FINISHED, DT, ICASE, TSTOP)
! *****************************************************************************

REAL(8), INTENT(IN) :: T
INTEGER, INTENT(IN) :: IT
INTEGER, INTENT(IN) :: ICASE
REAL, INTENT(INOUT) :: DT, TSTOP
INTEGER, INTENT(INOUT) :: RANK_FINISHED

TYPE(NODE), POINTER :: C
REAL(8) :: NORM_TIME
INTEGER :: I, CURRENT_SEG, PREV_SEG
REAL :: CAP_0, L_CAP, REF_DEG, DEG_UPPER_LIM, DEG_LOWER_LIM, DEG_STEP, CONTAINMENT_COEF
REAL, ALLOCATABLE :: L_SEG(:), L_REQ(:), SEG_IXCEN(:), SEG_IYCEN(:), SEG_DEG(:)
LOGICAL, ALLOCATABLE :: SEG_SUPPRESSED(:), SEG_CANDIDATE(:)

! Initialize current time-step values
SUPP(IT)%FIRE_LINE_LENGTH = 0.0
SUPP(IT)%SUPPRESSED_FIRELINE_LENGTH = 0.0

IF (IT .NE. 1) THEN
   SUPP(IT)%SUPPRESSED_FIRELINE_LENGTH = SUPP(IT)%SUPPRESSED_FIRELINE_LENGTH + SUPP(IT-1)%SUPPRESSED_FIRELINE_LENGTH + SUPP(IT)%INDIRECT_SUPPRESSED_FIRELINE_LENGTH
   SUPP(IT)%FIRE_LINE_LENGTH = SUPP(IT)%FIRE_LINE_LENGTH + SUPP(IT)%SUPPRESSED_FIRELINE_LENGTH
ENDIF

CAP_0 = FIRE_LINE_THICKNESS * AVAILABLE_SUPPRESSION_CAPACITY * (1.0 / 3600.0)
L_CAP = CAP_0 * DT_EXTENDED_ATTACK


ALLOCATE(L_SEG(LIST_TAGGED%NUM_SEGMENTS))
ALLOCATE(SEG_IXCEN(LIST_TAGGED%NUM_SEGMENTS))
ALLOCATE(SEG_IYCEN(LIST_TAGGED%NUM_SEGMENTS))
ALLOCATE(SEG_DEG(LIST_TAGGED%NUM_SEGMENTS))
ALLOCATE(L_REQ(LIST_TAGGED%NUM_SEGMENTS))
ALLOCATE(SEG_SUPPRESSED(LIST_TAGGED%NUM_SEGMENTS))
ALLOCATE(SEG_CANDIDATE(LIST_TAGGED%NUM_SEGMENTS))

L_SEG = 0.0
L_REQ = 0.0
SEG_IXCEN = 0.0
SEG_IYCEN = 0.0
SEG_DEG = 0.0
SEG_SUPPRESSED = .FALSE.
SEG_CANDIDATE = .FALSE.

! Calculate center of the active fire area
CALL CENTROID(IT)

! Calculate active fireline length in each segment, center of each segment
C => LIST_TAGGED%HEAD
DO I = 1, LIST_TAGGED%NUM_NODES

   IF (.NOT. C%FIRE_LINE) THEN
      C => C%NEXT
      CYCLE
   ENDIF

   SUPP(IT)%FIRE_LINE_LENGTH = SUPP(IT)%FIRE_LINE_LENGTH + ANALYSIS_CELLSIZE

   IF (C%SEGMENT_GROUP .GT. 0 .AND. C%SEGMENT_GROUP .LE. LIST_TAGGED%NUM_SEGMENTS) THEN
      L_SEG(C%SEGMENT_GROUP) = L_SEG(C%SEGMENT_GROUP) + ANALYSIS_CELLSIZE
      SEG_IXCEN(C%SEGMENT_GROUP) = SEG_IXCEN(C%SEGMENT_GROUP) + C%IX
      SEG_IYCEN(C%SEGMENT_GROUP) = SEG_IYCEN(C%SEGMENT_GROUP) + C%IY
   ENDIF

   C => C%NEXT

ENDDO

! Required effort for each segment
DO I = 1, LIST_TAGGED%NUM_SEGMENTS

   IF (L_SEG(I) .LE. 0.0) THEN
      L_REQ(I) = 0.0
      SEG_IXCEN(I) = 0.0
      SEG_IYCEN(I) = 0.0
   ELSEIF ((1.0 - SUPPRESSION_TYPE_SCORE(I)) .GT. 0.0) THEN
      L_REQ(I) = L_SEG(I) / (1.0 - SUPPRESSION_TYPE_SCORE(I))
      SEG_IXCEN(I) = SEG_IXCEN(I) / (L_SEG(I)/ANALYSIS_CELLSIZE)
      SEG_IYCEN(I) = SEG_IYCEN(I) / (L_SEG(I)/ANALYSIS_CELLSIZE)
   ELSE
      L_REQ(I) = 0.0
      SEG_IXCEN(I) = 0.0
      SEG_IYCEN(I) = 0.0
   ENDIF

ENDDO


IF (T .GE. 86400.0 .AND. EXTENDED_ATTACK_TIME .EQ. -1.0) THEN
   EXTENDED_ATTACK_TIME = (T/SUPP(IT)%FIRE_LINE_LENGTH) * FIRELINE_LENGTH_REF * FIRE_LINE_THICKNESS
ENDIF

IF (EXTENDED_ATTACK_TIME .EQ. -1.0) THEN
   NORM_TIME = T / 864000.0
ELSE
   NORM_TIME = T / EXTENDED_ATTACK_TIME
ENDIF

CONTAINMENT_COEF = NORM_TIME ** INITIAL_CONTAINMENT_SHAPE_FACTOR
CONTAINMENT_COEF = MIN(CONTAINMENT_COEF, 1.0)

L_CAP = MIN(CONTAINMENT_COEF * L_CAP, SUM(L_SEG))

IF (L_CAP .LE. ANALYSIS_CELLSIZE) L_CAP = 0.0

! Start first continuous attack chain
PREV_SEG = -1
CURRENT_SEG = -1
! Store the start location of the suppression
IF (IT .EQ. 1) THEN
   CALL GET_BEST_START_SEGMENT(L_REQ, SEG_SUPPRESSED, CURRENT_SEG)
   IX_SUPP_START = SEG_IXCEN(CURRENT_SEG)
   IY_SUPP_START = SEG_IYCEN(CURRENT_SEG)
ENDIF

! Calculate segment degrees relative to the start suppression point 
REF_DEG = 90.0 - ATAN2(REAL(IY_SUPP_START - SUPP(IT)%IYCEN), REAL(IX_SUPP_START - SUPP(IT)%IXCEN)) / PIO180 ! north zero, clockwise positive
IF (REF_DEG .LT.   0) REF_DEG = REF_DEG + 360
IF (REF_DEG .EQ. 360) REF_DEG = 0
DO I = 1, LIST_TAGGED%NUM_SEGMENTS

   IF (L_SEG(I) .LE. 0.0) THEN
      SEG_DEG(I) = -1.0
      CYCLE
   ENDIF

   SEG_DEG(I) = 90.0 - ATAN2(REAL(SEG_IYCEN(I) - SUPP(IT)%IYCEN), REAL(SEG_IXCEN(I) - SUPP(IT)%IXCEN)) / PIO180

   IF (SEG_DEG(I) .LT. 0.0) SEG_DEG(I) = SEG_DEG(I) + 360.0
   IF (SEG_DEG(I) .GE. 360.0) SEG_DEG(I) = SEG_DEG(I) - 360.0

   SEG_DEG(I) = SEG_DEG(I) - REF_DEG
   IF (SEG_DEG(I) .LT. 0.0) SEG_DEG(I) = SEG_DEG(I) + 360.0
   IF (SEG_DEG(I) .GE. 360.0) SEG_DEG(I) = SEG_DEG(I) - 360.0

ENDDO

! initialize the current segment
DEG_STEP = 20
DEG_UPPER_LIM = 0
DEG_LOWER_LIM = 360
DO WHILE (CURRENT_SEG .LT. 1)

   DEG_UPPER_LIM = DEG_UPPER_LIM + DEG_STEP
   DEG_LOWER_LIM = DEG_LOWER_LIM - DEG_STEP
   IF (DEG_UPPER_LIM .GT. 180 .OR. DEG_LOWER_LIM .LT. 180) EXIT

   CALL GET_BEST_REMAINING_SEGMENT(DEG_UPPER_LIM, DEG_LOWER_LIM, SEG_DEG, L_REQ, SEG_SUPPRESSED, CURRENT_SEG)

ENDDO

! Grow suppression continuously.
! If one continuous chain ends, start another chain with remaining capacity.
DO WHILE (L_CAP .GT. 0.0)

   IF (CURRENT_SEG .LE. 0) THEN
      DO WHILE (CURRENT_SEG .LT. 0)

         DEG_UPPER_LIM = DEG_UPPER_LIM + DEG_STEP
         DEG_LOWER_LIM = DEG_LOWER_LIM - DEG_STEP
         IF (DEG_UPPER_LIM .GT. 180 .OR. DEG_LOWER_LIM .LT. 180) EXIT

         CALL GET_BEST_REMAINING_SEGMENT(DEG_UPPER_LIM, DEG_LOWER_LIM, SEG_DEG, L_REQ, SEG_SUPPRESSED, CURRENT_SEG)

      ENDDO
      PREV_SEG = -1
   ENDIF

   IF (CURRENT_SEG .LE. 0) THEN
      ! WRITE(*,*) "HEREEEEEEE!!!!"
      EXIT
   ENDIF

   IF (L_REQ(CURRENT_SEG) .LE. 0.0) THEN
      SEG_SUPPRESSED(CURRENT_SEG) = .TRUE.
      CURRENT_SEG = -1
      CYCLE
   ENDIF

   IF (L_CAP .GE. L_REQ(CURRENT_SEG)) THEN

      CALL SUPPRESS_FULL_SEGMENT(T, IT, CURRENT_SEG)

      L_CAP = L_CAP - L_REQ(CURRENT_SEG)
      SEG_SUPPRESSED(CURRENT_SEG) = .TRUE.

   ELSE

      CALL SUPPRESS_PARTIAL_SEGMENT_CONNECTED(T, IT, CURRENT_SEG, PREV_SEG, L_CAP)

      L_CAP = 0.0
      SEG_SUPPRESSED(CURRENT_SEG) = .TRUE.

   ENDIF

   SEG_CANDIDATE = .FALSE.
   CALL FIND_ADJACENT_SEGMENTS(CURRENT_SEG, SEG_SUPPRESSED, SEG_CANDIDATE)

   PREV_SEG = CURRENT_SEG
   CALL GET_BEST_CANDIDATE_SEGMENT(SEG_CANDIDATE, L_REQ, CURRENT_SEG)

ENDDO

IF (SUPP(IT)%FIRE_LINE_LENGTH .GT. 0.0) THEN !1000.0*FIRE_LINE_THICKNESS) THEN
   SUPP(IT)%CONTAINMENT = SUPP(IT)%SUPPRESSED_FIRELINE_LENGTH / &
                          SUPP(IT)%FIRE_LINE_LENGTH
ELSE
   SUPP(IT)%CONTAINMENT = 0.0
ENDIF

WRITE(*,'(F12.4,",",F10.0,",",F10.0,",",F10.7)') &
    SUPP(IT)%T, &
    SUPP(IT)%SUPPRESSED_FIRELINE_LENGTH/FIRE_LINE_THICKNESS, &
    SUPP(IT)%FIRE_LINE_LENGTH/FIRE_LINE_THICKNESS, &
    SUPP(IT)%CONTAINMENT

IF (SUPP(IT)%CONTAINMENT .GE. 1.0) THEN
   RANK_FINISHED = 1
   STATS_FINAL_CONTAINMENT_FRAC(ICASE) = 1.0
   STATS_SIMULATION_TSTOP_HOURS(ICASE) = T / 3600.0
   TSTOP = T
   WRITE(*,*) "Fire Fully Suppressed!"
ENDIF

DEALLOCATE(L_SEG)
DEALLOCATE(L_REQ)
DEALLOCATE(SEG_IXCEN)
DEALLOCATE(SEG_IYCEN)
DEALLOCATE(SEG_DEG)
DEALLOCATE(SEG_SUPPRESSED)
DEALLOCATE(SEG_CANDIDATE)

! *****************************************************************************
END SUBROUTINE DIRECT_ATTACK
! *****************************************************************************

! *****************************************************************************
SUBROUTINE CALC_OTSU_PCL_THRESHOLD(NX, NY, PCL_R4, PCL_THRESHOLD_INTERNAL)
! *****************************************************************************

   IMPLICIT NONE

   INTEGER, INTENT(IN) :: NX, NY

   REAL, INTENT(IN)  :: PCL_R4(:,:,:)
   REAL, INTENT(INOUT) :: PCL_THRESHOLD_INTERNAL

   INTEGER, PARAMETER :: NBINS = 256

   INTEGER :: IX, IY, ICOL, IROW
   INTEGER :: IBIN, K
   INTEGER :: TOTAL_COUNT
   INTEGER :: THRESH_BIN

   INTEGER :: HIST(NBINS)

   REAL :: V
   REAL :: VMIN, VMAX
   REAL :: BIN_VALUE
   REAL :: SUM_TOTAL, SUM_BACK
   REAL :: W_BACK, W_FORE
   REAL :: MEAN_BACK, MEAN_FORE
   REAL :: VAR_BETWEEN, VAR_MAX

   !------------------------------------------------------------
   ! Find min and max PCL values in analysis domain
   ! Ignore zero values
   !------------------------------------------------------------

   VMIN = HUGE(1.0)
   VMAX = -HUGE(1.0)
   TOTAL_COUNT = 0

   DO IY = 1, NY
      DO IX = 1, NX

         ICOL = ICOL_ANALYSIS_F2C(IX)
         IROW = IROW_ANALYSIS_F2C(IY)

         V = PCL_R4(ICOL, IROW, 1)

         IF (V > 0.0) THEN
            VMIN = MIN(VMIN, V)
            VMAX = MAX(VMAX, V)
            TOTAL_COUNT = TOTAL_COUNT + 1
         END IF

      END DO
   END DO

   IF (TOTAL_COUNT == 0) THEN
      PCL_THRESHOLD_INTERNAL = 0.0
      RETURN
   END IF

   IF (VMAX <= VMIN) THEN
      PCL_THRESHOLD_INTERNAL = VMIN
      RETURN
   END IF

   !------------------------------------------------------------
   ! Build histogram
   !------------------------------------------------------------

   HIST = 0

   DO IY = 1, NY
      DO IX = 1, NX

         ICOL = ICOL_ANALYSIS_F2C(IX)
         IROW = IROW_ANALYSIS_F2C(IY)

         V = PCL_R4(ICOL, IROW, 1)

         IF (V > 0.0) THEN

            IBIN = INT( (V - VMIN) / (VMAX - VMIN) * REAL(NBINS - 1) ) + 1

            IBIN = MAX(1, MIN(NBINS, IBIN))

            HIST(IBIN) = HIST(IBIN) + 1

         END IF

      END DO
   END DO

   !------------------------------------------------------------
   ! Compute total weighted sum
   !------------------------------------------------------------

   SUM_TOTAL = 0.0

   DO K = 1, NBINS
      BIN_VALUE = VMIN + (REAL(K - 1) / REAL(NBINS - 1)) * (VMAX - VMIN)
      SUM_TOTAL = SUM_TOTAL + BIN_VALUE * REAL(HIST(K))
   END DO

   !------------------------------------------------------------
   ! Otsu threshold
   !------------------------------------------------------------

   SUM_BACK = 0.0
   W_BACK = 0.0
   VAR_MAX = -1.0
   THRESH_BIN = 1

   DO K = 1, NBINS

      BIN_VALUE = VMIN + (REAL(K - 1) / REAL(NBINS - 1)) * (VMAX - VMIN)

      W_BACK = W_BACK + REAL(HIST(K))

      IF (W_BACK <= 0.0) CYCLE

      W_FORE = REAL(TOTAL_COUNT) - W_BACK

      IF (W_FORE <= 0.0) EXIT

      SUM_BACK = SUM_BACK + BIN_VALUE * REAL(HIST(K))

      MEAN_BACK = SUM_BACK / W_BACK
      MEAN_FORE = (SUM_TOTAL - SUM_BACK) / W_FORE

      VAR_BETWEEN = W_BACK * W_FORE * (MEAN_BACK - MEAN_FORE)**2

      IF (VAR_BETWEEN > VAR_MAX) THEN
         VAR_MAX = VAR_BETWEEN
         THRESH_BIN = K
      END IF

   END DO

   PCL_THRESHOLD_INTERNAL = VMIN + (REAL(THRESH_BIN - 1) / REAL(NBINS - 1)) * (VMAX - VMIN)

! *****************************************************************************
END SUBROUTINE CALC_OTSU_PCL_THRESHOLD
! *****************************************************************************


! *****************************************************************************
SUBROUTINE CALCULATE_PCL_HOLD_MAP(NX, NY)
! *****************************************************************************
INTEGER, INTENT(IN)  :: NX, NY
INTEGER              :: ICOL,IROW, IX, IY, K, X0, Y0, XN, YN
INTEGER              :: HEAD, TAIL
INTEGER              :: COUNT_SEG
INTEGER              :: MAXCELLS
INTEGER              :: NSEG
INTEGER              :: NCAND

REAL :: SUM_SEG
REAL, ALLOCATABLE :: PCL_ORIG(:,:)

INTEGER, ALLOCATABLE :: QX(:), QY(:)
LOGICAL, ALLOCATABLE :: VISITED(:,:)

INTEGER, DIMENSION(8) :: DX = (/ -1, 0, 1, -1, 1, -1, 0, 1 /)
INTEGER, DIMENSION(8) :: DY = (/ -1,-1,-1,  0, 0,  1, 1, 1 /)

IF (PCL_THRESHOLD .GT. 100.0) THEN
   CALL CALC_OTSU_PCL_THRESHOLD(NX, NY, PCL%R4, PCL_THRESHOLD)
   WRITE(*,*) "Otsu thresholding on PCL ::::: the threshold is ", PCL_THRESHOLD
ELSE
   WRITE(*,*) "user-defined threshold on PCL ::::: the threshold is ", PCL_THRESHOLD
ENDIF

! thresholding the pcl
DO IY = 1, NY
   DO IX = 1, NX
      ICOL = ICOL_ANALYSIS_F2C(IX)
      IROW = IROW_ANALYSIS_F2C(IY)

      IF (PCL%R4(ICOL,IROW,1) .GE. PCL_THRESHOLD) PCL_HOLD_PROB(IX, IY) = PCL%R4(ICOL,IROW,1)
   ENDDO
ENDDO

! Segment PCL_HOLD_PROB and replace each segment by its mean
MAXCELLS = NX * NY
NCAND = COUNT(PCL_HOLD_PROB > 0.0)

ALLOCATE(PCL_ORIG(NX,NY))
ALLOCATE(PCL_MEAN_SEG(NCAND))
ALLOCATE(QX(MAXCELLS))
ALLOCATE(QY(MAXCELLS))
ALLOCATE(VISITED(NX,NY))

! Save original PCL values before overwriting with segment IDs
PCL_ORIG = PCL_HOLD_PROB

! Initialize outputs
PCL_HOLD_PROB = 0.0
PCL_MEAN_SEG = 0.0
VISITED = .FALSE.
NSEG = 0

DO IY = 1, NY
   DO IX = 1, NX

      ! Start a new segment from an unvisited positive PCL cell
      IF (PCL_ORIG(IX,IY) > 0.0 .AND. .NOT. VISITED(IX,IY)) THEN

         NSEG = NSEG + 1

         HEAD = 1
         TAIL = 1

         QX(TAIL) = IX
         QY(TAIL) = IY

         VISITED(IX,IY) = .TRUE.

         SUM_SEG = 0.0
         COUNT_SEG = 0

         !------------------------------------------------------
         ! Flood-fill this connected PCL segment
         !------------------------------------------------------
         DO WHILE (HEAD <= TAIL)

            X0 = QX(HEAD)
            Y0 = QY(HEAD)
            HEAD = HEAD + 1

            SUM_SEG = SUM_SEG + PCL_ORIG(X0,Y0)
            COUNT_SEG = COUNT_SEG + 1

            DO K = 1, 8

               XN = X0 + DX(K)
               YN = Y0 + DY(K)

               ! Skip outside domain
               IF (XN < 1 .OR. XN > NX) CYCLE
               IF (YN < 1 .OR. YN > NY) CYCLE

               ! Add connected positive PCL cell
               IF (PCL_ORIG(XN,YN) > 0.0 .AND. .NOT. VISITED(XN,YN)) THEN

                  TAIL = TAIL + 1
                  QX(TAIL) = XN
                  QY(TAIL) = YN

                  VISITED(XN,YN) = .TRUE.

               END IF

            END DO

         END DO

         !------------------------------------------------------
         ! Store mean PCL value for this segment
         !------------------------------------------------------
         PCL_MEAN_SEG(NSEG) = SUM_SEG / REAL(COUNT_SEG)

         !------------------------------------------------------
         ! Fill PCL_HOLD_PROB with the segment ID
         !------------------------------------------------------
         DO K = 1, TAIL
            PCL_HOLD_PROB(QX(K),QY(K)) = REAL(NSEG)
         END DO

      END IF

   END DO
END DO

DEALLOCATE(PCL_ORIG)
DEALLOCATE(QX)
DEALLOCATE(QY)
DEALLOCATE(VISITED)


! *****************************************************************************
END SUBROUTINE CALCULATE_PCL_HOLD_MAP
! *****************************************************************************


! *****************************************************************************
SUBROUTINE INDIRECT_ATTACK(NX, NY, T)
! *****************************************************************************
TYPE(NODE), POINTER :: C
REAL(8), INTENT(IN) :: T
INTEGER, INTENT(IN) :: NX, NY
INTEGER :: I
REAL :: R, F_FL


C => LIST_TAGGED%HEAD
DO I = 1, LIST_TAGGED%NUM_NODES

   IF (.NOT. C%FIRE_LINE) THEN
      C => C%NEXT
      CYCLE
   ENDIF

   IF (PCL_HOLD_PROB(C%IX, C%IY) .NE. 0.0) THEN

      CALL RANDOM_NUMBER(R)
      F_FL = 1.0 / (1.0 + C%FLAME_LENGTH / FL_MAX_DIRECT_ATTACK)
      R = R*100
      IF (R .LT. PCL_MEAN_SEG(NINT(PCL_HOLD_PROB(C%IX, C%IY)))*F_FL) THEN
         PCL_MEAN_SEG(NINT(PCL_HOLD_PROB(C%IX, C%IY))) = 100
         C%SUPPRESSION_ADJUSTMENT_FACTOR = 0
         C%TIME_SUPPRESSED = T
      ENDIF
      
   ENDIF

   C => C%NEXT

ENDDO

! *****************************************************************************
END SUBROUTINE INDIRECT_ATTACK
! *****************************************************************************


! *****************************************************************************
END MODULE ELMFIRE_SUPPRESSION
! *****************************************************************************
