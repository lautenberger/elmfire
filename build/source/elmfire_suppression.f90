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

INTEGER, INTENT(IN) :: IT
REAL, INTENT(IN) :: T
INTEGER :: I, IDEG, J, COUNT, N_NONZERO
REAL :: DX, DY, CURRENT_CONTAINMENT, VELOCITY_SUM
REAL, DIMENSION (0:359) :: SUPPRESSED_FRACTION, FIRELINE_FRACTION, DEG, &
                           VELOCITY_SMOOTHED, VELOCITY0_SMOOTHED
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
      IDEG = NINT(ATAN2(DY,DX)*180.0/3.14159 - 90.0)
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

   I=-1
   DO WHILE (CURRENT_CONTAINMENT .LT. SUPP(IT)%TARGET_CONTAINMENT .AND. I .LT. 359)
      I = I + 1
      IDEG = INT(DEG(I))
      IF (SUPP(IT)%NCELLS(IDEG) .EQ. 0) CYCLE

      C => LIST_TAGGED%HEAD
      DO J = 1, LIST_TAGGED%NUM_NODES

         IF (C%SUPPRESSION_IDEG .NE. IDEG) THEN
            C => C%NEXT
            CYCLE
         ENDIF

         IF (C%TIME_SUPPRESSED .LT. 0.) THEN
            C%TIME_SUPPRESSED = T
            C%SUPPRESSION_ADJUSTMENT_FACTOR = 0.0
         ENDIF
         C => C%NEXT
      ENDDO

      CURRENT_CONTAINMENT = CURRENT_CONTAINMENT + (1. - SUPP(IT)%SUPPRESSED_FRACTION(IDEG)) * SUPP(IT)%FIRELINE_FRACTION(IDEG)
      SUPP(IT)%SUPPRESSED_FRACTION(IDEG) = 1.0

   ENDDO
ENDIF

CONTINUE

! *****************************************************************************
END SUBROUTINE CONTAINMENT
! *****************************************************************************

! *****************************************************************************
END MODULE ELMFIRE_SUPPRESSION
! *****************************************************************************
