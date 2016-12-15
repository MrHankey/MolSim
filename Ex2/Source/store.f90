SUBROUTINE store(iout, dr)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 09:42:56

!     Writes Configuration To Disk

!     Iout (Input) File Number
!     Dr   (Input) Maximum Displacement
use parameter_mod
use conf
use system
IMPLICIT NONE

INTEGER, INTENT(IN OUT)                  :: iout
DOUBLE PRECISION, INTENT(IN OUT)         :: dr

INTEGER :: i


WRITE (iout, *) box, hbox
WRITE (iout, *) npart
WRITE (iout, *) dr

DO i = 1, npart
  WRITE (iout, *) x(i), y(i), z(i)
END DO

REWIND (iout)

RETURN
END SUBROUTINE store
