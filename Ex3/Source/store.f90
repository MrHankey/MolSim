SUBROUTINE store(iout, dr, vmax)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 10:53:50

!     Writes Configuration To Disk
use parameter_mod
use conf
use system 
IMPLICIT NONE

INTEGER, INTENT(IN OUT)                  :: iout
DOUBLE PRECISION, INTENT(IN OUT)         :: dr
DOUBLE PRECISION, INTENT(IN OUT)         :: vmax

INTEGER :: i


WRITE (iout, *) box(1), hbox(1), box(2), hbox(2)
WRITE (iout, *) npart, npbox(1), npbox(2)
WRITE (iout, *) dr, vmax

DO i = 1, npart
  WRITE (iout, *) x(i), y(i), z(i), id(i)
END DO
REWIND (iout)

RETURN
END SUBROUTINE store
