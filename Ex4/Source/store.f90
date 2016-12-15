SUBROUTINE store(iout)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-11-27  Time: 15:47:07

!     writes configuration to disk

!  Iout (input) file number
use parameter_mod
use conf
use system
use veloc
IMPLICIT NONE

INTEGER, INTENT(IN OUT)                  :: iout

INTEGER :: i

WRITE (iout, *) box, hbox
WRITE (iout, *) npart

DO i = 1, npart
  WRITE (iout, *) x(i), y(i), z(i), vx(i), vy(i), vz(i)
END DO
REWIND (iout)
RETURN
END SUBROUTINE store
