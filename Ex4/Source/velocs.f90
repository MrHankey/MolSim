
! Code converted using TO_F90 by Alan Miller
! Date: 2013-11-27  Time: 15:47:27
 
SUBROUTINE velocs(temp)


!  Simple velocity scaling (only used during equilbration!!!!)

!   Temp (input) : target velocity
use parameter_mod
use conf
use veloc
IMPLICIT NONE

DOUBLE PRECISION, INTENT(IN OUT)         :: temp

DOUBLE PRECISION :: v2, f
INTEGER :: i


!     --rescale velocities
v2 = 0
DO i = 1, npart
  v2 = v2 + vx(i)*vx(i) + vy(i)*vy(i) + vz(i)*vz(i)
END DO
v2 = v2/(3*npart)
f = SQRT(temp/v2)
DO i = 1, npart
  vx(i) = vx(i)*f
  vy(i) = vy(i)*f
  vz(i) = vz(i)*f
END DO
RETURN
END SUBROUTINE velocs
