SUBROUTINE setvel(temp, iseed, vx0t, vy0t, vz0t)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-11-27  Time: 15:46:59

!   Give particles an radom initial velocity

!   Temp (input)  : requested temperature
!   Iseed (input) : seed random number generator
!                   (not used in present implementation)
!   Vx0t (output) : x component veloocity center of mass
!   Vy0t (output) : y component veloocity center of mass
!   Vz0t (output) : z component veloocity center of mass

use parameter_mod
use conf
use veloc
IMPLICIT NONE

DOUBLE PRECISION, INTENT(IN OUT)         :: temp
INTEGER, INTENT(IN OUT)                  :: iseed
DOUBLE PRECISION, INTENT(OUT)            :: vx0t
DOUBLE PRECISION, INTENT(OUT)            :: vy0t
DOUBLE PRECISION, INTENT(OUT)            :: vz0t

INTEGER :: i
DOUBLE PRECISION :: v2, vx0, vy0, vz0, f
Double precision, external :: ranf

!     ===give particle a velocity
!     ===velocity
vx0 = 0.d0
vy0 = 0.d0
vz0 = 0.d0
v2 = 0.d0
DO i = 1, npart
  vx(i) = ranf(iseed) - 0.5D0
  vy(i) = ranf(iseed) - 0.5D0
  vz(i) = ranf(iseed) - 0.5D0
  vx0 = vx0 + vx(i)
  vy0 = vy0 + vy(i)
  vz0 = vz0 + vz(i)
  v2 = v2 + vx(i)**2 + vy(i)**2 + vz(i)**2
END DO
!     ===set centre of mass movement to zero
vx0 = vx0/npart
vy0 = vy0/npart
vz0 = vz0/npart
vx0t = 0.d0
vy0t = 0.d0
vz0t = 0.d0
f = SQRT(3*npart*temp/v2)
v2 = 0.d0
DO i = 1, npart
  vx(i) = (vx(i)-vx0)*f
  vy(i) = (vy(i)-vy0)*f
  vz(i) = (vz(i)-vz0)*f
  vx0t = vx0t + vx(i)
  vy0t = vy0t + vy(i)
  vz0t = vz0t + vz(i)
  v2 = v2 + vx(i)**2 + vy(i)**2 + vz(i)**2
END DO
v2 = v2/DBLE(3*npart)
vx0t = vx0t/npart
vy0t = vy0t/npart
vz0t = vz0t/npart
temp = v2
WRITE (6, 99001) v2
WRITE (6, 99002) vx0t, vy0t, vz0t
RETURN
99001 FORMAT (' Initial temperature     : ', f6.3)
99002 FORMAT (' Velocity centre of mass : ', /, '          x = ', e9.2,  &
    /, '          y = ', e9.2, /, '          z = ', e9.2)
END SUBROUTINE setvel
