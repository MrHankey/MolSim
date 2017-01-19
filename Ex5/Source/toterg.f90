SUBROUTINE toterg(ener, vir, enk)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-11-27  Time: 15:47:19

!     calculates total energy

!  Ener (output) : total (potential + kinetic) energy
!  Vir  (output) : total virial
!  Enk  (ouput)  : total kinetic energy
use parameter_mod
use conf
use veloc
IMPLICIT NONE
DOUBLE PRECISION, INTENT(OUT)            :: ener
DOUBLE PRECISION, INTENT(OUT)            :: vir
DOUBLE PRECISION, INTENT(OUT)            :: enk

DOUBLE PRECISION :: xi, yi, zi, eni, viri
INTEGER :: i, jb

ener = 0
vir = 0
enk = 0
DO i = 1, npart
  xi = x(i)
  yi = y(i)
  zi = z(i)
  jb = i + 1
  CALL eneri(xi, yi, zi, i, jb, eni, viri)
  ener = ener + eni
  vir = vir + viri
  enk = enk + (vx(i)*vx(i)+vy(i)*vy(i)+vz(i)*vz(i))
END DO
!     --kinetic energy
enk = 0.5D0*enk
ener = ener + enk
RETURN
END SUBROUTINE toterg
