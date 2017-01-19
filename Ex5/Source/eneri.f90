SUBROUTINE eneri(xi, yi, zi, i, jb, en, vir)

!    calculates the energy of particle I with particles j=jb,npart

!  Xi (input) x coordinate particle I
!  Yi (input) y coordinate particle I
!  Zi (input) z coordinate particle I
!  I  (input) particle number
!  Jb (input) = 0 calculates energy particle I with all other particle
!             = jb calculates energy particle I with all particles j > jb
!  En  (output) energy particle i
!  Vir (output) virial particle i

use parameter_mod
use conf
use potential
use system

IMPLICIT NONE
DOUBLE PRECISION, INTENT(IN)             :: xi
DOUBLE PRECISION, INTENT(IN)             :: yi
DOUBLE PRECISION, INTENT(IN)             :: zi
INTEGER, INTENT(IN)                      :: i
INTEGER, INTENT(IN)                      :: jb
DOUBLE PRECISION, INTENT(OUT)            :: en
DOUBLE PRECISION, INTENT(OUT)            :: vir


DOUBLE PRECISION :: dx, dy, dz, r2, virij, enij, r2i, r6i

INTEGER :: j

en = 0.d0
vir = 0.d0
DO j = jb, npart
  IF (j /= i) THEN
    dx = xi - x(j)
    dy = yi - y(j)
    dz = zi - z(j)
!           ---periodic boundary conditions
    dx = dx - box*ANINT(dx/box)
    dy = dy - box*ANINT(dy/box)
    dz = dz - box*ANINT(dz/box)
    r2 = dx*dx + dy*dy + dz*dz
    IF (r2 <= rc2) THEN
      r2i = 1/r2
      r6i = r2i*r2i*r2i
      enij = 4*(r6i*r6i-r6i) - ecut
      virij = 48*(r6i*r6i-0.5D0*r6i)
      en = en + enij
      vir = vir + virij
    END IF
  END IF
END DO
RETURN
END SUBROUTINE eneri
