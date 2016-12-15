SUBROUTINE lattice

!     place `npart' particles on a simple cubic
!     lattice with density 'rho'
use parameter_mod
use conf
use system

IMPLICIT NONE
INTEGER :: i, j, k, itel, n
DOUBLE PRECISION :: dx, dy, dz, del

n = INT(npart**(1./3.)) + 1
IF (n == 0) n = 1
del = box/DBLE(n)
itel = 0
dx = -del
DO i = 1, n
  dx = dx + del
  dy = -del
  DO j = 1, n
    dy = dy + del
    dz = -del
    DO k = 1, n
      dz = dz + del
      IF (itel < npart) THEN
        itel = itel + 1
        x(itel) = dx
        y(itel) = dy
        z(itel) = dz
      END IF
    END DO
  END DO
END DO
WRITE (6, 99001) itel
RETURN
99001 FORMAT (' Initialisation on lattice: ', /, i10,  &
    ' particles placed on a lattice')
END SUBROUTINE lattice
