SUBROUTINE lattice
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 09:42:19

!     Place `Npart' Particles On A Simple Cubic
!     Lattice With Density 'Rho'
use parameter_mod
use conf
use system
IMPLICIT NONE

INTEGER :: i, j, k, itel, n
DOUBLE PRECISION :: dx, dy, dz, del

n = INT(npart**(1.0D0/3.0D0)) + 1
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
99001 FORMAT (' Initialisation On Lattice: ', /, i10,  &
    ' Particles Placed On A Lattice')
END SUBROUTINE lattice
