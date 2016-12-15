SUBROUTINE lattice
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 10:53:10
use parameter_mod
use conf
use system 
IMPLICIT NONE

!     ---Place `Npart' Particles On A Lattice With Density 'Rho'
!      --Half The Number In Box 1 And The Other Half In Box 2

INTEGER :: i, j, k, itel, n, ib
DOUBLE PRECISION :: del

del      = (box(1)**3)**(1.d0/3.d0)
npbox(1) = npart/2
npbox(2) = npbox(1)

IF (npbox(1)+npbox(2) /= npart) THEN
  STOP 'Error Npart'
END IF

WRITE (6, *) ' Generate Simple Cubic Lattice'

n = INT(DBLE(npart)**(1.d0/3.d0)) + 1
IF (n == 0) n = 1
del = del/DBLE(n)
itel = 0
DO i = 0, n - 1
  DO j = 0, n - 1
    DO k = 0, n - 1
      DO ib = 1, 2
        IF (itel < npart) THEN
          itel = itel + 1
          x(itel) = DBLE(k)*del
          y(itel) = DBLE(j)*del
          z(itel) = DBLE(i)*del
          id(itel) = ib
        END IF
      END DO
    END DO
  END DO
END DO

WRITE (6, 99001) itel

99001 FORMAT (' Initialisation On Lattice: ', /, i10,  &
    ' Particles Placed On A Lattice')

RETURN
END SUBROUTINE lattice
