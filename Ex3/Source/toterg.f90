SUBROUTINE toterg(ener, vir, ib)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 10:53:53

!     ---Calculates Total Energy
use parameter_mod
use conf
IMPLICIT NONE
DOUBLE PRECISION, INTENT(OUT)            :: ener
DOUBLE PRECISION, INTENT(OUT)            :: vir
INTEGER, INTENT(IN)                      :: ib

DOUBLE PRECISION :: xi, yi, zi, eni, viri
INTEGER :: i, jb

ener = 0.0D0
vir = 0.0D0
DO i = 1, npart - 1
  IF (id(i) == ib) THEN
    xi = x(i)
    yi = y(i)
    zi = z(i)
    jb = i + 1
    CALL eneri(xi, yi, zi, i, jb, eni, viri, ib)
    ener = ener + eni
    vir = vir + viri
  END IF
END DO

RETURN
END SUBROUTINE toterg
