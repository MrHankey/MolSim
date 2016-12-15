SUBROUTINE eneri(xi, yi, zi, i, jb, en, vir, ib)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 10:53:03
use parameter_mod
use conf
use system
IMPLICIT NONE

DOUBLE PRECISION, INTENT(IN)             :: xi
DOUBLE PRECISION, INTENT(IN)             :: yi
DOUBLE PRECISION, INTENT(IN)             :: zi
INTEGER, INTENT(IN)                      :: i
INTEGER, INTENT(IN)                      :: jb
DOUBLE PRECISION, INTENT(OUT)            :: en
DOUBLE PRECISION, INTENT(OUT)            :: vir
INTEGER, INTENT(IN)                      :: ib

DOUBLE PRECISION :: dx, dy, dz, r2, virij, enij
INTEGER :: j

en = 0.d0
vir = 0.d0
DO j = jb, npart
  IF (id(j) == ib) THEN
    IF (j /= i) THEN
      dx = xi - x(j)
      dy = yi - y(j)
      dz = zi - z(j)
      IF (dx > hbox(ib)) THEN
        dx = dx - box(ib)
      ELSE
        IF (dx < -hbox(ib)) dx = dx + box(ib)
      END IF
      IF (dy > hbox(ib)) THEN
        dy = dy - box(ib)
      ELSE
        IF (dy < -hbox(ib)) dy = dy + box(ib)
      END IF
      IF (dz > hbox(ib)) THEN
        dz = dz - box(ib)
      ELSE
        IF (dz < -hbox(ib)) dz = dz + box(ib)
      END IF
      r2 = dx*dx + dy*dy + dz*dz
      CALL ener(enij, virij, r2, ib)
      en = en + enij
      vir = vir + virij
    END IF
  END IF
END DO

RETURN
END SUBROUTINE eneri
