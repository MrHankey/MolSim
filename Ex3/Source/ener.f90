SUBROUTINE ener(en, vir, r2, ib)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 10:53:01

!  ---Calculates Energy (En) And Virial (Vir) For Given
!     Distance Squared Between (R2) Two Particles
use potential
IMPLICIT NONE

DOUBLE PRECISION, INTENT(OUT)            :: en
DOUBLE PRECISION, INTENT(OUT)            :: vir
DOUBLE PRECISION, INTENT(IN)             :: r2
INTEGER, INTENT(IN OUT)                  :: ib
DOUBLE PRECISION :: r2i, r6i


IF (r2 <= rc2(ib)) THEN
  r2i = sig2/r2
  r6i = r2i*r2i*r2i
  en  = eps4*(r6i*r6i-r6i)
  vir = eps48*(r6i*r6i-0.5D0*r6i)
ELSE
  en  = 0.d0
  vir = 0.d0
END IF

RETURN
END SUBROUTINE ener
