SUBROUTINE sample(i, en, vir, press)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 09:42:54
use parameter_mod
use conf
use system
IMPLICIT NONE

INTEGER, INTENT(IN OUT)                  :: i
DOUBLE PRECISION, INTENT(IN)             :: en
DOUBLE PRECISION, INTENT(IN)             :: vir
DOUBLE PRECISION, INTENT(OUT)            :: press

!      Write Quantities (Pressure And Energy) To File


!      Ener (Input) : Total Energy
!      Vir  (Input) : Total Virial





DOUBLE PRECISION :: enp, vol

IF (npart /= 0) THEN
  enp   = en/DBLE(npart)
  vol   = box**3
  press = (DBLE(npart)/vol)/beta + vir/(3.0D0*vol)
ELSE
  enp   = 0.0D0
  press = 0.0D0
END IF

WRITE (66, *) i, enp
WRITE (67, *) i, press

RETURN
END SUBROUTINE sample
