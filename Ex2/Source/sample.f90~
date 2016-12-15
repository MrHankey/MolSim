SUBROUTINE sample(i, en, vir, press)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 09:42:54
use parameter_mod
use potential
use conf
use system
IMPLICIT NONE

INTEGER, INTENT(IN OUT)                  :: i
DOUBLE PRECISION, INTENT(IN)             :: en
DOUBLE PRECISION, INTENT(IN)             :: vir
DOUBLE PRECISION, INTENT(OUT)            :: press
DOUBLE PRECISION                         :: p_id

!      Write Quantities (Pressure And Energy) To File


!      Ener (Input) : Total Energy
!      Vir  (Input) : Total Virial





DOUBLE PRECISION :: enp, vol

IF (npart /= 0) THEN
  enp   = en/DBLE(npart)
  vol   = box**3
  press = (DBLE(npart)/vol)/beta + vir/(3.0D0*vol)
  p_id = DBLE(npart) * 1.38064852E-23 * DBLE(temp) / DBLE(vol)
ELSE
  enp   = 0.0D0
  press = 0.0D0
  p_id = 0.0D0
END IF

!WRITE (66, *) i, enp
!WRITE (67, *) i, press
WRITE (66, *) i, p_id
WRITE (67, *) i, press

RETURN
END SUBROUTINE sample
