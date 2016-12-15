SUBROUTINE solve(fx, fy, fz, enkin, delt)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-11-27  Time: 15:47:04

!   Solve the equations of motion

!  Fx    (input) array: x component of the force acting on the particles
!  Fy    (input) array: y component of the force acting on the particles
!  Fz    (input) array: z component of the force acting on the particles
!  Enkin (ouput)      : total kinetic energy
!  Delt  (input)      : time step MD simulation
use parameter_mod
use conf
use veloc
IMPLICIT NONE

DOUBLE PRECISION, INTENT(IN OUT)         :: fx(*)
DOUBLE PRECISION, INTENT(IN OUT)         :: fy(*)
DOUBLE PRECISION, INTENT(IN OUT)         :: fz(*)
DOUBLE PRECISION, INTENT(OUT)            :: enkin
DOUBLE PRECISION, INTENT(IN OUT)         :: delt

DOUBLE PRECISION :: v2, vxt, vyt, vzt
INTEGER :: i

v2 = 0.d0
!     ===solve equations of motion
DO i = 1, npart
  
!-------Start modification: verlet and predictor-corrector integration methods
!   Do not forget to update the square of the velocity v2 to calculate the kinetic energy
  
  IF(verlet) THEN
  ELSE
!         For the predictor-corrector method, the variable storing the derivative of the
!         force, or the term x3, should be defined in the program, and calculated in the
!         force routine, or better a new routine
  END IF
  
!-------End modification
  
END DO
enkin = v2/2
RETURN
END SUBROUTINE solve
