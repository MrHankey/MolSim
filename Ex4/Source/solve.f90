MODULE solve_mod
use parameter_mod, only: npmax
use parameter_mod
use conf
use veloc
IMPLICIT NONE
private
public :: solve

DOUBLE PRECISION :: fx_old(npmax) = 0
DOUBLE PRECISION :: fy_old(npmax) = 0
DOUBLE PRECISION :: fz_old(npmax) = 0


CONTAINS
  SUBROUTINE solve(fx, fy, fz, enkin, delt, enpot, vir)
  
  ! Code converted using TO_F90 by Alan Miller
  ! Date: 2013-11-27  Time: 15:47:04

  !   Solve the equations of motion

  !  Fx    (input) array: x component of the force acting on the particles
  !  Fy    (input) array: y component of the force acting on the particles
  !  Fz    (input) array: z component of the force acting on the particles
  !  Enkin (ouput)      : total kinetic energy
  !  Delt  (input)      : time step MD simulation

  DOUBLE PRECISION, INTENT(IN OUT)         :: fx(npmax)
  DOUBLE PRECISION, INTENT(IN OUT)         :: fy(npmax)
  DOUBLE PRECISION, INTENT(IN OUT)         :: fz(npmax)
  DOUBLE PRECISION, INTENT(OUT)            :: enkin
  DOUBLE PRECISION, INTENT(IN OUT)         :: delt
  DOUBLE PRECISION, INTENT(OUT)            :: enpot
  DOUBLE PRECISION, INTENT(OUT)            :: vir

  DOUBLE PRECISION :: v2, vxt, vyt, vzt
  INTEGER :: i

  fx_old = fx
  fy_old = fy
  fz_old = fz

  v2 = 0.d0
  !     ===solve equations of motion
  DO i = 1, npart
    
  !-------Start modification: verlet and predictor-corrector integration methods
  !   Do not forget to update the square of the velocity v2 to calculate the kinetic energy
    
    IF(verlet) THEN
      ! update positions
      x(i) = x(i) + vx(i)*delt + fx(i)/2.D0*delt**2
      y(i) = y(i) + vy(i)*delt + fy(i)/2.D0*delt**2
      z(i) = z(i) + vz(i)*delt + fz(i)/2.D0*delt**2
    ELSE
  !         For the predictor-corrector method, the variable storing the derivative of the
  !         force, or the term x3, should be defined in the program, and calculated in the
  !         force routine, or better a new routine
    END IF
    
  !-------End modification
    
  END DO

  ! update forces
  CALL force(fx, fy, fz, enpot, vir)

  DO i = 1, npart
    
  !-------Start modification: verlet and predictor-corrector integration methods
  !   Do not forget to update the square of the velocity v2 to calculate the kinetic energy
    
    IF(verlet) THEN
      ! update velocities
      vx(i) = vx(i) + ( fx(i) + fx_old(i) ) / 2.D0 * delt
      vy(i) = vy(i) + ( fy(i) + fy_old(i) ) / 2.D0 * delt
      vz(i) = vz(i) + ( fz(i) + fz_old(i) ) / 2.D0 * delt

      ! update squared velocity
      v2 = v2 + vx(i)**2 + vy(i)**2 + vz(i)**2
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

END MODULE solve_mod