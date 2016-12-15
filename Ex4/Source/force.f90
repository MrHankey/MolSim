SUBROUTINE force(fx, fy, fz, en, vir)

!  Calculate the force acting on the particles

!  Fx  (output) array: x component of the force acting on the particles
!  Fy  (output) array: y component of the force acting on the particles
!  Fz  (output) array: z component of the force acting on the particles
!  En  (output)      : total energy
!  Vir (output)      : total virial
use parameter_mod
use conf
use potential
use system

IMPLICIT NONE


DOUBLE PRECISION, INTENT(OUT)            :: fx(*)
DOUBLE PRECISION, INTENT(OUT)            :: fy(*)
DOUBLE PRECISION, INTENT(OUT)            :: fz(*)
DOUBLE PRECISION, INTENT(OUT)            :: en
DOUBLE PRECISION, INTENT(OUT)            :: vir

DOUBLE PRECISION :: xi, yi, zi, dx, dy, dz, r2, virij, enij, fr, r2i, r6i
INTEGER :: i, j


en = 0.d0
vir = 0.d0
DO i = 1, npart
  fx(i) = 0
  fy(i) = 0
  fz(i) = 0
END DO
DO i = 1, npart - 1
  xi = x(i)
  yi = y(i)
  zi = z(i)
  DO j = i + 1, npart
    dx = xi - x(j)
    dy = yi - y(j)
    dz = zi - z(j)
!           ---periodic boundary conditions
    dx = dx - box*ANINT(dx/box)
    dy = dy - box*ANINT(dy/box)
    dz = dz - box*ANINT(dz/box)
    r2 = dx*dx + dy*dy + dz*dz
    IF (r2 <= rc2) THEN
      r2i = 1/r2
      r6i = r2i*r2i*r2i
      enij = 4*(r6i*r6i-r6i) - ecut
      virij = 48*(r6i*r6i-0.5D0*r6i)
      en = en + enij
      vir = vir + virij
      fr = virij*r2i
      fx(i) = fx(i) + fr*dx
      fy(i) = fy(i) + fr*dy
      fz(i) = fz(i) + fr*dz
      fx(j) = fx(j) - fr*dx
      fy(j) = fy(j) - fr*dy
      fz(j) = fz(j) - fr*dz
      
!-----------Start modification
      
!    Here the derivative of the force should be calculated, in order
!    to integrate with the predictor-corrector method
      
!-----------End modification
    END IF
  END DO
END DO
RETURN
END SUBROUTINE force
