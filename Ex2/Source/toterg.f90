SUBROUTINE toterg(ener, vir)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 09:43:04

!     Calculates Total Energy Of The System
!     Only Used In The Beginning Or At The End Of The Program

!     Ener (Output) : Total Energy
!     Vir  (Output) : Total Virial
use parameter_mod
use conf
IMPLICIT NONE

DOUBLE PRECISION, INTENT(OUT)            :: ener
DOUBLE PRECISION, INTENT(OUT)            :: vir

DOUBLE PRECISION :: xi, yi, zi, eni, viri
INTEGER :: i, jb

ener = 0.0D0
vir  = 0.0D0

DO i = 1, npart - 1
  xi = x(i)
  yi = y(i)
  zi = z(i)
  jb = i + 1
  
  CALL eneri(xi, yi, zi, i, jb, eni, viri)
  
  ener = ener + eni
  vir  = vir + viri
END DO

RETURN
END SUBROUTINE toterg
