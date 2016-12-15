SUBROUTINE eneri(xi, yi, zi, i, jb, en, vir)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 09:42:10

!    Calculates The Energy Of Particle I With Particles J=Jb,Npart

!     Xi (Input)    X Coordinate Particle I
!     Yi (Input)    Y Coordinate Particle I
!     Zi (Input)    Z Coordinate Particle I
!     I  (Input)    Particle Number (Excluded !!!)
!     En  (Output)  Energy Particle I
!     Vir (Output)  Virial Particle I
use parameter_mod
use potential
use conf
use system, only: box, hbox
IMPLICIT NONE

DOUBLE PRECISION, INTENT(IN)             :: xi
DOUBLE PRECISION, INTENT(IN)             :: yi
DOUBLE PRECISION, INTENT(IN)             :: zi
INTEGER, INTENT(IN)                      :: i
INTEGER, INTENT(IN)                      :: jb
DOUBLE PRECISION, INTENT(OUT)            :: en
DOUBLE PRECISION, INTENT(OUT)            :: vir


DOUBLE PRECISION :: dx, dy, dz, r2, virij, enij
INTEGER :: j

en  = 0.0D0
vir = 0.0D0

DO j = jb, npart
  
!cccccccccccccccccccccccccc
!     Excluse Particle I  C
!cccccccccccccccccccccccccc
  
  IF (j /= i) THEN
    
    dx = xi - x(j)
    dy = yi - y(j)
    dz = zi - z(j)
    
!cccccccccccccccccccccccccccccccccc
!     Nearest Image Convention    C
!cccccccccccccccccccccccccccccccccc

!cccccccccccccccccccccccccccccccccc
!     Start modification          C
!cccccccccccccccccccccccccccccccccc    
    
    IF (abs(dx) > Box/2) dx = dx - Box*nint(dx/Box)
   
   IF (abs(dy) > Box/2) dy = dy - Box*nint(dy/Box)
   
   IF (abs(dz) > Box/2) dz = dz - Box*nint(dz/Box)
    
!cccccccccccccccccccccccccccccccccc
!     End modification          C
!cccccccccccccccccccccccccccccccccc
    
    r2 = dx*dx + dy*dy + dz*dz
    
!ccccccccccccccccccccccccccccc
!     Calculate The Energy   C
!ccccccccccccccccccccccccccccc
    
    CALL ener(enij, virij, r2)
    
    en  = en + enij
    vir = vir + virij
  END IF
END DO

RETURN
END SUBROUTINE eneri
