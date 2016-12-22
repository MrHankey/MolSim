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

     dx = dx - NINT(dx/box)*box
     dy = dy - NINT(dy/box)*box
     dz = dz - NINT(dz/box)*box

!    IF (dx > hbox) THEN
!        dx = dx - box
!    ELSE
!        IF (dx < -hbox) dx = dx + box
!    END IF
!
!    IF (dy > hbox) THEN
!        dy = dy - box
!    ELSE
!        IF (dy < -hbox) dy = dy + box
!    END IF

!    IF (dz > hbox) THEN
!        dz = dz - box
!    ELSE
!        IF (dz < -hbox) dz = dz + box
!    END IF
    
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
