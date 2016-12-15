SUBROUTINE ener(en, vir, r2)


!     Calculate Energy Between A (Single) Pair Of Atoms

!     En : (Output) Energy
!     Vir: (Output) Virial
!     R2 : (Input) Distance Squared Between Two Particles
 use potential
 IMPLICIT NONE

  DOUBLE PRECISION, INTENT(OUT)            :: en
  DOUBLE PRECISION, INTENT(OUT)            :: vir
  DOUBLE PRECISION, INTENT(IN)             :: r2
  DOUBLE PRECISION :: r2i, r6i




  en = 0.0D0
  vir = 0.0D0

  IF (r2 < rc2) THEN
    r2i = sig2/r2
    r6i = r2i*r2i*r2i
    en  = eps4*(r6i*r6i-r6i)
  !    SHIFT LJ to remove discontinuity
  !  en = en - eps4*((sig2/rc2)**6 - (sig2/rc2)**3)
    
  !     Start Modification Virial
  
    vir = - eps4*12.0D0*(r6i*r6i - 0.5D0*r6i)
    
  !     End   Modification Virial
  
  END IF

  RETURN
END SUBROUTINE ener
