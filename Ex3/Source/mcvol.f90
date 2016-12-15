SUBROUTINE mcvol(en, vir, attempt, acc, vmax)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 10:53:16

!     Attempts To Change The Volume
use parameter_mod
use conf
use potential
use system 
IMPLICIT NONE

DOUBLE PRECISION, INTENT(IN OUT)         :: en(*)
DOUBLE PRECISION, INTENT(OUT)            :: vir(*)
INTEGER, INTENT(OUT)                     :: attempt
INTEGER, INTENT(OUT)                     :: acc
DOUBLE PRECISION, INTENT(IN)             :: vmax


DOUBLE PRECISION :: enn(2), virn(2), yy, f(2), arg, volo(2), volt, dlnv,  &
    voln(2), dele1, dele2, dlnv1, dlnv2, enold,ran_uniform

INTEGER :: i, ib, idi

attempt = attempt + 1

!     ---Calulate New Volume By Making Random Walk In Ln V

volo(1) = box(1)**3
volo(2) = box(2)**3
volt = volo(1) + volo(2)
dlnv = LOG(volo(1)/volo(2)) + (ran_uniform()-0.5D0)*vmax
voln(1) = EXP(dlnv)*volt/(1.0D0+EXP(dlnv))
voln(2) = volt - voln(1)

DO ib = 1, 2
  box(ib) = voln(ib)**(1.d0/3.d0)
  f(ib) = box(ib)/volo(ib)**(1D0/3D0)
  hbox(ib) = box(ib)/2
  rc(ib) = f(ib)*rc(ib)
  rc2(ib) = rc(ib)**2
END DO

!     ---Determine New Coordinates

DO i = 1, npart
  idi = id(i)
  x(i) = f(idi)*x(i)
  y(i) = f(idi)*y(i)
  z(i) = f(idi)*z(i)
END DO

!        ---Calculate New Energy Using Scaling

DO ib = 1, 2
  enold = en(ib)
  yy = (volo(ib)/voln(ib))**2
  enn(ib) = enold*yy*(2.0D0-yy) - vir(ib)*yy*(1.0D0-yy)/6.0D0
  virn(ib) = -12.0D0*enold*yy*(yy-1.0D0) + vir(ib)*yy*(2.0D0*yy-1.0D0)
  
END DO

!     ---Acceptance:

dele1 = enn(1) - en(1)
dele2 = enn(2) - en(2)
dlnv1 = LOG(voln(1)/volo(1))
dlnv2 = LOG(voln(2)/volo(2))
arg = EXP(-beta*(dele1+dele2-DBLE((npbox(1)+1))*  &
    dlnv1/beta-DBLE((npbox(2)+1)) *dlnv2/beta))
IF (ran_uniform() < arg) THEN
  
!        ---Accepted
  
  acc = acc + 1
  DO ib = 1, 2
    en(ib) = enn(ib)
    vir(ib) = virn(ib)
  END DO
ELSE
  
!        ---Restore The Old Configuration
  
  DO ib = 1, 2
    f(ib) = 1/f(ib)
    box(ib) = box(ib)*f(ib)
    hbox(ib) = 0.5D0*box(ib)
    rc(ib) = f(ib)*rc(ib)
    rc2(ib) = rc(ib)**2
  END DO
  DO i = 1, npart
    idi = id(i)
    x(i) = f(idi)*x(i)
    y(i) = f(idi)*y(i)
    z(i) = f(idi)*z(i)
  END DO
END IF

RETURN
END SUBROUTINE mcvol
