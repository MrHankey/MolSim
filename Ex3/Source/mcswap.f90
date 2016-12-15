SUBROUTINE mcswap(en, vir, attempt, acc)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 10:53:14
use parameter_mod
use conf
use system 
use chem
IMPLICIT NONE

DOUBLE PRECISION, INTENT(OUT)            :: en(*)
DOUBLE PRECISION, INTENT(OUT)            :: vir(*)
INTEGER, INTENT(OUT)                     :: attempt
INTEGER, INTENT(OUT)                     :: acc

!     ---Exchange A Particle Bewteen The Two Boxes

DOUBLE PRECISION :: xn, yn, zn, enn, virn, eno, viro, arg, vola, vold,  &
    xo, yo, zo, dele,ran_uniform

INTEGER :: o, iadd, idel, jb, idi



attempt = attempt + 1

!     ===Select A Box At Random

IF (ran_uniform() < 0.5D0) THEN
  iadd = 1
  idel = 2
ELSE
  iadd = 2
  idel = 1
END IF

vola = box(iadd)**3
vold = box(idel)**3

!     ---Add A Particle To Box Iadd

xn = box(iadd)*ran_uniform()
yn = box(iadd)*ran_uniform()
zn = box(iadd)*ran_uniform()

!     ---Calculate Energy Of This Particle

jb = 1
o  = npart + 1

CALL eneri(xn, yn, zn, o, jb, enn, virn, iadd)

!     ---Calculate Contibution To The Chemical Potential:

arg = -beta*enn
chp(iadd) = chp(iadd) + vola*EXP(arg)/DBLE(npbox(iadd)+1)
IF (npbox(iadd) == npart) chp(iadd) = chp(iadd) + vola*EXP(arg)  &
    /DBLE(npbox(iadd)+1)
ichp(iadd) = ichp(iadd) + 1


!     ---Delete Particle From Box B:

IF (npbox(idel) == 0) THEN
  RETURN
END IF
idi = 0
DO WHILE (idi /= idel)
  o = INT(DBLE(npart)*ran_uniform()) + 1
  idi = id(o)
END DO
xo = x(o)
yo = y(o)
zo = z(o)
CALL eneri(xo, yo, zo, o, jb, eno, viro, idel)

!     ---Acceptence Test:

dele = enn - eno + DLOG(vold*DBLE((npbox(iadd)+1))/  &
    (vola*DBLE(npbox(idel)))) /beta

IF (ran_uniform() < EXP(-beta*dele)) THEN
  
!        ---Accepted:
  
  acc = acc + 1
  npbox(iadd) = npbox(iadd) + 1
  x(o) = xn
  y(o) = yn
  z(o) = zn
  id(o) = iadd
  en(iadd) = en(iadd) + enn
  vir(iadd) = vir(iadd) + virn
  npbox(idel) = npbox(idel) - 1
  en(idel) = en(idel) - eno
  vir(idel) = vir(idel) - viro
END IF
RETURN
END SUBROUTINE mcswap
