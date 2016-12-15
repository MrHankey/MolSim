SUBROUTINE mcmove(en, vir, attempt, nacc, dr)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 10:53:13

!     Attempts To Displace A Randomly Selected Particle
use parameter_mod
use conf
use system 
implicit none

DOUBLE PRECISION, INTENT(OUT)            :: en(*)
DOUBLE PRECISION, INTENT(OUT)            :: vir(*)
INTEGER, INTENT(OUT)                     :: attempt
INTEGER, INTENT(OUT)                     :: nacc
DOUBLE PRECISION, INTENT(IN)             :: dr


DOUBLE PRECISION :: enn, eno, xn, yn, zn, viro, virn,  ran_uniform


INTEGER :: o, jb, ido

attempt = attempt + 1
jb = 1

!     ---Select A Particle At Random

o = INT(DBLE(npart)*ran_uniform()) + 1
ido = id(o)

!     ---Calculate Energy Old Configuration

CALL eneri(x(o), y(o), z(o), o, jb, eno, viro, ido)

!     ---Give Particle A Random Displacement

xn = x(o) + (ran_uniform()-0.5D0)*dr
yn = y(o) + (ran_uniform()-0.5D0)*dr
zn = z(o) + (ran_uniform()-0.5D0)*dr

!     ---Calculate Energy New Configuration:

CALL eneri(xn, yn, zn, o, jb, enn, virn, ido)

!     ---Acceptance Test

IF (ran_uniform() < EXP(-beta*(enn-eno))) THEN
  
!        --Accepted
  
  nacc     = nacc     + 1
  en(ido)  = en(ido)  + (enn-eno)
  vir(ido) = vir(ido) + (virn-viro)
  
!        ---Put Particle In Simulation Box
  
  IF (xn < 0)        xn = xn + box(ido)
  IF (xn > box(ido)) xn = xn - box(ido)
  IF (yn < 0)        yn = yn + box(ido)
  IF (yn > box(ido)) yn = yn - box(ido)
  IF (zn < 0)        zn = zn + box(ido)
  IF (zn > box(ido)) zn = zn - box(ido)
  x(o) = xn
  y(o) = yn
  z(o) = zn
END IF
RETURN
END SUBROUTINE mcmove
