SUBROUTINE mcmove(en, vir, attempt, nacc, dr)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 09:42:25


!     Attempts To Displace A Randomly Selected Particle


!     Ener   (Input/Output) : Total Energy
!     Vir    (Input/Output) : Total Virial
!     Attemp (Input/Output) : Number Of Attemps That Have Been
!                             Performed To Displace A Particle
!     Nacc   (Input/Output) : Number Of Successful Attemps
!                             To Displace A Particle
!     Dr     (Input)        : Maximum Displacement
use parameter_mod
use conf
use system
IMPLICIT NONE

DOUBLE PRECISION, INTENT(OUT)            :: en
DOUBLE PRECISION, INTENT(OUT)            :: vir
INTEGER, INTENT(OUT)                     :: attempt
INTEGER, INTENT(OUT)                     :: nacc
DOUBLE PRECISION, INTENT(IN)             :: dr

DOUBLE PRECISION :: enn, eno, ran_uniform, xn, yn, zn, viro, virn
INTEGER :: o, jb

attempt = attempt + 1
jb = 1

!     ---Select A Particle At Random

o = INT(DBLE(npart)*ran_uniform()) + 1

!     ---Calculate Energy Old Configuration

CALL eneri(x(o), y(o), z(o), o, jb, eno, viro)

!     ---Give Particle A Random Displacement

xn = x(o) + (ran_uniform()-0.5D0)*dr
yn = y(o) + (ran_uniform()-0.5D0)*dr
zn = z(o) + (ran_uniform()-0.5D0)*dr

!     ---Calculate Energy New Configuration:

CALL eneri(xn, yn, zn, o, jb, enn, virn)

!     ---Acceptance Test

IF (ran_uniform() < EXP(-beta*(enn-eno))) THEN
  
!        --Accepted
  
  nacc = nacc + 1
  en = en + (enn-eno)
  vir = vir + (virn-viro)
  
!        ---Put Particle In Simulation Box
  IF (xn < 0) xn = xn + box
  IF (xn > box) xn = xn - box
  IF (yn < 0) yn = yn + box
  IF (yn > box) yn = yn - box
  IF (zn < 0) zn = zn + box
  IF (zn > box) zn = zn - box

!        ---Start modification
  
  x(o) = xn
  y(o) = yn
  z(o) = zn
!        ---End modification
END IF
RETURN
END SUBROUTINE mcmove
