PROGRAM gibbs
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 10:53:04
use parameter_mod
use chem
use conf
use potential
use system

IMPLICIT NONE

!     ---Gibbs-Ensemble Simulation Of The Lennard-Joned Fluid

INTEGER :: equil, prod, nsamp, ii, icycl, ndispl, attempt,  &
    nacc, ncycl, nmoves, imove, nvol, accv, attv, ib, nswap, accsw, attsw, sstmm
DOUBLE PRECISION :: en(2), ent(2), vir(2), virt(2), vmax, dr,  &
    ran, succ,ran_uniform,m1

m1 = 0.001D0*DBLE(MOD((10+10*sstmm()),1000))

IF(m1 < 0.001D0) m1 = 0.001D0
IF(m1 > 0.999D0) m1 = 0.999D0

CALL genrand(m1)

WRITE (6, *) '**************** GIBBS ***************'

!     ---Initialize Sysem

CALL readdat(equil, prod, nsamp, ndispl, dr, nvol, vmax, nswap, succ)
nmoves = ndispl + nvol + nswap

!     ---Total Energy Of The System

DO ib = 1, 2
  CALL toterg(en(ib), vir(ib), ib)
  WRITE (6, 99001) ib, en(ib), vir(ib)
END DO

!     ---Start Mc-Cycle

DO ii = 1, 2
  
!        --- Ii=1 Equilibration
!        --- Ii=2 Production
  
  IF (ii == 1) THEN
    ncycl = equil
    IF (ncycl /= 0) WRITE (6, *) ' Start Equilibration '
  ELSE
    IF (ncycl /= 0) WRITE (6, *) ' Start Production '
    ncycl = prod
  END IF
  attempt = 0
  nacc = 0
  attv = 0
  accv = 0
  attsw = 0
  accsw = 0
  
!        ---Initialize Calculation Chemical Potential
  
  CALL init_chem(0)
  
!        ---Intialize The Subroutine That Adjust The Maximum Displacement
  
  CALL adjust(attempt, nacc, dr, attv, accv, vmax, succ)
  
  DO icycl = 1, ncycl
    DO imove = 1, nmoves
      ran = ran_uniform()*DBLE(ndispl+nvol+nswap)
      IF (ran < DBLE(ndispl)) THEN
        
!                 ---Attempt To Displace A Particle
        
        CALL mcmove(en, vir, attempt, nacc, dr)
      ELSE IF (ran < DBLE(ndispl+nvol)) THEN
        
!                 ---Attempt To Change The Volume
        
        CALL mcvol(en, vir, attv, accv, vmax)
      ELSE
        
!                 ---Attemp To Exchange Particles
        
        CALL mcswap(en, vir, attsw, accsw)
        
      END IF
    END DO
    IF (ii == 2) THEN
      
!              ---Sample Averages
      
      IF (MOD(icycl,nsamp) == 0) CALL sample(icycl, en, vir)
    END IF
    IF (MOD(icycl,ncycl/5) == 0) THEN
      WRITE (6, *) '======>> Done ', icycl, ' Out Of ', ncycl,  &
          npbox(1), npbox(2)
      
!              ---Write Intermediate Configuration To File
      
      CALL store(8, dr, vmax)
      
!              ---Adjust Maximum Displacements
      
      CALL adjust(attempt, nacc, dr, attv, accv, vmax, succ)
    END IF
  END DO
  IF (ncycl /= 0) THEN
    IF (attempt /= 0) WRITE (6, 99003) attempt, nacc,  &
        100.*FLOAT(nacc)/FLOAT(attempt)
    IF (attv /= 0) WRITE (6, 99004) attv, accv, 100.*FLOAT(accv) /FLOAT(attv)
    IF (attsw /= 0) WRITE (6, 99005) attsw, accsw,  &
        100.*FLOAT(accsw)/FLOAT(attsw)
    DO ib = 1, 2
      
!              ---Test Total Energy
      
      CALL toterg(ent(ib), virt(ib), ib)
      IF (ABS(ent(ib)-en(ib)) > 1.d-6) THEN
        WRITE (6, *) ' ######### Problems Energy ################ '
      END IF
      IF (ABS(virt(ib)-vir(ib)) > 1.d-6) THEN
        WRITE (6, *) ' ######### Problems Virial ################ '
      END IF
      WRITE (6, 99002) ib, ent(ib), en(ib), ent(ib) - en(ib),  &
          virt(ib), vir(ib), virt(ib) - vir(ib)
    END DO
!           ---Calculation Chemical Potential
    CALL init_chem(2)
  END IF
END DO
CALL store(21, dr, vmax)


99001 FORMAT (' Box : ', i3, /,  &
    ' Total Energy Initial Configuration : ', f12.5, /,  &
    ' Total Virial Initial Configuration : ', f12.5)
99002 FORMAT (' Box : ', i3, /, ' Total Energy End Of Simulation   : ',  &
    f12.5, /, '       Running Energy             : ', f12.5,  &
    /, '       Difference                 :  ', e12.5, /,  &
    ' Total Virial End Of Simulation   : ', f12.5, /,  &
    '       Running Virial             : ', f12.5, /,  &
    '       Difference                 :  ', e12.5)
99003 FORMAT (' Number Of Att. To Displ. A Part.  : ', i10, /,  &
    ' Success: ', i10, '(= ', f5.2, '%)')
99004 FORMAT (' Number Of Att. To Chan. Volume    : ', i10, /,  &
    ' Success: ', i10, '(= ', f5.2, '%)')
99005 FORMAT (' Number Of Att. To Exchange Part.  : ', i10, /,  &
    ' Success: ', i10, '(= ', f5.2, '%)')

STOP
END PROGRAM gibbs
