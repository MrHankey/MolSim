PROGRAM mc_nvt
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 09:42:35
 
!____________________________________________________C
!                                                    C
!                                                    C
!   Equation Of State Of The Lennard-Jones Fluid     C
!                                                    C
!____________________________________________________C

use parameter_mod
use potential
use conf
use system, only: box, beta
IMPLICIT NONE

INTEGER :: equil, prod, nsamp, ii, icycl, ndispl, attempt,  &
    nacc, ncycl, nmoves, imove, kkk
DOUBLE PRECISION :: en, ent, vir, virt, dr, av1, av2, press, bv1,bv2,x_test,y_test,z_test, ran_uniform, &
    en_test, vir_test

WRITE (6, *) '**************** Mc_Nvt ***************'

!     ---Initialize Sysem

CALL readdat(equil, prod, nsamp, ndispl, dr)

nmoves = ndispl
av1    = 0.0D0
av2    = 0.0D0
bv1    = 0.0D0
bv2    = 0.0D0

!     ---Total Energy Of The System

CALL toterg(en, vir)
WRITE (6, 99001) en, vir

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
  nacc    = 0
  
!        ---Intialize The Subroutine That Adjust The Maximum Displacement
  
  CALL adjust(attempt, nacc, dr)
  
  DO icycl = 1, ncycl
    
    DO imove = 1, nmoves
      
!              ---Attempt To Displace A Particle
      
      CALL mcmove(en, vir, attempt, nacc, dr)
    END DO
    
    IF (ii == 2) THEN
      
!              ---Sample Averages
      
      IF (MOD(icycl,nsamp) == 0) THEN
        CALL sample(icycl, en, vir, press)
        av1 = av1 + press
        av2 = av2 + 1.0D0
        
!cccccccccccccccccccccccccccccccccccccccc
!     Calculate The Chemical Potential  C
!     Do 10 Trial Chains                C
!     Calculate The Average Of          C
!     [Exp(-Beta*Energy)]               C
!     You Can Use The Subroutine Eneri  C
!     For This. Good Luck !!!           C
!cccccccccccccccccccccccccccccccccccccccc
        
        DO kkk=1,10
          
!     Start Modification
          x_test = ran_uniform()*box
          y_test = ran_uniform()*box
          z_test = ran_uniform()*box
          
          CALL eneri(x_test, y_test, z_test, npart+1, 1, en_test, vir_test)
          bv1 = bv1 + DEXP(-beta*en_test)
          bv2 = bv2 + 1.0D0
          
!     End   Modification
          
        END DO
      END IF
    END IF
    
    IF(MOD(icycl,20) == 0) CALL writepdb
    
    IF (MOD(icycl,ncycl/5) == 0) THEN
      WRITE (6, *) '======>> Done ', icycl, ' Out Of ', ncycl
      
!              ---Write Intermediate Configuration To File
      
      CALL store(8, dr)
      
!              ---Adjust Maximum Displacements
      
      CALL adjust(attempt, nacc, dr)
    END IF
  END DO
  IF (ncycl /= 0) THEN
    IF (attempt /= 0) WRITE (6, 99003) attempt, nacc,  &
        100.0D0*DBLE(nacc)/DBLE(attempt)
    
!           ---Test Total Energy
    
    CALL toterg(ent, virt)
    IF (ABS(ent-en) > 1.d-6) THEN
      WRITE (6, *) ' ######### Problems Energy ################ '
    END IF
    IF (ABS(virt-vir) > 1.d-6) THEN
      WRITE (6, *) ' ######### Problems Virial ################ '
    END IF
    WRITE (6, 99002) ent, en, ent - en, virt, vir, virt - vir
    WRITE (6,*)
    
!cccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccc
!     Print Chemical Potential And Pressure   C
!cccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccc
    
    IF(ii == 2) THEN
      WRITE (6,*) 'Average Pressure                  : ', av1/av2
      WRITE (6,*) 'Chemical Potential                : ',  &
          -LOG((bv1/bv2)*(box*box*box/ DBLE(npart)))/beta

      open(unit=72, file="chem_pot.txt", status="old", action="write", position="append")
      
      WRITE (72,*) (DBLE(npart)/(box*box*box)), av1/av2, -LOG((bv1/bv2)*(box*box*box/DBLE(npart)))/beta
    END IF
  END IF
END DO
CALL store(21, dr)
STOP



99001 FORMAT (' Total Energy Initial Configuration: ', f12.5, /,  &
    ' Total Virial Initial Configuration: ', f12.5)
99002 FORMAT (' Total Energy End Of Simulation    : ', f12.5, /,  &
    '       Running Energy              : ', f12.5, /,  &
    '       Difference                  :  ', e12.5, /,  &
    ' Total Virial End Of Simulation    : ', f12.5, /,  &
    '       Running Virial              : ', f12.5, /,  &
    '       Difference                  :  ', e12.5)
99003 FORMAT (' Number Of Att. To Displ. A Part.  : ', i10, /,  &
    ' Success: ', i10, '(= ', f5.2, '%)')
END PROGRAM mc_nvt
