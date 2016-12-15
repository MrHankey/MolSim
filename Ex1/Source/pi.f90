!cccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 09:17:59
 
!     Calculates Pi Using The Circle/Square Problem   C
!     After Execution: xmgr results.data              C
!cccccccccccccccccccccccccccccccccccccccccccccccccccccc

PROGRAM pi
IMPLICIT NONE

INTEGER :: i,j,nstep,sstmm
DOUBLE PRECISION :: ran_uniform,length,radious,ratio,x,y,at1,at2,pii,m1

!cccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     Initialize the random number generator          C
!cccccccccccccccccccccccccccccccccccccccccccccccccccccc

m1 = 0.001D0*DBLE(MOD((10+10*sstmm()),1000))

IF(m1 < 0.001D0) m1 = 0.001D0
IF(m1 > 0.999D0) m1 = 0.999D0

CALL genrand(m1)

!cccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     Read the simulation parameters from the user    C
!     The length of the square will be set to 1 and   C
!     the radius of the circle calculated accordingly C
!cccccccccccccccccccccccccccccccccccccccccccccccccccccc

WRITE(*,*) 'Number Of Cycles ? (Example: 1000)'
READ(*,*)  nstep

WRITE(*,*) 'Ratio L/D        ? (Always >= 1 !)'
READ(*,*)  ratio

IF(ratio < 1.0D0) THEN
  WRITE(*,*) 'Ratio Must Be At Least 1 !!!'
  STOP
END IF

length = 1.0

radious=length/(2.0*ratio)

at1 = 0.0D0
at2 = 0.0D0

OPEN(21,FILE='results.data',FORM='Formatted')

!cccccccccccccccccccccccccccc
!     Loop Over All Cycles  C
!cccccccccccccccccccccccccccc

DO i=1,nstep
  DO j=1,1000
    
!cccccccccccccccccccccccccccccccccccc
!     Generate A Uniform Point      C
!     Check If It Is In The Circle  C
!     At2 = Number Of Points        C
!     At1 = Number In The Circle    C
!cccccccccccccccccccccccccccccccccccc
    
! Start Modifications
    x = ran_uniform()*length/2
    y = ran_uniform()*length/2
    
    At2 = At2 + 1
    IF(x**2+y**2 <= radious**2) THEN
        At1=At1+1
    END IF
    
  END DO
!cccccccccccccccccccccccccccccccccccccccccccccccccc
!     Write every 10 cycles the estimation of pi  C
!     to an output file. This is useful to        C
!     check convergence                           C
!cccccccccccccccccccccccccccccccccccccccccccccccccc
  IF(MOD(i,10) == 0) WRITE(21,*)
END DO

CLOSE(21)

!cccccccccccccccccccccccccccccccccccc
!     Calculate the estimate of Pi  C
!cccccccccccccccccccccccccccccccccccc

pii=1.0

IF (At1 /= 0) THEN
    pii = At1*ratio**2 / At2 * 4
END IF

! End Modifications

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     The Real Value Of Pi Can Be Calculated Using       C
!     Pi = 4.0 * Arctan (1.0)                            C
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

WRITE(6,*) 'Estimate Of Pi : ',pii
WRITE(6,*) 'Real Pi        : ',4.0D0*DATAN(1.0D0)
WRITE(6,*) 'Relative Error : ',  &
    DABS(pii-4.0D0*DATAN(1.0D0))/(4.0D0*DATAN(1.0D0))

STOP
END PROGRAM pi
