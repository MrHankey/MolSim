SUBROUTINE readdat(equil, prod, nsamp, ndispl, dr, nvol, vmax,  &
        nswap, succ)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 10:53:47
use parameter_mod
use system 
use potential
use chem
use conf

IMPLICIT NONE

INTEGER, INTENT(IN OUT)                  :: equil
INTEGER, INTENT(IN OUT)                  :: prod
INTEGER, INTENT(IN OUT)                  :: nsamp
INTEGER, INTENT(IN OUT)                  :: ndispl
DOUBLE PRECISION, INTENT(IN OUT)         :: dr
INTEGER, INTENT(IN OUT)                  :: nvol
DOUBLE PRECISION, INTENT(IN OUT)         :: vmax
INTEGER, INTENT(IN OUT)                  :: nswap
DOUBLE PRECISION, INTENT(IN OUT)         :: succ

!     Reads Input Data And Model Parameters

!     ---Input Parameters: File: Fort.15
!    Ibeg  =  0 : Initilaize From A Lattice
!             1 : Read Configuration From Disk
!    Equil      : Number Of Monte Carlo Cycles During Equilibration
!    Prod       : Number Of Monte Carlo Cycles During Production
!    Nsamp      : Number Of Monte Carlo Cycles Between Two Sampling Periods
!    Dr         : Maximum Displacement
!    Vmax       : Maximum Volume Change
!    Succ       : Optimal Percentance Of Accepted Attemps
!                 The Program Adjusts Vmax Or Dr In Just A Way That
!                 On Average Succ% Of The Moves Are Accepted
!    Ndispl     : Number Of Attemps To Displace A Particle Per Mc Cycle
!    Nvol       : Number Of Attemps To Change The Volume  Per Mc Cycle
!    Nswap      : Number Of Attemps To Swap Particle Between The Two Boxes Per Mc Cycle
!    Npart      : Total Numbero Fo Particles
!    Temp       : Temperature
!    Rho        : Density

!     ---Input Parameters: File: Fort.25
!    Eps    = Epsilon Lennard-Jones Potential
!    Sig    = Sigma Lennard-Jones Potential
!    Mass   = Mass Of The Particle
!    Rcc    = Cut-Off Radius Of The Potential

!     ---Input Parameters: File: Fort.11 (Restart File
!                To Continue A Simulation From Disk)
!    Box(1)   = Length Box 1 Old Configuration
!    Hbox(1)  = Box(1)/2
!    Box(2)   = Length Box 2 Old Configuration
!    Hbox(2)  = Box(2)/2
!    Npart    = Total Number Of Particles (Over Rules Fort.15!!)
!    Npbox(1) = Number Of Particles In Box 1
!    Npbox(2) = Number Of Particles In Box 2
!    Dr     = Optimized Maximum Displacement Old Configurations
!    Vmax   = Optimized Maximum Volume Change Old Configurations
!    X(1),Y(1),Z(1)            : Position First Particle 1
!                  ,Id(1)   = 1 Particle In Box 1
!                  ,Id(1)   = 2 Particle In Box 2
!       ....
!    X(Npart),Y(Npart),Z(Npart): Position Particle Last Particle


INTEGER :: ibeg, i, ib
DOUBLE PRECISION :: eps, sig, rho, rcc

READ (15, *)
READ (15, *) ibeg, equil, prod, nsamp
READ (15, *)
READ (15, *) dr, vmax, succ
READ (15, *)
READ (15, *) ndispl, nvol, nswap
READ (15, *)
READ (15, *) npart, temp, rho

IF (npart > npmax) THEN
  WRITE (6, *) ' Error: Number Of Particles Too Large'
  STOP
END IF

!     ---Read Model Parameters

READ (25, *)
READ (25, *) eps, sig, mass, rcc

!     ---Read/Generate Configuration

box(1)  = (DBLE(npart)/(2*rho))**(1.d00/3.d00)
hbox(1) = 0.5D00*box(1)
box(2)  = box(1)
hbox(2) = hbox(1)

IF (ibeg == 0) THEN
  
!        ---Generate Configuration Form Lattice
  
  CALL lattice
ELSE
  WRITE (6, *) ' Read Conf From Disk '
  READ (11, *) box(1), hbox(1), box(2), hbox(2)
  READ (11, *) npart, npbox(1), npbox(2)
  READ (11, *) dr, vmax
  DO i = 1, npart
    READ (11, *) x(i), y(i), z(i), id(i)
  END DO
  REWIND (11)
END IF

!     ---Write Input Data

WRITE (6, 99001) equil, prod, nsamp
WRITE (6, 99002) ndispl, dr, nvol, vmax, nswap
WRITE (6, 99003) npart, temp, 0.0D0, DBLE(npbox(1))/box(1)**3, box(1),  &
    DBLE(npbox(2))/box(2)**3, box(2)
WRITE (6, 99004) eps, sig, mass

!     ---Calculate Parameters:

beta  = 1.0D0/temp
eps4  = 4.0D0*eps
eps48 = 48.d0*eps
sig2  = sig*sig

!     ---Calculate Cut-Off Radius Potential

DO ib = 1, 2
  rc(ib) = MIN(rcc, hbox(ib))
  rc2(ib) = rc(ib)*rc(ib)
END DO

99001 FORMAT ('  Number Of Equilibration Cycles             :', i10, /,  &
    '  Number Of Production Cycles                :', i10, /,  &
    '  Sample Frequency                           :', i10, /)

99002 FORMAT ('  Number Of Att. To Displ. A Part. Per Cycle :', i10, /,  &
    '  Maximum Displacement                       :', f10.3,  &
    /, '  Number Of Att. To Change Volume  Per Cycle :', i10,  &
    /, '  Maximum Change Volume                      :',  &
    f10.3, /, '  Number Of Att. To Exch Part.  Per Cycle    :' , i10, //)
99003 FORMAT ('  Number Of Particles                        :', i10, /,  &
    '  Temperature                                :', f10.3,  &
    /, '  Pressure                                   :',  &
    f10.3, /, '  Density Box 1                              :' , f10.3, /,  &
    '  Box 1 Length                               :', f10.3,  &
    /, '  Density Box 1                              :',  &
    f10.3, /, '  Box 2 Length                               :' , f10.3, /)
99004 FORMAT ('  Model Parameters: ', /, '     Epsilon: ', f6.3, /,  &
    '     Sigma  : ', f6.3, /, '     Mass   : ', f6.3)
RETURN
END SUBROUTINE readdat
