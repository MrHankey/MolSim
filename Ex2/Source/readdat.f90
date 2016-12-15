SUBROUTINE readdat(equil, prod, nsamp, ndispl, dr)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 09:42:41
use parameter_mod
use potential
use conf
use system
IMPLICIT NONE

INTEGER, INTENT(IN OUT)                  :: equil
INTEGER, INTENT(IN OUT)                  :: prod
INTEGER, INTENT(IN OUT)                  :: nsamp
INTEGER, INTENT(IN OUT)                  :: ndispl
DOUBLE PRECISION, INTENT(IN OUT)         :: dr

!     ---Read Input Data And Model Parameters

!     ---Input Parameters: File: Fort.15
!    Ibeg  =  0 : Initialize From A Lattice
!             1 : Read Configuration From Disk
!    Equil      : Number Of Monte Carlo Cycles During Equilibration
!    Prod       : Number Of Monte Carlo Cycles During Production
!    Nsamp      : Number Of Monte Carlo Cycles Between Two Sampling Periods
!    Dr         : Maximum Displacement
!    Ndispl     : Number Of Attemps To Displace A Particle Per Mc Cycle
!    Npart      : Total Number of Particles
!    Temp       : Temperature
!    Rho        : Density

!     ---Input Parameters: File: Fort.25
!    Eps    = Epsilon Lennard-Jones Potential
!    Sig    = Sigma Lennard-Jones Potential
!    Mass   = Mass Of The Particle
!    Rc     = Cut-Off Radius Of The Potential

!     ---Input Parameters: File: Fort.11 (Restart File
!                To Continue A Simulation From Disk)
!    Boxf   = Box Length Old Configuration (If This One
!             Does Not Correspond To The Requested Density, The Positions
!             Of The Particles Are Rescaled!
!    Npart  = Number Of Particles (Over Rules Fort.15!!)
!    Dr     = Optimized Maximum Displacement Old Configurations


!    X(1),Y(1),Z(1)            : Position First Particle 1
!        ...
!    X(Npart),Y(Npart),Z(Npart): Position Particle Last Particle

INTEGER :: ibeg, i, sstmm
DOUBLE PRECISION :: eps, sig, boxf, rhof, rho, m1

!     ---Read Simulation Data
READ (15, *)
READ (15, *) ibeg, equil, prod, nsamp
READ (15, *)
READ (15, *) dr
READ (15, *)
READ (15, *) ndispl
READ (15, *)
READ (15, *) npart, temp, rho

!     ---Initialise And Random Number Generator

m1 = 0.001D0*DBLE(MOD((10+10*sstmm()),1000))

IF(m1 < 0.001D0) m1 = 0.001D0
IF(m1 > 0.999D0) m1 = 0.999D0

CALL genrand(m1)

IF (npart > npmax) THEN
  WRITE (6, *) ' Error: Number Of Particles Too Large'
  STOP
END IF

box  = (DBLE(npart)/rho)**(1.d0/3.d0)
hbox = box/2

!     ---Read Model Parameters

READ (25, *)
READ (25, *) eps, sig, mass, rc

!     ---Read/Generate Configuration

IF (ibeg == 0) THEN
  
!        ---Generate Configuration Form Lattice
  
  CALL lattice
ELSE
  WRITE (6, *) ' Read Conf From Disk '
  READ (11, *) boxf
  READ (11, *) npart
  READ (11, *) dr
  rhof = DBLE(npart)/boxf**3
  IF (ABS(boxf-box) > 1D-6) THEN
    WRITE (6, 99007) rho, rhof
  END IF
  DO i = 1, npart
    READ (11, *) x(i), y(i), z(i)
    x(i) = x(i)*box/boxf
    y(i) = y(i)*box/boxf
    z(i) = z(i)*box/boxf
  END DO
  REWIND (11)
END IF

!     ---Write Input Data

WRITE (6, 99001) equil, prod, nsamp
WRITE (6, 99002) ndispl, dr
WRITE (6, 99003) npart, temp, rho, box
WRITE (6, 99004) eps, sig, mass

!     ---Calculate Parameters:

beta = 1/temp

!     ---Calculate Cut-Off Radius Potential

rc    = MIN(rc, hbox)
rc2   = rc*rc
eps4  = 4.0D0*eps
eps48 = 48.0D0*eps
sig2  = sig*sig

RETURN

99001 FORMAT ('  Number Of Equilibration Cycles             :', i10, /,  &
    '  Number Of Production Cycles                :', i10, /,  &
    '  Sample Frequency                           :', i10, /)
99002 FORMAT ('  Number Of Att. To Displ. A Part. Per Cycle :', i10, /,  &
    '  Maximum Displacement                       :', f10.3, //)
99003 FORMAT ('  Number Of Particles                        :', i10, /,  &
    '  Temperature                                :', f10.3,  &
    /, '  Density                                    :',  &
    f10.3, /, '  Box Length                                 :' , f10.3, /)
99004 FORMAT ('  Model Parameters: ', /, '     Epsilon: ', f6.3, /,  &
    '     Sigma  : ', f6.3, /, '     Mass   : ', f6.3)
99007 FORMAT (' Requested Density: ', f5.2,  &
    ' Different From Density On Disk: ', f5.2, /, ' Rescaling Of Coordinates!')
END SUBROUTINE readdat
