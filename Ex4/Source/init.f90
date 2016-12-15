SUBROUTINE init(delt, tmax, tequil, temprqs, scale)

!     reads input data and model parameters

!    Delt    (output) : time step MD simulation
!    Tmax    (output) : total simulation time
!    Tequil  (output) : total equilibration time
!    Temprqs (output) : requisted temperature
!    Scale  (output)  : if .true. use temperature scaling
!                     : if .falsee. no temperature scaling
use parameter_mod
use conf
use potential
use system
use veloc
use samp

IMPLICIT NONE

DOUBLE PRECISION, INTENT(IN OUT)         :: delt
DOUBLE PRECISION, INTENT(IN OUT)         :: tmax
DOUBLE PRECISION, INTENT(IN OUT)         :: tequil
DOUBLE PRECISION, INTENT(IN OUT)         :: temprqs
LOGICAL, INTENT(IN OUT)                  :: scale

INTEGER :: ibeg, i, iseed

DOUBLE PRECISION :: rho, rc, sumvx, sumvy, sumvz, sumv2, temp


!     ---read simulation data
READ (15, *)
READ (15, *) ibeg, delt, tmax, tequil, nsamp
READ (15, *)
READ (15, *) npart, temp, rho, rc, iseed
READ (15, *)
READ (15, *) scale, temprqs, verlet
READ (15, *)
READ (15, *) iout1, igr, iout4

!     ---initialise and test random number generator
CALL ranset(iseed)

IF (npart > npmax) THEN
  WRITE (6, *) ' ERROR: number of particles too large'
  STOP
END IF
!     ---read/generate configuration
IF (ibeg == 0) THEN
!        ---generate configuration form lattice
  box = (npart/rho)**(1.d00/3.d00)
  hbox = 0.5D00*box
  CALL lattice()
  CALL setvel(temp, iseed, sumvx, sumvy, sumvz)
ELSE
  WRITE (6, *) ' read conf from disk '
  READ (11, *) box, hbox
  READ (11, *) npart
  rho = npart/box**3
  sumv2 = 0
  sumvx = 0
  sumvy = 0
  sumvz = 0
  DO i = 1, npart
    READ (11, *) x(i), y(i), z(i), vx(i), vy(i), vz(i)
    sumv2 = sumv2 + vx(i)**2 + vy(i)**2 + vz(i)**2
    sumvx = sumvx + vx(i)
    sumvy = sumvy + vy(i)
    sumvz = sumvz + vz(i)
  END DO
  temp = sumv2/DBLE(3*npart)
  sumvx = sumvx/DBLE(npart)
  sumvy = sumvy/DBLE(npart)
  sumvz = sumvz/DBLE(npart)
  REWIND (11)
END IF
!     ---calculate cut-off radius potential
rc = MIN(rc, hbox)
rc2 = rc*rc
ecut = 4*(1/rc2**6-1/rc2**3)
!     ---write input data
WRITE (6, 99001) npart, rho, box
WRITE (6, 99003) temp, sumvx, sumvy, sumvz
WRITE (6, 99002) delt, tmax, tequil, nsamp, igr
WRITE (6, 99004) rc, ecut

pi = 4*ATAN(1.d0)
RETURN
99001 FORMAT ('  Number of particles                        :', i10, /,  &
    '  Density                                    :', f10.3,  &
    /, '  Box length                                 :', f10.3, /)
99002 FORMAT ('  Time step                                   :', f10.3,  &
    /, '  Total simulation time                       : ', f10.2, /,  &
    '  Equilibration                               : ', f10.2,  &
    /, '  Number of timesteps between two samples     : ',  &
    i10, /, '  Number of timesteps between two samples g(r): ' , i10)
99003 FORMAT ('  Initial Temperature                         :', f10.3,  &
    /, '    velocity centre of mass x-dir            :',  &
    f10.3, /, '    velocity centre of mass y-dir            :' , f10.3, /,  &
    '    velocity centre of mass z-dir            :', f10.3)
99004 FORMAT (' Simulations with TRUNCATED AND SHIFTED potential: ', /,  &
    ' Potential truncated at                      :', f10.3,  &
    /, ' Energy shift                                :', f10.6, //)
99005 FORMAT (' Time step                                   :', f10.3,  &
    /, ' Maximum simulation time                     :',  &
    f10.3, /, ' Number of timesteps between two samples     :' , f10.3)

END SUBROUTINE init
