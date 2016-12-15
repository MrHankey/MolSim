SUBROUTINE sample(switch, is, en, vir, enk)

!     Samples averages:
!        a) density, pressure, potenial energy

!    Switch (input) = 1: sample averages
!                   = 0: initialize variables
!                   = 2: writes results to disk
!    Is      = total number of time steps since start simulation
!    En     (input) total energy (potenial + kinetic)
!    Vir    (input) total virial

use parameter_mod
use conf
use potential
use system
use veloc
use samp
IMPLICIT NONE

INTEGER, INTENT(IN)                      :: switch
INTEGER, INTENT(IN)                      :: is
DOUBLE PRECISION, INTENT(IN OUT)         :: en
DOUBLE PRECISION, INTENT(IN)             :: vir
DOUBLE PRECISION, INTENT(IN)             :: enk

INTEGER ::  i, j, ig, nhgr, t, ngr, ihbmax
INTEGER, PARAMETER :: nhismax=250
DOUBLE PRECISION :: enp, press, vol, rho, temp
DOUBLE PRECISION :: tempav, delg, delgi, g(nhismax), r, e, p, r6i,  &
    r2, dx, dy, dz, xi, yi, zi, dr
DOUBLE PRECISION :: dif, vtime
DOUBLE PRECISION :: thmax
DOUBLE PRECISION :: tau0, tauc, errvacf

SAVE ngr, tempav, delg, delgi, g, nhgr

IF (switch == 1) THEN
!        ---Sample averages
  IF (npart /= 0) THEN
    enp = (en-enk)/DBLE(npart)
    temp = 2*enk/DBLE(3*npart)
    vol = box**3
    rho = npart/vol
    press = rho*temp + vir/(3.d0*vol)
  ELSE
    rho = 0.d0
    enp = 0.d0
    press = 0.d0
  END IF
  WRITE (66, *) is, SNGL(temp), SNGL(press), SNGL(enp)
  IF (MOD(is,igr) == 0) THEN
!           ---sample radial distribution function
    ngr = ngr + 1
    tempav = tempav + temp
    DO i = 1, npart
      
!-----------Start modification: sample the RDF
!      The variable g will contain the RDF
!      Variables that can be used here: xi, yi, zi, dx, dy, dz, r2, r, ig
      
      
!-----------End modification
    END DO
  END IF
ELSE IF (switch == 0) THEN
!        ---Initialize
!        ---radial distribution function:
  ngr = 0
  nhgr = nhismax
  delg = hbox/nhgr
  delgi = 1/delg
  DO i = 1, nhgr
    g(i) = 0
  END DO
ELSE IF (switch == 2) THEN
!        ---write results to file
!        ---radial distribution function
  IF (ngr /= 0) THEN
    rho = npart/box**3
    tempav = tempav/ngr
    e = 0
    p = 0
    DO i = 1, nhgr
      
!-----------Start modification: calculation of the RDF and other variables
!      e=energy calculated from RDF
!      p=pressure calculated from RDF
!      do not forget to write the RDF to the file IOUT1
      
      
!-----------End modification
    END DO
    WRITE (6, 99003) ngr, tempav, e, p + rho*tempav
  END IF
ELSE
  STOP 'Error (sample.f) switch'
END IF

RETURN
99001 FORMAT (' Number of samples for tmin = ', f8.3, ' is : ', i10, /,  &
    ' Number of samples for tmax = ', f8.3, ' is : ', i10)
99002 FORMAT ('  Decorrelation time ', e12.4)
99003 FORMAT (' Energy and pressure calculated from g(r) ', /,  &
    '     Number of samples     : ', i8, /,  &
    '     Average temperature   : ', f8.3, /,  &
    '     Energy                : ', f8.3, /,  &
    '     Pressure              : ', f8.3, /)
END SUBROUTINE sample
