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
INTEGER, PARAMETER :: nhismax=500
DOUBLE PRECISION :: enp, press, vol, rho, temp
DOUBLE PRECISION :: tempav, delg, delgi, g(nhismax), r, e, p, r6i,  &
    r2, dx, dy, dz, xi, yi, zi, dr, vb, nid
DOUBLE PRECISION :: dif, vtime
DOUBLE PRECISION :: thmax
DOUBLE PRECISION :: tau0, tauc, errvacf
DOUBLE PRECISION :: enr, virr, int_r, int_r2, int_r2i, int_r6i, int_en, int_vir
DOUBLE PRECISIOn :: k_b = 1.38064852D-23
!DOUBLE PRECISION :: pi = 3.14159265359

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
  WRITE (66, *) is, SNGL(temp), SNGL(press), SNGL(en), SNGL(enp), SNGL(enk), SNGL(vir)
  IF (MOD(is,igr) == 0) THEN
!           ---sample radial distribution function
    ngr = ngr + 1
    tempav = tempav + temp
    DO i = 1, npart
      
!-----------Start modification: sample the RDF
!      The variable g will contain the RDF
!      Variables that can be used here: xi, yi, zi, dx, dy, dz, r2, r, ig
      DO j = i+1, npart
        dx = x(i) - x(j)
        dy = y(i) - y(j)
        dz = z(i) - z(j)

        dx = dx - box*NINT(dx/box)
        dy = dy - box*NINT(dy/box)
        dz = dz - box*NINT(dz/box)

        r = sqrt(dx**2 + dy**2 + dz**2)

        IF (r < hbox) THEN
          ig = r/delg
          g(ig) = g(ig) + 2
        END IF
      END DO
      
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
  open(44, file='lj.rdf')
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
      r = delg*(REAL(i)+0.5D0)
      vb = DBLE((i+1)**3 - i**3) * delg**3
      nid = (4.0D0/3.0D0) * pi * vb * rho
      g(i) = g(i)/(DBLE(ngr)*DBLE(npart)*nid)
      WRITE (44, *) r, g(i)
      
!-----------End modification

    END DO

    int_en = 0.0D0
    int_vir = 0.0D0

    DO i = 1, nhgr
        int_r = delg*(REAL(i)+0.5D0)
        int_r2 = int_r**2
        int_r2i = 1.0D0/(int_r2)
        int_r6i = int_r2i*int_r2i*int_r2i
        ! ecut entfernen?
        enr = (4*(int_r6i*int_r6i-int_r6i)) * int_r2 * g(i)
        virr = (48*(int_r6i*int_r6i-0.5D0*int_r6i)) * int_r2 * g(i)

        int_r = delg*(REAL(i+1)+0.5D0)
        int_r2 = int_r**2
        int_r2i = 1.0D0/(int_r2)
        int_r6i = int_r2i*int_r2i*int_r2i
        ! ecut entfernen?
        enr = (enr + (4*(int_r6i*int_r6i-int_r6i)) * int_r2 * g(i)) / 2.0D0
        virr = (virr + (48*(int_r6i*int_r6i-0.5D0*int_r6i)) * int_r2 * g(i)) / 2.0D0

        int_en = int_en + delg*enr
        int_vir = int_vir + delg*virr
    END DO

    e = 3/2.0D0*npart*k_b*temp + 2*pi*npart*rho*int_en
    WRITE(*,*) e
    WRITE(*,*) rho*temp + vir/(3.d0*box**3)
    p = rho*k_b*temp - 2/3.0D0*pi*rho**2*int_en

    WRITE (6, 99003) ngr, tempav, e, p + rho*tempav
  END IF
  endfile(44)
  close(44)

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
    '     Energy                : ', f12.5, /,  &
    '     Pressure              : ', f8.3, /)
END SUBROUTINE sample