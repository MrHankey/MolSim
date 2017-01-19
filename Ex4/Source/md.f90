PROGRAM md

use parameter_mod
use conf
use potential
use system
use veloc
use samp
use solve_mod, only: solve

IMPLICIT NONE

INTEGER :: nstep, nstep10, step
LOGICAL :: scale
DOUBLE PRECISION :: en, ent, vir, virt, enk, time, enpot, delt, tmax,  &
    enkt, tequil, temprsq

DOUBLE PRECISION :: fx(npmax), fy(npmax), fz(npmax)


WRITE (6, *) '**************** MD_NVE ***************'
!     ---initialize sysem
CALL init(delt, tmax, tequil, temprsq, scale)
!     ---total energy of the system
CALL toterg(en, vir, enk)
WRITE (6, 99001) en - enk, enk, en + enk, vir
step = 0
time = 0
CALL sample(0, step, en, vir, enk)
nstep = INT(tmax/delt)
nstep10 = INT(nstep/10)
IF (nstep == 0) nstep10 = 0
CALL force(fx, fy, fz, enpot, vir)
DO WHILE (time < tmax)
  CALL solve(fx, fy, fz, enk, delt, enpot, vir)
  time = time + delt
  en = enpot + enk
  step = step + 1
  IF (time < tequil) THEN
    IF (scale) THEN
      IF (MOD(step,20) == 0) CALL velocs(temprsq)
    END IF
!           ---if system equilibrated sample averages:
  ELSE IF (MOD(step,nsamp) == 0) THEN
    CALL sample(1, step, en, vir, enk)
  END IF
  IF (MOD(step,nstep10) == 0) THEN
    WRITE (6, *) '======>> Done ', SNGL(time), ' out of ', SNGL(tmax), en
!           ---write intermediate configuration to file
    CALL store(8)
  END IF
END DO
CALL toterg(ent, virt, enkt)
CALL sample(2, step, en, vir, enk)
WRITE (6, 99002) ent, virt
CALL store(21)
STOP

99001 FORMAT (' Total pot. energy in. conf.       : ', f12.5, /,  &
    ' Total kinetic energy in. conf.    : ', f12.5, /,  &
    ' Total energy in. conf.            : ', f12.5, /,  &
    ' Total virial initial configuration: ', f12.5)
99002 FORMAT (' Total energy end of simulation    : ', f12.5, /,  &
    ' Total virial end of simulation    : ', f12.5)

END PROGRAM md
