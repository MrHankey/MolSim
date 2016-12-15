SUBROUTINE adjust(attemp, nacc, dr, attv, accv, vmax, succ)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 10:52:55

!     Sets Maximum Displacement And Maximum Volume Change
!     Such That 50% Of The Move Will Be Accepted
use system
IMPLICIT NONE

INTEGER, INTENT(IN)                      :: attemp
INTEGER, INTENT(IN)                      :: nacc
DOUBLE PRECISION, INTENT(IN OUT)         :: dr
INTEGER, INTENT(IN)                      :: attv
INTEGER, INTENT(IN)                      :: accv
DOUBLE PRECISION, INTENT(IN OUT)         :: vmax
DOUBLE PRECISION, INTENT(IN OUT)         :: succ

INTEGER :: attempp, naccp, accvp, attvp
DOUBLE PRECISION :: dro, frac, vmaxo
SAVE naccp, attempp, attvp, accvp

DATA attempp/0/
DATA attvp/0/

!     ---Displacement:

IF (attemp == 0.OR.attempp >= attemp) THEN
  naccp = nacc
  attempp = attemp
ELSE
  frac = DBLE(nacc-naccp)/DBLE(attemp-attempp)
  dro = dr
  dr = dr*ABS(frac/(succ/100.d0))
  
!        ---Limit The Change:
  
  IF (dr/dro > 1.5D0) dr = dro*1.5D0
  IF (dr/dro < 0.5D0) dr = dro*0.5D0
  IF (dr > hbox(1)/2.d0) dr = hbox(1)/2.d0
  WRITE (6, 99001) dr, dro, frac, attemp - attempp, nacc - naccp
  
!        ---Store Nacc And Attemp For Next Use
  
  naccp = nacc
  attempp = attemp
END IF

!     ---Volume:

IF (attv == 0.OR.attvp >= attv) THEN
  accvp = accv
  attvp = attv
ELSE
  frac = DBLE(accv-accvp)/DBLE(attv-attvp)
  vmaxo = vmax
  vmax = vmax*ABS(frac/(succ/100.d0))
  
!        ---Limit The Change:
  
  IF (vmax/vmaxo > 1.5D0) vmax = vmaxo*1.5D0
  IF (vmax/vmaxo < 0.5D0) vmax = vmaxo*0.5D0
  WRITE (6, 99002) vmax, vmaxo, frac, attv - attvp, accv - accvp
  
!        ---Store Nacc And Attemp For Next Use
  
  accvp = accv
  attvp = attv
END IF
RETURN
99001 FORMAT (' Max. Displ. Set To      : ', f6.3, ' (Old : ', f6.3,  &
    ')', /, ' Frac. Acc.: ', f5.2, ' Attempts: ', i7, ' Succes: ', i7)
99002 FORMAT (' Max. Vol. Chan. Set To: ', f6.3, ' (Old : ', f6.3, ')',  &
    /, ' Frac. Acc.: ', f5.2, ' Attempts: ', i7, ' Succes: ', i7)
END SUBROUTINE adjust
