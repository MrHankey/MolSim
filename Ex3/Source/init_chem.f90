SUBROUTINE init_chem(switch)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 10:53:08

!     ---Initialize And Calculate Chemical Potentials
use chem
use system
IMPLICIT NONE


INTEGER, INTENT(IN)                      :: switch

INTEGER :: ib


IF (switch == 0) THEN
!        ---Initialize
  DO ib = 1, 2
    chp(ib) = 0.0D0
    ichp(ib) = 0
  END DO
ELSE IF (switch == 2) THEN
!        ---Print Final Results
  DO ib = 1, 2
    IF (ichp(ib) /= 0) THEN
      chp(ib) = -DLOG(chp(ib)/DBLE(ichp(ib)))/beta
    END IF
  END DO
  WRITE (6, 99001) (ichp(1)+ichp(2))/2, chp(1), chp(2)
ELSE
  STOP 'Error: Init_Chem'
END IF
99001 FORMAT (' Chemical Potentials : ', /, ' Number Of Samples : ',  &
    i12, /, ' Box 1 ', f7.3, /, ' Box 2 ', f7.3, /)
RETURN
END SUBROUTINE init_chem
