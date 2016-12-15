SUBROUTINE adjust(attemp, nacc, dr)
 
  use system
  IMPLICIT NONE
  ! Code converted using TO_F90 by Alan Miller
  ! Date: 2013-10-02  Time: 09:41:10

  INTEGER, INTENT(IN)                      :: attemp
  INTEGER, INTENT(IN)                      :: nacc
  DOUBLE PRECISION, INTENT(IN OUT)         :: dr
  INTEGER :: attempp, naccp
  DOUBLE PRECISION :: dro, frac

  SAVE naccp, attempp
  DATA naccp/0/
  DATA attempp/0/

  !     Adjusts Maximum Displacement Such That 50% Of The
  !     Movels Will Be Accepted

  !  Attemp (Input)  Number Of Attemps That Have Been Performed
  !                  To Displace A Particle
  !  Nacc   (Input)  Number Of Successful Attemps To
  !                  Displace A Particle
  !  Dr     (Output) New Maximum Displacement




  IF (attemp == 0.OR.attempp >= attemp) THEN
    naccp = nacc
    attempp = attemp
  ELSE
    frac = DBLE(nacc-naccp)/DBLE(attemp-attempp)
    dro  = dr
    dr   = dr*ABS(frac/0.5D0)
    
  !        ---Limit The Change:
    
    IF (dr/dro > 1.5D0) dr = dro*1.5D0
    IF (dr/dro < 0.5D0) dr = dro*0.5D0
    IF (dr > hbox/2.d0) dr = hbox/2.d0
    WRITE (6, 99001) dr, dro, frac, attemp - attempp, nacc - naccp
    
  !        ---Store Nacc And Attemp For Next Use
    
    naccp = nacc
    attempp = attemp
  END IF
  RETURN
  99001 FORMAT (' Max. Displ. Set To : ', f6.3, ' (Old : ', f6.3, ')', /,  &
      ' Frac. Acc.: ', f5.2, ' Attempts: ', i7, ' Succes: ', i7)

END SUBROUTINE adjust
