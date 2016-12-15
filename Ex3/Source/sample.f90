SUBROUTINE sample(i, en, vir)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 10:53:48

!     Writes Quantities To File
use parameter_mod
use conf
use system 
IMPLICIT NONE
INTEGER, INTENT(IN OUT)                  :: i
DOUBLE PRECISION, INTENT(IN)             :: en(*)
DOUBLE PRECISION, INTENT(IN)             :: vir(*)

INTEGER :: ib
DOUBLE PRECISION :: enp(2), press(2), vol, rho(2)

DO ib = 1, 2
  vol = box(ib)**3
  rho(ib) = DBLE(npbox(ib))/vol
  press(ib) = rho(ib)/beta + vir(ib)/(3.d0*vol)
  
  IF (npbox(ib) /= 0) THEN
    enp(ib) = en(ib)/DBLE(npbox(ib))
  ELSE
    enp(ib) = 0.d0
  END IF
END DO
WRITE (66,*) i, DBLE(enp(1)), DBLE(enp(2)), DBLE(press(1)),  &
    DBLE(press(2)), DBLE(rho(1)), DBLE(rho(2))
WRITE (44,'(2(I6,F10.2))') npbox(1), box(1)**3, npbox(2), box(2) **3
WRITE(45,'(3e20.10)') DBLE(i),(DBLE(npbox(1))/box(1)**3),  &
    (DBLE(npbox(2))/box(2)**3)
RETURN
END SUBROUTINE sample
