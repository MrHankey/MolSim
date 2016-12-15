!   Calculation of MSD and Diffusioncoefficient


! START Modification: Complete variables 
! ---------------------------------------
Subroutine MSD(Switch,)
! ---------------------------------------
! END Modification    SUBROUTINE msd(switch,cx,cy,cz,samplemsd,delt)
 
use parameter_mod
use conf
use veloc
use system
IMPLICIT NONE

INTEGER, INTENT(IN)                      :: switch
INTEGER, INTENT(IN OUT)                  :: cx(npmax)
INTEGER, INTENT(IN OUT)                  :: cy(npmax)
INTEGER, INTENT(IN OUT)                  :: cz(npmax)
INTEGER, INTENT(IN OUT)                  :: samplemsd
DOUBLE PRECISION, INTENT(IN)             :: delt

!     Tmax  = Maximum Timesteps For The Correlation Time
!     T0max = Maximum Number Of Time Origins
!     Tvacf = Timestep counter
!     R2t = MSD

INTEGER :: iout,nbl

INTEGER, PARAMETER :: tmax=1000
INTEGER, PARAMETER :: t0max=200

INTEGER :: time0(t0max),t0, i,t,tvacf,dt,tt0, ttv0(tmax),  step1, ttel

DOUBLE PRECISION :: xcor(npmax),ycor(npmax),zcor(npmax),r2t(tmax),  &
    x0(npmax,t0max),y0(npmax,t0max), z0(npmax,t0max),nvacf(tmax),intt,dtime,  &
     r2a, vtime

SAVE nvacf,tvacf,r2t,tt0,t0,time0,step1

!     Initialize Everything
IF (switch == 1) THEN
  
  tvacf = 0
  t0    = 0
  step1  = 0
  
  DO i = 1, tmax
    r2t(i)   = 0.0D0
    nvacf(i) = 0.0D0
  END DO


! Sample
! --------------------------------------------------------
! Hint: Introduce variables for the sample rates and the 
! rates for new t0. (Default value 10 for both)
! -------------------------------------------------------- 
 
ELSE IF(switch == 2) THEN
  step1 = step1+1

  IF (MOD(step1,10) == 0) THEN
    tvacf = tvacf + 1
!           ---sample mean square displacement
    IF (MOD(tvacf,10) == 0) THEN
!     --new t=0
      t0 = t0 + 1
      ttel = MOD(t0-1, t0max) + 1
      ttv0(ttel) = tvacf
      DO i = 1, npart

!         START Modification: Store Positions at ttel  
!         ---------------------------------------




!         ---------------------------------------
!         END Modification    
        
      END DO
    END IF
    DO t = 1, MIN(t0, t0max)
      dt = tvacf - ttv0(t) + 1

      IF (dt < tmax) THEN
        nvacf(dt) = nvacf(dt) + 1
        DO i = 1, npart

!            START Modification: Calculate R2t - MSD 
!            ---------------------------------------




!            ---------------------------------------
!            END Modification   
 
        END DO
      END IF
    END DO
  END IF
  
  
ELSE
  
!cccccccccccccccccccccccccccccccccc
!     Write Everything To Disk    C
!cccccccccccccccccccccccccccccccccc
  
  
  DO i = 1, tmax

!     START Modification: Write Results 
!     ---------------------------------------




    

!     ---------------------------------------
!     END Modification

  END DO
END IF

RETURN
END SUBROUTINE msd
