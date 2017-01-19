FUNCTION ranf(idum)

!     random number generator

!     Idum (input): can be used as seed (not used in present
!                   random number generator.

IMPLICIT NONE

INTEGER, INTENT(IN OUT)                  :: idum
DOUBLE PRECISION :: ranf, rcarry

ranf = rcarry()
RETURN
END FUNCTION ranf

! ----------------------------------------------------C

FUNCTION randx(iseed)
!----------------------------------------------------------------------C
!  Random number generator, fast and rough, machine independent.
!  Returns an uniformly distributed deviate in the 0 to 1 interval.
!  This random number generator is portable, machine-independent and
!  reproducible, for any machine with at least 32 bits / real number.
!  REF: Press, Flannery, Teukolsky, Vetterling, Numerical Recipes (1986)
!----------------------------------------------------------------------C
IMPLICIT NONE
INTEGER, INTENT(OUT)                     :: iseed
DOUBLE PRECISION :: randx
INTEGER, PARAMETER :: m1=714025
INTEGER, PARAMETER :: ia=1366
INTEGER, PARAMETER :: ic=150889
DOUBLE PRECISION, PARAMETER :: rm=1.d+0/m1

iseed = MOD(ia*iseed+ic, m1)
randx = iseed*rm
IF (randx < 0.d+0) THEN
  STOP '*** Random number is negative ***'
END IF

RETURN
END FUNCTION randx

SUBROUTINE ranset(iseed)
!     --- initializes random number generator
  IMPLICIT NONE
  INTEGER, INTENT(IN OUT)                  :: iseed

  CALL rstart(iseed)
  RETURN
  END SUBROUTINE ranset

SUBROUTINE rstart(iseeda)
!----------------------------------------------------------------------C
!       Initialize Marsaglia list of 24 random numbers.
!----------------------------------------------------------------------C
IMPLICIT NONE
INTEGER, INTENT(IN)                      :: iseeda
DOUBLE PRECISION :: carry, ran, randx, seed
INTEGER :: i, i24, iseed, j24
COMMON /random/ seed(24), carry, i24, j24, iseed

i24 = 24
j24 = 10
carry = 0.d+0
iseed = iseeda

!       get rid of initial correlations in rand by throwing
!       away the first 100 random numbers generated.

DO i = 1, 100
  ran = randx(iseed)
END DO

!       initialize the 24 elements of seed


DO i = 1, 24
  seed(i) = randx(iseed)
END DO

RETURN
END SUBROUTINE rstart


FUNCTION rcarry()
!----------------------------------------------------------------------C
!       Random number generator from Marsaglia.
!----------------------------------------------------------------------C
IMPLICIT NONE
DOUBLE PRECISION :: carry, rcarry, seed, twom24, twop24, uni
INTEGER :: i24, iseed, j24
PARAMETER (twop24=16777216.d+0, twom24=1.d+0/twop24)
COMMON /random/ seed(24), carry, i24, j24, iseed

!       F. James Comp. Phys. Comm. 60, 329  (1990)
!       algorithm by G. Marsaglia and A. Zaman
!       base b = 2**24  lags r=24 and s=10

uni = seed(i24) - seed(j24) - carry
IF (uni < 0.d+0) THEN
  uni = uni + 1.d+0
  carry = twom24
ELSE
  carry = 0.d+0
END IF
seed(i24) = uni
i24 = i24 - 1
IF (i24 == 0) i24 = 24
j24 = j24 - 1
IF (j24 == 0) j24 = 24
rcarry = uni

RETURN
END FUNCTION rcarry
