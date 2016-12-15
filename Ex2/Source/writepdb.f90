SUBROUTINE writepdb
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-10-02  Time: 09:43:16

use parameter_mod
use conf
IMPLICIT NONE

INTEGER :: i,countmodel,countatom

DATA countmodel/ 0/
DATA countatom/ 0/

SAVE countmodel,countatom

countmodel = countmodel + 1

WRITE(22,'(A,I9)') 'MODEL',countmodel

DO i=1,npart
  
  countatom = countatom + 1
  
  WRITE(22,'(A,I7,A,I12,4x,3f8.3)') 'ATOM',countatom,'  O',  &
      countatom,x(i),y(i),z(i)
END DO

WRITE(22,'(A)') 'ENDMDL'

RETURN
END SUBROUTINE writepdb
