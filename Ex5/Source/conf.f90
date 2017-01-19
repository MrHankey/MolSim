! Module conf !
module conf
  use parameter_mod
  DOUBLE PRECISION, dimension(npmax):: x,y,z
  INTEGER :: npart

end module conf

!     x(i),y(i),z(i)    : position particle i
!     npart             : actual number of particles
