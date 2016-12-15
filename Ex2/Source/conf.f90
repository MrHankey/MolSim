module conf
  use parameter_mod
 
  DOUBLE PRECISION, dimension(npmax) :: x,y,z
  INTEGER :: npart

end module conf

!     X(I),Y(I),Z(I)    : Position Particle I
!     Npart             : Actual Number Of Particles
