module conf

  use parameter_mod
  DOUBLE PRECISION :: x(npmax),y(npmax),z(npmax)
  INTEGER :: npart,id(npmax),npbox(2)

end module conf

!     X(I),Y(I),Z(I)    : Position Particle I
!     Id(I)             : Box 1 Or 2
!     Npart             : Actual Number Of Particles
!     Npbox(I)          : Number Of Particles In Box I
