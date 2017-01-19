FCOMPFLAGS    =	-O3
CCOMPFLAGS    =	-O3

FFLAGS        =	$(FCOMPFLAGS)
CFLAGS        = $(CCOMPFLAGS)
LDFLAGS       =	$(FCOMPFLAGS)

LD            = gfortran
FC            =	gfortran
CC            =	gcc

MAKEFILE      =	Makefile
PROGRAM       = MD 

SRC	      = parameter.f90 \
		conf.f90      \
		potential.f90 \
		system.f90    \
		samp.f90      \
		veloc.f90     \
		eneri.f90       \
		force.f90       \
		init.f90        \
		lattice.f90     \
		solve.f90       \
		md.f90          \
		ranf.f90        \
		sample.f90      \
		setvel.f90      \
		store.f90       \
		toterg.f90      \
		velocs.f90

OBJS	      =	$(SRC:%.f90=%.o)

%.o : %.mod

.SUFFIXES:  .f90 .o

.f90.o:

	$(FC) $(FFLAGS) -c $< -o $@

all:		$(PROGRAM)

$(PROGRAM)::	$(OBJS) $(MAKEFILE)
		@$(LD) $(LDFLAGS) $(OBJS) -o $(PROGRAM)

clean:;		@rm -f $(OBJS) $(PROGRAM) core
