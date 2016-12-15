FCOMPFLAGS    =	-O3 -132
CCOMPFLAGS    =	-O3

FFLAGS        =	$(FCOMPFLAGS)
CFLAGS        = $(CCOMPFLAGS)
LDFLAGS       =	$(FCOMPFLAGS)

LD            = ifort	
FC            =	ifort
CC            =	gcc

MAKEFILE      =	Makefile
PROGRAM       =	Gibbs

SRC	      =	chem.f90      \
		conf.f90      \
		parameter.f90 \
		potential.f90 \
		system.f90    \
		adjust.f90      \
		ener.f90        \
		eneri.f90       \
		gibbs.f90       \
		init_chem.f90   \
		lattice.f90     \
		mcmove.f90      \
		mcswap.f90      \
		mcvol.f90       \
		readdat.f90     \
		sample.f90      \
		store.f90       \
		toterg.f90

OBJS	      =	$(SRC:%.f90=%.o) ran_uniform.o sstmm.o

%.o : %.mod

.SUFFIXES:  .f90 .o

.f90.o:

	$(FC) $(FFLAGS) -c $< -o $@

all:		$(PROGRAM)

$(PROGRAM)::	$(OBJS) $(MAKEFILE)
		@$(LD) $(LDFLAGS) $(OBJS) -o $(PROGRAM)

clean:;		@rm -f $(OBJS) $(PROGRAM) core
