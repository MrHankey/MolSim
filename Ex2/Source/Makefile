FCOMPFLAGS    =	-O3 -ffixed-line-length-none
CCOMPFLAGS    =	-O3

FFLAGS        =	$(FCOMPFLAGS)
CFLAGS        = $(CCOMPFLAGS)
LDFLAGS       =	$(FCOMPFLAGS)

LD            = gfortran	
FC            =	gfortran
CC            =	gcc

MAKEFILE      =	Makefile
PROGRAM       =	mc_nvt

SRC	      =	parameter.f90  \
		conf.f90       \
		potential.f90  \
		system.f90     \
		adjust.f90     \
		ener.f90       \
		eneri.f90      \
		lattice.f90    \
		mc_nvt.f90     \
		mcmove.f90     \
		readdat.f90    \
		sample.f90     \
		store.f90      \
		toterg.f90     \
		writepdb.f90 

OBJS	      =	$(SRC:%.f90=%.o) ran_uniform.o sstmm.o

%.o : %.mod

.SUFFIXES:  .f90 .o

.f90.o:

	$(FC) $(FFLAGS) -c $< -o $@

all:		$(PROGRAM)

$(PROGRAM)::	$(OBJS) $(MAKEFILE)
		@$(LD) $(LDFLAGS) $(OBJS) -o $(PROGRAM)

clean:;		@rm -f $(OBJS) $(PROGRAM) core
