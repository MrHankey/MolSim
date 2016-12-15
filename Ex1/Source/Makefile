FCOMPFLAGS    =	-O3 -ffixed-line-length-none
CCOMPFLAGS    = -O

FFLAGS        =	$(FCOMPFLAGS)
CFLAGS        = $(CCOMPFLAGS)
LDFLAGS       =	$(FCOMPFLAGS)

LD            = gfortran	
FC            =	gfortran
CC            =	gcc

MAKEFILE      =	Makefile
PROGRAM       =	pi

INCS	      =	

SRC	      = pi.f90

OBJS	      =	$(SRC:%.f90=%.o) ran_uniform.o sstmm.o

.SUFFIXES:  .f90 .o

.f90.o:
	$(LD) $(FFLAGS) -c $< -o $@

all:		$(PROGRAM)

$(PROGRAM)::	$(INCS)
		@/bin/rm -f $(OBJS) core

$(PROGRAM)::	$(OBJS) $(MAKEFILE)
		$(LD) $(LDFLAGS) $(OBJS) -o $(PROGRAM)

clean:;		@rm -f $(OBJS) core
