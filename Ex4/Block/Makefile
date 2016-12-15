FCOMPFLAGS    =	-O3 -132
CCOMPFLAGS    =	-O3

FFLAGS        =	$(FCOMPFLAGS)
CFLAGS        = $(CCOMPFLAGS)
LDFLAGS       =	$(FCOMPFLAGS)

LD            = ifort	
FC            =	ifort
CC            =	gcc

MAKEFILE      =	Makefile
PROGRAM       = block	

OBJS	      =	block.o       \

all:		$(PROGRAM)

$(PROGRAM)::	$(INCS)
		@/bin/rm -f $(OBJS) core

$(PROGRAM)::	$(OBJS) $(MAKEFILE)
		@$(LD) $(LDFLAGS) $(OBJS) -o $(PROGRAM)

clean:;		@rm -f $(OBJS) core $(PROGRAM)
