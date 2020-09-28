FFLAGS = -O2 -std=legacy
CFLAGS =
LIBS   = -lX11
LIBDIR = -L/usr/X11/lib
CC     = gcc -I/usr/X11/include
FC     = gfortran
 
FSUB1= evalplot.f endfio.f scratchb.f timer.f seconds.f screen.f
 
CSUBS= dash.c
 
OBJ= $(FSUB1:.f=.o) $(CSUBS:.c=.o)
 
SRC= $(FSUB1) $(CSUBS)
 
.PHONY: all
all: evalplot evalhard
 
evalplot: $(OBJ)
	$(FC) -o evalplot $(FFLAGS) $(OBJ) $(LIBDIR) $(LIBS)
 
evalhard: evalplot.o endfio.o scratchb.o timer.o seconds.o saveps.o
	$(FC) -o evalhard $(FFLAGS) evalplot.o endfio.o scratchb.o timer.o \
        seconds.o saveps.o

evalhard1: evalplot.o endfio.o scratchb.o timer.o seconds.o saveps.o
	$(FC) -o evalhard $(FFLAGS) evalplot.o endfio.o scratchb.o timer.o \
        seconds.o saveps1.o

.PHONY: install
install: evalplot evalhard
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp $^ $(DESTDIR)$(PREFIX)/bin

.PHONY: clean
clean:
	rm -f evalplot evalhard *.o
	rm -f endfio.o evalplot.o saveps.o \
		scratchb.o screen.o seconds.o timer.o

