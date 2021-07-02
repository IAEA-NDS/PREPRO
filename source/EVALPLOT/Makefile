# This file is part of PREPRO.
#
#    Author: Dermott (Red) Cullen
# Copyright: (C) International Atomic Energy Agency
#
# PREPRO is free software; you can redistribute it and/or modify it
# under the terms of the MIT License; see LICENSE file for more details.


FFLAGS = -O0 -w -static-libgfortran
CFLAGS = -w
LIBS   = -lXt -lX11
LIBDIR = -L/opt/X11/lib
CC     = gcc -I/opt/X11/include
FC     = gfortran

FSUB1= evalplot.f endfio.f scratchb.f screen.f

CSUBS= dash.c

OBJ= $(FSUB1:.f=.o) $(CSUBS:.c=.o)

SRC= $(FSUB1) $(CSUBS)

.PHONY: all
all: evalplot evalhard evalhard1

evalplot: $(OBJ)
	$(FC) -o evalplot $(FFLAGS) $(OBJ) $(LIBDIR) $(LIBS)

evalhard: evalplot.o endfio.o scratchb.o saveps.o
	$(FC) -o evalhard $(FFLAGS) evalplot.o endfio.o scratchb.o saveps.o

evalhard1: evalplot.o endfio.o scratchb.o saveps1.o
	$(FC) -o evalhard1 $(FFLAGS) evalplot.o endfio.o scratchb.o saveps1.o

.PHONY: install
install: evalplot evalhard evalhard1
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	mv $^ $(DESTDIR)$(PREFIX)/bin

.PHONY: clean
clean:
	rm -f evalplot evalhard evalhard1
	rm -f evalplot.o endfio.o scratchb.o screen.o \
	      dash.o saveps.o saveps1.o 
#
