# This file is part of PREPRO.
#
#    Author: Dermott (Red) Cullen
# Copyright: (C) International Atomic Energy Agency
#
# PREPRO is free software; you can redistribute it and/or modify it
# under the terms of the MIT License; see LICENSE file for more details.


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
all: evalplot evalhard evalhard1

evalplot: $(OBJ)
	$(FC) -o evalplot $(FFLAGS) $(OBJ) $(LIBDIR) $(LIBS)

evalhard: evalplot.o endfio.o scratchb.o timer.o seconds.o saveps.o
	$(FC) -o evalhard $(FFLAGS) evalplot.o endfio.o scratchb.o timer.o \
        seconds.o saveps.o

evalhard1: evalplot.o endfio.o scratchb.o timer.o seconds.o saveps1.o
	$(FC) -o evalhard1 $(FFLAGS) evalplot.o endfio.o scratchb.o timer.o \
        seconds.o saveps1.o

.PHONY: install
install: evalplot evalhard evalhard1
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp $^ $(DESTDIR)$(PREFIX)/bin

.PHONY: clean
clean:
	rm -f evalplot evalhard evalhard1
	rm -f endfio.o evalplot.o saveps.o saveps1.o \
		dash.o scratchb.o screen.o seconds.o timer.o
