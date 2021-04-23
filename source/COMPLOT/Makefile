# This file is part of PREPRO.
#
#    Author: Dermott (Red) Cullen
# Copyright: (C) International Atomic Energy Agency
#
# PREPRO is free software; you can redistribute it and/or modify it
# under the terms of the MIT License; see LICENSE file for more details.


ifeq ($(PREFIX),)
	PREFIX := /usr/local
endif

FFLAGS = -O2 -std=legacy
CFLAGS = -fPIC
LIBS   = -lX11
LIBDIR = -L/usr/X11/lib
CC     = gcc -I/usr/X11/include
FC     = gfortran

FSUB1= complot.f endfio.f scratchb.f timer.f seconds.f screen.f

CSUBS= dash.c

OBJ= $(FSUB1:.f=.o) $(CSUBS:.c=.o)

SRC= $(FSUB1) $(CSUBS)

.PHONY: all
all: complot comhard comhard1

complot: $(OBJ)
	$(FC) -o complot $(FFLAGS) $(OBJ) $(LIBDIR) $(LIBS)

comhard: complot.o endfio.o scratchb.o timer.o seconds.o saveps.o
	$(FC) -o comhard $(FFLAGS) complot.o endfio.o scratchb.o timer.o \
        seconds.o saveps.o

comhard1: complot.o endfio.o scratchb.o timer.o seconds.o saveps1.o
	$(FC) -o comhard1 $(FFLAGS) complot.o endfio.o scratchb.o timer.o \
        seconds.o saveps1.o

.PHONY: install
install: complot comhard comhard1
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp $^ $(DESTDIR)$(PREFIX)/bin

.PHONY: clean
clean:
	rm -f complot comhard comhard1
	rm -f complot.o dash.o endfio.o saveps.o \
	    saveps1.o scratchb.o screen.o seconds.o timer.o
