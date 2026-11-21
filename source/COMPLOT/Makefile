# This file is part of PREPRO.
#
#    Author: Dermott (Red) Cullen
# Copyright: (C) International Atomic Energy Agency
#
# PREPRO is free software; you can redistribute it and/or modify it
# under the terms of the MIT License; see LICENSE file for more details.

os:=$(shell uname -s)

ifeq ($(os),Darwin)
     CFLAGS = -I/opt/X11/include -w
     LIBS   = -lXt -lX11
     LIBDIR = -L/opt/X11/lib
else
     CFLAGS = -I/usr/X11/include -w
     LIBS   = -lX11
     LIBDIR = -L/usr/X11/lib
endif

FFLAGS = -O0 -w -static-libgfortran
CC     = gcc
FC     = gfortran

FSUB1= complot.f endfio.f scratchb.f screen.f

CSUBS= dash.c

OBJ= $(FSUB1:.f=.o) $(CSUBS:.c=.o)

SRC= $(FSUB1) $(CSUBS)

.PHONY: all
all: complot comhard comhard1

complot: $(OBJ)
	$(FC) -o complot $(FFLAGS) $(OBJ) $(LIBDIR) $(LIBS)

comhard: complot.o endfio.o scratchb.o saveps.o
	$(FC) -o comhard $(FFLAGS) complot.o endfio.o scratchb.o saveps.o

comhard1: complot.o endfio.o scratchb.o saveps1.o
	$(FC) -o comhard1 $(FFLAGS) complot.o endfio.o scratchb.o saveps1.o

.PHONY: install
install: complot comhard comhard1
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp $^ $(DESTDIR)$(PREFIX)/bin

.PHONY: clean
clean:
	rm -f complot comhard comhard1
	rm -f complot.o endfio.o scratchb.o screen.o \
	      dash.o saveps.o saveps1.o 

