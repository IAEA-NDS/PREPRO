# This file is part of PREPRO.
#
#    Author: Dermott (Red) Cullen
# Copyright: (C) International Atomic Energy Agency
#
# PREPRO is free software; you can redistribute it and/or modify it
# under the terms of the MIT License; see LICENSE file for more details.


# makefile to create PREPRO code suite

FORT = gfortran
FLAGS = -O0 -static-libgfortran

PROG = \
activate \
convert \
dictin \
endf2c \
fixup \
groupie \
legend \
linear \
merger \
mixer \
recent \
relabel \
sigma1 \
sixpak \
spectra \
virgin

ifeq ($(graphics),yes)
	GRAPH_PROG = COMPLOT EVALPLOT
else
	GRAPH_PROG =
endif

CWD = $(shell pwd)
PREFIX ?= $(CWD)/../

COMMON_SRC = endfio.f scratchb.f 

.PHONY: all
all: $(PROG) $(GRAPH_PROG)

$(PROG): %: %.f
	$(FORT) $(FLAGS) -o $@ $< $(COMMON_SRC)

.PHONY: COMPLOT EVALPLOT
COMPLOT EVALPLOT:
	$(MAKE) -C $@

.PHONY: install
install:: $(PROG)
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp $^ $(DESTDIR)$(PREFIX)/bin

ifneq ($(GRAPH_PROG),)
install:: $(GRAPH_PROG)
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	$(MAKE) -C COMPLOT install DESTDIR=$(DESTDIR) PREFIX=$(PREFIX)
	$(MAKE) -C EVALPLOT install DESTDIR=$(DESTDIR) PREFIX=$(PREFIX)
endif

.PHONY: clean
clean:
	rm -f $(PROG)
	$(MAKE) -C COMPLOT clean
	$(MAKE) -C EVALPLOT clean
