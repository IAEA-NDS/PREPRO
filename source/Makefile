# makefile to create PREPRO code suite

FORT = gfortran
FLAGS = -O2 -std=legacy

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

ifeq ($(graphics),no)
	GRAPH_PROG =
else
	GRAPH_PROG = COMPLOT EVALPLOT
endif

CWD = $(shell pwd)
PREFIX ?= $(CWD)/../

COMMON_SRC = endfio.f scratchb.f timer.f seconds.f

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

install:: $(GRAPH_PROG)
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	$(MAKE) -C COMPLOT install DESTDIR=$(DESTDIR) PREFIX=$(PREFIX)
	$(MAKE) -C EVALPLOT install DESTDIR=$(DESTDIR) PREFIX=$(PREFIX)

.PHONY: clean
clean:
	rm -f $(PROG)
	$(MAKE) -C COMPLOT clean
	$(MAKE) -C EVALPLOT clean

