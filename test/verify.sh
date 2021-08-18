#!/bin/sh
PATH="$(pwd)/../../bin:$PATH"

mkdir -p output
cp input/* output
cd output

../../bin/endf2c
../../bin/merger
../../bin/linear
../../bin/recent
rm LINEAR.OUT
../../bin/sigma1
rm RECENT.OUT
../../bin/activate
rm SIGMA1.OUT
../../bin/legend
rm ACTIVATE.OUT
../../bin/fixup
rm LEGEND.OUT
../../bin/dictin
../../bin/sixpak
../../bin/spectra
../../bin/groupie
../../bin/mixer
../../bin/virgin
../../bin/evalplot
../../bin/complot
../../bin/evalhard1
mv PLOT0001.ps evalhard1.ps
../../bin/comhard1
mv PLOT0001.ps comhard1.ps
